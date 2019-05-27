-- | Haskell language pragma
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

-- | Haskell module declaration
module Main where


import Control.Applicative (Const(..))
import Data.Aeson
import Data.Bool (bool)
import Data.Functor.Compose
import Data.Monoid
import qualified Data.Map as M
import Data.List (isPrefixOf, intersperse)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Data.ByteString.Base64 as Base64


import qualified Clay as Clay

-- | Miso framework import
import Miso
import Miso.String (fromMisoString, MisoString)

-- import Merkle.Types
import Merkle.Types.IPFS
-- import Merkle.Store
import Data.Aeson.Orphans
import Merkle.Store.Deref
import qualified Merkle.Store as MS
import qualified Merkle.Types as MT
import HGit.Core.Types
import Util.RecursionSchemes

import JavaScript.Web.XMLHttpRequest as JS

import GHCJS.Types
import GHCJS.Marshal

import Language.Javascript.JSaddle.Value
import Language.Javascript.JSaddle.Marshal.String

import Data.ByteString as BS (ByteString, pack)

import Data.ByteString.Lazy (toStrict)

import Network.URI

import qualified Data.Text.Lazy as TL

-- TODO: organize to separate out my stuff
import Css


-- | Entry point for a miso application
main :: IO ()
main = do
    currentURI <- getCurrentURI
    toJSVal ("starting, with current URI:  " <> show currentURI) >>= logConsole
    -- ipfsBase' <- promptWindow "enter ipfs daemon base URI" "http://localhost:5001"
    --   >>= fromJSVal
    --   >>= maybe (fail "could not convert prompt result to string") pure
    -- TODO: parse relative to URI
    let ipfsBase' = MockSubdir "http://localhost:8000"
    --           uriPath :: String
    -- /ghc
    -- uriQuery :: String
    -- ?query
    -- uriFragment :: String

    -- toJSVal ("you entered this IPFS daemon base URL:  " <> show ipfsBase') >>= logConsole
    startApp $ app currentURI ipfsBase'



app :: URI -> IPFSBase -> App Model Action
app currentURI ipfsBase' = do
    App { model = Model
                { uri = currentURI
                , ipfsBase = ipfsBase'
                , focusState = NoFocus "" "" ""
                }
        , initialAction = HandleURI currentURI
        , update = updateModel
        , view = viewModel'
        , events = defaultEvents -- default delegated events
          -- pops this event on URI update - wonder what other subscriptions exist?
          -- presumably this only applies to window events which (I think are basically just uri change, mouse move, etc)
        , subs          = [uriSub HandleURI ]
        , mountPoint    = Nothing       -- mount point for application (Nothing defaults to 'body')
        }


-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel a m = updateFocusState a (focusState m)

  where
    updateFocusState (UpdateBlobHashField   b) (NoFocus _ d c) = noEff $ m { focusState = NoFocus b d c }
    updateFocusState (UpdateDirHashField    d) (NoFocus b _ c) = noEff $ m { focusState = NoFocus b d c }
    updateFocusState (UpdateCommitHashField c) (NoFocus b d _) = noEff $ m { focusState = NoFocus b d c }

    updateFocusState (ChangeURI u) _ = m <# do
      toJSVal ("change uri:  " <> show u) >>= logConsole
      pushURI u
      pure NoOp
    updateFocusState (HandleURI u) _ =
      let m' = m { uri = u }
       in m' <# do
            toJSVal ("handleURI: " <> show u) >>= logConsole

            case fragmentRouting (uriFragment u) of
                (Just (BlobHash h)) -> do
                    toJSVal ("select blob" :: String) >>= logConsole
                    toJSVal ("fetching blob from hash:" <> show h) >>= logConsole
                    x <- ipfsGet (ipfsGetCapShallow "blob" (ipfsBase m)) h
                    let x' = fmap (\h -> Fix $ Compose (h, Compose Nothing)) x
                    pure . FocusAction $ FocusBlob x' -- todo: should handle failure/not found case gracefully, eventually

                (Just (DirHash h)) -> do
                    toJSVal ("select dir" :: String) >>= logConsole
                    toJSVal ("fetching dir from hash:" <> show h) >>= logConsole
                    x <- ipfsGet (ipfsGetCapShallow "dir" (ipfsBase m)) h
                    let x' = fmap (\h -> Fix $ Compose (h, Compose Nothing)) x
                    pure . FocusAction $ FocusDir x' -- todo: should handle failure/not found case gracefully, eventually

                (Just (CommitHash h)) -> do
                    toJSVal ("select commit" :: String) >>= logConsole
                    toJSVal ("fetching commit from hash:" <> show h) >>= logConsole
                    x <- ipfsGet (ipfsGetCapShallow "commit" (ipfsBase m)) h
                    let x' = fmap (\h -> Fix $ Compose (h, Compose Nothing)) x
                    pure . FocusAction $ FocusCommit x' -- todo: should handle failure/not found case gracefully, eventually

                Nothing -> do
                  toJSVal ("home handler" :: String) >>= logConsole
                  toJSVal ("url: " <> show (uri m)) >>= logConsole
                  pure Reset -- reset state, used when transitioning back to hone state


    updateFocusState (FocusAction (FocusBlob   b)) _ =
      noEff $ m { focusState = FocusState $ FocusBlob b }
    updateFocusState (FocusAction (FocusDir    d)) _ =
      noEff $ m { focusState = FocusState $ FocusDir d }
    updateFocusState (FocusAction (FocusCommit c)) _ =
      noEff $ m { focusState = FocusState $ FocusCommit c }

    -- used to update a leaf hash of some type if present (todo these could all be one thing maybe?)
    updateFocusState (ExpandHash (BlobHash h)) fs = case fs of
      FocusState (FocusBlob c) -> m <# do
        toJSVal ("expand hash (blob)" ++ show h) >>= logConsole

        let alg x@(Compose (h', Compose Nothing))
              | h' == h   = do
                  toJSVal ("fetching blob from hash for expand op:" <> show h) >>= logConsole
                  substantiated <- ipfsGet (ipfsGetCapShallow "blob" (ipfsBase m)) h
                  let substantiated' = fmap (\h -> Fix $ Compose (h, Compose Nothing)) substantiated
                  pure . Fix $ Compose (h, Compose $ Just substantiated')
              | otherwise = pure $ Fix x
            alg x = pure $ Fix x

        c' <- traverse (cataM alg) c
        pure . FocusAction $ FocusBlob c'
      _             -> fail "whoops! TODO better msg (blob focus state expandhash match failure)"

    updateFocusState (ExpandHash (DirHash h)) fs = case fs of
      FocusState (FocusDir c) -> m <# do
        toJSVal ("expand hash (dir)" ++ show h) >>= logConsole

        let alg x@(Compose (h', Compose Nothing))
              | h' == h   = do
                  toJSVal ("fetching dir from hash for expand op:" <> show h) >>= logConsole
                  substantiated <- ipfsGet (ipfsGetCapShallow "dir" (ipfsBase m)) h
                  let substantiated' = fmap (\h -> Fix $ Compose (h, Compose Nothing)) substantiated
                  pure . Fix $ Compose (h, Compose $ Just substantiated')
              | otherwise = pure $ Fix x
            alg x = pure $ Fix x

        c' <- traverse (cataM alg) c
        pure . FocusAction $ FocusDir c'
      _             -> fail "whoops! TODO better msg (dir focus state expandhash match failure)"

    updateFocusState (ExpandHash (CommitHash h)) fs = case fs of
      FocusState (FocusCommit c) -> m <# do
        toJSVal ("expand hash (commit)" ++ show h) >>= logConsole

        let alg x@(Compose (h', Compose Nothing))
              | h' == h   = do
                  toJSVal ("fetching commit from hash for expand op:" <> show h) >>= logConsole
                  substantiated <- ipfsGet (ipfsGetCapShallow "commit" (ipfsBase m)) h
                  let substantiated' = fmap (\h -> Fix $ Compose (h, Compose Nothing)) substantiated
                  pure . Fix $ Compose (h, Compose $ Just substantiated')
              | otherwise = pure $ Fix x
            alg x = pure $ Fix x

        c' <- traverse (cataM alg) c
        pure . FocusAction $ FocusCommit c'
      _             -> fail "whoops! TODO better msg (commit focus state expandhash match failure)"

    updateFocusState Reset _ = noEff $ m { focusState = NoFocus "" "" ""}
    updateFocusState NoOp _ = noEff m
    updateFocusState _ _ = noEff m -- should never happen (FIXME, this probably isn't true anymore, needs logging, etc)



renderBlob :: URI -> Blob (PartiallySubstantiated Blob) -> View Action
renderBlob u hb = algDefined $ fmap (cata algFull) hb
  where
    algFull :: (HashAnnotated Blob `Compose` Maybe `Compose` Blob) (View Action) -> View Action
    algFull (Compose (h, Compose Nothing))
          = div_ [ class_ "hashlink blob"]
          [ text $ toJSString $ unRawIPFSHash (getConst h)
          , button_ [onClick $ ExpandHash (BlobHash h), class_ "blob"]
                    [text $ toJSString $ unRawIPFSHash (getConst h)]
          ]
    algFull (Compose (h, Compose (Just x))) = algDefined x

    algDefined :: Blob (View Action) -> View Action
    algDefined Empty = div_ [ class_ "entity blob"] [text "empty chunk"]
    algDefined (Chunk contents next)
          = div_ [class_ "entity blob"] $
          [ div_ [] $ intersperse (br_ []) $ fmap (text . toJSString . mkNbsp) $ lines contents
          , next
          ]

-- janky function to make sure spaces render right, only supports spaces because I don't use tabs
mkNbsp :: String -> String
mkNbsp (' ':xs) = '\160' : mkNbsp xs
mkNbsp (x:xs) = x : mkNbsp xs
mkNbsp [] = []



renderDir :: URI -> HashableDir (PartiallySubstantiated HashableDir) -> View Action
renderDir u hd = algDefined $ fmap (cata algFull) hd
  where
    algFull :: (HashAnnotated HashableDir `Compose` Maybe `Compose` HashableDir) (View Action) -> View Action
    algFull (Compose (h, Compose Nothing))
          = div_ [class_ "hashlink dir"]
                 [button_ [onClick $ ExpandHash (DirHash h), class_ "dir"]
                          [text $ toJSString $ unRawIPFSHash (getConst h)]
                 ]
    algFull (Compose (h, Compose (Just x))) = algDefined x

    algDefined :: HashableDir (View Action) -> View Action
    algDefined (Dir contents)
          = div_ [class_ "entity dir"] $
          [ text "directory contents:"
          , br_ []
          , ul_ [] $ fmap mkC contents
          ]

    mkC (fp, FileEntity h) = li_ [] [ button_ [onClick $ gotoBlob u h, class_ "blob"]
                                              [text $ toJSString $ "file: " ++ fp]
                              ]
    mkC (fp, DirEntity  x) = li_ [] [text $ toJSString $ "dir: " ++ fp, x]

-- TODO: could have all fields expandable, Commit (PartiallySubstantiated Dir) instead
renderCommit :: URI -> HashableCommit (PartiallySubstantiated HashableCommit) -> View Action
renderCommit u hc = algDefined $ fmap (cata algFull) hc
  where
    algFull :: (HashAnnotated HashableCommit `Compose` Maybe `Compose` HashableCommit) (View Action) -> View Action
    algFull (Compose (h, Compose Nothing))
          = li_ [class_ "hashlink commit"]
          [ button_ [onClick $ ExpandHash (CommitHash h), class_ "commit"]
                    [text $ toJSString $ unRawIPFSHash (getConst h)]
          ]
    algFull (Compose (h, Compose (Just x))) = algDefined x

    algDefined :: HashableCommit (View Action) -> View Action
    algDefined (Commit msg root parents)
          = li_ [class_ "entity commit"] $
          [ text $ toJSString $ "msg: " ++ msg
          , br_ []
          , button_ [onClick $ gotoDir u root, class_ "dir"]
                    [text $ toJSString $ unRawIPFSHash (getConst root)]
          , br_ []
          ] ++ case parents of
                 [] -> [text "commit has no parents"]
                 parents' -> [ text "parents: "
                             , br_ []
                             , ul_ [] parents
                             ]

viewModel' :: Model -> View Action
viewModel' (Model u _ fs) = div_ []
      -- [ nodeHtml "style" [] [text ".myclass {\n background-color: #6c71c4;\n}"]
      [ nodeHtml "style" [] [text $ toJSString $ TL.toStrict $ Clay.render myCSS]
      , handler $ fragmentRouting (uriFragment u)
      ]

  where

    handler Nothing = home fs
    handler (Just (BlobHash    _h)) = case fs of -- NOTE: doesn't use h, which feels weird
      (FocusState (FocusBlob b))   -> div_ [] [text "focus: blob", br_ [], renderBlob u b]
      x               -> div_ [] [text "unexpected state, (expected FocusBlob)"]
    handler (Just (DirHash     _h)) = case fs of -- NOTE: doesn't use h, which feels weird
      (FocusState (FocusDir d))    -> div_ [] [text "focus: dir", br_ [], renderDir u d]
      x               -> div_ [] [text "unexpected state, (expected FocusDir)"]
    handler (Just (CommitHash  _h)) = case fs of -- NOTE: doesn't use h, which feels weird
      (FocusState (FocusCommit c)) -> div_ [] [text "focus: commit", br_ [], renderCommit u c]
      x               -> div_ [] [text "unexpected state, (expected FocusCommit)"]

    home (NoFocus b d c) = div_ []
      [ input_ [ type_ "text"
               , autofocus_ True
               , onInput UpdateBlobHashField
               , onEnter . gotoBlob u . Const . RawIPFSHash $ fromMisoString b
               ]
      , br_ []
      , text "press enter to load blob from IPFS daemon via hash"
      , br_ []
      , br_ []
      , input_ [ type_ "text"
               , autofocus_ True
               , onInput UpdateDirHashField
               , onEnter . gotoDir u . Const . RawIPFSHash $ fromMisoString d
               ]
      , br_ []
      , text "press enter to load dir from IPFS daemon via hash"
      , br_ []
      , br_ []
      , input_ [ type_ "text"
               , autofocus_ True
               , onInput UpdateCommitHashField
               , onEnter . gotoCommit u . Const . RawIPFSHash $ fromMisoString c
               ]
      , br_ []
      , text "press enter to load commit from IPFS daemon via hash"
      , br_ []
      , br_ []
      , br_ []
      , br_ []
      , br_ []
      ]
    home _ = div_ [] ["not expected state (home but not nofocus - loading release/torrent?)"]


onEnter :: Action -> Attribute Action
onEnter action =
  onKeyDown $ bool NoOp action . (== KeyCode 13)


-- TODO: use local storage to cache ipfs node info
-- localStorage.setItem("lastname", "Smith");
-- localStorage.getItem("lastname");



-- | pop text prompt alert window
foreign import javascript unsafe
  "window.prompt($1,$2)"
  promptWindow :: JSString -> JSString -> IO JSVal

-- | Log javascript value to console
foreign import javascript unsafe
  "console.log($1);"
  logConsole :: JSVal -> IO ()

-- this works for test file/test b64!
foreign import javascript unsafe
  "saveByteArray($1, base64ToArrayBuffer($2))"
  downloadFile :: JSString -> JSString -> IO ()

data IPFSPutResp f = IPFSPutResp (Hash f) Int

instance FromJSON (IPFSPutResp i) where
    parseJSON = withObject "IPFS Put Resp" $ \v -> IPFSPutResp
        <$> v .: "Key"
        <*> v .: "Size"

ipfsGet :: MS.GetCapabilityShallow IO RawIPFSHash f -> Hash f -> IO (f (Hash f))
ipfsGet cap h = MS.gcGetShallow cap h >>= maybe (fail "ipfs get failed for hash, TODO: add better msg") pure


-- | 'IPFS' fetcher based on directory with files in same static site
--   used to avoid complexity for demos
mockIpfsGetCapShallow
  :: FromJSON (f (Hash f))
  => String
  -> JSString
  -> MS.GetCapabilityShallow IO RawIPFSHash f
mockIpfsGetCapShallow typeName baseUri = MS.GetCapabilityShallow $ \(Const (RawIPFSHash h)) -> do
      (toJSVal $ "get value of type: " ++ typeName ++ " via hash " ++ show h) >>= logConsole

      let req = Request { reqMethod = JS.GET
                        , reqURI =  baseUri <> "/mockipfs/" <> toJSString (T.unpack h) <> ".json"
                        , reqLogin = Nothing
                        , reqHeaders = []
                        , reqWithCredentials = False
                        , reqData = NoData
                        }

      Just resp <- contents <$> xhrByteString req -- yolo, pattern match
      case eitherDecodeStrict resp of
        Left s -> error s
        Right (DagNode x _) -> pure $ Just x


ipfsGetCapShallow
  :: FromJSON (f (Hash f))
  => String
  -> IPFSBase
  -> MS.GetCapabilityShallow IO RawIPFSHash f
ipfsGetCapShallow typeName (IPFSDaemon node) = ipfsGetCapShallow' typeName node
ipfsGetCapShallow typeName (MockSubdir path) = mockIpfsGetCapShallow typeName path

ipfsGetCapShallow'
  :: FromJSON (f (Hash f))
  => String
  -> JSString
  -> MS.GetCapabilityShallow IO RawIPFSHash f
ipfsGetCapShallow' typeName baseUri = MS.GetCapabilityShallow $ \(Const (RawIPFSHash h)) -> do
      (toJSVal $ "get value of type: " ++ typeName ++ " via hash " ++ show h) >>= logConsole

      let req = Request { reqMethod = JS.GET
                        , reqURI =  baseUri <> "/api/v0/object/get?data-encoding=base64&arg=" <> toJSString (T.unpack h)
                        , reqLogin = Nothing
                        , reqHeaders = []
                        , reqWithCredentials = False
                        , reqData = NoData
                        }

      Just resp <- contents <$> xhrByteString req -- yolo, pattern match
      case eitherDecodeStrict resp of
        Left s -> error s
        Right (DagNode x _) -> pure $ Just x

blobPrefix, dirPrefix, commitPrefix :: String
blobPrefix   = "#blob:"
dirPrefix    = "#dir:"
commitPrefix = "#commit:"

fragmentRouting :: String -> Maybe HGitMerkleHash
fragmentRouting s
    | blobPrefix   `isPrefixOf` s = Just $ BlobHash   $ Const $ RawIPFSHash $ T.pack $ drop (length blobPrefix)   s
    | dirPrefix    `isPrefixOf` s = Just $ DirHash    $ Const $ RawIPFSHash $ T.pack $ drop (length dirPrefix)    s
    | commitPrefix `isPrefixOf` s = Just $ CommitHash $ Const $ RawIPFSHash $ T.pack $ drop (length commitPrefix) s
    | otherwise = Nothing -- parse failure or nothing go to, home screen

mkFragment :: Maybe HGitMerkleHash -> String
mkFragment Nothing = ""
mkFragment (Just (BlobHash h)) = blobPrefix ++ T.unpack (unRawIPFSHash $ getConst h)
mkFragment (Just (DirHash h)) = dirPrefix  ++ T.unpack (unRawIPFSHash $ getConst h)
mkFragment (Just (CommitHash h)) = commitPrefix ++ T.unpack (unRawIPFSHash $ getConst h)

gotoBlob :: URI -> Hash Blob -> Action
gotoBlob u h = ChangeURI $ u { uriFragment = mkFragment (Just $ BlobHash h) }

gotoDir :: URI -> Hash HashableDir -> Action
gotoDir u h = ChangeURI $ u { uriFragment = mkFragment (Just $ DirHash h) }

gotoCommit :: URI -> Hash HashableCommit -> Action
gotoCommit u h = ChangeURI $ u { uriFragment = mkFragment (Just $ CommitHash h) }



type PartiallySubstantiated f = Fix (HashAnnotated f `Compose` Maybe `Compose` f)

-- | Type synonym for an application model
data FocusState
  = NoFocus
      MisoString -- blob hash field value
      MisoString -- dir hash field value
      MisoString -- commit hash field value

  -- after entering a hash, can be lazily expanded.. (via hash lookup?)
  -- NOTE: what if I use IORefs?!
  | FocusState HGitMerkleFocus
  deriving (Eq)

data IPFSBase
  = IPFSDaemon JSString
  | MockSubdir JSString
  deriving (Eq, Show)

data Model
  = Model
  { uri :: URI -- current URI of application
  , ipfsBase :: IPFSBase
  , focusState :: FocusState
  } deriving (Eq)

data Action
  = UpdateBlobHashField   MisoString
  | UpdateDirHashField    MisoString
  | UpdateCommitHashField MisoString

  | ExpandHash HGitMerkleHash -- used to update a leaf hash of some type if present

  | FocusAction HGitMerkleFocus
  | DownloadFile FilePath

  -- todo branching sum type? getting kinda big..
  | HandleURI URI
  | ChangeURI URI

  | Reset
  | NoOp
  deriving (Eq)

data HGitMerkleFocus
  = FocusBlob   (Blob (PartiallySubstantiated Blob))
  | FocusDir    (HashableDir (PartiallySubstantiated HashableDir))
  | FocusCommit (HashableCommit (PartiallySubstantiated HashableCommit))
  deriving (Eq)

instance Show HGitMerkleFocus where
  show (FocusCommit _) = "focus commit"
  show (FocusDir _) = "focus dir"
  show (FocusBlob _) = "focus blob"


data HGitMerkleHash
  = BlobHash   (Hash Blob)
  | DirHash    (Hash HashableDir)
  | CommitHash (Hash HashableCommit)
  deriving (Eq, Show)
