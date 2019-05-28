-- | Haskell language pragma
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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

            let mhgmh = fragmentRouting (uriFragment u)
            case mhgmh of
              Just hgmh -> case hgmh of
                BlobHash h -> do
                    toJSVal ("select blob" :: String) >>= logConsole
                    toJSVal ("fetching blob from hash:" <> show h) >>= logConsole
                    x <- ipfsGet (ipfsGetCapShallow "blob" (ipfsBase m)) h
                    let x' = fmap (\h -> Fix $ Compose (h, Compose Nothing)) x
                    pure . FocusAction hgmh $ FocusBlob x'

                DirHash h -> do
                    toJSVal ("select dir" :: String) >>= logConsole
                    toJSVal ("fetching dir from hash:" <> show h) >>= logConsole
                    x <- ipfsGet (ipfsGetCapShallow "dir" (ipfsBase m)) h
                    let x' = x { dirEntries = fmap f <$> dirEntries x}
                        f (FileEntity bh) = FileEntity $ Fix (Compose (bh, Compose Nothing))
                        f (DirEntity dh) = DirEntity dh
                        x'' = fmap (\h -> Fix $ Compose (h, Compose Nothing)) x'

                    pure . FocusAction hgmh $ FocusDir x''

                CommitHash h -> do
                    toJSVal ("select commit" :: String) >>= logConsole
                    toJSVal ("fetching commit from hash:" <> show h) >>= logConsole
                    x <- ipfsGet (ipfsGetCapShallow "commit" (ipfsBase m)) h
                    let x' = x { commitRootDir = Fix (Compose (commitRootDir x, Compose Nothing))}
                        x'' = fmap (\h -> Fix $ Compose (h, Compose Nothing)) x'

                    pure . FocusAction hgmh $ FocusCommit x''

              Nothing -> do
                toJSVal ("home handler" :: String) >>= logConsole
                toJSVal ("url: " <> show (uri m)) >>= logConsole
                pure Reset -- reset state, used when transitioning back to hone state


    updateFocusState (FocusAction h (FocusBlob   b)) _ =
      noEff $ m { focusState = FocusState h $ FocusBlob b }
    updateFocusState (FocusAction h (FocusDir    d)) _ =
      noEff $ m { focusState = FocusState h $ FocusDir d }
    updateFocusState (FocusAction h (FocusCommit c)) _ =
      noEff $ m { focusState = FocusState h $ FocusCommit c }

    -- used to update a leaf hash of some type if present (todo these could all be one thing maybe?)
    updateFocusState (ExpandHash hgmh) fs = case fs of
      FocusState _h (FocusBlob c) -> m <# do
        toJSVal ("expand hash (blob)" ++ show hgmh) >>= logConsole

        c' <- traverse (cataM $ expandBlobAlg m hgmh) c
        pure . FocusAction _h  $ FocusBlob c'

      FocusState _h (FocusDir c) -> m <# do
        toJSVal ("expand hash (dir)" ++ show hgmh) >>= logConsole

        c' <- traverse (cataM $ expandDirAlg m hgmh) c

        let f (FileEntity x) = FileEntity <$> cataM (expandBlobAlg m hgmh) x
            f (DirEntity x) = pure $ DirEntity x
        ls' <- traverse (traverse f) (dirEntries c')

        pure . FocusAction _h $ FocusDir $ c' { dirEntries = ls'}

      FocusState _h (FocusCommit c) -> m <# do
        toJSVal ("expand hash (commit)" ++ show hgmh) >>= logConsole

        c' <- traverse (cataM $ expandCommitAlg m hgmh) c
        rootDir <- cataM (expandDirAlg m hgmh) (commitRootDir c')

        pure . FocusAction _h $ FocusCommit $ c' { commitRootDir = rootDir}
      _             -> fail "whoops! TODO better msg (expandhash match failure)"

    updateFocusState Reset _ = noEff $ m { focusState = NoFocus "" "" ""}
    updateFocusState NoOp _ = noEff m
    updateFocusState _ _ = noEff m -- should never happen (FIXME, this probably isn't true anymore, needs logging, etc)








-- FIXME: lots of low hanging fruit re: early termination of traversal
expandBlobAlg
  :: Model
  -> HGitMerkleHash
  -> AlgebraM IO (HashAnnotated Blob `Compose` Maybe `Compose` Blob)
                 (Fix (HashAnnotated Blob `Compose` Maybe `Compose` Blob))
expandBlobAlg m hgmh x@(Compose (h', Compose Nothing))
              | BlobHash h' == hgmh   = do
                  toJSVal ("fetching blob from hash for expand op:" <> show h') >>= logConsole
                  substantiated <- ipfsGet (ipfsGetCapShallow "blob" (ipfsBase m)) h'
                  let substantiated' = fmap (\h -> Fix $ Compose (h, Compose Nothing)) substantiated
                  pure . Fix $ Compose (h', Compose $ Just substantiated')
              | otherwise = pure $ Fix x
expandBlobAlg m h x = pure $ Fix x

-- FIXME: lots of low hanging fruit re: early termination of traversal
expandDirAlg
  :: Model
  -> HGitMerkleHash
  -> AlgebraM IO (HashAnnotated HashableDir `Compose` Maybe `Compose` Dir PartialBlob)
                 (Fix (HashAnnotated HashableDir `Compose` Maybe `Compose` Dir PartialBlob))
expandDirAlg m hgmh x@(Compose (h', Compose Nothing))
      | DirHash h' == hgmh = do
          toJSVal ("fetching dir from hash for expand op:" <> show h') >>= logConsole
          substantiated <- ipfsGet (ipfsGetCapShallow "dir" (ipfsBase m)) h'
          let substantiated' = fmap (\h -> Fix $ Compose (h, Compose Nothing)) substantiated
              f (DirEntity x) = DirEntity x
              f (FileEntity h) = FileEntity $ Fix $ Compose (h, Compose Nothing)
              substantiated''
                = substantiated'
                { dirEntries = fmap (fmap f) $ dirEntries substantiated'
                }
          pure . Fix $ Compose (h', Compose $ Just substantiated'')
      | otherwise = pure $ Fix x -- no need to recurse on blob, no dir present here :)
expandDirAlg m hgmh (Compose (h, Compose (Just (Dir ls)))) = do -- need to recurse on blob
  let f (FileEntity x) = FileEntity <$> cataM (expandBlobAlg m hgmh) x
      f (DirEntity x) = pure $ DirEntity x
  ls' <- traverse (traverse f) ls
  pure $ Fix $ Compose (h, Compose (Just $ Dir ls'))

-- FIXME: lots of low hanging fruit re: early termination of traversal
expandCommitAlg
  :: Model
  -> HGitMerkleHash
  -> AlgebraM IO (HashAnnotated HashableCommit `Compose` Maybe `Compose` Commit PartialDir)
                 (Fix (HashAnnotated HashableCommit `Compose` Maybe `Compose` Commit PartialDir))
expandCommitAlg m hgmh x@(Compose (h', Compose Nothing))
              | CommitHash h' == hgmh = do
                  toJSVal ("fetching commit from hash for expand op:" <> show h') >>= logConsole
                  substantiated <- ipfsGet (ipfsGetCapShallow "commit" (ipfsBase m)) h'
                  let substantiated' = fmap (\h -> Fix $ Compose (h, Compose Nothing)) substantiated
                      substantiated''
                        = substantiated'
                        { commitRootDir = Fix (Compose (commitRootDir substantiated', Compose Nothing))
                        }
                  pure . Fix $ Compose (h', Compose $ Just substantiated'')
              | otherwise = pure $ Fix x  -- no need to recurse on dir, no commit present here :)
expandCommitAlg m hgmh (Compose (h, Compose (Just c))) = do -- need to recurse on blob
  rootDir <- cataM (expandDirAlg m hgmh) (commitRootDir c)
  pure $ Fix $ Compose (h, Compose (Just c { commitRootDir = rootDir }))

hTable :: URI -> HGitMerkleHash -> View Action -> View Action
hTable u hgmh v
  = table_ [class_ $ "entity " <> class']
  [ tbody_ [class_ $ "entity " <> class']
    [ tr_ [class_ $ "entity " <> class']
          [ button_ [onClick $ goto u hgmh, class_ class']
                    [text header]
          ]

    , tr_ [class_ $ "entity " <> class'] [v]
    ]
  ]
  where
    class' = case hgmh of
      BlobHash h   -> "blob"
      DirHash h    -> "dir"
      CommitHash h -> "commit"
    header = case hgmh of
      BlobHash h   -> "set focus to blob: " <> renderPartialHash h
      DirHash h    -> "set focus to dir: " <> renderPartialHash h
      CommitHash h -> "set focus to commit: " <> renderPartialHash h

renderBlob :: URI -> PartialBlob1 -> View Action
renderBlob u hb = blobAlgDefined u $ fmap (cata $ blobAlgFull u) hb

blobAlgFull :: URI -> Algebra (HashAnnotated Blob `Compose` Maybe `Compose` Blob) (View Action)
blobAlgFull u (Compose (h, Compose Nothing))
      = button_ [onClick $ ExpandHash (BlobHash h), class_ "blob"]
                [text $ "expand blob with hash: " <> renderPartialHash h]
blobAlgFull u (Compose (h, Compose (Just x))) = hTable u (BlobHash h) $ blobAlgDefined u x

blobAlgDefined :: URI -> Blob (View Action) -> View Action
blobAlgDefined u (ChunkList chunks) = div_ [] $ text "blob chunk list:" : br_ [] : intersperse (br_ []) chunks
blobAlgDefined u (Chunk contents)
      = pre_ [] [text $ toJSString contents]

renderDir :: URI -> PartialDir1 -> View Action
renderDir u hd = dirAlgDefined u $ fmap (cata $ dirAlgFull u) hd

dirAlgFull :: URI -> Algebra (HashAnnotated HashableDir `Compose` Maybe `Compose` Dir PartialBlob) (View Action)
dirAlgFull u (Compose (h, Compose Nothing))
      = button_ [onClick $ ExpandHash (DirHash h), class_ "dir"]
                [text $ "expand dir with hash: ", code_ [] [text $ renderPartialHash h]]
dirAlgFull u (Compose (h, Compose (Just x))) = hTable u (DirHash h) $ dirAlgDefined u x

dirAlgDefined :: URI -> Algebra (Dir PartialBlob) (View Action)
dirAlgDefined u (Dir contents)
      = div_ [] $ intersperse (br_ []) $ fmap mkC contents

  where
    mkC (fp, FileEntity h) = div_ [] [ code_ [] [text $ toJSString fp]
                                     , code_ [] [text " "]
                                     , cata (blobAlgFull u) h
                                     ]
    mkC (fp, DirEntity  x) = div_ [] [ code_ [] [text $ toJSString $ fp <> "/"]
                                     , code_ [] [text " "]
                                     , x
                                     ]

renderCommit :: URI -> PartialCommit1 -> View Action
renderCommit u hc = commitAlgDefined u $ fmap (cata $ commitAlgFull u) hc

commitAlgFull :: URI -> Algebra (HashAnnotated HashableCommit `Compose` Maybe `Compose` Commit PartialDir) (View Action)
commitAlgFull u (Compose (h, Compose Nothing))
      = button_ [onClick $ ExpandHash (CommitHash h), class_ "commit"]
                [text $ "expand commit with hash: ", code_ [] [text $ renderPartialHash h]]
commitAlgFull u (Compose (h, Compose (Just x))) = hTable u (CommitHash h) $ commitAlgDefined u x

commitAlgDefined :: URI -> Algebra (Commit PartialDir) (View Action)
commitAlgDefined u (Commit msg root parents)
      = div_ [] $
      [ text "commit msg: "
      , pre_ [] [text $ toJSString msg]
      , br_ []
      , cata (dirAlgFull u) root
      , br_ []
      ] ++ case parents of
              [] -> [text "commit has no parents"]
              parents' -> [ text "commit parents: "
                          , br_ []
                          , div_ [] $ intersperse (br_ []) parents
                          ]

viewModel' :: Model -> View Action
viewModel' (Model u _ fs) = div_ []
      [ nodeHtml "style" [] [text $ toJSString $ TL.toStrict $ Clay.render myCSS]
      , handler $ fragmentRouting (uriFragment u)
      ]

  where

    handler Nothing = home fs
    handler (Just (BlobHash    _h)) = case fs of -- NOTE: doesn't use h, which feels weird
      (FocusState h (FocusBlob b))   -> div_ [] [text "top-level focus: blob", br_ [], hTable u h $ renderBlob u b]
      x               -> div_ [] [text "unexpected state, (expected FocusBlob)"]
    handler (Just (DirHash     _h)) = case fs of -- NOTE: doesn't use h, which feels weird
      (FocusState h (FocusDir d))    -> div_ [] [text "top-level focus: dir", br_ [], hTable u h $ renderDir u d]
      x               -> div_ [] [text "unexpected state, (expected FocusDir)"]
    handler (Just (CommitHash  _h)) = case fs of -- NOTE: doesn't use h, which feels weird
      (FocusState h (FocusCommit c)) -> div_ [] [text "top-level focus: commit", br_ [], hTable u h $ renderCommit u c]
      x               -> div_ [] [text "unexpected state, (expected FocusCommit)"]

    home (NoFocus b d c) = div_ []
      [ input_ [ type_ "text"
               , autofocus_ True
               , onInput UpdateBlobHashField
               , onEnter . goto u . BlobHash . Const . RawIPFSHash $ fromMisoString b
               ]
      , br_ []
      , text "press enter to load blob from IPFS daemon via hash"
      , br_ []
      , br_ []
      , input_ [ type_ "text"
               , autofocus_ True
               , onInput UpdateDirHashField
               , onEnter . goto u . BlobHash . Const . RawIPFSHash $ fromMisoString d
               ]
      , br_ []
      , text "press enter to load dir from IPFS daemon via hash"
      , br_ []
      , br_ []
      , input_ [ type_ "text"
               , autofocus_ True
               , onInput UpdateCommitHashField
               , onEnter . goto u . BlobHash . Const . RawIPFSHash $ fromMisoString c
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

goto :: URI -> HGitMerkleHash -> Action
goto u h = ChangeURI $ u { uriFragment = mkFragment (Just h) }

type PartiallySubstantiated h f = Fix (HashAnnotated h `Compose` Maybe `Compose` f)

type PartialBlob = PartiallySubstantiated Blob Blob
type PartialDir   = PartiallySubstantiated HashableDir (Dir PartialBlob)
type PartialCommit  = PartiallySubstantiated HashableCommit (Commit PartialDir)


type PartialBlob1 = Blob PartialBlob
type PartialDir1   = Dir PartialBlob PartialDir
type PartialCommit1  = Commit PartialDir PartialCommit

-- | Type synonym for an application model
data FocusState
  = NoFocus
      MisoString -- blob hash field value
      MisoString -- dir hash field value
      MisoString -- commit hash field value

  -- after entering a hash, can be lazily expanded.. (via hash lookup?)
  -- NOTE: what if I use IORefs?!
  | FocusState HGitMerkleHash HGitMerkleFocus -- TODO.. hash and focus should be same type (assumption)
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

  | FocusAction HGitMerkleHash HGitMerkleFocus -- TODO.. hash and focus should be same type (assumption)
  | DownloadFile FilePath

  -- todo branching sum type? getting kinda big..
  | HandleURI URI
  | ChangeURI URI

  | Reset
  | NoOp
  deriving (Eq)

data HGitMerkleFocus
  = FocusBlob   PartialBlob1
  | FocusDir    PartialDir1
  | FocusCommit PartialCommit1
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

renderPartialHash :: Hash x -> MisoString
renderPartialHash = toJSString . T.take hashDisplayLen . unRawIPFSHash . getConst
  where hashDisplayLen = 7
