{-# LANGUAGE OverloadedStrings #-}
module Css where


-- | Css gen'd via this
import Clay
import Clay.Color
import qualified Solarized as Sol


myCSS :: Css
myCSS = do
    body ?
      do background  Sol.base3
         color       Sol.base02
    ".entity" ?
      do background  Sol.base2
         padding (px 5) (px 10) (px 10) (px 10)
         color       Sol.base02
         borderStyle solid
         borderWidth (px 2)
    ".hashlink" ?
      do background  Sol.base2
         padding (px 5) (px 10) (px 10) (px 10)
         color       Sol.base02
         borderStyle dashed
         borderWidth (px 2)
    ".blob" ?
      borderColor Sol.violet
    ".dir" ?
      borderColor Sol.magenta
    ".commit" ?
      borderColor Sol.blue
