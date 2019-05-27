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
         padding (px 10) (px 5) (px 5) (px 10)
         color       Sol.base02
         borderStyle solid
         borderWidth (px 2)
    ".hashlink" ?
      do background  Sol.base2
         padding (px 10) (px 5) (px 5) (px 10)
         color       Sol.base02
         borderStyle dashed
         borderWidth (px 2)

    ".blob" ? borderColor Sol.violet
    ".dir" ? borderColor Sol.magenta
    ".commit" ? borderColor Sol.blue

    "button.blob" ?
      do background   Sol.violet
         color        Sol.base3
         borderRadius (px 10) (px 10) (px 10) (px 10)

    "button.dir" ?
      do background  Sol.magenta
         color       Sol.base3
         borderRadius (px 10) (px 10) (px 10) (px 10)

    "button.commit" ?
      do background  Sol.blue
         color       Sol.base3
         borderRadius (px 10) (px 10) (px 10) (px 10)
