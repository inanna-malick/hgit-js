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
      do background  Sol.base3
         padding (px 2) (px 5) (px 5) (px 2)
         color       Sol.base02
         width       (pct 100)


         borderRightColor Sol.base2
         borderTopColor Sol.base2
         borderBottomColor Sol.base2

         borderStyle solid
         -- borderLeftStyle solid
         -- borderRightStyle none
         -- borderTopStyle solid
         -- borderBottomStyle none

         borderWidth (px 3)

    ul ? do listStyleType none

    ".blob" ? borderLeftColor Sol.violet
    ".dir" ? borderLeftColor Sol.magenta
    ".commit" ? borderLeftColor Sol.blue

    pre ?
      do background Sol.base2
         padding (px 5) (px 5) (px 5) (px 5)

    "button.blob" ?
      do background   Sol.violet
         color        Sol.base3
         borderRadius (px 0) (px 5) (px 5) (px 5)

    "button.dir" ?
      do background  Sol.magenta
         color       Sol.base3
         borderRadius (px 0) (px 5) (px 5) (px 5)

    "button.commit" ?
      do background  Sol.blue
         color       Sol.base3
         borderRadius (px 0) (px 5) (px 5) (px 5)
