{-# LANGUAGE OverloadedStrings #-}
module Css where


-- | Css gen'd via this
import Clay
import Clay.Color
import qualified Solarized as Sol


myCSS :: Css
myCSS = do
    body ?
      do background  Sol.base2
         color       Sol.base02
         border      dashed (px 2) Sol.violet
    ".entity" ?
      do background  Sol.base2
         color       Sol.base02
         ".blob" ?
           border      solid (px 2) Sol.violet
         ".dir" ?
           border      solid (px 2) Sol.magenta
         ".commit" ?
           border      solid (px 2) Sol.blue
    ".hashlink" ?
      do background  Sol.base2
         color       Sol.base02
         ".blob" ?
           border      dashed (px 2) Sol.violet
         ".dir" ?
           border      dashed (px 2) Sol.magenta
         ".commit" ?
           border      dashed (px 2) Sol.blue
