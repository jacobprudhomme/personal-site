{-# LANGUAGE OverloadedStrings #-}

import Clay
import Data.List.NonEmpty (fromList)
import Data.Semigroup (sconcat)
import Prelude hiding (div, span)

import qualified Clay.Flexbox as CF
import qualified Data.Text.Lazy.IO as T


-- CSS Reset
reset :: Css
reset = do
  elements1 ? theStyles
  html ? boxSizing inherit
  elements2 ? boxSizing inherit
  where
    elements1 = sconcat $ fromList [html, body, div, span, object, h1, h2, h3, h4, h5, h6, p, a, abbr, img, ol, ul, li]
    elements2 = sconcat $ fromList $ [Prelude.id, (# before), (# after)] <*> [star]
    theStyles = do
      borderWidth 0
      fontWeight inherit
      fontStyle inherit
      fontSize (pct 100)
      fontFamily [] [inherit]
      verticalAlign vAlignBaseline
      margin (px 0) (px 0) (px 0) (px 0)
      padding (px 0) (px 0) (px 0) (px 0)

-- CSS Variables
variables :: Css
variables = do
  html ? do
    fontFamily ["Georgia"] [serif]
    color "#333"
    backgroundColor "#fffffc"
    background $
      linearGradient (straight sideTop)
        [ ("#eee", 0)
        , ("#fff", 10)
        , ("#fff", 90)
        , ("#eee", 100)
        ]
    backgroundAttachment attachFixed
    height (pct 100)
    width (pct 100)
    zIndex (-10)

    ".dark" & do
      backgroundColor "#333"
      background $
        linearGradient (straight sideTop)
          [ ("#2a2a2a", 0)
          , ("#333", 10)
          , ("#333", 90)
          , ("#2a2a2a", 100)
          ]
      backgroundAttachment attachFixed
      height (pct 100)
      width (pct 100)
      color "#ddd"

  body ? do
    display flex
    flexFlow column (CF.nowrap)
    justifyContent center


styles :: Css
styles = do
  reset
  variables

main :: IO ()
main = T.putStr $ renderWith compact [] styles
