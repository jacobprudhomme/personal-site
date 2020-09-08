import Clay
import Data.List.NonEmpty (fromList)
import Data.Semigroup (sconcat)
import Prelude hiding (div, span)

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


styles :: Css
styles = reset

main :: IO ()
main = T.putStr $ renderWith compact [] styles
