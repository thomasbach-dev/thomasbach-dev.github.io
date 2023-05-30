-- |

module Main (main) where

import Clay

main :: IO ()
main = putCss defaultCss

defaultCss :: Css
defaultCss = do
  nav ? do
    background lightcyan
    float floatLeft
    a ? do
      display block
      textAlign (alignSide sideRight)
