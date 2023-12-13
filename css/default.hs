-- | Generate CSS for my website
--
-- As I myself know very little about CSS, I mostly just follow
-- https://www.w3schools.com/css/css_website_layout.asp
module Main (main) where

import Clay

main :: IO ()
main = putCss defaultCss

defaultCss :: Css
defaultCss = do
  header ? do
    textAlign (alignSide sideCenter)
    padding (px 20) (px 20) (px 20) (px 20)

  nav ? do
    overflow hidden
    background black
    a ? do
      float floatLeft
      display block
      color white
      textAlign (alignSide sideCenter)
      padding (px 14) (px 14) (px 16) (px 14)
    a # hover ? do
      background grey
      color black
