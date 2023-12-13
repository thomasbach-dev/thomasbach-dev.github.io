module Main (main) where

import qualified GHC.IO.Encoding as E
import Hakyll

config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "docs"
    }

--------------------------------------------------------------------------------
main :: IO ()
main = do
  E.setLocaleEncoding E.utf8
  hakyllWith config $ do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler
    match "css/*.hs" $ do
      route $ setExtension "css"
      compile $ getResourceString >>= withItemBody (unixFilter "runghc" [])
    match "static/*" $ do
      route idRoute
      compile copyFileCompiler
    match (fromList ["about.org", "contact.org"]) $ do
      route $ setExtension "html"
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls
    match (fromGlob "posts/20*.org") $ do
      route $ setExtension "html"
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "templates/post.html" postCtx
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls
    create ["archive.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let archiveCtx =
              listField "posts" postCtx (return posts)
                <> constField "title" "Archives"
                <> defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls

    match "index.org" $ do
      route $ setExtension "html"
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let indexCtx =
              listField "posts" postCtx (return posts)
                <> constField "title" "Home"
                <> defaultContext

        pandocCompiler
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    <> defaultContext
