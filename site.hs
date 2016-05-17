--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Char
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Monoid
import           Data.Time
import           Debug.Trace
import           Hakyll
import           System.FilePath
-- fixing issue with file encodin
-- https://groups.google.com/forum/#!topic/hakyll/jrGATyI1omI/discussion

import           GHC.IO.Encoding

escaped :: String -> Context String
escaped fieldName = field fieldName $ \item -> do
    content <- getMetadataField (itemIdentifier item) fieldName
    case content of
      Just s  -> return $ escapeHtml s
      Nothing -> return ""

--------------------------------------------------------------------------------
main :: IO ()
main = do
  setLocaleEncoding utf8
  hakyll $ do
    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "_posts/*" $ do
        compile $ pandocCompiler

    match "_includes/**" $ do
        route   stripPrefix
        compile copyFileCompiler

    create ["index.html"] $ do
        route idRoute
        compile $ do
          (postsMenu,postsBody) <- postList chronological
          item <- makeItem postsBody
          menuitem <- makeItem postsMenu
          index <- loadAndApplyTemplate "templates/index.html" (homeCtx postsMenu) item
          relativizeUrls index

    create ["css/main.css"] $ do
        route idRoute
        compile $ do
          em <- makeItem ""
          posts <- chronological =<< loadAll "_posts/*"
          let listOfPosts = listField "posts" postCtx (return posts) <> cssContext
          loadAndApplyTemplate "templates/main.css" listOfPosts em

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
homeCtx :: String -> Context String
homeCtx menu =
  constField "menu" menu  <>
  defaultContext

postCtx :: Context String
postCtx =
  escaped   "title"            <>
  field     "post-id" slugify     <>
  dateField "date" "%B %e, %Y" <>
  defaultContext

cssContext :: Context String
cssContext =
  constField     "navborder"  "3"        <>
  constField     "navborder_active"  "6" <>
  defaultContext

slugify :: Item String -> Compiler String
slugify item = do
  metadata <- getMetadata (itemIdentifier item)
  let slug  = ("post-" ++) . dropExtension . takeFileName . toFilePath
  return $ slug (itemIdentifier item)

--------------------------------------------------------------------------------
postList sortFilter = do
  posts     <- sortFilter =<< loadAll "_posts/*"
  itemTpl   <- loadBody "templates/post-item.html"
  menuTpl   <- loadBody "templates/post-menu-item.html"
  postsBody <- applyTemplateList itemTpl postCtx posts
  postsMenu <- applyTemplateList menuTpl postCtx posts
  return (postsMenu, postsBody)

stripPrefix :: Routes
stripPrefix = customRoute stripPrefixRoute
  where
    stripPrefixRoute ident = dropFirstDirectory p </> takeFileName p
      where
        p = toFilePath ident
    dropFirstDirectory = joinPath . tail . splitPath . takeDirectory

