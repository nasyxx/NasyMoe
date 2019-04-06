{-
 Excited without bugs, have fun ("▔□▔)/hi~♡ Nasy.
 ------------------------------------------------
 |             *         *
 |                  .                .
 |           .
 |     *                      ,
 |                   .
 |
 |                               *
 |          |\___/|
 |          )    -(             .              ·
 |         =\ -   /=
 |           )===(       *
 |          /   - \
 |          |-    |
 |         /   -   \     0.|.0
 |  NASY___\__( (__/_____(\=/)__+1s____________
 |  ______|____) )______|______|______|______|_
 |  ___|______( (____|______|______|______|____
 |  ______|____\_|______|______|______|______|_
 |  ___|______|______|______|______|______|____
 |  ______|______|______|______|______|______|_
 |  ___|______|______|______|______|______|____

There are more things in heaven and earth, Horatio, than are dreamt.
   -- From "Hamlet"
--------------------------------------------------------------------------------

-}

--------------------------------------------------------------------------------
-- |
-- Filename   : Site.hs
-- Project    : src
-- Author     : Nasy
-- License    : GPL-3.0+
--
-- Maintainer : Nasy <nasyxx+haskell@gmail.com>
--
--
--
--------------------------------------------------------------------------------
module Main (main) where
--------------------------------------------------------------------------------
import           Data.List                      ( isSuffixOf )
--------------------------------------------------------------------------------
import           Hakyll                         ( Compiler
                                                , Configuration
                                                , Item
                                                , Routes
                                                , compile
                                                , constField
                                                , create
                                                , customRoute
                                                , defaultConfiguration
                                                , defaultContext
                                                , destinationDirectory
                                                , hakyllWith
                                                , idRoute
                                                , listField
                                                , loadAll
                                                , match
                                                , recentFirst
                                                , replaceAll
                                                , route
                                                , templateBodyCompiler
                                                , toFilePath
                                                , copyFileCompiler
                                                , withUrls
                                                , makeItem
                                                , loadAndApplyTemplate
                                                , pandocCompiler
                                                , relativizeUrls
                                                , Context
                                                , withItemBody
                                                , unixFilter
                                                )
--------------------------------------------------------------------------------
import           Network.HTTP.Base              ( urlEncode )
import           System.FilePath.Posix          ( takeBaseName
                                                , takeDirectory
                                                , takeFileName
                                                , (</>)
                                                )
--------------------------------------------------------------------------------


config :: Configuration
config = defaultConfiguration { destinationDirectory = "public" }

main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "templates/*" $ compile templateBodyCompiler
    match "templates/partials/*" $ compile templateBodyCompiler

    match "blogs/*" $ do
        route cleanRoute
        compile
            $   pandocCompiler
            >>= tBlog defaultContext
            >>= tLayout defaultContext
            >>= relativizeUrls
            >>= cleanIndexUrls

    create ["index.html"] $ do
        route idRoute
        compile $ do
            let context = nTitle <> defaultContext
            makeItem []
                >>= tLayout context
                >>= relativizeUrls
                >>= cleanIndexUrls

    create ["styles/main.css"] $ do
        route idRoute
        compile $ makeItem [] >>= withItemBody
            (unixFilter "stack" ["exec", "style"])
  where
    tLayout = loadAndApplyTemplate "templates/layout.html"
    tBlog   = loadAndApplyTemplate "templates/blog.html"

--------------------------------------------------------------------------------
-- | Context
nTitle :: Context a
nTitle = constField "title" "Nasy Land"

--------------------------------------------------------------------------------
-- | Clean Route.
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident =
        takeDirectory p </> (urlEString . takeBaseName) p </> "index.html"
        where p = toFilePath ident


cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = pure . fmap (withUrls cleanIndex)


cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = pure . fmap (replaceAll pattern' replacement)
  where
    pattern'    = "/index.html"
    replacement = const "/"


cleanIndex :: String -> String
cleanIndex url | idx `isSuffixOf` url = take (length url - length idx) url
               | otherwise            = url
    where idx = "index.html"

-- | Custom Funntions
replaceSpace :: String -> String
replaceSpace = map repl
  where
    repl ' ' = '-'
    repl c   = c

-- | I am not really happy with this, though gitalk makes me have to do like this.
urlEString :: String -> String
urlEString = urlEncode . replaceSpace
