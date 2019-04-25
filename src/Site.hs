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
import           Data.Char                      ( toLower )
import           Data.List                      ( isSuffixOf )
import           Data.List.Split                ( splitOn )
--------------------------------------------------------------------------------
import           Hakyll                  hiding ( pandocCompiler )
--------------------------------------------------------------------------------
import           Network.HTTP.Base              ( urlEncode )
import           System.FilePath.Posix          ( takeBaseName
                                                , takeFileName
                                                , takeDirectory
                                                , splitDirectories
                                                , joinPath
                                                , (</>)
                                                )
import           Text.Blaze.Html                ( toHtml
                                                , toValue
                                                , (!)
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.Pandoc                    ( writerExtensions
                                                , writerHTMLMathMethod
                                                , writerHighlightStyle
                                                , Extension(..)
                                                , extensionsFromList
                                                , HTMLMathMethod(KaTeX)
                                                )
import           Text.Pandoc.Highlighting       ( haddock )
--------------------------------------------------------------------------------
import           Templates                      ( Templet(..)
                                                , fromTemplet
                                                )
--------------------------------------------------------------------------------
-- Main
config :: Configuration
config = defaultConfiguration { destinationDirectory = "public" }

main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match ("blogs/*.org" .||. "blogs/*/*.org") $ do
        route tempRoute
        compile $ getResourceString >>= fromOrgCompiler

    tags <- buildTags ("_temp/blogs/*.org" .||. "_temp/blogs/*/*.org")
                      (fromCapture "tags/*")

    tagsRules tags $ \tag pat -> do
        route cleanRoute
        compile $ do
            blogs <- recentFirst =<< loadAll pat
            let blogsContext = listField
                    "blogs"
                    (blogContext tags <> defaultContext)
                    (pure blogs)
            makeItem ""
                >>= applyTemplets
                        [Toc, Layout]
                        (blogsContext <> tagsContext tags <> defaultContext)
                >>= relativizeUrls
                >>= cleanIndexHtmls

    match ("_temp/blogs/*" .||. "_temp/blogs/*/*") $ do
        route cleanRouteFromTemp
        compile
            $   pandocCompiler
            >>= applyTemplets
                    [Blog, Layout]
                    (authorx <> blogContext tags <> defaultContext)
            >>= relativizeUrls
            >>= cleanIndexUrls

    create ["index.html"] $ do
        route idRoute
        compile $ do
            blogs <- recentFirst
                =<< loadAll ("_temp/blogs/*.org" .||. "_temp/blogs/*/*.org")
            let context =
                    listField "blogs"
                              (blogContext tags <> defaultContext)
                              (pure blogs)
                        <> nTitle
                        <> defaultContext
            makeItem []
                >>= applyTemplets [Toc, Layout] context
                >>= relativizeUrls
                >>= cleanIndexUrls

    create ["styles/main.css"] $ do
        route idRoute
        compile $ makeItem [] >>= withItemBody
            (unixFilter "stack" ["exec", "style"])

--------------------------------------------------------------------------------
-- | Context
nTitle :: Context a
nTitle = constField "title" "Nasy Land"

blogContext :: Tags -> Context String
blogContext tags = tagsContext tags <> dateField "date" "%B %e, %Y"

tagsContext :: Tags -> Context a
tagsContext = tagsFieldWith getTags (simpleLink "tags-li") mconcat "tags"

authorx :: Context a
authorx = functionField "authorx"
    $ \args _ -> pure (if args == ["Nasy"] then "hide" else "")
--------------------------------------------------------------------------------
-- | Temp Route
tempRoute :: Routes
tempRoute = customRoute tempRoute'
  where
    tempRoute' i = ".." </> "_temp" </> takeDirectory p </> takeFileName p
        where p = toFilePath i


cleanRouteFromTemp :: Routes
cleanRouteFromTemp = customRoute createIndexRoute
  where
    createIndexRoute ident =
        (joinPath . tail . splitDirectories . takeDirectory) p
            </> (urlEString . takeBaseName) p
            </> "index.html"
        where p = toFilePath ident


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


--------------------------------------------------------------------------------
-- | From org get metadatas.
fromOrgCompiler :: Item String -> Compiler (Item String)
fromOrgCompiler = pure . fmap (\s -> (metadatasToStr . orgMetadatas) s ++ s)

orgMetadatas :: String -> [String]
orgMetadatas = map (format . lower . clean) . takeWhile (/= "") . lines
  where
    clean = concat . splitOn ">" . concat . splitOn "#+" . concat . splitOn "<"
    lower s = (map toLower . takeWhile (/= ':')) s ++ dropWhile (/= ':') s
    format xs@('d' : 'a' : 't' : 'e' : _) = take 16 xs  -- drop weekday str. 2018-05-03 Thu -> 2018-05-03
    format a                              = a

metadatasToStr :: [String] -> String
metadatasToStr = ("----------\n" ++) . (++ "----------\n") . unlines

--------------------------------------------------------------------------------
-- | Compiler
pandocCompiler :: Compiler (Item String)
pandocCompiler = pandocCompilerWith defaultHakyllReaderOptions writerOptions
  where
    extensions =
        [ -- Math
          Ext_tex_math_dollars
        , Ext_tex_math_double_backslash
        , Ext_latex_macros
          -- Org
        , Ext_citations
        , Ext_auto_identifiers
          -- Html
        , Ext_native_divs
        , Ext_native_spans
          -- Others
        , Ext_yaml_metadata_block
        , Ext_table_captions
        , Ext_implicit_figures
        , Ext_simple_tables
        , Ext_multiline_tables
        , Ext_grid_tables
        , Ext_pipe_tables
        , Ext_citations
        , Ext_literate_haskell
        , Ext_fancy_lists
        , Ext_smart
        ]
    writerOptions = defaultHakyllWriterOptions
        { writerHTMLMathMethod = KaTeX ""
        , writerExtensions     = extensionsFromList extensions
        , writerHighlightStyle = Just haddock
        }

--------------------------------------------------------------------------------
-- | Custom Helper
replaceSpace :: String -> String
replaceSpace = map repl
  where
    repl ' ' = '-'
    repl c   = c

-- | Encode url
-- I am not really happy with this, though gitalk makes me have to do like this.
urlEString :: String -> String
urlEString = urlEncode . replaceSpace

-- | Apply templates
applyTemplets
    :: [Templet] -> Context String -> Item String -> Compiler (Item String)
applyTemplets ts = applyTemplates (map fromTemplet ts)

applyTemplates
    :: [Template] -> Context String -> Item String -> Compiler (Item String)
applyTemplates []  _       _    = error "Need a template"
applyTemplates [t] context item = applyTemplate t context item
applyTemplates (t : ts) context item =
    applyTemplate t context item >>= applyTemplates ts context

--------------------------------------------------------------------------------
-- | Templates
simpleLink :: H.AttributeValue -> String -> Maybe FilePath -> Maybe H.Html
simpleLink _ _ Nothing = Nothing
simpleLink cstr str (Just filepath) =
    Just
        $ H.li
        ! A.class_ cstr
        $ H.a
        ! A.href (toValue $ toUrl filepath)
        $ toHtml ("#" ++ str)
