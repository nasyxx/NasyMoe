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
import           Control.Applicative            ( empty )
import           Control.Monad                  ( zipWithM_ )
import           Data.Char                      ( isAscii
                                                , toLower
                                                )
import           Data.List                      ( isPrefixOf
                                                , isSuffixOf
                                                )
import           Data.List.Split                ( splitOn )
--------------------------------------------------------------------------------
import           Hakyll                  hiding ( pandocCompiler )
--------------------------------------------------------------------------------
import           System.FilePath.Posix          ( joinPath
                                                , splitDirectories
                                                , takeBaseName
                                                , takeDirectory
                                                , takeFileName
                                                , (</>)
                                                )
import           Text.Blaze.Html                ( toHtml
                                                , toValue
                                                , (!)
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.Pandoc                    ( Extension(..)
                                                , HTMLMathMethod(KaTeX)
                                                , extensionsFromList
                                                , writerExtensions
                                                , writerHTMLMathMethod
                                                , writerHighlightStyle
                                                )
import           Text.Pandoc.Highlighting       ( haddock )
--------------------------------------------------------------------------------
import           Templates                      ( Templet(..)
                                                , fromTemplet
                                                )
--------------------------------------------------------------------------------
-- Main
config :: Configuration
config = defaultConfiguration { destinationDirectory = "build" }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Nasy Land"
    , feedDescription = "The place to plant flower, feed fish and chat -- Blog."
    , feedAuthorName  = "Nasy"
    , feedAuthorEmail = "nasyxx+nasymoe@gmail.com"
    , feedRoot        = "https://nasy.moe"
    }

main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match ("articles/*.org" .||. "articles/*/*.org" .||. "articles/*/*/*.org")
        $ do
              route tempRoute
              compile $ getResourceString >>= orgCompiler

    tags <- buildTags blogPattern (fromCapture "tags/*")

    tagsRules tags $ \tag pat -> do
        route cleanRoute
        compile $ do
            blogs <- recentFirst =<< loadAll pat
            let blogsContext = listField
                    "blogs"
                    (blogContext tags <> defaultContext)
                    (pure blogs)
                tag'         = "Tag: " ++ tag
            makeItem ""
                >>= applyTemplets
                        [Blogs, Layout]
                        (  constField "tag" tag'
                        <> blogsContext
                        <> tagsContext tags
                        <> defaultContext
                        )
                >>= relativizeUrls
                >>= cleanIndexUrls
                >>= cleanIndexHtmls

    create ["tags/index.html"] $ do
        route idRoute
        compile $ do
            let context = tagCloudField "cloud" 80 125 tags <> defaultContext
            makeItem []
                >>= applyTemplets [Cloud, Layout] context
                >>= relativizeUrls
                >>= cleanIndexUrls
                >>= cleanIndexHtmls

    create ["index.html", "blogs/index.html"] $ do
        route idRoute
        compile $ do
            blogs <-
                recentFirst =<< loadAll
                    (complement "_temp/articles/About.org" .&&. blogPattern)
            let context =
                    listField "blogs"
                              (blogContext tags <> defaultContext)
                              (pure blogs)
                        <> nTitle
                        <> defaultContext
            makeItem []
                >>= applyTemplets [Blogs, Layout] context
                >>= relativizeUrls
                >>= cleanIndexUrls
                >>= cleanIndexHtmls

    match blogPattern $ do
        route cleanRouteFromTemp
        compile $ do
            let context = authorx <> blogContext tags <> defaultContext
            pandocCompiler
                >>= applyTemplets [Blog] context
                >>= saveSnapshot "contents"
                >>= applyTemplets [Layout] context
                >>= relativizeUrls
                >>= cleanIndexUrls
                >>= cleanIndexHtmls

    zipWithM_
        (\address render -> create [address] $ do
            route idRoute
            compile
                $   loadAllSnapshots blogPattern "contents"
                >>= recentFirst
                >>= feedCompiler render
        )
        ["atom.xml", "rss.xml"]
        [renderAtom, renderRss]


    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            blogs <- recentFirst =<< loadAll blogPattern
            let context =
                    constField "root" "https://nasy.moe/"
                        <> listField
                               "blogs"
                               (  constField "root" "https://nasy.moe"
                               <> dateField "date" "%Y-%m-%d"
                               <> defaultContext
                               )
                               (pure blogs)
                        <> defaultContext
            makeItem []
                >>= applyTemplets [Sitemap] context
                >>= cleanIndexUrls
                >>= cleanIndexHtmls

    create ["CNAME"] $ do
        route idRoute
        compile $ makeItem ("nasy.moe\n" :: String)

    create ["README.org"] $ do
        route idRoute
        compile $ makeItem readme

    create ["styles/main.css"] $ do
        route idRoute
        compile $ makeItem [] >>= withItemBody
            (unixFilter "stack" ["exec", "style"])
  where
    readme :: String
    readme
        = "* Nasy Personal Blog\n\n\
             \+ Address: https://nays.moe\n\
             \+ Source: https://github.com/nasyxx/nasyxx.github.io\n"
    blogPattern :: Pattern
    blogPattern =
        "_temp/articles/*.org"
            .||. "_temp/articles/*/*.org"
            .||. "_temp/articles/*/*/*.org"

--------------------------------------------------------------------------------
-- | Context
nTitle :: Context a
nTitle = constField "title" "Nasy Land"

blogContext :: Tags -> Context String
blogContext tags = tagsContext tags <> dateField "date" "%B %e, %Y"

tagsContext :: Tags -> Context a
tagsContext = tagsFieldWith getTags (simpleLink "tags-li" "🏷") mconcat "tags"

authorx :: Context a
authorx = functionField "authorx"
    $ \args _ -> pure (if args == ["Nasy"] then "hide" else "")

descContext :: Context String
descContext = field "description" $ \i -> do
    metadata <- getMetadata $ itemIdentifier i
    maybe empty pure $ lookupString "summary" metadata
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
        (joinPath . tail . tail . splitDirectories . takeDirectory) p
            </> (toUrlString . takeBaseName) p
            </> "index.html"
        where p = toFilePath ident


-- | Clean Route.
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident =
        takeDirectory p </> takeBaseName p </> "index.html"
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
-- | Compilers

-- | From org get metadatas.
orgCompiler :: Item String -> Compiler (Item String)
orgCompiler = pure . fmap (\s -> (metadatasToStr . orgMetadatas) s ++ s)

orgMetadatas :: String -> [String]
orgMetadatas = map (format . lower . clean) . takeWhile (/= "") . lines
  where
    clean = concat . splitOn "#+"
    lower s = (map toLower . takeWhile (/= ':')) s ++ dropWhile (/= ':') s
    format xs
        | -- drop weekday str. 2018-05-03 Thu -> 2018-05-03
          "date" `isPrefixOf` xs
        = take 16 . concat . splitOn ">" . concat . splitOn "<" $ xs
        | otherwise
        = xs

metadatasToStr :: [String] -> String
metadatasToStr = ("----------\n" ++) . (++ "----------\n") . unlines

-- | My Custom Pandoc Compiler
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


-- | Atom and RSS XML feeds compiler
feedCompiler :: (FeedConfiguration -> Context String -> t) -> t
feedCompiler render = render
    feedConfiguration
    (dateField "date" "%Y-%m-%dT%H:%M:%SZ" <> descContext <> defaultContext)

--------------------------------------------------------------------------------
-- | Establish url string.
toUrlString :: String -> String
toUrlString = foldr shortit [] . filter isAscii . map (toLower . repl)
  where
    repl ' ' = '-'
    repl c   = c
    shortit a []        = [a]
    shortit a s@(b : _) = if a == b && a == '-' then s else a : s


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
simpleLink :: H.AttributeValue -> String -> String -> Maybe FilePath -> Maybe H.Html
simpleLink _ _ _ Nothing = Nothing
simpleLink cstr pref str (Just filepath) =
    Just
        $ H.li
        ! A.class_ cstr
        $ H.a
        ! A.href (toValue $ toUrl filepath)
        $ toHtml (pref ++ str)
