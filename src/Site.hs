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
import           Data.List                      ( isSuffixOf
                                                , intersperse
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Char                      ( toLower )
--------------------------------------------------------------------------------
import           Hakyll
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
--------------------------------------------------------------------------------
import           Templates                      ( Templet(..)
                                                , fromTemplet
                                                )
--------------------------------------------------------------------------------

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
            let blogsContext =
                    listField "blogs" (blogContext tags) (pure blogs)
            makeItem ""
                >>= applyTemplets
                        [Tags', Layout]
                        (blogsContext <> tagsContext tags <> defaultContext)
                >>= cleanIndexUrls

    match ("_temp/blogs/*" .||. "_temp/blogs/*/*") $ do
        route cleanRouteFromTemp
        compile
            $   pandocCompiler
            >>= applyTemplets [Blog, Layout] (authorx <> blogContext tags)
            >>= relativizeUrls
            >>= cleanIndexUrls

    create ["index.html"] $ do
        route idRoute
        compile $ do
            let context = nTitle <> defaultContext
            makeItem [] >>= applyTemplets [Layout] context >>= relativizeUrls

    create ["styles/main.css"] $ do
        route idRoute
        compile $ makeItem [] >>= withItemBody
            (unixFilter "stack" ["exec", "style"])

--------------------------------------------------------------------------------
-- | Context
nTitle :: Context a
nTitle = constField "title" "Nasy Land"

blogContext :: Tags -> Context String
blogContext tags =
    tagsContext tags <> dateField "date" "%B %e, %Y" <> defaultContext

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
