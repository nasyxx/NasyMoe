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
-- Filename   : Templates.hs
-- Project    : templates
-- Author     : Nasy
-- License    : GPL-3.0+
--
-- Maintainer : Nasy <nasyxx+haskell@gmail.com>
--
--
--
--------------------------------------------------------------------------------
module Templates where
--------------------------------------------------------------------------------
import           Control.Monad                  ( zipWithM_ )
import           Data.Char                      ( toLower )
--------------------------------------------------------------------------------
import           Hakyll                         ( Template
                                                , readTemplate
                                                )
import           Text.Blaze.Html                ( (!)
                                                , toValue
                                                , toHtml
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.Blaze.Html.Renderer.Pretty
                                                ( renderHtml )
import           Text.Blaze.Internal            ( customAttribute )
--------------------------------------------------------------------------------
data Templet = Layout | Blog
--------------------------------------------------------------------------------
fromTemplet :: Templet -> Template
fromTemplet = readTemplate . renderHtml . \case
    Layout -> layout
    Blog   -> blog
    Blogs  -> blogs
    Tags'  -> tagsTemplate

--------------------------------------------------------------------------------
layout :: H.Html
layout = do
    H.docType
    H.html
        ! A.charset "utf-8"
        ! customAttribute "encoding" "utf-8"
        ! A.lang "zh-CN"
        ! customAttribute "language" "Chinese"
        ! customAttribute "prefix"   "og: http://ogp.me/ns#"
        $ do
              H.head $ do
                  H.meta ! A.charset "utf-8"
                  meta "language" "Chinese"

                  H.title "$title$"

                  mapM_
                      (uncurry meta)
                      [ ("author"        , "Nasy")
                      , ("owner"         , "Nasy")
                      , ("copyright"     , "© 2019 Nasy")
                      , ("og:site_name"  , "Nasy Land")
                      , ("og:title"      , "$title$")
                      , ("og:url"        , "https://nasy.moe")
                      , ("og:description", "Nasy 的花园，栽花养鱼闲聊的地方～")
                      , ("og:image"      , "https://nasy.moe/images/icon.png")
                      , ("og:type"       , "blog")
                      , ( "viewport"
                        , "height=device-height,width=device-width,initial-scale=1.0,maximum-scale=1.0,minimum-scale=1.0,user-scalable=no,minimal-ui"
                        )
                      , ("HandleFriendly"              , "True")
                      , ("MSThemeCompatible"           , "no")
                      , ("apple-mobile-web-app-capable", "yes")
                      , ( "apple-mobile-web-app-status-bar-style"
                        , "translucent black"
                        )
                      , ("msapplication-navbutton-color", "translucent black")
                      , ("mssmarttagspreventparsing"    , "true")
                      , ("theme-color"                  , "#ffeab6")
                      ]

                  H.meta
                      ! A.httpEquiv "Cache-Control"
                      ! A.content
                            "public,max-age=1800,max-stale,stale-while-revalidate=86400,stale-if-error=259200"
                      ! customAttribute "rem" "max-age=30minutes"
                  H.meta ! A.httpEquiv "Page-Enter" ! A.content
                      "RevealTrans(Duration=1.0,Transition=1)"
                  H.meta ! A.httpEquiv "Page-Exit" ! A.content
                      "RevealTrans(Duration=1.0,Transition=1)"
                  H.link
                      ! A.rel "stylesheet"
                      ! A.href "/style/main.css"
                      ! A.type_ "text/css"
                      ! A.media "screen"
                  googleAnalytics
              H.body $ do
                  H.header ! A.id "header" ! A.class_ "header" $ do
                      H.h1 $ H.a ! A.title "home" ! A.href "/" $ "Nasy Land"
                      nav
                  H.main ! A.id "main" ! A.class_ "main" $ "$body$"
                  H.footer ! A.id "header" ! A.class_ "footer" $ do
                      H.header $ do
                          nav
                          friendLinks
                      H.section ! A.id "copyright" $ H.p "Copyright © 2019 Nasy"


blog :: H.Html
blog = H.article ! A.class_ "blog" $ do
    H.header ! A.class_ "blog-header" $ do
        H.h2 "$title$"
        metas
    H.section ! A.class_ "blog-section" $ "$body$"


blogs :: H.Html
blogs = H.section ! A.class_ "blogs-list" $ H.ul $ do
    "$for(blogs)$"
    H.li $ H.a ! A.title "$title$" ! A.href "$url$" $ "$title$"
    "$endfor$"

--------------------------------------------------------------------------------
nav :: H.Html
nav = H.nav ! A.class_ "nasy-links" $ H.ul $ sequence_ $ zipWith3
    (\h t c -> H.li $ H.a ! A.href h ! A.title t $ c)
    ["/", "/about#About", "mailto:nasyxx+nasymoe@gmail.com"]
    ["home", "about", "email me"]
    ["Home", "About", "Email Me"]


friendLinks :: H.Html
friendLinks = H.nav ! A.class_ "friend-links" $ H.ul $ zipWithM_
    (\h c ->
        H.li $ H.a ! A.href h ! A.title (toValue (map toLower c)) $ toHtml c
    )
    [""]
    [""]

metas :: H.Html
metas = H.section ! A.class_ "metas" $ do
    mapM_
        (\m -> do
            toHtml $ "$if(" ++ m ++ ")$"
            H.section ! A.class_ (toValue m) $ H.p $ toHtml $ "$" ++ m ++ "$"
            "$endif$"
        )
        ["author", "date", "summary"]
    "$if(tags)$"
    H.section ! A.class_ "meta tags" $ H.ul "$tags$"
    "$endif$"

--------------------------------------------------------------------------------

meta :: H.AttributeValue -> H.AttributeValue -> H.Html
meta name content = H.meta ! A.name name ! A.content content


googleAnalytics :: H.Html
googleAnalytics = do
    H.script
        ! A.async ""
        ! A.src "https://www.googletagmanager.com/gtag/js?id=UA-102577027-1"
        $ ""
    H.script
        "window.dataLayer = window.dataLayer || [];\
        \function gtag(){dataLayer.push(arguments);}\
        \gtag('js', new Date());\
        \gtag('config', 'UA-102577027-1');"
    H.script
        ! A.src "//instant.page/1.1.0"
        ! A.type_ "module"
        ! customAttribute
              "Integrity"
              "sha384-EwBObn5QAxP8f09iemwAJljc+sU+eUXeL9vSBw1eNmVarwhKk2F9vBEpaN9rsrtp"
        $ ""
