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
module Templates (Templet(..), fromTemplet, simpleLink) where
--------------------------------------------------------------------------------
import           Control.Monad                  ( forM_
                                                , zipWithM_
                                                )
import           Data.Char                      ( toLower )
import           Data.List                      ( zipWith4 )
--------------------------------------------------------------------------------
import           Hakyll                         ( Compiler
                                                , Template
                                                , compileTemplateItem
                                                , makeItem
                                                , toUrl
                                                )
import           Text.Blaze.Html                ( toHtml
                                                , toValue
                                                , (!)
                                                )
import           Text.Blaze.Html.Renderer.Pretty
                                                ( renderHtml )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.Blaze.Internal            ( customAttribute
                                                , customParent
                                                , preEscapedText
                                                )
--------------------------------------------------------------------------------

data Templet = Layout | Blog | Blogs | Cloud | Sitemap

--------------------------------------------------------------------------------
-- Helper
fromTemplet :: Templet -> Compiler Template
fromTemplet = (compileTemplateItem =<<) . makeItem . renderHtml . \case
    Layout  -> layout
    Blog    -> blog
    Blogs   -> blogs
    Cloud   -> cloud
    Sitemap -> sitemap


simpleLink :: H.AttributeValue -> String -> Maybe FilePath -> Maybe H.Html
simpleLink _ _ Nothing = Nothing
simpleLink cstr str (Just filepath) =
    Just
        $ H.li
        ! A.class_ cstr
        $ H.a
        ! A.href (toValue $ toUrl filepath)
        $ toHtml ("#" ++ str)

--------------------------------------------------------------------------------
-- Templates
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
                      ! A.href "/styles/main.css"
                      ! A.type_ "text/css"
                      ! A.media "screen"

                  googleAnalytics
              H.body $ do
                  H.header ! A.id "header" ! A.class_ "header" $ do
                      H.h1 $ H.a ! A.title "home" ! A.href "/" $ "Nasy Land"
                      nav
                  H.main ! A.id "main" ! A.class_ "main" $ "$body$"
                  H.footer ! A.id "footer" ! A.class_ "footer" $ do
                      H.header
                          ! customAttribute "data-aos"          "zoom-out-up"
                          ! customAttribute "data-aos-duration" "400"
                          ! customAttribute "data-aos-anchor-placement"
                                            "top-bottom"
                          $ do
                                nav
                                friendLinks
                      H.section ! A.id "copyright" $ H.p "Copyright © 2019 Nasy"
                  aos


blog :: H.Html
blog = H.article ! A.class_ "blog" $ do
    H.header ! A.class_ "blog-header" $ do
        H.h2 ! A.class_ "center-title" $ "$title$"
        metas
    H.section ! A.class_ "blog-section" $ "$body$"
    -- H.footer $ H.section ! A.id "comment" $ ""
    -- H.script ! A.src "https://unpkg.com/vue/dist/vue.runtime.min.js" $ ""
    -- vssue
    -- H.script
    --     "$if(comment)$\n\
    --   \const comment = \"$comment$\"\n\
    --   \$else$\n\
    --   \const comment = \"$title$\"\n\
    --   \$endif$\n\
    --   \$if(ctitle)$\n\
    --   \const ctitle = \"$ctitle$\"\n\
    --   \$else$\n\
    --   \const ctitle = \"$title$\"\n\
    --   \$endif$\n\
    --   \new Vue({\n\
    --   \  el: '#comment',\n\
    --   \  render: h => h('Vssue', {\n\
    --   \    props: {\n\
    --   \      title: ctitle,\
    --   \      options: {\n\
    --   \        state: \"Nasy\",\n\
    --   \        labels: [\"comment\", comment],\n\
    --   \        prefix: \"\",\n\
    --   \        owner: \"nasyxx\",\n\
    --   \        repo: \"comments\",\n\
    --   \        clientId: \"cb5605d5ea28ce5ba8d2\",\n\
    --   \        clientSecret: \"942e0d9f31ea8d7d30845fa26bc51ed6551153d8\",\n\
    --   \        autoCreateIssue: false,\n\
    --   \        issueContent: url => \"Comment of: \\n\" + document.URL,\n\
    --   \      },\n\
    --   \    }\n\
    --   \  })\n\
    --   \})"


blogs :: H.Html
blogs = H.section ! A.class_ "blogs-list" $ do
    "$if(subtitle)$"
    H.h2 ! A.class_ "center-title" $ "$subtitle$"
    "$endif$"
    "$for(blogs)$"
    H.section
        ! customAttribute "data-aos"                  "fade-up"
        ! customAttribute "data-aos-duration"         "1000"
        ! customAttribute "data-aos-anchor-placement" "center-bottom"
        $ do
              H.a ! A.href "$url$" ! A.title "$title$" $ H.h2 "$title$"
              metas
    "$endfor$"


cloud :: H.Html
cloud = H.section ! A.id "tags-cloud" $ "$cloud$"


sitemap :: H.Html
sitemap = do
    preEscapedText "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    customParent "urlset"
        ! customAttribute "xmlns" "http://www.sitemaps.org/schemas/sitemap/0.9"
        ! customAttribute "xmlns:news"
                          "http://www.google.com/schemas/sitemap-news/0.9"
        ! customAttribute "xmlns:xhtml" "http://www.w3.org/1999/xhtml"
        ! customAttribute "xmlns:mobile"
                          "http://www.google.com/schemas/sitemap-mobile/1.0"
        ! customAttribute "xmlns:image"
                          "http://www.google.com/schemas/sitemap-image/1.1"
        ! customAttribute "xmlns:video"
                          "http://www.google.com/schemas/sitemap-video/1.1"
        $ do
              url $ do
                  loc "$root$"
                  priority "1.0"
              "$for(blogs)$"
              url $ do
                  loc "$root$$url$"
                  "$if(date)$"
                  customParent "lastmod" "$date$"
                  "$endif$"
                  priority "0.8"
              "$endfor$"
  where
    url      = customParent "url"
    loc      = customParent "loc"
    priority = customParent "priority"

--------------------------------------------------------------------------------
-- Partials
nav :: H.Html
nav = H.nav ! A.class_ "nasy-links" $ H.ul $ sequence_ $ zipWith4
    (\h t c cl -> H.li ! A.class_ cl $ H.a ! A.href h ! A.title t $ c)
    [ "/"
    , "/about#About"
    , "/tags"
    , "https://emacs.nasy.moe"
    , "mailto:nasyxx+nasymoe@gmail.com"
    , "/atom.xml"
    , "/rss.xml"
    ]
    [ "home"
    , "about"
    , "tags"
    , "emacs configuration"
    , "email me"
    , "atom feed"
    , "rss feed"
    ]
    [ "Home"
    , "About"
    , "Tags"
    , "Emacs Configuration"
    , "Email Me"
    , "Atom Feed"
    , "RSS Feed"
    ]
    ["", "", "", "", "header-hide", "header-hide", "header-hide"]



friendLinks :: H.Html
friendLinks = H.nav ! A.class_ "friend-links" $ H.ul $ zipWithM_
    (\h c ->
        H.li $ H.a ! A.href h ! A.title (toValue (map toLower c)) $ toHtml c
    )
    [ "https://laobubu.net"
    , "https://yuki.yuki233.com"
    , "https://mitsuhachan.ml"
    , "https://blog.zsakvo.cc"
    , "https://daisuke.moe"
    , "http://desvl.xyz/"
    ]
    ["Laobubu", "初雪", "三葉的喵窝", "水水的小窝", "白兔", "Desvl's Blog"]


metas :: H.Html
metas = H.section ! A.class_ "metas" $ do
    "$if(cats)$"
    H.section ! A.class_ "meta cats" $ "$cats$"
    "$endif$"
    forM_ ["author", "date", "summary"] $ \m -> do
        toHtml $ "$if(" ++ m ++ ")$"
        H.section ! A.class_ (cc m) $ H.p $ toHtml $ "$" ++ m ++ "$"
        "$endif$"
    "$if(tags)$"
    H.section ! A.class_ "meta tags" $ H.ul "$tags$"
    "$endif$"
  where
    cc "author" = "author $authorx(author)$"
    cc m'       = toValue m'

--------------------------------------------------------------------------------
-- Special
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
        ! A.src "//instant.page/1.2.2"
        ! A.type_ "module"
        ! customAttribute
              "Integrity"
              "sha384-2xV8M5griQmzyiY3CDqh1dn4z3llDVqZDqzjzcY+jCBCk/a5fXJmuZ/40JJAPeoU"
        $ ""


vssue :: H.Html
vssue = do
    H.link
        ! A.rel "stylesheet"
        ! A.href "https://unpkg.com/vssue/dist/vssue.min.css"
        ! A.type_ "text/css"
        ! A.media "screen"
    H.script ! A.src "https://unpkg.com/vssue/dist/vssue.github.min.js" $ ""


aos :: H.Html
aos = do
    H.link
        ! A.rel "stylesheet"
        ! A.href "https://unpkg.com/aos/dist/aos.css"
        ! A.type_ "text/css"
        ! A.media "screen"
    H.script ! A.src "https://unpkg.com/aos/dist/aos.js" $ ""
    H.script "AOS.init()"
