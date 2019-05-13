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
-- Filename   : Style.hs
-- Project    : NasyMoe
-- Author     : Nasy
-- License    : GPL-3.0+
--
-- Maintainer : Nasy <nasyxx+haskell@gmail.com>
--
-- Style of https://nasy.moe
--
--------------------------------------------------------------------------------
module Main (main) where
--------------------------------------------------------------------------------
import           Prelude                 hiding ( rem
                                                , (**)
                                                , div
                                                , span
                                                )
--------------------------------------------------------------------------------
import           Control.Monad                  ( zipWithM_
                                                , forM_
                                                )
--------------------------------------------------------------------------------
import           Clay                    hiding ( fontColor
                                                , map
                                                , s
                                                )
import qualified Clay.Flexbox                  as CF
--------------------------------------------------------------------------------
-- Main
main :: IO ()
main =
    putCss
        $  specialB
        >> basic
        >> layout
        >> metas
        >> tags
        >> blogs
        >> blog
        >> codeHighlight
        >> comment
        >> mix

--------------------------------------------------------------------------------
-- Main Css
specialB :: Css
specialB = ".hide" ? display none

basic :: Css
basic = do
    -- | Quote style
    blockquote ? do
        flip (sym2 margin) 0 $ rem 1
        borderLeft solid (rem 1) quoteColor
        hover & borderLeftColor (darken 0.3 quoteColor)
        (borderTop <> borderBottom) solid (px 2) transparent
        star <? (marginTop <> marginBottom) nil

    -- | Paragraph style
    p ? do
        sym2 padding 0 $ rem 1

    -- | List Style
    ul ? do
        sym padding (px 1)
        (borderLeft <> borderRight) solid (rem 0.5) ulColor
        hover & (borderLeft <> borderRight) solid (rem 0.5) (darken 0.2 ulColor)
        (borderTop <> borderBottom) solid (px 1) transparent
    li ? do
        listStyleType none
        borderLeft solid (rem 0.5) liColor
        (borderTop <> borderBottom) solid (px 1) transparent
        flip (sym2 margin) 0 $ px 1
        sym2 padding 0 $ px 2

        hover & borderColor dliColor

        a <? display block

    -- | Text style
    -- Text Shadow
    zipWithM_
        (\e c -> e ? do
            textShadow (px 1) (px 2) (px 3) (setA 0.3 c)
            hover & textShadow (px 1) (px 2) (px 5) (setA 0.5 c)
        )
        ([p, a] ++ hs')
        ([fontColor, hyperColor] ++ hsc)
    (pre <> code) ? do
        textShadow (px 1) (px 2) (px 3) (setA 0.3 black)
        hover & textShadow (px 1) (px 2) (px 5) (setA 0.5 black)

    -- Text FontFamily
    (hs <> a <> p <> span <> textarea) ? fontFamily
        ["chalkboard", "comic sans", "pingfang sc", "meslo lg"]
        [sansSerif]
    (pre <> code <> ".sourceCode" <> ".sourceLine")
        ? (p <> a <> span)
        ? fontFamily
              ["Operator Mono SSm", "Operator Mono", "FiraCode", "hermit"]
              [monospace]

    -- Text Color
    (p <> span) ? color fontColor
    blockquote |> (p <> span) ? color (lighten 0.2 fontColor)
    a # "@href" ? do
        color hyperColor
        active & color hyperColorLight
        hover & color hyperColor
        visited & color hyperColorDark
    zipWithM_ (\e c -> e ? color c) hs' hsc

    -- | Everything Style
    star ? transition "all" 0.4 easeInOut 0


layout :: Css
layout = do
    body ? do
        -- | Body Box
        sym2 padding 0 $ rem 1
        sym margin auto

        -- | Color
        backgroundColor bgColor

        -- | Width & Height
        minHeight $ vh 100
        maxWidth $ px 840
        minWidth $ px 320 @-@ rem 2

        -- | Display
        display flex
        flexFlow column CF.nowrap
        alignItems stretch

        -- | Shadow
        boxShadow
            [ bsColor shadowColor $ shadowWithBlur nil nil (px 3)
            , bsInset . bsColor shadowColor $ shadowWithBlur 0 0 (px 3)
            ]
        hover & boxShadow
            [ bsColor shadowColor $ shadowWithBlur nil nil (px 5)
            , bsInset . bsColor shadowColor $ shadowWithBlur 0 0 (px 5)
            ]

    ("#header" <> "#footer" |> header) ? do
        backgroundColor $ setA 0.1 hbgColor
        hover & backgroundColor (setA 0.3 hbgColor)

    "#header" ? do
        display flex
        justifyContent spaceAround
        alignItems center

        ul ? do
            borderRightWidth nil
            marginLeft $ rem 1

        noUnderline

        h1 <? do
            a <? fontFamily ["Chalkduster"] [fantasy]
            paddingLeft (rem 1)

        nav <? minWidth (rem 8)

        sym borderRadius $ px 5
        borderBottom dashed (px 5) lineColor
        boxShadow [bsColor lineColor2 $ shadowWithBlur nil (px 2) (px 3)]
        hover & boxShadow
            [bsColor lineColor2 $ shadowWithBlur nil (px 2) (px 5)]

        ".header-hide" ? display none

    "#main" ? hover & backgroundColor (lighten 0.1 bgColor)

    "#footer" ? do
        marginTop $ rem 1
        display flex
        flexFlow column CF.nowrap
        flexGrow 1
        flexShrink 0
        justifyContent flexEnd

        noUnderline
        header <? do
            display flex
            borderTop dashed (px 5) lineColor
            sym borderRadius $ px 5
            justifyContent spaceAround

            boxShadow [bsColor lineColor2 $ shadowWithBlur nil (px 2) (px 3)]
            hover & boxShadow
                [bsColor lineColor2 $ shadowWithBlur nil (px 2) (px 5)]

    "#copyright" ? do
        fontFamily ["American Typewriter"] [cursive, serif]
        fontSizeCustom smaller
        textAlign center
        color "#d988bc"
        marginTop $ rem 1
        backgroundColor bgColor


metas :: Css
metas = do
    section # ".metas" ? do
        sym padding $ rem 1
        borderBottom dashed (px 2) lineColor
        sym borderRadius $ px 5
        backgroundColor $ lighten 0.1 bgColor
        hover & backgroundColor (lighten 0.2 bgColor)
        boxShadow [bsColor lineColor2 $ shadowWithBlur nil (px 2) (px 3)]
        hover & boxShadow
            [bsColor lineColor3 $ shadowWithBlur nil (px 2) (px 5)]
    section ? do
        forM_ (map byClass ["author", "date", "summary"]) $ \e -> e & p <? do
            color metaColor
            textDecoration underline
            textDecorationColor ulColor
    section # ".tags" |> ul ? do
        display flex
        flexFlow CF.row CF.wrap
        borderWidth nil
        sym2 padding nil $ rem 1
        li # ".tags-li" <? do
            sym2 margin nil $ px 5
            nthChild "1" & marginLeft nil
            CF.flexShrink 0
            sym borderRadius $ px 2
            (borderLeft <> borderRight) solid (px 3) liColor
            (borderTop <> borderBottom) solid (px 1) liColor
            hover & do
                backgroundColor $ lighten 0.3 bgColor
                borderColor dliColor
            boxShadow
                [ bsColor (setA 0.5 dliColor) $ shadowWithBlur nil nil (px 3)
                , bsInset . bsColor liColor $ shadowWithBlur 0 0 (px 10)
                ]
            hover & boxShadow
                [ bsColor (setA 0.5 dliColor) $ shadowWithBlur nil nil (px 3)
                , bsInset . bsColor dliColor $ shadowWithBlur 0 0 (px 10)
                ]


tags :: Css
tags = section # "#tags-cloud" ? do
    fontSize $ rem 3
    display flex
    flexFlow row CF.wrap
    justifyContent center
    alignItems center
    a <? do
        sym margin $ rem 1
        hover & color lineColor3


blogs :: Css
blogs = section # ".blogs-list" ? do
    display flex
    flexFlow column CF.nowrap
    alignContent center
    justifyContent flexStart
    section <? do
        boxShadow [bsColor lineColor $ shadowWithBlur nil (px 2) (px 3)]
        hover & boxShadow
            [bsColor bHeaderBColor $ shadowWithBlur nil (px 2) (px 5)]
        sym borderRadius (rem 0.5)
        sym margin       (rem 0.5)
        h2 ? do
            paddingLeft $ rem 0.5
            sym margin nil
            color bHeaderFColor
            "-webkit-text-stroke" -: "1px rgba(0,0,0,0.5)"
            (  borderLeft solid (rem 1)
                <> (borderBottom <> borderTop) solid (px 2)
                <> backgroundColor
                .  setA 0.3
                )
                bHeaderBColor
            (borderTopLeftRadius <> borderTopRightRadius) (rem 0.5) (rem 0.5)
        hover & h2 ? do
            color bgColor
            (  borderLeftColor
                <> borderBottomColor
                <> borderTopColor
                <> backgroundColor
                .  setA 0.3
                )
                quoteColor


blog :: Css
blog = section # ".blog-section" ? do
    (div # ".sourceCode" <> pre) <? do
        overflowX scroll
        sym2 padding 0 (rem 1)
    ul ? borderWidth nil
    li # hover ? (borderTopColor <> borderBottomColor) transparent
    zipWithM_ (?) hs' $ map (fontSize . rem) [1.5, 1.3, 1.2, 1.1]
    ".footnotes" ? p ? sym margin nil


codeHighlight :: Css
codeHighlight = do
    ".blog-section" |> (pre <> div # ".sourceCode") ? do
        sym borderRadius (px 5)
        boxShadow
            [ bsColor lineColor2 $ shadowWithBlur nil nil (px 3)
            , bsInset . bsColor ccBox $ shadowWithBlur nil nil (px 1)
            ]
        hover & boxShadow [bsColor ccBox $ shadowWithBlur nil nil (px 5)]
    ".blog-section" |> pre ? (important . sym padding $ rem 1)
    p |> code ? do
        color ccVariable
        border solid (px 1) ccBox
        hover & color ccText
    pre |> code ? color ccText

    ".im" ? color ccImport
    ".kw" ? do
        color ccKeyword
        fontStyle italic
    ".bu" ? color ccBuiltIn
    ".va" ? do
        color ccVariable
        textDecoration underline
    ".op" ? color ccOperator
    ".co" ? do
        color ccComment
        fontStyle italic
    ".ex" ? color ccExtension
    ".fu" ? color ccFunction
    ".st" ? color ccString


comment :: Css
comment = do
    ".vssue-header-powered-by" ? display none
    ".vssue" ? do
        ".vssue-header" ? borderBottomColor lineColor
        ".vssue-new-comment" ? do
            borderBottomColor lineColor
            ".vssue-new-comment-input" ? do
                color fontColor
                backgroundColor hbgColor
                disabled & backgroundColor hbgColor
                focus & backgroundColor bgColor
                textShadow (px 1) (px 2) (px 3) (setA 0.3 fontColor)
                "::placeholder" & do
                    color ulColor
                    focus & color lineColor
        ".vssue-button" ? do
            disabled & (color <> borderColor) h2Color
            fontFamily
                ["chalkboard", "comic sans", "pingfang sc", "meslo lg"]
                [sansSerif]
        ".vssue-comments " ? ".vssue-comment" ? do
            ".vssue-comment-header"
                <> ".vssue-comment-main"
                <> ".vssue-comment-footer"
                ?  borderWidth nil
            zipWithM_
                (\cl c -> cl ? do
                    backgroundColor (setA 0.1 c)
                    hover & backgroundColor (setA 0.2 c)
                )
                [ ".vssue-comment-header"
                , ".vssue-comment-main"
                , ".vssue-comment-footer"
                ]
                [lineColor, lineColor3, lineColor2]
            ".vssue-comment-main" ? ".vssue-edit-comment-input" ? do
                fontSize $ rem 1
                textShadow (px 1) (px 2) (px 3) (setA 0.3 fontColor)
            ".vssue-comment-avatar" ? img ? sym borderRadius (pct 50)
        ".vssue-pagination" ? ".vssue-pagination-per-page" ? display none
    ".markdown-body" ? blockquote ? borderLeftColor quoteColor

mix :: Css
mix = h2 # ".center-title" ? do
    sym2 padding (rem 0.5) 0
    backgroundColor $ setA 0.2 lineColor3
    hover & backgroundColor (setA 0.5 lineColor3)
    color bHeaderFColor
    textAlign center
    "-webkit-text-stroke" -: "1px rgba(0,0,0,0.5)"
    boxShadow [bsColor lineColor2 $ shadowWithBlur nil (px 2) (px 3)]
    hover & boxShadow [bsColor bHeaderBColor $ shadowWithBlur nil (px 2) (px 5)]
    sym borderRadius (rem 0.5)
    textShadow (px 1) (px 2) (px 3) (setA 0.3 black)
    hover & textShadow (px 1) (px 2) (px 5) (setA 0.5 black)

--------------------------------------------------------------------------------
-- Parts
noUnderline :: Css
noUnderline = a ? textDecoration none

hs :: Selector
hs' :: [Selector]
hsc :: [Color]
hs = h1 <> h2 <> h3 <> h4 <> h5 <> h6
hs' = [h1, h2, h3, h4]
hsc = [h1Color, h2Color, h3Color, h4Color]

--------------------------------------------------------------------------------
-- Color
bgColor, hbgColor, fontColor, quoteColor, ulColor, liColor, dliColor, hyperColor, hyperColorDark, hyperColorLight, shadowColor, lineColor, lineColor2, lineColor3, h1Color, h2Color, h3Color, h4Color, bHeaderFColor, bHeaderBColor, metaColor
    :: Color
bgColor = "#ffeab6"
hbgColor = "#fad284"
fontColor = "#303a52"
quoteColor = "#f47c7c"
ulColor = "#9e579d"
liColor = lighten 0.2 "#f69d9d"
dliColor = darken 0.5 liColor
hyperColorDark = "#574b90"
hyperColor = ulColor
hyperColorLight = "#fc85ae"
lineColor = "#70a1d7"
lineColor2 = "#d988bc"
lineColor3 = "#a1de93"
shadowColor = "#a9eec2"
h1Color = "#6c567b"
h2Color = "#ff8364"
h3Color = "#616f39"
h4Color = "#c06c84"
bHeaderFColor = lineColor3
bHeaderBColor = lineColor
metaColor = lineColor

ccText, ccBox, ccImport, ccKeyword, ccBuiltIn, ccVariable, ccOperator, ccComment, ccExtension, ccFunction, ccString
    :: Color
ccBox = lineColor3
ccText = h3Color
ccImport = quoteColor
ccKeyword = "#17b978"
ccBuiltIn = lineColor
ccVariable = "#705772"
ccOperator = "#f38181"
ccComment = "#aaaa88"
ccExtension = ccImport
ccFunction = hyperColorDark
ccString = ulColor
