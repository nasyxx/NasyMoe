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
import           Control.Monad                  ( zipWithM_ )
--------------------------------------------------------------------------------
import           Clay                    hiding ( fontColor
                                                , map
                                                )
import qualified Clay.Flexbox                  as CF
--------------------------------------------------------------------------------
main :: IO ()
main = putCss $ specialB >> basic >> layout >> metas >> blog

specialB :: Css
specialB = ".hide" ? display none

basic :: Css
basic = do
    -- | Quote style
    blockquote ? do
        flip (sym2 margin) 0 $ rem 1
        borderLeft solid (rem 1) quoteColor

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
    (hs <> a <> p <> span) ? fontFamily
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
  where
    hs  = h1 <> h2 <> h3 <> h4 <> h5 <> h6
    hs' = [h1, h2, h3, h4]
    hsc = [h1Color, h2Color, h3Color, h4Color]


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


    "#footer" ? do
        display flex
        flexFlow column CF.nowrap
        flexGrow 1
        flexShrink 0
        justifyContent spaceBetween

        noUnderline
        header <? do
            display flex
            marginBottom $ rem 1
            sym borderRadius $ px 5
            justifyContent spaceAround

            boxShadow [bsColor lineColor2 $ shadowWithBlur nil (px 2) (px 3)]
            hover & boxShadow
                [bsColor lineColor2 $ shadowWithBlur nil (px 2) (px 5)]

        borderTop dashed (px 5) lineColor
        sym borderRadius $ px 5

    "#copyright" ? do
        fontFamily ["American Typewriter"] [cursive, serif]
        fontSizeCustom smaller
        textAlign center
        color "#d988bc"


metas :: Css
metas = do
    section # ".metas" ? do
        sym padding $ rem 1
        borderBottom dashed (px 2) lineColor
        sym borderRadius $ px 5
        backgroundColor $ lighten 0.1 bgColor
        boxShadow [bsColor lineColor2 $ shadowWithBlur nil (px 2) (px 3)]
        hover & boxShadow
            [bsColor lineColor3 $ shadowWithBlur nil (px 2) (px 5)]
    section # ".tags" |> ul ? do
        display flex
        flexFlow CF.row CF.wrap
        li # ".tags-li" <? do
            sym2 margin nil $ px 5
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


blog :: Css
blog = do
    header # ".blog-header" ? do
        h2 <? do
            sym2 padding (rem 0.5) 0
            backgroundColor $ setA 0.5 $ lighten 0.2 bgColor
            color bHeaderFColor
            "-webkit-text-stroke" -: "1px rgba(0,0,0,0.5)"
            boxShadow [bsColor lineColor2 $ shadowWithBlur nil (px 2) (px 3)]
            hover & boxShadow
                [bsColor bHeaderBColor $ shadowWithBlur nil (px 2) (px 5)]
            sym borderRadius (rem 0.5)
            textShadow (px 1) (px 2) (px 3) (setA 0.3 black)
            hover & textShadow (px 1) (px 2) (px 5) (setA 0.5 black)
            textAlign center
    section # ".blog-section" ? do
        (div # ".sourceCode" <> pre) <? do
            overflowX scroll
            sym2 padding 0 (rem 1)

        ul ? borderWidth nil
        li # hover ? (borderTopColor <> borderBottomColor) transparent


noUnderline :: Css
noUnderline = a ? textDecoration none


bgColor, fontColor, quoteColor, ulColor, liColor, dliColor, hyperColor, hyperColorDark, hyperColorLight, shadowColor, lineColor, lineColor2, lineColor3, h1Color, h2Color, h3Color, h4Color, bHeaderFColor, bHeaderBColor
    :: Color
bgColor = "#ffeab6"
fontColor = "#303a52"
quoteColor = "#f47c7c"
ulColor = "#9e579d"
liColor = lighten 0.2 "#f69d9d"
dliColor = darken 0.5 liColor
hyperColorDark = "#574b90"
hyperColor = "#9e579d"
hyperColorLight = "#fc85ae"
lineColor = "#70a1d7"
lineColor2 = "#d988bc"
lineColor3 = "#a1de93"
shadowColor = "#f67280"
h1Color = "#6c567b"
h2Color = "#ff8364"
h3Color = "#616f39"
h4Color = "#c06c84"
bHeaderFColor = lineColor3
bHeaderBColor = lineColor
