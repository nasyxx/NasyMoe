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
main = putCss $ specialB >> basic >> layout >> blog

specialB :: Css
specialB = do
    ".hide" ? display none

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
        (borderLeft <> borderRight) solid (rem 0.5) ulColor
        sym padding (px 1)
    li ? do
        listStyleType none
        borderLeft solid (rem 0.5) liColor
        (borderTop <> borderBottom) solid (px 1) transparent
        flip (sym2 margin) 0 $ px 1
        sym2 padding 0 $ px 2

        hover & borderColor (darken 0.5 liColor)

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
    (pre <> ".sourceCode" <> code) ? fontFamily
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


blog :: Css
blog = do
    section # ".blog-section" ? do
        (div # ".sourceCode" <> pre) <? do
            overflowX scroll
            sym2 padding 0 (rem 1)

        ul ? borderWidth nil
        li # hover ? (borderTopColor <> borderBottomColor) transparent


noUnderline :: Css
noUnderline = a ? textDecoration none


bgColor, fontColor, quoteColor, ulColor, liColor, hyperColor, hyperColorDark, hyperColorLight, shadowColor, lineColor, lineColor2, h1Color, h2Color, h3Color, h4Color, bHeaderFColor, bHeaderBColor
    :: Color
bgColor = "#ffeab6"
fontColor = "#303a52"
quoteColor = "#fc85ae"
ulColor = "#9e579d"
liColor = lighten 0.2 "#f69d9d"
hyperColorDark = "#574b90"
hyperColor = "#9e579d"
hyperColorLight = "#fc85ae"
lineColor = "#70a1d7"
lineColor2 = "#d988bc"
shadowColor = "#f67280"
h1Color = "#6c567b"
h2Color = "#ff8364"
h3Color = "#616f39"
h4Color = "#c06c84"
bHeaderFColor = "#a1de93"
bHeaderBColor = lineColor
