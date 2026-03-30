module PF6.Flex exposing
    ( Flex
    , flex
    , withColumn, withRow
    , withWrap, withNoWrap
    , withJustifyStart, withJustifyCenter, withJustifyEnd, withJustifySpaceBetween
    , withAlignStart, withAlignCenter, withAlignEnd, withAlignStretch
    , withGap, withGapSm, withGapMd, withGapLg, withGapXl
    , withGutter
    , withAttributes
    , toMarkup
    )

{-| PF6 Flex layout

Flexible layout with configurable direction, alignment, justification, wrapping, and gap.

See: <https://www.patternfly.org/layouts/flex>


# Definition

@docs Flex


# Constructor

@docs flex


# Direction

@docs withColumn, withRow


# Wrapping

@docs withWrap, withNoWrap


# Justification

@docs withJustifyStart, withJustifyCenter, withJustifyEnd, withJustifySpaceBetween


# Alignment

@docs withAlignStart, withAlignCenter, withAlignEnd, withAlignStretch


# Gap

@docs withGap, withGapSm, withGapMd, withGapLg, withGapXl


# Gutter

@docs withGutter


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque Flex type
-}
type Flex msg
    = Flex (Options msg)


type Direction
    = Row
    | Column


type Wrap
    = Wrap
    | NoWrap


type Justify
    = JustifyStart
    | JustifyCenter
    | JustifyEnd
    | JustifySpaceBetween
    | JustifyDefault


type Align
    = AlignStart
    | AlignCenter
    | AlignEnd
    | AlignStretch
    | AlignDefault


type Gap
    = GapNone
    | GapSm
    | GapMd
    | GapLg
    | GapXl


type alias Options msg =
    { children : List (Html msg)
    , direction : Direction
    , wrap : Maybe Wrap
    , justify : Justify
    , align : Align
    , gap : Gap
    , hasGutter : Bool
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a Flex layout with a list of children
-}
flex : List (Html msg) -> Flex msg
flex children =
    Flex
        { children = children
        , direction = Row
        , wrap = Nothing
        , justify = JustifyDefault
        , align = AlignDefault
        , gap = GapNone
        , hasGutter = False
        , extraAttrs = []
        }


{-| Vertical (column) direction
-}
withColumn : Flex msg -> Flex msg
withColumn (Flex opts) =
    Flex { opts | direction = Column }


{-| Horizontal (row) direction — default
-}
withRow : Flex msg -> Flex msg
withRow (Flex opts) =
    Flex { opts | direction = Row }


{-| Allow items to wrap
-}
withWrap : Flex msg -> Flex msg
withWrap (Flex opts) =
    Flex { opts | wrap = Just Wrap }


{-| Prevent items from wrapping
-}
withNoWrap : Flex msg -> Flex msg
withNoWrap (Flex opts) =
    Flex { opts | wrap = Just NoWrap }


{-| Justify items to the start
-}
withJustifyStart : Flex msg -> Flex msg
withJustifyStart (Flex opts) =
    Flex { opts | justify = JustifyStart }


{-| Justify items to the center
-}
withJustifyCenter : Flex msg -> Flex msg
withJustifyCenter (Flex opts) =
    Flex { opts | justify = JustifyCenter }


{-| Justify items to the end
-}
withJustifyEnd : Flex msg -> Flex msg
withJustifyEnd (Flex opts) =
    Flex { opts | justify = JustifyEnd }


{-| Space items evenly between
-}
withJustifySpaceBetween : Flex msg -> Flex msg
withJustifySpaceBetween (Flex opts) =
    Flex { opts | justify = JustifySpaceBetween }


{-| Align items to the start
-}
withAlignStart : Flex msg -> Flex msg
withAlignStart (Flex opts) =
    Flex { opts | align = AlignStart }


{-| Align items to the center
-}
withAlignCenter : Flex msg -> Flex msg
withAlignCenter (Flex opts) =
    Flex { opts | align = AlignCenter }


{-| Align items to the end
-}
withAlignEnd : Flex msg -> Flex msg
withAlignEnd (Flex opts) =
    Flex { opts | align = AlignEnd }


{-| Stretch items to fill the cross axis
-}
withAlignStretch : Flex msg -> Flex msg
withAlignStretch (Flex opts) =
    Flex { opts | align = AlignStretch }


{-| Default gap (var token spacing)
-}
withGap : Flex msg -> Flex msg
withGap (Flex opts) =
    Flex { opts | gap = GapMd }


{-| Small gap
-}
withGapSm : Flex msg -> Flex msg
withGapSm (Flex opts) =
    Flex { opts | gap = GapSm }


{-| Medium gap
-}
withGapMd : Flex msg -> Flex msg
withGapMd (Flex opts) =
    Flex { opts | gap = GapMd }


{-| Large gap
-}
withGapLg : Flex msg -> Flex msg
withGapLg (Flex opts) =
    Flex { opts | gap = GapLg }


{-| Extra-large gap
-}
withGapXl : Flex msg -> Flex msg
withGapXl (Flex opts) =
    Flex { opts | gap = GapXl }


{-| Add PF6 gutter spacing between items
-}
withGutter : Flex msg -> Flex msg
withGutter (Flex opts) =
    Flex { opts | hasGutter = True }


{-| Append extra HTML attributes to the root element.
-}
withAttributes : List (Html.Attribute msg) -> Flex msg -> Flex msg
withAttributes attrs (Flex opts) =
    Flex { opts | extraAttrs = opts.extraAttrs ++ attrs }


-- Internal helpers


flexClasses : Options msg -> String
flexClasses opts =
    [ Just "pf-v6-l-flex"
    , if opts.direction == Column then Just "pf-m-column" else Nothing
    , case opts.wrap of
        Just Wrap ->
            Just "pf-m-wrap"

        Just NoWrap ->
            Just "pf-m-nowrap"

        Nothing ->
            Nothing
    , case opts.justify of
        JustifyStart ->
            Just "pf-m-justify-content-flex-start"

        JustifyCenter ->
            Just "pf-m-justify-content-center"

        JustifyEnd ->
            Just "pf-m-justify-content-flex-end"

        JustifySpaceBetween ->
            Just "pf-m-justify-content-space-between"

        JustifyDefault ->
            Nothing
    , case opts.align of
        AlignStart ->
            Just "pf-m-align-items-flex-start"

        AlignCenter ->
            Just "pf-m-align-items-center"

        AlignEnd ->
            Just "pf-m-align-items-flex-end"

        AlignStretch ->
            Just "pf-m-align-items-stretch"

        AlignDefault ->
            Nothing
    , case opts.gap of
        GapNone ->
            Nothing

        GapSm ->
            Just "pf-m-gap-sm"

        GapMd ->
            Just "pf-m-gap-md"

        GapLg ->
            Just "pf-m-gap-lg"

        GapXl ->
            Just "pf-m-gap-xl"
    , if opts.hasGutter then Just "pf-m-gutter" else Nothing
    ]
        |> List.filterMap identity
        |> String.join " "


{-| Render the Flex as an `Html msg`
-}
toMarkup : Flex msg -> Html msg
toMarkup (Flex opts) =
    Html.div
        (Attr.class (flexClasses opts) :: opts.extraAttrs)
        opts.children
