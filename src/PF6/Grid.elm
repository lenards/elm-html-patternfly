module PF6.Grid exposing
    ( Grid, GridItem
    , grid
    , gridItem, withSpan, withOffset
    , withGutter
    , withAttributes
    , toMarkup
    )

{-| PF6 Grid layout

A 12-column grid system. Each item specifies how many columns it spans.

See: <https://www.patternfly.org/layouts/grid>


# Definition

@docs Grid, GridItem


# Constructor

@docs grid


# Item constructors

@docs gridItem, withSpan, withOffset


# Layout modifiers

@docs withGutter


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque Grid type
-}
type Grid msg
    = Grid (Options msg)


{-| A grid item with an optional column span (1–12)
-}
type GridItem msg
    = GridItem
        { child : Html msg
        , span : Maybe Int
        , offset : Maybe Int
        }


type alias Options msg =
    { items : List (GridItem msg)
    , hasGutter : Bool
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a Grid with a list of GridItems
-}
grid : List (GridItem msg) -> Grid msg
grid items =
    Grid { items = items, hasGutter = False, extraAttrs = [] }


{-| Wrap content as a grid item
-}
gridItem : Html msg -> GridItem msg
gridItem child =
    GridItem { child = child, span = Nothing, offset = Nothing }


{-| Set the column span for a grid item (1–12)
-}
withSpan : Int -> GridItem msg -> GridItem msg
withSpan n (GridItem i) =
    GridItem { i | span = Just (clamp 1 12 n) }


{-| Set the column offset for a grid item (1–11)
-}
withOffset : Int -> GridItem msg -> GridItem msg
withOffset n (GridItem i) =
    GridItem { i | offset = Just (clamp 1 11 n) }


{-| Add gutters between grid items
-}
withGutter : Grid msg -> Grid msg
withGutter (Grid opts) =
    Grid { opts | hasGutter = True }


{-| Append extra HTML attributes to the root element.
-}
withAttributes : List (Html.Attribute msg) -> Grid msg -> Grid msg
withAttributes attrs (Grid opts) =
    Grid { opts | extraAttrs = opts.extraAttrs ++ attrs }


-- Internal helpers


itemClass : GridItem msg -> String
itemClass (GridItem i) =
    [ Just "pf-v6-l-grid__item"
    , Maybe.map (\n -> "pf-m-" ++ String.fromInt n ++ "-col") i.span
    , Maybe.map (\n -> "pf-m-offset-" ++ String.fromInt n ++ "-col") i.offset
    ]
        |> List.filterMap identity
        |> String.join " "


{-| Render the Grid as an `Html msg`
-}
toMarkup : Grid msg -> Html msg
toMarkup (Grid opts) =
    Html.div
        (Attr.class
            (if opts.hasGutter then
                "pf-v6-l-grid pf-m-gutter"

             else
                "pf-v6-l-grid"
            )
            :: opts.extraAttrs
        )
        (List.map
            (\item ->
                Html.div [ Attr.class (itemClass item) ] [ (\(GridItem i) -> i.child) item ]
            )
            opts.items
        )
