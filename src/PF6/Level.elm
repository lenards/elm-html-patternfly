module PF6.Level exposing
    ( Level
    , level
    , withGutter
    , withAttributes
    , toMarkup
    )

{-| PF6 Level layout

Distributes items evenly across a horizontal row, centering vertically. Items wrap on resize.

See: <https://www.patternfly.org/layouts/level>


# Definition

@docs Level


# Constructor

@docs level


# Modifiers

@docs withGutter


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque Level type
-}
type Level msg
    = Level (Options msg)


type alias Options msg =
    { items : List (Html msg)
    , hasGutter : Bool
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a Level with a list of items distributed evenly
-}
level : List (Html msg) -> Level msg
level items =
    Level { items = items, hasGutter = False, extraAttrs = [] }


{-| Add gutters between level items
-}
withGutter : Level msg -> Level msg
withGutter (Level opts) =
    Level { opts | hasGutter = True }


{-| Append extra HTML attributes to the root element.
-}
withAttributes : List (Html.Attribute msg) -> Level msg -> Level msg
withAttributes attrs (Level opts) =
    Level { opts | extraAttrs = opts.extraAttrs ++ attrs }


{-| Render the Level as an `Html msg`
-}
toMarkup : Level msg -> Html msg
toMarkup (Level opts) =
    Html.div
        (Attr.class
            (if opts.hasGutter then
                "pf-v6-l-level pf-m-gutter"

             else
                "pf-v6-l-level"
            )
            :: opts.extraAttrs
        )
        (List.map
            (\item -> Html.div [ Attr.class "pf-v6-l-level__item" ] [ item ])
            opts.items
        )
