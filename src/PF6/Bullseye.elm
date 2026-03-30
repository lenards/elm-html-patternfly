module PF6.Bullseye exposing
    ( Bullseye
    , bullseye
    , withAttributes
    , toMarkup
    )

{-| PF6 Bullseye layout

Centers a single piece of content both horizontally and vertically.

See: <https://www.patternfly.org/layouts/bullseye>


# Definition

@docs Bullseye


# Constructor

@docs bullseye


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque Bullseye type
-}
type Bullseye msg
    = Bullseye (Options msg)


type alias Options msg =
    { child : Html msg
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a Bullseye centering container
-}
bullseye : Html msg -> Bullseye msg
bullseye child =
    Bullseye { child = child, extraAttrs = [] }


{-| Append extra HTML attributes to the root element.
-}
withAttributes : List (Html.Attribute msg) -> Bullseye msg -> Bullseye msg
withAttributes attrs (Bullseye opts) =
    Bullseye { opts | extraAttrs = opts.extraAttrs ++ attrs }


{-| Render the Bullseye as an `Html msg`
-}
toMarkup : Bullseye msg -> Html msg
toMarkup (Bullseye opts) =
    Html.div
        (Attr.class "pf-v6-l-bullseye" :: opts.extraAttrs)
        [ opts.child ]
