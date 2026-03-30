module PF6.Title exposing
    ( Title
    , title
    , withH1, withH2, withH3, withH4, withH5, withH6
    , withAttributes
    , toMarkup
    )

{-| PF6 Title component

Provides semantic heading levels (h1–h6) with PF6 typography sizing.

See: <https://www.patternfly.org/components/title>


# Definition

@docs Title


# Constructor

@docs title


# Heading level modifiers

@docs withH1, withH2, withH3, withH4, withH5, withH6


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque Title type
-}
type Title msg
    = Title (Options msg)


type HeadingLevel
    = H1
    | H2
    | H3
    | H4
    | H5
    | H6


type alias Options msg =
    { text : String
    , level : HeadingLevel
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a Title

Defaults to h1 with 2xl size.

-}
title : String -> Title msg
title t =
    Title
        { text = t
        , level = H1
        , extraAttrs = []
        }


{-| Render as h1 (4xl — largest)
-}
withH1 : Title msg -> Title msg
withH1 (Title opts) =
    Title { opts | level = H1 }


{-| Render as h2 (3xl)
-}
withH2 : Title msg -> Title msg
withH2 (Title opts) =
    Title { opts | level = H2 }


{-| Render as h3 (2xl)
-}
withH3 : Title msg -> Title msg
withH3 (Title opts) =
    Title { opts | level = H3 }


{-| Render as h4 (xl)
-}
withH4 : Title msg -> Title msg
withH4 (Title opts) =
    Title { opts | level = H4 }


{-| Render as h5 (lg)
-}
withH5 : Title msg -> Title msg
withH5 (Title opts) =
    Title { opts | level = H5 }


{-| Render as h6 (md)
-}
withH6 : Title msg -> Title msg
withH6 (Title opts) =
    Title { opts | level = H6 }


{-| Append extra HTML attributes to the heading element.
-}
withAttributes : List (Html.Attribute msg) -> Title msg -> Title msg
withAttributes attrs (Title opts) =
    Title { opts | extraAttrs = opts.extraAttrs ++ attrs }


-- Internal helpers


sizeClass : HeadingLevel -> String
sizeClass level =
    case level of
        H1 ->
            "pf-m-4xl"

        H2 ->
            "pf-m-3xl"

        H3 ->
            "pf-m-2xl"

        H4 ->
            "pf-m-xl"

        H5 ->
            "pf-m-lg"

        H6 ->
            "pf-m-md"


{-| Render the Title as an `Html msg`
-}
toMarkup : Title msg -> Html msg
toMarkup (Title opts) =
    let
        attrs =
            Attr.class ("pf-v6-c-title " ++ sizeClass opts.level)
                :: opts.extraAttrs

        content =
            [ Html.text opts.text ]
    in
    case opts.level of
        H1 ->
            Html.h1 attrs content

        H2 ->
            Html.h2 attrs content

        H3 ->
            Html.h3 attrs content

        H4 ->
            Html.h4 attrs content

        H5 ->
            Html.h5 attrs content

        H6 ->
            Html.h6 attrs content
