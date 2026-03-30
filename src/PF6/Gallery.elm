module PF6.Gallery exposing
    ( Gallery
    , gallery
    , withGutter, withMinWidth
    , withAttributes
    , toMarkup
    )

{-| PF6 Gallery layout

Responsive grid of uniform items. Items wrap and maintain consistent sizing.

See: <https://www.patternfly.org/layouts/gallery>


# Definition

@docs Gallery


# Constructor

@docs gallery


# Modifiers

@docs withGutter, withMinWidth


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque Gallery type
-}
type Gallery msg
    = Gallery (Options msg)


type alias Options msg =
    { items : List (Html msg)
    , hasGutter : Bool
    , minWidth : Maybe String
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a Gallery with a list of items
-}
gallery : List (Html msg) -> Gallery msg
gallery items =
    Gallery
        { items = items
        , hasGutter = False
        , minWidth = Nothing
        , extraAttrs = []
        }


{-| Add gutters between gallery items
-}
withGutter : Gallery msg -> Gallery msg
withGutter (Gallery opts) =
    Gallery { opts | hasGutter = True }


{-| Set a minimum width for gallery items (e.g. "200px" or "16rem")

This sets the CSS custom property `--pf-v6-l-gallery--GridTemplateColumns--min`.

-}
withMinWidth : String -> Gallery msg -> Gallery msg
withMinWidth w (Gallery opts) =
    Gallery { opts | minWidth = Just w }


{-| Append extra HTML attributes to the root element.
-}
withAttributes : List (Html.Attribute msg) -> Gallery msg -> Gallery msg
withAttributes attrs (Gallery opts) =
    Gallery { opts | extraAttrs = opts.extraAttrs ++ attrs }


{-| Render the Gallery as an `Html msg`
-}
toMarkup : Gallery msg -> Html msg
toMarkup (Gallery opts) =
    let
        rootClass =
            if opts.hasGutter then
                "pf-v6-l-gallery pf-m-gutter"

            else
                "pf-v6-l-gallery"

        minWidthAttr =
            case opts.minWidth of
                Nothing ->
                    []

                Just w ->
                    [ Attr.style "--pf-v6-l-gallery--GridTemplateColumns--min" w ]
    in
    Html.div
        (Attr.class rootClass :: minWidthAttr ++ opts.extraAttrs)
        (List.map
            (\item -> Html.div [ Attr.class "pf-v6-l-gallery__item" ] [ item ])
            opts.items
        )
