module PF6.Masthead exposing
    ( Masthead
    , masthead
    , withToggle, withBrand, withToolbar
    , withAttributes
    , toMarkup
    )

{-| PF6 Masthead component

The Masthead is the horizontal bar at the top of the page, holding the brand
and navigation toolbar.

See: <https://www.patternfly.org/components/masthead>


# Definition

@docs Masthead


# Constructor

@docs masthead


# Content modifiers

@docs withToggle, withBrand, withToolbar


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events


{-| Opaque Masthead type
-}
type Masthead msg
    = Masthead (Options msg)


type alias Options msg =
    { toggle : Maybe msg
    , brand : Maybe (Html msg)
    , toolbar : Maybe (Html msg)
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct an empty Masthead
-}
masthead : Masthead msg
masthead =
    Masthead
        { toggle = Nothing
        , brand = Nothing
        , toolbar = Nothing
        , extraAttrs = []
        }


{-| Add a sidebar toggle button that sends this msg when clicked

The button renders a hamburger icon (fa-bars) and announces itself as
"Global navigation" to screen readers.

-}
withToggle : msg -> Masthead msg -> Masthead msg
withToggle msg (Masthead opts) =
    Masthead { opts | toggle = Just msg }


{-| Set the brand area (logo, product name)
-}
withBrand : Html msg -> Masthead msg -> Masthead msg
withBrand content (Masthead opts) =
    Masthead { opts | brand = Just content }


{-| Set the toolbar area (user menu, settings icons, etc.)
-}
withToolbar : Html msg -> Masthead msg -> Masthead msg
withToolbar content (Masthead opts) =
    Masthead { opts | toolbar = Just content }


{-| Append extra HTML attributes to the root element.
-}
withAttributes : List (Html.Attribute msg) -> Masthead msg -> Masthead msg
withAttributes attrs (Masthead opts) =
    Masthead { opts | extraAttrs = opts.extraAttrs ++ attrs }


{-| Render the Masthead as an `Html msg`
-}
toMarkup : Masthead msg -> Html msg
toMarkup (Masthead opts) =
    let
        toggleEl =
            case opts.toggle of
                Nothing ->
                    Html.text ""

                Just msg ->
                    Html.div
                        [ Attr.class "pf-v6-c-masthead__toggle" ]
                        [ Html.button
                            [ Attr.class "pf-v6-c-button pf-m-plain"
                            , Attr.type_ "button"
                            , Attr.attribute "aria-label" "Global navigation"
                            , Events.onClick msg
                            ]
                            [ Html.i
                                [ Attr.class "fas fa-bars"
                                , Attr.attribute "aria-hidden" "true"
                                ]
                                []
                            ]
                        ]

        brandEl =
            case opts.brand of
                Nothing ->
                    Html.text ""

                Just content ->
                    Html.div
                        [ Attr.class "pf-v6-c-masthead__main" ]
                        [ content ]

        toolbarEl =
            case opts.toolbar of
                Nothing ->
                    Html.text ""

                Just content ->
                    Html.div
                        [ Attr.class "pf-v6-c-masthead__toolbar" ]
                        [ content ]
    in
    Html.header
        (Attr.class "pf-v6-c-masthead" :: opts.extraAttrs)
        [ toggleEl, brandEl, toolbarEl ]
