module PF6.Banner exposing
    ( Banner, Variant
    , banner
    , withDefault, withInfo, withSuccess, withWarning, withDanger
    , withIcon, withLink
    , withAttributes
    , toMarkup
    )

{-| PF6 Banner component

Banners display important, site-wide information above the page navigation.

See: <https://www.patternfly.org/components/banner>


# Definition

@docs Banner, Variant


# Constructor

@docs banner


# Variant modifiers

@docs withDefault, withInfo, withSuccess, withWarning, withDanger


# Content modifiers

@docs withIcon, withLink


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque Banner type
-}
type Banner msg
    = Banner (Options msg)


{-| Banner severity variant
-}
type Variant
    = Default
    | Info
    | Success
    | Warning
    | Danger


type alias Options msg =
    { text : String
    , variant : Variant
    , icon : Maybe (Html msg)
    , link : Maybe { label : String, href : String }
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a Banner with body text
-}
banner : String -> Banner msg
banner text =
    Banner
        { text = text
        , variant = Default
        , icon = Nothing
        , link = Nothing
        , extraAttrs = []
        }


{-| Default (gray) variant
-}
withDefault : Banner msg -> Banner msg
withDefault (Banner opts) =
    Banner { opts | variant = Default }


{-| Info (blue) variant
-}
withInfo : Banner msg -> Banner msg
withInfo (Banner opts) =
    Banner { opts | variant = Info }


{-| Success (green) variant
-}
withSuccess : Banner msg -> Banner msg
withSuccess (Banner opts) =
    Banner { opts | variant = Success }


{-| Warning (gold) variant
-}
withWarning : Banner msg -> Banner msg
withWarning (Banner opts) =
    Banner { opts | variant = Warning }


{-| Danger (red) variant
-}
withDanger : Banner msg -> Banner msg
withDanger (Banner opts) =
    Banner { opts | variant = Danger }


{-| Add a leading icon
-}
withIcon : Html msg -> Banner msg -> Banner msg
withIcon el (Banner opts) =
    Banner { opts | icon = Just el }


{-| Add a trailing link
-}
withLink : { label : String, href : String } -> Banner msg -> Banner msg
withLink l (Banner opts) =
    Banner { opts | link = Just l }


{-| Append extra HTML attributes to the banner element.
-}
withAttributes : List (Html.Attribute msg) -> Banner msg -> Banner msg
withAttributes attrs (Banner opts) =
    Banner { opts | extraAttrs = opts.extraAttrs ++ attrs }


-- Internal helpers


variantClass : Variant -> Maybe String
variantClass variant =
    case variant of
        Default ->
            Nothing

        Info ->
            Just "pf-m-info"

        Success ->
            Just "pf-m-success"

        Warning ->
            Just "pf-m-warning"

        Danger ->
            Just "pf-m-danger"


bannerClasses : Options msg -> String
bannerClasses opts =
    [ Just "pf-v6-c-banner"
    , variantClass opts.variant
    ]
        |> List.filterMap identity
        |> String.join " "


{-| Render the Banner as an `Html msg`
-}
toMarkup : Banner msg -> Html msg
toMarkup (Banner opts) =
    let
        iconEl =
            case opts.icon of
                Nothing ->
                    Html.text ""

                Just el ->
                    Html.span
                        [ Attr.class "pf-v6-c-banner__icon" ]
                        [ el ]

        linkEl =
            case opts.link of
                Nothing ->
                    Html.text ""

                Just l ->
                    Html.a
                        [ Attr.class "pf-v6-c-banner__link"
                        , Attr.href l.href
                        ]
                        [ Html.text l.label ]
    in
    Html.div
        (Attr.class (bannerClasses opts) :: opts.extraAttrs)
        [ iconEl
        , Html.span [] [ Html.text opts.text ]
        , linkEl
        ]
