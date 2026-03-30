module PF6.Alert exposing
    ( Alert, Variant
    , alert
    , withSuccess, withDanger, withWarning, withInfo, withCustom
    , withTitle, withInline, withPlain
    , withCloseMsg, withActions
    , withAttributes
    , toMarkup
    )

{-| PF6 Alert component

Alerts communicate status and provide brief, contextual information to users.

See: <https://www.patternfly.org/components/alert>


# Definition

@docs Alert, Variant


# Constructor

@docs alert


# Variant modifiers

@docs withSuccess, withDanger, withWarning, withInfo, withCustom


# Display modifiers

@docs withTitle, withInline, withPlain


# Action modifiers

@docs withCloseMsg, withActions


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events


{-| Opaque Alert type
-}
type Alert msg
    = Alert (Options msg)


{-| Alert severity variants
-}
type Variant
    = Default
    | Success
    | Danger
    | Warning
    | Info
    | Custom


type alias Options msg =
    { title : String
    , body : Maybe String
    , variant : Variant
    , isInline : Bool
    , isPlain : Bool
    , onClose : Maybe msg
    , actions : Maybe (Html msg)
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a default Alert with body text
-}
alert : String -> Alert msg
alert bodyText =
    Alert
        { title = ""
        , body = Just bodyText
        , variant = Default
        , isInline = False
        , isPlain = False
        , onClose = Nothing
        , actions = Nothing
        , extraAttrs = []
        }


{-| Set the alert title
-}
withTitle : String -> Alert msg -> Alert msg
withTitle t (Alert opts) =
    Alert { opts | title = t }


{-| Success variant (green)
-}
withSuccess : Alert msg -> Alert msg
withSuccess (Alert opts) =
    Alert { opts | variant = Success }


{-| Danger variant (red)
-}
withDanger : Alert msg -> Alert msg
withDanger (Alert opts) =
    Alert { opts | variant = Danger }


{-| Warning variant (yellow)
-}
withWarning : Alert msg -> Alert msg
withWarning (Alert opts) =
    Alert { opts | variant = Warning }


{-| Info variant (blue/purple)
-}
withInfo : Alert msg -> Alert msg
withInfo (Alert opts) =
    Alert { opts | variant = Info }


{-| Custom variant (teal)
-}
withCustom : Alert msg -> Alert msg
withCustom (Alert opts) =
    Alert { opts | variant = Custom }


{-| Inline display — no box shadow, fits within content areas
-}
withInline : Alert msg -> Alert msg
withInline (Alert opts) =
    Alert { opts | isInline = True }


{-| Plain display — no background color
-}
withPlain : Alert msg -> Alert msg
withPlain (Alert opts) =
    Alert { opts | isPlain = True }


{-| Add a close button that sends msg on click
-}
withCloseMsg : msg -> Alert msg -> Alert msg
withCloseMsg msg (Alert opts) =
    Alert { opts | onClose = Just msg }


{-| Add action elements (e.g. buttons) below the alert body
-}
withActions : Html msg -> Alert msg -> Alert msg
withActions el (Alert opts) =
    Alert { opts | actions = Just el }


{-| Append extra HTML attributes to the root alert element.
-}
withAttributes : List (Html.Attribute msg) -> Alert msg -> Alert msg
withAttributes attrs (Alert opts) =
    Alert { opts | extraAttrs = opts.extraAttrs ++ attrs }


-- Internal helpers


variantClass : Variant -> Maybe String
variantClass variant =
    case variant of
        Default ->
            Nothing

        Success ->
            Just "pf-m-success"

        Danger ->
            Just "pf-m-danger"

        Warning ->
            Just "pf-m-warning"

        Info ->
            Just "pf-m-info"

        Custom ->
            Just "pf-m-custom"


variantIconLabel : Variant -> String
variantIconLabel variant =
    case variant of
        Default ->
            "Default alert:"

        Success ->
            "Success alert:"

        Danger ->
            "Danger alert:"

        Warning ->
            "Warning alert:"

        Info ->
            "Info alert:"

        Custom ->
            "Custom alert:"


variantIconClass : Variant -> String
variantIconClass variant =
    case variant of
        Default ->
            "fas fa-fw fa-bell"

        Success ->
            "fas fa-fw fa-check-circle"

        Danger ->
            "fas fa-fw fa-exclamation-circle"

        Warning ->
            "fas fa-fw fa-exclamation-triangle"

        Info ->
            "fas fa-fw fa-info-circle"

        Custom ->
            "fas fa-fw fa-bell"


alertClasses : Options msg -> String
alertClasses opts =
    [ Just "pf-v6-c-alert"
    , variantClass opts.variant
    , if opts.isInline then Just "pf-m-inline" else Nothing
    , if opts.isPlain then Just "pf-m-plain" else Nothing
    ]
        |> List.filterMap identity
        |> String.join " "


{-| Render the Alert as an `Html msg`
-}
toMarkup : Alert msg -> Html msg
toMarkup (Alert opts) =
    let
        screenReaderLabel =
            Html.span
                [ Attr.class "pf-v6-screen-reader" ]
                [ Html.text (variantIconLabel opts.variant) ]

        iconEl =
            Html.div
                [ Attr.class "pf-v6-c-alert__icon" ]
                [ Html.i
                    [ Attr.class (variantIconClass opts.variant)
                    , Attr.attribute "aria-hidden" "true"
                    ]
                    []
                ]

        titleEl =
            if String.isEmpty opts.title then
                Html.text ""

            else
                Html.p
                    [ Attr.class "pf-v6-c-alert__title" ]
                    [ screenReaderLabel
                    , Html.text opts.title
                    ]

        bodyEl =
            case opts.body of
                Nothing ->
                    Html.text ""

                Just b ->
                    Html.div
                        [ Attr.class "pf-v6-c-alert__description" ]
                        [ Html.p [] [ Html.text b ] ]

        closeEl =
            case opts.onClose of
                Nothing ->
                    Html.text ""

                Just msg ->
                    Html.div
                        [ Attr.class "pf-v6-c-alert__action" ]
                        [ Html.button
                            [ Attr.class "pf-v6-c-button pf-m-plain"
                            , Attr.type_ "button"
                            , Attr.attribute "aria-label" "Close alert"
                            , Events.onClick msg
                            ]
                            [ Html.text "×" ]
                        ]

        actionsEl =
            case opts.actions of
                Nothing ->
                    Html.text ""

                Just el ->
                    Html.div [ Attr.class "pf-v6-c-alert__action-group" ] [ el ]
    in
    Html.div
        ([ Attr.class (alertClasses opts)
         , Attr.attribute "aria-label" (variantIconLabel opts.variant)
         ]
            ++ opts.extraAttrs
        )
        [ iconEl
        , titleEl
        , bodyEl
        , closeEl
        , actionsEl
        ]
