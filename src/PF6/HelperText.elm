module PF6.HelperText exposing
    ( HelperText, Variant
    , helperText
    , withDefault, withError, withWarning, withSuccess, withIndeterminate
    , withIcon, withDynamic
    , withAttributes
    , toMarkup
    )

{-| PF6 HelperText component

Helper text provides contextual information below form controls.

See: <https://www.patternfly.org/components/helper-text>


# Definition

@docs HelperText, Variant


# Constructor

@docs helperText


# Variant modifiers

@docs withDefault, withError, withWarning, withSuccess, withIndeterminate


# Content modifiers

@docs withIcon, withDynamic


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque HelperText type
-}
type HelperText msg
    = HelperText (Options msg)


{-| Helper text variant
-}
type Variant
    = Default
    | Error
    | Warning
    | Success
    | Indeterminate


type alias Options msg =
    { items : List (HelperItem msg)
    , extraAttrs : List (Html.Attribute msg)
    }


type HelperItem msg
    = HelperItem
        { text : String
        , variant : Variant
        , icon : Maybe (Html msg)
        , isDynamic : Bool
        }


{-| Construct a HelperText with a single message
-}
helperText : String -> HelperText msg
helperText text =
    HelperText
        { items =
            [ HelperItem
                { text = text
                , variant = Default
                , icon = Nothing
                , isDynamic = False
                }
            ]
        , extraAttrs = []
        }


applyToLast : (HelperItem msg -> HelperItem msg) -> HelperText msg -> HelperText msg
applyToLast f (HelperText opts) =
    let
        updated =
            case List.reverse opts.items of
                [] ->
                    []

                last :: rest ->
                    List.reverse (f last :: rest)
    in
    HelperText { opts | items = updated }


{-| Default (gray) variant
-}
withDefault : HelperText msg -> HelperText msg
withDefault =
    applyToLast (\(HelperItem i) -> HelperItem { i | variant = Default })


{-| Error (red) variant
-}
withError : HelperText msg -> HelperText msg
withError =
    applyToLast (\(HelperItem i) -> HelperItem { i | variant = Error })


{-| Warning (yellow) variant
-}
withWarning : HelperText msg -> HelperText msg
withWarning =
    applyToLast (\(HelperItem i) -> HelperItem { i | variant = Warning })


{-| Success (green) variant
-}
withSuccess : HelperText msg -> HelperText msg
withSuccess =
    applyToLast (\(HelperItem i) -> HelperItem { i | variant = Success })


{-| Indeterminate variant
-}
withIndeterminate : HelperText msg -> HelperText msg
withIndeterminate =
    applyToLast (\(HelperItem i) -> HelperItem { i | variant = Indeterminate })


{-| Add an icon to the last item
-}
withIcon : Html msg -> HelperText msg -> HelperText msg
withIcon icon =
    applyToLast (\(HelperItem i) -> HelperItem { i | icon = Just icon })


{-| Mark the last item as dynamic (sets aria-live for screen reader announcements)
-}
withDynamic : HelperText msg -> HelperText msg
withDynamic =
    applyToLast (\(HelperItem i) -> HelperItem { i | isDynamic = True })


{-| Append extra HTML attributes to the root helper text element.
-}
withAttributes : List (Html.Attribute msg) -> HelperText msg -> HelperText msg
withAttributes attrs (HelperText opts) =
    HelperText { opts | extraAttrs = opts.extraAttrs ++ attrs }


-- Internal helpers


variantClass : Variant -> String
variantClass variant =
    case variant of
        Default ->
            "pf-m-default"

        Error ->
            "pf-m-error"

        Warning ->
            "pf-m-warning"

        Success ->
            "pf-m-success"

        Indeterminate ->
            "pf-m-indeterminate"


variantIconClass : Variant -> Maybe String
variantIconClass variant =
    case variant of
        Default ->
            Nothing

        Error ->
            Just "fas fa-fw fa-exclamation-circle"

        Warning ->
            Just "fas fa-fw fa-exclamation-triangle"

        Success ->
            Just "fas fa-fw fa-check-circle"

        Indeterminate ->
            Just "fas fa-fw fa-minus"


variantScreenReader : Variant -> Maybe String
variantScreenReader variant =
    case variant of
        Default ->
            Nothing

        Error ->
            Just "Error:"

        Warning ->
            Just "Warning:"

        Success ->
            Just "Success:"

        Indeterminate ->
            Just "Indeterminate:"


renderItem : HelperItem msg -> Html msg
renderItem (HelperItem i) =
    let
        liveAttr =
            if i.isDynamic then
                [ Attr.attribute "aria-live" "polite" ]

            else
                []

        iconEl =
            case i.icon of
                Just el ->
                    Html.span
                        [ Attr.class "pf-v6-c-helper-text__item-icon" ]
                        [ el ]

                Nothing ->
                    case variantIconClass i.variant of
                        Just cls ->
                            Html.span
                                [ Attr.class "pf-v6-c-helper-text__item-icon" ]
                                [ Html.i [ Attr.class cls, Attr.attribute "aria-hidden" "true" ] [] ]

                        Nothing ->
                            Html.text ""

        srLabel =
            case variantScreenReader i.variant of
                Just lbl ->
                    [ Html.span [ Attr.class "pf-v6-screen-reader" ] [ Html.text lbl ] ]

                Nothing ->
                    []
    in
    Html.div
        ([ Attr.class ("pf-v6-c-helper-text__item " ++ variantClass i.variant) ]
            ++ liveAttr
        )
        [ iconEl
        , Html.span
            [ Attr.class "pf-v6-c-helper-text__item-text" ]
            (srLabel ++ [ Html.text i.text ])
        ]


{-| Render the HelperText as an `Html msg`
-}
toMarkup : HelperText msg -> Html msg
toMarkup (HelperText opts) =
    Html.div
        (Attr.class "pf-v6-c-helper-text" :: opts.extraAttrs)
        (List.map renderItem opts.items)
