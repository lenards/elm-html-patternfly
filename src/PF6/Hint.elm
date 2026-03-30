module PF6.Hint exposing
    ( Hint
    , hint
    , withTitle, withActions, withFooter
    , withAttributes
    , toMarkup
    )

{-| PF6 Hint component

A contextual hint with lighter styling than an Alert. Used to provide
supplemental information or guidance without the urgency of an alert.

See: <https://www.patternfly.org/components/hint>


# Definition

@docs Hint


# Constructor

@docs hint


# Modifiers

@docs withTitle, withActions, withFooter


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque Hint type
-}
type Hint msg
    = Hint (Options msg)


type alias Options msg =
    { body : String
    , title : Maybe String
    , actions : Maybe (Html msg)
    , footer : Maybe (Html msg)
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a Hint with body text

    hint "Try using the search bar to find items quickly."

-}
hint : String -> Hint msg
hint body =
    Hint
        { body = body
        , title = Nothing
        , actions = Nothing
        , footer = Nothing
        , extraAttrs = []
        }


{-| Add a title above the hint body
-}
withTitle : String -> Hint msg -> Hint msg
withTitle t (Hint opts) =
    Hint { opts | title = Just t }


{-| Add action elements (e.g. buttons) to the hint
-}
withActions : Html msg -> Hint msg -> Hint msg
withActions el (Hint opts) =
    Hint { opts | actions = Just el }


{-| Add footer content below the hint body
-}
withFooter : Html msg -> Hint msg -> Hint msg
withFooter el (Hint opts) =
    Hint { opts | footer = Just el }


{-| Append extra HTML attributes to the root hint element.
-}
withAttributes : List (Html.Attribute msg) -> Hint msg -> Hint msg
withAttributes attrs (Hint opts) =
    Hint { opts | extraAttrs = opts.extraAttrs ++ attrs }


{-| Render the Hint as an `Html msg`
-}
toMarkup : Hint msg -> Html msg
toMarkup (Hint opts) =
    let
        titleEl =
            case opts.title of
                Nothing ->
                    Html.text ""

                Just t ->
                    Html.div
                        [ Attr.class "pf-v6-c-hint__title" ]
                        [ Html.text t ]

        actionsEl =
            case opts.actions of
                Nothing ->
                    Html.text ""

                Just el ->
                    Html.div
                        [ Attr.class "pf-v6-c-hint__actions" ]
                        [ el ]

        footerEl =
            case opts.footer of
                Nothing ->
                    Html.text ""

                Just el ->
                    Html.div
                        [ Attr.class "pf-v6-c-hint__footer" ]
                        [ el ]
    in
    Html.div
        (Attr.class "pf-v6-c-hint" :: opts.extraAttrs)
        [ titleEl
        , Html.div
            [ Attr.class "pf-v6-c-hint__body" ]
            [ Html.text opts.body ]
        , actionsEl
        , footerEl
        ]
