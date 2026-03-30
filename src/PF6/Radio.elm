module PF6.Radio exposing
    ( Radio
    , radio
    , withLabel, withDescription, withBody
    , withChecked, withDisabled
    , withAttributes
    , toMarkup
    )

{-| PF6 Radio component

Radio buttons allow users to select one option from a set.

See: <https://www.patternfly.org/components/forms/radio>


# Definition

@docs Radio


# Constructor

@docs radio


# Content modifiers

@docs withLabel, withDescription, withBody


# State modifiers

@docs withChecked, withDisabled


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events


{-| Opaque Radio type
-}
type Radio msg
    = Radio (Options msg)


type alias Options msg =
    { id : String
    , name : String
    , isChecked : Bool
    , isDisabled : Bool
    , label : String
    , description : Maybe String
    , body : Maybe (Html msg)
    , onChange : Bool -> msg
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a Radio button

    radio
        { id = "my-radio"
        , name = "my-group"
        , onChange = RadioSelected
        }

-}
radio : { id : String, name : String, onChange : Bool -> msg } -> Radio msg
radio { id, name, onChange } =
    Radio
        { id = id
        , name = name
        , isChecked = False
        , isDisabled = False
        , label = ""
        , description = Nothing
        , body = Nothing
        , onChange = onChange
        , extraAttrs = []
        }


{-| Set the radio label
-}
withLabel : String -> Radio msg -> Radio msg
withLabel l (Radio opts) =
    Radio { opts | label = l }


{-| Set a description below the label
-}
withDescription : String -> Radio msg -> Radio msg
withDescription d (Radio opts) =
    Radio { opts | description = Just d }


{-| Set body content below the radio
-}
withBody : Html msg -> Radio msg -> Radio msg
withBody el (Radio opts) =
    Radio { opts | body = Just el }


{-| Set the checked state
-}
withChecked : Bool -> Radio msg -> Radio msg
withChecked checked (Radio opts) =
    Radio { opts | isChecked = checked }


{-| Disable the radio button
-}
withDisabled : Radio msg -> Radio msg
withDisabled (Radio opts) =
    Radio { opts | isDisabled = True }


{-| Append extra HTML attributes to the root wrapper element.
-}
withAttributes : List (Html.Attribute msg) -> Radio msg -> Radio msg
withAttributes attrs (Radio opts) =
    Radio { opts | extraAttrs = opts.extraAttrs ++ attrs }


{-| Render the Radio as an `Html msg`
-}
toMarkup : Radio msg -> Html msg
toMarkup (Radio opts) =
    let
        descriptionEl =
            case opts.description of
                Nothing ->
                    Html.text ""

                Just d ->
                    Html.span
                        [ Attr.class "pf-v6-c-radio__description" ]
                        [ Html.text d ]

        bodyEl =
            case opts.body of
                Nothing ->
                    Html.text ""

                Just el ->
                    Html.span
                        [ Attr.class "pf-v6-c-radio__body" ]
                        [ el ]
    in
    Html.div
        (Attr.class "pf-v6-c-radio" :: opts.extraAttrs)
        [ Html.input
            [ Attr.class "pf-v6-c-radio__input"
            , Attr.type_ "radio"
            , Attr.id opts.id
            , Attr.name opts.name
            , Attr.checked opts.isChecked
            , Attr.disabled opts.isDisabled
            , Events.onCheck
                (if opts.isDisabled then
                    \_ -> opts.onChange opts.isChecked

                 else
                    opts.onChange
                )
            ]
            []
        , Html.label
            [ Attr.class
                (if opts.isDisabled then
                    "pf-v6-c-radio__label pf-m-disabled"

                 else
                    "pf-v6-c-radio__label"
                )
            , Attr.for opts.id
            ]
            [ Html.text opts.label ]
        , descriptionEl
        , bodyEl
        ]
