module PF6.NumberInput exposing
    ( NumberInput
    , numberInput
    , withMin, withMax, withStep
    , withUnit, withAriaLabel
    , withSuccess, withDanger, withWarning
    , withDisabled
    , withAttributes
    , toMarkup
    )

{-| PF6 NumberInput component

NumberInput allows users to enter a numeric value with stepper controls.

See: <https://www.patternfly.org/components/number-input>


# Definition

@docs NumberInput


# Constructor

@docs numberInput


# Range modifiers

@docs withMin, withMax, withStep


# Content modifiers

@docs withUnit, withAriaLabel


# Validation modifiers

@docs withSuccess, withDanger, withWarning


# State modifiers

@docs withDisabled


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events


{-| Opaque NumberInput type
-}
type NumberInput msg
    = NumberInput (Options msg)


type Validation
    = Default
    | Success
    | Danger
    | Warning


type alias Options msg =
    { value : Int
    , onChange : Int -> msg
    , min : Maybe Int
    , max : Maybe Int
    , step : Int
    , unit : String
    , ariaLabel : String
    , validation : Validation
    , isDisabled : Bool
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a NumberInput

    numberInput
        { value = model.quantity
        , onChange = QuantityChanged
        }

-}
numberInput : { value : Int, onChange : Int -> msg } -> NumberInput msg
numberInput { value, onChange } =
    NumberInput
        { value = value
        , onChange = onChange
        , min = Nothing
        , max = Nothing
        , step = 1
        , unit = ""
        , ariaLabel = "Number input"
        , validation = Default
        , isDisabled = False
        , extraAttrs = []
        }


{-| Set the minimum allowed value
-}
withMin : Int -> NumberInput msg -> NumberInput msg
withMin n (NumberInput opts) =
    NumberInput { opts | min = Just n }


{-| Set the maximum allowed value
-}
withMax : Int -> NumberInput msg -> NumberInput msg
withMax n (NumberInput opts) =
    NumberInput { opts | max = Just n }


{-| Set the step increment (default 1)
-}
withStep : Int -> NumberInput msg -> NumberInput msg
withStep n (NumberInput opts) =
    NumberInput { opts | step = n }


{-| Set a unit label (e.g. "GB", "%", "items")
-}
withUnit : String -> NumberInput msg -> NumberInput msg
withUnit u (NumberInput opts) =
    NumberInput { opts | unit = u }


{-| Set an aria-label for the input
-}
withAriaLabel : String -> NumberInput msg -> NumberInput msg
withAriaLabel l (NumberInput opts) =
    NumberInput { opts | ariaLabel = l }


{-| Success validation state
-}
withSuccess : NumberInput msg -> NumberInput msg
withSuccess (NumberInput opts) =
    NumberInput { opts | validation = Success }


{-| Danger (error) validation state
-}
withDanger : NumberInput msg -> NumberInput msg
withDanger (NumberInput opts) =
    NumberInput { opts | validation = Danger }


{-| Warning validation state
-}
withWarning : NumberInput msg -> NumberInput msg
withWarning (NumberInput opts) =
    NumberInput { opts | validation = Warning }


{-| Disable the number input
-}
withDisabled : NumberInput msg -> NumberInput msg
withDisabled (NumberInput opts) =
    NumberInput { opts | isDisabled = True }


{-| Append extra HTML attributes to the root element.
-}
withAttributes : List (Html.Attribute msg) -> NumberInput msg -> NumberInput msg
withAttributes attrs (NumberInput opts) =
    NumberInput { opts | extraAttrs = opts.extraAttrs ++ attrs }


-- Internal helpers


canDecrement : Options msg -> Bool
canDecrement opts =
    not opts.isDisabled
        && (case opts.min of
                Nothing ->
                    True

                Just m ->
                    opts.value > m
           )


canIncrement : Options msg -> Bool
canIncrement opts =
    not opts.isDisabled
        && (case opts.max of
                Nothing ->
                    True

                Just m ->
                    opts.value < m
           )


validationClass : Validation -> Maybe String
validationClass v =
    case v of
        Default ->
            Nothing

        Success ->
            Just "pf-m-success"

        Danger ->
            Just "pf-m-error"

        Warning ->
            Just "pf-m-warning"


inputClass : Options msg -> String
inputClass opts =
    [ Just "pf-v6-c-form-control"
    , validationClass opts.validation
    ]
        |> List.filterMap identity
        |> String.join " "


{-| Render the NumberInput as an `Html msg`
-}
toMarkup : NumberInput msg -> Html msg
toMarkup (NumberInput opts) =
    let
        minusDisabled =
            not (canDecrement opts)

        plusDisabled =
            not (canIncrement opts)

        minusEl =
            Html.button
                [ Attr.class "pf-v6-c-button pf-m-control"
                , Attr.type_ "button"
                , Attr.disabled minusDisabled
                , Attr.attribute "aria-label" "Minus"
                , Events.onClick (opts.onChange (opts.value - opts.step))
                ]
                [ Html.i
                    [ Attr.class "fas fa-minus"
                    , Attr.attribute "aria-hidden" "true"
                    ]
                    []
                ]

        inputEl =
            Html.input
                ([ Attr.class (inputClass opts)
                 , Attr.type_ "number"
                 , Attr.value (String.fromInt opts.value)
                 , Attr.attribute "aria-label" opts.ariaLabel
                 , Attr.disabled opts.isDisabled
                 , Attr.step (String.fromInt opts.step)
                 , Events.onInput
                    (\s ->
                        String.toInt s
                            |> Maybe.withDefault opts.value
                            |> opts.onChange
                    )
                 ]
                    ++ (case opts.min of
                            Nothing ->
                                []

                            Just m ->
                                [ Attr.min (String.fromInt m) ]
                       )
                    ++ (case opts.max of
                            Nothing ->
                                []

                            Just m ->
                                [ Attr.max (String.fromInt m) ]
                       )
                )
                []

        plusEl =
            Html.button
                [ Attr.class "pf-v6-c-button pf-m-control"
                , Attr.type_ "button"
                , Attr.disabled plusDisabled
                , Attr.attribute "aria-label" "Plus"
                , Events.onClick (opts.onChange (opts.value + opts.step))
                ]
                [ Html.i
                    [ Attr.class "fas fa-plus"
                    , Attr.attribute "aria-hidden" "true"
                    ]
                    []
                ]

        unitEl =
            if String.isEmpty opts.unit then
                Html.text ""

            else
                Html.span
                    [ Attr.class "pf-v6-c-number-input__unit" ]
                    [ Html.text opts.unit ]
    in
    Html.div
        (Attr.class "pf-v6-c-number-input" :: opts.extraAttrs)
        [ minusEl, inputEl, plusEl, unitEl ]
