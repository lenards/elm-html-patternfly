module PF6.Switch exposing
    ( Switch
    , switch
    , withLabel, withLabelOff, withAriaLabel
    , withChecked, withDisabled, withReversed
    , withAttributes
    , toMarkup
    )

{-| PF6 Switch component

A switch toggles the state of a setting on or off.

See: <https://www.patternfly.org/components/switch>


# Definition

@docs Switch


# Constructor

@docs switch


# Content modifiers

@docs withLabel, withLabelOff, withAriaLabel


# State modifiers

@docs withChecked, withDisabled, withReversed


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events


{-| Opaque Switch type
-}
type Switch msg
    = Switch (Options msg)


type alias Options msg =
    { id : String
    , isChecked : Bool
    , isDisabled : Bool
    , isReversed : Bool
    , labelOn : String
    , labelOff : String
    , ariaLabel : String
    , onChange : Bool -> msg
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a Switch

    switch
        { id = "my-switch"
        , onChange = SwitchToggled
        }

-}
switch : { id : String, onChange : Bool -> msg } -> Switch msg
switch { id, onChange } =
    Switch
        { id = id
        , isChecked = False
        , isDisabled = False
        , isReversed = False
        , labelOn = ""
        , labelOff = ""
        , ariaLabel = ""
        , onChange = onChange
        , extraAttrs = []
        }


{-| Set the label shown when the switch is on

If set without `withLabelOff`, the same label is used for both states.

-}
withLabel : String -> Switch msg -> Switch msg
withLabel l (Switch opts) =
    Switch { opts | labelOn = l }


{-| Set the label shown when the switch is off
-}
withLabelOff : String -> Switch msg -> Switch msg
withLabelOff l (Switch opts) =
    Switch { opts | labelOff = l }


{-| Set an aria-label for the checkbox input (for screen readers)
-}
withAriaLabel : String -> Switch msg -> Switch msg
withAriaLabel l (Switch opts) =
    Switch { opts | ariaLabel = l }


{-| Set the checked state
-}
withChecked : Bool -> Switch msg -> Switch msg
withChecked checked (Switch opts) =
    Switch { opts | isChecked = checked }


{-| Disable the switch
-}
withDisabled : Switch msg -> Switch msg
withDisabled (Switch opts) =
    Switch { opts | isDisabled = True }


{-| Reverse the layout — label first, toggle second
-}
withReversed : Switch msg -> Switch msg
withReversed (Switch opts) =
    Switch { opts | isReversed = True }


{-| Append extra HTML attributes to the root label element.
-}
withAttributes : List (Html.Attribute msg) -> Switch msg -> Switch msg
withAttributes attrs (Switch opts) =
    Switch { opts | extraAttrs = opts.extraAttrs ++ attrs }


{-| Render the Switch as an `Html msg`
-}
toMarkup : Switch msg -> Html msg
toMarkup (Switch opts) =
    let
        rootClass =
            [ Just "pf-v6-c-switch"
            , if opts.isReversed then Just "pf-m-reverse" else Nothing
            ]
                |> List.filterMap identity
                |> String.join " "

        inputEl =
            Html.input
                ([ Attr.class "pf-v6-c-switch__input"
                 , Attr.type_ "checkbox"
                 , Attr.id opts.id
                 , Attr.checked opts.isChecked
                 , Attr.disabled opts.isDisabled
                 , Events.onCheck
                    (if opts.isDisabled then
                        \_ -> opts.onChange opts.isChecked

                     else
                        opts.onChange
                    )
                 ]
                    ++ (if String.isEmpty opts.ariaLabel then
                            []

                        else
                            [ Attr.attribute "aria-label" opts.ariaLabel ]
                       )
                )
                []

        toggleEl =
            Html.span
                [ Attr.class "pf-v6-c-switch__toggle" ]
                [ Html.span [ Attr.class "pf-v6-c-switch__toggle-icon" ] [] ]

        activeLabel =
            if opts.isChecked then
                opts.labelOn

            else if String.isEmpty opts.labelOff then
                opts.labelOn

            else
                opts.labelOff

        labelEl =
            if String.isEmpty opts.labelOn && String.isEmpty opts.labelOff then
                Html.text ""

            else
                Html.span
                    [ Attr.class "pf-v6-c-switch__label" ]
                    [ Html.text activeLabel ]
    in
    Html.label
        (Attr.class rootClass
            :: Attr.for opts.id
            :: opts.extraAttrs
        )
        [ inputEl, toggleEl, labelEl ]
