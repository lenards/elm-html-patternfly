module PF6.Checkbox exposing
    ( Checkbox
    , checkbox
    , withLabel, withDescription, withBody
    , withChecked, withIndeterminate, withDisabled
    , withAttributes
    , toMarkup
    )

{-| PF6 Checkbox component

Checkboxes allow users to select one or more items from a list.

See: <https://www.patternfly.org/components/checkbox>


# Definition

@docs Checkbox


# Constructor

@docs checkbox


# Content modifiers

@docs withLabel, withDescription, withBody


# State modifiers

@docs withChecked, withIndeterminate, withDisabled


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events


{-| Opaque Checkbox type
-}
type Checkbox msg
    = Checkbox (Options msg)


type alias Options msg =
    { id : String
    , isChecked : Bool
    , isIndeterminate : Bool
    , isDisabled : Bool
    , label : String
    , description : Maybe String
    , body : Maybe (Html msg)
    , onChange : Bool -> msg
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a Checkbox

    checkbox
        { id = "my-check"
        , onChange = CheckboxToggled
        }

-}
checkbox : { id : String, onChange : Bool -> msg } -> Checkbox msg
checkbox { id, onChange } =
    Checkbox
        { id = id
        , isChecked = False
        , isIndeterminate = False
        , isDisabled = False
        , label = ""
        , description = Nothing
        , body = Nothing
        , onChange = onChange
        , extraAttrs = []
        }


{-| Set the checkbox label
-}
withLabel : String -> Checkbox msg -> Checkbox msg
withLabel l (Checkbox opts) =
    Checkbox { opts | label = l }


{-| Set a description below the label
-}
withDescription : String -> Checkbox msg -> Checkbox msg
withDescription d (Checkbox opts) =
    Checkbox { opts | description = Just d }


{-| Set body content below the checkbox
-}
withBody : Html msg -> Checkbox msg -> Checkbox msg
withBody el (Checkbox opts) =
    Checkbox { opts | body = Just el }


{-| Set the checked state
-}
withChecked : Bool -> Checkbox msg -> Checkbox msg
withChecked checked (Checkbox opts) =
    Checkbox { opts | isChecked = checked }


{-| Set indeterminate state (partially checked).

The visual indeterminate appearance requires setting the `indeterminate` DOM
property, which cannot be done via HTML attributes alone. Use `withAttributes`
to add it via a port, or use the escape hatch:

    import Html.Attributes exposing (property)
    import Json.Encode as Encode

    Checkbox.checkbox { id = "cb", onChange = Toggle }
        |> Checkbox.withIndeterminate
        |> Checkbox.withAttributes [ property "indeterminate" (Encode.bool True) ]
        |> Checkbox.toMarkup

`aria-checked="mixed"` is set automatically for screen reader support.

-}
withIndeterminate : Checkbox msg -> Checkbox msg
withIndeterminate (Checkbox opts) =
    Checkbox { opts | isIndeterminate = True, isChecked = False }


{-| Disable the checkbox
-}
withDisabled : Checkbox msg -> Checkbox msg
withDisabled (Checkbox opts) =
    Checkbox { opts | isDisabled = True }


{-| Append extra HTML attributes to the root wrapper element.
-}
withAttributes : List (Html.Attribute msg) -> Checkbox msg -> Checkbox msg
withAttributes attrs (Checkbox opts) =
    Checkbox { opts | extraAttrs = opts.extraAttrs ++ attrs }


-- Internal helpers


inputAttrs : Options msg -> List (Html.Attribute msg)
inputAttrs opts =
    [ Attr.class "pf-v6-c-check__input"
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
        ++ (if opts.isIndeterminate then
                [ Attr.attribute "aria-checked" "mixed" ]

            else
                []
           )


{-| Render the Checkbox as an `Html msg`
-}
toMarkup : Checkbox msg -> Html msg
toMarkup (Checkbox opts) =
    let
        descriptionEl =
            case opts.description of
                Nothing ->
                    Html.text ""

                Just d ->
                    Html.span
                        [ Attr.class "pf-v6-c-check__description" ]
                        [ Html.text d ]

        bodyEl =
            case opts.body of
                Nothing ->
                    Html.text ""

                Just el ->
                    Html.span
                        [ Attr.class "pf-v6-c-check__body" ]
                        [ el ]
    in
    Html.div
        (Attr.class "pf-v6-c-check" :: opts.extraAttrs)
        [ Html.input (inputAttrs opts) []
        , Html.label
            [ Attr.class
                (if opts.isDisabled then
                    "pf-v6-c-check__label pf-m-disabled"

                 else
                    "pf-v6-c-check__label"
                )
            , Attr.for opts.id
            ]
            [ Html.text opts.label ]
        , descriptionEl
        , bodyEl
        ]
