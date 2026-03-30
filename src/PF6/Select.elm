module PF6.Select exposing
    ( Select, SelectOption
    , select
    , option, optionDisabled
    , withPlaceholder, withAriaLabel
    , withSuccess, withDanger, withWarning
    , withDisabled
    , withAttributes
    , toMarkup
    )

{-| PF6 Select component

A native HTML select for choosing a single value from a list.

See: <https://www.patternfly.org/components/menus/select>


# Definition

@docs Select, SelectOption


# Constructor

@docs select


# Option constructors

@docs option, optionDisabled


# Content modifiers

@docs withPlaceholder, withAriaLabel


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


{-| A select option
-}
type SelectOption
    = SelectOption
        { value : String
        , label : String
        , isDisabled : Bool
        }


{-| Opaque Select type
-}
type Select msg
    = Select (Options msg)


type Validation
    = Default
    | Success
    | Danger
    | Warning


type alias Options msg =
    { id : String
    , value : String
    , onChange : String -> msg
    , options : List SelectOption
    , placeholder : String
    , ariaLabel : String
    , validation : Validation
    , isDisabled : Bool
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a Select

    select
        { id = "my-select"
        , value = model.selectedColor
        , onChange = ColorSelected
        , options =
            [ Select.option "red" "Red"
            , Select.option "blue" "Blue"
            , Select.option "green" "Green"
            ]
        }

-}
select :
    { id : String
    , value : String
    , onChange : String -> msg
    , options : List SelectOption
    }
    -> Select msg
select args =
    Select
        { id = args.id
        , value = args.value
        , onChange = args.onChange
        , options = args.options
        , placeholder = ""
        , ariaLabel = ""
        , validation = Default
        , isDisabled = False
        , extraAttrs = []
        }


{-| Create a select option with a value and display label
-}
option : String -> String -> SelectOption
option value label =
    SelectOption { value = value, label = label, isDisabled = False }


{-| Create a disabled select option
-}
optionDisabled : String -> String -> SelectOption
optionDisabled value label =
    SelectOption { value = value, label = label, isDisabled = True }


{-| Set placeholder text (rendered as the first disabled option)
-}
withPlaceholder : String -> Select msg -> Select msg
withPlaceholder p (Select opts) =
    Select { opts | placeholder = p }


{-| Set an accessible aria-label
-}
withAriaLabel : String -> Select msg -> Select msg
withAriaLabel l (Select opts) =
    Select { opts | ariaLabel = l }


{-| Success validation state
-}
withSuccess : Select msg -> Select msg
withSuccess (Select opts) =
    Select { opts | validation = Success }


{-| Danger (error) validation state
-}
withDanger : Select msg -> Select msg
withDanger (Select opts) =
    Select { opts | validation = Danger }


{-| Warning validation state
-}
withWarning : Select msg -> Select msg
withWarning (Select opts) =
    Select { opts | validation = Warning }


{-| Disable the select
-}
withDisabled : Select msg -> Select msg
withDisabled (Select opts) =
    Select { opts | isDisabled = True }


{-| Append extra HTML attributes to the root wrapper element.
-}
withAttributes : List (Html.Attribute msg) -> Select msg -> Select msg
withAttributes attrs (Select opts) =
    Select { opts | extraAttrs = opts.extraAttrs ++ attrs }


-- Internal helpers


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


wrapperClass : Options msg -> String
wrapperClass opts =
    [ Just "pf-v6-c-form-control"
    , validationClass opts.validation
    , if opts.isDisabled then Just "pf-m-disabled" else Nothing
    ]
        |> List.filterMap identity
        |> String.join " "


renderOption : String -> SelectOption -> Html msg
renderOption currentValue (SelectOption o) =
    Html.option
        [ Attr.value o.value
        , Attr.selected (o.value == currentValue)
        , Attr.disabled o.isDisabled
        ]
        [ Html.text o.label ]


{-| Render the Select as an `Html msg`
-}
toMarkup : Select msg -> Html msg
toMarkup (Select opts) =
    let
        placeholderOption =
            if String.isEmpty opts.placeholder then
                []

            else
                [ Html.option
                    [ Attr.value ""
                    , Attr.disabled True
                    , Attr.selected (String.isEmpty opts.value)
                    ]
                    [ Html.text opts.placeholder ]
                ]

        selectAttrs =
            [ Attr.class "pf-v6-c-form-control__select"
            , Attr.id opts.id
            , Attr.disabled opts.isDisabled
            , Events.onInput opts.onChange
            ]
                ++ (if String.isEmpty opts.ariaLabel then
                        []

                    else
                        [ Attr.attribute "aria-label" opts.ariaLabel ]
                   )
    in
    Html.div
        (Attr.class (wrapperClass opts) :: opts.extraAttrs)
        [ Html.select selectAttrs
            (placeholderOption ++ List.map (renderOption opts.value) opts.options)
        ]
