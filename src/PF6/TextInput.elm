module PF6.TextInput exposing
    ( TextInput, InputType, Validation
    , textInput
    , withPlaceholder, withAriaLabel
    , withPasswordType, withEmailType, withSearchType, withNumberType, withTelType
    , withSuccess, withDanger, withWarning
    , withDisabled, withReadOnly
    , withAttributes
    , toMarkup
    )

{-| PF6 TextInput component

Text inputs gather free-form text input from the user.

See: <https://www.patternfly.org/components/forms/text-input>


# Definition

@docs TextInput, InputType, Validation


# Constructor

@docs textInput


# Content modifiers

@docs withPlaceholder, withAriaLabel


# Input type modifiers

@docs withPasswordType, withEmailType, withSearchType, withNumberType, withTelType


# Validation modifiers

@docs withSuccess, withDanger, withWarning


# State modifiers

@docs withDisabled, withReadOnly


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events


{-| Input type
-}
type InputType
    = Text
    | Password
    | Email
    | Search
    | Number
    | Tel


{-| Validation state
-}
type Validation
    = Default
    | Success
    | Danger
    | Warning


{-| Opaque TextInput type
-}
type TextInput msg
    = TextInput (Options msg)


type alias Options msg =
    { id : String
    , value : String
    , onChange : String -> msg
    , placeholder : String
    , ariaLabel : String
    , inputType : InputType
    , validation : Validation
    , isDisabled : Bool
    , isReadOnly : Bool
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a TextInput

    textInput
        { id = "my-input"
        , value = model.name
        , onChange = NameChanged
        }

-}
textInput : { id : String, value : String, onChange : String -> msg } -> TextInput msg
textInput { id, value, onChange } =
    TextInput
        { id = id
        , value = value
        , onChange = onChange
        , placeholder = ""
        , ariaLabel = ""
        , inputType = Text
        , validation = Default
        , isDisabled = False
        , isReadOnly = False
        , extraAttrs = []
        }


{-| Set the placeholder text
-}
withPlaceholder : String -> TextInput msg -> TextInput msg
withPlaceholder p (TextInput opts) =
    TextInput { opts | placeholder = p }


{-| Set an accessible aria-label (use when there is no visible label)
-}
withAriaLabel : String -> TextInput msg -> TextInput msg
withAriaLabel l (TextInput opts) =
    TextInput { opts | ariaLabel = l }


{-| Password input type (obscures characters)
-}
withPasswordType : TextInput msg -> TextInput msg
withPasswordType (TextInput opts) =
    TextInput { opts | inputType = Password }


{-| Email input type
-}
withEmailType : TextInput msg -> TextInput msg
withEmailType (TextInput opts) =
    TextInput { opts | inputType = Email }


{-| Search input type
-}
withSearchType : TextInput msg -> TextInput msg
withSearchType (TextInput opts) =
    TextInput { opts | inputType = Search }


{-| Number input type
-}
withNumberType : TextInput msg -> TextInput msg
withNumberType (TextInput opts) =
    TextInput { opts | inputType = Number }


{-| Telephone input type
-}
withTelType : TextInput msg -> TextInput msg
withTelType (TextInput opts) =
    TextInput { opts | inputType = Tel }


{-| Success validation state
-}
withSuccess : TextInput msg -> TextInput msg
withSuccess (TextInput opts) =
    TextInput { opts | validation = Success }


{-| Danger (error) validation state
-}
withDanger : TextInput msg -> TextInput msg
withDanger (TextInput opts) =
    TextInput { opts | validation = Danger }


{-| Warning validation state
-}
withWarning : TextInput msg -> TextInput msg
withWarning (TextInput opts) =
    TextInput { opts | validation = Warning }


{-| Disable the input
-}
withDisabled : TextInput msg -> TextInput msg
withDisabled (TextInput opts) =
    TextInput { opts | isDisabled = True }


{-| Make the input read-only
-}
withReadOnly : TextInput msg -> TextInput msg
withReadOnly (TextInput opts) =
    TextInput { opts | isReadOnly = True }


{-| Append extra HTML attributes to the root wrapper element.
-}
withAttributes : List (Html.Attribute msg) -> TextInput msg -> TextInput msg
withAttributes attrs (TextInput opts) =
    TextInput { opts | extraAttrs = opts.extraAttrs ++ attrs }


-- Internal helpers


typeAttr : InputType -> Html.Attribute msg
typeAttr t =
    case t of
        Text ->
            Attr.type_ "text"

        Password ->
            Attr.type_ "password"

        Email ->
            Attr.type_ "email"

        Search ->
            Attr.type_ "search"

        Number ->
            Attr.type_ "number"

        Tel ->
            Attr.type_ "tel"


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
    , if opts.isReadOnly then Just "pf-m-readonly" else Nothing
    ]
        |> List.filterMap identity
        |> String.join " "


{-| Render the TextInput as an `Html msg`
-}
toMarkup : TextInput msg -> Html msg
toMarkup (TextInput opts) =
    let
        inputAttrs =
            [ Attr.class "pf-v6-c-form-control__input"
            , typeAttr opts.inputType
            , Attr.id opts.id
            , Attr.value opts.value
            , Attr.disabled opts.isDisabled
            , Attr.readonly opts.isReadOnly
            , Events.onInput opts.onChange
            ]
                ++ (if String.isEmpty opts.placeholder then
                        []

                    else
                        [ Attr.placeholder opts.placeholder ]
                   )
                ++ (if String.isEmpty opts.ariaLabel then
                        []

                    else
                        [ Attr.attribute "aria-label" opts.ariaLabel ]
                   )
    in
    Html.div
        (Attr.class (wrapperClass opts) :: opts.extraAttrs)
        [ Html.input inputAttrs [] ]
