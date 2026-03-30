module PF6.TextArea exposing
    ( TextArea, Resize
    , textArea
    , withPlaceholder, withAriaLabel, withRows
    , withResizeVertical, withResizeHorizontal, withResizeBoth, withResizeNone
    , withSuccess, withDanger, withWarning
    , withDisabled, withReadOnly, withRequired
    , withAttributes
    , toMarkup
    )

{-| PF6 TextArea component

A multi-line text input for gathering longer text content.

See: <https://www.patternfly.org/components/forms/text-area>


# Definition

@docs TextArea, Resize


# Constructor

@docs textArea


# Content modifiers

@docs withPlaceholder, withAriaLabel, withRows


# Resize modifiers

@docs withResizeVertical, withResizeHorizontal, withResizeBoth, withResizeNone


# Validation modifiers

@docs withSuccess, withDanger, withWarning


# State modifiers

@docs withDisabled, withReadOnly, withRequired


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events


{-| Resize direction
-}
type Resize
    = ResizeDefault
    | ResizeVertical
    | ResizeHorizontal
    | ResizeBoth
    | ResizeNone


{-| Opaque TextArea type
-}
type TextArea msg
    = TextArea (Options msg)


type Validation
    = Default
    | Success
    | Danger
    | Warning


type alias Options msg =
    { id : String
    , value : String
    , onChange : String -> msg
    , placeholder : String
    , ariaLabel : String
    , rows : Maybe Int
    , resize : Resize
    , validation : Validation
    , isDisabled : Bool
    , isReadOnly : Bool
    , isRequired : Bool
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a TextArea

    textArea
        { id = "my-textarea"
        , value = model.description
        , onChange = DescriptionChanged
        }

-}
textArea : { id : String, value : String, onChange : String -> msg } -> TextArea msg
textArea { id, value, onChange } =
    TextArea
        { id = id
        , value = value
        , onChange = onChange
        , placeholder = ""
        , ariaLabel = ""
        , rows = Nothing
        , resize = ResizeDefault
        , validation = Default
        , isDisabled = False
        , isReadOnly = False
        , isRequired = False
        , extraAttrs = []
        }


{-| Set the placeholder text
-}
withPlaceholder : String -> TextArea msg -> TextArea msg
withPlaceholder p (TextArea opts) =
    TextArea { opts | placeholder = p }


{-| Set an accessible aria-label
-}
withAriaLabel : String -> TextArea msg -> TextArea msg
withAriaLabel l (TextArea opts) =
    TextArea { opts | ariaLabel = l }


{-| Set the visible row count
-}
withRows : Int -> TextArea msg -> TextArea msg
withRows n (TextArea opts) =
    TextArea { opts | rows = Just n }


{-| Allow vertical resize only
-}
withResizeVertical : TextArea msg -> TextArea msg
withResizeVertical (TextArea opts) =
    TextArea { opts | resize = ResizeVertical }


{-| Allow horizontal resize only
-}
withResizeHorizontal : TextArea msg -> TextArea msg
withResizeHorizontal (TextArea opts) =
    TextArea { opts | resize = ResizeHorizontal }


{-| Allow resize in both directions
-}
withResizeBoth : TextArea msg -> TextArea msg
withResizeBoth (TextArea opts) =
    TextArea { opts | resize = ResizeBoth }


{-| Disable resize
-}
withResizeNone : TextArea msg -> TextArea msg
withResizeNone (TextArea opts) =
    TextArea { opts | resize = ResizeNone }


{-| Success validation state
-}
withSuccess : TextArea msg -> TextArea msg
withSuccess (TextArea opts) =
    TextArea { opts | validation = Success }


{-| Danger (error) validation state
-}
withDanger : TextArea msg -> TextArea msg
withDanger (TextArea opts) =
    TextArea { opts | validation = Danger }


{-| Warning validation state
-}
withWarning : TextArea msg -> TextArea msg
withWarning (TextArea opts) =
    TextArea { opts | validation = Warning }


{-| Disable the textarea
-}
withDisabled : TextArea msg -> TextArea msg
withDisabled (TextArea opts) =
    TextArea { opts | isDisabled = True }


{-| Make the textarea read-only
-}
withReadOnly : TextArea msg -> TextArea msg
withReadOnly (TextArea opts) =
    TextArea { opts | isReadOnly = True }


{-| Mark the textarea as required
-}
withRequired : TextArea msg -> TextArea msg
withRequired (TextArea opts) =
    TextArea { opts | isRequired = True }


{-| Append extra HTML attributes to the root wrapper element.
-}
withAttributes : List (Html.Attribute msg) -> TextArea msg -> TextArea msg
withAttributes attrs (TextArea opts) =
    TextArea { opts | extraAttrs = opts.extraAttrs ++ attrs }


-- Internal helpers


resizeClass : Resize -> Maybe String
resizeClass r =
    case r of
        ResizeDefault ->
            Nothing

        ResizeVertical ->
            Just "pf-m-resize-vertical"

        ResizeHorizontal ->
            Just "pf-m-resize-horizontal"

        ResizeBoth ->
            Just "pf-m-resize-both"

        ResizeNone ->
            Just "pf-m-resize-none"


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
    , resizeClass opts.resize
    , if opts.isDisabled then Just "pf-m-disabled" else Nothing
    , if opts.isReadOnly then Just "pf-m-readonly" else Nothing
    ]
        |> List.filterMap identity
        |> String.join " "


{-| Render the TextArea as an `Html msg`
-}
toMarkup : TextArea msg -> Html msg
toMarkup (TextArea opts) =
    let
        textareaAttrs =
            [ Attr.class "pf-v6-c-form-control__textarea"
            , Attr.id opts.id
            , Attr.value opts.value
            , Attr.disabled opts.isDisabled
            , Attr.readonly opts.isReadOnly
            , Attr.required opts.isRequired
            , Events.onInput opts.onChange
            ]
                ++ (case opts.rows of
                        Nothing ->
                            []

                        Just n ->
                            [ Attr.rows n ]
                   )
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
        [ Html.textarea textareaAttrs [] ]
