module PF6.SearchInput exposing
    ( SearchInput
    , searchInput
    , withPlaceholder, withAriaLabel
    , withClearMsg, withSubmitMsg
    , withAttributes
    , toMarkup
    )

{-| PF6 SearchInput component

SearchInput provides a text field for search with clear and optional submit actions.

See: <https://www.patternfly.org/components/search-input>


# Definition

@docs SearchInput


# Constructor

@docs searchInput


# Content modifiers

@docs withPlaceholder, withAriaLabel


# Action modifiers

@docs withClearMsg, withSubmitMsg


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events


{-| Opaque SearchInput type
-}
type SearchInput msg
    = SearchInput (Options msg)


type alias Options msg =
    { value : String
    , onChange : String -> msg
    , placeholder : String
    , ariaLabel : String
    , onClear : Maybe msg
    , onSubmit : Maybe msg
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a SearchInput

    searchInput
        { value = model.query
        , onChange = QueryChanged
        }

-}
searchInput : { value : String, onChange : String -> msg } -> SearchInput msg
searchInput { value, onChange } =
    SearchInput
        { value = value
        , onChange = onChange
        , placeholder = "Search"
        , ariaLabel = "Search"
        , onClear = Nothing
        , onSubmit = Nothing
        , extraAttrs = []
        }


{-| Set the placeholder text
-}
withPlaceholder : String -> SearchInput msg -> SearchInput msg
withPlaceholder p (SearchInput opts) =
    SearchInput { opts | placeholder = p }


{-| Set the aria-label for the input
-}
withAriaLabel : String -> SearchInput msg -> SearchInput msg
withAriaLabel l (SearchInput opts) =
    SearchInput { opts | ariaLabel = l }


{-| Show a clear button that sends this msg when clicked
-}
withClearMsg : msg -> SearchInput msg -> SearchInput msg
withClearMsg msg (SearchInput opts) =
    SearchInput { opts | onClear = Just msg }


{-| Show a submit button that sends this msg when clicked
-}
withSubmitMsg : msg -> SearchInput msg -> SearchInput msg
withSubmitMsg msg (SearchInput opts) =
    SearchInput { opts | onSubmit = Just msg }


{-| Append extra HTML attributes to the root element.
-}
withAttributes : List (Html.Attribute msg) -> SearchInput msg -> SearchInput msg
withAttributes attrs (SearchInput opts) =
    SearchInput { opts | extraAttrs = opts.extraAttrs ++ attrs }


{-| Render the SearchInput as an `Html msg`
-}
toMarkup : SearchInput msg -> Html msg
toMarkup (SearchInput opts) =
    let
        clearEl =
            case opts.onClear of
                Nothing ->
                    Html.text ""

                Just msg ->
                    Html.span
                        [ Attr.class "pf-v6-c-search-input__clear" ]
                        [ Html.button
                            [ Attr.class "pf-v6-c-button pf-m-plain"
                            , Attr.type_ "button"
                            , Attr.attribute "aria-label" "Clear search"
                            , Events.onClick msg
                            ]
                            [ Html.text "×" ]
                        ]

        submitEl =
            case opts.onSubmit of
                Nothing ->
                    Html.text ""

                Just msg ->
                    Html.span
                        [ Attr.class "pf-v6-c-search-input__search" ]
                        [ Html.button
                            [ Attr.class "pf-v6-c-button pf-m-control"
                            , Attr.type_ "button"
                            , Attr.attribute "aria-label" "Search"
                            , Events.onClick msg
                            ]
                            [ Html.i
                                [ Attr.class "fas fa-search"
                                , Attr.attribute "aria-hidden" "true"
                                ]
                                []
                            ]
                        ]

        hasUtilities =
            opts.onClear /= Nothing || opts.onSubmit /= Nothing

        utilitiesEl =
            if hasUtilities then
                Html.span
                    [ Attr.class "pf-v6-c-search-input__utilities" ]
                    [ submitEl, clearEl ]

            else
                Html.text ""
    in
    Html.div
        (Attr.class "pf-v6-c-search-input" :: opts.extraAttrs)
        [ Html.div
            [ Attr.class "pf-v6-c-search-input__bar" ]
            [ Html.span
                [ Attr.class "pf-v6-c-search-input__text" ]
                [ Html.span
                    [ Attr.class "pf-v6-c-search-input__icon" ]
                    [ Html.i
                        [ Attr.class "fas fa-search"
                        , Attr.attribute "aria-hidden" "true"
                        ]
                        []
                    ]
                , Html.input
                    [ Attr.class "pf-v6-c-search-input__text-input"
                    , Attr.type_ "search"
                    , Attr.value opts.value
                    , Attr.placeholder opts.placeholder
                    , Attr.attribute "aria-label" opts.ariaLabel
                    , Events.onInput opts.onChange
                    ]
                    []
                ]
            , utilitiesEl
            ]
        ]
