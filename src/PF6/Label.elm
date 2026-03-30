module PF6.Label exposing
    ( Label, Variant, Color
    , label
    , withFill, withOutline, withCompact
    , withBlueColor, withGreenColor, withOrangeColor, withRedColor, withPurpleColor, withCyanColor, withGoldColor
    , withIcon, withHyperlink, withCloseMsg
    , withAttributes
    , toMarkup
    )

{-| PF6 Label component

Labels are used to highlight an item's status for quick recognition,
or to flag content that needs action.

See: <https://www.patternfly.org/components/label>


# Definition

@docs Label, Variant, Color


# Constructor

@docs label


# Variant modifiers

@docs withFill, withOutline, withCompact


# Color modifiers

@docs withBlueColor, withGreenColor, withOrangeColor, withRedColor, withPurpleColor, withCyanColor, withGoldColor


# Content modifiers

@docs withIcon, withHyperlink, withCloseMsg


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events


{-| Opaque Label type
-}
type Label msg
    = Label (Options msg)


{-| Fill or outline variant
-}
type Variant
    = Fill
    | Outline
    | Compact


{-| Label color theme
-}
type Color
    = Grey
    | Blue
    | Green
    | Orange
    | Red
    | Purple
    | Cyan
    | Gold


type alias Options msg =
    { text : String
    , variant : Variant
    , color : Color
    , icon : Maybe (Html msg)
    , href : Maybe String
    , onClose : Maybe msg
    , extraAttrs : List (Html.Attribute msg)
    }


defaultOptions : String -> Options msg
defaultOptions text =
    { text = text
    , variant = Fill
    , color = Grey
    , icon = Nothing
    , href = Nothing
    , onClose = Nothing
    , extraAttrs = []
    }


{-| Construct a Label with the given text
-}
label : String -> Label msg
label text =
    Label (defaultOptions text)


{-| Filled background (default)
-}
withFill : Label msg -> Label msg
withFill (Label opts) =
    Label { opts | variant = Fill }


{-| Outlined border, transparent background
-}
withOutline : Label msg -> Label msg
withOutline (Label opts) =
    Label { opts | variant = Outline }


{-| Compact sizing — smaller padding
-}
withCompact : Label msg -> Label msg
withCompact (Label opts) =
    Label { opts | variant = Compact }


{-| Blue color theme
-}
withBlueColor : Label msg -> Label msg
withBlueColor (Label opts) =
    Label { opts | color = Blue }


{-| Green color theme
-}
withGreenColor : Label msg -> Label msg
withGreenColor (Label opts) =
    Label { opts | color = Green }


{-| Orange color theme
-}
withOrangeColor : Label msg -> Label msg
withOrangeColor (Label opts) =
    Label { opts | color = Orange }


{-| Red color theme
-}
withRedColor : Label msg -> Label msg
withRedColor (Label opts) =
    Label { opts | color = Red }


{-| Purple color theme
-}
withPurpleColor : Label msg -> Label msg
withPurpleColor (Label opts) =
    Label { opts | color = Purple }


{-| Cyan color theme
-}
withCyanColor : Label msg -> Label msg
withCyanColor (Label opts) =
    Label { opts | color = Cyan }


{-| Gold color theme
-}
withGoldColor : Label msg -> Label msg
withGoldColor (Label opts) =
    Label { opts | color = Gold }


{-| Add a leading icon
-}
withIcon : Html msg -> Label msg -> Label msg
withIcon ic (Label opts) =
    Label { opts | icon = Just ic }


{-| Make the label text a hyperlink
-}
withHyperlink : String -> Label msg -> Label msg
withHyperlink href (Label opts) =
    Label { opts | href = Just href }


{-| Add a close/dismiss button that sends msg on click
-}
withCloseMsg : msg -> Label msg -> Label msg
withCloseMsg msg (Label opts) =
    Label { opts | onClose = Just msg }


{-| Append extra HTML attributes to the root label element.
-}
withAttributes : List (Html.Attribute msg) -> Label msg -> Label msg
withAttributes attrs (Label opts) =
    Label { opts | extraAttrs = opts.extraAttrs ++ attrs }


-- Internal helpers


colorClass : Color -> Maybe String
colorClass color =
    case color of
        Grey ->
            Nothing

        Blue ->
            Just "pf-m-blue"

        Green ->
            Just "pf-m-green"

        Orange ->
            Just "pf-m-orange"

        Red ->
            Just "pf-m-red"

        Purple ->
            Just "pf-m-purple"

        Cyan ->
            Just "pf-m-cyan"

        Gold ->
            Just "pf-m-gold"


labelClasses : Options msg -> String
labelClasses opts =
    [ Just "pf-v6-c-label"
    , colorClass opts.color
    , case opts.variant of
        Fill ->
            Nothing

        Outline ->
            Just "pf-m-outline"

        Compact ->
            Just "pf-m-compact"
    ]
        |> List.filterMap identity
        |> String.join " "


contentInner : Options msg -> Html msg
contentInner opts =
    let
        iconEl =
            case opts.icon of
                Nothing ->
                    Html.text ""

                Just ic ->
                    Html.span
                        [ Attr.class "pf-v6-c-label__icon" ]
                        [ ic ]

        textEl =
            Html.span
                [ Attr.class "pf-v6-c-label__text" ]
                [ Html.text opts.text ]
    in
    case opts.href of
        Nothing ->
            Html.span
                [ Attr.class "pf-v6-c-label__content" ]
                [ iconEl, textEl ]

        Just href ->
            Html.a
                [ Attr.class "pf-v6-c-label__content"
                , Attr.href href
                ]
                [ iconEl, textEl ]


{-| Render the Label as an `Html msg`
-}
toMarkup : Label msg -> Html msg
toMarkup (Label opts) =
    let
        closeEl =
            case opts.onClose of
                Nothing ->
                    Html.text ""

                Just msg ->
                    Html.button
                        [ Attr.class "pf-v6-c-label__action"
                        , Attr.type_ "button"
                        , Attr.attribute "aria-label" "Remove label"
                        , Events.onClick msg
                        ]
                        [ Html.text "×" ]
    in
    Html.span
        (Attr.class (labelClasses opts) :: opts.extraAttrs)
        [ contentInner opts
        , closeEl
        ]
