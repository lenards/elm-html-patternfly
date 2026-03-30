module PF6.Button exposing
    ( Button, Variant, Size, Position
    , button, primary, secondary, tertiary, danger, warning, plain, link, control
    , withSmallSize, withLargeSize, withDefaultSize
    , withIcon, withIconLeft, withIconRight
    , withDisabled
    , withAttributes
    , toMarkup
    )

{-| PF6 Button component

Supports all PF6 button variants, sizes, and icon positions.

See: <https://www.patternfly.org/components/button>


# Definition

@docs Button, Variant, Size, Position


# Constructor functions

@docs button, primary, secondary, tertiary, danger, warning, plain, link, control


# Size modifiers

@docs withSmallSize, withLargeSize, withDefaultSize


# Icon modifiers

@docs withIcon, withIconLeft, withIconRight


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


{-| Opaque Button type
-}
type Button msg
    = Button (Options msg)


{-| Button visual variants
-}
type Variant
    = Primary
    | Secondary
    | Tertiary
    | Danger
    | Warning
    | Plain
    | Link
    | Control


{-| Button size
-}
type Size
    = Default
    | Small
    | Large


{-| Icon position relative to label text
-}
type Position
    = Left
    | Right


type alias IconEl msg =
    { element : Html msg
    , position : Position
    }


type alias Options msg =
    { label : String
    , variant : Variant
    , size : Size
    , icon : Maybe (IconEl msg)
    , onPress : Maybe msg
    , isDisabled : Bool
    , extraAttrs : List (Html.Attribute msg)
    }


defaultOptions : { label : String, onPress : Maybe msg } -> Options msg
defaultOptions { label, onPress } =
    { label = label
    , onPress = onPress
    , variant = Plain
    , size = Default
    , icon = Nothing
    , isDisabled = False
    , extraAttrs = []
    }


construct : { label : String, onPress : Maybe msg } -> Variant -> Button msg
construct args variant =
    let
        defaults =
            defaultOptions args
    in
    Button { defaults | variant = variant }


{-| Plain button (no variant styling applied)
-}
button : { label : String, onPress : Maybe msg } -> Button msg
button args =
    Button (defaultOptions args)


{-| Primary filled button — main call to action
-}
primary : { label : String, onPress : Maybe msg } -> Button msg
primary args =
    construct args Primary


{-| Secondary outlined button
-}
secondary : { label : String, onPress : Maybe msg } -> Button msg
secondary args =
    construct args Secondary


{-| Tertiary outlined button with dark border
-}
tertiary : { label : String, onPress : Maybe msg } -> Button msg
tertiary args =
    construct args Tertiary


{-| Danger filled button — destructive action
-}
danger : { label : String, onPress : Maybe msg } -> Button msg
danger args =
    construct args Danger


{-| Warning filled button — potentially risky action
-}
warning : { label : String, onPress : Maybe msg } -> Button msg
warning args =
    construct args Warning


{-| Plain button with no background or border (typically icon-only)
-}
plain : { label : String, onPress : Maybe msg } -> Button msg
plain args =
    construct args Plain


{-| Link-style button
-}
link : { label : String, onPress : Maybe msg } -> Button msg
link args =
    construct args Link


{-| Control button — used in toolbars and input groups
-}
control : { label : String, onPress : Maybe msg } -> Button msg
control args =
    construct args Control


{-| Set button size to small
-}
withSmallSize : Button msg -> Button msg
withSmallSize (Button opts) =
    Button { opts | size = Small }


{-| Set button size to large
-}
withLargeSize : Button msg -> Button msg
withLargeSize (Button opts) =
    Button { opts | size = Large }


{-| Set button size to default (removes any size modifier)
-}
withDefaultSize : Button msg -> Button msg
withDefaultSize (Button opts) =
    Button { opts | size = Default }


{-| Add an icon; defaults to left (start) position
-}
withIcon : Html msg -> Button msg -> Button msg
withIcon icon (Button opts) =
    Button { opts | icon = Just { element = icon, position = Left } }


{-| Position icon to the left (start) of the label
-}
withIconLeft : Button msg -> Button msg
withIconLeft (Button opts) =
    Button { opts | icon = opts.icon |> Maybe.map (\i -> { i | position = Left }) }


{-| Position icon to the right (end) of the label
-}
withIconRight : Button msg -> Button msg
withIconRight (Button opts) =
    Button { opts | icon = opts.icon |> Maybe.map (\i -> { i | position = Right }) }


{-| Mark the button as disabled. Adds `disabled`, `aria-disabled="true"`,
and `pf-m-disabled` and removes the click handler.
-}
withDisabled : Button msg -> Button msg
withDisabled (Button opts) =
    Button { opts | isDisabled = True, onPress = Nothing }


{-| Append extra HTML attributes to the rendered `<button>` element.

Use this escape hatch for things the builder does not expose directly —
`id`, `aria-*`, `data-*`, or one-off class overrides:

    Button.primary { label = "Save", onPress = Just Save }
        |> Button.withAttributes
            [ Attr.id "save-btn"
            , Attr.attribute "data-testid" "save-button"
            ]
        |> Button.toMarkup

Attributes are appended after the library-controlled ones, so they can
add but not accidentally replace structural PatternFly classes.

Calling `withAttributes` multiple times is additive — each call appends.

-}
withAttributes : List (Html.Attribute msg) -> Button msg -> Button msg
withAttributes attrs (Button opts) =
    Button { opts | extraAttrs = opts.extraAttrs ++ attrs }


-- Internal helpers


variantClass : Variant -> String
variantClass variant =
    case variant of
        Primary ->
            "pf-m-primary"

        Secondary ->
            "pf-m-secondary"

        Tertiary ->
            "pf-m-tertiary"

        Danger ->
            "pf-m-danger"

        Warning ->
            "pf-m-warning"

        Plain ->
            "pf-m-plain"

        Link ->
            "pf-m-link"

        Control ->
            "pf-m-control"


sizeClass : Size -> Maybe String
sizeClass size =
    case size of
        Small ->
            Just "pf-m-sm"

        Default ->
            Nothing

        Large ->
            Just "pf-m-lg"


buttonClasses : Options msg -> String
buttonClasses opts =
    [ Just "pf-v6-c-button"
    , Just (variantClass opts.variant)
    , sizeClass opts.size
    , if opts.isDisabled then Just "pf-m-disabled" else Nothing
    ]
        |> List.filterMap identity
        |> String.join " "


stateAttrs : Options msg -> List (Html.Attribute msg)
stateAttrs opts =
    if opts.isDisabled then
        [ Attr.disabled True
        , Attr.attribute "aria-disabled" "true"
        ]

    else
        case opts.onPress of
            Just msg ->
                [ Events.onClick msg ]

            Nothing ->
                []


iconSpan : Position -> Html msg -> Html msg
iconSpan position element =
    let
        posClass =
            case position of
                Left ->
                    "pf-m-start"

                Right ->
                    "pf-m-end"
    in
    Html.span
        [ Attr.class ("pf-v6-c-button__icon " ++ posClass) ]
        [ element ]


buttonChildren : Options msg -> List (Html msg)
buttonChildren opts =
    let
        labelNode =
            Html.text opts.label
    in
    case opts.icon of
        Nothing ->
            [ labelNode ]

        Just { element, position } ->
            case position of
                Left ->
                    [ iconSpan Left element, labelNode ]

                Right ->
                    [ labelNode, iconSpan Right element ]


{-| Render the Button as an `Html msg`
-}
toMarkup : Button msg -> Html msg
toMarkup (Button opts) =
    Html.button
        ([ Attr.class (buttonClasses opts)
         , Attr.type_ "button"
         ]
            ++ stateAttrs opts
            ++ opts.extraAttrs
        )
        (buttonChildren opts)
