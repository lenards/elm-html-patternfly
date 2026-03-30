module PF6.Divider exposing
    ( Divider, Orientation
    , divider
    , withHorizontal, withVertical
    , withInset, withInsetMd, withInsetLg, withInsetXl
    , withAttributes
    , toMarkup
    )

{-| PF6 Divider component

A horizontal or vertical rule to visually separate content.

See: <https://www.patternfly.org/components/divider>


# Definition

@docs Divider, Orientation


# Constructor

@docs divider


# Orientation

@docs withHorizontal, withVertical


# Inset modifiers

@docs withInset, withInsetMd, withInsetLg, withInsetXl


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque Divider type
-}
type Divider msg
    = Divider (Options msg)


{-| Divider orientation
-}
type Orientation
    = Horizontal
    | Vertical


type Inset
    = None
    | Sm
    | Md
    | Lg
    | Xl


type alias Options msg =
    { orientation : Orientation
    , inset : Inset
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a horizontal divider
-}
divider : Divider msg
divider =
    Divider
        { orientation = Horizontal
        , inset = None
        , extraAttrs = []
        }


{-| Horizontal divider (default)
-}
withHorizontal : Divider msg -> Divider msg
withHorizontal (Divider opts) =
    Divider { opts | orientation = Horizontal }


{-| Vertical divider
-}
withVertical : Divider msg -> Divider msg
withVertical (Divider opts) =
    Divider { opts | orientation = Vertical }


{-| Small inset (8px each side)
-}
withInset : Divider msg -> Divider msg
withInset (Divider opts) =
    Divider { opts | inset = Sm }


{-| Medium inset (16px each side)
-}
withInsetMd : Divider msg -> Divider msg
withInsetMd (Divider opts) =
    Divider { opts | inset = Md }


{-| Large inset (24px each side)
-}
withInsetLg : Divider msg -> Divider msg
withInsetLg (Divider opts) =
    Divider { opts | inset = Lg }


{-| XL inset (32px each side)
-}
withInsetXl : Divider msg -> Divider msg
withInsetXl (Divider opts) =
    Divider { opts | inset = Xl }


{-| Append extra HTML attributes to the divider element.
-}
withAttributes : List (Html.Attribute msg) -> Divider msg -> Divider msg
withAttributes attrs (Divider opts) =
    Divider { opts | extraAttrs = opts.extraAttrs ++ attrs }


-- Internal helpers


insetClass : Inset -> Maybe String
insetClass inset =
    case inset of
        None ->
            Nothing

        Sm ->
            Just "pf-m-inset-sm"

        Md ->
            Just "pf-m-inset-md"

        Lg ->
            Just "pf-m-inset-lg"

        Xl ->
            Just "pf-m-inset-xl"


dividerClasses : Options msg -> String
dividerClasses opts =
    [ Just "pf-v6-c-divider"
    , if opts.orientation == Vertical then Just "pf-m-vertical" else Nothing
    , insetClass opts.inset
    ]
        |> List.filterMap identity
        |> String.join " "


{-| Render the Divider as an `Html msg`

Horizontal dividers render as `<hr>`. Vertical dividers render as a `<div>`
with `role="separator"`.

-}
toMarkup : Divider msg -> Html msg
toMarkup (Divider opts) =
    case opts.orientation of
        Horizontal ->
            Html.hr
                (Attr.class (dividerClasses opts) :: opts.extraAttrs)
                []

        Vertical ->
            Html.div
                ([ Attr.class (dividerClasses opts)
                 , Attr.attribute "role" "separator"
                 ]
                    ++ opts.extraAttrs
                )
                []
