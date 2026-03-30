module PF6.Icon exposing
    ( Icon, Size, Status
    , icon
    , withSmallSize, withMediumSize, withLargeSize, withXLargeSize
    , withDefaultStatus, withSuccessStatus, withDangerStatus, withWarningStatus, withInfoStatus
    , withAriaLabel, withAriaHidden
    , withAttributes
    , toMarkup
    )

{-| PF6 Icon component

A wrapper for inline SVG or HTML icons with semantic color status support.

See: <https://www.patternfly.org/components/icon>


# Definition

@docs Icon, Size, Status


# Constructor

@docs icon


# Size modifiers

@docs withSmallSize, withMediumSize, withLargeSize, withXLargeSize


# Status modifiers

@docs withDefaultStatus, withSuccessStatus, withDangerStatus, withWarningStatus, withInfoStatus


# Accessibility modifiers

@docs withAriaLabel, withAriaHidden


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque Icon type
-}
type Icon msg
    = Icon (Options msg)


{-| Icon display size
-}
type Size
    = Small
    | Medium
    | Large
    | XLarge


{-| Icon semantic status (controls color via PF CSS)
-}
type Status
    = Default
    | Success
    | Danger
    | Warning
    | Info


type alias Options msg =
    { content : Html msg
    , size : Size
    , status : Status
    , ariaLabel : Maybe String
    , ariaHidden : Bool
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct an Icon wrapping any Html element (SVG, `<i>`, text, etc.)
-}
icon : Html msg -> Icon msg
icon content =
    Icon
        { content = content
        , size = Medium
        , status = Default
        , ariaLabel = Nothing
        , ariaHidden = False
        , extraAttrs = []
        }


{-| Small icon
-}
withSmallSize : Icon msg -> Icon msg
withSmallSize (Icon opts) =
    Icon { opts | size = Small }


{-| Medium icon (default)
-}
withMediumSize : Icon msg -> Icon msg
withMediumSize (Icon opts) =
    Icon { opts | size = Medium }


{-| Large icon
-}
withLargeSize : Icon msg -> Icon msg
withLargeSize (Icon opts) =
    Icon { opts | size = Large }


{-| XLarge icon
-}
withXLargeSize : Icon msg -> Icon msg
withXLargeSize (Icon opts) =
    Icon { opts | size = XLarge }


{-| Default color (inherits text color)
-}
withDefaultStatus : Icon msg -> Icon msg
withDefaultStatus (Icon opts) =
    Icon { opts | status = Default }


{-| Success (green) color
-}
withSuccessStatus : Icon msg -> Icon msg
withSuccessStatus (Icon opts) =
    Icon { opts | status = Success }


{-| Danger (red) color
-}
withDangerStatus : Icon msg -> Icon msg
withDangerStatus (Icon opts) =
    Icon { opts | status = Danger }


{-| Warning (yellow) color
-}
withWarningStatus : Icon msg -> Icon msg
withWarningStatus (Icon opts) =
    Icon { opts | status = Warning }


{-| Info (blue/purple) color
-}
withInfoStatus : Icon msg -> Icon msg
withInfoStatus (Icon opts) =
    Icon { opts | status = Info }


{-| Set an accessible label (adds `aria-label`, removes `aria-hidden`)
-}
withAriaLabel : String -> Icon msg -> Icon msg
withAriaLabel label (Icon opts) =
    Icon { opts | ariaLabel = Just label, ariaHidden = False }


{-| Hide from screen readers (`aria-hidden="true"`)
-}
withAriaHidden : Icon msg -> Icon msg
withAriaHidden (Icon opts) =
    Icon { opts | ariaHidden = True }


{-| Append extra HTML attributes to the icon wrapper span.
-}
withAttributes : List (Html.Attribute msg) -> Icon msg -> Icon msg
withAttributes attrs (Icon opts) =
    Icon { opts | extraAttrs = opts.extraAttrs ++ attrs }


-- Internal helpers


sizeClass : Size -> String
sizeClass size =
    case size of
        Small ->
            "pf-m-sm"

        Medium ->
            "pf-m-md"

        Large ->
            "pf-m-lg"

        XLarge ->
            "pf-m-xl"


statusClass : Status -> Maybe String
statusClass status =
    case status of
        Default ->
            Nothing

        Success ->
            Just "pf-m-success"

        Danger ->
            Just "pf-m-danger"

        Warning ->
            Just "pf-m-warning"

        Info ->
            Just "pf-m-info"


iconClasses : Options msg -> String
iconClasses opts =
    [ Just "pf-v6-c-icon"
    , Just (sizeClass opts.size)
    , statusClass opts.status
    ]
        |> List.filterMap identity
        |> String.join " "


accessibilityAttrs : Options msg -> List (Html.Attribute msg)
accessibilityAttrs opts =
    case ( opts.ariaLabel, opts.ariaHidden ) of
        ( Just label, _ ) ->
            [ Attr.attribute "aria-label" label ]

        ( Nothing, True ) ->
            [ Attr.attribute "aria-hidden" "true" ]

        ( Nothing, False ) ->
            []


{-| Render the Icon as an `Html msg`
-}
toMarkup : Icon msg -> Html msg
toMarkup (Icon opts) =
    Html.span
        ([ Attr.class (iconClasses opts) ]
            ++ accessibilityAttrs opts
            ++ opts.extraAttrs
        )
        [ Html.span
            [ Attr.class "pf-v6-c-icon__content" ]
            [ opts.content ]
        ]
