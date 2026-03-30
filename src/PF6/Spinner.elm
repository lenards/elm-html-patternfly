module PF6.Spinner exposing
    ( Spinner, Size
    , spinner
    , withSmallSize, withMediumSize, withLargeSize, withXLargeSize
    , withAriaLabel
    , withAttributes
    , toMarkup
    )

{-| PF6 Spinner component

Spinners are used to indicate that content is loading.

See: <https://www.patternfly.org/components/spinner>


# Definition

@docs Spinner, Size


# Constructor

@docs spinner


# Size modifiers

@docs withSmallSize, withMediumSize, withLargeSize, withXLargeSize


# Accessibility

@docs withAriaLabel


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque Spinner type
-}
type Spinner msg
    = Spinner (Options msg)


{-| Spinner size
-}
type Size
    = Small
    | Medium
    | Large
    | XLarge


type alias Options msg =
    { size : Size
    , ariaLabel : String
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a default medium spinner
-}
spinner : Spinner msg
spinner =
    Spinner
        { size = Medium
        , ariaLabel = "Loading..."
        , extraAttrs = []
        }


{-| Small spinner
-}
withSmallSize : Spinner msg -> Spinner msg
withSmallSize (Spinner opts) =
    Spinner { opts | size = Small }


{-| Medium spinner (default)
-}
withMediumSize : Spinner msg -> Spinner msg
withMediumSize (Spinner opts) =
    Spinner { opts | size = Medium }


{-| Large spinner
-}
withLargeSize : Spinner msg -> Spinner msg
withLargeSize (Spinner opts) =
    Spinner { opts | size = Large }


{-| XLarge spinner
-}
withXLargeSize : Spinner msg -> Spinner msg
withXLargeSize (Spinner opts) =
    Spinner { opts | size = XLarge }


{-| Set the accessible label for screen readers
-}
withAriaLabel : String -> Spinner msg -> Spinner msg
withAriaLabel lbl (Spinner opts) =
    Spinner { opts | ariaLabel = lbl }


{-| Append extra HTML attributes to the spinner element.
-}
withAttributes : List (Html.Attribute msg) -> Spinner msg -> Spinner msg
withAttributes attrs (Spinner opts) =
    Spinner { opts | extraAttrs = opts.extraAttrs ++ attrs }


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


spinnerClasses : Options msg -> String
spinnerClasses opts =
    "pf-v6-c-spinner " ++ sizeClass opts.size


{-| Render the Spinner as an `Html msg`

The spinner uses an SVG circle with PatternFly's CSS-driven stroke animation.
No inline styles or JavaScript needed. The PF stylesheet must be loaded.

-}
toMarkup : Spinner msg -> Html msg
toMarkup (Spinner opts) =
    Html.node "svg"
        ([ Attr.class (spinnerClasses opts)
         , Attr.attribute "role" "progressbar"
         , Attr.attribute "viewBox" "0 0 100 100"
         , Attr.attribute "aria-label" opts.ariaLabel
         ]
            ++ opts.extraAttrs
        )
        [ Html.node "circle"
            [ Attr.class "pf-v6-c-spinner__path"
            , Attr.attribute "cx" "50"
            , Attr.attribute "cy" "50"
            , Attr.attribute "r" "45"
            , Attr.attribute "fill" "none"
            ]
            []
        ]
