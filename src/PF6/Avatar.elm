module PF6.Avatar exposing
    ( Avatar, Size
    , avatar
    , withSmallSize, withMediumSize, withLargeSize, withXLargeSize
    , withBorder
    , withAttributes
    , toMarkup
    )

{-| PF6 Avatar component

Displays a user's profile image with optional border, in various sizes.

See: <https://www.patternfly.org/components/avatar>


# Definition

@docs Avatar, Size


# Constructor

@docs avatar


# Size modifiers

@docs withSmallSize, withMediumSize, withLargeSize, withXLargeSize


# Style modifiers

@docs withBorder


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque Avatar type
-}
type Avatar msg
    = Avatar (Options msg)


{-| Avatar size variants
-}
type Size
    = Small
    | Medium
    | Large
    | XLarge


type alias Options msg =
    { src : String
    , alt : String
    , size : Size
    , hasBorder : Bool
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct an Avatar from an image src and alt text
-}
avatar : { src : String, alt : String } -> Avatar msg
avatar { src, alt } =
    Avatar
        { src = src
        , alt = alt
        , size = Medium
        , hasBorder = False
        , extraAttrs = []
        }


{-| Small avatar — 24px
-}
withSmallSize : Avatar msg -> Avatar msg
withSmallSize (Avatar opts) =
    Avatar { opts | size = Small }


{-| Medium avatar — 36px (default)
-}
withMediumSize : Avatar msg -> Avatar msg
withMediumSize (Avatar opts) =
    Avatar { opts | size = Medium }


{-| Large avatar — 72px
-}
withLargeSize : Avatar msg -> Avatar msg
withLargeSize (Avatar opts) =
    Avatar { opts | size = Large }


{-| XLarge avatar — 128px
-}
withXLargeSize : Avatar msg -> Avatar msg
withXLargeSize (Avatar opts) =
    Avatar { opts | size = XLarge }


{-| Add a border around the avatar
-}
withBorder : Avatar msg -> Avatar msg
withBorder (Avatar opts) =
    Avatar { opts | hasBorder = True }


{-| Append extra HTML attributes to the `<img>` element.
-}
withAttributes : List (Html.Attribute msg) -> Avatar msg -> Avatar msg
withAttributes attrs (Avatar opts) =
    Avatar { opts | extraAttrs = opts.extraAttrs ++ attrs }


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


avatarClasses : Options msg -> String
avatarClasses opts =
    [ Just "pf-v6-c-avatar"
    , Just (sizeClass opts.size)
    , if opts.hasBorder then Just "pf-m-border" else Nothing
    ]
        |> List.filterMap identity
        |> String.join " "


{-| Render the Avatar as an `Html msg`
-}
toMarkup : Avatar msg -> Html msg
toMarkup (Avatar opts) =
    Html.img
        ([ Attr.class (avatarClasses opts)
         , Attr.src opts.src
         , Attr.alt opts.alt
         ]
            ++ opts.extraAttrs
        )
        []
