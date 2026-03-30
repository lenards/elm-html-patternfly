module PF6.Badge exposing
    ( Badge, Status
    , badge, unreadBadge
    , withReadStatus, withUnreadStatus
    , withOverflowAt
    , isRead, isUnread
    , withAttributes
    , toMarkup
    )

{-| PF6 Badge component

Displays counts or status indicators as pill-shaped elements.

See: <https://www.patternfly.org/components/badge>


# Definition

@docs Badge, Status


# Constructor functions

@docs badge, unreadBadge


# Configuration functions

@docs withReadStatus, withUnreadStatus
@docs withOverflowAt


# Predicates

@docs isRead, isUnread


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque Badge type
-}
type Badge msg
    = Badge (Options msg)


{-| Read or Unread status
-}
type Status
    = Read
    | Unread


type alias Options msg =
    { value : Int
    , overflowAt : Int
    , status : Status
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a Badge with read status
-}
badge : Int -> Badge msg
badge value =
    Badge
        { value = value
        , overflowAt = 999
        , status = Read
        , extraAttrs = []
        }


{-| Construct a Badge with unread status
-}
unreadBadge : Int -> Badge msg
unreadBadge value =
    badge value |> withUnreadStatus


{-| Set status to Unread (blue background)
-}
withUnreadStatus : Badge msg -> Badge msg
withUnreadStatus (Badge opts) =
    Badge { opts | status = Unread }


{-| Set status to Read (gray background)
-}
withReadStatus : Badge msg -> Badge msg
withReadStatus (Badge opts) =
    Badge { opts | status = Read }


{-| Set the overflow threshold. Counts above this display as "N+"
-}
withOverflowAt : Int -> Badge msg -> Badge msg
withOverflowAt n (Badge opts) =
    Badge { opts | overflowAt = n }


{-| True if badge status is Read
-}
isRead : Badge msg -> Bool
isRead (Badge opts) =
    opts.status == Read


{-| True if badge status is Unread
-}
isUnread : Badge msg -> Bool
isUnread (Badge opts) =
    opts.status == Unread


{-| Append extra HTML attributes to the badge `<span>`.
-}
withAttributes : List (Html.Attribute msg) -> Badge msg -> Badge msg
withAttributes attrs (Badge opts) =
    Badge { opts | extraAttrs = opts.extraAttrs ++ attrs }


-- Internal helpers


badgeClasses : Options msg -> String
badgeClasses opts =
    [ Just "pf-v6-c-badge"
    , case opts.status of
        Read ->
            Just "pf-m-read"

        Unread ->
            Just "pf-m-unread"
    ]
        |> List.filterMap identity
        |> String.join " "


displayValue : Options msg -> String
displayValue opts =
    if opts.value > opts.overflowAt then
        String.fromInt opts.overflowAt ++ "+"

    else
        String.fromInt opts.value


{-| Render the Badge as an `Html msg`
-}
toMarkup : Badge msg -> Html msg
toMarkup (Badge opts) =
    Html.span
        (Attr.class (badgeClasses opts) :: opts.extraAttrs)
        [ Html.text (displayValue opts) ]
