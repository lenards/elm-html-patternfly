module PF6.Split exposing
    ( Split, SplitItem
    , split
    , splitItem, withFill
    , withWrap, withGutter
    , withAttributes
    , toMarkup
    )

{-| PF6 Split layout

Distributes items horizontally. One item can fill the remaining space.

See: <https://www.patternfly.org/layouts/split>


# Definition

@docs Split, SplitItem


# Constructor

@docs split


# Item constructors

@docs splitItem, withFill


# Layout modifiers

@docs withWrap, withGutter


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque Split type
-}
type Split msg
    = Split (Options msg)


{-| An item in a Split
-}
type SplitItem msg
    = SplitItem
        { child : Html msg
        , isFill : Bool
        }


type alias Options msg =
    { items : List (SplitItem msg)
    , hasGutter : Bool
    , wraps : Bool
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a Split with a list of SplitItems
-}
split : List (SplitItem msg) -> Split msg
split items =
    Split { items = items, hasGutter = False, wraps = False, extraAttrs = [] }


{-| Wrap content as a split item
-}
splitItem : Html msg -> SplitItem msg
splitItem child =
    SplitItem { child = child, isFill = False }


{-| Make a split item fill the remaining horizontal space
-}
withFill : SplitItem msg -> SplitItem msg
withFill (SplitItem i) =
    SplitItem { i | isFill = True }


{-| Allow items to wrap when they overflow
-}
withWrap : Split msg -> Split msg
withWrap (Split opts) =
    Split { opts | wraps = True }


{-| Add gutters between split items
-}
withGutter : Split msg -> Split msg
withGutter (Split opts) =
    Split { opts | hasGutter = True }


{-| Append extra HTML attributes to the root element.
-}
withAttributes : List (Html.Attribute msg) -> Split msg -> Split msg
withAttributes attrs (Split opts) =
    Split { opts | extraAttrs = opts.extraAttrs ++ attrs }


{-| Render the Split as an `Html msg`
-}
toMarkup : Split msg -> Html msg
toMarkup (Split opts) =
    let
        rootClass =
            [ Just "pf-v6-l-split"
            , if opts.wraps then Just "pf-m-wrap" else Nothing
            , if opts.hasGutter then Just "pf-m-gutter" else Nothing
            ]
                |> List.filterMap identity
                |> String.join " "
    in
    Html.div
        (Attr.class rootClass :: opts.extraAttrs)
        (List.map
            (\(SplitItem i) ->
                Html.div
                    [ Attr.class
                        (if i.isFill then
                            "pf-v6-l-split__item pf-m-fill"

                         else
                            "pf-v6-l-split__item"
                        )
                    ]
                    [ i.child ]
            )
            opts.items
        )
