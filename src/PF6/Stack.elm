module PF6.Stack exposing
    ( Stack, StackItem
    , stack
    , stackItem, withFill
    , withGutter
    , withAttributes
    , toMarkup
    )

{-| PF6 Stack layout

Arranges items vertically in a single column. One or more items can fill the remaining vertical space.

See: <https://www.patternfly.org/layouts/stack>


# Definition

@docs Stack, StackItem


# Constructor

@docs stack


# Item constructors

@docs stackItem, withFill


# Layout modifiers

@docs withGutter


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque Stack type
-}
type Stack msg
    = Stack (Options msg)


{-| An item in a Stack
-}
type StackItem msg
    = StackItem
        { child : Html msg
        , isFill : Bool
        }


type alias Options msg =
    { items : List (StackItem msg)
    , hasGutter : Bool
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a Stack with a list of StackItems
-}
stack : List (StackItem msg) -> Stack msg
stack items =
    Stack { items = items, hasGutter = False, extraAttrs = [] }


{-| Wrap content as a stack item
-}
stackItem : Html msg -> StackItem msg
stackItem child =
    StackItem { child = child, isFill = False }


{-| Make a stack item fill the remaining vertical space
-}
withFill : StackItem msg -> StackItem msg
withFill (StackItem i) =
    StackItem { i | isFill = True }


{-| Add gutters between stack items
-}
withGutter : Stack msg -> Stack msg
withGutter (Stack opts) =
    Stack { opts | hasGutter = True }


{-| Append extra HTML attributes to the root element.
-}
withAttributes : List (Html.Attribute msg) -> Stack msg -> Stack msg
withAttributes attrs (Stack opts) =
    Stack { opts | extraAttrs = opts.extraAttrs ++ attrs }


{-| Render the Stack as an `Html msg`
-}
toMarkup : Stack msg -> Html msg
toMarkup (Stack opts) =
    Html.div
        (Attr.class
            (if opts.hasGutter then
                "pf-v6-l-stack pf-m-gutter"

             else
                "pf-v6-l-stack"
            )
            :: opts.extraAttrs
        )
        (List.map
            (\(StackItem i) ->
                Html.div
                    [ Attr.class
                        (if i.isFill then
                            "pf-v6-l-stack__item pf-m-fill"

                         else
                            "pf-v6-l-stack__item"
                        )
                    ]
                    [ i.child ]
            )
            opts.items
        )
