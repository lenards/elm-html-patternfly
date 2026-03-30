module PF6.ActionList exposing
    ( ActionList
    , actionList
    , withItem, withGroup
    , withIcons
    , withAttributes
    , toMarkup
    )

{-| PF6 ActionList component

An action list is a group of actions, controls, or buttons with set spacing.

See: <https://www.patternfly.org/components/action-list>


# Definition

@docs ActionList


# Constructor

@docs actionList


# Item and group modifiers

@docs withItem, withGroup


# Variant modifiers

@docs withIcons


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque ActionList type
-}
type ActionList msg
    = ActionList (Options msg)


type Child msg
    = Item (Html msg)
    | Group (List (Html msg))


type alias Options msg =
    { children : List (Child msg)
    , isIcons : Bool
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct an empty ActionList
-}
actionList : ActionList msg
actionList =
    ActionList
        { children = []
        , isIcons = False
        , extraAttrs = []
        }


{-| Add an item to the action list
-}
withItem : Html msg -> ActionList msg -> ActionList msg
withItem el (ActionList opts) =
    ActionList { opts | children = opts.children ++ [ Item el ] }


{-| Add a group of items to the action list
-}
withGroup : List (Html msg) -> ActionList msg -> ActionList msg
withGroup items (ActionList opts) =
    ActionList { opts | children = opts.children ++ [ Group items ] }


{-| Use icon spacing (removes item padding for icon-only actions)
-}
withIcons : ActionList msg -> ActionList msg
withIcons (ActionList opts) =
    ActionList { opts | isIcons = True }


{-| Append extra HTML attributes to the root element.
-}
withAttributes : List (Html.Attribute msg) -> ActionList msg -> ActionList msg
withAttributes attrs (ActionList opts) =
    ActionList { opts | extraAttrs = opts.extraAttrs ++ attrs }


-- Internal helpers


renderChild : Child msg -> Html msg
renderChild child =
    case child of
        Item el ->
            Html.div
                [ Attr.class "pf-v6-c-action-list__item" ]
                [ el ]

        Group items ->
            Html.div
                [ Attr.class "pf-v6-c-action-list__group" ]
                (List.map
                    (\el -> Html.div [ Attr.class "pf-v6-c-action-list__item" ] [ el ])
                    items
                )


{-| Render the ActionList as an `Html msg`
-}
toMarkup : ActionList msg -> Html msg
toMarkup (ActionList opts) =
    Html.div
        (Attr.class
            (if opts.isIcons then
                "pf-v6-c-action-list pf-m-icons"

             else
                "pf-v6-c-action-list"
            )
            :: opts.extraAttrs
        )
        (List.map renderChild opts.children)
