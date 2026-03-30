module PF6.List exposing
    ( PFList, ListVariant
    , pFList
    , withPlain, withOrdered, withInlined, withIconed
    , withLarge
    , withAttributes
    , toMarkup
    )

{-| PF6 List component

Lists display items in a simple list format with optional styling.

See: <https://www.patternfly.org/components/list>


# Definition

@docs PFList, ListVariant


# Constructor

@docs pFList


# Variant modifiers

@docs withPlain, withOrdered, withInlined, withIconed


# Size modifiers

@docs withLarge


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque PFList type

Named PFList to avoid conflict with Elm's built-in List type.

-}
type PFList msg
    = PFList (Options msg)


{-| List display variant
-}
type ListVariant
    = Bulleted
    | Ordered
    | Plain
    | Inlined
    | Iconed


type alias Options msg =
    { items : List (Html msg)
    , variant : ListVariant
    , isLarge : Bool
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a List from item elements.

Each item is wrapped in an `<li>` automatically.

    pFList
        [ Html.text "First item"
        , Html.text "Second item"
        ]

-}
pFList : List (Html msg) -> PFList msg
pFList items =
    PFList
        { items = items
        , variant = Bulleted
        , isLarge = False
        , extraAttrs = []
        }


{-| Plain list — no bullets, no indentation
-}
withPlain : PFList msg -> PFList msg
withPlain (PFList opts) =
    PFList { opts | variant = Plain }


{-| Ordered (numbered) list
-}
withOrdered : PFList msg -> PFList msg
withOrdered (PFList opts) =
    PFList { opts | variant = Ordered }


{-| Inline list — items displayed in a row
-}
withInlined : PFList msg -> PFList msg
withInlined (PFList opts) =
    PFList { opts | variant = Inlined }


{-| Icon list — items expected to contain a leading icon
-}
withIconed : PFList msg -> PFList msg
withIconed (PFList opts) =
    PFList { opts | variant = Iconed }


{-| Large font size
-}
withLarge : PFList msg -> PFList msg
withLarge (PFList opts) =
    PFList { opts | isLarge = True }


{-| Append extra HTML attributes to the list root element.
-}
withAttributes : List (Html.Attribute msg) -> PFList msg -> PFList msg
withAttributes attrs (PFList opts) =
    PFList { opts | extraAttrs = opts.extraAttrs ++ attrs }


-- Internal helpers


listClasses : Options msg -> String
listClasses opts =
    [ Just "pf-v6-c-list"
    , case opts.variant of
        Bulleted ->
            Nothing

        Ordered ->
            Nothing

        Plain ->
            Just "pf-m-plain"

        Inlined ->
            Just "pf-m-inline"

        Iconed ->
            Just "pf-m-icon"
    , if opts.isLarge then Just "pf-m-lg" else Nothing
    ]
        |> List.filterMap identity
        |> String.join " "


wrapItem : Html msg -> Html msg
wrapItem content =
    Html.li [] [ content ]


{-| Render the PFList as an `Html msg`
-}
toMarkup : PFList msg -> Html msg
toMarkup (PFList opts) =
    let
        classes =
            listClasses opts

        liItems =
            List.map wrapItem opts.items
    in
    case opts.variant of
        Ordered ->
            Html.ol
                (Attr.class classes :: opts.extraAttrs)
                liItems

        _ ->
            Html.ul
                (Attr.class classes :: opts.extraAttrs)
                liItems
