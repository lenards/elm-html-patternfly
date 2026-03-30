module PF6.Breadcrumb exposing
    ( Breadcrumb, BreadcrumbItem
    , breadcrumb
    , item, currentItem, dropdownItem
    , withAttributes
    , toMarkup
    )

{-| PF6 Breadcrumb component

Breadcrumbs provide users with a sense of location within a hierarchy.

See: <https://www.patternfly.org/components/breadcrumb>


# Definition

@docs Breadcrumb, BreadcrumbItem


# Constructor

@docs breadcrumb


# Item constructors

@docs item, currentItem, dropdownItem


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque Breadcrumb type
-}
type Breadcrumb msg
    = Breadcrumb (Options msg)


{-| A single breadcrumb item
-}
type BreadcrumbItem msg
    = LinkItem { label : String, href : String }
    | CurrentItem String
    | DropdownItem { label : String, items : List { label : String, onClick : msg } }


type alias Options msg =
    { items : List (BreadcrumbItem msg)
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a Breadcrumb from a list of items
-}
breadcrumb : List (BreadcrumbItem msg) -> Breadcrumb msg
breadcrumb items =
    Breadcrumb { items = items, extraAttrs = [] }


{-| A linked breadcrumb item
-}
item : { label : String, href : String } -> BreadcrumbItem msg
item config =
    LinkItem config


{-| The current page breadcrumb (not linked, aria-current="page")
-}
currentItem : String -> BreadcrumbItem msg
currentItem label =
    CurrentItem label


{-| A dropdown breadcrumb item
-}
dropdownItem : { label : String, items : List { label : String, onClick : msg } } -> BreadcrumbItem msg
dropdownItem config =
    DropdownItem config


{-| Append extra HTML attributes to the `<nav>` element.
-}
withAttributes : List (Html.Attribute msg) -> Breadcrumb msg -> Breadcrumb msg
withAttributes attrs (Breadcrumb opts) =
    Breadcrumb { opts | extraAttrs = opts.extraAttrs ++ attrs }


-- Internal helpers


renderItemContent : BreadcrumbItem msg -> Html msg
renderItemContent bcItem =
    case bcItem of
        LinkItem { label, href } ->
            Html.a
                [ Attr.class "pf-v6-c-breadcrumb__link"
                , Attr.href href
                ]
                [ Html.text label ]

        CurrentItem label ->
            Html.span
                [ Attr.class "pf-v6-c-breadcrumb__link pf-m-current"
                , Attr.attribute "aria-current" "page"
                ]
                [ Html.text label ]

        DropdownItem { label } ->
            Html.span
                [ Attr.class "pf-v6-c-breadcrumb__link" ]
                [ Html.text label
                , Html.span
                    [ Attr.class "pf-v6-c-breadcrumb__dropdown-arrow"
                    , Attr.attribute "aria-hidden" "true"
                    ]
                    [ Html.text " ▾" ]
                ]


renderItem : Bool -> BreadcrumbItem msg -> Html msg
renderItem isLast bcItem =
    let
        divider =
            if isLast then
                Html.text ""

            else
                Html.span
                    [ Attr.class "pf-v6-c-breadcrumb__item-divider"
                    , Attr.attribute "aria-hidden" "true"
                    ]
                    [ Html.text "/" ]
    in
    Html.li
        [ Attr.class "pf-v6-c-breadcrumb__item" ]
        [ renderItemContent bcItem
        , divider
        ]


{-| Render the Breadcrumb as an `Html msg`
-}
toMarkup : Breadcrumb msg -> Html msg
toMarkup (Breadcrumb opts) =
    let
        total =
            List.length opts.items

        itemsHtml =
            List.indexedMap
                (\i bcItem -> renderItem (i == total - 1) bcItem)
                opts.items
    in
    Html.nav
        (Attr.attribute "aria-label" "breadcrumb" :: opts.extraAttrs)
        [ Html.ol
            [ Attr.class "pf-v6-c-breadcrumb__list" ]
            itemsHtml
        ]
