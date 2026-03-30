module PF6.Navigation exposing
    ( Navigation
    , NavItem
    , navigation
    , navLink
    , navLinkMsg
    , navGroup
    , navExpandable
    , withCurrentLink
    , withAriaLabel
    , withHorizontal
    , withAttributes
    , toMarkup
    )

{-| PF6 Navigation component

Navigation provides a sidebar or horizontal menu for moving between pages.

See: <https://www.patternfly.org/components/navigation>


# Definition

@docs Navigation, NavItem


# Constructor

@docs navigation


# Nav item constructors

@docs navLink, navLinkMsg, navGroup, navExpandable


# Nav item modifiers

@docs withCurrentLink


# Navigation modifiers

@docs withAriaLabel, withHorizontal


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events


{-| Opaque Navigation type
-}
type Navigation msg
    = Navigation (Options msg)


{-| Opaque NavItem type — construct with navLink, navLinkMsg, navGroup, or navExpandable
-}
type NavItem msg
    = NavItem (NavItemConfig msg)


type NavItemConfig msg
    = LinkItem (LinkOptions msg)
    | GroupItem (GroupOptions msg)
    | ExpandableItem (ExpandableOptions msg)


type alias LinkOptions msg =
    { label : String
    , href : String
    , onClick : Maybe msg
    , isCurrent : Bool
    }


type alias GroupOptions msg =
    { title : String
    , items : List (NavItem msg)
    }


type alias ExpandableOptions msg =
    { title : String
    , isExpanded : Bool
    , onToggle : msg
    , items : List (NavItem msg)
    , isCurrent : Bool
    }


type alias Options msg =
    { items : List (NavItem msg)
    , ariaLabel : String
    , isHorizontal : Bool
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a Navigation with a list of nav items
-}
navigation : List (NavItem msg) -> Navigation msg
navigation items =
    Navigation
        { items = items
        , ariaLabel = "Global"
        , isHorizontal = False
        , extraAttrs = []
        }


{-| Create a nav link with an href

Use this for multi-page apps where navigation changes the URL.

-}
navLink : String -> String -> NavItem msg
navLink label href =
    NavItem
        (LinkItem
            { label = label
            , href = href
            , onClick = Nothing
            , isCurrent = False
            }
        )


{-| Create a nav link that sends a message when clicked

Use this for single-page apps where navigation is handled via Elm messages.

-}
navLinkMsg : String -> msg -> NavItem msg
navLinkMsg label msg =
    NavItem
        (LinkItem
            { label = label
            , href = "#"
            , onClick = Just msg
            , isCurrent = False
            }
        )


{-| Mark a nav link as the currently active item
-}
withCurrentLink : NavItem msg -> NavItem msg
withCurrentLink (NavItem config) =
    case config of
        LinkItem opts ->
            NavItem (LinkItem { opts | isCurrent = True })

        ExpandableItem opts ->
            NavItem (ExpandableItem { opts | isCurrent = True })

        GroupItem _ ->
            NavItem config


{-| Create a static nav group with a section heading

Groups collect related links under a heading but have no toggle.

-}
navGroup : String -> List (NavItem msg) -> NavItem msg
navGroup title items =
    NavItem (GroupItem { title = title, items = items })


{-| Create an expandable nav section

    navExpandable
        { title = "Settings"
        , isExpanded = model.settingsOpen
        , onToggle = ToggleSettings
        }
        [ navLinkMsg "General" GoGeneral
        , navLinkMsg "Account" GoAccount
        ]

-}
navExpandable : { title : String, isExpanded : Bool, onToggle : msg } -> List (NavItem msg) -> NavItem msg
navExpandable { title, isExpanded, onToggle } items =
    NavItem
        (ExpandableItem
            { title = title
            , isExpanded = isExpanded
            , onToggle = onToggle
            , items = items
            , isCurrent = False
            }
        )


{-| Set the aria-label on the nav element (default: "Global")
-}
withAriaLabel : String -> Navigation msg -> Navigation msg
withAriaLabel label (Navigation opts) =
    Navigation { opts | ariaLabel = label }


{-| Render the navigation horizontally (for top nav bars)
-}
withHorizontal : Navigation msg -> Navigation msg
withHorizontal (Navigation opts) =
    Navigation { opts | isHorizontal = True }


{-| Append extra HTML attributes to the root element.
-}
withAttributes : List (Html.Attribute msg) -> Navigation msg -> Navigation msg
withAttributes attrs (Navigation opts) =
    Navigation { opts | extraAttrs = opts.extraAttrs ++ attrs }


-- Rendering helpers


renderItem : NavItem msg -> Html msg
renderItem (NavItem config) =
    case config of
        LinkItem opts ->
            let
                linkClass =
                    if opts.isCurrent then
                        "pf-v6-c-nav__link pf-m-current"

                    else
                        "pf-v6-c-nav__link"
            in
            Html.li
                [ Attr.class "pf-v6-c-nav__item" ]
                [ Html.a
                    ([ Attr.class linkClass
                     , Attr.href opts.href
                     ]
                        ++ (case opts.onClick of
                                Nothing ->
                                    []

                                Just msg ->
                                    [ Events.onClick msg ]
                           )
                    )
                    [ Html.text opts.label ]
                ]

        GroupItem opts ->
            Html.li
                [ Attr.class "pf-v6-c-nav__item" ]
                [ Html.section
                    [ Attr.class "pf-v6-c-nav__section" ]
                    [ Html.h2
                        [ Attr.class "pf-v6-c-nav__section-title" ]
                        [ Html.text opts.title ]
                    , Html.ul
                        [ Attr.class "pf-v6-c-nav__list" ]
                        (List.map renderItem opts.items)
                    ]
                ]

        ExpandableItem opts ->
            let
                itemClass =
                    "pf-v6-c-nav__item pf-m-expandable"
                        ++ (if opts.isExpanded then " pf-m-expanded" else "")
                        ++ (if opts.isCurrent then " pf-m-current" else "")
            in
            Html.li
                [ Attr.class itemClass ]
                [ Html.button
                    [ Attr.class "pf-v6-c-nav__link"
                    , Attr.type_ "button"
                    , Attr.attribute "aria-expanded"
                        (if opts.isExpanded then
                            "true"

                         else
                            "false"
                        )
                    , Events.onClick opts.onToggle
                    ]
                    [ Html.text opts.title
                    , Html.span
                        [ Attr.class "pf-v6-c-nav__toggle" ]
                        [ Html.span
                            [ Attr.class "pf-v6-c-nav__toggle-icon" ]
                            [ Html.i
                                [ Attr.class "fas fa-angle-right"
                                , Attr.attribute "aria-hidden" "true"
                                ]
                                []
                            ]
                        ]
                    ]
                , Html.section
                    [ Attr.class "pf-v6-c-nav__subnav"
                    , Attr.attribute "aria-hidden"
                        (if opts.isExpanded then
                            "false"

                         else
                            "true"
                        )
                    ]
                    [ Html.ul
                        [ Attr.class "pf-v6-c-nav__list" ]
                        (List.map renderItem opts.items)
                    ]
                ]


{-| Render the Navigation as an `Html msg`
-}
toMarkup : Navigation msg -> Html msg
toMarkup (Navigation opts) =
    let
        navClass =
            if opts.isHorizontal then
                "pf-v6-c-nav pf-m-horizontal"

            else
                "pf-v6-c-nav"
    in
    Html.nav
        ([ Attr.class navClass
         , Attr.attribute "aria-label" opts.ariaLabel
         ]
            ++ opts.extraAttrs
        )
        [ Html.ul
            [ Attr.class "pf-v6-c-nav__list" ]
            (List.map renderItem opts.items)
        ]
