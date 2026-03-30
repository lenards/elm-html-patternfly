module PF6.Page exposing
    ( Page
    , page
    , pageSection
    , withMasthead
    , withSidebar
    , withSidebarExpanded
    , withMainId
    , withAttributes
    , toMarkup
    )

{-| PF6 Page component

Page provides the outer chrome for a full-page application layout, composing
a Masthead, Sidebar, and main content area.

See: <https://www.patternfly.org/components/page>


# Definition

@docs Page


# Constructor

@docs page


# Helpers

@docs pageSection


# Content modifiers

@docs withMasthead, withSidebar, withSidebarExpanded, withMainId


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque Page type
-}
type Page msg
    = Page (Options msg)


type alias Options msg =
    { masthead : Maybe (Html msg)
    , sidebar : Maybe (Html msg)
    , isSidebarExpanded : Bool
    , mainId : String
    , mainContent : List (Html msg)
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a Page with main content

Each element in the list is rendered directly inside the `<main>` element.
Wrap content in `pageSection` to get the standard inset and padding.

-}
page : List (Html msg) -> Page msg
page content =
    Page
        { masthead = Nothing
        , sidebar = Nothing
        , isSidebarExpanded = True
        , mainId = "main-content"
        , mainContent = content
        , extraAttrs = []
        }


{-| Wrap content in a `pf-v6-c-page__main-section`

Use this to get the standard page section padding:

    page
        [ pageSection
            (Title.title "Dashboard" |> Title.withH1 |> Title.toMarkup)
        ]

-}
pageSection : Html msg -> Html msg
pageSection content =
    Html.section
        [ Attr.class "pf-v6-c-page__main-section" ]
        [ content ]


{-| Set the masthead (rendered as the page header)

Pass the output of `Masthead.toMarkup` here.

-}
withMasthead : Html msg -> Page msg -> Page msg
withMasthead content (Page opts) =
    Page { opts | masthead = Just content }


{-| Set the sidebar content (typically a Navigation)

Pass the output of `Navigation.toMarkup` here.

-}
withSidebar : Html msg -> Page msg -> Page msg
withSidebar content (Page opts) =
    Page { opts | sidebar = Just content }


{-| Control whether the sidebar is expanded (default: True)

Set to False to collapse the sidebar, e.g. on mobile or when the toggle is clicked.

-}
withSidebarExpanded : Bool -> Page msg -> Page msg
withSidebarExpanded expanded (Page opts) =
    Page { opts | isSidebarExpanded = expanded }


{-| Set the `id` on the `<main>` element (default: "main-content")

Used as the target for skip-to-content links.

-}
withMainId : String -> Page msg -> Page msg
withMainId id (Page opts) =
    Page { opts | mainId = id }


{-| Append extra HTML attributes to the root element.
-}
withAttributes : List (Html.Attribute msg) -> Page msg -> Page msg
withAttributes attrs (Page opts) =
    Page { opts | extraAttrs = opts.extraAttrs ++ attrs }


{-| Render the Page as an `Html msg`
-}
toMarkup : Page msg -> Html msg
toMarkup (Page opts) =
    let
        mastheadEl =
            case opts.masthead of
                Nothing ->
                    Html.text ""

                Just content ->
                    content

        sidebarEl =
            case opts.sidebar of
                Nothing ->
                    Html.text ""

                Just content ->
                    let
                        sidebarClass =
                            if opts.isSidebarExpanded then
                                "pf-v6-c-page__sidebar pf-m-expanded"

                            else
                                "pf-v6-c-page__sidebar"
                    in
                    Html.div
                        [ Attr.class sidebarClass ]
                        [ Html.div
                            [ Attr.class "pf-v6-c-page__sidebar-body" ]
                            [ content ]
                        ]
    in
    Html.div
        (Attr.class "pf-v6-c-page" :: opts.extraAttrs)
        [ mastheadEl
        , sidebarEl
        , Html.main_
            [ Attr.class "pf-v6-c-page__main"
            , Attr.id opts.mainId
            , Attr.tabindex -1
            ]
            opts.mainContent
        ]
