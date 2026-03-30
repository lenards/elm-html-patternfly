module PF6.ExpandableSection exposing
    ( ExpandableSection
    , expandableSection
    , withToggleText, withToggleTextCollapsed
    , withIndented
    , withAttributes
    , toMarkup
    )

{-| PF6 ExpandableSection component

Expandable sections toggle the visibility of content.

See: <https://www.patternfly.org/components/expandable-section>


# Definition

@docs ExpandableSection


# Constructor

@docs expandableSection


# Content modifiers

@docs withToggleText, withToggleTextCollapsed


# Display modifiers

@docs withIndented


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events


{-| Opaque ExpandableSection type
-}
type ExpandableSection msg
    = ExpandableSection (Options msg)


type alias Options msg =
    { body : Html msg
    , isExpanded : Bool
    , onToggle : Bool -> msg
    , toggleTextExpanded : String
    , toggleTextCollapsed : String
    , isIndented : Bool
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct an ExpandableSection

    expandableSection
        { body = Html.p [] [ Html.text "Hidden content" ]
        , isExpanded = model.sectionOpen
        , onToggle = SectionToggled
        }

-}
expandableSection :
    { body : Html msg
    , isExpanded : Bool
    , onToggle : Bool -> msg
    }
    -> ExpandableSection msg
expandableSection { body, isExpanded, onToggle } =
    ExpandableSection
        { body = body
        , isExpanded = isExpanded
        , onToggle = onToggle
        , toggleTextExpanded = "Show less"
        , toggleTextCollapsed = "Show more"
        , isIndented = False
        , extraAttrs = []
        }


{-| Set the toggle button text when the section is expanded
-}
withToggleText : String -> ExpandableSection msg -> ExpandableSection msg
withToggleText t (ExpandableSection opts) =
    ExpandableSection { opts | toggleTextExpanded = t }


{-| Set the toggle button text when the section is collapsed
-}
withToggleTextCollapsed : String -> ExpandableSection msg -> ExpandableSection msg
withToggleTextCollapsed t (ExpandableSection opts) =
    ExpandableSection { opts | toggleTextCollapsed = t }


{-| Indent the expandable content
-}
withIndented : ExpandableSection msg -> ExpandableSection msg
withIndented (ExpandableSection opts) =
    ExpandableSection { opts | isIndented = True }


{-| Append extra HTML attributes to the root element.
-}
withAttributes : List (Html.Attribute msg) -> ExpandableSection msg -> ExpandableSection msg
withAttributes attrs (ExpandableSection opts) =
    ExpandableSection { opts | extraAttrs = opts.extraAttrs ++ attrs }


{-| Render the ExpandableSection as an `Html msg`
-}
toMarkup : ExpandableSection msg -> Html msg
toMarkup (ExpandableSection opts) =
    let
        rootClass =
            [ Just "pf-v6-c-expandable-section"
            , if opts.isExpanded then
                Just "pf-m-expanded"

              else
                Nothing
            , if opts.isIndented then
                Just "pf-m-indented"

              else
                Nothing
            ]
                |> List.filterMap identity
                |> String.join " "

        toggleText =
            if opts.isExpanded then
                opts.toggleTextExpanded

            else
                opts.toggleTextCollapsed

        toggleEl =
            Html.button
                [ Attr.class "pf-v6-c-expandable-section__toggle"
                , Attr.type_ "button"
                , Attr.attribute "aria-expanded"
                    (if opts.isExpanded then
                        "true"

                     else
                        "false"
                    )
                , Events.onClick (opts.onToggle (not opts.isExpanded))
                ]
                [ Html.span
                    [ Attr.class "pf-v6-c-expandable-section__toggle-icon" ]
                    [ Html.i
                        [ Attr.class "fas fa-angle-right"
                        , Attr.attribute "aria-hidden" "true"
                        ]
                        []
                    ]
                , Html.span
                    [ Attr.class "pf-v6-c-expandable-section__toggle-text" ]
                    [ Html.text toggleText ]
                ]

        contentEl =
            Html.div
                [ Attr.class "pf-v6-c-expandable-section__content"
                , Attr.attribute "hidden"
                    (if opts.isExpanded then
                        "false"

                     else
                        "true"
                    )
                ]
                [ opts.body ]
    in
    Html.div
        (Attr.class rootClass :: opts.extraAttrs)
        [ toggleEl, contentEl ]
