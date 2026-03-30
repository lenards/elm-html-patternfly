module PF6.DescriptionList exposing
    ( DescriptionList, DescriptionGroup
    , descriptionList
    , group
    , withHorizontal, withCompact, withAutoColumnSize, withColumnCount
    , withAttributes
    , toMarkup
    )

{-| PF6 DescriptionList component

Description lists display term/value pairs, useful for metadata or detail panels.

See: <https://www.patternfly.org/components/description-list>


# Definition

@docs DescriptionList, DescriptionGroup


# Constructor

@docs descriptionList


# Group constructor

@docs group


# Layout modifiers

@docs withHorizontal, withCompact, withAutoColumnSize, withColumnCount


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque DescriptionList type
-}
type DescriptionList msg
    = DescriptionList (Options msg)


{-| A term/value group
-}
type DescriptionGroup msg
    = DescriptionGroup
        { term : String
        , description : Html msg
        }


type alias Options msg =
    { groups : List (DescriptionGroup msg)
    , isHorizontal : Bool
    , isCompact : Bool
    , columnCount : Maybe Int
    , isAutoColumnSize : Bool
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a DescriptionList with groups
-}
descriptionList : List (DescriptionGroup msg) -> DescriptionList msg
descriptionList groups =
    DescriptionList
        { groups = groups
        , isHorizontal = False
        , isCompact = False
        , columnCount = Nothing
        , isAutoColumnSize = False
        , extraAttrs = []
        }


{-| Construct a term/value group

    group "Name" (Html.text "John Doe")

-}
group : String -> Html msg -> DescriptionGroup msg
group term description =
    DescriptionGroup { term = term, description = description }


{-| Horizontal layout — term and description side by side
-}
withHorizontal : DescriptionList msg -> DescriptionList msg
withHorizontal (DescriptionList opts) =
    DescriptionList { opts | isHorizontal = True }


{-| Compact layout with reduced spacing
-}
withCompact : DescriptionList msg -> DescriptionList msg
withCompact (DescriptionList opts) =
    DescriptionList { opts | isCompact = True }


{-| Auto-fit column layout (responsive)
-}
withAutoColumnSize : DescriptionList msg -> DescriptionList msg
withAutoColumnSize (DescriptionList opts) =
    DescriptionList { opts | isAutoColumnSize = True }


{-| Fixed column count layout (1–3)
-}
withColumnCount : Int -> DescriptionList msg -> DescriptionList msg
withColumnCount n (DescriptionList opts) =
    DescriptionList { opts | columnCount = Just (clamp 1 3 n) }


{-| Append extra HTML attributes to the root element.
-}
withAttributes : List (Html.Attribute msg) -> DescriptionList msg -> DescriptionList msg
withAttributes attrs (DescriptionList opts) =
    DescriptionList { opts | extraAttrs = opts.extraAttrs ++ attrs }


-- Internal helpers


listClasses : Options msg -> String
listClasses opts =
    [ Just "pf-v6-c-description-list"
    , if opts.isHorizontal then
        Just "pf-m-horizontal"

      else
        Nothing
    , if opts.isCompact then
        Just "pf-m-compact"

      else
        Nothing
    , if opts.isAutoColumnSize then
        Just "pf-m-auto-fit"

      else
        Nothing
    , Maybe.map (\n -> "pf-m-" ++ String.fromInt n ++ "-col") opts.columnCount
    ]
        |> List.filterMap identity
        |> String.join " "


renderGroup : DescriptionGroup msg -> Html msg
renderGroup (DescriptionGroup g) =
    Html.div
        [ Attr.class "pf-v6-c-description-list__group" ]
        [ Html.dt
            [ Attr.class "pf-v6-c-description-list__term" ]
            [ Html.span
                [ Attr.class "pf-v6-c-description-list__text" ]
                [ Html.text g.term ]
            ]
        , Html.dd
            [ Attr.class "pf-v6-c-description-list__description" ]
            [ Html.div
                [ Attr.class "pf-v6-c-description-list__text" ]
                [ g.description ]
            ]
        ]


{-| Render the DescriptionList as an `Html msg`
-}
toMarkup : DescriptionList msg -> Html msg
toMarkup (DescriptionList opts) =
    Html.dl
        (Attr.class (listClasses opts) :: opts.extraAttrs)
        (List.map renderGroup opts.groups)
