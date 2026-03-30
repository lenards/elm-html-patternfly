module PF6.EmptyState exposing
    ( EmptyState
    , emptyState
    , withIcon, withTitleH1, withTitleH2, withBody, withPrimaryAction, withSecondaryActions
    , withSmallSize, withLargeSize, withFullHeight
    , withAttributes
    , toMarkup
    )

{-| PF6 EmptyState component

Empty states are used when there is no data to display in a component or page.

See: <https://www.patternfly.org/components/empty-state>


# Definition

@docs EmptyState


# Constructor

@docs emptyState


# Content modifiers

@docs withIcon, withTitleH1, withTitleH2, withBody, withPrimaryAction, withSecondaryActions


# Size modifiers

@docs withSmallSize, withLargeSize, withFullHeight


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque EmptyState type
-}
type EmptyState msg
    = EmptyState (Options msg)


type Size
    = Small
    | Default
    | Large


type alias Options msg =
    { icon : Maybe (Html msg)
    , title : Maybe String
    , titleLevel : Int
    , body : Maybe String
    , primaryAction : Maybe (Html msg)
    , secondaryActions : List (Html msg)
    , size : Size
    , isFullHeight : Bool
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct an EmptyState
-}
emptyState : EmptyState msg
emptyState =
    EmptyState
        { icon = Nothing
        , title = Nothing
        , titleLevel = 2
        , body = Nothing
        , primaryAction = Nothing
        , secondaryActions = []
        , size = Default
        , isFullHeight = False
        , extraAttrs = []
        }


{-| Add an icon element
-}
withIcon : Html msg -> EmptyState msg -> EmptyState msg
withIcon el (EmptyState opts) =
    EmptyState { opts | icon = Just el }


{-| Set the title at H1 level
-}
withTitleH1 : String -> EmptyState msg -> EmptyState msg
withTitleH1 t (EmptyState opts) =
    EmptyState { opts | title = Just t, titleLevel = 1 }


{-| Set the title at H2 level (default)
-}
withTitleH2 : String -> EmptyState msg -> EmptyState msg
withTitleH2 t (EmptyState opts) =
    EmptyState { opts | title = Just t, titleLevel = 2 }


{-| Set body description text
-}
withBody : String -> EmptyState msg -> EmptyState msg
withBody b (EmptyState opts) =
    EmptyState { opts | body = Just b }


{-| Set the primary action element (usually a Button)
-}
withPrimaryAction : Html msg -> EmptyState msg -> EmptyState msg
withPrimaryAction el (EmptyState opts) =
    EmptyState { opts | primaryAction = Just el }


{-| Set secondary action elements
-}
withSecondaryActions : List (Html msg) -> EmptyState msg -> EmptyState msg
withSecondaryActions els (EmptyState opts) =
    EmptyState { opts | secondaryActions = els }


{-| Small variant
-}
withSmallSize : EmptyState msg -> EmptyState msg
withSmallSize (EmptyState opts) =
    EmptyState { opts | size = Small }


{-| Large variant
-}
withLargeSize : EmptyState msg -> EmptyState msg
withLargeSize (EmptyState opts) =
    EmptyState { opts | size = Large }


{-| Fill the full height of the container
-}
withFullHeight : EmptyState msg -> EmptyState msg
withFullHeight (EmptyState opts) =
    EmptyState { opts | isFullHeight = True }


{-| Append extra HTML attributes to the root empty state element.
-}
withAttributes : List (Html.Attribute msg) -> EmptyState msg -> EmptyState msg
withAttributes attrs (EmptyState opts) =
    EmptyState { opts | extraAttrs = opts.extraAttrs ++ attrs }


-- Internal helpers


emptyStateClasses : Options msg -> String
emptyStateClasses opts =
    [ Just "pf-v6-c-empty-state"
    , case opts.size of
        Small ->
            Just "pf-m-sm"

        Default ->
            Nothing

        Large ->
            Just "pf-m-lg"
    , if opts.isFullHeight then Just "pf-m-full-height" else Nothing
    ]
        |> List.filterMap identity
        |> String.join " "


titleNode : Int -> String -> Html msg
titleNode level text =
    let
        attrs =
            [ Attr.class "pf-v6-c-empty-state__title-text" ]
    in
    case level of
        1 ->
            Html.h1 attrs [ Html.text text ]

        _ ->
            Html.h2 attrs [ Html.text text ]


{-| Render the EmptyState as an `Html msg`
-}
toMarkup : EmptyState msg -> Html msg
toMarkup (EmptyState opts) =
    let
        iconEl =
            case opts.icon of
                Nothing ->
                    Html.text ""

                Just el ->
                    Html.div
                        [ Attr.class "pf-v6-c-empty-state__icon" ]
                        [ el ]

        titleEl =
            case opts.title of
                Nothing ->
                    Html.text ""

                Just t ->
                    Html.div
                        [ Attr.class "pf-v6-c-empty-state__title" ]
                        [ titleNode opts.titleLevel t ]

        bodyEl =
            case opts.body of
                Nothing ->
                    Html.text ""

                Just b ->
                    Html.div
                        [ Attr.class "pf-v6-c-empty-state__body" ]
                        [ Html.text b ]

        primaryEl =
            case opts.primaryAction of
                Nothing ->
                    Html.text ""

                Just el ->
                    Html.div
                        [ Attr.class "pf-v6-c-empty-state__primary" ]
                        [ el ]

        secondaryEl =
            if List.isEmpty opts.secondaryActions then
                Html.text ""

            else
                Html.div
                    [ Attr.class "pf-v6-c-empty-state__secondary" ]
                    opts.secondaryActions
    in
    Html.div
        (Attr.class (emptyStateClasses opts) :: opts.extraAttrs)
        [ Html.div
            [ Attr.class "pf-v6-c-empty-state__content" ]
            [ iconEl
            , titleEl
            , bodyEl
            , primaryEl
            , secondaryEl
            ]
        ]
