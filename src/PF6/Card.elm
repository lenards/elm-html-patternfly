module PF6.Card exposing
    ( Card
    , card
    , withTitle, withFooter, withActions
    , withSelectable, withSelected, withFlat, withCompact, withRounded
    , withAttributes
    , toMarkup
    )

{-| PF6 Card component

A card is a square or rectangular container that can contain any kind of content.
Cards symbolize units of information, and each one acts as an entry point for users to access more details.

See: <https://www.patternfly.org/components/card>


# Definition

@docs Card


# Constructor

@docs card


# Header & Footer

@docs withTitle, withFooter, withActions


# Variant modifiers

@docs withSelectable, withSelected, withFlat, withCompact, withRounded


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque Card type
-}
type Card msg
    = Card (Options msg)


type alias Options msg =
    { title : Maybe String
    , body : List (Html msg)
    , footer : Maybe (Html msg)
    , actions : Maybe (Html msg)
    , isSelectable : Bool
    , isSelected : Bool
    , isFlat : Bool
    , isCompact : Bool
    , isRounded : Bool
    , extraAttrs : List (Html.Attribute msg)
    }


defaultOptions : List (Html msg) -> Options msg
defaultOptions body =
    { title = Nothing
    , body = body
    , footer = Nothing
    , actions = Nothing
    , isSelectable = False
    , isSelected = False
    , isFlat = False
    , isCompact = False
    , isRounded = False
    , extraAttrs = []
    }


{-| Construct a Card with body content
-}
card : List (Html msg) -> Card msg
card body =
    Card (defaultOptions body)


{-| Add a title to the card header
-}
withTitle : String -> Card msg -> Card msg
withTitle t (Card opts) =
    Card { opts | title = Just t }


{-| Add footer content
-}
withFooter : Html msg -> Card msg -> Card msg
withFooter el (Card opts) =
    Card { opts | footer = Just el }


{-| Add actions element to the card header (top-right)
-}
withActions : Html msg -> Card msg -> Card msg
withActions el (Card opts) =
    Card { opts | actions = Just el }


{-| Make the card selectable (adds cursor pointer)
-}
withSelectable : Card msg -> Card msg
withSelectable (Card opts) =
    Card { opts | isSelectable = True }


{-| Mark the card as currently selected
-}
withSelected : Card msg -> Card msg
withSelected (Card opts) =
    Card { opts | isSelected = True }


{-| Flat card — no box shadow
-}
withFlat : Card msg -> Card msg
withFlat (Card opts) =
    Card { opts | isFlat = True }


{-| Compact card — reduced padding
-}
withCompact : Card msg -> Card msg
withCompact (Card opts) =
    Card { opts | isCompact = True }


{-| Rounded card — larger border radius
-}
withRounded : Card msg -> Card msg
withRounded (Card opts) =
    Card { opts | isRounded = True }


{-| Append extra HTML attributes to the root card element.
-}
withAttributes : List (Html.Attribute msg) -> Card msg -> Card msg
withAttributes attrs (Card opts) =
    Card { opts | extraAttrs = opts.extraAttrs ++ attrs }


-- Internal helpers


cardClasses : Options msg -> String
cardClasses opts =
    [ Just "pf-v6-c-card"
    , if opts.isSelectable then Just "pf-m-selectable" else Nothing
    , if opts.isSelected then Just "pf-m-selected" else Nothing
    , if opts.isFlat then Just "pf-m-flat" else Nothing
    , if opts.isCompact then Just "pf-m-compact" else Nothing
    , if opts.isRounded then Just "pf-m-rounded" else Nothing
    ]
        |> List.filterMap identity
        |> String.join " "


headerMarkup : Options msg -> Html msg
headerMarkup opts =
    case ( opts.title, opts.actions ) of
        ( Nothing, Nothing ) ->
            Html.text ""

        _ ->
            Html.div
                [ Attr.class "pf-v6-c-card__header" ]
                [ Html.div
                    [ Attr.class "pf-v6-c-card__header-main" ]
                    [ case opts.title of
                        Nothing ->
                            Html.text ""

                        Just t ->
                            Html.div
                                [ Attr.class "pf-v6-c-card__title" ]
                                [ Html.h2
                                    [ Attr.class "pf-v6-c-card__title-text" ]
                                    [ Html.text t ]
                                ]
                    ]
                , case opts.actions of
                    Nothing ->
                        Html.text ""

                    Just el ->
                        Html.div
                            [ Attr.class "pf-v6-c-card__actions" ]
                            [ el ]
                ]


footerMarkup : Options msg -> Html msg
footerMarkup opts =
    case opts.footer of
        Nothing ->
            Html.text ""

        Just el ->
            Html.div
                [ Attr.class "pf-v6-c-card__footer" ]
                [ el ]


{-| Render the Card as an `Html msg`
-}
toMarkup : Card msg -> Html msg
toMarkup (Card opts) =
    Html.div
        (Attr.class (cardClasses opts) :: opts.extraAttrs)
        [ headerMarkup opts
        , Html.div
            [ Attr.class "pf-v6-c-card__body" ]
            opts.body
        , footerMarkup opts
        ]
