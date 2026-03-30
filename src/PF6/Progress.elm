module PF6.Progress exposing
    ( Progress, Size, Status
    , progress
    , withTitle, withHelperText
    , withSmallSize, withLargeSize
    , withSuccess, withDanger, withWarning, withInfo
    , withOutside, withInside, withNone
    , withAttributes
    , toMarkup
    )

{-| PF6 Progress component

Progress bars communicate the status of an ongoing process.

See: <https://www.patternfly.org/components/progress>


# Definition

@docs Progress, Size, Status


# Constructor

@docs progress


# Label modifiers

@docs withTitle, withHelperText


# Size modifiers

@docs withSmallSize, withLargeSize


# Status modifiers

@docs withSuccess, withDanger, withWarning, withInfo


# Measure label position

@docs withOutside, withInside, withNone


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque Progress type
-}
type Progress msg
    = Progress (Options msg)


{-| Progress bar size
-}
type Size
    = Default
    | Small
    | Large


{-| Progress bar status
-}
type Status
    = None
    | Success
    | Danger
    | Warning
    | Info


{-| Measure label position
-}
type MeasureLocation
    = Outside
    | Inside
    | NoMeasure


type alias Options msg =
    { id : String
    , value : Int
    , title : String
    , helperText : Maybe String
    , size : Size
    , status : Status
    , measureLocation : MeasureLocation
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a Progress bar

    progress
        { id = "my-progress"
        , value = 33
        }

The value should be an integer between 0 and 100.

-}
progress : { id : String, value : Int } -> Progress msg
progress { id, value } =
    Progress
        { id = id
        , value = clamp 0 100 value
        , title = ""
        , helperText = Nothing
        , size = Default
        , status = None
        , measureLocation = Outside
        , extraAttrs = []
        }


{-| Set the progress title/description
-}
withTitle : String -> Progress msg -> Progress msg
withTitle t (Progress opts) =
    Progress { opts | title = t }


{-| Set helper text below the progress bar
-}
withHelperText : String -> Progress msg -> Progress msg
withHelperText t (Progress opts) =
    Progress { opts | helperText = Just t }


{-| Small progress bar
-}
withSmallSize : Progress msg -> Progress msg
withSmallSize (Progress opts) =
    Progress { opts | size = Small }


{-| Large progress bar
-}
withLargeSize : Progress msg -> Progress msg
withLargeSize (Progress opts) =
    Progress { opts | size = Large }


{-| Success status (green)
-}
withSuccess : Progress msg -> Progress msg
withSuccess (Progress opts) =
    Progress { opts | status = Success }


{-| Danger status (red)
-}
withDanger : Progress msg -> Progress msg
withDanger (Progress opts) =
    Progress { opts | status = Danger }


{-| Warning status (yellow)
-}
withWarning : Progress msg -> Progress msg
withWarning (Progress opts) =
    Progress { opts | status = Warning }


{-| Info status (blue)
-}
withInfo : Progress msg -> Progress msg
withInfo (Progress opts) =
    Progress { opts | status = Info }


{-| Show the measure label outside the bar (default)
-}
withOutside : Progress msg -> Progress msg
withOutside (Progress opts) =
    Progress { opts | measureLocation = Outside }


{-| Show the measure label inside the bar
-}
withInside : Progress msg -> Progress msg
withInside (Progress opts) =
    Progress { opts | measureLocation = Inside }


{-| Hide the measure label
-}
withNone : Progress msg -> Progress msg
withNone (Progress opts) =
    Progress { opts | measureLocation = NoMeasure }


{-| Append extra HTML attributes to the root progress element.
-}
withAttributes : List (Html.Attribute msg) -> Progress msg -> Progress msg
withAttributes attrs (Progress opts) =
    Progress { opts | extraAttrs = opts.extraAttrs ++ attrs }


-- Internal helpers


sizeClass : Size -> Maybe String
sizeClass size =
    case size of
        Default ->
            Nothing

        Small ->
            Just "pf-m-sm"

        Large ->
            Just "pf-m-lg"


statusClass : Status -> Maybe String
statusClass status =
    case status of
        None ->
            Nothing

        Success ->
            Just "pf-m-success"

        Danger ->
            Just "pf-m-danger"

        Warning ->
            Just "pf-m-warning"

        Info ->
            Just "pf-m-info"


progressClasses : Options msg -> String
progressClasses opts =
    [ Just "pf-v6-c-progress"
    , sizeClass opts.size
    , statusClass opts.status
    , if opts.measureLocation == Outside then
        Just "pf-m-outside"

      else
        Nothing
    , if opts.measureLocation == Inside then
        Just "pf-m-inside"

      else
        Nothing
    ]
        |> List.filterMap identity
        |> String.join " "


measureText : Int -> String
measureText value =
    String.fromInt value ++ "%"


{-| Render the Progress as an `Html msg`
-}
toMarkup : Progress msg -> Html msg
toMarkup (Progress opts) =
    let
        descriptionId =
            opts.id ++ "-description"

        measureEl =
            Html.span
                [ Attr.class "pf-v6-c-progress__measure" ]
                [ Html.text (measureText opts.value) ]

        descriptionEl =
            if String.isEmpty opts.title then
                Html.text ""

            else
                Html.div
                    [ Attr.class "pf-v6-c-progress__description"
                    , Attr.id descriptionId
                    , Attr.attribute "aria-hidden" "true"
                    ]
                    [ Html.text opts.title ]

        statusEl =
            Html.div
                [ Attr.class "pf-v6-c-progress__status"
                , Attr.attribute "aria-hidden" "true"
                ]
                [ measureEl ]

        indicatorChildren =
            if opts.measureLocation == Inside then
                [ Html.span
                    [ Attr.class "pf-v6-c-progress__measure" ]
                    [ Html.text (measureText opts.value) ]
                ]

            else
                []

        barEl =
            Html.div
                [ Attr.class "pf-v6-c-progress__bar"
                , Attr.attribute "role" "progressbar"
                , Attr.attribute "aria-valuemin" "0"
                , Attr.attribute "aria-valuenow" (String.fromInt opts.value)
                , Attr.attribute "aria-valuemax" "100"
                , Attr.attribute "aria-labelledby"
                    (if String.isEmpty opts.title then
                        opts.id

                     else
                        descriptionId
                    )
                ]
                [ Html.div
                    [ Attr.class "pf-v6-c-progress__indicator"
                    , Attr.style "width" (measureText opts.value)
                    ]
                    indicatorChildren
                ]

        helperEl =
            case opts.helperText of
                Nothing ->
                    Html.text ""

                Just t ->
                    Html.div
                        [ Attr.class "pf-v6-c-helper-text" ]
                        [ Html.div
                            [ Attr.class "pf-v6-c-helper-text__item" ]
                            [ Html.span
                                [ Attr.class "pf-v6-c-helper-text__item-text" ]
                                [ Html.text t ]
                            ]
                        ]

        children =
            case opts.measureLocation of
                Inside ->
                    [ descriptionEl, barEl, helperEl ]

                _ ->
                    [ descriptionEl, statusEl, barEl, helperEl ]
    in
    Html.div
        (Attr.class (progressClasses opts) :: Attr.id opts.id :: opts.extraAttrs)
        children
