module PF6.CodeBlock exposing
    ( CodeBlock
    , codeBlock
    , withActions
    , withAttributes
    , toMarkup
    )

{-| PF6 CodeBlock component

Code blocks display read-only code in a formatted, accessible container.

See: <https://www.patternfly.org/components/code-block>


# Definition

@docs CodeBlock


# Constructor

@docs codeBlock


# Modifiers

@docs withActions


# Escape hatch

@docs withAttributes


# Rendering

@docs toMarkup

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| Opaque CodeBlock type
-}
type CodeBlock msg
    = CodeBlock (Options msg)


type alias Options msg =
    { code : String
    , actions : Maybe (Html msg)
    , extraAttrs : List (Html.Attribute msg)
    }


{-| Construct a CodeBlock with code text
-}
codeBlock : String -> CodeBlock msg
codeBlock code =
    CodeBlock
        { code = code
        , actions = Nothing
        , extraAttrs = []
        }


{-| Add an actions area to the header (e.g. copy button)
-}
withActions : Html msg -> CodeBlock msg -> CodeBlock msg
withActions el (CodeBlock opts) =
    CodeBlock { opts | actions = Just el }


{-| Append extra HTML attributes to the root element.
-}
withAttributes : List (Html.Attribute msg) -> CodeBlock msg -> CodeBlock msg
withAttributes attrs (CodeBlock opts) =
    CodeBlock { opts | extraAttrs = opts.extraAttrs ++ attrs }


{-| Render the CodeBlock as an `Html msg`
-}
toMarkup : CodeBlock msg -> Html msg
toMarkup (CodeBlock opts) =
    let
        headerEl =
            case opts.actions of
                Nothing ->
                    Html.text ""

                Just actionsEl ->
                    Html.div
                        [ Attr.class "pf-v6-c-code-block__header" ]
                        [ Html.div
                            [ Attr.class "pf-v6-c-code-block__actions" ]
                            [ actionsEl ]
                        ]
    in
    Html.div
        (Attr.class "pf-v6-c-code-block" :: opts.extraAttrs)
        [ headerEl
        , Html.div
            [ Attr.class "pf-v6-c-code-block__content" ]
            [ Html.pre
                [ Attr.class "pf-v6-c-code-block__pre" ]
                [ Html.code
                    [ Attr.class "pf-v6-c-code-block__code" ]
                    [ Html.text opts.code ]
                ]
            ]
        ]
