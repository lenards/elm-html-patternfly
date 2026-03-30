module PF6.Css exposing
    ( cdnUrl
    , stylesheet
    , inlineImport
    )

{-| Helpers for including the PatternFly 6 CSS.

PatternFly components require the PatternFly stylesheet to render correctly.
Font Awesome 5 is bundled inside it — no separate icon font setup needed.

There are three ways to load it:


## Option 1 — Static HTML file (recommended)

Add this to the `<head>` of your `index.html` before your Elm script tag:

```html
<link rel="stylesheet" href="https://unpkg.com/@patternfly/patternfly@6/patternfly.css">
```

This is the most reliable approach and works with any Elm program type.


## Option 2 — npm / bundler

```bash
npm install @patternfly/patternfly
```

Then import in your CSS entry point:

```css
@import "@patternfly/patternfly/patternfly.css";
```


## Option 3 — From Elm (escape hatch)

If you cannot edit the HTML file, use `inlineImport` inside your Elm `view`:

    view model =
        Html.div []
            [ PF6.Css.inlineImport
            , -- your content
            ]

Or use `stylesheet` if you are rendering into a shadow DOM or custom element
context where a `<link>` inside the component root is appropriate.


# Values

@docs cdnUrl, stylesheet, inlineImport

-}

import Html exposing (Html)
import Html.Attributes as Attr


{-| The CDN URL for the PatternFly 6 CSS bundle (via unpkg).

Useful if you need the URL as a string — e.g. to build your own `<link>` tag
or pass it to a port.

-}
cdnUrl : String
cdnUrl =
    "https://unpkg.com/@patternfly/patternfly@6/patternfly.css"


{-| A `<link rel="stylesheet">` element for the PatternFly 6 CDN bundle.

Best used in a static `index.html` `<head>`. If you need to emit it from
inside an Elm `view`, prefer `inlineImport` instead, which uses a `<style>`
element and is valid anywhere in the DOM.

-}
stylesheet : Html msg
stylesheet =
    Html.node "link"
        [ Attr.rel "stylesheet"
        , Attr.href cdnUrl
        ]
        []


{-| A `<style>` element that `@import`s the PatternFly 6 CDN bundle.

Unlike a `<link>` element, a `<style>` is valid anywhere in the HTML body,
making this safe to emit directly from an Elm `view` function when you
cannot edit the surrounding HTML file.

    view model =
        Html.div []
            [ PF6.Css.inlineImport
            , PF6.Button.primary { label = "Go", onPress = Just Go }
                |> PF6.Button.toMarkup
            ]

Note: `@import` inside `<style>` is resolved by the browser before other
styles, so load order behaves the same as a `<link>` in `<head>`.

-}
inlineImport : Html msg
inlineImport =
    Html.node "style"
        []
        [ Html.text ("@import url(\"" ++ cdnUrl ++ "\");") ]
