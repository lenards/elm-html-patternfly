# elm-html-patternfly

An `elm/html` implementation of [PatternFly 6](https://www.patternfly.org) design system components for Elm 0.19.1.

Components use the builder pattern — construct, configure with `withX` modifiers, render with `toMarkup`.

```elm
PF6.Button.primary { label = "Save", onPress = Just Save }
    |> PF6.Button.withLargeSize
    |> PF6.Button.toMarkup
```


## ⚠️ You must include the PatternFly CSS

This library emits semantic HTML with PatternFly CSS class names. **Without the stylesheet, components will render as unstyled HTML.**

Add this to the `<head>` of your `index.html`:

```html
<link rel="stylesheet"
      href="https://unpkg.com/@patternfly/patternfly@6/patternfly.css">
```

Or install via npm:

```bash
npm install @patternfly/patternfly
```

Then import in your CSS entry point:

```css
@import "@patternfly/patternfly/patternfly.css";
```

Font Awesome 5 is bundled inside the PatternFly stylesheet — no separate icon setup needed.

If you cannot edit your HTML file, `PF6.Css.inlineImport` emits a `<style>@import ...` element you can place directly in your Elm `view`.


## Install

```bash
elm install lenards/elm-html-patternfly
```


## Usage

```elm
import PF6.Css
import PF6.Button
import PF6.Badge

view : Model -> Html Msg
view model =
    Html.div []
        [ PF6.Button.primary { label = "Submit", onPress = Just Submit }
            |> PF6.Button.toMarkup
        , PF6.Badge.unreadBadge 5
            |> PF6.Badge.toMarkup
        ]
```

For the escape hatch on any component — passing `id`, `aria-*`, `data-*`, or one-off class overrides:

```elm
PF6.Button.secondary { label = "Cancel", onPress = Just Cancel }
    |> PF6.Button.withAttributes [ Attr.id "cancel-btn" ]
    |> PF6.Button.toMarkup
```


## Components

See the [PatternFly component documentation](https://www.patternfly.org/components/all-components) for the full reference on each component's variants and behavior.
