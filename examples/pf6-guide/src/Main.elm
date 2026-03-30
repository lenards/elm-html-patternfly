module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, h2, p, span, text)
import Html.Attributes as Attr
import PF6.Alert as Alert
import PF6.Avatar as Avatar
import PF6.Badge as Badge
import PF6.Banner as Banner
import PF6.Breadcrumb as Breadcrumb
import PF6.Button as Button
import PF6.Card as Card
import PF6.Checkbox as Checkbox
import PF6.Divider as Divider
import PF6.EmptyState as EmptyState
import PF6.HelperText as HelperText
import PF6.Hint as Hint
import PF6.Icon as Icon
import PF6.Label as Label
import PF6.ActionList as ActionList
import PF6.Bullseye as Bullseye
import PF6.CodeBlock as CodeBlock
import PF6.DescriptionList as DescriptionList
import PF6.ExpandableSection as ExpandableSection
import PF6.Flex as Flex
import PF6.Gallery as Gallery
import PF6.Grid as Grid
import PF6.Level as Level
import PF6.List as PFList
import PF6.NumberInput as NumberInput
import PF6.Progress as Progress
import PF6.Radio as Radio
import PF6.SearchInput as SearchInput
import PF6.Select as Select
import PF6.Spinner as Spinner
import PF6.Split as Split
import PF6.Stack as Stack
import PF6.Switch as Switch
import PF6.TextArea as TextArea
import PF6.TextInput as TextInput
import PF6.Title as Title
import PF6.Masthead as Masthead
import PF6.Navigation as Navigation
import PF6.Page as Page


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { alertVisible : Bool
    , checkboxChecked : Bool
    , checkbox2Checked : Bool
    , labelVisible : Bool
    , badgeCount : Int
    , selectedRadio : String
    , expandableSectionOpen : Bool
    , textInputValue : String
    , textAreaValue : String
    , selectValue : String
    , switchChecked : Bool
    , searchValue : String
    , numberValue : Int
    , sidebarExpanded : Bool
    , navExpandedSection : Bool
    }


init : Model
init =
    { alertVisible = True
    , checkboxChecked = False
    , checkbox2Checked = True
    , labelVisible = True
    , badgeCount = 7
    , selectedRadio = "option-1"
    , expandableSectionOpen = False
    , textInputValue = ""
    , textAreaValue = ""
    , selectValue = ""
    , switchChecked = False
    , searchValue = ""
    , numberValue = 1
    , sidebarExpanded = True
    , navExpandedSection = False
    }



-- UPDATE


type Msg
    = DismissAlert
    | ToggleCheckbox Bool
    | ToggleCheckbox2 Bool
    | DismissLabel
    | IncrementBadge
    | DecrementBadge
    | SelectRadio String
    | ToggleExpandableSection Bool
    | TextInputChanged String
    | TextAreaChanged String
    | SelectChanged String
    | SwitchToggled Bool
    | SearchChanged String
    | NumberChanged Int
    | ToggleSidebar
    | ToggleNavSection


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DismissAlert ->
            ( { model | alertVisible = False }, Cmd.none )

        ToggleCheckbox val ->
            ( { model | checkboxChecked = val }, Cmd.none )

        ToggleCheckbox2 val ->
            ( { model | checkbox2Checked = val }, Cmd.none )

        DismissLabel ->
            ( { model | labelVisible = False }, Cmd.none )

        IncrementBadge ->
            ( { model | badgeCount = model.badgeCount + 1 }, Cmd.none )

        DecrementBadge ->
            ( { model | badgeCount = max 0 (model.badgeCount - 1) }, Cmd.none )

        SelectRadio val ->
            ( { model | selectedRadio = val }, Cmd.none )

        ToggleExpandableSection val ->
            ( { model | expandableSectionOpen = val }, Cmd.none )

        TextInputChanged val ->
            ( { model | textInputValue = val }, Cmd.none )

        TextAreaChanged val ->
            ( { model | textAreaValue = val }, Cmd.none )

        SelectChanged val ->
            ( { model | selectValue = val }, Cmd.none )

        SwitchToggled val ->
            ( { model | switchChecked = val }, Cmd.none )

        SearchChanged val ->
            ( { model | searchValue = val }, Cmd.none )

        NumberChanged val ->
            ( { model | numberValue = val }, Cmd.none )

        ToggleSidebar ->
            ( { model | sidebarExpanded = not model.sidebarExpanded }, Cmd.none )

        ToggleNavSection ->
            ( { model | navExpandedSection = not model.navExpandedSection }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ pageHeader
        , div
            [ Attr.style "padding" "2rem"
            , Attr.style "max-width" "960px"
            , Attr.style "margin" "0 auto"
            ]
                [ h1
                    [ Attr.style "font-size" "2rem"
                    , Attr.style "font-weight" "700"
                    , Attr.style "margin-bottom" "0.5rem"
                    ]
                    [ text "elm-html-patternfly" ]
                , p
                    [ Attr.style "color" "var(--pf-t--global--text--color--subtle)"
                    , Attr.style "margin-bottom" "2rem"
                    ]
                    [ text "PatternFly 6 components built with elm/html. 40 components — Batch 1, 2, 3 & 4." ]
                , viewActionList
                , viewAlert model
                , viewAvatar
                , viewBadge model
                , viewBanner
                , viewBreadcrumb
                , viewBullseye
                , viewButton
                , viewCard
                , viewCheckbox model
                , viewCodeBlock
                , viewDescriptionList
                , viewDivider
                , viewEmptyState
                , viewExpandableSection model
                , viewFlex
                , viewGallery
                , viewGrid
                , viewHelperText
                , viewHint
                , viewIcon
                , viewLabel model
                , viewLevel
                , viewList
                , viewMasthead model
                , viewNavigation model
                , viewNumberInput model
                , viewPage model
                , viewProgress
                , viewRadio model
                , viewSearchInput model
                , viewSelect model
                , viewSpinner
                , viewSplit
                , viewStack
                , viewSwitch model
                , viewTextArea model
                , viewTextInput model
                , viewTitle
                ]
        ]


pageHeader : Html Msg
pageHeader =
    div
        [ Attr.class "pf-v6-c-masthead"
        , Attr.style "background-color" "var(--pf-t--global--background--color--primary--default)"
        ]
        [ div [ Attr.class "pf-v6-c-masthead__main" ]
            [ span
                [ Attr.style "color" "var(--pf-t--global--text--color--on-dark--regular, #fff)"
                , Attr.style "font-weight" "700"
                , Attr.style "font-size" "1.125rem"
                , Attr.style "padding" "0.75rem 1rem"
                , Attr.style "display" "block"
                ]
                [ text "PF6 Component Guide" ]
            ]
        ]



-- Section layout helpers


section : String -> String -> List (Html Msg) -> Html Msg
section title description content =
    div [ Attr.class "guide-section" ]
        [ h2 [ Attr.class "guide-section__title" ] [ text title ]
        , p [ Attr.class "guide-section__description" ] [ text description ]
        , div [] content
        ]


row : List (Html Msg) -> Html Msg
row children =
    div [ Attr.class "guide-row" ] children


rowVertical : List (Html Msg) -> Html Msg
rowVertical children =
    div [ Attr.class "guide-row guide-row--vertical" ] children


label : String -> Html Msg
label t =
    p [ Attr.class "guide-label" ] [ text t ]



-- ALERT


viewAlert : Model -> Html Msg
viewAlert model =
    section "Alert"
        "Alerts communicate status and provide brief, contextual information to users."
        [ label "Variants"
        , rowVertical
            [ Alert.alert "Default alert — general information."
                |> Alert.toMarkup
            , Alert.alert "Success! Your changes have been saved."
                |> Alert.withSuccess
                |> Alert.withTitle "Success"
                |> Alert.toMarkup
            , Alert.alert "Something went wrong. Please try again."
                |> Alert.withDanger
                |> Alert.withTitle "Danger"
                |> Alert.toMarkup
            , Alert.alert "Review these settings before continuing."
                |> Alert.withWarning
                |> Alert.withTitle "Warning"
                |> Alert.toMarkup
            , Alert.alert "This is an informational message."
                |> Alert.withInfo
                |> Alert.withTitle "Info"
                |> Alert.toMarkup
            ]
        , label "Inline"
        , rowVertical
            [ Alert.alert "This alert is inline — no box shadow."
                |> Alert.withSuccess
                |> Alert.withTitle "Inline success"
                |> Alert.withInline
                |> Alert.toMarkup
            ]
        , label "With close button"
        , rowVertical
            [ if model.alertVisible then
                Alert.alert "Click × to dismiss this alert."
                    |> Alert.withInfo
                    |> Alert.withTitle "Dismissible"
                    |> Alert.withCloseMsg DismissAlert
                    |> Alert.toMarkup

              else
                div []
                    [ text "Alert dismissed. "
                    , Button.link { label = "Show again", onPress = Just DismissAlert }
                        |> Button.toMarkup
                    ]
            ]
        ]



-- AVATAR


-- PF6 avatar placeholder — a simple gray person silhouette as a data URI
avatarSrc : String
avatarSrc =
    "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 36 36'%3E%3Crect width='36' height='36' fill='%23d2d2d2'/%3E%3Ccircle cx='18' cy='13' r='7' fill='%23a8a8a8'/%3E%3Cpath d='M2 34 Q2 22 18 22 Q34 22 34 34 Z' fill='%23a8a8a8'/%3E%3C/svg%3E"


viewAvatar : Html Msg
viewAvatar =
    section "Avatar"
        "Displays a user's profile image. Uses a placeholder when no image is available."
        [ label "Sizes"
        , row
            [ Avatar.avatar { src = avatarSrc, alt = "Small avatar" }
                |> Avatar.withSmallSize
                |> Avatar.toMarkup
            , Avatar.avatar { src = avatarSrc, alt = "Medium avatar" }
                |> Avatar.toMarkup
            , Avatar.avatar { src = avatarSrc, alt = "Large avatar" }
                |> Avatar.withLargeSize
                |> Avatar.toMarkup
            , Avatar.avatar { src = avatarSrc, alt = "XLarge avatar" }
                |> Avatar.withXLargeSize
                |> Avatar.toMarkup
            ]
        , label "With border"
        , row
            [ Avatar.avatar { src = avatarSrc, alt = "Avatar with border" }
                |> Avatar.withBorder
                |> Avatar.toMarkup
            , Avatar.avatar { src = avatarSrc, alt = "Large with border" }
                |> Avatar.withLargeSize
                |> Avatar.withBorder
                |> Avatar.toMarkup
            ]
        ]



-- BADGE


viewBadge : Model -> Html Msg
viewBadge model =
    section "Badge"
        "Badges display counts or status indicators as pill-shaped elements."
        [ label "Read vs Unread"
        , row
            [ Badge.badge 7 |> Badge.toMarkup
            , Badge.unreadBadge 7 |> Badge.toMarkup
            ]
        , label "Overflow"
        , row
            [ Badge.badge 1500 |> Badge.toMarkup
            , Badge.unreadBadge 1500 |> Badge.toMarkup
            , Badge.badge 1500 |> Badge.withOverflowAt 999 |> Badge.toMarkup
            ]
        , label "Interactive (click buttons)"
        , row
            [ Button.secondary { label = "−", onPress = Just DecrementBadge } |> Button.toMarkup
            , Badge.unreadBadge model.badgeCount |> Badge.toMarkup
            , Button.secondary { label = "+", onPress = Just IncrementBadge } |> Button.toMarkup
            ]
        ]



-- BANNER


viewBanner : Html Msg
viewBanner =
    section "Banner"
        "Banners display important, site-wide information at the top of the page."
        [ label "Variants"
        , rowVertical
            [ Banner.banner "Default banner — general site-wide information."
                |> Banner.toMarkup
            , Banner.banner "Info banner — informational site-wide message."
                |> Banner.withInfo
                |> Banner.toMarkup
            , Banner.banner "Success banner — a site-wide success event occurred."
                |> Banner.withSuccess
                |> Banner.toMarkup
            , Banner.banner "Warning banner — attention required site-wide."
                |> Banner.withWarning
                |> Banner.toMarkup
            , Banner.banner "Danger banner — a critical site-wide issue."
                |> Banner.withDanger
                |> Banner.toMarkup
            ]
        , label "With link"
        , rowVertical
            [ Banner.banner "Scheduled maintenance window this Friday."
                |> Banner.withInfo
                |> Banner.withLink { label = "Learn more", href = "#" }
                |> Banner.toMarkup
            ]
        ]



-- BREADCRUMB


viewBreadcrumb : Html Msg
viewBreadcrumb =
    section "Breadcrumb"
        "Breadcrumbs provide users with a sense of location within a navigation hierarchy."
        [ label "Basic"
        , row
            [ Breadcrumb.breadcrumb
                [ Breadcrumb.item { label = "Home", href = "#" }
                , Breadcrumb.item { label = "Settings", href = "#" }
                , Breadcrumb.currentItem "Profile"
                ]
                |> Breadcrumb.toMarkup
            ]
        , label "Deeper hierarchy"
        , row
            [ Breadcrumb.breadcrumb
                [ Breadcrumb.item { label = "Home", href = "#" }
                , Breadcrumb.item { label = "Projects", href = "#" }
                , Breadcrumb.item { label = "Acme Corp", href = "#" }
                , Breadcrumb.item { label = "Resources", href = "#" }
                , Breadcrumb.currentItem "Instances"
                ]
                |> Breadcrumb.toMarkup
            ]
        ]



-- BUTTON


viewButton : Html Msg
viewButton =
    section "Button"
        "Buttons allow users to trigger an action or navigate to another page."
        [ label "Variants"
        , row
            [ Button.primary { label = "Primary", onPress = Nothing } |> Button.toMarkup
            , Button.secondary { label = "Secondary", onPress = Nothing } |> Button.toMarkup
            , Button.tertiary { label = "Tertiary", onPress = Nothing } |> Button.toMarkup
            , Button.danger { label = "Danger", onPress = Nothing } |> Button.toMarkup
            , Button.warning { label = "Warning", onPress = Nothing } |> Button.toMarkup
            , Button.link { label = "Link", onPress = Nothing } |> Button.toMarkup
            , Button.plain { label = "Plain", onPress = Nothing } |> Button.toMarkup
            , Button.control { label = "Control", onPress = Nothing } |> Button.toMarkup
            ]
        , label "Sizes"
        , row
            [ Button.primary { label = "Small", onPress = Nothing } |> Button.withSmallSize |> Button.toMarkup
            , Button.primary { label = "Default", onPress = Nothing } |> Button.toMarkup
            , Button.primary { label = "Large", onPress = Nothing } |> Button.withLargeSize |> Button.toMarkup
            ]
        , label "Disabled"
        , row
            [ Button.primary { label = "Primary", onPress = Nothing } |> Button.withDisabled |> Button.toMarkup
            , Button.secondary { label = "Secondary", onPress = Nothing } |> Button.withDisabled |> Button.toMarkup
            , Button.link { label = "Link", onPress = Nothing } |> Button.withDisabled |> Button.toMarkup
            ]
        , label "With icon (using Font Awesome — included in PF CSS)"
        , row
            [ Button.primary { label = "Add item", onPress = Nothing }
                |> Button.withIcon (Html.i [ Attr.class "fas fa-plus" ] [])
                |> Button.toMarkup
            , Button.secondary { label = "Download", onPress = Nothing }
                |> Button.withIcon (Html.i [ Attr.class "fas fa-download" ] [])
                |> Button.withIconRight
                |> Button.toMarkup
            ]
        ]



-- CARD


viewCard : Html Msg
viewCard =
    section "Card"
        "Cards are square or rectangular containers that can hold any kind of content."
        [ label "Basic"
        , row
            [ Card.card [ p [] [ text "This is the card body content." ] ]
                |> Card.withTitle "Card title"
                |> Card.toMarkup
            ]
        , label "With footer"
        , row
            [ Card.card [ p [] [ text "Card with a title, body, and footer." ] ]
                |> Card.withTitle "Card with footer"
                |> Card.withFooter
                    (div [ Attr.style "display" "flex", Attr.style "gap" "0.5rem" ]
                        [ Button.primary { label = "Action", onPress = Nothing } |> Button.toMarkup
                        , Button.link { label = "Cancel", onPress = Nothing } |> Button.toMarkup
                        ]
                    )
                |> Card.toMarkup
            ]
        , label "Variants"
        , row
            [ Card.card [ p [] [ text "Flat card — no shadow." ] ]
                |> Card.withTitle "Flat"
                |> Card.withFlat
                |> Card.toMarkup
            , Card.card [ p [] [ text "Compact card — reduced padding." ] ]
                |> Card.withTitle "Compact"
                |> Card.withCompact
                |> Card.toMarkup
            , Card.card [ p [] [ text "Rounded card — larger radius." ] ]
                |> Card.withTitle "Rounded"
                |> Card.withRounded
                |> Card.toMarkup
            ]
        ]



-- CHECKBOX


viewCheckbox : Model -> Html Msg
viewCheckbox model =
    section "Checkbox"
        "Checkboxes allow users to select one or more items from a list."
        [ label "Basic"
        , row
            [ Checkbox.checkbox { id = "check-1", onChange = ToggleCheckbox }
                |> Checkbox.withLabel "Uncontrolled checkbox"
                |> Checkbox.withChecked model.checkboxChecked
                |> Checkbox.toMarkup
            , Checkbox.checkbox { id = "check-2", onChange = ToggleCheckbox2 }
                |> Checkbox.withLabel "Pre-checked checkbox"
                |> Checkbox.withChecked model.checkbox2Checked
                |> Checkbox.toMarkup
            ]
        , label "With description"
        , row
            [ Checkbox.checkbox { id = "check-desc", onChange = ToggleCheckbox }
                |> Checkbox.withLabel "Option with description"
                |> Checkbox.withDescription "Helpful context about what this option does."
                |> Checkbox.withChecked model.checkboxChecked
                |> Checkbox.toMarkup
            ]
        , label "Disabled"
        , row
            [ Checkbox.checkbox { id = "check-dis-1", onChange = ToggleCheckbox }
                |> Checkbox.withLabel "Disabled unchecked"
                |> Checkbox.withDisabled
                |> Checkbox.toMarkup
            , Checkbox.checkbox { id = "check-dis-2", onChange = ToggleCheckbox }
                |> Checkbox.withLabel "Disabled checked"
                |> Checkbox.withChecked True
                |> Checkbox.withDisabled
                |> Checkbox.toMarkup
            ]
        ]



-- DIVIDER


viewDivider : Html Msg
viewDivider =
    section "Divider"
        "A horizontal or vertical rule to visually separate content."
        [ label "Horizontal (default)"
        , div [ Attr.style "width" "100%" ]
            [ Divider.divider |> Divider.toMarkup ]
        , label "Horizontal with insets"
        , div [ Attr.style "width" "100%" ]
            [ Divider.divider |> Divider.withInsetMd |> Divider.toMarkup ]
        , label "Vertical (inside a flex row)"
        , div [ Attr.style "display" "flex", Attr.style "align-items" "center", Attr.style "gap" "1rem", Attr.style "height" "2rem" ]
            [ span [] [ text "Left content" ]
            , Divider.divider |> Divider.withVertical |> Divider.toMarkup
            , span [] [ text "Right content" ]
            ]
        ]



-- EMPTY STATE


viewEmptyState : Html Msg
viewEmptyState =
    section "EmptyState"
        "Empty states are used when there is no data to display in a component or page."
        [ label "Default"
        , div [ Attr.style "border" "1px solid var(--pf-t--global--border--color--default)", Attr.style "border-radius" "4px" ]
            [ EmptyState.emptyState
                |> EmptyState.withIcon (Html.i [ Attr.class "fas fa-search", Attr.style "font-size" "3rem", Attr.style "color" "var(--pf-t--global--text--color--subtle)" ] [])
                |> EmptyState.withTitleH2 "No results found"
                |> EmptyState.withBody "No results match the filter criteria. Clear all filters and try again."
                |> EmptyState.withPrimaryAction
                    (Button.primary { label = "Clear all filters", onPress = Nothing } |> Button.toMarkup)
                |> EmptyState.toMarkup
            ]
        , label "Small variant"
        , div [ Attr.style "border" "1px solid var(--pf-t--global--border--color--default)", Attr.style "border-radius" "4px" ]
            [ EmptyState.emptyState
                |> EmptyState.withSmallSize
                |> EmptyState.withTitleH2 "No items"
                |> EmptyState.withBody "Add items to get started."
                |> EmptyState.toMarkup
            ]
        ]



-- HELPER TEXT


viewHelperText : Html Msg
viewHelperText =
    section "HelperText"
        "Helper text provides contextual information below form controls."
        [ label "Variants"
        , rowVertical
            [ HelperText.helperText "Default helper text." |> HelperText.toMarkup
            , HelperText.helperText "Success — validation passed." |> HelperText.withSuccess |> HelperText.toMarkup
            , HelperText.helperText "Warning — check this value." |> HelperText.withWarning |> HelperText.toMarkup
            , HelperText.helperText "Error — this field is required." |> HelperText.withError |> HelperText.toMarkup
            , HelperText.helperText "Indeterminate — validation in progress." |> HelperText.withIndeterminate |> HelperText.toMarkup
            ]
        ]



-- HINT


viewHint : Html Msg
viewHint =
    section "Hint"
        "A contextual hint with lighter styling than an Alert. Used for supplemental guidance."
        [ label "Basic"
        , rowVertical
            [ Hint.hint "This panel shows a filtered list. To see all items, clear the active filters."
                |> Hint.toMarkup
            ]
        , label "With title and actions"
        , rowVertical
            [ Hint.hint "All projects in this workspace share the same network policy. Changes apply to all members."
                |> Hint.withTitle "Workspace note"
                |> Hint.withActions
                    (Button.link { label = "View policy", onPress = Nothing } |> Button.toMarkup)
                |> Hint.toMarkup
            ]
        ]



-- ICON


viewIcon : Html Msg
viewIcon =
    section "Icon"
        "A wrapper for inline SVG or font icons with semantic color status support."
        [ label "Sizes (using Font Awesome)"
        , row
            [ Icon.icon (Html.i [ Attr.class "fas fa-star" ] []) |> Icon.withSmallSize |> Icon.toMarkup
            , Icon.icon (Html.i [ Attr.class "fas fa-star" ] []) |> Icon.toMarkup
            , Icon.icon (Html.i [ Attr.class "fas fa-star" ] []) |> Icon.withLargeSize |> Icon.toMarkup
            , Icon.icon (Html.i [ Attr.class "fas fa-star" ] []) |> Icon.withXLargeSize |> Icon.toMarkup
            ]
        , label "Status colors"
        , row
            [ Icon.icon (Html.i [ Attr.class "fas fa-check-circle" ] []) |> Icon.withSuccessStatus |> Icon.toMarkup
            , Icon.icon (Html.i [ Attr.class "fas fa-times-circle" ] []) |> Icon.withDangerStatus |> Icon.toMarkup
            , Icon.icon (Html.i [ Attr.class "fas fa-exclamation-triangle" ] []) |> Icon.withWarningStatus |> Icon.toMarkup
            , Icon.icon (Html.i [ Attr.class "fas fa-info-circle" ] []) |> Icon.withInfoStatus |> Icon.toMarkup
            ]
        ]



-- LABEL


viewLabel : Model -> Html Msg
viewLabel model =
    section "Label"
        "Labels highlight an item's status for quick recognition, or flag content that needs action."
        [ label "Colors (filled)"
        , row
            [ Label.label "Grey" |> Label.toMarkup
            , Label.label "Blue" |> Label.withBlueColor |> Label.toMarkup
            , Label.label "Green" |> Label.withGreenColor |> Label.toMarkup
            , Label.label "Orange" |> Label.withOrangeColor |> Label.toMarkup
            , Label.label "Red" |> Label.withRedColor |> Label.toMarkup
            , Label.label "Purple" |> Label.withPurpleColor |> Label.toMarkup
            , Label.label "Cyan" |> Label.withCyanColor |> Label.toMarkup
            , Label.label "Gold" |> Label.withGoldColor |> Label.toMarkup
            ]
        , label "Outline"
        , row
            [ Label.label "Grey" |> Label.withOutline |> Label.toMarkup
            , Label.label "Blue" |> Label.withBlueColor |> Label.withOutline |> Label.toMarkup
            , Label.label "Green" |> Label.withGreenColor |> Label.withOutline |> Label.toMarkup
            , Label.label "Red" |> Label.withRedColor |> Label.withOutline |> Label.toMarkup
            ]
        , label "Compact"
        , row
            [ Label.label "Compact grey" |> Label.withCompact |> Label.toMarkup
            , Label.label "Compact blue" |> Label.withBlueColor |> Label.withCompact |> Label.toMarkup
            ]
        , label "With close (click × to dismiss)"
        , row
            [ if model.labelVisible then
                Label.label "Dismissible"
                    |> Label.withBlueColor
                    |> Label.withCloseMsg DismissLabel
                    |> Label.toMarkup

              else
                text "(dismissed)"
            ]
        ]



-- LIST


viewList : Html Msg
viewList =
    section "List"
        "Lists display items in a simple list format with optional styling variants."
        [ label "Bulleted (default)"
        , PFList.pFList
            [ text "First list item"
            , text "Second list item"
            , text "Third list item"
            ]
            |> PFList.toMarkup
        , label "Ordered"
        , PFList.pFList
            [ text "First step"
            , text "Second step"
            , text "Third step"
            ]
            |> PFList.withOrdered
            |> PFList.toMarkup
        , label "Plain"
        , PFList.pFList
            [ text "Plain item one"
            , text "Plain item two"
            , text "Plain item three"
            ]
            |> PFList.withPlain
            |> PFList.toMarkup
        , label "Inline"
        , PFList.pFList
            [ text "Cats"
            , text "Dogs"
            , text "Birds"
            , text "Fish"
            ]
            |> PFList.withInlined
            |> PFList.toMarkup
        ]



-- SPINNER


viewSpinner : Html Msg
viewSpinner =
    section "Spinner"
        "Spinners indicate that content is loading. Animation is driven by PatternFly CSS — no JavaScript needed."
        [ label "Sizes"
        , row
            [ Spinner.spinner |> Spinner.withSmallSize |> Spinner.toMarkup
            , Spinner.spinner |> Spinner.toMarkup
            , Spinner.spinner |> Spinner.withLargeSize |> Spinner.toMarkup
            , Spinner.spinner |> Spinner.withXLargeSize |> Spinner.toMarkup
            ]
        ]



-- ACTION LIST


viewActionList : Html Msg
viewActionList =
    section "ActionList"
        "An action list is a group of actions, controls, or buttons with set spacing."
        [ label "Basic"
        , ActionList.actionList
            |> ActionList.withItem
                (Button.primary { label = "Confirm", onPress = Nothing }
                    |> Button.toMarkup
                )
            |> ActionList.withItem
                (Button.secondary { label = "Cancel", onPress = Nothing }
                    |> Button.toMarkup
                )
            |> ActionList.toMarkup
        , label "With group"
        , ActionList.actionList
            |> ActionList.withGroup
                [ Button.primary { label = "Next", onPress = Nothing }
                    |> Button.toMarkup
                , Button.secondary { label = "Back", onPress = Nothing }
                    |> Button.toMarkup
                ]
            |> ActionList.withItem
                (Button.link { label = "Cancel", onPress = Nothing }
                    |> Button.toMarkup
                )
            |> ActionList.toMarkup
        ]



-- CODE BLOCK


viewCodeBlock : Html Msg
viewCodeBlock =
    section "CodeBlock"
        "Code blocks display read-only code in a formatted, accessible container."
        [ label "Basic"
        , CodeBlock.codeBlock "apiVersion: v1\nkind: Pod\nmetadata:\n  name: my-pod"
            |> CodeBlock.toMarkup
        , label "With actions"
        , CodeBlock.codeBlock "elm make src/Main.elm --output=main.js"
            |> CodeBlock.withActions
                (Button.plain { label = "Copy", onPress = Nothing }
                    |> Button.toMarkup
                )
            |> CodeBlock.toMarkup
        ]



-- DESCRIPTION LIST


viewDescriptionList : Html Msg
viewDescriptionList =
    section "DescriptionList"
        "Description lists display term/value pairs for metadata and detail panels."
        [ label "Default"
        , DescriptionList.descriptionList
            [ DescriptionList.group "Name" (text "Example pod")
            , DescriptionList.group "Namespace" (text "my-namespace")
            , DescriptionList.group "Status" (text "Running")
            , DescriptionList.group "Created" (text "2 hours ago")
            ]
            |> DescriptionList.toMarkup
        , label "Horizontal"
        , DescriptionList.descriptionList
            [ DescriptionList.group "CPU" (text "250m")
            , DescriptionList.group "Memory" (text "128Mi")
            , DescriptionList.group "Replicas" (text "3")
            ]
            |> DescriptionList.withHorizontal
            |> DescriptionList.toMarkup
        , label "Compact"
        , DescriptionList.descriptionList
            [ DescriptionList.group "Cluster" (text "production")
            , DescriptionList.group "Region" (text "us-east-1")
            , DescriptionList.group "Zone" (text "us-east-1a")
            ]
            |> DescriptionList.withCompact
            |> DescriptionList.toMarkup
        ]



-- EXPANDABLE SECTION


viewExpandableSection : Model -> Html Msg
viewExpandableSection model =
    section "ExpandableSection"
        "Expandable sections toggle the visibility of content."
        [ label "Default (click to toggle)"
        , ExpandableSection.expandableSection
            { body =
                p [] [ text "This is the hidden content that appears when the section is expanded. It can contain any HTML elements." ]
            , isExpanded = model.expandableSectionOpen
            , onToggle = ToggleExpandableSection
            }
            |> ExpandableSection.toMarkup
        , label "Custom toggle text"
        , ExpandableSection.expandableSection
            { body =
                p [] [ text "Advanced configuration options go here." ]
            , isExpanded = False
            , onToggle = \_ -> ToggleExpandableSection False
            }
            |> ExpandableSection.withToggleText "Collapse advanced options"
            |> ExpandableSection.withToggleTextCollapsed "Show advanced options"
            |> ExpandableSection.toMarkup
        , label "Indented"
        , ExpandableSection.expandableSection
            { body =
                p [] [ text "Indented content is offset from the toggle." ]
            , isExpanded = True
            , onToggle = \_ -> ToggleExpandableSection True
            }
            |> ExpandableSection.withIndented
            |> ExpandableSection.toMarkup
        ]



-- PROGRESS


viewProgress : Html Msg
viewProgress =
    section "Progress"
        "Progress bars communicate the status of an ongoing process."
        [ label "Default (33%)"
        , Progress.progress { id = "prog-default", value = 33 }
            |> Progress.withTitle "Title"
            |> Progress.toMarkup
        , label "Success (100%)"
        , Progress.progress { id = "prog-success", value = 100 }
            |> Progress.withTitle "Completed"
            |> Progress.withSuccess
            |> Progress.toMarkup
        , label "Danger (25%)"
        , Progress.progress { id = "prog-danger", value = 25 }
            |> Progress.withTitle "Error"
            |> Progress.withDanger
            |> Progress.toMarkup
        , label "Warning (66%)"
        , Progress.progress { id = "prog-warning", value = 66 }
            |> Progress.withTitle "Warning"
            |> Progress.withWarning
            |> Progress.toMarkup
        , label "Small"
        , Progress.progress { id = "prog-small", value = 50 }
            |> Progress.withTitle "Uploading"
            |> Progress.withSmallSize
            |> Progress.toMarkup
        , label "Inside measure"
        , Progress.progress { id = "prog-inside", value = 75 }
            |> Progress.withTitle "Processing"
            |> Progress.withInside
            |> Progress.toMarkup
        , label "No measure"
        , Progress.progress { id = "prog-none", value = 45 }
            |> Progress.withTitle "Loading"
            |> Progress.withNone
            |> Progress.toMarkup
        ]



-- RADIO


viewRadio : Model -> Html Msg
viewRadio model =
    section "Radio"
        "Radio buttons allow users to select one option from a set."
        [ label "Basic group (click to select)"
        , Radio.radio
            { id = "radio-option-1"
            , name = "radio-group-1"
            , onChange = \_ -> SelectRadio "option-1"
            }
            |> Radio.withLabel "Option 1"
            |> Radio.withChecked (model.selectedRadio == "option-1")
            |> Radio.toMarkup
        , Radio.radio
            { id = "radio-option-2"
            , name = "radio-group-1"
            , onChange = \_ -> SelectRadio "option-2"
            }
            |> Radio.withLabel "Option 2"
            |> Radio.withChecked (model.selectedRadio == "option-2")
            |> Radio.toMarkup
        , Radio.radio
            { id = "radio-option-3"
            , name = "radio-group-1"
            , onChange = \_ -> SelectRadio "option-3"
            }
            |> Radio.withLabel "Option 3"
            |> Radio.withChecked (model.selectedRadio == "option-3")
            |> Radio.toMarkup
        , label "With description"
        , Radio.radio
            { id = "radio-desc-1"
            , name = "radio-group-2"
            , onChange = \_ -> SelectRadio "desc-1"
            }
            |> Radio.withLabel "Standard"
            |> Radio.withDescription "Best for small workloads"
            |> Radio.withChecked True
            |> Radio.toMarkup
        , Radio.radio
            { id = "radio-desc-2"
            , name = "radio-group-2"
            , onChange = \_ -> SelectRadio "desc-2"
            }
            |> Radio.withLabel "Professional"
            |> Radio.withDescription "Best for medium workloads"
            |> Radio.toMarkup
        , label "Disabled"
        , Radio.radio
            { id = "radio-disabled"
            , name = "radio-group-3"
            , onChange = \_ -> SelectRadio "disabled"
            }
            |> Radio.withLabel "Disabled option"
            |> Radio.withDisabled
            |> Radio.toMarkup
        ]



-- TITLE


viewTitle : Html Msg
viewTitle =
    section "Title"
        "Titles provide semantic heading levels (h1–h6) with PF6 typography sizing."
        [ label "All levels"
        , Title.title "Page Title (h1 / 4xl)" |> Title.withH1 |> Title.toMarkup
        , Title.title "Section Title (h2 / 3xl)" |> Title.withH2 |> Title.toMarkup
        , Title.title "Subsection (h3 / 2xl)" |> Title.withH3 |> Title.toMarkup
        , Title.title "Group heading (h4 / xl)" |> Title.withH4 |> Title.toMarkup
        , Title.title "Small heading (h5 / lg)" |> Title.withH5 |> Title.toMarkup
        , Title.title "Micro heading (h6 / md)" |> Title.withH6 |> Title.toMarkup
        ]



-- BULLSEYE


viewBullseye : Html Msg
viewBullseye =
    section "Bullseye"
        "Bullseye centers a single piece of content both horizontally and vertically."
        [ label "Centered content"
        , Bullseye.bullseye
            (Badge.badge 42 |> Badge.toMarkup)
            |> Bullseye.withAttributes
                [ Attr.style "height" "100px"
                , Attr.style "border" "1px dashed var(--pf-t--global--border--color--default)"
                ]
            |> Bullseye.toMarkup
        ]



-- STACK


viewStack : Html Msg
viewStack =
    section "Stack"
        "Stack arranges items in a single vertical column."
        [ label "Default"
        , Stack.stack
            [ Stack.stackItem (p [] [ text "First item" ])
            , Stack.stackItem (p [] [ text "Second item" ])
            , Stack.stackItem (p [] [ text "Third item" ])
            ]
            |> Stack.toMarkup
        , label "With gutter"
        , Stack.stack
            [ Stack.stackItem (Card.card [] |> Card.withTitle "Card A" |> Card.toMarkup)
            , Stack.stackItem (Card.card [] |> Card.withTitle "Card B" |> Card.toMarkup)
            ]
            |> Stack.withGutter
            |> Stack.toMarkup
        ]



-- SPLIT


viewSplit : Html Msg
viewSplit =
    section "Split"
        "Split distributes items horizontally. One item can fill the remaining space."
        [ label "Basic"
        , Split.split
            [ Split.splitItem (span [] [ text "Left" ])
            , Split.splitItem (span [] [ text "Middle" ]) |> Split.withFill
            , Split.splitItem (span [] [ text "Right" ])
            ]
            |> Split.toMarkup
        , label "With gutter"
        , Split.split
            [ Split.splitItem (Badge.badge 3 |> Badge.toMarkup)
            , Split.splitItem (span [] [ text "Item count" ]) |> Split.withFill
            , Split.splitItem (Button.secondary { label = "Action", onPress = Nothing } |> Button.toMarkup)
            ]
            |> Split.withGutter
            |> Split.toMarkup
        ]



-- LEVEL


viewLevel : Html Msg
viewLevel =
    section "Level"
        "Level distributes items evenly across a horizontal row with wrapping."
        [ label "Default"
        , Level.level
            [ span [] [ text "Left content" ]
            , span [] [ text "Center content" ]
            , span [] [ text "Right content" ]
            ]
            |> Level.toMarkup
        , label "With gutter"
        , Level.level
            [ Title.title "Section heading" |> Title.withH2 |> Title.toMarkup
            , Button.primary { label = "Add item", onPress = Nothing } |> Button.toMarkup
            ]
            |> Level.withGutter
            |> Level.toMarkup
        ]



-- GALLERY


viewGallery : Html Msg
viewGallery =
    section "Gallery"
        "Gallery is a responsive grid of uniform items that wrap and maintain consistent sizing."
        [ label "With gutter"
        , Gallery.gallery
            [ Card.card [] |> Card.withTitle "Item 1" |> Card.toMarkup
            , Card.card [] |> Card.withTitle "Item 2" |> Card.toMarkup
            , Card.card [] |> Card.withTitle "Item 3" |> Card.toMarkup
            , Card.card [] |> Card.withTitle "Item 4" |> Card.toMarkup
            , Card.card [] |> Card.withTitle "Item 5" |> Card.toMarkup
            ]
            |> Gallery.withGutter
            |> Gallery.withMinWidth "200px"
            |> Gallery.toMarkup
        ]



-- GRID


viewGrid : Html Msg
viewGrid =
    section "Grid"
        "Grid is a 12-column grid system. Each item specifies how many columns it spans."
        [ label "Unequal spans"
        , Grid.grid
            [ Grid.gridItem (Card.card [] |> Card.withTitle "Span 4" |> Card.toMarkup)
                |> Grid.withSpan 4
            , Grid.gridItem (Card.card [] |> Card.withTitle "Span 8" |> Card.toMarkup)
                |> Grid.withSpan 8
            ]
            |> Grid.withGutter
            |> Grid.toMarkup
        , label "Equal thirds"
        , Grid.grid
            [ Grid.gridItem (Card.card [] |> Card.withTitle "Col 1" |> Card.toMarkup)
                |> Grid.withSpan 4
            , Grid.gridItem (Card.card [] |> Card.withTitle "Col 2" |> Card.toMarkup)
                |> Grid.withSpan 4
            , Grid.gridItem (Card.card [] |> Card.withTitle "Col 3" |> Card.toMarkup)
                |> Grid.withSpan 4
            ]
            |> Grid.withGutter
            |> Grid.toMarkup
        ]



-- FLEX


viewFlex : Html Msg
viewFlex =
    section "Flex"
        "Flex layout with configurable direction, alignment, justification, and gap."
        [ label "Row (default)"
        , Flex.flex
            [ Badge.badge 1 |> Badge.toMarkup
            , Badge.badge 2 |> Badge.withReadStatus |> Badge.toMarkup
            , Badge.badge 3 |> Badge.toMarkup
            ]
            |> Flex.withGapMd
            |> Flex.toMarkup
        , label "Column"
        , Flex.flex
            [ span [] [ text "First" ]
            , span [] [ text "Second" ]
            , span [] [ text "Third" ]
            ]
            |> Flex.withColumn
            |> Flex.withGapSm
            |> Flex.toMarkup
        , label "Space between"
        , Flex.flex
            [ span [] [ text "Left" ]
            , span [] [ text "Right" ]
            ]
            |> Flex.withJustifySpaceBetween
            |> Flex.withAttributes [ Attr.style "width" "100%" ]
            |> Flex.toMarkup
        ]



-- TEXT INPUT


viewTextInput : Model -> Html Msg
viewTextInput model =
    section "TextInput"
        "Text inputs gather free-form text from the user."
        [ label ("Default (value: \"" ++ model.textInputValue ++ "\")")
        , TextInput.textInput
            { id = "text-input-default"
            , value = model.textInputValue
            , onChange = TextInputChanged
            }
            |> TextInput.withPlaceholder "Enter text…"
            |> TextInput.toMarkup
        , label "Success"
        , TextInput.textInput
            { id = "text-input-success"
            , value = "valid@example.com"
            , onChange = \_ -> TextInputChanged ""
            }
            |> TextInput.withSuccess
            |> TextInput.toMarkup
        , label "Danger"
        , TextInput.textInput
            { id = "text-input-danger"
            , value = "bad input"
            , onChange = \_ -> TextInputChanged ""
            }
            |> TextInput.withDanger
            |> TextInput.toMarkup
        , label "Warning"
        , TextInput.textInput
            { id = "text-input-warning"
            , value = "maybe ok"
            , onChange = \_ -> TextInputChanged ""
            }
            |> TextInput.withWarning
            |> TextInput.toMarkup
        , label "Disabled"
        , TextInput.textInput
            { id = "text-input-disabled"
            , value = "disabled value"
            , onChange = \_ -> TextInputChanged ""
            }
            |> TextInput.withDisabled
            |> TextInput.toMarkup
        , label "Password"
        , TextInput.textInput
            { id = "text-input-password"
            , value = "secret"
            , onChange = \_ -> TextInputChanged ""
            }
            |> TextInput.withPasswordType
            |> TextInput.toMarkup
        ]



-- TEXT AREA


viewTextArea : Model -> Html Msg
viewTextArea model =
    section "TextArea"
        "Text areas gather multi-line text from the user."
        [ label ("Default (value: " ++ String.fromInt (String.length model.textAreaValue) ++ " chars)")
        , TextArea.textArea
            { id = "textarea-default"
            , value = model.textAreaValue
            , onChange = TextAreaChanged
            }
            |> TextArea.withPlaceholder "Enter a description…"
            |> TextArea.withRows 4
            |> TextArea.toMarkup
        , label "No resize"
        , TextArea.textArea
            { id = "textarea-noresize"
            , value = "This textarea cannot be resized."
            , onChange = \_ -> TextAreaChanged ""
            }
            |> TextArea.withResizeNone
            |> TextArea.withRows 3
            |> TextArea.toMarkup
        , label "Danger"
        , TextArea.textArea
            { id = "textarea-danger"
            , value = "Invalid content"
            , onChange = \_ -> TextAreaChanged ""
            }
            |> TextArea.withDanger
            |> TextArea.withRows 2
            |> TextArea.toMarkup
        ]



-- SELECT


viewSelect : Model -> Html Msg
viewSelect model =
    section "Select"
        "A native HTML select for choosing a single value from a list."
        [ label ("Default (selected: \"" ++ model.selectValue ++ "\")")
        , Select.select
            { id = "select-default"
            , value = model.selectValue
            , onChange = SelectChanged
            , options =
                [ Select.option "east" "us-east-1"
                , Select.option "west" "us-west-2"
                , Select.option "eu" "eu-west-1"
                , Select.option "ap" "ap-southeast-1"
                ]
            }
            |> Select.withPlaceholder "Select a region"
            |> Select.toMarkup
        , label "Disabled"
        , Select.select
            { id = "select-disabled"
            , value = "east"
            , onChange = \_ -> SelectChanged ""
            , options =
                [ Select.option "east" "us-east-1"
                , Select.option "west" "us-west-2"
                ]
            }
            |> Select.withDisabled
            |> Select.toMarkup
        ]



-- SWITCH


viewSwitch : Model -> Html Msg
viewSwitch model =
    section "Switch"
        "A switch toggles the state of a setting on or off."
        [ label ("Default (state: " ++ (if model.switchChecked then "on" else "off") ++ ")")
        , Switch.switch { id = "switch-default", onChange = SwitchToggled }
            |> Switch.withLabel "Enable feature"
            |> Switch.withChecked model.switchChecked
            |> Switch.toMarkup
        , label "With off label"
        , Switch.switch { id = "switch-labels", onChange = SwitchToggled }
            |> Switch.withLabel "Active"
            |> Switch.withLabelOff "Inactive"
            |> Switch.withChecked model.switchChecked
            |> Switch.toMarkup
        , label "Disabled"
        , Switch.switch { id = "switch-disabled", onChange = SwitchToggled }
            |> Switch.withLabel "Always off"
            |> Switch.withDisabled
            |> Switch.toMarkup
        , label "Disabled (checked)"
        , Switch.switch { id = "switch-disabled-on", onChange = SwitchToggled }
            |> Switch.withLabel "Always on"
            |> Switch.withChecked True
            |> Switch.withDisabled
            |> Switch.toMarkup
        ]



-- SEARCH INPUT


viewSearchInput : Model -> Html Msg
viewSearchInput model =
    section "SearchInput"
        "SearchInput provides a text field for search with icon and clear button."
        [ label ("Default (query: \"" ++ model.searchValue ++ "\")")
        , SearchInput.searchInput
            { value = model.searchValue
            , onChange = SearchChanged
            }
            |> SearchInput.withPlaceholder "Search pods…"
            |> SearchInput.withClearMsg (SearchChanged "")
            |> SearchInput.toMarkup
        , label "Without clear button"
        , SearchInput.searchInput
            { value = ""
            , onChange = \_ -> SearchChanged ""
            }
            |> SearchInput.withPlaceholder "Search…"
            |> SearchInput.toMarkup
        ]



-- NUMBER INPUT


viewNumberInput : Model -> Html Msg
viewNumberInput model =
    section "NumberInput"
        "NumberInput allows users to enter a numeric value with stepper controls."
        [ label ("Default (value: " ++ String.fromInt model.numberValue ++ ")")
        , NumberInput.numberInput
            { value = model.numberValue
            , onChange = NumberChanged
            }
            |> NumberInput.toMarkup
        , label "With min/max (1–10)"
        , NumberInput.numberInput
            { value = model.numberValue
            , onChange = NumberChanged
            }
            |> NumberInput.withMin 1
            |> NumberInput.withMax 10
            |> NumberInput.toMarkup
        , label "With unit"
        , NumberInput.numberInput
            { value = model.numberValue
            , onChange = NumberChanged
            }
            |> NumberInput.withMin 0
            |> NumberInput.withUnit "GB"
            |> NumberInput.toMarkup
        , label "Disabled"
        , NumberInput.numberInput
            { value = 5
            , onChange = NumberChanged
            }
            |> NumberInput.withDisabled
            |> NumberInput.toMarkup
        ]



-- MASTHEAD


viewMasthead : Model -> Html Msg
viewMasthead model =
    section "Masthead"
        "Masthead provides the top horizontal bar with brand and toolbar areas."
        [ label "With toggle, brand, and toolbar"
        , Masthead.masthead
            |> Masthead.withToggle ToggleSidebar
            |> Masthead.withBrand
                (span
                    [ Attr.style "font-weight" "700"
                    , Attr.style "padding" "0 1rem"
                    ]
                    [ text "My App" ]
                )
            |> Masthead.withToolbar
                (span [ Attr.style "padding" "0 1rem" ] [ text "User ▾" ])
            |> Masthead.toMarkup
        , label "Brand only"
        , Masthead.masthead
            |> Masthead.withBrand
                (span [ Attr.style "padding" "0 1rem", Attr.style "font-weight" "700" ]
                    [ text "Minimal App" ]
                )
            |> Masthead.toMarkup
        ]



-- NAVIGATION


viewNavigation : Model -> Html Msg
viewNavigation model =
    section "Navigation"
        "Navigation provides a sidebar menu for moving between pages of an application."
        [ label "Simple links"
        , Navigation.navigation
            [ Navigation.navLinkMsg "Dashboard" (SearchChanged "")
                |> Navigation.withCurrentLink
            , Navigation.navLinkMsg "Clusters" (SearchChanged "")
            , Navigation.navLinkMsg "Deployments" (SearchChanged "")
            , Navigation.navLinkMsg "Settings" (SearchChanged "")
            ]
            |> Navigation.toMarkup
        , label "With expandable section"
        , Navigation.navigation
            [ Navigation.navLinkMsg "Dashboard" (SearchChanged "")
                |> Navigation.withCurrentLink
            , Navigation.navExpandable
                { title = "Workloads"
                , isExpanded = model.navExpandedSection
                , onToggle = ToggleNavSection
                }
                [ Navigation.navLinkMsg "Pods" (SearchChanged "")
                , Navigation.navLinkMsg "Deployments" (SearchChanged "")
                , Navigation.navLinkMsg "StatefulSets" (SearchChanged "")
                ]
            , Navigation.navLinkMsg "Settings" (SearchChanged "")
            ]
            |> Navigation.withAriaLabel "Application"
            |> Navigation.toMarkup
        , label "With group headings"
        , Navigation.navigation
            [ Navigation.navGroup "Storage"
                [ Navigation.navLinkMsg "Persistent Volumes" (SearchChanged "")
                , Navigation.navLinkMsg "Storage Classes" (SearchChanged "")
                ]
            , Navigation.navGroup "Networking"
                [ Navigation.navLinkMsg "Services" (SearchChanged "")
                , Navigation.navLinkMsg "Ingresses" (SearchChanged "")
                ]
            ]
            |> Navigation.toMarkup
        ]



-- PAGE


viewPage : Model -> Html Msg
viewPage model =
    section "Page"
        "Page composes a Masthead, Sidebar (with Navigation), and main content area into a full application layout."
        [ label "Full page layout (sidebar toggle controls expansion)"
        , div [ Attr.style "height" "300px", Attr.style "position" "relative", Attr.style "overflow" "hidden", Attr.style "border" "1px solid var(--pf-t--global--border--color--default)" ]
            [ Page.page
                [ Page.pageSection
                    (div []
                        [ Title.title "Page Demo"
                            |> Title.withH2
                            |> Title.toMarkup
                        , p [] [ text "Sidebar is ", text (if model.sidebarExpanded then "expanded" else "collapsed"), text ". Click the hamburger in the masthead to toggle." ]
                        ]
                    )
                ]
                |> Page.withMasthead
                    (Masthead.masthead
                        |> Masthead.withToggle ToggleSidebar
                        |> Masthead.withBrand
                            (span [ Attr.style "padding" "0 1rem", Attr.style "font-weight" "700" ]
                                [ text "My App" ]
                            )
                        |> Masthead.toMarkup
                    )
                |> Page.withSidebar
                    (Navigation.navigation
                        [ Navigation.navLinkMsg "Dashboard" (SearchChanged "")
                            |> Navigation.withCurrentLink
                        , Navigation.navLinkMsg "Clusters" (SearchChanged "")
                        , Navigation.navLinkMsg "Settings" (SearchChanged "")
                        ]
                        |> Navigation.toMarkup
                    )
                |> Page.withSidebarExpanded model.sidebarExpanded
                |> Page.toMarkup
            ]
        ]
