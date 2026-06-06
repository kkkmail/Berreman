/// §D — the office-style ribbon-and-menu shell + the standard element catalogue
/// (Spec 0026 Part D + Part F, slice 006). The ribbon presents task tabs whose controls
/// are GENERATED from the slice-005 centralized command registry (`Commands.registry`),
/// never hand-placed per tab (D.1.2); when the ribbon is collapsed the SAME top-level
/// entries expand into full drop-down menus, so a command added once to the registry
/// appears in both the ribbon and the menus with no second wiring site (constraint 0.4 /
/// AC-D2). The contextual Element tab is APPENDED beside the persistent tabs when an
/// element is selected (D.3). The Build tab surfaces the standard catalogue (Part F):
/// Optical Table + the eight `CatalogueKind`s (LS/LP/CP/S/Lens/Flat Mirror/Curved
/// Mirror/D) — no "analyzer" entry, and a chain may hold more than two polarizers
/// (F.1). The Settings tab carries the language/theme selectors and the *Legacy* entry.
///
/// The module is authored against the public MIT `Avalonia.FuncUI` DSL (constraint 0.3);
/// the audit-gated clone stays UNREFERENCED. The tab classification + the catalogue +
/// the element local-menu actions are PURE, comparable values (no Avalonia type), so the
/// single-source ribbon↔menu equivalence and the catalogue roster are provable headless.
/// To keep the compile order acyclic the ribbon does NOT reference `Shell`: shell-level
/// actions (open Legacy / set language / toggle theme) arrive as plain callbacks the
/// shell wires to its `RootMsg`. Command-button labels and help text resolve through the
/// slice-003 `Localization` resource (the EN/RU `strings.json`, D.4 bilingual layout).
module OpticalConstructor.Ui.Ribbon

open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Ui.Commands
open OpticalConstructor.Ui.Localization
open OpticalConstructor.Ui.UserEnvironment

/// Alias the local-help module so the `Commands.LocalHelp` command case (brought in by
/// `open Commands`) never collides with the module name in value position.
module LH = OpticalConstructor.Ui.LocalHelp

// ---------------------------------------------------------------------------
// Pure ribbon model + tab vocabulary (D.1.1). No Avalonia type.
// ---------------------------------------------------------------------------

/// The ribbon's task tabs (D.1.1). `ElementTab` is the contextual element tab, appended
/// only when an element is selected (D.3); the other four are persistent.
type RibbonTab =
    | Build
    | ElementTab
    | TraceView
    | Experiment
    | Settings

/// The persistent tab order (D.1.1) — `ElementTab` is appended contextually (D.3).
let persistentTabs : RibbonTab list = [ Build; TraceView; Experiment; Settings ]

/// Every tab in the ribbon's vocabulary, including the contextual element tab.
let allTabs : RibbonTab list = [ Build; ElementTab; TraceView; Experiment; Settings ]

/// The ribbon's own ephemeral UI state (D.2): whether it is collapsed to its menus, and
/// the selected tab. Pure and serializable (no Avalonia handle).
type Model =
    {
        collapsed : bool
        activeTab : RibbonTab
    }

/// The default ribbon state: expanded, the Build tab selected.
let init : Model = { collapsed = false; activeTab = Build }

/// Ribbon UI messages (D.2): collapse/expand and tab selection.
type Msg =
    | ToggleCollapsed
    | SelectTab of RibbonTab

let update (msg : Msg) (model : Model) : Model =
    match msg with
    | ToggleCollapsed -> { model with collapsed = not model.collapsed }
    | SelectTab tab -> { model with activeTab = tab }

// ---------------------------------------------------------------------------
// THE projection from the one registry (D.1.2 / AC-D2 / constraint 0.4). `tabOf`
// classifies every registry command onto exactly one tab; `tabCommands` is the single
// projection BOTH the expanded ribbon and the collapsed menus render from.
// ---------------------------------------------------------------------------

/// Which tab a registry command surfaces on (D.1.2). Total: any command a later slice
/// adds to the registry without a rule here defaults to the always-present Build tab, so
/// it still appears in BOTH the ribbon and the menus (AC-D2) with no second wiring site.
let tabOf (cmd : Command) : RibbonTab =
    match cmd with
    | PlaceFromRibbon -> Build
    | OpenElementDialog | ElementContextMenu | ResetRotation | DeleteElement
    | DuplicateElement | CopyElement | PasteElement
    | NextElement | PreviousElement
    | RotateR1 | RotateR2 | RotateR3
    | SlideAlongRay | MoveToRay -> ElementTab
    | PanView | ZoomView | ResetView -> TraceView
    // Element groups & multiple detectors (Part G, slice 007) live on the Experiment tab.
    | ToggleGroup | SwapGroup | AddDetector | RemoveDetector | SetPrimaryDetector -> Experiment
    | LocalHelp -> Settings
    // Global file/edit commands (Undo/Redo/Save/Cancel) — and any unclassified future
    // registry command — surface on the always-present Build tab.
    | _ -> Build

/// The registry commands a tab surfaces (D.1.2). The ONE projection the ribbon controls
/// and the collapsed menus both read — there is no second wiring site.
let tabCommands (tab : RibbonTab) : Command list =
    Commands.registry
    |> List.filter (fun d -> tabOf d.command = tab)
    |> List.map (fun d -> d.command)

/// The tabs visible for a constructor model (D.3): the persistent tabs, with the
/// contextual Element tab APPENDED when an element is selected (never taking over).
let visibleTabs (cv : ConstructorView.Model) : RibbonTab list =
    match ConstructorView.activeIndex cv with
    | Some _ -> persistentTabs @ [ ElementTab ]
    | None -> persistentTabs

// ---------------------------------------------------------------------------
// The standard element catalogue (Part F / F.1). Eight roles map through
// `CatalogueKind.toConstructorElement`; the Optical Table is the workspace surface.
// ---------------------------------------------------------------------------

/// A catalogue role surfaced on the Build tab (F.1.1). The eight `ElementRole`s map
/// through `Placement.CatalogueKind.toConstructorElement` (no new DU case for LP/CP —
/// both are `Polarizer`; the engine `Analyzer` case is never produced, F.3); the
/// `TableRole` is the workspace itself (Part C), not a placeable beam element.
type CatalogueRole =
    | TableRole
    | ElementRole of CatalogueKind

/// The standard catalogue roster (F.1.1): Optical Table, then LS/LP/CP/S/Lens/Flat
/// Mirror/Curved Mirror/D. There is deliberately NO "analyzer" role (F.1.2).
let catalogue : CatalogueRole list =
    [
        TableRole
        ElementRole LightSource
        ElementRole LinearPolarizer
        ElementRole CircularPolarizer
        ElementRole Sample
        ElementRole Lens
        ElementRole FlatMirror
        ElementRole CurvedMirror
        ElementRole Detector
    ]

/// The catalogue's placeable element kinds (F.1.1) — the roles that map through
/// `CatalogueKind.toConstructorElement`. A chain may hold more than two polarizers
/// (F.1.2): nothing here limits how many `LinearPolarizer`/`CircularPolarizer` land.
let catalogueKinds : CatalogueKind list =
    catalogue |> List.choose (function TableRole -> None | ElementRole k -> Some k)

// ---------------------------------------------------------------------------
// Localized label keys (D.4 — bilingual, resolving through `strings.json`).
// ---------------------------------------------------------------------------

/// The localization key for a tab title.
let tabTitleKey (tab : RibbonTab) : string =
    match tab with
    | Build -> "ribbon.tab.build"
    | ElementTab -> "ribbon.tab.element"
    | TraceView -> "ribbon.tab.traceView"
    | Experiment -> "ribbon.tab.experiment"
    | Settings -> "ribbon.tab.settings"

/// A tab's localized title (D.4) — resolves through the active language, falling back to
/// English then the key (never blank, never a crash).
let tabTitle (resource : Resource) (language : Language) (tab : RibbonTab) : string =
    lookup resource language (tabTitleKey tab)

/// The localization key for a catalogue role's label.
let roleTitleKey (role : CatalogueRole) : string =
    match role with
    // The optical-table role reuses the existing table-panel title key (slice 003), so the
    // one concept carries one translation — no `element.opticalTable` duplicate to drift.
    | TableRole -> "table.title"
    | ElementRole LightSource -> "element.lightSource"
    | ElementRole LinearPolarizer -> "element.linearPolarizer"
    | ElementRole CircularPolarizer -> "element.circularPolarizer"
    | ElementRole Sample -> "element.sample"
    | ElementRole Lens -> "element.lens"
    | ElementRole FlatMirror -> "element.flatMirror"
    | ElementRole CurvedMirror -> "element.curvedMirror"
    | ElementRole Detector -> "element.detector"

/// A catalogue role's localized label (D.4).
let roleTitle (resource : Resource) (language : Language) (role : CatalogueRole) : string =
    lookup resource language (roleTitleKey role)

// ---------------------------------------------------------------------------
// The dispatch bundle (kept out of `Shell` so the ribbon compiles before it). Each is a
// plain callback the shell wires to the matching `RootMsg`.
// ---------------------------------------------------------------------------

/// The ribbon's dispatch callbacks. `onRibbon`/`onConstructor` drive the ribbon UI and
/// the constructor MVU page; the three shell hooks open the Legacy screen, set the UI
/// language, and toggle the theme — wired by the shell so the ribbon never references it.
type Dispatch =
    {
        onRibbon : Msg -> unit
        onConstructor : ConstructorView.Msg -> unit
        onNavigateLegacy : unit -> unit
        onSetLanguage : Language -> unit
        onToggleTheme : unit -> unit
    }

// ---------------------------------------------------------------------------
// FuncUI rendering (Avalonia boundary). Built from public-Avalonia primitives and the
// reused `Controls.fs` button flavours (AC-J1); no Menu/Expander/clone control.
// ---------------------------------------------------------------------------

/// A short, wrapping text block used for ribbon section labels and modal body text.
let private labelBlock (text : string) : IView =
    TextBlock.create [
        TextBlock.text text
        TextBlock.textWrapping TextWrapping.Wrap
        TextBlock.maxWidth 360.0
        TextBlock.verticalAlignment VerticalAlignment.Center
    ] :> IView

/// A command control: a default-flavour button labelled through `Localization` that
/// dispatches the command via `ConstructorView.Invoke` — the SAME projection the
/// collapsed menu uses (no second wiring site, AC-D2). A gesture-only command (one that
/// needs a wheel notch / drag delta / drop point, so `Invoke` is inert) renders DISABLED
/// in BOTH surfaces rather than as a clickable button that silently does nothing.
let private commandButton (resource : Resource) (language : Language) (d : Dispatch) (cmd : Command) : IView =
    let label = LH.commandLabel resource language cmd
    if ConstructorView.isParameterlessInvokable cmd then
        Controls.defaultButton label (fun () -> d.onConstructor (ConstructorView.Invoke cmd))
    else
        Controls.disabledButton label

/// The catalogue control (F.1): a labelled button per role. An element role drops a new
/// placement — `RibbonDrop` snaps it to the central-ray middle (E.7), so the passed
/// point is irrelevant; the optical-table role deselects to the table workspace (C.3).
let private catalogueView (resource : Resource) (language : Language) (d : Dispatch) : IView =
    let roleButton (role : CatalogueRole) : IView =
        let label = roleTitle resource language role
        match role with
        | TableRole -> Controls.defaultButton label (fun () -> d.onConstructor (ConstructorView.Invoke CancelOrDeselect))
        | ElementRole kind -> Controls.defaultButton label (fun () -> d.onConstructor (ConstructorView.RibbonDrop (kind, TablePoint.origin)))
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 4.0
        StackPanel.children [
            yield labelBlock (lookup resource language "catalogue.title")
            for role in catalogue do yield roleButton role
        ]
    ] :> IView

/// The value-id binding action (F.2.1): opens the working, dismissible empty modal.
let private bindValueButton (resource : Resource) (language : Language) (d : Dispatch) : IView =
    Controls.defaultButton (lookup resource language "valueId.action") (fun () -> d.onConstructor ConstructorView.OpenValueIdModal)

/// The current theme's localized label.
let private themeLabel (resource : Resource) (language : Language) (theme : Theme) : string =
    match theme with
    | Light -> lookup resource language "settings.theme.light"
    | Dark -> lookup resource language "settings.theme.dark"

/// The Settings-tab extras (D.1.1): the language and theme selectors plus the *Legacy*
/// entry (D.5) that opens the present dockable-panel main screen.
let private settingsView (resource : Resource) (language : Language) (env : EnvironmentSettings) (d : Dispatch) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 6.0
        StackPanel.children [
            yield labelBlock (lookup resource language "settings.language")
            yield Controls.toggle (lookup resource language "settings.language.english") (env.language = English) (fun _ -> d.onSetLanguage English)
            yield Controls.toggle (lookup resource language "settings.language.russian") (env.language = Russian) (fun _ -> d.onSetLanguage Russian)
            yield labelBlock (lookup resource language "settings.theme")
            yield Controls.toggle (themeLabel resource language env.theme) (env.theme = Dark) (fun _ -> d.onToggleTheme ())
            yield Controls.defaultButton (lookup resource language "nav.legacy") (fun () -> d.onNavigateLegacy ())
        ]
    ] :> IView

/// The registry-command buttons for a tab (D.1.2) — the ribbon controls generated from
/// the one registry.
let private tabCommandButtons (resource : Resource) (language : Language) (d : Dispatch) (tab : RibbonTab) : IView list =
    tabCommands tab |> List.map (commandButton resource language d)

/// The Experiment-tab extras (Part G/H, slice 007): the group controls + a detector summary.
/// The group member toggles / swaps are PARAMETERIZED by group + member (which a parameterless
/// command `Invoke` cannot carry), so they dispatch the dedicated `GroupToggle`/`GroupSwap`
/// messages; "Group selected element" dispatches `GroupActiveElement`. The detector summary shows
/// the count and which detector is primary (the detector *commands* — Add / Remove / Set as
/// primary — are the Experiment-tab command buttons, acting on the active selection, G.2).
let private experimentView (resource : Resource) (language : Language) (cv : ConstructorView.Model) (d : Dispatch) : IView =
    let memberLabel (m : Groups.GroupMember) : string = roleTitle resource language (ElementRole m.placement.catalogueKind)
    let groupRows : IView list =
        [ for gi, g in List.indexed cv.groups do
            yield labelBlock g.name
            for mi, m in List.indexed g.members do
                yield StackPanel.create [
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.spacing 4.0
                    StackPanel.children [
                        yield Controls.toggle (memberLabel m) m.inBeam
                                (fun on -> d.onConstructor (ConstructorView.GroupToggle (gi, mi, on)))
                        match g.mode with
                        | Groups.MutuallyExclusive ->
                            yield Controls.defaultButton (lookup resource language "experiment.swap")
                                    (fun () -> d.onConstructor (ConstructorView.GroupSwap (gi, mi)))
                        | Groups.MultiSelect -> ()
                    ]
                ] ]
    let detectorSummary : IView =
        let count = List.length (ConstructorView.detectorIndices cv)
        let text =
            match ConstructorView.primaryDetectorIndex cv with
            | Some p -> sprintf "%s: %d — %s #%d" (lookup resource language "experiment.detectors") count (lookup resource language "experiment.primary") p
            | None -> sprintf "%s: 0" (lookup resource language "experiment.detectors")
        labelBlock text
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 4.0
        StackPanel.children (
            [ labelBlock (lookup resource language "experiment.groups")
              Controls.defaultButton (lookup resource language "experiment.newGroup") (fun () -> d.onConstructor ConstructorView.GroupActiveElement) ]
            @ (if List.isEmpty cv.groups then [ labelBlock (lookup resource language "experiment.noGroups") ] else groupRows)
            @ [ detectorSummary ])
    ] :> IView

/// The tab-specific extra controls (the non-command controls a tab also carries): the
/// catalogue on Build, the value-id action on the contextual Element tab, the Experiment-tab
/// group/detector controls, the language/theme/Legacy controls on Settings.
let private tabExtras (resource : Resource) (language : Language) (env : EnvironmentSettings) (cv : ConstructorView.Model) (d : Dispatch) (tab : RibbonTab) : IView list =
    match tab with
    | Build -> [ catalogueView resource language d ]
    | ElementTab -> [ bindValueButton resource language d ]
    | Experiment -> [ experimentView resource language cv d ]
    | Settings -> [ settingsView resource language env d ]
    | TraceView -> []

/// The expanded content for the active tab (D.1): its registry-command buttons followed
/// by its tab-specific extras, on one flexing row (D.4 — flexes, never truncates).
let private expandedContent (resource : Resource) (language : Language) (env : EnvironmentSettings) (cv : ConstructorView.Model) (d : Dispatch) (tab : RibbonTab) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 8.0
        StackPanel.children (tabCommandButtons resource language d tab @ tabExtras resource language env cv d tab)
    ] :> IView

/// One collapsed-menu column for a tab (D.2.1): the tab title over its command items and
/// extras. Reads the SAME `tabCommands`/`tabExtras` the expanded ribbon does, so the menu
/// exposes everything the ribbon offers — both are projections of the one command set.
let private menuColumn (resource : Resource) (language : Language) (env : EnvironmentSettings) (cv : ConstructorView.Model) (d : Dispatch) (tab : RibbonTab) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.children (
            labelBlock (tabTitle resource language tab)
            :: tabCommandButtons resource language d tab
            @ tabExtras resource language env cv d tab)
    ] :> IView

/// The collapsed drop-down menus (D.2.1): every visible tab listed with its commands —
/// the same projection the ribbon renders, so no command is reachable from only one.
let private collapsedContent (resource : Resource) (language : Language) (env : EnvironmentSettings) (cv : ConstructorView.Model) (d : Dispatch) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 12.0
        StackPanel.children [ for tab in visibleTabs cv -> menuColumn resource language env cv d tab ]
    ] :> IView

/// The tab-header row (D.1 / D.3): a selector toggle per visible tab (the contextual
/// Element tab appended on selection) plus the collapse/expand control (D.2).
let private tabHeaders (resource : Resource) (language : Language) (model : Model) (cv : ConstructorView.Model) (d : Dispatch) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 4.0
        StackPanel.children [
            for tab in visibleTabs cv do
                yield Controls.toggle (tabTitle resource language tab) (model.activeTab = tab) (fun _ -> d.onRibbon (SelectTab tab))
            yield Controls.defaultButton
                    (lookup resource language (if model.collapsed then "ribbon.expand" else "ribbon.collapse"))
                    (fun () -> d.onRibbon ToggleCollapsed)
        ]
    ] :> IView

/// The ribbon-and-menu shell (D.1 / D.2 / D.3 / D.4). The header row of tab selectors +
/// collapse control sits over either the active tab's expanded content or — when
/// collapsed — the full drop-down menus, both projected from the one command registry.
let view
    (resource : Resource)
    (language : Language)
    (model : Model)
    (cv : ConstructorView.Model)
    (env : EnvironmentSettings)
    (d : Dispatch)
    : IView =
    // The effective tab: fall back to Build if a contextual selection cleared the
    // Element tab out from under a stale `activeTab`.
    let activeTab = if List.contains model.activeTab (visibleTabs cv) then model.activeTab else Build
    let body =
        if model.collapsed then collapsedContent resource language env cv d
        else expandedContent resource language env cv d activeTab
    Border.create [
        Border.dock Dock.Top
        Border.borderThickness 1.0
        Border.padding 4.0
        Border.child (
            StackPanel.create [
                StackPanel.orientation Orientation.Vertical
                StackPanel.spacing 4.0
                StackPanel.children [ tabHeaders resource language model cv d; body ]
            ])
    ] :> IView

// ---------------------------------------------------------------------------
// Modal + help overlays (rendered by the shell over the constructor page). Both are
// fully working, dismissible overlays; the value-id modal carries no picker content.
// ---------------------------------------------------------------------------

/// A centred modal dialog over a dimming backdrop: a bold title, the body views, and a
/// Close button that dismisses it. A schematic-grade working modal (constraint 0.5).
let private modalOverlay (title : string) (content : IView list) (closeLabel : string) (onClose : unit -> unit) : IView =
    let dialog =
        Border.create [
            Border.horizontalAlignment HorizontalAlignment.Center
            Border.verticalAlignment VerticalAlignment.Center
            Border.background (Brushes.White :> IBrush)
            Border.borderBrush (Brushes.Gray :> IBrush)
            Border.borderThickness 1.0
            Border.padding 16.0
            Border.minWidth 320.0
            Border.child (
                StackPanel.create [
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.spacing 12.0
                    StackPanel.children (
                        (TextBlock.create [ TextBlock.text title; TextBlock.fontWeight FontWeight.Bold ] :> IView)
                        :: content
                        @ [ Controls.defaultButton closeLabel onClose ])
                ])
        ]
    Border.create [
        Border.background (SolidColorBrush(Color.FromArgb(120uy, 0uy, 0uy, 0uy)) :> IBrush)
        Border.child dialog
    ] :> IView

/// The value-id binding modal (F.2.1): a fully working, dismissible empty modal — a
/// title, an explicit "no picker yet" note, and a Close button. No picker content ships
/// (0.6 / F.2.2); the committed deliverable is the working action + the working modal.
let valueIdModal (resource : Resource) (language : Language) (onConstructor : ConstructorView.Msg -> unit) : IView =
    modalOverlay
        (lookup resource language "valueId.title")
        [ labelBlock (lookup resource language "valueId.placeholder") ]
        (lookup resource language "common.close")
        (fun () -> onConstructor ConstructorView.CloseValueIdModal)

/// The local-help overlay (E.2): the context-sensitive help for the current selection —
/// the active element's catalogue-role help, else the table help — resolved through
/// `LocalHelp`/`Localization`. Dismissed via the dedicated `CloseHelp` message, which only
/// clears the help flag — so closing help preserves the selection / contextual Element tab
/// (unlike `Invoke CancelOrDeselect`, which would deselect the active element).
let helpOverlay (resource : Resource) (language : Language) (cv : ConstructorView.Model) (onConstructor : ConstructorView.Msg -> unit) : IView =
    let ctx =
        match ConstructorView.activeElement cv with
        | Some p -> LH.ElementHelp p.catalogueKind
        | None -> LH.TableHelp
    modalOverlay
        (lookup resource language "help.title")
        [ labelBlock (LH.helpText resource language ctx) ]
        (lookup resource language "common.close")
        (fun () -> onConstructor ConstructorView.CloseHelp)

// ---------------------------------------------------------------------------
// Slice-007 front-door overlays (Part G/K). The element dialog (placeholder), the right-click
// element context menu, and the same-row Confirm/Cancel destructive gate — the three overlays
// slice 006 deferred. Each is rendered by the shell over the constructor page when its model flag
// is set; each dismisses through the constructor's own messages (the page stays single-sourced).
// ---------------------------------------------------------------------------

/// The element-properties dialog overlay (§E.2): a working, dismissible PLACEHOLDER modal opened
/// by `OpenElementDialog` (no property editor ships yet, 0.6). Dismissed via `CloseElementDialog`,
/// which clears the flag without deselecting.
let elementDialogOverlay (resource : Resource) (language : Language) (onConstructor : ConstructorView.Msg -> unit) : IView =
    modalOverlay
        (lookup resource language "elementDialog.title")
        [ labelBlock (lookup resource language "elementDialog.placeholder") ]
        (lookup resource language "common.close")
        (fun () -> onConstructor ConstructorView.CloseElementDialog)

/// The right-click element context menu overlay (§E.2): the registry commands flagged
/// `inContextMenu`, projected from the ONE registry, each dispatching its `Invoke` — a parameterized
/// command with no parameterless effect renders DISABLED (mirroring the ribbon's `commandButton`),
/// so it stays discoverable without a silent no-op. Dismissed via `CloseContextMenu`.
let contextMenuOverlay (resource : Resource) (language : Language) (onConstructor : ConstructorView.Msg -> unit) : IView =
    let item (cmd : Command) : IView =
        let label = LH.commandLabel resource language cmd
        if ConstructorView.isParameterlessInvokable cmd then
            Controls.defaultButton label (fun () -> onConstructor (ConstructorView.Invoke cmd))
        else Controls.disabledButton label
    modalOverlay
        (lookup resource language "contextMenu.title")
        (ConstructorView.contextMenuCommands |> List.map item)
        (lookup resource language "common.close")
        (fun () -> onConstructor ConstructorView.CloseContextMenu)

/// The same-row Confirm/Cancel destructive gate overlay (K.2 / UX 5): the pending action's
/// localized prompt over a `Controls.destructiveGate` (distinct positive/negative CTA colours, one
/// row, a visible gap). Confirm applies the action (`ConfirmPending`); Cancel clears it
/// (`CancelPending`). The shell renders this only when an action is pending.
let confirmGateOverlay (resource : Resource) (language : Language) (cv : ConstructorView.Model) (onConstructor : ConstructorView.Msg -> unit) : IView =
    let promptKey = ConstructorView.pendingPromptKey cv |> Option.defaultValue "common.confirm"
    let gate =
        Controls.destructiveGate
            (lookup resource language promptKey)
            (lookup resource language "common.confirm")
            (lookup resource language "common.cancel")
            (fun () -> onConstructor ConstructorView.ConfirmPending)
            (fun () -> onConstructor ConstructorView.CancelPending)
    let dialog =
        Border.create [
            Border.horizontalAlignment HorizontalAlignment.Center
            Border.verticalAlignment VerticalAlignment.Center
            Border.background (Brushes.White :> IBrush)
            Border.borderBrush (Brushes.Gray :> IBrush)
            Border.borderThickness 1.0
            Border.padding 16.0
            Border.child gate
        ]
    Border.create [
        Border.background (SolidColorBrush(Color.FromArgb(120uy, 0uy, 0uy, 0uy)) :> IBrush)
        Border.child dialog
    ] :> IView
