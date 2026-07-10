namespace OpticalConstructor.Controls

open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

/// The reusable SAMPLES control (Spec 0033, step 016 — UICOMP_XDUO_0002, the samples-workbench
/// list surface): a searchable samples list with a search box, a substrate-kind facet selector,
/// Add / Edit / Remove / View verbs over the selected entry, and the multilayer entry point (the
/// "make a multilayer" creation flow). This control is DOMAIN-FREE (matching `MaterialsControls` /
/// `LibraryControls`): the host runs the search seam (`SampleProxy.searchSamples` over a
/// `SampleQuery`) and flattens the result into `Row`s and the substrate facet into
/// `FacetOption`s; the control renders the rows it is given and never touches a proxy. Behaviour
/// is injected as a `Handlers` function record (the functional-proxy seam; tests pass stubs).
module SampleLibraryControls =

    /// One row the host pre-flattens from its samples search (Controls carries no domain types).
    /// `sampleId` is the sample's stable id in its string form.
    type Row =
        {
            sampleId : string
            label : string
        }

    /// One substrate facet option the host offers: a stable `code` the host maps back to its
    /// domain `SubstrateKind option` (the host includes its own "all" option → `None`), and the
    /// label shown on the option box.
    type FacetOption =
        {
            code : string
            label : string
        }

    /// The bay's pure, serializable state. The search text and the substrate selection mirror the
    /// domain `SampleQuery` seam (text + substrate) without importing it; the host re-runs its
    /// search and hands back new `rows` whenever they change.
    type State =
        {
            /// The live search text (the host filters; the control only echoes and dispatches).
            searchText : string
            /// The substrate-kind facet options, in display order (the host includes its "all" option).
            substrateOptions : FacetOption list
            /// The selected substrate option's `code`.
            selectedSubstrate : string
            /// The rows the host's search produced — rendered as given, never re-filtered here.
            rows : Row list
            /// The selected row's sample id, if any (the Edit / Remove / View verbs' target).
            selectedId : string option
        }

    let empty : State =
        {
            searchText = ""
            substrateOptions = []
            selectedSubstrate = ""
            rows = []
            selectedId = None
        }

    /// Behaviour injected by the host (the functional-proxy seam; tests pass stubs). The verbs
    /// take no argument — the host owns the selection (`selectedId`) the verb applies to.
    type Handlers =
        {
            /// The search text changed — the host re-runs its search seam.
            setSearchText : string -> unit
            /// Pick a substrate-kind facet option (by its `code`).
            selectSubstrate : string -> unit
            /// Select the row with this sample id (the verbs' target).
            selectSample : string -> unit
            /// The verbs: create a new sample; edit / remove / view the selected one.
            addSample : unit -> unit
            editSample : unit -> unit
            removeSample : unit -> unit
            viewSample : unit -> unit
            /// The multilayer entry point — start the make-a-multilayer creation flow
            /// (a second creation verb; like Add it needs no selection).
            makeMultilayer : unit -> unit
        }

    /// Stable intent-named automation ids (CLAUDE.md UI guidance).
    [<RequireQualifiedAccess>]
    module UiIds =
        [<Literal>]
        let searchBox = "SampleSearchBox"
        [<Literal>]
        let substrateFilter = "SampleSubstrateFilter"
        [<Literal>]
        let list = "SamplesList"
        [<Literal>]
        let addButton = "AddSampleButton"
        [<Literal>]
        let editButton = "EditSampleButton"
        [<Literal>]
        let removeButton = "RemoveSampleButton"
        [<Literal>]
        let viewButton = "ViewSampleButton"
        [<Literal>]
        let makeMultilayerButton = "MakeMultilayerButton"
        /// A listed row's clickable id — the sample id, prefixed so it cannot collide.
        let row (sampleId : string) : string = "SampleRow_" + sampleId
        /// A substrate facet option's clickable id, by its host-supplied code.
        let substrateOption (code : string) : string = "SampleSubstrateOption_" + code

    /// The row the current selection points at; `None` when nothing is selected or the host's
    /// filter no longer lists the selected id (the row-targeted verbs then disable).
    let selectedRow (state : State) : Row option =
        match state.selectedId with
        | Some id -> state.rows |> List.tryFind (fun r -> r.sampleId = id)
        | None -> None

    // -- The button look, identical to the other bars' idle/chosen boxes (so they MATCH). --
    let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private idleBackground = color 232 232 232
    let private chosenBackground = color 150 185 235
    let private idleBorder = color 120 120 120

    /// Set `AutomationProperties.AutomationId` (a freely-mutable attached property — unlike
    /// `Control.Name`) through FuncUI's attr builder. The rows, the facet options, and the verb
    /// buttons all live in child lists whose MEMBERSHIP can change (the host's filter rewrites the
    /// rows), so a sibling can shift onto a reused control's slot — Avalonia forbids renaming a
    /// styled control, and an AutomationId survives that reuse (the `MaterialsControls` precedent).
    let private automationId (autoId : string) : IAttr<Border> =
        AttrBuilder<Border>.CreateProperty<string>(AutomationProperties.AutomationIdProperty, autoId, ValueNone)

    /// A clickable, styled box (a facet option or a listed row), highlighted when chosen, KEYED by its
    /// id (`View.withKey`, spec 0038) so a membership change recreates a shifted box instead of
    /// patching another item's styled control in place.
    /// `e.Handled <- true` drops FuncUI's duplicate Tunnel|Bubble pass; re-subscribe when the id or
    /// the highlight changes so a reused box can't keep a stale handler.
    let private clickBox (autoId : string) (label : string) (chosen : bool) (onClick : unit -> unit) : IView =
        let keyedBox =
            Border.create [
                automationId autoId
                Border.background (brush (if chosen then chosenBackground else idleBackground))
                Border.borderBrush (brush idleBorder)
                Border.borderThickness 1.0
                Border.cornerRadius (CornerRadius 3.0)
                Border.padding (Thickness(10.0, 4.0))
                Border.margin (Thickness(0.0, 0.0, 6.0, 4.0))
                Border.verticalAlignment VerticalAlignment.Center
                Border.child (TextBlock.create [ TextBlock.text label ])
                Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (autoId, chosen)))
            ]
            // Fully qualified: `Avalonia.FuncUI.Types` (opened above for `IView`) also exports a
            // `View<'t>` type, so the bare `View` name would be ambiguous with the DSL `View` module.
            |> Avalonia.FuncUI.DSL.View.withKey autoId
        keyedBox :> IView

    /// A verb button (Add / Edit / Remove / View / Make multilayer), accented when it is the
    /// primary verb, keyed by its id (matching `clickBox` — spec 0038).
    let private verbButton (autoId : string) (label : string) (accent : bool) (enabled : bool) (onClick : unit -> unit) : IView =
        let keyedButton =
            Border.create [
                automationId autoId
                Border.isEnabled enabled
                Border.opacity (if enabled then 1.0 else 0.4)
                Border.background (brush (if accent then chosenBackground else idleBackground))
                Border.borderBrush (brush idleBorder)
                Border.borderThickness 1.0
                Border.cornerRadius (CornerRadius 3.0)
                Border.padding (Thickness(12.0, 5.0))
                Border.margin (Thickness(0.0, 0.0, 8.0, 0.0))
                Border.verticalAlignment VerticalAlignment.Center
                Border.child (TextBlock.create [ TextBlock.text label ])
                Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (autoId, enabled)))
            ]
            |> Avalonia.FuncUI.DSL.View.withKey autoId
        keyedButton :> IView

    /// The search row: a label and the live search box (the host filters on every text change —
    /// the same idiom as the materials workbench's search field).
    let private searchRow (state : State) (handlers : Handlers) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 6.0
            StackPanel.children [
                TextBlock.create [ TextBlock.text "Search:"; TextBlock.verticalAlignment VerticalAlignment.Center ]
                TextBox.create [
                    TextBox.name UiIds.searchBox
                    TextBox.width 220.0
                    TextBox.text state.searchText
                    TextBox.onTextChanged handlers.setSearchText
                ]
            ]
        ] :> IView

    /// The substrate facet selector: a titled, NAMED WrapPanel (the UiIds container id) of
    /// clickable option boxes, the selected code highlighted.
    let private substrateRow (state : State) (handlers : Handlers) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 6.0
            StackPanel.children [
                TextBlock.create [ TextBlock.text "Substrate:"; TextBlock.verticalAlignment VerticalAlignment.Center ]
                WrapPanel.create [
                    WrapPanel.name UiIds.substrateFilter
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.children (
                        state.substrateOptions
                        |> List.map (fun o ->
                            clickBox (UiIds.substrateOption o.code) o.label (state.selectedSubstrate = o.code) (fun () -> handlers.selectSubstrate o.code)))
                ]
            ]
        ] :> IView

    /// One listed sample row — clickable (selects it), highlighted when it is the selection.
    let private rowView (state : State) (handlers : Handlers) (r : Row) : IView =
        clickBox (UiIds.row r.sampleId) r.label (state.selectedId = Some r.sampleId) (fun () -> handlers.selectSample r.sampleId)

    /// The samples list: a NAMED vertical stack of the host's rows, rendered as given.
    let private listView (state : State) (handlers : Handlers) : IView =
        ScrollViewer.create [
            ScrollViewer.maxHeight 220.0
            ScrollViewer.content (
                StackPanel.create [
                    StackPanel.name UiIds.list
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.children (state.rows |> List.map (rowView state handlers))
                ])
        ] :> IView

    /// The verb row. Add and Make-multilayer are creation entry points and always apply; Edit /
    /// Remove / View act on the selection and disable while nothing (listed) is selected.
    let private verbRow (state : State) (handlers : Handlers) : IView =
        let hasSelection =
            match selectedRow state with
            | Some _ -> true
            | None -> false
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 0.0
            StackPanel.children [
                verbButton UiIds.addButton "Add" true true (fun () -> handlers.addSample ())
                verbButton UiIds.editButton "Edit" false hasSelection (fun () -> handlers.editSample ())
                verbButton UiIds.removeButton "Remove" false hasSelection (fun () -> handlers.removeSample ())
                verbButton UiIds.viewButton "View" false hasSelection (fun () -> handlers.viewSample ())
                verbButton UiIds.makeMultilayerButton "Make multilayer" false true (fun () -> handlers.makeMultilayer ())
            ]
        ] :> IView

    /// The samples workbench surface — search box, the substrate facet selector, the samples
    /// list, and the Add / Edit / Remove / View / Make-multilayer verbs, top to bottom.
    let view (state : State) (handlers : Handlers) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 4.0
            StackPanel.children [
                searchRow state handlers
                substrateRow state handlers
                listView state handlers
                verbRow state handlers
            ]
        ] :> IView
