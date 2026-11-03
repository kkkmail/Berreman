namespace OpticalConstructor.Controls

open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
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
    /// `sampleId` is the sample's stable id, as the host keys it.
    type SampleLibraryControlsRow =
        {
            sampleId : ConstructorUiKey
            label : ControlCaption
        }

    /// The bay's pure, serializable state. The search text and the substrate selection mirror the
    /// domain `SampleQuery` seam (text + substrate) without importing it; the host re-runs its
    /// search and hands back new `rows` whenever they change.
    type SampleLibraryControlsState =
        {
            /// The live search text (the host filters; the control only echoes and dispatches).
            searchText : ControlCaption
            /// The substrate-kind facet options, in display order: each `code` maps back to the host's
            /// domain `SubstrateKind option` (its own "all" option → `None`).
            substrateOptions : FacetOption list
            /// The selected substrate option's `code`, if any.
            selectedSubstrate : ConstructorUiKey option
            /// The rows the host's search produced — rendered as given, never re-filtered here.
            rows : SampleLibraryControlsRow list
            /// The selected row's sample id, if any (the Edit / Remove / View verbs' target).
            selectedId : ConstructorUiKey option
        }

        /// The row the current selection points at; `None` when nothing is selected or the host's
        /// filter no longer lists the selected id (the row-targeted verbs then disable).
        member this.selectedRow : SampleLibraryControlsRow option =
            match this.selectedId with
            | Some id -> this.rows |> List.tryFind (fun r -> r.sampleId = id)
            | None -> None

    let emptySampleLibraryControls : SampleLibraryControlsState =
        {
            searchText = ControlCaption ""
            substrateOptions = []
            selectedSubstrate = None
            rows = []
            selectedId = None
        }

    /// Behaviour injected by the host (the functional-proxy seam; tests pass stubs). The verbs
    /// take no argument — the host owns the selection (`selectedId`) the verb applies to.
    type SampleLibraryControlsHandlers =
        {
            /// The search text changed — the host re-runs its search seam.
            setSearchText : ControlCaption -> unit
            /// Pick a substrate-kind facet option (by its `code`).
            selectSubstrate : ConstructorUiKey -> unit
            /// Select the row with this sample id (the verbs' target).
            selectSample : ConstructorUiKey -> unit
            /// The verbs: create a new sample; edit / remove / view the selected one.
            addSample : unit -> unit
            editSample : unit -> unit
            removeSample : unit -> unit
            viewSample : unit -> unit
            /// The multilayer entry point — start the make-a-multilayer creation flow
            /// (a second creation verb; like Add it needs no selection).
            makeMultilayer : unit -> unit
        }

    // -- The button look, identical to the other bars' idle/chosen boxes (so they MATCH). --
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private idleBackground = Color.FromRgb(232uy, 232uy, 232uy)
    let private chosenBackground = Color.FromRgb(150uy, 185uy, 235uy)
    let private idleBorder = Color.FromRgb(120uy, 120uy, 120uy)

    /// The chosen look for the box keyed `key` while it is the selection, the idle look otherwise.
    let private boxBackground (selected : ConstructorUiKey option) (key : ConstructorUiKey) : Color =
        if selected = Some key then chosenBackground else idleBackground

    /// A clickable, styled box (a facet option or a listed row). Its id is an AutomationId, not a
    /// `Name`: the host's filter rewrites the rows, so FuncUI can shift a sibling onto a reused box, and
    /// Avalonia forbids renaming a styled control (the `MaterialsControls` precedent). KEYED by its id
    /// (`View.withKey`, spec 0038) so a membership change recreates a shifted box instead of patching
    /// another item's in place.
    /// `e.Handled <- true` drops FuncUI's duplicate Tunnel|Bubble pass; re-subscribe when the id or
    /// the highlight changes so a reused box can't keep a stale handler.
    let private clickBox (id : ConstructorUiId) (label : ControlCaption) (background : Color) (onClick : unit -> unit) : IView =
        let keyedBox =
            Border.create [
                id.automationAttribute<Border> ()
                Border.background (brush background)
                Border.borderBrush (brush idleBorder)
                Border.borderThickness 1.0
                Border.cornerRadius (CornerRadius 3.0)
                Border.padding (Thickness(10.0, 4.0))
                Border.margin (Thickness(0.0, 0.0, 6.0, 4.0))
                Border.verticalAlignment VerticalAlignment.Center
                Border.child (TextBlock.create [ TextBlock.text label.value ])
                Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (id.value, background)))
            ]
            // Fully qualified: `Avalonia.FuncUI.Types` (opened above for `IView`) also exports a
            // `View<'t>` type, so the bare `View` name would be ambiguous with the DSL `View` module.
            |> Avalonia.FuncUI.DSL.View.withKey id.value
        keyedBox :> IView

    /// A verb button (Add / Edit / Remove / View / Make multilayer), on the accent background when it
    /// is the primary verb, keyed by its id (matching `clickBox` — spec 0038).
    let private verbButton (id : ConstructorUiId) (label : ControlCaption) (background : Color) (availability : ControlAvailability) (onClick : unit -> unit) : IView =
        let keyedButton =
            Border.create [
                id.automationAttribute<Border> ()
                Border.isEnabled availability.isEnabled
                Border.opacity (match availability with ControlOffered -> 1.0 | ControlWithheld -> 0.4)
                Border.background (brush background)
                Border.borderBrush (brush idleBorder)
                Border.borderThickness 1.0
                Border.cornerRadius (CornerRadius 3.0)
                Border.padding (Thickness(12.0, 5.0))
                Border.margin (Thickness(0.0, 0.0, 8.0, 0.0))
                Border.verticalAlignment VerticalAlignment.Center
                Border.child (TextBlock.create [ TextBlock.text label.value ])
                Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (id.value, availability.isEnabled)))
            ]
            |> Avalonia.FuncUI.DSL.View.withKey id.value
        keyedButton :> IView

    /// The search row: a label and the live search box (the host filters on every text change —
    /// the same idiom as the materials workbench's search field).
    let private searchRow (state : SampleLibraryControlsState) (handlers : SampleLibraryControlsHandlers) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 6.0
            StackPanel.children [
                TextBlock.create [ TextBlock.text "Search:"; TextBlock.verticalAlignment VerticalAlignment.Center ]
                TextBox.create [
                    TextBox.name ConstructorUiId.sampleSearchBox.value
                    TextBox.width 220.0
                    TextBox.text state.searchText.value
                    TextBox.onTextChanged (ControlCaption >> handlers.setSearchText)
                ]
            ]
        ] :> IView

    /// The substrate facet selector: a titled, NAMED WrapPanel (the container id) of clickable option
    /// boxes, the selected code highlighted.
    let private substrateRow (state : SampleLibraryControlsState) (handlers : SampleLibraryControlsHandlers) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 6.0
            StackPanel.children [
                TextBlock.create [ TextBlock.text "Substrate:"; TextBlock.verticalAlignment VerticalAlignment.Center ]
                WrapPanel.create [
                    WrapPanel.name ConstructorUiId.sampleSubstrateFilter.value
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.children (
                        state.substrateOptions
                        |> List.map (fun o ->
                            clickBox (ConstructorUiId.sampleSubstrateOption o.code) o.label (boxBackground state.selectedSubstrate o.code) (fun () -> handlers.selectSubstrate o.code)))
                ]
            ]
        ] :> IView

    /// One listed sample row — clickable (selects it), highlighted when it is the selection.
    let private rowView (state : SampleLibraryControlsState) (handlers : SampleLibraryControlsHandlers) (r : SampleLibraryControlsRow) : IView =
        clickBox (ConstructorUiId.sampleRow r.sampleId) r.label (boxBackground state.selectedId r.sampleId) (fun () -> handlers.selectSample r.sampleId)

    /// The samples list: a NAMED vertical stack of the host's rows, rendered as given.
    let private listView (state : SampleLibraryControlsState) (handlers : SampleLibraryControlsHandlers) : IView =
        ScrollViewer.create [
            ScrollViewer.maxHeight 220.0
            ScrollViewer.content (
                StackPanel.create [
                    StackPanel.name ConstructorUiId.samplesList.value
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.children (state.rows |> List.map (rowView state handlers))
                ])
        ] :> IView

    /// The verb row. Add and Make-multilayer are creation entry points and always apply; Edit /
    /// Remove / View act on the selection and disable while nothing (listed) is selected.
    let private verbRow (state : SampleLibraryControlsState) (handlers : SampleLibraryControlsHandlers) : IView =
        let onSelection =
            match state.selectedRow with
            | Some _ -> ControlOffered
            | None -> ControlWithheld
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 0.0
            StackPanel.children [
                verbButton ConstructorUiId.addSampleButton (ControlCaption "Add") chosenBackground ControlOffered handlers.addSample
                verbButton ConstructorUiId.editSampleButton (ControlCaption "Edit") idleBackground onSelection handlers.editSample
                verbButton ConstructorUiId.removeSampleButton (ControlCaption "Remove") idleBackground onSelection handlers.removeSample
                verbButton ConstructorUiId.viewSampleButton (ControlCaption "View") idleBackground onSelection handlers.viewSample
                verbButton ConstructorUiId.makeMultilayerButton (ControlCaption "Make multilayer") idleBackground ControlOffered handlers.makeMultilayer
            ]
        ] :> IView

    /// The samples workbench surface — search box, the substrate facet selector, the samples
    /// list, and the Add / Edit / Remove / View / Make-multilayer verbs, top to bottom.
    let sampleLibraryControlsView (state : SampleLibraryControlsState) (handlers : SampleLibraryControlsHandlers) : IView =
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
