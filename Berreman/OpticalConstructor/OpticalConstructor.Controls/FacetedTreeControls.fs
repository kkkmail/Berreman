namespace OpticalConstructor.Controls

open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

/// The domain-free faceted, filterable, reconfigurable tree control (spec 0038 Part E, step 012 —
/// UICOMP_XDUO_0008): the ONE rendering surface both the Materials and the Library windows
/// instantiate over the Part D facet engine. This control is DOMAIN-FREE (the `MaterialsControls`
/// State+Handlers shape): the HOST runs the engine (`Facets.buildTree` / `breadcrumbCounts` /
/// `countFor` / `FacetBuckets.bucketsFor`) and projects the results into the control-local `State`
/// below — the control renders exactly what it is given and never touches a proxy or the facet
/// engine. Behaviour is injected as a `Handlers` function record (the functional-proxy seam; tests
/// pass stubs). Every code in `State` is a host-supplied stable token the host maps back to its own
/// domain value when a handler dispatches; the control never interprets one.
///
/// Rendering contract (spec 0038 §D.0 / §E.0): a breadcrumb strip of removable chips in application
/// order, each showing its after-count; a live result count; count-previews on every offered value;
/// a representation picker (search order ≠ representation order — the HOST keeps the tree stable
/// under constraint changes); tree materialization gated behind an explicit Show/Search button
/// whenever `State` says so (the host compares its live result count against
/// `TreeAutoBuildThreshold` — the threshold lives in Domain settings, not here); branch rows read
/// `label (count)` with unexpanded branches still carrying their counts; everything scrolls (the
/// tree may span pages). The text filter commits on Enter/LostFocus ONLY — never per keystroke
/// (§0.4: filter boxes over tree-rebuilding state).
module FacetedTreeControls =

    /// Whether the tree body is materialized (node rows render) or gated behind the explicit
    /// Show/Search button (§0.7: the one potentially heavy render). The HOST decides — result
    /// count above its `TreeAutoBuildThreshold` — and the control only obeys: in `TreeGated` mode
    /// it renders the button INSTEAD of tree rows and generates ZERO node rows, whatever `tree`
    /// holds.
    type TreeMaterialization =
        | TreeMaterialized
        | TreeGated

    /// Whether a tree node's children render below it (expanded) or the node renders as a single
    /// row (collapsed). A collapsed branch still SHOWS its count — the row text is the same
    /// `label (count)` either way.
    type NodeExpansion =
        | ExpandedNode
        | CollapsedNode

    /// Whether an offer group additionally takes a manual min–max range entry (numeric facets —
    /// spec 0038 §D.0: a manual min–max applies as an ordinary constraint chip) or offers
    /// clickable values only. The range box hands its RAW text to the host, which parses it.
    type ManualRange =
        | ManualRangeOffered
        | NoManualRange

    /// A count the host's facet engine computed: the live result count, a chip's after-count, an
    /// offered value's preview, or a branch's total.
    type FacetItemCount =
        | FacetItemCount of int

        member this.value = let (FacetItemCount c) = this in c

    /// How deep a tree row sits (0 = a top-level node); it drives the row's indent.
    type FacetTreeDepth =
        | FacetTreeDepth of int

        member this.value = let (FacetTreeDepth d) = this in d

        /// The depth of this row's children.
        member this.deeper : FacetTreeDepth = FacetTreeDepth (this.value + 1)

        /// The row's left indent, in device-independent pixels.
        member this.indent : float = float this.value * 16.0

    /// One applied-constraint breadcrumb chip, in application order: a stable `code` the host maps
    /// back to its applied constraint, the display label, and the cumulative result count after
    /// applying this chip and everything before it (the engine's `breadcrumbCounts` shape).
    /// Clicking the chip removes it.
    type BreadcrumbChip =
        {
            code : ConstructorUiKey
            label : ControlCaption
            afterCount : FacetItemCount
        }

    /// One offered value under a facet: a stable `code`, the display label, and the count-preview
    /// (the result count IF this value were applied on top of everything already applied — the
    /// engine's `countFor` shape). Clicking it applies the constraint.
    type OfferedValue =
        {
            code : ConstructorUiKey
            label : ControlCaption
            previewCount : FacetItemCount
        }

    /// One facet's offer group: a stable `code`, the facet title, its offered values in display
    /// order (numeric facets offer their buckets here, labels bucket-shaped: `10-20 nm (7)`), and
    /// whether a manual min–max range entry is offered alongside them.
    type OfferGroup =
        {
            code : ConstructorUiKey
            title : ControlCaption
            values : OfferedValue list
            manualRange : ManualRange
        }


    /// One node of the built tree the host projects (facet heading, value branch, or item leaf —
    /// the control does not distinguish). `code` is a host-supplied stable token, UNIQUE across
    /// the whole tree (path-shaped codes work); it keys the row and returns through `selectNode`.
    /// `countOpt` carries the branch's item count where the host has one — a branch row reads
    /// `label (count)`, a row without a count (a heading or a leaf) reads its bare label.
    /// Collapsed nodes render no children but still show their counts.
    type TreeNode =
        {
            code : ConstructorUiKey
            label : ControlCaption
            countOpt : FacetItemCount option
            expansion : NodeExpansion
            children : TreeNode list
        }

        /// The row's text: `label (count)` where the node carries a count (a branch — shown
        /// expanded OR collapsed), the bare label where it does not (a heading or a leaf).
        member this.rowText : ControlCaption =
            match this.countOpt with
            | Some count -> ControlCaption $"%s{this.label.value} (%d{count.value})"
            | None -> this.label

    /// The control's pure, serializable state — everything the render needs, projected in by the
    /// host on every change (the host owns the engine; the control owns nothing).
    type FacetedTreeControlsState =
        {
            /// The built tree's top-level nodes (empty until the host materializes one).
            tree : TreeNode list
            /// The applied-constraint chips, in application (breadcrumb) order.
            breadcrumbs : BreadcrumbChip list
            /// The offer groups, facets in display order, every value carrying its count-preview.
            offers : OfferGroup list
            /// The named representations (tree-shaping facet orders) the picker offers, in display order.
            representations : FacetOption list
            /// The active representation's `code` (`None` before the host projects one).
            activeRepresentationOpt : ConstructorUiKey option
            /// The text-filter DRAFT the box shows (the last committed query, until the user
            /// types); typing never dispatches — only Enter/LostFocus commit.
            filterDraft : ControlCaption
            /// The live result count under everything applied.
            resultCount : FacetItemCount
            /// Whether the tree body renders or the Show/Search button gates it.
            materialization : TreeMaterialization
            /// The `code` of the currently SELECTED tree node — the row painted visibly distinct
            /// (spec 0040 step 003); `None` = nothing selected. The HOST tracks the selection in its
            /// Model and projects the code here; the control only reads it to highlight the matching row.
            selectedCodeOpt : ConstructorUiKey option
        }

    let emptyFacetedTreeControls : FacetedTreeControlsState =
        {
            tree = []
            breadcrumbs = []
            offers = []
            representations = []
            activeRepresentationOpt = None
            filterDraft = ControlCaption ""
            resultCount = FacetItemCount 0
            materialization = TreeMaterialized
            selectedCodeOpt = None
        }

    /// Behaviour injected by the host (the functional-proxy seam; tests pass stubs). Every
    /// argument is a `State`-supplied stable token (or, for the text boxes, the raw box text) —
    /// the host maps tokens back to its domain values and re-projects a new `State`.
    type FacetedTreeControlsHandlers =
        {
            /// Apply the offered value as a constraint: the offer group's `code`, the value's `code`.
            applyConstraint : ConstructorUiKey -> ConstructorUiKey -> unit
            /// Remove the applied-constraint chip with this `code`.
            removeConstraint : ConstructorUiKey -> unit
            /// Commit the filter box's text (dispatched on Enter/LostFocus ONLY, never per keystroke).
            commitTextFilter : ControlCaption -> unit
            /// Choose the named representation with this `code`.
            chooseRepresentation : ConstructorUiKey -> unit
            /// Materialize the gated tree (the Show/Search button).
            requestBuild : unit -> unit
            /// Select the tree node with this `code`.
            selectNode : ConstructorUiKey -> unit
            /// Toggle the tree node with this `code` between expanded and collapsed — a
            /// disclosure-chevron press (spec 0040 step 002). Only a PARENT node (one with
            /// children) renders a chevron, so only a parent's code is ever dispatched here; the
            /// HOST holds the expanded-code set and re-projects each node's `expansion`.
            toggleNode : ConstructorUiKey -> unit
            /// Apply a manual min–max range on the offer group with this `code`; the second
            /// argument is the range box's RAW text (e.g. "10-20") — the HOST parses it and, on
            /// success, applies an ordinary constraint chip.
            applyManualRange : ConstructorUiKey -> ControlCaption -> unit
        }

    // -- The button look, identical to the other bars' idle/chosen boxes (so they MATCH). --
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private idleBackground = Color.FromRgb(232uy, 232uy, 232uy)
    let private chosenBackground = Color.FromRgb(150uy, 185uy, 235uy)
    let private idleBorder = Color.FromRgb(120uy, 120uy, 120uy)
    // A SELECTED tree row (spec 0040 step 003) reads with the `chosenBackground` fill AND a
    // thicker border — a non-hue, colourblind-safe cue that survives when the hue is imperceptible,
    // so the selection never rests on colour alone.
    let private idleBorderThickness = Thickness 1.0
    let private selectedBorderThickness = Thickness 3.0

    /// The chosen fill for the box keyed `key` when it is the `chosen` one, the idle fill otherwise.
    let private boxBackground (chosen : ConstructorUiKey option) (key : ConstructorUiKey) : Color =
        if chosen = Some key then chosenBackground else idleBackground

    // Every id below is an AutomationId, never `StyledElement.Name`: every list here has variable
    // membership (the host rewrites chips, offers, and rows on each constraint change, and the tree
    // body swaps with the Show/Search button), so a sibling can shift onto a reused control's slot —
    // Avalonia forbids renaming a styled control, and an AutomationId survives that reuse (the
    // `MaterialsControls` / `ExperimentControls` precedent, spec 0038 Part A).

    /// A clickable, styled box (a chip, an offered value, a representation option, or the
    /// Show/Search button) on its `background`, KEYED by its id (`View.withKey`) so a
    /// membership change recreates a shifted box instead of patching another item's styled control
    /// in place. `e.Handled <- true` drops FuncUI's duplicate Tunnel|Bubble pass; re-subscribe
    /// when the id or the highlight changes so a reused box can't keep a stale handler.
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

    /// Dispatch `commit` with the event source's CURRENT text. The boxes below subscribe this on
    /// Enter and LostFocus ONLY — no text-change handler exists anywhere in this control, so
    /// typing dispatches nothing and the host's (tree-rebuilding) state stays untouched until an
    /// explicit commit (spec 0038 §0.4; the `RotationControls` / `ExperimentControls` idiom).
    let private commitFrom (commit : ControlCaption -> unit) (src : obj) : unit =
        match src with
        | :? TextBox as tb when not (isNull tb.Text) -> commit (ControlCaption tb.Text)
        | _ -> ()

    /// The Enter-key commit: `e.Handled <- true` BEFORE dispatching drops FuncUI's duplicate
    /// Tunnel|Bubble pass (the same discipline `clickBox` applies to pointer events) — without it
    /// one Enter press commits twice.
    let private commitOnEnter (commit : ControlCaption -> unit) (e : KeyEventArgs) : unit =
        if e.Key = Key.Enter then
            e.Handled <- true
            commitFrom commit e.Source

    /// The filter row: a label and the text-filter box. The box shows the host's committed draft
    /// and commits on Enter / blur only.
    let private filterRow (state : FacetedTreeControlsState) (handlers : FacetedTreeControlsHandlers) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 6.0
            StackPanel.children [
                TextBlock.create [ TextBlock.text "Filter:"; TextBlock.verticalAlignment VerticalAlignment.Center ]
                TextBox.create [
                    ConstructorUiId.facetFilterBox.automationAttribute<TextBox> ()
                    TextBox.width 220.0
                    TextBox.text state.filterDraft.value
                    TextBox.onKeyDown (commitOnEnter handlers.commitTextFilter)
                    TextBox.onLostFocus (fun e -> commitFrom handlers.commitTextFilter e.Source)
                ]
            ]
        ] :> IView

    /// The representation picker: a titled WrapPanel of clickable representation options, the
    /// active one highlighted.
    /// Spec 0046: a DockPanel, not a horizontal StackPanel — a horizontal stack hands its children an
    /// unbounded width, so the WrapPanel never wrapped and its options ran off the window's right edge.
    let private representationRow (state : FacetedTreeControlsState) (handlers : FacetedTreeControlsHandlers) : IView =
        DockPanel.create [
            DockPanel.children [
                TextBlock.create [ TextBlock.dock Dock.Left; TextBlock.margin (Thickness(0.0, 0.0, 6.0, 0.0)); TextBlock.text "View by:"; TextBlock.verticalAlignment VerticalAlignment.Center ]
                WrapPanel.create [
                    ConstructorUiId.facetRepresentationPicker.automationAttribute<WrapPanel> ()
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.children (
                        state.representations
                        |> List.map (fun r ->
                            clickBox
                                (ConstructorUiId.facetRepresentationOption r.code)
                                r.label
                                (boxBackground state.activeRepresentationOpt r.code)
                                (fun () -> handlers.chooseRepresentation r.code)))
                ]
            ]
        ] :> IView

    /// The breadcrumb strip: the applied constraints in application order, each a removable chip
    /// reading `label (afterCount) ×` — clicking a chip REMOVES its constraint. Chips render
    /// highlighted (they are applied state, not offers).
    let private breadcrumbStripView (state : FacetedTreeControlsState) (handlers : FacetedTreeControlsHandlers) : IView =
        WrapPanel.create [
            ConstructorUiId.facetBreadcrumbStrip.automationAttribute<WrapPanel> ()
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.children (
                state.breadcrumbs
                |> List.map (fun chip ->
                    clickBox
                        (ConstructorUiId.facetBreadcrumbChip chip.code)
                        (ControlCaption $"%s{chip.label.value} (%d{chip.afterCount.value}) ×")
                        chosenBackground
                        (fun () -> handlers.removeConstraint chip.code)))
        ] :> IView

    /// One offer group: the facet title, its offered values as clickable count-previewed boxes,
    /// and — for numeric groups — the manual min–max box, committing its RAW text on Enter / blur
    /// only (the host parses; an unparseable or empty commit is the host's no-op).
    let private offerGroupView (handlers : FacetedTreeControlsHandlers) (group : OfferGroup) : IView =
        let rangeBox : IView list =
            match group.manualRange with
            | NoManualRange -> []
            | ManualRangeOffered ->
                [
                    (TextBox.create [
                        (ConstructorUiId.facetManualRangeBox group.code).automationAttribute<TextBox> ()
                        TextBox.width 110.0
                        TextBox.onKeyDown (commitOnEnter (handlers.applyManualRange group.code))
                        TextBox.onLostFocus (fun e -> commitFrom (handlers.applyManualRange group.code) e.Source)
                     ]
                     |> Avalonia.FuncUI.DSL.View.withKey (ConstructorUiId.facetManualRangeBox group.code).value) :> IView
                ]
        // Spec 0046: a DockPanel (the title docked left, the values filling the rest) so the values WRAP at
        // the window's width instead of running off its right edge.
        let keyedGroup =
            DockPanel.create [
                (ConstructorUiId.facetOfferGroup group.code).automationAttribute<DockPanel> ()
                DockPanel.children [
                    TextBlock.create [ TextBlock.dock Dock.Left; TextBlock.margin (Thickness(0.0, 0.0, 6.0, 0.0)); TextBlock.text (group.title.value + ":"); TextBlock.verticalAlignment VerticalAlignment.Center ]
                    WrapPanel.create [
                        WrapPanel.orientation Orientation.Horizontal
                        WrapPanel.children (
                            (group.values
                             |> List.map (fun v ->
                                 clickBox
                                     (ConstructorUiId.facetOfferedValue group.code v.code)
                                     (ControlCaption $"%s{v.label.value} (%d{v.previewCount.value})")
                                     idleBackground
                                     (fun () -> handlers.applyConstraint group.code v.code)))
                            @ rangeBox)
                    ]
                ]
            ]
            |> Avalonia.FuncUI.DSL.View.withKey (ConstructorUiId.facetOfferGroup group.code).value
        keyedGroup :> IView

    /// The offers panel: one row per offer group, facets in display order.
    let private offersPanelView (state : FacetedTreeControlsState) (handlers : FacetedTreeControlsHandlers) : IView =
        StackPanel.create [
            ConstructorUiId.facetOffersPanel.automationAttribute<StackPanel> ()
            StackPanel.orientation Orientation.Vertical
            StackPanel.children (state.offers |> List.map (offerGroupView handlers))
        ] :> IView


    /// One node's rows: a horizontal row of its disclosure chevron (a PARENT only — a leaf with
    /// empty `children` shows none) and its clickable, depth-indented label, then — only when the
    /// node is expanded — its children's rows below it. The chevron press toggles the node; the
    /// label press selects it. The flattened rows are siblings in ONE vertical stack, so every row
    /// (and the chevron and label within it) is keyed by its (tree-unique) node code.
    ///
    /// The row whose code matches `selectedCodeOpt` (the host's tracked selection, spec 0040 step 003)
    /// paints its label with `chosenBackground` AND a thicker border (a non-hue, colourblind-safe
    /// cue) so exactly that row reads distinct while every other stays idle. Re-subscribe the
    /// pointer handler when the code OR the selection flag changes so a reused, restyled box cannot
    /// keep a stale handler (the `clickBox` discipline).
    let rec private nodeRows (handlers : FacetedTreeControlsHandlers) (selectedCodeOpt : ConstructorUiKey option) (depth : FacetTreeDepth) (node : TreeNode) : IView list =
        let isSelected = selectedCodeOpt = Some node.code
        let label =
            Border.create [
                (ConstructorUiId.facetTreeNode node.code).automationAttribute<Border> ()
                Border.background (brush (boxBackground selectedCodeOpt node.code))
                Border.borderBrush (brush idleBorder)
                Border.borderThickness (if isSelected then selectedBorderThickness else idleBorderThickness)
                Border.cornerRadius (CornerRadius 3.0)
                Border.padding (Thickness(10.0, 3.0))
                Border.verticalAlignment VerticalAlignment.Center
                Border.child (TextBlock.create [ TextBlock.text node.rowText.value ])
                Border.onPointerPressed ((fun e -> e.Handled <- true; handlers.selectNode node.code), SubPatchOptions.OnChangeOf (box (node.code.value, isSelected)))
            ]
            |> Avalonia.FuncUI.DSL.View.withKey (ConstructorUiId.facetTreeNode node.code).value
            :> IView
        // A per-parent disclosure chevron (▾ expanded, ▸ collapsed): a node with no children shows
        // none. `e.Handled <- true` drops FuncUI's duplicate Tunnel|Bubble pass; re-subscribe when
        // the code or the glyph changes so a reused box can't keep a stale handler (the `clickBox`
        // discipline).
        let chevron : IView list =
            match node.children with
            | [] -> []
            | _ :: _ ->
                let glyph =
                    match node.expansion with
                    | ExpandedNode -> "▾"
                    | CollapsedNode -> "▸"
                let chevronId = ConstructorUiId.facetTreeChevron node.code
                [ Border.create [
                      chevronId.automationAttribute<Border> ()
                      Border.background (brush idleBackground)
                      Border.borderBrush (brush idleBorder)
                      Border.borderThickness 1.0
                      Border.cornerRadius (CornerRadius 3.0)
                      Border.padding (Thickness(6.0, 3.0))
                      Border.margin (Thickness(0.0, 0.0, 4.0, 0.0))
                      Border.verticalAlignment VerticalAlignment.Center
                      Border.child (TextBlock.create [ TextBlock.text glyph ])
                      Border.onPointerPressed ((fun e -> e.Handled <- true; handlers.toggleNode node.code), SubPatchOptions.OnChangeOf (box (node.code.value, glyph)))
                  ]
                  |> Avalonia.FuncUI.DSL.View.withKey chevronId.value
                  :> IView ]
        let row =
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.margin (Thickness(depth.indent, 0.0, 0.0, 2.0))
                StackPanel.children (chevron @ [ label ])
            ]
            |> Avalonia.FuncUI.DSL.View.withKey ("FacetTreeRow_" + node.code.value)
            :> IView
        match node.expansion with
        | ExpandedNode -> row :: (node.children |> List.collect (nodeRows handlers selectedCodeOpt depth.deeper))
        | CollapsedNode -> [ row ]

    /// The tree area: the materialized node rows — or, when `State` says materialization is
    /// gated, the explicit Show/Search button INSTEAD (zero node rows are generated, whatever
    /// `tree` holds). The two alternatives carry DIFFERENT keys, so the swap recreates cleanly
    /// instead of patching one styled subtree into the other.
    let private treeArea (state : FacetedTreeControlsState) (handlers : FacetedTreeControlsHandlers) : IView =
        match state.materialization with
        | TreeGated ->
            clickBox ConstructorUiId.facetShowTreeButton (ControlCaption "Show / Search") idleBackground (fun () -> handlers.requestBuild ())
        | TreeMaterialized ->
            let keyedTree =
                StackPanel.create [
                    ConstructorUiId.facetTree.automationAttribute<StackPanel> ()
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.children (state.tree |> List.collect (nodeRows handlers state.selectedCodeOpt (FacetTreeDepth 0)))
                ]
                |> Avalonia.FuncUI.DSL.View.withKey ConstructorUiId.facetTree.value
            keyedTree :> IView

    /// The faceted-tree surface — filter box, representation picker, breadcrumb strip, live
    /// result count (under everything applied), offer groups, and the tree (or its Show/Search
    /// gate), top to bottom, all in ONE ScrollViewer (the tree may span pages).
    let facetedTreeControlsView (state : FacetedTreeControlsState) (handlers : FacetedTreeControlsHandlers) : IView =
        ScrollViewer.create [
            ConstructorUiId.facetScroll.automationAttribute<ScrollViewer> ()
            ScrollViewer.content (
                StackPanel.create [
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.spacing 4.0
                    StackPanel.children [
                        filterRow state handlers
                        representationRow state handlers
                        breadcrumbStripView state handlers
                        TextBlock.create [
                            ConstructorUiId.facetResultCount.automationAttribute<TextBlock> ()
                            TextBlock.text $"%d{state.resultCount.value} results"
                        ]
                        offersPanelView state handlers
                        treeArea state handlers
                    ]
                ])
        ] :> IView
