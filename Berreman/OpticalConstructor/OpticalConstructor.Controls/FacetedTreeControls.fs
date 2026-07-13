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

    /// One applied-constraint breadcrumb chip, in application order: a stable `code` the host maps
    /// back to its applied constraint, the display label, and the cumulative result count after
    /// applying this chip and everything before it (the engine's `breadcrumbCounts` shape).
    /// Clicking the chip removes it.
    type BreadcrumbChip =
        {
            code : string
            label : string
            afterCount : int
        }

    /// One offered value under a facet: a stable `code`, the display label, and the count-preview
    /// (the result count IF this value were applied on top of everything already applied — the
    /// engine's `countFor` shape). Clicking it applies the constraint.
    type OfferedValue =
        {
            code : string
            label : string
            previewCount : int
        }

    /// One facet's offer group: a stable `code`, the facet title, its offered values in display
    /// order (numeric facets offer their buckets here, labels bucket-shaped: `10-20 nm (7)`), and
    /// whether a manual min–max range entry is offered alongside them.
    type OfferGroup =
        {
            code : string
            title : string
            values : OfferedValue list
            manualRange : ManualRange
        }

    /// One named representation (tree-shaping facet order) the picker offers, by stable `code`.
    type NamedRepresentation =
        {
            code : string
            label : string
        }

    /// One node of the built tree the host projects (facet heading, value branch, or item leaf —
    /// the control does not distinguish). `code` is a host-supplied stable token, UNIQUE across
    /// the whole tree (path-shaped codes work); it keys the row and returns through `selectNode`.
    /// `countOpt` carries the branch's item count where the host has one — a branch row reads
    /// `label (count)`, a row without a count (a heading or a leaf) reads its bare label.
    /// Collapsed nodes render no children but still show their counts.
    type TreeNode =
        {
            code : string
            label : string
            countOpt : int option
            expansion : NodeExpansion
            children : TreeNode list
        }

    /// The control's pure, serializable state — everything the render needs, projected in by the
    /// host on every change (the host owns the engine; the control owns nothing).
    type State =
        {
            /// The built tree's top-level nodes (empty until the host materializes one).
            tree : TreeNode list
            /// The applied-constraint chips, in application (breadcrumb) order.
            breadcrumbs : BreadcrumbChip list
            /// The offer groups, facets in display order, every value carrying its count-preview.
            offers : OfferGroup list
            /// The named representations the picker offers, in display order.
            representations : NamedRepresentation list
            /// The active representation's `code`.
            activeRepresentation : string
            /// The text-filter DRAFT the box shows (the last committed query, until the user
            /// types); typing never dispatches — only Enter/LostFocus commit.
            filterDraft : string
            /// The live result count under everything applied.
            resultCount : int
            /// Whether the tree body renders or the Show/Search button gates it.
            materialization : TreeMaterialization
            /// The `code` of the currently SELECTED tree node — the row painted visibly distinct
            /// (spec 0040 step 003). Empty string = nothing selected (the `activeRepresentation`
            /// convention). The HOST tracks the selection in its Model and projects the code here;
            /// the control only reads it to highlight the matching row.
            selectedCode : string
        }

    let empty : State =
        {
            tree = []
            breadcrumbs = []
            offers = []
            representations = []
            activeRepresentation = ""
            filterDraft = ""
            resultCount = 0
            materialization = TreeMaterialized
            selectedCode = ""
        }

    /// Behaviour injected by the host (the functional-proxy seam; tests pass stubs). Every
    /// argument is a `State`-supplied stable token (or, for the text boxes, the raw box text) —
    /// the host maps tokens back to its domain values and re-projects a new `State`.
    type Handlers =
        {
            /// Apply the offered value as a constraint: the offer group's `code`, the value's `code`.
            applyConstraint : string -> string -> unit
            /// Remove the applied-constraint chip with this `code`.
            removeConstraint : string -> unit
            /// Commit the filter box's text (dispatched on Enter/LostFocus ONLY, never per keystroke).
            commitTextFilter : string -> unit
            /// Choose the named representation with this `code`.
            chooseRepresentation : string -> unit
            /// Materialize the gated tree (the Show/Search button).
            requestBuild : unit -> unit
            /// Select the tree node with this `code`.
            selectNode : string -> unit
            /// Toggle the tree node with this `code` between expanded and collapsed — a
            /// disclosure-chevron press (spec 0040 step 002). Only a PARENT node (one with
            /// children) renders a chevron, so only a parent's code is ever dispatched here; the
            /// HOST holds the expanded-code set and re-projects each node's `expansion`.
            toggleNode : string -> unit
            /// Apply a manual min–max range on the offer group with this `code`; the second
            /// argument is the range box's RAW text (e.g. "10-20") — the HOST parses it and, on
            /// success, applies an ordinary constraint chip.
            applyManualRange : string -> string -> unit
        }

    // -- The button look, identical to the other bars' idle/chosen boxes (so they MATCH). --
    let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private idleBackground = color 232 232 232
    let private chosenBackground = color 150 185 235
    let private idleBorder = color 120 120 120
    // A SELECTED tree row (spec 0040 step 003) reads with the `chosenBackground` fill AND a
    // thicker border — a non-hue, colourblind-safe cue that survives when the hue is imperceptible,
    // so the selection never rests on colour alone.
    let private idleBorderThickness = 1.0
    let private selectedBorderThickness = 3.0

    /// Set `AutomationProperties.AutomationId` (a freely-mutable attached property — unlike
    /// `Control.Name`) through FuncUI's attr builder. NOTHING in this control sets
    /// `StyledElement.Name`: every list here has variable membership (the host rewrites chips,
    /// offers, and rows on each constraint change, and the tree body swaps with the Show/Search
    /// button), so a sibling can shift onto a reused control's slot — Avalonia forbids renaming a
    /// styled control, and an AutomationId survives that reuse (the `MaterialsControls` /
    /// `ExperimentControls` precedent, spec 0038 Part A).
    let private automationId<'View when 'View :> Control> (autoId : string) : IAttr<'View> =
        AttrBuilder<'View>.CreateProperty<string>(AutomationProperties.AutomationIdProperty, autoId, ValueNone)

    /// A clickable, styled box (a chip, an offered value, a representation option, or the
    /// Show/Search button), highlighted when chosen, KEYED by its id (`View.withKey`) so a
    /// membership change recreates a shifted box instead of patching another item's styled control
    /// in place. `e.Handled <- true` drops FuncUI's duplicate Tunnel|Bubble pass; re-subscribe
    /// when the id or the highlight changes so a reused box can't keep a stale handler.
    let private clickBox (autoId : string) (label : string) (chosen : bool) (onClick : unit -> unit) : IView =
        let keyedBox =
            Border.create [
                automationId<Border> autoId
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

    /// Dispatch `commit` with the event source's CURRENT text. The boxes below subscribe this on
    /// Enter and LostFocus ONLY — no text-change handler exists anywhere in this control, so
    /// typing dispatches nothing and the host's (tree-rebuilding) state stays untouched until an
    /// explicit commit (spec 0038 §0.4; the `RotationControls` / `ExperimentControls` idiom).
    let private commitFrom (commit : string -> unit) (src : obj) : unit =
        match src with
        | :? TextBox as tb when not (isNull tb.Text) -> commit tb.Text
        | _ -> ()

    /// The Enter-key commit: `e.Handled <- true` BEFORE dispatching drops FuncUI's duplicate
    /// Tunnel|Bubble pass (the same discipline `clickBox` applies to pointer events) — without it
    /// one Enter press commits twice.
    let private commitOnEnter (commit : string -> unit) (e : KeyEventArgs) : unit =
        if e.Key = Key.Enter then
            e.Handled <- true
            commitFrom commit e.Source

    /// The filter row: a label and the text-filter box. The box shows the host's committed draft
    /// and commits on Enter / blur only.
    let private filterRow (state : State) (handlers : Handlers) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 6.0
            StackPanel.children [
                TextBlock.create [ TextBlock.text "Filter:"; TextBlock.verticalAlignment VerticalAlignment.Center ]
                TextBox.create [
                    automationId<TextBox> UiIds.FacetedTree.filterBox
                    TextBox.width 220.0
                    TextBox.text state.filterDraft
                    TextBox.onKeyDown (commitOnEnter handlers.commitTextFilter)
                    TextBox.onLostFocus (fun e -> commitFrom handlers.commitTextFilter e.Source)
                ]
            ]
        ] :> IView

    /// The representation picker: a titled WrapPanel of clickable representation options, the
    /// active one highlighted.
    let private representationRow (state : State) (handlers : Handlers) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 6.0
            StackPanel.children [
                TextBlock.create [ TextBlock.text "View by:"; TextBlock.verticalAlignment VerticalAlignment.Center ]
                WrapPanel.create [
                    automationId<WrapPanel> UiIds.FacetedTree.representationPicker
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.children (
                        state.representations
                        |> List.map (fun r ->
                            clickBox
                                (UiIds.FacetedTree.representationOption r.code)
                                r.label
                                (state.activeRepresentation = r.code)
                                (fun () -> handlers.chooseRepresentation r.code)))
                ]
            ]
        ] :> IView

    /// The breadcrumb strip: the applied constraints in application order, each a removable chip
    /// reading `label (afterCount) ×` — clicking a chip REMOVES its constraint. Chips render
    /// highlighted (they are applied state, not offers).
    let private breadcrumbStripView (state : State) (handlers : Handlers) : IView =
        WrapPanel.create [
            automationId<WrapPanel> UiIds.FacetedTree.breadcrumbStrip
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.children (
                state.breadcrumbs
                |> List.map (fun chip ->
                    clickBox
                        (UiIds.FacetedTree.breadcrumbChip chip.code)
                        $"%s{chip.label} (%d{chip.afterCount}) ×"
                        true
                        (fun () -> handlers.removeConstraint chip.code)))
        ] :> IView

    /// The live result count under everything applied.
    let private resultCountRow (state : State) : IView =
        TextBlock.create [
            automationId<TextBlock> UiIds.FacetedTree.resultCount
            TextBlock.text $"%d{state.resultCount} results"
        ] :> IView

    /// One offer group: the facet title, its offered values as clickable count-previewed boxes,
    /// and — for numeric groups — the manual min–max box, committing its RAW text on Enter / blur
    /// only (the host parses; an unparseable or empty commit is the host's no-op).
    let private offerGroupView (handlers : Handlers) (group : OfferGroup) : IView =
        let rangeBox : IView list =
            match group.manualRange with
            | NoManualRange -> []
            | ManualRangeOffered ->
                [
                    (TextBox.create [
                        automationId<TextBox> (UiIds.FacetedTree.manualRangeBox group.code)
                        TextBox.width 110.0
                        TextBox.onKeyDown (commitOnEnter (handlers.applyManualRange group.code))
                        TextBox.onLostFocus (fun e -> commitFrom (handlers.applyManualRange group.code) e.Source)
                     ]
                     |> Avalonia.FuncUI.DSL.View.withKey (UiIds.FacetedTree.manualRangeBox group.code)) :> IView
                ]
        let keyedGroup =
            StackPanel.create [
                automationId<StackPanel> (UiIds.FacetedTree.offerGroup group.code)
                StackPanel.orientation Orientation.Horizontal
                StackPanel.spacing 6.0
                StackPanel.children [
                    TextBlock.create [ TextBlock.text (group.title + ":"); TextBlock.verticalAlignment VerticalAlignment.Center ]
                    WrapPanel.create [
                        WrapPanel.orientation Orientation.Horizontal
                        WrapPanel.children (
                            (group.values
                             |> List.map (fun v ->
                                 clickBox
                                     (UiIds.FacetedTree.offeredValue group.code v.code)
                                     $"%s{v.label} (%d{v.previewCount})"
                                     false
                                     (fun () -> handlers.applyConstraint group.code v.code)))
                            @ rangeBox)
                    ]
                ]
            ]
            |> Avalonia.FuncUI.DSL.View.withKey (UiIds.FacetedTree.offerGroup group.code)
        keyedGroup :> IView

    /// The offers panel: one row per offer group, facets in display order.
    let private offersPanelView (state : State) (handlers : Handlers) : IView =
        StackPanel.create [
            automationId<StackPanel> UiIds.FacetedTree.offersPanel
            StackPanel.orientation Orientation.Vertical
            StackPanel.children (state.offers |> List.map (offerGroupView handlers))
        ] :> IView

    /// A tree row's text: `label (count)` where the node carries a count (a branch — shown
    /// expanded OR collapsed), the bare label where it does not (a heading or a leaf).
    let private nodeText (node : TreeNode) : string =
        match node.countOpt with
        | Some count -> $"%s{node.label} (%d{count})"
        | None -> node.label

    /// One node's rows: a horizontal row of its disclosure chevron (a PARENT only — a leaf with
    /// empty `children` shows none) and its clickable, depth-indented label, then — only when the
    /// node is expanded — its children's rows below it. The chevron press toggles the node; the
    /// label press selects it. The flattened rows are siblings in ONE vertical stack, so every row
    /// (and the chevron and label within it) is keyed by its (tree-unique) node code.
    ///
    /// The row whose code matches `selectedCode` (the host's tracked selection, spec 0040 step 003)
    /// paints its label with `chosenBackground` AND a thicker border (a non-hue, colourblind-safe
    /// cue) so exactly that row reads distinct while every other stays idle. Re-subscribe the
    /// pointer handler when the code OR the selection flag changes so a reused, restyled box cannot
    /// keep a stale handler (the `clickBox` discipline).
    let rec private nodeRows (handlers : Handlers) (selectedCode : string) (depth : int) (node : TreeNode) : IView list =
        let isSelected = node.code = selectedCode
        let label =
            Border.create [
                automationId<Border> (UiIds.FacetedTree.treeNode node.code)
                Border.background (brush (if isSelected then chosenBackground else idleBackground))
                Border.borderBrush (brush idleBorder)
                Border.borderThickness (if isSelected then selectedBorderThickness else idleBorderThickness)
                Border.cornerRadius (CornerRadius 3.0)
                Border.padding (Thickness(10.0, 3.0))
                Border.verticalAlignment VerticalAlignment.Center
                Border.child (TextBlock.create [ TextBlock.text (nodeText node) ])
                Border.onPointerPressed ((fun e -> e.Handled <- true; handlers.selectNode node.code), SubPatchOptions.OnChangeOf (box (node.code, isSelected)))
            ]
            |> Avalonia.FuncUI.DSL.View.withKey (UiIds.FacetedTree.treeNode node.code)
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
                let chevronId = UiIds.FacetedTree.treeNodeChevron node.code
                [ Border.create [
                      automationId<Border> chevronId
                      Border.background (brush idleBackground)
                      Border.borderBrush (brush idleBorder)
                      Border.borderThickness 1.0
                      Border.cornerRadius (CornerRadius 3.0)
                      Border.padding (Thickness(6.0, 3.0))
                      Border.margin (Thickness(0.0, 0.0, 4.0, 0.0))
                      Border.verticalAlignment VerticalAlignment.Center
                      Border.child (TextBlock.create [ TextBlock.text glyph ])
                      Border.onPointerPressed ((fun e -> e.Handled <- true; handlers.toggleNode node.code), SubPatchOptions.OnChangeOf (box (node.code, glyph)))
                  ]
                  |> Avalonia.FuncUI.DSL.View.withKey chevronId
                  :> IView ]
        let row =
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.margin (Thickness(float depth * 16.0, 0.0, 0.0, 2.0))
                StackPanel.children (chevron @ [ label ])
            ]
            |> Avalonia.FuncUI.DSL.View.withKey ("FacetTreeRow_" + node.code)
            :> IView
        match node.expansion with
        | ExpandedNode -> row :: (node.children |> List.collect (nodeRows handlers selectedCode (depth + 1)))
        | CollapsedNode -> [ row ]

    /// The tree area: the materialized node rows — or, when `State` says materialization is
    /// gated, the explicit Show/Search button INSTEAD (zero node rows are generated, whatever
    /// `tree` holds). The two alternatives carry DIFFERENT keys, so the swap recreates cleanly
    /// instead of patching one styled subtree into the other.
    let private treeArea (state : State) (handlers : Handlers) : IView =
        match state.materialization with
        | TreeGated ->
            clickBox UiIds.FacetedTree.showTreeButton "Show / Search" false (fun () -> handlers.requestBuild ())
        | TreeMaterialized ->
            let keyedTree =
                StackPanel.create [
                    automationId<StackPanel> UiIds.FacetedTree.tree
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.children (state.tree |> List.collect (nodeRows handlers state.selectedCode 0))
                ]
                |> Avalonia.FuncUI.DSL.View.withKey UiIds.FacetedTree.tree
            keyedTree :> IView

    /// The faceted-tree surface — filter box, representation picker, breadcrumb strip, live
    /// result count, offer groups, and the tree (or its Show/Search gate), top to bottom, all in
    /// ONE ScrollViewer (the tree may span pages).
    let view (state : State) (handlers : Handlers) : IView =
        ScrollViewer.create [
            automationId<ScrollViewer> UiIds.FacetedTree.scrollViewer
            ScrollViewer.content (
                StackPanel.create [
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.spacing 4.0
                    StackPanel.children [
                        filterRow state handlers
                        representationRow state handlers
                        breadcrumbStripView state handlers
                        resultCountRow state
                        offersPanelView state handlers
                        treeArea state handlers
                    ]
                ])
        ] :> IView
