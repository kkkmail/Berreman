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

/// The reusable MATERIALS control (Spec 0033, step 015 — UICOMP_XDUO_0001, the materials-workbench
/// list surface): a searchable materials list with a search box, category and dispersion facet
/// selectors, and Add / Edit / Remove / View verbs over the selected entry. This control is
/// DOMAIN-FREE (matching `LibraryControls` / `ExperimentControls`): the host runs the search seam
/// (`MaterialProxy.searchMaterials` over a `MaterialQuery`) and flattens the result into `Row`s and
/// the facets into `FacetOption`s; the control renders the rows it is given and never touches a
/// proxy. Behaviour is injected as a `Handlers` function record (the functional-proxy seam; tests
/// pass stubs). Rows for view-only entries (no edit model — spec 0033 step 013's
/// `complexity = None` built-ins) carry no Edit affordance: the Edit verb is REMOVED from the verb
/// row while such a row is selected, not greyed.
module MaterialsControls =

    /// Whether a listed material carries an edit model. A `ViewOnly` entry (a built-in whose
    /// complexity is not editable) can still be selected, viewed, and removed — but never edited.
    type MaterialEditability =
        | Editable
        | ViewOnly

    /// One row the host pre-flattens from its materials search (Controls carries no domain types).
    /// `materialId` is the material's stable id in its string form.
    type Row =
        {
            materialId : string
            label : string
            editability : MaterialEditability
        }

    /// One facet option the host offers (category / dispersion): a stable `code` the host maps back
    /// to its domain facet, and the label shown on the option box.
    type FacetOption =
        {
            code : string
            label : string
        }

    /// The bay's pure, serializable state. The search text and the two facet selections mirror the
    /// domain `MaterialQuery` seam (text + category + dispersion) without importing it; the host
    /// re-runs its search and hands back new `rows` whenever they change.
    type State =
        {
            /// The live search text (the host filters; the control only echoes and dispatches).
            searchText : string
            /// The category facet options, in display order (the host includes its "all" option).
            categoryOptions : FacetOption list
            /// The selected category option's `code`.
            selectedCategory : string
            /// The dispersion facet options, in display order.
            dispersionOptions : FacetOption list
            /// The selected dispersion option's `code`.
            selectedDispersion : string
            /// The rows the host's search produced — rendered as given, never re-filtered here.
            rows : Row list
            /// The selected row's material id, if any (the Edit / Remove / View verbs' target).
            selectedId : string option
        }

    let empty : State =
        {
            searchText = ""
            categoryOptions = []
            selectedCategory = ""
            dispersionOptions = []
            selectedDispersion = ""
            rows = []
            selectedId = None
        }

    /// Behaviour injected by the host (the functional-proxy seam; tests pass stubs). The verbs take
    /// no argument — the host owns the selection (`selectedId`) the verb applies to.
    type Handlers =
        {
            /// The search text changed — the host re-runs its search seam.
            setSearchText : string -> unit
            /// Pick a category facet option (by its `code`).
            selectCategory : string -> unit
            /// Pick a dispersion facet option (by its `code`).
            selectDispersion : string -> unit
            /// Select the row with this material id (the verbs' target).
            selectMaterial : string -> unit
            /// The verbs: create a new material; edit / remove / view the selected one.
            addMaterial : unit -> unit
            editMaterial : unit -> unit
            removeMaterial : unit -> unit
            viewMaterial : unit -> unit
        }

    /// The row the current selection points at; `None` when nothing is selected or the host's
    /// filter no longer lists the selected id (the row-targeted verbs then disable).
    let selectedRow (state : State) : Row option =
        match state.selectedId with
        | Some id -> state.rows |> List.tryFind (fun r -> r.materialId = id)
        | None -> None

    // -- The button look, identical to the other bars' idle/chosen boxes (so they MATCH). --
    let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private idleBackground = color 232 232 232
    let private chosenBackground = color 150 185 235
    let private idleBorder = color 120 120 120

    /// Set `AutomationProperties.AutomationId` (a freely-mutable attached property — unlike
    /// `Control.Name`) through FuncUI's attr builder. The rows, the facet options, and the verb
    /// buttons all live in child lists whose MEMBERSHIP changes (the host's filter rewrites the
    /// rows; the Edit verb is removed for a view-only selection), so a sibling can shift onto a
    /// reused control's slot — Avalonia forbids renaming a styled control, and an AutomationId
    /// survives that reuse (the `ExperimentControls` precedent).
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

    /// A verb button (Add / Edit / Remove / View), accented when it is the primary verb.
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
            // Keyed: the verb row's membership changes (a view-only selection REMOVES the Edit verb),
            // so a shifted verb recreates instead of patching its neighbour's styled control.
            |> Avalonia.FuncUI.DSL.View.withKey autoId
        keyedButton :> IView

    /// The search row: a label and the live search box (the host filters on every text change —
    /// the same idiom as the Ui materials panel's search field).
    let private searchRow (state : State) (handlers : Handlers) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 6.0
            StackPanel.children [
                TextBlock.create [ TextBlock.text "Search:"; TextBlock.verticalAlignment VerticalAlignment.Center ]
                TextBox.create [
                    TextBox.name UiIds.Materials.searchBox
                    TextBox.width 220.0
                    TextBox.text state.searchText
                    TextBox.onTextChanged handlers.setSearchText
                ]
            ]
        ] :> IView

    /// One facet selector: a titled, NAMED WrapPanel (the UiIds container id) of clickable option
    /// boxes, the selected code highlighted.
    let private facetRow (containerId : string) (title : string) (options : FacetOption list) (selectedCode : string) (optionId : string -> string) (select : string -> unit) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 6.0
            StackPanel.children [
                TextBlock.create [ TextBlock.text title; TextBlock.verticalAlignment VerticalAlignment.Center ]
                WrapPanel.create [
                    WrapPanel.name containerId
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.children (
                        options
                        |> List.map (fun o -> clickBox (optionId o.code) o.label (selectedCode = o.code) (fun () -> select o.code)))
                ]
            ]
        ] :> IView

    /// One listed material row — clickable (selects it), highlighted when it is the selection.
    let private rowView (state : State) (handlers : Handlers) (r : Row) : IView =
        clickBox (UiIds.Materials.row r.materialId) r.label (state.selectedId = Some r.materialId) (fun () -> handlers.selectMaterial r.materialId)

    /// The materials list: a NAMED vertical stack of the host's rows, rendered as given.
    let private listView (state : State) (handlers : Handlers) : IView =
        ScrollViewer.create [
            ScrollViewer.maxHeight 220.0
            ScrollViewer.content (
                StackPanel.create [
                    StackPanel.name UiIds.Materials.list
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.children (state.rows |> List.map (rowView state handlers))
                ])
        ] :> IView

    /// The verb row. Add always applies; Remove / View need a listed selection; Edit additionally
    /// needs the selection to be `Editable` — a `ViewOnly` selection REMOVES the Edit verb from the
    /// row (no Edit affordance at all, per the slice), it does not grey it out.
    let private verbRow (state : State) (handlers : Handlers) : IView =
        let selected = selectedRow state
        let hasSelection =
            match selected with
            | Some _ -> true
            | None -> false
        let editVerb =
            match selected with
            | Some { editability = ViewOnly } -> []
            | Some { editability = Editable } -> [ verbButton UiIds.Materials.editButton "Edit" false true (fun () -> handlers.editMaterial ()) ]
            | None -> [ verbButton UiIds.Materials.editButton "Edit" false false (fun () -> handlers.editMaterial ()) ]
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 0.0
            StackPanel.children (
                [ verbButton UiIds.Materials.addButton "Add" true true (fun () -> handlers.addMaterial ()) ]
                @ editVerb
                @ [
                    verbButton UiIds.Materials.removeButton "Remove" false hasSelection (fun () -> handlers.removeMaterial ())
                    verbButton UiIds.Materials.viewButton "View" false hasSelection (fun () -> handlers.viewMaterial ())
                ])
        ] :> IView

    /// The materials workbench surface — search box, the two facet selectors, the materials list,
    /// and the Add / Edit / Remove / View verbs, top to bottom.
    let view (state : State) (handlers : Handlers) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 4.0
            StackPanel.children [
                searchRow state handlers
                facetRow UiIds.Materials.categoryFilter "Category:" state.categoryOptions state.selectedCategory UiIds.Materials.categoryOption handlers.selectCategory
                facetRow UiIds.Materials.dispersionFilter "Dispersion:" state.dispersionOptions state.selectedDispersion UiIds.Materials.dispersionOption handlers.selectDispersion
                listView state handlers
                verbRow state handlers
            ]
        ] :> IView
