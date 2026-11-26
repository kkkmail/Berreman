namespace OpticalConstructor.Controls

open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

/// One option of a workbench list's facet selector or picker (a materials category or dispersion, a
/// samples substrate, a faceted tree's representation): the stable `code` the host maps back to its
/// domain value, and the option box's label.
type FacetOption =
    {
        code : ConstructorUiKey
        label : ControlCaption
    }

/// The reusable MATERIALS control (Spec 0033, step 015 — UICOMP_XDUO_0001, the materials-workbench
/// list surface): a searchable materials list with a search box, category and dispersion facet
/// selectors, and Add / Edit / Remove / View verbs over the selected entry. This control is
/// DOMAIN-FREE (matching `LibraryControls` / `ExperimentControls`): the host runs the search seam
/// (`MaterialProxy.searchMaterials` over a `MaterialQuery`) and flattens the result into `Row`s and
/// the facets into `FacetOption`s; the control renders the rows it is given and never touches a
/// proxy. Behaviour is injected as a `Handlers` function record (the functional-proxy seam; tests
/// pass stubs).
module MaterialsControls =

    /// One row the host pre-flattens from its materials search (Controls carries no domain types).
    /// `materialId` is the material's stable id, as the host keys it.
    type MaterialsControlsRow =
        {
            materialId : ConstructorUiKey
            label : ControlCaption
        }

    /// The bay's pure, serializable state. The search text and the two facet selections mirror the
    /// domain `MaterialQuery` seam (text + category + dispersion) without importing it; the host
    /// re-runs its search and hands back new `rows` whenever they change.
    type MaterialsControlsState =
        {
            /// The live search text (the host filters; the control only echoes and dispatches).
            searchText : ControlCaption
            /// The category facet options, in display order (the host includes its "all" option).
            categoryOptions : FacetOption list
            /// The selected category option's `code`, if any.
            selectedCategory : ConstructorUiKey option
            /// The dispersion facet options, in display order.
            dispersionOptions : FacetOption list
            /// The selected dispersion option's `code`, if any.
            selectedDispersion : ConstructorUiKey option
            /// The rows the host's search produced — rendered as given, never re-filtered here.
            rows : MaterialsControlsRow list
            /// The selected row's material id, if any (the Edit / Remove / View verbs' target).
            selectedId : ConstructorUiKey option
        }

        /// The row the current selection points at; `None` when nothing is selected or the host's
        /// filter no longer lists the selected id (the row-targeted verbs then disable).
        member this.selectedRow : MaterialsControlsRow option =
            match this.selectedId with
            | Some id -> this.rows |> List.tryFind (fun r -> r.materialId = id)
            | None -> None

    let emptyMaterialsControls : MaterialsControlsState =
        {
            searchText = ControlCaption ""
            categoryOptions = []
            selectedCategory = None
            dispersionOptions = []
            selectedDispersion = None
            rows = []
            selectedId = None
        }

    /// Behaviour injected by the host (the functional-proxy seam; tests pass stubs). The verbs take
    /// no argument — the host owns the selection (`selectedId`) the verb applies to.
    type MaterialsControlsHandlers =
        {
            /// The search text changed — the host re-runs its search seam.
            setSearchText : ControlCaption -> unit
            /// Pick a category facet option (by its `code`).
            selectCategory : ConstructorUiKey -> unit
            /// Pick a dispersion facet option (by its `code`).
            selectDispersion : ConstructorUiKey -> unit
            /// Select the row with this material id (the verbs' target).
            selectMaterial : ConstructorUiKey -> unit
            /// The verbs: create a new material; edit / remove / view the selected one.
            addMaterial : unit -> unit
            editMaterial : unit -> unit
            removeMaterial : unit -> unit
            viewMaterial : unit -> unit
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
    /// Avalonia forbids renaming a styled control. KEYED by its id (`View.withKey`, spec 0038) so a
    /// membership change recreates a shifted box instead of patching another item's in place.
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

    /// A verb button (Add / Edit / Remove / View), on the accent background when it is the primary verb.
    let private verbButton (id : ConstructorUiId) (label : ControlCaption) (background : Color) (availability : ControlAvailability) (onClick : unit -> unit) : IView =
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
        ] :> IView

    /// The search row: a label and the live search box (the host filters on every text change —
    /// the same idiom as the Ui materials panel's search field).
    let private searchRow (state : MaterialsControlsState) (handlers : MaterialsControlsHandlers) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 6.0
            StackPanel.children [
                TextBlock.create [ TextBlock.text "Search:"; TextBlock.verticalAlignment VerticalAlignment.Center ]
                TextBox.create [
                    TextBox.name ConstructorUiId.materialSearchBox.value
                    TextBox.width 220.0
                    TextBox.text state.searchText.value
                    TextBox.onTextChanged (ControlCaption >> handlers.setSearchText)
                ]
            ]
        ] :> IView

    /// One facet selector: a titled, NAMED WrapPanel (the container id) of clickable option boxes, the
    /// selected code highlighted.
    let private facetRow (containerId : ConstructorUiId) (title : ControlCaption) (options : FacetOption list) (selected : ConstructorUiKey option) (optionId : ConstructorUiKey -> ConstructorUiId) (select : ConstructorUiKey -> unit) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 6.0
            StackPanel.children [
                TextBlock.create [ TextBlock.text title.value; TextBlock.verticalAlignment VerticalAlignment.Center ]
                WrapPanel.create [
                    WrapPanel.name containerId.value
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.children (
                        options
                        |> List.map (fun o -> clickBox (optionId o.code) o.label (boxBackground selected o.code) (fun () -> select o.code)))
                ]
            ]
        ] :> IView

    /// One listed material row — clickable (selects it), highlighted when it is the selection.
    let private rowView (state : MaterialsControlsState) (handlers : MaterialsControlsHandlers) (r : MaterialsControlsRow) : IView =
        clickBox (ConstructorUiId.materialRow r.materialId) r.label (boxBackground state.selectedId r.materialId) (fun () -> handlers.selectMaterial r.materialId)

    /// The materials list: a NAMED vertical stack of the host's rows, rendered as given.
    let private listView (state : MaterialsControlsState) (handlers : MaterialsControlsHandlers) : IView =
        ScrollViewer.create [
            ScrollViewer.maxHeight 220.0
            ScrollViewer.content (
                StackPanel.create [
                    StackPanel.name ConstructorUiId.materialsList.value
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.children (state.rows |> List.map (rowView state handlers))
                ])
        ] :> IView

    /// The verb row. Add always applies; Edit / Remove / View need a listed selection.
    let private verbRow (state : MaterialsControlsState) (handlers : MaterialsControlsHandlers) : IView =
        let onSelection =
            match state.selectedRow with
            | Some _ -> ControlOffered
            | None -> ControlWithheld
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 0.0
            StackPanel.children [
                verbButton ConstructorUiId.addMaterialButton (ControlCaption "Add") chosenBackground ControlOffered handlers.addMaterial
                verbButton ConstructorUiId.editMaterialButton (ControlCaption "Edit") idleBackground onSelection handlers.editMaterial
                verbButton ConstructorUiId.removeMaterialButton (ControlCaption "Remove") idleBackground onSelection handlers.removeMaterial
                verbButton ConstructorUiId.viewMaterialButton (ControlCaption "View") idleBackground onSelection handlers.viewMaterial
            ]
        ] :> IView

    /// The materials workbench surface — search box, the two facet selectors, the materials list,
    /// and the Add / Edit / Remove / View verbs, top to bottom.
    let materialsControlsView (state : MaterialsControlsState) (handlers : MaterialsControlsHandlers) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 4.0
            StackPanel.children [
                searchRow state handlers
                facetRow ConstructorUiId.materialCategoryFilter (ControlCaption "Category:") state.categoryOptions state.selectedCategory ConstructorUiId.materialCategoryOption handlers.selectCategory
                facetRow ConstructorUiId.materialDispersionFilter (ControlCaption "Dispersion:") state.dispersionOptions state.selectedDispersion ConstructorUiId.materialDispersionOption handlers.selectDispersion
                listView state handlers
                verbRow state handlers
            ]
        ] :> IView
