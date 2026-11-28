namespace OpticalConstructor.Controls

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
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
/// DOMAIN-FREE (matching `ExperimentControls`): the host runs the search seam
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

    /// The box keyed `key` is chosen while it is the selection.
    let private boxMark (selected : ConstructorUiKey option) (key : ConstructorUiKey) : ChoiceMark =
        if selected = Some key then ChosenChoice else UnchosenChoice

    /// A facet option or a listed row: a choice, chosen while it is the selection, keyed by its id so a membership
    /// change recreates a shifted box instead of patching another item's in place (spec 0038).
    let private choiceBox (id : ConstructorUiId) (label : ControlCaption) (mark : ChoiceMark) (onClick : unit -> unit) : IView =
        CommandControls.choiceToggle id label mark ChosenCommand ControlOffered onClick
            [ ToggleButton.padding (Thickness(10.0, 4.0)); ToggleButton.margin (Thickness(0.0, 0.0, 6.0, 4.0)) ]

    /// A verb button (Add / Edit / Remove / View), chosen-blue when it is the primary verb.
    let private verbButton (id : ConstructorUiId) (label : ControlCaption) (emphasis : CommandEmphasis) (availability : ControlAvailability) (onClick : unit -> unit) : IView =
        CommandControls.commandButton id label emphasis availability onClick [ Button.margin (Thickness(0.0, 0.0, 8.0, 0.0)) ]

    /// The search row: a label and the live search box (the host filters on every text change —
    /// the same idiom as the Ui materials panel's search field).
    let private searchRow (state : MaterialsControlsState) (handlers : MaterialsControlsHandlers) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 6.0
            StackPanel.children [
                TextBlock.create [ TextBlock.text "Search:"; TextBlock.verticalAlignment VerticalAlignment.Center ]
                TextBox.create [
                    yield! ConstructorUiId.materialSearchBox.inputAttributes<TextBox> ()
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
                    yield! containerId.groupAttributes<WrapPanel> ()
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.children (
                        options
                        |> List.map (fun o -> choiceBox (optionId o.code) o.label (boxMark selected o.code) (fun () -> select o.code)))
                ]
            ]
        ] :> IView

    /// One listed material row — clickable (selects it), highlighted when it is the selection.
    let private rowView (state : MaterialsControlsState) (handlers : MaterialsControlsHandlers) (r : MaterialsControlsRow) : IView =
        choiceBox (ConstructorUiId.materialRow r.materialId) r.label (boxMark state.selectedId r.materialId) (fun () -> handlers.selectMaterial r.materialId)

    /// The materials list: a NAMED vertical stack of the host's rows, rendered as given.
    let private listView (state : MaterialsControlsState) (handlers : MaterialsControlsHandlers) : IView =
        ScrollViewer.create [
            ScrollViewer.maxHeight 220.0
            ScrollViewer.content (
                StackPanel.create [
                    yield! ConstructorUiId.materialsList.groupAttributes<StackPanel> ()
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
                verbButton ConstructorUiId.addMaterialButton (ControlCaption "Add") ChosenCommand ControlOffered handlers.addMaterial
                verbButton ConstructorUiId.editMaterialButton (ControlCaption "Edit") NeutralCommand onSelection handlers.editMaterial
                verbButton ConstructorUiId.removeMaterialButton (ControlCaption "Remove") NeutralCommand onSelection handlers.removeMaterial
                verbButton ConstructorUiId.viewMaterialButton (ControlCaption "View") NeutralCommand onSelection handlers.viewMaterial
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
