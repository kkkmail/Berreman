/// Materials panel view (spec 0024 Part U3 / R-1..R-3, AC-U3.1 / AC-U3.2). Browses the
/// materials library, previews a material's dispersion, and assigns a material to a
/// layer by drag-drop. All data and plot builders already exist; this slice renders and
/// wires them, hosting the dispersion preview in the WebView2 adapter delivered by slice
/// 002 (`ChartHosts.webView2Host`), never an `AvaPlot`.
///
/// Like the slice-002 `ChartView.chartPanel` and slice-003 `ConstructionView.stackPanel`,
/// the panel takes its sub-state (the `MaterialLibrary`, the `Filter` UI state, and the
/// `ConstructionPage.Model` whose layer rows are drop targets) plus dispatchers — NOT
/// `RootModel`, which lives in `Shell.fs` — so it composes under the root without a
/// module cycle. `Shell` passes `RootMsg.Materials >> dispatch` and
/// `RootMsg.Construction >> dispatch`.
///
/// This view re-implements NO search index and NO material resolution: filtering reuses
/// the existing `MaterialLibrary.byCategory` / `byNameContains` seams (R-1 /
/// Non-requirements) and a drop routes the single resolution seam
/// `StackEditor.layerMaterialDrop` (→ `MaterialLibrary.resolveMaterial`, R-3).
///
/// New sibling `*View.fs` module per §0.1; authored against the public MIT
/// `Avalonia.FuncUI` 1.6.0 DSL surface (§0.2) — no clone reference.
module OpticalConstructor.Ui.MaterialsView

open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Plotly.NET
open Berreman.Fields
open Berreman.Media
open OpticalConstructor.Domain
open OpticalConstructor.Domain.BeamTree
open OpticalConstructor.Domain.Units

// ---------------------------------------------------------------------------
// The materials panel's local UI state + message (R-1). Pure and serializable
// (§0.5): plain values only — no Avalonia handle, renderer instance, or token
// source. `Shell` holds a `Filter` and wraps `MaterialsMsg` in `RootMsg.Materials`.
// ---------------------------------------------------------------------------

/// The browse filter + current selection. `search`/`category` drive the filtered list
/// (through the existing library seams); `selected` is the id of the entry whose
/// dispersion preview is shown.
type Filter =
    {
        search : string
        category : MaterialLibrary.MaterialCategory option
        selected : string option
    }

module Filter =
    /// No filter applied, nothing selected — the first-frame state.
    let empty : Filter = { search = ""; category = None; selected = None }

/// Materials-panel edits: set the name fragment, set/clear the category filter, or
/// select a material to preview. Each reaches the model via the `Materials` `RootMsg`
/// case (R-1).
type MaterialsMsg =
    | SetSearch of string
    | SetCategory of MaterialLibrary.MaterialCategory option
    | SelectMaterial of string

/// Pure dispatcher over the filter state (R-1).
let update (msg : MaterialsMsg) (f : Filter) : Filter =
    match msg with
    | SetSearch s -> { f with search = s }
    | SetCategory c -> { f with category = c }
    | SelectMaterial id -> { f with selected = Some id }

// ---------------------------------------------------------------------------
// Filtering (R-1) — reuse the existing library seams; build NO new index
// (Non-requirement). Category narrows via `byCategory`; a non-empty search
// intersects with `byNameContains`.
// ---------------------------------------------------------------------------

/// The library entries matching the current filter, computed ONLY through the existing
/// `MaterialLibrary.byCategory` / `byNameContains` seams (R-1). The search/category
/// composition is a plain set intersection — no search index is built.
let filteredEntries (lib : MaterialLibrary.MaterialLibrary) (f : Filter) : MaterialLibrary.MaterialEntry list =
    let byCat =
        match f.category with
        | Some c -> MaterialLibrary.byCategory c lib
        | None -> lib.entries
    if System.String.IsNullOrWhiteSpace f.search then
        byCat
    else
        let namedIds = MaterialLibrary.byNameContains f.search lib |> List.map (fun e -> e.id) |> Set.ofList
        byCat |> List.filter (fun e -> Set.contains e.id namedIds)

// ---------------------------------------------------------------------------
// Active wavelength (R-2 / R-3). `resolveMaterial` (and therefore the preview and a
// drop) needs a `WaveLength`: a dispersive entry has no single tensor until a
// wavelength is chosen. The model carries no single active wavelength yet (the
// source-driven lift is a later part), so use the seed source's 550 nm light —
// `Templates.defaultLight`, the one canonical light the project is seeded from.
// ---------------------------------------------------------------------------

/// The reference wavelength dispersive entries are resolved at for the preview and a
/// drop. The model-driven active-wavelength lift is deferred; see Gotchas.
let referenceWavelength : WaveLength = Templates.defaultLight.waveLength

// ---------------------------------------------------------------------------
// Drag-drop (R-3). Material entries are drag sources carrying ONLY the stable
// material id; the selected node's layer rows are drop targets. A drop routes the
// single seam `StackEditor.layerMaterialDrop` (→ `resolveMaterial`) and dispatches
// the resulting `Construction (EditStack …)`. The view resolves nothing itself.
// ---------------------------------------------------------------------------

/// The drag payload format key carrying a dragged material's stable id.
[<Literal>]
let MaterialDataFormat = "oc-material-id"

/// Apply a material drop on the layer at `index` of the node at `path` (R-3 /
/// AC-U3.2). Builds the edit via `StackEditor.layerMaterialDrop` at
/// `referenceWavelength` — the view resolves NO material itself — and dispatches the
/// resulting `Construction (EditStack (path, SetLayerMaterial …))`, which the frozen
/// `ConstructionPage.update` applies (thickness unchanged). An unknown id resolves to
/// `Error` and the drop is a no-op (`layerMaterialDrop` never throws). The `onDrop`
/// handler below is a thin wrapper over this seam; the headless test drives it directly.
let materialDrop
    (lib : MaterialLibrary.MaterialLibrary)
    (w : WaveLength)
    (path : ConstructionPage.NodePath)
    (dispatch : ConstructionPage.Msg -> unit)
    (index : int)
    (materialId : string)
    : unit =
    match StackEditor.layerMaterialDrop lib w index materialId with
    | Ok sm -> dispatch (ConstructionPage.EditStack (path, sm))
    | Error _ -> ()

/// The selected node whose layer rows are the drop targets, falling back to the tree
/// root when the selected path no longer resolves so the view is total.
let private selectedNode (model : ConstructionPage.Model) : BeamNode =
    match ConstructionPage.tryGetNode model.selected model.project.beamTree.root with
    | Some node -> node
    | None -> model.project.beamTree.root

// ---------------------------------------------------------------------------
// View.
// ---------------------------------------------------------------------------

/// A category filter button (R-1): clicking sets/clears the category filter.
let private categoryButton (dispatch : MaterialsMsg -> unit) (label : string) (cat : MaterialLibrary.MaterialCategory option) (active : MaterialLibrary.MaterialCategory option) : IView =
    Button.create [
        Button.content label
        Button.background (if cat = active then Brushes.SteelBlue :> IBrush else Brushes.Transparent :> IBrush)
        Button.onClick (fun _ -> dispatch (SetCategory cat))
    ] :> IView

/// The search box + category filter row (R-1).
let private filterBar (filter : Filter) (dispatch : MaterialsMsg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 4.0
        StackPanel.margin 4.0
        StackPanel.children [
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.spacing 4.0
                StackPanel.children [
                    TextBlock.create [ TextBlock.text "Search:"; TextBlock.verticalAlignment VerticalAlignment.Center ]
                    TextBox.create [
                        TextBox.width 160.0
                        TextBox.text filter.search
                        TextBox.onTextChanged (fun t -> dispatch (SetSearch t))
                    ]
                ]
            ]
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.spacing 4.0
                StackPanel.children [
                    categoryButton dispatch "All" None filter.category
                    categoryButton dispatch "Glass" (Some MaterialLibrary.Glass) filter.category
                    categoryButton dispatch "Metal" (Some MaterialLibrary.Metal) filter.category
                    categoryButton dispatch "Semiconductor" (Some MaterialLibrary.Semiconductor) filter.category
                    categoryButton dispatch "Crystal" (Some MaterialLibrary.Crystal) filter.category
                ]
            ]
        ]
    ] :> IView

/// One library entry row (R-1 / R-3): a selectable button (selects it for preview) wrapped
/// in a drag-source border that carries the entry's stable id on a drag gesture.
let private entryRow (dispatch : MaterialsMsg -> unit) (selected : string option) (entry : MaterialLibrary.MaterialEntry) : IView =
    let isSelected = selected = Some entry.id
    Border.create [
        Border.background (if isSelected then Brushes.LightSteelBlue :> IBrush else Brushes.Transparent :> IBrush)
        // Real Avalonia drag source: a drag gesture carries ONLY the stable material id
        // (§A.7); resolution happens on drop through `layerMaterialDrop` (R-3). Never
        // fires headlessly (no pointer), so the smoke/view tests are unaffected.
        Border.onPointerPressed (fun e ->
            let data = Avalonia.Input.DataObject()
            data.Set(MaterialDataFormat, entry.id)
            Avalonia.Input.DragDrop.DoDragDrop(e, data, Avalonia.Input.DragDropEffects.Copy) |> ignore)
        Border.child (
            Button.create [
                Button.content entry.name
                Button.horizontalAlignment HorizontalAlignment.Stretch
                Button.onClick (fun _ -> dispatch (SelectMaterial entry.id))
            ])
    ] :> IView

/// The filtered library list (R-1). Scrollable so it lays out independently of panel height.
let private libraryList (lib : MaterialLibrary.MaterialLibrary) (filter : Filter) (dispatch : MaterialsMsg -> unit) : IView =
    let rows =
        match filteredEntries lib filter with
        | [] -> [ TextBlock.create [ TextBlock.text "(no matching materials)" ] :> IView ]
        | entries -> entries |> List.map (entryRow dispatch filter.selected)
    ScrollViewer.create [
        ScrollViewer.maxHeight 220.0
        ScrollViewer.content (
            StackPanel.create [
                StackPanel.orientation Orientation.Vertical
                StackPanel.spacing 2.0
                StackPanel.children rows
            ])
    ] :> IView

/// The dispersion preview for the selected entry (R-2 / AC-U3.1). A Plotly
/// `GenericChart` of the entry's Re[ε₁₁] dispersion, hosted in the WebView2 adapter
/// (`ChartHosts.webView2Host`) — NEVER an `AvaPlot`. The chart is passed lazily so the
/// headless placeholder path never pays to build it; headlessly the host degrades to the
/// §U1.8 "unavailable" placeholder.
///
/// NOTE (§7 skepticism rule): the slice spec assumed `MaterialPreview.show*` return a
/// `Plotly.NET.GenericChart`, but in the code they pipe through `Analytics.Charting`'s
/// `plotDispersion`, which ends in `Chart.show` and returns `unit` (it opens a browser —
/// unembeddable, and unsafe to call in a headless render). So the preview reuses the
/// engine dispersion DATA calculator `Analytics.Variables.calculateN11Re` (no dispersion
/// re-derived) and constructs the Plotly chart with `Chart.Line` — the same construction
/// `plotDispersion` performs, minus the `Chart.show` side-effect. The spectral range is
/// still built through the existing `MaterialPreview.spectralRange` seam. See Gotchas.
let private dispersionPreview (lib : MaterialLibrary.MaterialLibrary) (filter : Filter) : IView =
    let entryOpt =
        filter.selected
        |> Option.bind (fun id -> lib.entries |> List.tryFind (fun e -> e.id = id))
    match entryOpt with
    | Some entry ->
        let range = MaterialPreview.spectralRange Nanometer 200.0 800.0 50
        let chart : Lazy<Plotly.NET.GenericChart> =
            lazy (
                let data = Analytics.Variables.calculateN11Re entry.properties range
                Chart.Line(data, Name = "Re[e11]"))
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 2.0
            StackPanel.margin 4.0
            StackPanel.children [
                TextBlock.create [ TextBlock.text (sprintf "Dispersion — %s" entry.name); TextBlock.fontWeight FontWeight.Bold ]
                Border.create [ Border.height 220.0; Border.child (ChartHosts.webView2Host chart) ]
            ]
        ] :> IView
    | None ->
        TextBlock.create [ TextBlock.text "Select a material to preview its dispersion."; TextBlock.margin 4.0 ] :> IView

/// One layer row that is a drop target (R-3): dropping a material here routes
/// `materialDrop`, which builds the edit via `layerMaterialDrop` and dispatches
/// `Construction (EditStack …)` (thickness unchanged). `allowDrop` opens the row to a drag.
let private dropRow
    (lib : MaterialLibrary.MaterialLibrary)
    (path : ConstructionPage.NodePath)
    (dispatchC : ConstructionPage.Msg -> unit)
    (u : UnitOfMeasure)
    (index : int)
    (layer : Layer)
    : IView =
    let label = sprintf "Layer %d — %s  (drop material here)" index (StackEditor.displayThickness u layer.thickness)
    Border.create [
        Border.borderThickness 1.0
        Border.borderBrush (Brushes.Gray :> IBrush)
        Border.padding 4.0
        Control.allowDrop true
        Control.onDrop (fun (e : Avalonia.Input.DragEventArgs) ->
            if e.Data.Contains MaterialDataFormat then
                match e.Data.Get MaterialDataFormat with
                | :? string as id -> materialDrop lib referenceWavelength path dispatchC index id
                | _ -> ())
        Border.child (TextBlock.create [ TextBlock.text label ])
    ] :> IView

/// The selected node's layer rows as drop targets (R-3). An empty stack still renders a
/// note so the panel always lays out one frame.
let private dropTargets (lib : MaterialLibrary.MaterialLibrary) (construction : ConstructionPage.Model) (dispatchC : ConstructionPage.Msg -> unit) : IView =
    let node = selectedNode construction
    let path = construction.selected
    let rows =
        match node.system.films with
        | [] -> [ TextBlock.create [ TextBlock.text "(no layers to assign to)" ] :> IView ]
        | films -> films |> List.mapi (fun i l -> dropRow lib path dispatchC node.defaultUnit i l)
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.margin 4.0
        StackPanel.children (
            (TextBlock.create [ TextBlock.text "Assign to layer"; TextBlock.fontWeight FontWeight.Bold ] :> IView)
            :: rows)
    ] :> IView

/// The materials panel (R-1/R-2/R-3): the search/category filter, the filtered library
/// list (drag sources), the selected material's WebView2-hosted dispersion preview, and
/// the selected node's layer rows as drop targets. Scrollable so each section lays out
/// independently of the panel height.
let materialsPanel
    (lib : MaterialLibrary.MaterialLibrary)
    (filter : Filter)
    (construction : ConstructionPage.Model)
    (dispatchM : MaterialsMsg -> unit)
    (dispatchC : ConstructionPage.Msg -> unit)
    : IView =
    let body =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 6.0
            StackPanel.children [
                TextBlock.create [ TextBlock.text "Materials"; TextBlock.fontWeight FontWeight.Bold; TextBlock.margin 4.0 ]
                filterBar filter dispatchM
                libraryList lib filter dispatchM
                dispersionPreview lib filter
                dropTargets lib construction dispatchC
            ]
        ]
    ScrollViewer.create [ ScrollViewer.content body ] :> IView
