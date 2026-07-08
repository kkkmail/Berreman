namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open Xunit
open Berreman.Dispersion
open OpticalConstructor.Domain
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.DispersionModels
open OpticalConstructor.Domain.MaterialComplexityEditor
open OpticalConstructor.Domain.Units
open OpticalConstructor.Controls
open OpticalConstructor.TestWindows
open OpticalConstructor.TestWindows.TableAndElementRotationView

/// Spec 0035 (016) — the shared dual-axis ScottPlot chart (`OpticalConstructor.Controls.EmbeddedChart`)
/// embedded as the Material editor's live n/k preview AND the Materials-bay View panel (replacing the
/// deleted primitive `NkDispersionChart.inlineCanvas`). Headless render proofs: the embedded control
/// carries the n series on the LEFT axis and the k series on the RIGHT, draws n and k for EVERY model
/// kind — including the four transcendental (step 15) — and rasterizes one frame without throwing under
/// the `ui-smoke` gate; and both host sites (the real editor preview and the real Materials View panel)
/// embed the AvaPlot and render, for a dispersive (incl. transcendental) and a non-dispersive entry.
module EmbeddedChartTests =

    // -- semantic-tree probes (the MaterialEditorWindow / MainWorkbench precedent) ----------------

    let private matchesId (id : string) (c : Control) : bool =
        c.Name = id || Avalonia.Automation.AutomationProperties.GetAutomationId(c) = id

    let private tryFindControl (window : Window) (id : string) : Control option =
        window.GetVisualDescendants()
        |> Seq.tryPick (function :? Control as c when matchesId id c -> Some c | _ -> None)

    let private isPresent (window : Window) (id : string) : bool =
        match tryFindControl window id with
        | Some _ -> true
        | None -> false

    let private clickOn (window : Window) (id : string) : unit =
        let found =
            window.GetVisualDescendants()
            |> Seq.tryPick (function :? Border as b when matchesId id b && b.IsEffectivelyVisible -> Some b | _ -> None)
        match found with
        | None -> Assert.Fail($"%s{id} was not found (or not visible)")
        | Some b ->
            let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
            if c.HasValue then
                window.MouseDown(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                if window.IsVisible then
                    window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
            else Assert.Fail($"%s{id} has no on-screen position")

    let private setText (window : Window) (id : string) (text : string) : unit =
        match tryFindControl window id with
        | Some (:? TextBox as tb) ->
            tb.Text <- text
            Dispatcher.UIThread.RunJobs()
        | Some c -> Assert.Fail($"%s{id} is a %s{c.GetType().Name}, not a TextBox")
        | None -> Assert.Fail($"%s{id} was not found")

    // -- AvaPlot probes ---------------------------------------------------------------------------

    /// The live `AvaPlot` hosted under the control carrying `id` (the `ContentControl.content` of the
    /// embedded chart host), or `None` if the host degraded to its placeholder.
    let private avaUnder (window : Window) (id : string) : ScottPlot.Avalonia.AvaPlot option =
        match tryFindControl window id with
        | Some host ->
            host.GetVisualDescendants()
            |> Seq.tryPick (function :? ScottPlot.Avalonia.AvaPlot as a -> Some a | _ -> None)
        | None -> None

    /// The scatters a plot carries (n first, k second for the n/k chart).
    let private scattersOf (ava : ScottPlot.Avalonia.AvaPlot) : ScottPlot.Plottables.Scatter list =
        ava.Plot.GetPlottables()
        |> Seq.choose (function :? ScottPlot.Plottables.Scatter as s -> Some s | _ -> None)
        |> List.ofSeq

    /// Force ScottPlot's real Skia rasterization — a construction- or render-time throw surfaces here
    /// (headless Avalonia never rasterizes on its own), the `ChartWindow constructs and really renders`
    /// precedent — so this proves the embedded chart renders one frame without throwing.
    let private rasterizes (ava : ScottPlot.Avalonia.AvaPlot) : bool =
        ava.Plot.GetImage(400, 200).GetImageBytes().Length > 0

    /// Assert `ava` carries exactly the n (left) and k (right) series and rasterizes.
    let private assertNkDualAxis (label : string) (ava : ScottPlot.Avalonia.AvaPlot) : unit =
        match scattersOf ava with
        | [ n; k ] ->
            Assert.True(obj.ReferenceEquals(n.Axes.YAxis, ava.Plot.Axes.Left), $"%s{label}: n must plot against the LEFT axis")
            Assert.True(obj.ReferenceEquals(k.Axes.YAxis, ava.Plot.Axes.Right), $"%s{label}: k must plot against the RIGHT axis")
        | other -> Assert.Fail($"%s{label}: expected exactly two scatters (n and k), got %d{List.length other}")
        Assert.True(rasterizes ava, $"%s{label}: the embedded chart rendered no image bytes")

    // -- chart / properties builders --------------------------------------------------------------

    let private chartOf (props : OpticalPropertiesWithDisp) =
        NkDispersionChart.nkDispersionChart props Nanometer MaterialEditorView.previewRange

    let private builtInProps (id : MaterialId) : OpticalPropertiesWithDisp =
        (builtInEntries |> List.find (fun e -> e.id = id)).properties

    let private applyOk (msg : MaterialComplexityMsg) (s : MaterialComplexityEditState) : MaterialComplexityEditState =
        match applyMaterialComplexityMsg msg s with
        | Ok next -> next
        | Error e -> failwith $"unexpected edit rejection: %A{e}"

    /// The engine properties of a single-segment dispersive material whose model is `code` — the
    /// `MaterialComplexityTests` derive-and-evaluate recipe (used for the four transcendental models).
    let private modelProps (code : string) : OpticalPropertiesWithDisp =
        let model = defaultModelChoices |> List.find (fun m -> modelKindCode m = code)
        let st = [ SetDispersion DispersiveSegments; ChooseSegmentModel (0, model) ] |> List.fold (fun s m -> applyOk m s) defaultState
        match toComplexity st with
        | Ok c -> c.toProperties
        | Error e -> failwith $"expected a derivable complexity for %s{code}, got %A{e}"

    /// Mount a bare `IView` in a sized headless window and render one frame (an explicit size, the
    /// `ChartWindow` precedent, so the embedded AvaPlot realizes into the visual tree).
    let private mount (view : Avalonia.FuncUI.Types.IView) : Window =
        let window = Window(Width = 700.0, Height = 400.0)
        window.Content <- Avalonia.FuncUI.Component(fun _ctx -> view)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window

    /// A fresh in-memory MaterialProxy for the Material editor window (the App composition).
    let private freshMaterialProxy () : MaterialProxy =
        let samples = SampleProxy.createInMemory ()
        MaterialProxy.createInMemory (samplesReferencing samples)

    // -- the Main workbench (the MainWorkbenchTests composition) -----------------------------------

    let private freshStores () : MaterialProxy * SampleProxy =
        let samples = SampleProxy.createInMemory ()
        let materials = MaterialProxy.createInMemory (samplesReferencing samples)
        materials, samples

    let private mainWith (materials : MaterialProxy) (samples : SampleProxy) : Model =
        let categories = CategoryProxy.createInMemory (materialsReferencingCategory materials)
        initMainWith (Library.createInMemory ()) (Experiments.createInMemory ()) materials samples categories

    let private mountMain (model0 : Model) : HostWindow =
        let window = HostWindow(Width = 980.0, Height = 1050.0)
        Program.mkSimple (fun () -> model0) update mainView
        |> Program.withHost window
        |> Program.run
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window

    let private hostId = "EmbeddedNkChartTestHost"

    // -- the embedded control: dual axes + rasterization, over every model kind --------------------

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the embedded n/k chart renders one frame with n on the left and k on the right (non-dispersive)`` () =
        HeadlessSession.run (fun () ->
            let chart = chartOf (builtInProps MaterialIds.vacuum)
            let window = mount (EmbeddedChart.create hostId chart (NkDispersionChart.nkDispersionStyle chart))
            match avaUnder window hostId with
            | Some ava -> assertNkDualAxis "vacuum" ava
            | None -> Assert.Fail("no embedded AvaPlot for the non-dispersive entry")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the embedded n/k chart renders one frame for a dispersive built-in (silicon)`` () =
        HeadlessSession.run (fun () ->
            let chart = chartOf (builtInProps MaterialIds.silicon)
            let window = mount (EmbeddedChart.create hostId chart (NkDispersionChart.nkDispersionStyle chart))
            match avaUnder window hostId with
            | Some ava -> assertNkDualAxis "silicon" ava
            | None -> Assert.Fail("no embedded AvaPlot for the dispersive built-in")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the embedded n/k chart draws n and k and rasterizes for every transcendental model kind`` () =
        HeadlessSession.run (fun () ->
            for code in [ "TaucLorentz"; "GaussianOscillator"; "ForouhiBloomer"; "BrendelBormann" ] do
                let chart = chartOf (modelProps code)
                let window = mount (EmbeddedChart.create hostId chart (NkDispersionChart.nkDispersionStyle chart))
                match avaUnder window hostId with
                | Some ava -> assertNkDualAxis code ava
                | None -> Assert.Fail($"no embedded AvaPlot for the transcendental model %s{code}")
                window.Close())

    // -- host site: the Material editor's live preview --------------------------------------------

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Material editor preview embeds the shared AvaPlot for a non-dispersive default and a transcendental model`` () =
        HeadlessSession.run (fun () ->
            let window = MaterialEditorWindow(freshMaterialProxy (), None)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // The default (isotropic, non-dispersive) editor derives a preview: the embedded AvaPlot renders.
            Assert.True(isPresent window MaterialEditorView.UiIds.previewChart, "the editor must embed the preview chart host")
            match avaUnder window MaterialEditorView.UiIds.previewChart with
            | Some ava -> Assert.True(rasterizes ava, "the non-dispersive editor preview rendered no image bytes")
            | None -> Assert.Fail("no embedded AvaPlot under the non-dispersive editor preview")
            // A transcendental pick (ForouhiBloomer) still derives and draws n/k through the SAME embedded chart.
            clickOn window MaterialEditorView.UiIds.dispersiveToggle
            clickOn window (MaterialEditorView.UiIds.modelOption 0 "ForouhiBloomer")
            Dispatcher.UIThread.RunJobs()
            match avaUnder window MaterialEditorView.UiIds.previewChart with
            | Some ava -> Assert.True(rasterizes ava, "the transcendental editor preview rendered no image bytes")
            | None -> Assert.Fail("no embedded AvaPlot under the transcendental editor preview")
            window.Close())

    // -- host site: the Materials-bay View panel --------------------------------------------------

    /// Drive the real workbench to the Materials View panel for `search` / `rowId` and assert the
    /// embedded n/k chart host renders its AvaPlot.
    let private assertViewPanelChart (search : string) (rowId : string) (label : string) : unit =
        let materials, samples = freshStores ()
        let window = mountMain (mainWith materials samples)
        clickOn window (Ribbon.UiIds.tab BayNames.materials)
        setText window MaterialsControls.UiIds.searchBox search
        clickOn window (MaterialsControls.UiIds.row rowId)
        clickOn window MaterialsControls.UiIds.viewButton
        Assert.True(isPresent window WorkbenchIds.materialNkChart, $"%s{label}: the View panel must embed the n/k chart host")
        match avaUnder window WorkbenchIds.materialNkChart with
        | Some ava -> Assert.True(rasterizes ava, $"%s{label}: the View-panel embedded chart rendered no image bytes")
        | None -> Assert.Fail($"%s{label}: no embedded AvaPlot under the Materials View panel n/k chart host")
        window.Close()

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Materials View panel embeds the shared AvaPlot for a non-dispersive entry (glass)`` () =
        HeadlessSession.run (fun () ->
            assertViewPanelChart "1.52" (string MaterialIds.glass152.value) "glass (non-dispersive)")

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Materials View panel embeds the shared AvaPlot for a dispersive entry (silicon)`` () =
        HeadlessSession.run (fun () ->
            assertViewPanelChart "Silicon" (string MaterialIds.silicon.value) "silicon (dispersive)")
