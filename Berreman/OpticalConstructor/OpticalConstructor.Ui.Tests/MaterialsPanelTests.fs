/// Headless materials-panel view tests (spec 0024 Part U3 §U3.1–U3.3, gate `ui-tests`).
/// Trait `Category=ui-tests` (the `ui-tests` gate runs `--filter Category!=ui-smoke`).
///
/// Asserts the Part U3 materials panel:
///   • R-1 — the library list renders and a category filter narrows it (through the
///     existing `MaterialLibrary.byCategory` / `byNameContains` seams, no new index).
///   • AC-U3.1 — the dispersion preview routes to the WebView2 host (degrading to the
///     §U1.8 "unavailable" placeholder headlessly), NEVER an `AvaPlot`.
///   • AC-U3.2 — a material drop builds the edit via `StackEditor.layerMaterialDrop` and
///     dispatches `Construction (EditStack (path, SetLayerMaterial …))`, leaving the
///     layer's thickness unchanged. (Real drag gestures cannot be raised headlessly, so
///     the drop is driven through the `MaterialsView.materialDrop` seam the `onDrop`
///     handler wraps — mirroring `ChartPanelTests` testing `ChartView.update` directly.)
namespace OpticalConstructor.Ui.Tests

open System.Collections.Generic
open Avalonia.Controls
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open OpticalConstructor.Domain
open OpticalConstructor.Ui

module MaterialsPanelTests =

    /// The contents of every Button in the live visual tree, as strings.
    let private buttonContents (window : Window) : string list =
        window.GetVisualDescendants()
        |> Seq.choose (fun v -> match v with | :? Button as b -> Some (string b.Content) | _ -> None)
        |> List.ofSeq

    /// The text of every TextBlock in the live visual tree.
    let private textBlocks (window : Window) : string list =
        window.GetVisualDescendants()
        |> Seq.choose (fun v ->
            match v with
            | :? TextBlock as t -> Option.ofObj t.Text
            | _ -> None)
        |> List.ofSeq

    let private mount (content : Avalonia.FuncUI.Types.IView) : Window =
        let window = Window()
        window.Content <- Component(fun _ctx -> content)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``materials panel lists library entries and a category filter narrows it`` () =
        HeadlessSession.run (fun () ->
            let model = fst Shell.init

            // Unfiltered: every library entry renders as a selectable row button — the
            // seed library carries Silicon (a Semiconductor) and the glass presets.
            let all = mount (MaterialsView.materialsPanel model.materials MaterialsView.Filter.empty model.construction ignore ignore)
            let allButtons = buttonContents all
            Assert.Contains("Silicon", allButtons)
            Assert.True(allButtons |> List.exists (fun c -> c.Contains "Transparent glass"),
                        sprintf "expected a glass entry, got: %A" allButtons)
            all.Close()

            // Category = Glass narrows the list through `byCategory`: glass entries show,
            // Silicon does not. The filter button "Glass" is not a material row.
            let glassFilter = { MaterialsView.Filter.empty with category = Some MaterialLibrary.Glass }
            let glass = mount (MaterialsView.materialsPanel model.materials glassFilter model.construction ignore ignore)
            let glassButtons = buttonContents glass
            Assert.DoesNotContain("Silicon", glassButtons)
            Assert.True(glassButtons |> List.exists (fun c -> c.Contains "Transparent glass"),
                        sprintf "expected a glass entry after filtering, got: %A" glassButtons)
            glass.Close())

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``dispersion preview routes to the WebView2 host, never an AvaPlot`` () =
        HeadlessSession.run (fun () ->
            let model = fst Shell.init
            let filter = { MaterialsView.Filter.empty with selected = Some "glass-1.52" }
            let window = mount (MaterialsView.materialsPanel model.materials filter model.construction ignore ignore)

            // AC-U3.1: the preview hosts a Plotly chart through ChartHosts.webView2Host,
            // which degrades to the §U1.8 "unavailable" placeholder headlessly — so the
            // placeholder renders and NO ScottPlot AvaPlot is ever instantiated.
            let texts = textBlocks window
            Assert.True(texts |> List.exists (fun t -> t.Contains "renderer unavailable"),
                        sprintf "expected the WebView2 unavailable placeholder, got: %A" texts)
            let avaPlots =
                window.GetVisualDescendants()
                |> Seq.filter (fun v -> v :? ScottPlot.Avalonia.AvaPlot)
                |> Seq.length
            Assert.Equal(0, avaPlots)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``material drop builds EditStack via layerMaterialDrop with thickness unchanged`` () =
        let model = fst Shell.init
        let construction = model.construction
        let path = construction.selected
        let originalThickness = construction.project.beamTree.root.system.films.[0].thickness

        // AC-U3.2: a drop on layer 0 routes the single seam (layerMaterialDrop) and
        // dispatches Construction (EditStack (path, SetLayerMaterial (0, _))).
        let captured = List<ConstructionPage.Msg>()
        MaterialsView.materialDrop model.materials MaterialsView.referenceWavelength path captured.Add 0 "silicon"

        let edit =
            captured
            |> Seq.tryPick (fun m ->
                match m with
                | ConstructionPage.EditStack (p, (StackEditor.SetLayerMaterial (i, _) as sm)) when p = path && i = 0 -> Some sm
                | _ -> None)
        Assert.True(edit.IsSome, sprintf "expected EditStack(SetLayerMaterial 0), got: %A" (List.ofSeq captured))

        // The frozen update applies the edit, swapping the material but leaving the
        // layer's thickness unchanged (the view resolved nothing itself).
        let updated = ConstructionPage.update (ConstructionPage.EditStack (path, edit.Value)) construction
        Assert.Equal(originalThickness, updated.project.beamTree.root.system.films.[0].thickness)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``materials update routes filter edits and an unknown drop id is a no-op`` () =
        // Pure filter dispatcher (R-1): each edit reaches the Filter record.
        let f0 = MaterialsView.Filter.empty
        let f1 = MaterialsView.update (MaterialsView.SetCategory (Some MaterialLibrary.Glass)) f0
        Assert.Equal(Some MaterialLibrary.Glass, f1.category)
        let f2 = MaterialsView.update (MaterialsView.SelectMaterial "silicon") f1
        Assert.Equal(Some "silicon", f2.selected)

        // filteredEntries reuses byCategory: a Glass filter yields only glass entries.
        let lib = MaterialLibrary.standard
        let glass = MaterialsView.filteredEntries lib f1
        Assert.NotEmpty(glass)
        Assert.True(glass |> List.forall (fun e -> e.category = MaterialLibrary.Glass))

        // An unknown material id resolves to Error, so the drop dispatches nothing.
        let captured = List<ConstructionPage.Msg>()
        MaterialsView.materialDrop lib MaterialsView.referenceWavelength [] captured.Add 0 "no-such-material"
        Assert.Empty(captured)
