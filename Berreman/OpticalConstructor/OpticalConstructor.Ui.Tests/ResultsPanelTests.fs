/// Headless results-panel view tests (spec 0024 Part U6 §U6.1–U6.2, gate `ui-tests`).
/// Trait `Category=ui-tests` (the `ui-tests` gate runs `--filter Category!=ui-smoke`).
///
/// Asserts the Part U6 results panel:
///   • R-1 / AC-U6.1 — the schematic band layout (`Schematic.layout`) and the ray
///     overlay (`Schematic.rayGeometry`) render on the slice-002 Canvas host: the bands
///     as `Rectangle`s and the incident/reflected/transmitted ray as `Line`s on a
///     `Canvas`.
///   • R-2 / AC-U6.2 — a system's visibility toggle dispatches `Workspace (ToggleVisible
///     …)` (routed through the frozen `Workspace.update`), the visible set changes, and
///     the comparison overlay (`Workspace.renderOverlay`) hosts in the AvaPlot adapter
///     (a live `AvaPlot` or its §U1.8 placeholder), never a WebView2 host.
///     (Real text-field edits cannot be raised headlessly, so the reducer refresh is
///     checked through `Workspace.update` directly — mirroring `SourcePanelTests`.)
namespace OpticalConstructor.Ui.Tests

open System.Collections.Generic
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Interactivity
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open OpticalConstructor.Ui

module ResultsPanelTests =

    /// The seed workspace + source the `results` panel renders.
    let private seedWorkspace = (fst Shell.init).workspace
    let private seedSource = (fst Shell.init).source

    let private mount (content : Avalonia.FuncUI.Types.IView) : Window =
        let window = Window()
        window.Content <- Component(fun _ctx -> content)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window

    let private textBlocks (window : Window) : string list =
        window.GetVisualDescendants()
        |> Seq.choose (fun v -> match v with | :? TextBlock as t -> Option.ofObj t.Text | _ -> None)
        |> List.ofSeq

    let private buttonByContent (window : Window) (label : string) : Button =
        window.GetVisualDescendants()
        |> Seq.choose (fun v -> match v with | :? Button as b -> Some b | _ -> None)
        |> Seq.find (fun b -> string b.Content = label)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``results panel renders the schematic bands and ray overlay on a Canvas`` () =
        HeadlessSession.run (fun () ->
            let window = mount (ResultsView.resultsPanel seedWorkspace seedSource ignore)
            let descendants = window.GetVisualDescendants() |> List.ofSeq

            // R-1 / AC-U6.1: the schematic is hosted on a Canvas.
            let canvases = descendants |> List.filter (fun v -> v :? Canvas) |> List.length
            Assert.True(canvases >= 1, "expected the schematic hosted on a Canvas")

            // The band layout renders as Rectangles. The seed (bandpass filter) active
            // system has five films + the incident/exit half-spaces + a substrate band,
            // so at least eight band rectangles render.
            let rects = descendants |> List.filter (fun v -> v :? Rectangle) |> List.length
            Assert.True(rects >= 8, sprintf "expected >= 8 schematic band rectangles, got %d" rects)

            // The ray overlay renders as the three ray Lines (incident/reflected/transmitted).
            let lines = descendants |> List.filter (fun v -> v :? Line) |> List.length
            Assert.True(lines >= 3, sprintf "expected >= 3 ray lines, got %d" lines)
            let texts = textBlocks window
            Assert.Contains(texts, fun t -> t.Contains "Primary detector #1")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``toggling a system visibility dispatches Workspace and refreshes the overlay`` () =
        HeadlessSession.run (fun () ->
            // AC-U6.2: clicking the visibility toggle dispatches the matching ToggleVisible.
            let captured = List<Workspace.Msg>()
            let window = mount (ResultsView.resultsPanel seedWorkspace seedSource captured.Add)
            // The seed workspace starts with system 0 visible, so its toggle reads "Visible".
            (buttonByContent window "Visible").RaiseEvent(RoutedEventArgs(Button.ClickEvent))
            Dispatcher.UIThread.RunJobs()
            Assert.Contains(captured, fun m -> match m with | Workspace.ToggleVisible 0 -> true | _ -> false)

            // The overlay routes to the AvaPlot host (scottPlotHost): either a live AvaPlot
            // mounts, or it degrades to the ScottPlot "Plot — renderer unavailable"
            // placeholder — never the WebView2 text. Either way the overlay is hosted in
            // the AvaPlot adapter, not WebView2 (Non-requirements).
            let texts = textBlocks window
            let avaPlots =
                window.GetVisualDescendants()
                |> Seq.filter (fun v -> v :? ScottPlot.Avalonia.AvaPlot)
                |> Seq.length
            let scottPlaceholder = texts |> List.exists (fun t -> t.Contains "Plot — renderer unavailable")
            Assert.True(avaPlots >= 1 || scottPlaceholder,
                        sprintf "expected the overlay hosted via scottPlotHost (AvaPlot or its placeholder), got texts: %A" texts)
            Assert.DoesNotContain(texts, fun (t : string) -> t.Contains "Plotly chart (WebView2)")
            window.Close()

            // The overlay refreshes through the frozen Workspace.update: toggling system 0
            // changes the visible set the overlay is built from (AC-U6.2).
            let toggled = Workspace.update (Workspace.ToggleVisible 0) seedWorkspace
            Assert.NotEqual<int list>(Workspace.visibleIndices seedWorkspace, Workspace.visibleIndices toggled))
