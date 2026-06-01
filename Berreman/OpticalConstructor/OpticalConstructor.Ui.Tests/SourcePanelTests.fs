/// Headless sources-panel view tests (spec 0024 Part U4 §U4.1–U4.2, gate `ui-tests`).
/// Trait `Category=ui-tests` (the `ui-tests` gate runs `--filter Category!=ui-smoke`).
///
/// Asserts the Part U4 sources panel:
///   • R-1 / AC-U4.1 — the field editors render and a source edit routes the frozen
///     `SourceEditorView.update` (through `SourceView.update (Edit …)`), with the live
///     Stokes/ellipse readout refreshing from the updated source.
///   • R-2 / AC-U4.2 — a polarization preset button dispatches `Source (ApplyPreset …)`,
///     `applyPreset` refreshes `liveStokes`/`poincareMarker`, and the panel hosts the
///     ellipse in the AvaPlot adapter while the Poincaré sphere routes to the WebView2
///     adapter (degrading to the §U1.8 placeholder headlessly), NEVER the reverse.
///     (Real text-field edits cannot be raised headlessly, so a field edit is driven
///     through `SourceView.update` directly — mirroring `ChartPanelTests`.)
namespace OpticalConstructor.Ui.Tests

open System.Collections.Generic
open Avalonia.Controls
open Avalonia.Interactivity
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open Berreman.Constants
open Berreman.Fields
open OpticalConstructor.Ui
open OpticalConstructor.Ui.Sources

module SourcePanelTests =

    /// The seed source the `sources` panel edits.
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
    let ``source panel renders editor fields and the live Stokes readout`` () =
        HeadlessSession.run (fun () ->
            let window = mount (SourceView.sourcePanel seedSource ignore)

            // R-1: the numeric field editors render as TextBoxes.
            let textBoxes =
                window.GetVisualDescendants()
                |> Seq.filter (fun v -> v :? TextBox)
                |> Seq.length
            Assert.True(textBoxes >= 5, sprintf "expected >= 5 field editors, got %d" textBoxes)

            // R-2: the live Stokes/Poincaré/ellipse readout renders.
            let texts = textBlocks window
            Assert.True(texts |> List.exists (fun t -> t.StartsWith "Stokes:"),
                        sprintf "expected a live Stokes readout, got: %A" texts)
            Assert.True(texts |> List.exists (fun t -> t.StartsWith "Poincaré:"),
                        sprintf "expected a Poincaré readout, got: %A" texts)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``a source field edit routes SourceEditorView.update and refreshes the readout`` () =
        // AC-U4.1: a field edit reaches the source via the frozen SourceEditorView.update
        // (SourceView.update delegates the Edit case to it), so the wavelength changes.
        let edited = SourceView.update (SourceView.Edit (SourceEditorView.SetWaveLength (OpticalConstructor.Domain.Units.Nanometer, 700.0))) seedSource
        Assert.NotEqual(seedSource.light.waveLength.value / 1.0<meter>, edited.light.waveLength.value / 1.0<meter>)

        // The live readout refreshes: changing ellipticity changes the live Stokes S3.
        let elliptic = SourceView.update (SourceView.Edit (SourceEditorView.SetEllipticity 0.7)) seedSource
        let (StokesVector before) = PolarizationPicker.liveStokes seedSource
        let (StokesVector after) = PolarizationPicker.liveStokes elliptic
        Assert.NotEqual(before.[3], after.[3])

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``a preset button dispatches Source ApplyPreset and applyPreset refreshes the readout`` () =
        HeadlessSession.run (fun () ->
            // AC-U4.2: clicking a preset button dispatches the matching ApplyPreset.
            let captured = List<SourceView.SourceViewMsg>()
            let window = mount (SourceView.sourcePanel seedSource captured.Add)
            (buttonByContent window "RCP").RaiseEvent(RoutedEventArgs(Button.ClickEvent))
            Dispatcher.UIThread.RunJobs()
            Assert.Contains(captured, fun m -> match m with | SourceView.ApplyPreset PolarizationPicker.PresetRCP -> true | _ -> false)
            window.Close()

            // applyPreset (through SourceView.update) refreshes the Poincaré marker:
            // RCP drives the polarization to right-circular, so S3 climbs toward +1.
            let rcp = SourceView.update (SourceView.ApplyPreset PolarizationPicker.PresetRCP) seedSource
            let (_, _, beforeS3) = PolarizationPicker.poincareMarker seedSource
            let (_, _, afterS3) = PolarizationPicker.poincareMarker rcp
            Assert.NotEqual(beforeS3, afterS3))

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``ellipse hosts in AvaPlot and the Poincare sphere routes to the WebView2 host`` () =
        HeadlessSession.run (fun () ->
            let window = mount (SourceView.sourcePanel seedSource ignore)
            let texts = textBlocks window

            // AC-U4.2: exactly ONE WebView2 placeholder — the Poincaré sphere. The
            // ellipse never produces this (WebView2-specific) text, proving it is NOT
            // hosted in WebView2. The webView2Host placeholder reads "3-D / Plotly chart
            // (WebView2) — renderer unavailable" on this TFM (slice 004 carry-over).
            let webviewPlaceholders = texts |> List.filter (fun t -> t.Contains "Plotly chart (WebView2)")
            Assert.Equal(1, webviewPlaceholders.Length)

            // The ellipse routes to scottPlotHost: either a live AvaPlot mounts, or (if
            // the headless platform has no graphics surface) it degrades to the ScottPlot
            // placeholder "Plot — renderer unavailable" — never the WebView2 text. Either
            // way the ellipse is hosted in the AvaPlot adapter, not WebView2.
            let avaPlots =
                window.GetVisualDescendants()
                |> Seq.filter (fun v -> v :? ScottPlot.Avalonia.AvaPlot)
                |> Seq.length
            let scottPlaceholder = texts |> List.exists (fun t -> t.Contains "Plot — renderer unavailable")
            Assert.True(avaPlots >= 1 || scottPlaceholder,
                        sprintf "expected the ellipse hosted via scottPlotHost (AvaPlot or its placeholder), got texts: %A" texts)
            window.Close())
