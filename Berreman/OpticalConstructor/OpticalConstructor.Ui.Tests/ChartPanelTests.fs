/// Headless chart-panel view test (spec 0024 §0.7 / §U1.7, gate `ui-tests`). Trait
/// `Category=ui-tests` (the `ui-tests` gate runs `--filter Category!=ui-smoke`).
///
/// Asserts the Part U5 chart panel: its controls mount over a known chart sub-state,
/// a simulated settings edit dispatches the expected `Chart …` message, a simulated
/// cursor placement dispatches a marker `Chart …` message (AC-U5.2), and the pure
/// `ChartView.update` routes a settings edit into `ChartSettings` and a cursor
/// placement into `Readout.Markers` (AC-U5.1 / AC-U5.2).
namespace OpticalConstructor.Ui.Tests

open Avalonia.Controls
open Avalonia.Interactivity
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open OpticalConstructor.Ui
open OpticalConstructor.Ui.Charts

module ChartPanelTests =

    /// The buttons whose content equals `label`, found in the live visual tree.
    let private buttonByContent (window : Window) (label : string) : Button =
        window.GetVisualDescendants()
        |> Seq.choose (fun v -> match v with | :? Button as b -> Some b | _ -> None)
        |> Seq.find (fun b -> string b.Content = label)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``chart panel mounts and simulated edits dispatch Chart messages`` () =
        HeadlessSession.run (fun () ->
            let captured = System.Collections.Generic.List<ChartView.ChartMsg>()
            let window = Window()
            window.Content <-
                Component(fun _ctx ->
                    ChartView.chartPanel ChartSettings.ChartSettings.defaultValue Readout.Markers.empty captured.Add)
            window.Show()
            Dispatcher.UIThread.RunJobs()

            // Controls mount: the toolbar buttons + at least one hosted chart row render.
            let buttons =
                window.GetVisualDescendants()
                |> Seq.choose (fun v -> match v with | :? Button as b -> Some b | _ -> None)
                |> List.ofSeq
            Assert.NotEmpty(buttons)

            // A settings edit (toggle legend) dispatches the expected Chart message.
            (buttonByContent window "Legend: on").RaiseEvent(RoutedEventArgs(Button.ClickEvent))
            Dispatcher.UIThread.RunJobs()
            Assert.Contains(captured, fun m -> match m with | ChartView.SetShowLegend false -> true | _ -> false)

            // A cursor placement dispatches a marker Chart message (AC-U5.2).
            (buttonByContent window "Mark cursor at peak").RaiseEvent(RoutedEventArgs(Button.ClickEvent))
            Dispatcher.UIThread.RunJobs()
            Assert.Contains(captured, fun m -> match m with | ChartView.PlaceCursor1 _ -> true | _ -> false)

            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``chart update routes settings edits and cursor placement`` () =
        let s0 = ChartSettings.ChartSettings.defaultValue
        let m0 = Readout.Markers.empty

        // A settings edit reaches the ChartSettings record (the plot then applies it
        // via applyToScottPlot — AC-U5.2).
        let s1, _ = ChartView.update (ChartView.SetShowLegend false) (s0, m0)
        Assert.False(s1.showLegend)

        // A cursor placement updates Readout.Markers held in the model (AC-U5.2).
        let _, m1 = ChartView.update (ChartView.PlaceCursor1 (550.0, 0.5)) (s0, m0)
        Assert.Equal(Some (550.0, 0.5), m1.cursor1)
