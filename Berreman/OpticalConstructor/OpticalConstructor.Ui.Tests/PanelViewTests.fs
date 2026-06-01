/// Headless per-panel view test (spec 0024 §0.7 / §U1.7, gate `ui-tests`). Trait
/// `Category=ui-tests` (the `ui-tests` gate runs `--filter Category!=ui-smoke`).
///
/// Mounts the read-only `stack` panel view over the known seed `RootModel` and asserts
/// the selected node's layer rows actually render (AC-U1.2) — proving the MVU read
/// path produces real content, not merely a frame that does not throw.
namespace OpticalConstructor.Ui.Tests

open Avalonia.Controls
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open OpticalConstructor.Ui

module PanelViewTests =

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``stack panel renders the selected node layer rows`` () =
        HeadlessSession.run (fun () ->
            let model = fst Shell.init
            let window = Window()
            window.Content <- Component(fun _ctx -> ConstructionView.stackPanel model.construction)
            window.Show()
            Dispatcher.UIThread.RunJobs()

            let texts =
                window.GetVisualDescendants()
                |> Seq.choose (fun v ->
                    match v with
                    | :? TextBlock as t -> Option.ofObj t.Text
                    | _ -> None)
                |> List.ofSeq

            // The seed stack carries two layers, so at least two "Layer N — ..." rows render.
            let layerRows = texts |> List.filter (fun t -> t.StartsWith "Layer ")
            Assert.True(layerRows.Length >= 2, sprintf "expected >= 2 layer rows, got: %A" texts)
            window.Close())
