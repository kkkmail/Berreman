namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open OpticalConstructor.TestWindows
open OpticalConstructor.TestWindows.RendererTestView

/// Tests for the renderer test window (Spec 0027, task 010): the element renderer is a swappable
/// abstraction. Pure tests prove the swap; a headless render proves the shape renderer draws the
/// element code labels (e.g. "LP" for the linear polarizer) the wireframe renderer does not.
module RendererTestTests =

    [<Fact>]
    let ``swapping the renderer toggles between Wireframe and Shape`` () =
        let m0 = init ()
        Assert.Equal(Wireframe, m0.renderer)
        let m1 = update SwapRenderer m0
        Assert.Equal(Shape, m1.renderer)
        let m2 = update SwapRenderer m1
        Assert.Equal(Wireframe, m2.renderer)

    [<Fact>]
    let ``the scene seeds several element kinds so the codes / shapes are visible`` () =
        let m = init ()
        Assert.True(List.length m.elements >= 4, "at least a few elements of different kinds")
        Assert.Equal(Some 1, m.selected)

    [<Fact>]
    let ``clicking far from every element deselects; the renderer stays`` () =
        let m0 = init ()
        // A point well outside the elements' projected centres (top-left corner) selects nothing.
        let m1 = update (SelectAt { sx = 2.0; sy = 2.0 }) m0
        Assert.Equal(None, m1.selected)
        Assert.Equal(m0.renderer, m1.renderer)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the shape renderer draws an element code label (LP) headlessly; the wireframe renderer does not`` () =
        HeadlessSession.run (fun () ->
            let codesUnder (renderer : RendererKind) : string list =
                let mutable model = { init () with renderer = renderer }
                let dispatch (m : Msg) = model <- update m model
                let window = Window(Width = 820.0, Height = 660.0)
                window.Content <- Component(fun _ -> view model dispatch)
                window.Show()
                Dispatcher.UIThread.RunJobs()
                let texts =
                    window.GetVisualDescendants()
                    |> Seq.choose (function :? TextBlock as t when not (isNull t.Text) -> Some t.Text | _ -> None)
                    |> Seq.toList
                window.Close()
                texts
            Assert.Contains("LP", codesUnder Shape)
            Assert.DoesNotContain("LP", codesUnder Wireframe))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the renderer test window host opens and renders headlessly`` () =
        HeadlessSession.run (fun () ->
            let window = RendererTestWindow()
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(window.IsVisible)
            window.Close())
