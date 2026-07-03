namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open OpticalConstructor.Controls
open OpticalConstructor.Domain.TableView
open OpticalConstructor.TestWindows
open OpticalConstructor.TestWindows.RendererTestView

/// Tests for the renderer test SCENE (Spec 0027). After task 018 the renderer itself (config + draw) is
/// the shared `RendererControls` / `ElementRenderer` (tested separately); this scene just drives it. Pure
/// tests prove the swap / gestures; a headless render proves the shape renderer draws the element codes
/// and that the page exposes the shared renderer control.
module RendererTestTests =

    [<Fact>]
    let ``swapping the renderer toggles between Wireframe and Shapes`` () =
        let m0 = init ()
        Assert.Equal(RendererControls.Wireframe, m0.render.kind)
        let m1 = update SwapRenderer m0
        Assert.Equal(RendererControls.Shapes, m1.render.kind)
        Assert.Equal(RendererControls.Wireframe, (update SwapRenderer m1).render.kind)

    [<Fact>]
    let ``the scene seeds several element kinds, with both lens / mirror cap signs`` () =
        let m = init ()
        Assert.True(List.length m.elements >= 4, "at least a few elements of different kinds")
        Assert.Equal(Some 1, m.selected)
        let signsFor (k : OpticalConstructor.Domain.Placement.CatalogueKind) =
            m.elements |> List.filter (fun e -> e.placement.catalogueKind = k) |> List.map (fun e -> e.opticalSign) |> List.sort
        Assert.Equal<int list>([ -1; 1 ], signsFor OpticalConstructor.Domain.Placement.Lens)
        Assert.Equal<int list>([ -1; 1 ], signsFor OpticalConstructor.Domain.Placement.CurvedMirror)

    [<Fact>]
    let ``a clean click far from every element deselects; the renderer stays`` () =
        let m0 = init ()
        let far = { sx = 2.0; sy = 2.0 }
        let m1 = m0 |> update (PointerDown far) |> update (PointerUp far)
        Assert.Equal(None, m1.selected)
        Assert.Equal(m0.render.kind, m1.render.kind)

    [<Fact>]
    let ``the wheel zooms the table and rotates the selection`` () =
        let m0 = init ()   // an element is selected (index 1)
        Assert.True((update (Wheel (Set.empty, 1)) m0).view.zoom > m0.view.zoom, "plain wheel zooms in")
        let rotated = update (Wheel (Set.ofList [ ModShift ], 1)) m0
        Assert.True((List.item 1 rotated.elements).placement.r1.degrees > 0.0, "Shift+wheel rotated the selected element R1")
        Assert.True(abs (rotated.view.r1.degrees) < 1e-9, "the table view did not rotate while an element was selected")
        let tableTurned = update (Wheel (Set.ofList [ ModShift ], 1)) { m0 with selected = None }
        Assert.True(tableTurned.view.r1.degrees > 0.0, "Shift+wheel rotates the table view when nothing is selected")

    [<Fact>]
    let ``a drag pans the table`` () =
        let m0 = init ()
        let dragged =
            m0
            |> update (PointerDown { sx = 100.0; sy = 100.0 })
            |> update (PointerMove { sx = 160.0; sy = 130.0 })
        Assert.True(abs (dragged.view.panX - 60.0) < 1e-9 && abs (dragged.view.panY - 30.0) < 1e-9, "a drag pans the view")

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the shape renderer draws an element code label (LP) headlessly; the wireframe renderer does not`` () =
        HeadlessSession.run (fun () ->
            let codesUnder (kind : RendererControls.RendererKind) : string list =
                let mutable model = { init () with render = { (init ()).render with kind = kind } }
                let dispatch (m : Msg) = model <- update m model
                let window = Window(Width = 920.0, Height = 740.0)
                window.Content <- Component(fun _ -> view model dispatch)
                window.Show()
                Dispatcher.UIThread.RunJobs()
                let texts =
                    window.GetVisualDescendants()
                    |> Seq.choose (function :? TextBlock as t when not (isNull t.Text) -> Some t.Text | _ -> None)
                    |> Seq.toList
                window.Close()
                texts
            Assert.Contains("LP", codesUnder RendererControls.Shapes)
            Assert.DoesNotContain("LP", codesUnder RendererControls.Wireframe))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the page exposes the SHARED renderer control (rail / cap / transparency sliders)`` () =
        HeadlessSession.run (fun () ->
            let mutable model = init ()
            let dispatch (m : Msg) = model <- update m model
            let window = Window(Width = 920.0, Height = 740.0)
            window.Content <- Component(fun _ -> view model dispatch)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let names =
                window.GetVisualDescendants()
                |> Seq.choose (function :? Control as c when not (isNull c.Name) -> Some c.Name | _ -> None)
                |> Set.ofSeq
            window.Close()
            for id in [ RendererControls.UiIds.swapRenderer; RendererControls.UiIds.railsSlider
                        RendererControls.UiIds.capCirclesSlider; RendererControls.UiIds.capRadialsSlider
                        RendererControls.UiIds.railOpacitySlider; RendererControls.UiIds.faceOpacitySlider
                        RendererControls.UiIds.lineOpacitySlider ] do
                Assert.Contains(id, names))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the renderer test window host opens and renders headlessly`` () =
        HeadlessSession.run (fun () ->
            let window = RendererTestWindow()
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(window.IsVisible)
            window.Close())
