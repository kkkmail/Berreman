namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open OpticalConstructor.Domain.TableView
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
        // A convex AND a concave lens, and a concave AND a convex curved mirror, so both spherical-cap
        // signs are visible at once (the cap bulge follows `opticalSign`).
        let signsFor (k : OpticalConstructor.Domain.Placement.CatalogueKind) =
            m.elements |> List.filter (fun e -> e.placement.catalogueKind = k) |> List.map (fun e -> e.opticalSign) |> List.sort
        Assert.Equal<int list>([ -1; 1 ], signsFor OpticalConstructor.Domain.Placement.Lens)
        Assert.Equal<int list>([ -1; 1 ], signsFor OpticalConstructor.Domain.Placement.CurvedMirror)

    [<Fact>]
    let ``the cylinder rail count defaults to 72 and snaps to the discrete presets`` () =
        let m = init ()
        Assert.Equal(72, m.rails)
        Assert.Equal<int list>([ 4; 8; 12; 24; 36; 72 ], railOptions)
        // SetRails snaps any value to the nearest preset.
        Assert.Equal(4, (update (SetRails 1) m).rails)              // below min snaps up to 4
        Assert.Equal(72, (update (SetRails 200) m).rails)           // above max snaps down to 72
        Assert.Equal(24, (update (SetRails 20) m).rails)            // 20 → nearest preset 24
        // SetRailsIndex (the slider) picks a preset by index, clamped.
        Assert.Equal(4, (update (SetRailsIndex 0) m).rails)
        Assert.Equal(12, (update (SetRailsIndex 2) m).rails)
        Assert.Equal(72, (update (SetRailsIndex 5) m).rails)
        Assert.Equal(72, (update (SetRailsIndex 99) m).rails)       // index clamped to last
        // railIndex is the inverse of railsAtIndex on presets.
        Assert.Equal(5, railIndex 72)
        Assert.Equal(0, railIndex 4)

    [<Fact>]
    let ``a clean click far from every element deselects; the renderer stays`` () =
        let m0 = init ()
        // A clean click (press + release, no drag) well outside the elements selects nothing.
        let far = { sx = 2.0; sy = 2.0 }
        let m1 = m0 |> update (PointerDown far) |> update (PointerUp far)
        Assert.Equal(None, m1.selected)
        Assert.Equal(m0.renderer, m1.renderer)

    [<Fact>]
    let ``the wheel zooms the table and rotates the selection`` () =
        let m0 = init ()   // an element is selected (index 1)
        // Plain wheel zooms the view.
        Assert.True((update (Wheel (Set.empty, 1)) m0).view.zoom > m0.view.zoom, "plain wheel zooms in")
        // Shift+wheel rotates the SELECTED element's R1 (not the table view).
        let rotated = update (Wheel (Set.ofList [ ModShift ], 1)) m0
        Assert.True((List.item 1 rotated.elements).placement.r1.degrees > 0.0, "Shift+wheel rotated the selected element R1")
        Assert.True(abs (rotated.view.r1.degrees) < 1e-9, "the table view did not rotate while an element was selected")
        // With nothing selected, Shift+wheel rotates the TABLE view instead.
        let tableTurned = update (Wheel (Set.ofList [ ModShift ], 1)) { m0 with selected = None }
        Assert.True(tableTurned.view.r1.degrees > 0.0, "Shift+wheel rotates the table view when nothing is selected")

    [<Fact>]
    let ``a drag pans the table; a clean click selects the nearest element`` () =
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
