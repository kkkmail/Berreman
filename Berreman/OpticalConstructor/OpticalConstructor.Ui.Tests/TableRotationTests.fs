namespace OpticalConstructor.Ui.Tests

open Avalonia.Controls
open Avalonia.Threading
open Avalonia.FuncUI
open Avalonia.FuncUI.Types
open Xunit
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.TableView
open OpticalConstructor.TestWindows
open OpticalConstructor.TestWindows.TableRotationView

/// Tests for the first diagnostic test window (Spec 0027, task 002-rotate-table): the pure
/// MVU `update` proves the rotation steps, the reset, and select / unselect WITHOUT a window
/// (the "headless carcass" the task asks for); two `ui-smoke` cases then mount the real FuncUI
/// view and window host headlessly to prove the projection actually renders. The on-screen UI
/// test is the user's final manual step.
module TableRotationTests =

    let private close (a : float) (b : float) : bool = abs (a - b) <= 1.0e-9

    // --- Pure MVU: rotations (the thing that was broken) -------------------------------

    [<Fact>]
    let ``init starts straight top-down and unselected`` () =
        let m = TableRotationView.init ()
        Assert.True(close m.view.r1.degrees 0.0 && close m.view.r2.degrees 0.0 && close m.view.r3.degrees 0.0)
        Assert.Equal(TableUnselected, m.selection)

    [<Fact>]
    let ``each rotation message advances its own view axis by the configured step`` () =
        let init = TableRotationView.init ()
        let r1 = TableRotationView.update (RotateR1 1) init
        let r2 = TableRotationView.update (RotateR2 -1) init
        let r3 = TableRotationView.update (RotateR3 2) init
        Assert.True(close r1.view.r1.degrees rotationStepDegrees, $"R1 = {r1.view.r1.degrees}")
        Assert.True(close r2.view.r2.degrees (-rotationStepDegrees), $"R2 = {r2.view.r2.degrees}")
        Assert.True(close r3.view.r3.degrees (2.0 * rotationStepDegrees), $"R3 = {r3.view.r3.degrees}")
        // Rotating one axis leaves the others untouched.
        Assert.True(close r1.view.r2.degrees 0.0 && close r1.view.r3.degrees 0.0)

    [<Fact>]
    let ``reset returns a tumbled view to straight top-down`` () =
        let tumbled =
            TableRotationView.init ()
            |> TableRotationView.update (RotateR1 2)
            |> TableRotationView.update (RotateR2 1)
            |> TableRotationView.update (RotateR3 -3)
        let reset = TableRotationView.update ResetView tumbled
        Assert.True(close reset.view.r1.degrees 0.0 && close reset.view.r2.degrees 0.0 && close reset.view.r3.degrees 0.0)

    // --- Pure MVU: select / unselect (AC-C3) -------------------------------------------

    [<Fact>]
    let ``clicking the table selects it and clicking empty space unselects it`` () =
        let m0 = TableRotationView.init ()
        let selected = TableRotationView.update (ClickAt TableRotationView.center) m0
        Assert.Equal(TableSelected, selected.selection)
        let far : ScreenPoint = { sx = TableRotationView.center.sx + 100000.0; sy = TableRotationView.center.sy }
        let unselected = TableRotationView.update (ClickAt far) selected
        Assert.Equal(TableUnselected, unselected.selection)

    [<Fact>]
    let ``the table stays selectable after it is tumbled in 3-D`` () =
        let m =
            TableRotationView.init ()
            |> TableRotationView.update (RotateR1 2)
            |> TableRotationView.update (RotateR2 1)
            |> TableRotationView.update (RotateR3 1)
        // The centroid of the projected top face is inside the projected plate by convexity.
        let face =
            TableView.plateTopFace m.table
            |> List.map (TableView.project TableRotationView.pixelsPerMeter TableRotationView.center m.view)
        let pt : ScreenPoint =
            { sx = face |> List.averageBy (fun p -> p.sx)
              sy = face |> List.averageBy (fun p -> p.sy) }
        let selected = TableRotationView.update (ClickAt pt) m
        Assert.Equal(TableSelected, selected.selection)

    // --- Headless render proof (ui-smoke) ----------------------------------------------

    let private renderInWindow (content : IView) : unit =
        let window = Window()
        window.Content <- Component(fun _ctx -> content)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        Assert.True(window.IsVisible)
        window.Close()

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the table rotation view renders a frame top-down, tumbled, and selected`` () =
        HeadlessSession.run (fun () ->
            // straight top-down
            renderInWindow (TableRotationView.view (TableRotationView.init ()) ignore)
            // tumbled in all three axes, with the table selected
            let tumbledSelected =
                TableRotationView.init ()
                |> TableRotationView.update (RotateR1 2)
                |> TableRotationView.update (RotateR2 1)
                |> TableRotationView.update (RotateR3 1)
                |> TableRotationView.update (ClickAt TableRotationView.center)
            renderInWindow (TableRotationView.view tumbledSelected ignore))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the table rotation window host opens and renders headlessly`` () =
        HeadlessSession.run (fun () ->
            let window = TableRotationWindow()
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(window.IsVisible)
            window.Close())
