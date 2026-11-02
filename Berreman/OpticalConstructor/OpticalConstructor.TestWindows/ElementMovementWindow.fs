namespace OpticalConstructor.TestWindows

open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish

/// The "Test — Element Movement" window (Spec 0027, task 010): a FuncUI `HostWindow` mounting the pure
/// `ElementMovementView` MVU loop — a single element the user slides along the beam. Opened from the launcher.
type ElementMovementWindow() as this =
    inherit HostWindow()

    do
        this.Title <- "Test — Element Movement"
        this.Width <- ElementMovementView.elementMovementCanvasWidth
        this.Height <- ElementMovementView.elementMovementCanvasHeight + 100.0
        OpticalConstructor.Controls.WindowFit.keepOnScreen this
        Program.mkSimple ElementMovementView.initElementMovement ElementMovementView.updateElementMovement ElementMovementView.elementMovementView
        |> Program.withHost this
        |> Program.run
