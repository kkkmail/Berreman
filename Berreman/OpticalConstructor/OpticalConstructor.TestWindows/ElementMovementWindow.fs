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
        this.Width <- ElementMovementView.canvasWidth
        this.Height <- ElementMovementView.canvasHeight + 100.0
        Program.mkSimple ElementMovementView.init ElementMovementView.update ElementMovementView.view
        |> Program.withHost this
        |> Program.run
