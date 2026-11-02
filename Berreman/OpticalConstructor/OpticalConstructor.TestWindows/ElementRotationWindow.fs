namespace OpticalConstructor.TestWindows

open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish

/// The "Test Optical Element Rotations" window — a FuncUI `HostWindow` that mounts the pure
/// `ElementRotationView` MVU loop (Spec 0027). Opened from the launcher's third button.
type ElementRotationWindow() as this =
    inherit HostWindow()

    do
        this.Title <- "Test — Optical Element Rotations"
        this.Width <- ElementRotationView.elementRotationCanvasWidth
        this.Height <- ElementRotationView.elementRotationCanvasHeight + 84.0
        OpticalConstructor.Controls.WindowFit.keepOnScreen this
        Program.mkSimple ElementRotationView.initElementRotation ElementRotationView.updateElementRotation ElementRotationView.elementRotationView
        |> Program.withHost this
        |> Program.run
