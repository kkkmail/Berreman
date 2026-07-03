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
        this.Width <- ElementRotationView.canvasWidth
        this.Height <- ElementRotationView.canvasHeight + 84.0
        Program.mkSimple ElementRotationView.init ElementRotationView.update ElementRotationView.view
        |> Program.withHost this
        |> Program.run
