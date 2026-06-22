namespace OpticalConstructor.TestWindows

open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish

/// The "Test Optical Table Rotations" window — a FuncUI `HostWindow` that mounts the pure
/// `TableRotationView` MVU loop (Spec 0027, task 002). Opened from the launcher's `<test>`
/// button; it owns no state of its own (the model is the pure `TableRotationView.Model`),
/// so the same logic the headless tests drive is exactly what renders here.
type TableRotationWindow() as this =
    inherit HostWindow()

    do
        this.Title <- "Test — Optical Table Rotations"
        this.Width <- TableRotationView.canvasWidth
        this.Height <- TableRotationView.canvasHeight + 72.0
        Program.mkSimple TableRotationView.init TableRotationView.update TableRotationView.view
        |> Program.withHost this
        |> Program.run
