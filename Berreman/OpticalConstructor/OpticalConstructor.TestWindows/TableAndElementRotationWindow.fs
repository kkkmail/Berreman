namespace OpticalConstructor.TestWindows

open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish

/// The "Test Table + Element Rotations" window — a FuncUI `HostWindow` mounting the pure
/// `TableAndElementRotationView` MVU loop (Spec 0027, task 006 #3). Opened from the launcher.
type TableAndElementRotationWindow() as this =
    inherit HostWindow()

    do
        this.Title <- "Test — Table + Element Rotations"
        this.Width <- TableAndElementRotationView.canvasWidth
        this.Height <- TableAndElementRotationView.canvasHeight + 84.0
        Program.mkSimple TableAndElementRotationView.init TableAndElementRotationView.update TableAndElementRotationView.view
        |> Program.withHost this
        |> Program.run
