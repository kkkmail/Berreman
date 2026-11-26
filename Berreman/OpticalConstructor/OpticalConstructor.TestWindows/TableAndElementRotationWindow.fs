namespace OpticalConstructor.TestWindows

open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open OpticalConstructor.Ui
open OpticalConstructor.Domain.Kit

/// The "Test Table + Element Rotations" window — a FuncUI `HostWindow` mounting the pure
/// `TableAndElementRotationView` MVU loop (Spec 0027, task 006 #3). Opened from the launcher.
type TableAndElementRotationWindow private (model : TableAndElementRotationView.TableAndElementRotationModel) as this =
    inherit HostWindow()

    do
        this.Title <- "Test — Table + Element Rotations"
        this.Width <- TableAndElementRotationView.tableAndElementRotationCanvasWidth
        this.Height <- TableAndElementRotationView.tableAndElementRotationCanvasHeight + 84.0
        OpticalConstructor.Controls.WindowFit.keepOnScreen this
        Program.mkSimple (fun () -> model) TableAndElementRotationView.updateTableAndElementRotation TableAndElementRotationView.tableAndElementRotationView
        |> Program.withHost this
        |> Program.run

    /// Creates the diagnostic window only after its scratch kit initializes (spec 0050 .manual/010).
    static member tryCreate () : Result<TableAndElementRotationWindow, KitError> =
        TableAndElementRotationView.initTableAndElementRotation ()
        |> Result.map (fun model -> TableAndElementRotationWindow(model))
