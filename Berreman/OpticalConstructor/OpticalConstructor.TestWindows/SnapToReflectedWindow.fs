namespace OpticalConstructor.TestWindows

open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish

/// The "Test — Snap to Reflected Light" window (Spec 0027, task 012): a FuncUI `HostWindow` mounting the
/// pure `SnapToReflectedView` MVU loop. The scene is a source, a flat mirror (seeded at R2 = 45°) and a
/// detector; the light is REFLECTED off the mirror (never transmitted) and the detector snaps onto the
/// reflected beam. It reuses the shared `TableScene` for the table look / gestures and the shared
/// `RotationControls` bar for the selected element. Opened from the launcher.
type SnapToReflectedWindow() as this =
    inherit HostWindow()

    do
        this.Title <- "Test — Snap to Reflected Light"
        this.Width <- TableScene.canvasWidth
        this.Height <- TableScene.canvasHeight + 110.0
        Program.mkSimple SnapToReflectedView.init SnapToReflectedView.update SnapToReflectedView.view
        |> Program.withHost this
        |> Program.run
