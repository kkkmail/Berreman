namespace OpticalConstructor.TestWindows

open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish

/// The "Test — Snap to Beam" window (Spec 0027, task 010): a FuncUI `HostWindow` mounting the pure
/// `SnapToBeamView` MVU loop — rotating the light source re-aims the beam and snaps the downstream
/// elements to it. Opened from the launcher.
type SnapToBeamWindow() as this =
    inherit HostWindow()

    do
        this.Title <- "Test — Snap to Beam"
        this.Width <- OpticalConstructor.Ui.TableScene.canvasWidth.value
        this.Height <- OpticalConstructor.Ui.TableScene.canvasHeight.value + 100.0
        OpticalConstructor.Controls.WindowFit.keepOnScreen this
        Program.mkSimple SnapToBeamView.initSnapToBeam SnapToBeamView.updateSnapToBeam SnapToBeamView.snapToBeamView
        |> Program.withHost this
        |> Program.run
