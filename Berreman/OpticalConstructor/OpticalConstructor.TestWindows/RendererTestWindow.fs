namespace OpticalConstructor.TestWindows

open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish

/// The "Test — Renderers" window (Spec 0027, task 010): a FuncUI `HostWindow` mounting the pure
/// `RendererTestView` MVU loop, where the element renderer can be swapped (wireframe ⇄ shapes+codes).
/// Opened from the launcher.
type RendererTestWindow() as this =
    inherit HostWindow()

    do
        this.Title <- "Test — Renderers"
        this.Width <- RendererTestView.canvasWidth
        this.Height <- RendererTestView.canvasHeight + 84.0
        Program.mkSimple RendererTestView.init RendererTestView.update RendererTestView.view
        |> Program.withHost this
        |> Program.run
