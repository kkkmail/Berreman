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
        this.Width <- RendererTestView.rendererTestCanvasWidth
        // Three rows of controls now (swap+rails, cap circles/radials, the three transparency knobs).
        this.Height <- RendererTestView.rendererTestCanvasHeight + 150.0
        OpticalConstructor.Controls.WindowFit.keepOnScreen this
        Program.mkSimple RendererTestView.initRendererTest RendererTestView.updateRendererTest RendererTestView.rendererTestView
        |> Program.withHost this
        |> Program.run
