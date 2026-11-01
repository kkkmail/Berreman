namespace OpticalConstructor.Controls

open System
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

/// Spec 0046: lays its one child out at the width it is ARRANGED to — so wrapping content wraps at the
/// window's own width — but never narrower than `MinContentWidth`. Inside a horizontally scrolling
/// `ScrollViewer` (which measures its content at an unbounded width) it reports exactly that minimum as its
/// desired width, so the viewer arranges it at the viewport's width, and scrolls horizontally only when the
/// window is narrower than the minimum. Its desired height is its child's height AT the arranged width (an
/// arrange at a new width re-measures the child and, when that height changed, measures again), so a
/// vertically scrolling viewer's extent always holds all of the content — nothing is ever unreachable.
type MinWidthPanel() =
    inherit Decorator()

    static let minContentWidthProperty : StyledProperty<float> =
        AvaloniaProperty.Register<MinWidthPanel, float>("MinContentWidth", 0.0)

    /// The width the child was last laid out at (0 before the first arrange).
    let mutable layoutWidth : float = 0.0
    /// The height constraint of the last measure pass, reused when an arrange re-measures the child.
    let mutable heightConstraint : float = Double.PositiveInfinity

    static member MinContentWidthProperty : StyledProperty<float> = minContentWidthProperty

    member this.MinContentWidth
        with get () : float = this.GetValue minContentWidthProperty
        and set (value : float) = this.SetValue(minContentWidthProperty, value) |> ignore

    override this.MeasureOverride(available : Size) : Size =
        let minimum = this.MinContentWidth
        let unbounded = Double.IsInfinity available.Width
        let width = if unbounded then max minimum layoutWidth else max minimum available.Width
        heightConstraint <- available.Height
        match this.Child with
        | null -> Size(minimum, 0.0)
        | child ->
            child.Measure(Size(width, available.Height))
            Size((if unbounded then minimum else width), child.DesiredSize.Height)

    override this.ArrangeOverride(final : Size) : Size =
        match this.Child with
        | null -> final
        | child ->
            if abs (final.Width - layoutWidth) > 0.5 then
                layoutWidth <- final.Width
                let before = child.DesiredSize.Height
                child.Measure(Size(final.Width, heightConstraint))
                if abs (child.DesiredSize.Height - before) > 0.5 then this.InvalidateMeasure()
            child.Arrange(Rect final)
            final

/// The FuncUI DSL for `MinWidthPanel`.
[<RequireQualifiedAccess>]
module MinWidthPanel =
    let create (attrs : IAttr<MinWidthPanel> list) : IView<MinWidthPanel> = ViewBuilder.Create<MinWidthPanel> attrs

    let minContentWidth<'t when 't :> MinWidthPanel> (width : float) : IAttr<'t> =
        AttrBuilder<'t>.CreateProperty<float>(MinWidthPanel.MinContentWidthProperty, width, ValueNone)

    let child<'t when 't :> MinWidthPanel> (content : IView) : IAttr<'t> =
        AttrBuilder<'t>.CreateContentSingle(Decorator.ChildProperty, Some content)

/// Spec 0046: every window fits the screen it opens on. A window asks for a size (its design size); when
/// that is larger than the screen's working area (a laptop at 125–175 % scaling has as little as ~600 px of
/// height) it is shrunk to fit and moved inside the area, so no part of it — the bottom of its scrolling
/// content included — is ever off-screen. Its content then scrolls (`MinWidthPanel`, `ScrollViewer`).
module WindowFit =

    /// PURE: the client size and frame position that keep a window inside a screen's working area.
    /// `workArea` and `position` are device pixels; `client` and `decoration` (the frame around the client
    /// area: title bar, borders) are device-independent pixels; `scaling` is device pixels per DIP.
    let fitWithin (workArea : PixelRect) (scaling : float) (decoration : Size) (position : PixelPoint) (client : Size) : Size * PixelPoint =
        let available = Size(float workArea.Width / scaling - decoration.Width, float workArea.Height / scaling - decoration.Height)
        let fitted = Size(max 1.0 (min client.Width available.Width), max 1.0 (min client.Height available.Height))
        let frameOf (length : float) (frame : float) : int = int (ceil ((length + frame) * scaling))
        let clampAxis (at : int) (start : int) (length : int) (frame : int) : int = max start (min at (start + length - frame))
        let x = clampAxis position.X workArea.X workArea.Width (frameOf fitted.Width decoration.Width)
        let y = clampAxis position.Y workArea.Y workArea.Height (frameOf fitted.Height decoration.Height)
        fitted, PixelPoint(x, y)

    /// Fit `window` to its screen's working area each time it opens (IO edge: the platform's screen and
    /// frame queries; an unavailable screen — a headless host without one — leaves the window as it is).
    let keepOnScreen (window : Window) : unit =
        window.Opened.Add(fun _ ->
            try
                match Option.ofObj window.Screens with
                | None -> ()
                | Some screens ->
                    match Option.ofObj (screens.ScreenFromWindow window) |> Option.orElse (Option.ofObj screens.Primary) with
                    | None -> ()
                    | Some screen ->
                        let client = window.ClientSize
                        let frame = window.FrameSize |> Option.ofNullable |> Option.defaultValue client
                        let decoration = Size(max 0.0 (frame.Width - client.Width), max 0.0 (frame.Height - client.Height))
                        let fitted, position = fitWithin screen.WorkingArea screen.Scaling decoration window.Position client
                        if fitted <> client then
                            window.Width <- fitted.Width
                            window.Height <- fitted.Height
                        if position <> window.Position then window.Position <- position
            with _ -> ())
