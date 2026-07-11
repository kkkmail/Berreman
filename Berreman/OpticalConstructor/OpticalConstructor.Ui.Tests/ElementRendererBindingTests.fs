namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Media
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Xunit
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Ui

/// Tests for the shared renderer's value-BINDING cue (Spec 0038 step 030). An element's `Drawable` now
/// carries a `bindingState`; an UNBOUND element is drawn with a colourblind-safe PATTERN cue — a dashed
/// outline plus a ghosted (reduced-opacity) fill — while a BOUND element stays solid and a NotBindable
/// kind (lens / mirror) renders normally. Pure coverage of the host derivation, plus headless render
/// proofs of the three cases (one frame each, no throw).
module ElementRendererBindingTests =

    /// A drawable for `kind` bound to `valueId` (its binding state derived exactly as the host derives it).
    let private drawableOf (kind : CatalogueKind) (valueId : string option) : ElementRenderer.Drawable =
        let placement = { ElementPlacement.create kind TablePoint.origin with valueId = valueId }
        { placement = placement
          centre = ElementRenderer.centreOfPlacement placement
          zoom = 3.5
          opticalSign = 0
          bindingState = ElementRenderer.bindingStateOf placement }

    /// Render one drawable through the given renderer kind and collect the realized Avalonia shapes. MUST be
    /// called on the headless UI thread (inside `HeadlessSession.run`).
    let private renderShapes (kind : RendererControls.RendererKind) (e : ElementRenderer.Drawable) : Shape list =
        let renderer = ElementRenderer.rendererOf { RendererControls.defaultState with kind = kind }
        let views = renderer.draw (TableScene.project Table.defaultView) false e
        let window = Window(Width = TableScene.canvasWidth, Height = TableScene.canvasHeight)
        window.Content <- Component(fun _ -> Canvas.create [ Canvas.children views ] :> IView)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        let shapes =
            window.GetVisualDescendants()
            |> Seq.choose (function :? Shape as s -> Some s | _ -> None)
            |> Seq.toList
        window.Close()
        shapes

    /// Whether ANY shape carries a non-empty stroke dash pattern (the unbound outline cue).
    let private hasDashed (shapes : Shape list) : bool =
        shapes |> List.exists (fun s -> not (isNull s.StrokeDashArray) && s.StrokeDashArray.Count > 0)

    /// The strongest solid-fill alpha across the shapes (0..255); a ghosted fill is markedly fainter.
    let private maxFillAlpha (shapes : Shape list) : int =
        match shapes |> List.choose (fun s -> match s.Fill with | :? ISolidColorBrush as b -> Some (int b.Color.A) | _ -> None) with
        | [] -> 0
        | alphas -> List.max alphas

    // ============================ pure host derivation ============================

    [<Fact>]
    let ``bindingStateOf: valueId gates bindable kinds; lens and mirrors are never bindable`` () =
        let stateOf (kind : CatalogueKind) (valueId : string option) : ElementRenderer.BindingState =
            ElementRenderer.bindingStateOf { ElementPlacement.create kind TablePoint.origin with valueId = valueId }
        // Lens / flat / curved mirror are pure geometry — NotBindable regardless of any value id.
        Assert.Equal(ElementRenderer.NotBindableElement, stateOf Lens None)
        Assert.Equal(ElementRenderer.NotBindableElement, stateOf FlatMirror (Some "ignored"))
        Assert.Equal(ElementRenderer.NotBindableElement, stateOf CurvedMirror None)
        // Every other kind is Unbound while its value id is None and Bound once it is Some.
        Assert.Equal(ElementRenderer.UnboundElement, stateOf Sample None)
        Assert.Equal(ElementRenderer.BoundElement, stateOf Sample (Some "lib:sample"))
        Assert.Equal(ElementRenderer.UnboundElement, stateOf LinearPolarizer None)
        Assert.Equal(ElementRenderer.BoundElement, stateOf LinearPolarizer (Some "lib:lp"))
        Assert.Equal(ElementRenderer.UnboundElement, stateOf Detector None)
        Assert.Equal(ElementRenderer.BoundElement, stateOf LightSource (Some "lib:src"))

    // ============================ headless render proofs ============================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the shape renderer draws an unbound element dashed+ghosted, a bound one solid, a lens normally`` () =
        HeadlessSession.run (fun () ->
            let unboundSample = renderShapes RendererControls.Shapes (drawableOf Sample None)
            let boundPolarizer = renderShapes RendererControls.Shapes (drawableOf LinearPolarizer (Some "lib:lp"))
            let lens = renderShapes RendererControls.Shapes (drawableOf Lens None)
            // Unbound → a DASHED outline and a GHOSTED (fainter) fill than a solid element's.
            Assert.True(hasDashed unboundSample, "an unbound element draws a dashed outline")
            Assert.True(maxFillAlpha unboundSample < maxFillAlpha boundPolarizer,
                        $"an unbound fill is ghosted (unbound {maxFillAlpha unboundSample} < bound {maxFillAlpha boundPolarizer})")
            // Bound stays solid; a not-bindable lens renders normally. Neither shows the dash cue.
            Assert.False(hasDashed boundPolarizer, "a bound element stays solid")
            Assert.False(hasDashed lens, "a not-bindable element renders normally")
            // Every case produced a frame's worth of shapes without throwing.
            Assert.NotEmpty(unboundSample)
            Assert.NotEmpty(boundPolarizer)
            Assert.NotEmpty(lens))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the wireframe renderer dashes an unbound element's box edges and leaves a bound one solid`` () =
        HeadlessSession.run (fun () ->
            let unbound = renderShapes RendererControls.Wireframe (drawableOf Sample None)
            let bound = renderShapes RendererControls.Wireframe (drawableOf Sample (Some "lib:sample"))
            Assert.True(hasDashed unbound, "an unbound element's wireframe box is dashed")
            Assert.False(hasDashed bound, "a bound element's wireframe box is solid")
            Assert.NotEmpty(unbound)
            Assert.NotEmpty(bound))
