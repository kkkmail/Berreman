namespace OpticalConstructor.Controls

open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

/// The reusable RENDERER control (Spec 0027, task 018) — a large control ("Bay") that picks how the
/// optical elements are drawn and tunes "how it looks", so it can be dropped onto any screen (the renderer
/// test AND the Main screen). It carries ONLY the serializable config: the renderer is identified by a DU
/// (`RendererKind`) — the project maps such "function-valued" choices to DU cases so they can be passed /
/// serialized, the actual draw function being looked up from the case in the domain layer — plus the cap /
/// rail / transparency knobs. No domain types here (the drawing lives in `OpticalConstructor.TestWindows`,
/// keyed by this DU + `State`). Behaviour is injected as a `Handlers` function record (our functional-proxy
/// seam), exactly like `RotationControls` / `RayPositionControls`.
module RendererControls =

    /// WHICH renderer draws an element. A DU (not a function) so it is passable / serializable; the draw
    /// function is recovered from the case where the domain geometry is available.
    type RendererKind =
        | Wireframe
        | Shapes

    let nextRenderer (k : RendererKind) : RendererKind =
        match k with
        | Wireframe -> Shapes
        | Shapes -> Wireframe

    let rendererName (k : RendererKind) : string =
        match k with
        | Wireframe -> "Wireframe"
        | Shapes -> "Shapes + codes"

    // ----- the discrete "how it looks" knobs (pure; shared by every screen that draws shapes) -----

    /// The cylinder side-rail count is DISCRETE — these presets only; 72 is the default.
    let railOptions : int list = [ 4; 8; 12; 24; 36; 72 ]
    let defaultRails : int = 72
    let snapRails (n : int) : int = railOptions |> List.minBy (fun o -> abs (o - n))
    let railIndex (n : int) : int = railOptions |> List.findIndex (fun o -> o = snapRails n)
    let railsAtIndex (i : int) : int = List.item (max 0 (min (List.length railOptions - 1) i)) railOptions

    /// Inner latitude CIRCLES on a spherical cap — 1 (default) … 8.
    let capCirclesMin : int = 1
    let capCirclesMax : int = 8
    let defaultCapCircles : int = 1

    /// Meridian RADIALS on a spherical cap — discrete presets, 4 the default.
    let radialOptions : int list = [ 4; 8; 12; 24; 36 ]
    let defaultCapRadials : int = 4
    let snapRadials (n : int) : int = radialOptions |> List.minBy (fun o -> abs (o - n))
    let radialIndex (n : int) : int = radialOptions |> List.findIndex (fun o -> o = snapRadials n)
    let radialsAtIndex (i : int) : int = List.item (max 0 (min (List.length radialOptions - 1) i)) radialOptions

    /// The three transparency defaults (the current look). The yellow / blue normals are never affected.
    let defaultRailOpacity : float = 0.35
    let defaultFaceOpacity : float = 0.85
    let defaultLineOpacity : float = 0.70
    let clamp01 (x : float) : float = max 0.0 (min 1.0 x)

    /// The renderer config the screen draws with — the on-screen knobs, all serializable.
    type State =
        {
            kind : RendererKind
            rails : int
            circles : int
            radials : int
            railOpacity : float
            faceOpacity : float
            lineOpacity : float
        }

    let defaultState : State =
        {
            kind = Wireframe
            rails = defaultRails
            circles = defaultCapCircles
            radials = defaultCapRadials
            railOpacity = defaultRailOpacity
            faceOpacity = defaultFaceOpacity
            lineOpacity = defaultLineOpacity
        }

    // Pure updaters, so a host's `update` just delegates (and the values always stay valid).
    let swap (s : State) : State = { s with kind = nextRenderer s.kind }
    let withRails (n : int) (s : State) : State = { s with rails = snapRails n }
    let withRailsIndex (i : int) (s : State) : State = { s with rails = railsAtIndex i }
    let withCircles (n : int) (s : State) : State = { s with circles = max capCirclesMin (min capCirclesMax n) }
    let withRadials (n : int) (s : State) : State = { s with radials = snapRadials n }
    let withRadialsIndex (i : int) (s : State) : State = { s with radials = radialsAtIndex i }
    let withRailOpacity (v : float) (s : State) : State = { s with railOpacity = clamp01 v }
    let withFaceOpacity (v : float) (s : State) : State = { s with faceOpacity = clamp01 v }
    let withLineOpacity (v : float) (s : State) : State = { s with lineOpacity = clamp01 v }

    /// The behaviour the bar drives — a record of functions injected by the host (the functional-proxy
    /// seam; a test substitutes stubs).
    type Handlers =
        {
            swap : unit -> unit
            setRailsIndex : int -> unit
            setCircles : int -> unit
            setRadialsIndex : int -> unit
            setRailOpacity : float -> unit
            setFaceOpacity : float -> unit
            setLineOpacity : float -> unit
        }

    [<RequireQualifiedAccess>]
    module UiIds =
        let swapRenderer = "RendererSwapButton"
        let readout = "RendererReadout"
        let railsSlider = "RendererRailsSlider"
        let capCirclesSlider = "RendererCapCirclesSlider"
        let capRadialsSlider = "RendererCapRadialsSlider"
        let railOpacitySlider = "RendererRailOpacitySlider"
        let faceOpacitySlider = "RendererFaceOpacitySlider"
        let lineOpacitySlider = "RendererLineOpacitySlider"

    // ----- view helpers -----
    let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private idleBackground = color 232 232 232
    let private idleBorder = color 120 120 120

    let private clickBox (id : string) (label : string) (onClick : unit -> unit) : IView =
        Border.create [
            Border.name id
            Border.background (brush idleBackground)
            Border.borderBrush (brush idleBorder)
            Border.borderThickness 1.0
            Border.cornerRadius (CornerRadius 3.0)
            Border.padding (Thickness(10.0, 5.0))
            Border.margin (Thickness(0.0, 0.0, 8.0, 0.0))
            Border.verticalAlignment VerticalAlignment.Center
            Border.child (TextBlock.create [ TextBlock.text label ])
            Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box label))
        ] :> IView

    let private fixedLabel (text : string) : IView =
        TextBlock.create [ TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.margin (Thickness(14.0, 0.0, 6.0, 0.0)); TextBlock.text text ] :> IView

    let private valueLabel (text : string) : IView =
        TextBlock.create [ TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.margin (Thickness(6.0, 0.0, 0.0, 0.0)); TextBlock.text text ] :> IView

    let private row (children : IView list) : IView =
        StackPanel.create [ StackPanel.orientation Orientation.Horizontal; StackPanel.children children ] :> IView

    /// A DISCRETE slider riding the INDEX of a preset list, snapping to whole ticks.
    let private presetSlider (id : string) (width : float) (options : int list) (value : int) (indexOf : int -> int) (onIndex : int -> unit) : IView =
        Slider.create [
            Slider.name id
            Slider.width width
            Slider.minimum 0.0
            Slider.maximum (float (List.length options - 1))
            Slider.smallChange 1.0
            Slider.largeChange 1.0
            Slider.isSnapToTickEnabled true
            Slider.tickFrequency 1.0
            Slider.tickPlacement TickPlacement.BottomRight
            Slider.verticalAlignment VerticalAlignment.Center
            Slider.value (float (indexOf value))
            Slider.onValueChanged (fun v -> onIndex (int (System.Math.Round v)))
        ] :> IView

    let private intSlider (id : string) (lo : int) (hi : int) (value : int) (onValue : int -> unit) : IView =
        Slider.create [
            Slider.name id
            Slider.width 120.0
            Slider.minimum (float lo)
            Slider.maximum (float hi)
            Slider.smallChange 1.0
            Slider.largeChange 1.0
            Slider.isSnapToTickEnabled true
            Slider.tickFrequency 1.0
            Slider.tickPlacement TickPlacement.BottomRight
            Slider.verticalAlignment VerticalAlignment.Center
            Slider.value (float value)
            Slider.onValueChanged (fun v -> onValue (int (System.Math.Round v)))
        ] :> IView

    let private opacitySlider (id : string) (value : float) (onValue : float -> unit) : IView =
        Slider.create [
            Slider.name id
            Slider.width 110.0
            Slider.minimum 0.0
            Slider.maximum 1.0
            Slider.smallChange 0.05
            Slider.verticalAlignment VerticalAlignment.Center
            Slider.value value
            Slider.onValueChanged onValue
        ] :> IView

    /// The renderer control — swap + the cap / rail / transparency knobs, as a compact self-contained
    /// unit (a Bay's content). The yellow / blue normals are never transparency-controlled.
    let view (state : State) (handlers : Handlers) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 6.0
            StackPanel.children [
                row [
                    clickBox UiIds.swapRenderer "Swap renderer" (fun () -> handlers.swap ())
                    TextBlock.create [ TextBlock.name UiIds.readout; TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.text (sprintf "Renderer: %s" (rendererName state.kind)) ]
                    fixedLabel "Cylinder rails:"
                    presetSlider UiIds.railsSlider 170.0 railOptions state.rails railIndex handlers.setRailsIndex
                    valueLabel (string state.rails)
                ]
                row [
                    fixedLabel "Cap circles:"
                    intSlider UiIds.capCirclesSlider capCirclesMin capCirclesMax state.circles handlers.setCircles
                    valueLabel (string state.circles)
                    fixedLabel "Cap radials:"
                    presetSlider UiIds.capRadialsSlider 150.0 radialOptions state.radials radialIndex handlers.setRadialsIndex
                    valueLabel (string state.radials)
                ]
                row [
                    fixedLabel "Rail opacity:"
                    opacitySlider UiIds.railOpacitySlider state.railOpacity handlers.setRailOpacity
                    valueLabel (sprintf "%.2f" state.railOpacity)
                    fixedLabel "Face opacity:"
                    opacitySlider UiIds.faceOpacitySlider state.faceOpacity handlers.setFaceOpacity
                    valueLabel (sprintf "%.2f" state.faceOpacity)
                    fixedLabel "Line opacity:"
                    opacitySlider UiIds.lineOpacitySlider state.lineOpacity handlers.setLineOpacity
                    valueLabel (sprintf "%.2f" state.lineOpacity)
                ]
            ]
        ] :> IView
