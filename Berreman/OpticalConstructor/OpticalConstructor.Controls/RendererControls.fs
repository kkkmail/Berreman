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

        /// The other renderer: what Swap turns this one into.
        member this.next : RendererKind =
            match this with
            | Wireframe -> Shapes
            | Shapes -> Wireframe

        /// The renderer's name, as the bay's readout shows it.
        member this.text : string =
            match this with
            | Wireframe -> "Wireframe"
            | Shapes -> "Shapes + codes"

    // ----- the discrete "how it looks" knobs (pure; shared by every screen that draws shapes) -----

    /// A tick of a preset slider: the position of a value in its preset list.
    type RendererPresetIndex =
        | RendererPresetIndex of int

        member this.value = let (RendererPresetIndex i) = this in i

        /// The preset at this tick, the tick clamped to the list.
        member this.pick<'Preset> (presets : 'Preset list) : 'Preset =
            List.item (max 0 (min (List.length presets - 1) this.value)) presets

        /// The tick of `preset` in `presets`.
        static member ofPreset<'Preset when 'Preset : equality> (presets : 'Preset list) (preset : 'Preset) : RendererPresetIndex =
            RendererPresetIndex (List.findIndex (fun candidate -> candidate = preset) presets)

    /// The cylinder side-rail count. It is DISCRETE — the presets only; 72 is the default.
    type RailCount =
        | RailCount of int

        member this.value = let (RailCount n) = this in n

        static member presets : RailCount list = [ 4; 8; 12; 24; 36; 72 ] |> List.map RailCount

        /// The preset nearest this count.
        member this.snapped : RailCount = RailCount.presets |> List.minBy (fun preset -> abs (preset.value - this.value))

        /// The tick of the nearest preset.
        member this.presetIndex : RendererPresetIndex = RendererPresetIndex.ofPreset RailCount.presets this.snapped

        static member atPresetIndex (index : RendererPresetIndex) : RailCount = index.pick RailCount.presets

        /// The text the bay shows beside its slider.
        member this.valueCaption : ControlCaption = ControlCaption (string this.value)

    /// Inner latitude CIRCLES on a spherical cap — 1 (default) … 8.
    type CapCircles =
        | CapCircles of int

        member this.value = let (CapCircles n) = this in n

        static member fewest : CapCircles = CapCircles 1
        static member most : CapCircles = CapCircles 8

        /// This count, clamped to `fewest` … `most`.
        member this.clamped : CapCircles = CapCircles (max CapCircles.fewest.value (min CapCircles.most.value this.value))

        member this.valueCaption : ControlCaption = ControlCaption (string this.value)

    /// Meridian RADIALS on a spherical cap — discrete presets, 4 the default.
    type CapRadials =
        | CapRadials of int

        member this.value = let (CapRadials n) = this in n

        static member presets : CapRadials list = [ 4; 8; 12; 24; 36 ] |> List.map CapRadials

        /// The preset nearest this count.
        member this.snapped : CapRadials = CapRadials.presets |> List.minBy (fun preset -> abs (preset.value - this.value))

        /// The tick of the nearest preset.
        member this.presetIndex : RendererPresetIndex = RendererPresetIndex.ofPreset CapRadials.presets this.snapped

        static member atPresetIndex (index : RendererPresetIndex) : CapRadials = index.pick CapRadials.presets

        member this.valueCaption : ControlCaption = ControlCaption (string this.value)

    /// How opaque a drawn part is: 0 (invisible) … 1 (solid). The yellow / blue normals are never affected.
    type RendererOpacity =
        | RendererOpacity of float

        member this.value = let (RendererOpacity o) = this in o

        /// This opacity, clamped to 0 … 1.
        member this.clamped : RendererOpacity = RendererOpacity (max 0.0 (min 1.0 this.value))

        /// Two decimals: the text the bay shows beside its slider.
        member this.valueCaption : ControlCaption = ControlCaption $"%.2f{this.value}"

    /// The renderer config the screen draws with — the on-screen knobs, all serializable.
    type RendererControlsState =
        {
            kind : RendererKind
            rails : RailCount
            circles : CapCircles
            radials : CapRadials
            railOpacity : RendererOpacity
            faceOpacity : RendererOpacity
            lineOpacity : RendererOpacity
        }

        // Pure updaters, so a host's `update` just delegates (and the values always stay valid).
        member this.swapped : RendererControlsState = { this with kind = this.kind.next }
        member this.withRails (rails : RailCount) : RendererControlsState = { this with rails = rails.snapped }
        member this.withRailsIndex (index : RendererPresetIndex) : RendererControlsState = { this with rails = RailCount.atPresetIndex index }
        member this.withCircles (circles : CapCircles) : RendererControlsState = { this with circles = circles.clamped }
        member this.withRadials (radials : CapRadials) : RendererControlsState = { this with radials = radials.snapped }
        member this.withRadialsIndex (index : RendererPresetIndex) : RendererControlsState = { this with radials = CapRadials.atPresetIndex index }
        member this.withRailOpacity (opacity : RendererOpacity) : RendererControlsState = { this with railOpacity = opacity.clamped }
        member this.withFaceOpacity (opacity : RendererOpacity) : RendererControlsState = { this with faceOpacity = opacity.clamped }
        member this.withLineOpacity (opacity : RendererOpacity) : RendererControlsState = { this with lineOpacity = opacity.clamped }

    let defaultState : RendererControlsState =
        {
            kind = Wireframe
            rails = RailCount 72
            circles = CapCircles 1
            radials = CapRadials 4
            railOpacity = RendererOpacity 0.35
            faceOpacity = RendererOpacity 0.85
            lineOpacity = RendererOpacity 0.70
        }

    /// The behaviour the bar drives — a record of functions injected by the host (the functional-proxy
    /// seam; a test substitutes stubs).
    type RendererControlsHandlers =
        {
            swap : unit -> unit
            setRailsIndex : RendererPresetIndex -> unit
            setCircles : CapCircles -> unit
            setRadialsIndex : RendererPresetIndex -> unit
            setRailOpacity : RendererOpacity -> unit
            setFaceOpacity : RendererOpacity -> unit
            setLineOpacity : RendererOpacity -> unit
        }

    // ----- view helpers -----
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private idleBackground = Color.FromRgb(232uy, 232uy, 232uy)
    let private idleBorder = Color.FromRgb(120uy, 120uy, 120uy)

    let private clickBox (id : ConstructorUiId) (caption : ControlCaption) (onClick : unit -> unit) : IView =
        Border.create [
            Border.name id.value
            Border.background (brush idleBackground)
            Border.borderBrush (brush idleBorder)
            Border.borderThickness 1.0
            Border.cornerRadius (CornerRadius 3.0)
            Border.padding (Thickness(10.0, 5.0))
            Border.margin (Thickness(0.0, 0.0, 8.0, 0.0))
            Border.verticalAlignment VerticalAlignment.Center
            Border.child (TextBlock.create [ TextBlock.text caption.value ])
            Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box caption.value))
        ] :> IView

    /// One labelled knob: its label, the knob, and the knob's current value beside it.
    let private knobUnit (label : ControlCaption) (knob : IView) (value : ControlCaption) : IView list =
        [ TextBlock.create [ TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.margin (Thickness(14.0, 0.0, 6.0, 0.0)); TextBlock.text label.value ] :> IView
          knob
          TextBlock.create [ TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.margin (Thickness(6.0, 0.0, 0.0, 0.0)); TextBlock.text value.value ] :> IView ]

    /// Spec 0046: a row WRAPS its labelled units (a label + its knob + its value stay together) onto a new
    /// line in a narrow window, instead of running the knobs off its right edge.
    let private row (units : IView list list) : IView =
        WrapPanel.create [
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.children (units |> List.map (fun unit -> StackPanel.create [ StackPanel.orientation Orientation.Horizontal; StackPanel.children unit ] :> IView))
        ] :> IView

    /// The bay's two preset knobs: the slider each one is, how wide, and how many ticks it has.
    type private PresetKnob =
        | RailsKnob
        | RadialsKnob

        member this.id : ConstructorUiId =
            match this with
            | RailsKnob -> ConstructorUiId.rendererRailsSlider
            | RadialsKnob -> ConstructorUiId.rendererCapRadialsSlider

        member this.width : float =
            match this with
            | RailsKnob -> 170.0
            | RadialsKnob -> 150.0

        member this.presetCount : int =
            match this with
            | RailsKnob -> List.length RailCount.presets
            | RadialsKnob -> List.length CapRadials.presets

    /// A DISCRETE slider riding the INDEX of a preset list, snapping to whole ticks.
    let private presetSlider (knob : PresetKnob) (current : RendererPresetIndex) (onIndex : RendererPresetIndex -> unit) : IView =
        Slider.create [
            Slider.name knob.id.value
            Slider.width knob.width
            Slider.minimum 0.0
            Slider.maximum (float (knob.presetCount - 1))
            Slider.smallChange 1.0
            Slider.largeChange 1.0
            Slider.isSnapToTickEnabled true
            Slider.tickFrequency 1.0
            Slider.tickPlacement TickPlacement.BottomRight
            Slider.verticalAlignment VerticalAlignment.Center
            Slider.value (float current.value)
            Slider.onValueChanged (fun v -> onIndex (RendererPresetIndex (int (System.Math.Round v))))
        ] :> IView

    let private circlesSlider (circles : CapCircles) (onCircles : CapCircles -> unit) : IView =
        Slider.create [
            Slider.name ConstructorUiId.rendererCapCirclesSlider.value
            Slider.width 120.0
            Slider.minimum (float CapCircles.fewest.value)
            Slider.maximum (float CapCircles.most.value)
            Slider.smallChange 1.0
            Slider.largeChange 1.0
            Slider.isSnapToTickEnabled true
            Slider.tickFrequency 1.0
            Slider.tickPlacement TickPlacement.BottomRight
            Slider.verticalAlignment VerticalAlignment.Center
            Slider.value (float circles.value)
            Slider.onValueChanged (fun v -> onCircles (CapCircles (int (System.Math.Round v))))
        ] :> IView

    let private opacitySlider (id : ConstructorUiId) (opacity : RendererOpacity) (onOpacity : RendererOpacity -> unit) : IView =
        Slider.create [
            Slider.name id.value
            Slider.width 110.0
            Slider.minimum 0.0
            Slider.maximum 1.0
            Slider.smallChange 0.05
            Slider.verticalAlignment VerticalAlignment.Center
            Slider.value opacity.value
            Slider.onValueChanged (fun v -> onOpacity (RendererOpacity v))
        ] :> IView

    /// The renderer control — swap + the cap / rail / transparency knobs, as a compact self-contained
    /// unit (a Bay's content). The yellow / blue normals are never transparency-controlled.
    let rendererControlsView (state : RendererControlsState) (handlers : RendererControlsHandlers) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 6.0
            StackPanel.children [
                row [
                    [ clickBox ConstructorUiId.rendererSwapButton (ControlCaption "Swap renderer") (fun () -> handlers.swap ())
                      TextBlock.create [ TextBlock.name ConstructorUiId.rendererReadout.value; TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.text $"Renderer: {state.kind.text}" ] ]
                    knobUnit (ControlCaption "Cylinder rails:") (presetSlider RailsKnob state.rails.presetIndex handlers.setRailsIndex) state.rails.valueCaption
                ]
                row [
                    knobUnit (ControlCaption "Cap circles:") (circlesSlider state.circles handlers.setCircles) state.circles.valueCaption
                    knobUnit (ControlCaption "Cap radials:") (presetSlider RadialsKnob state.radials.presetIndex handlers.setRadialsIndex) state.radials.valueCaption
                ]
                row [
                    knobUnit (ControlCaption "Rail opacity:") (opacitySlider ConstructorUiId.rendererRailOpacitySlider state.railOpacity handlers.setRailOpacity) state.railOpacity.valueCaption
                    knobUnit (ControlCaption "Face opacity:") (opacitySlider ConstructorUiId.rendererFaceOpacitySlider state.faceOpacity handlers.setFaceOpacity) state.faceOpacity.valueCaption
                    knobUnit (ControlCaption "Line opacity:") (opacitySlider ConstructorUiId.rendererLineOpacitySlider state.lineOpacity handlers.setLineOpacity) state.lineOpacity.valueCaption
                ]
            ]
        ] :> IView
