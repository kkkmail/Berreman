namespace OpticalConstructor.Ui.Tests

open Xunit
open OpticalConstructor.Controls
open OpticalConstructor.Controls.RendererControls

/// Tests for the SHARED renderer control (Spec 0027, task 018) — the serializable renderer DU + the
/// discrete cap / rail presets + the transparency clamps, now in one reusable place used by both the
/// renderer test and the Main screen. Pure config logic; no headless session.
module RendererControlsTests =

    [<Fact>]
    let ``the default state is the current look`` () =
        let s = RendererControls.defaultState
        Assert.Equal(RendererControls.Wireframe, s.kind)
        Assert.Equal(72, s.rails.value)
        Assert.Equal(1, s.circles.value)
        Assert.Equal(4, s.radials.value)
        Assert.Equal(0.35, s.railOpacity.value)
        Assert.Equal(0.85, s.faceOpacity.value)
        Assert.Equal(0.70, s.lineOpacity.value)

    [<Fact>]
    let ``swap toggles the renderer DU`` () =
        Assert.Equal(RendererControls.Shapes, RendererControls.defaultState.swapped.kind)
        Assert.Equal(RendererControls.Wireframe, RendererControls.defaultState.swapped.swapped.kind)

    [<Fact>]
    let ``rails are the discrete presets; set snaps and the index round-trips`` () =
        Assert.Equal<int list>([ 4; 8; 12; 24; 36; 72 ], RailCount.presets |> List.map (fun r -> r.value))
        let s = RendererControls.defaultState
        Assert.Equal(4, (s.withRails (RailCount 1)).rails.value)        // below min snaps to 4
        Assert.Equal(72, (s.withRails (RailCount 200)).rails.value)     // above max snaps to 72
        Assert.Equal(24, (s.withRails (RailCount 20)).rails.value)      // 20 → nearest preset 24
        Assert.Equal(4, (s.withRailsIndex (RendererPresetIndex 0)).rails.value)
        Assert.Equal(72, (s.withRailsIndex (RendererPresetIndex 99)).rails.value) // index clamped
        Assert.Equal(5, (RailCount 72).presetIndex.value)
        Assert.Equal(0, (RailCount 4).presetIndex.value)

    [<Fact>]
    let ``cap circles clamp to [1, 8]; radials snap to their presets`` () =
        Assert.Equal<int list>([ 4; 8; 12; 24; 36 ], CapRadials.presets |> List.map (fun r -> r.value))
        let s = RendererControls.defaultState
        Assert.Equal(1, (s.withCircles (CapCircles 0)).circles.value)    // clamps up to 1
        Assert.Equal(8, (s.withCircles (CapCircles 99)).circles.value)   // clamps to 8
        Assert.Equal(5, (s.withCircles (CapCircles 5)).circles.value)
        Assert.Equal(4, (s.withRadials (CapRadials 1)).radials.value)    // snaps to 4
        Assert.Equal(36, (s.withRadials (CapRadials 200)).radials.value) // snaps to 36
        Assert.Equal(8, (s.withRadials (CapRadials 9)).radials.value)    // 9 → nearest 8
        Assert.Equal(36, (s.withRadialsIndex (RendererPresetIndex 4)).radials.value)

    [<Fact>]
    let ``the three transparency knobs clamp to [0, 1]`` () =
        let s = RendererControls.defaultState
        Assert.Equal(0.0, (s.withRailOpacity (RendererOpacity -1.0)).railOpacity.value)
        Assert.Equal(1.0, (s.withFaceOpacity (RendererOpacity 2.0)).faceOpacity.value)
        Assert.Equal(0.5, (s.withLineOpacity (RendererOpacity 0.5)).lineOpacity.value)
