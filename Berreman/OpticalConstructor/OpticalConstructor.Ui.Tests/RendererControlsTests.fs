namespace OpticalConstructor.Ui.Tests

open Xunit
open OpticalConstructor.Controls

/// Tests for the SHARED renderer control (Spec 0027, task 018) — the serializable renderer DU + the
/// discrete cap / rail presets + the transparency clamps, now in one reusable place used by both the
/// renderer test and the Main screen. Pure config logic; no headless session.
module RendererControlsTests =

    [<Fact>]
    let ``the default state is the current look`` () =
        let s = RendererControls.defaultState
        Assert.Equal(RendererControls.Wireframe, s.kind)
        Assert.Equal(72, s.rails)
        Assert.Equal(1, s.circles)
        Assert.Equal(4, s.radials)
        Assert.Equal(0.35, s.railOpacity)
        Assert.Equal(0.85, s.faceOpacity)
        Assert.Equal(0.70, s.lineOpacity)

    [<Fact>]
    let ``swap toggles the renderer DU`` () =
        Assert.Equal(RendererControls.Shapes, (RendererControls.swap RendererControls.defaultState).kind)
        Assert.Equal(RendererControls.Wireframe, (RendererControls.defaultState |> RendererControls.swap |> RendererControls.swap).kind)

    [<Fact>]
    let ``rails are the discrete presets; set snaps and the index round-trips`` () =
        Assert.Equal<int list>([ 4; 8; 12; 24; 36; 72 ], RendererControls.railOptions)
        let s = RendererControls.defaultState
        Assert.Equal(4, (RendererControls.withRails 1 s).rails)        // below min snaps to 4
        Assert.Equal(72, (RendererControls.withRails 200 s).rails)     // above max snaps to 72
        Assert.Equal(24, (RendererControls.withRails 20 s).rails)      // 20 → nearest preset 24
        Assert.Equal(4, (RendererControls.withRailsIndex 0 s).rails)
        Assert.Equal(72, (RendererControls.withRailsIndex 99 s).rails) // index clamped
        Assert.Equal(5, RendererControls.railIndex 72)
        Assert.Equal(0, RendererControls.railIndex 4)

    [<Fact>]
    let ``cap circles clamp to [1, 8]; radials snap to their presets`` () =
        Assert.Equal<int list>([ 4; 8; 12; 24; 36 ], RendererControls.radialOptions)
        let s = RendererControls.defaultState
        Assert.Equal(1, (RendererControls.withCircles 0 s).circles)    // clamps up to 1
        Assert.Equal(8, (RendererControls.withCircles 99 s).circles)   // clamps to 8
        Assert.Equal(5, (RendererControls.withCircles 5 s).circles)
        Assert.Equal(4, (RendererControls.withRadials 1 s).radials)    // snaps to 4
        Assert.Equal(36, (RendererControls.withRadials 200 s).radials) // snaps to 36
        Assert.Equal(8, (RendererControls.withRadials 9 s).radials)    // 9 → nearest 8
        Assert.Equal(36, (RendererControls.withRadialsIndex 4 s).radials)

    [<Fact>]
    let ``the three transparency knobs clamp to [0, 1]`` () =
        let s = RendererControls.defaultState
        Assert.Equal(0.0, (RendererControls.withRailOpacity -1.0 s).railOpacity)
        Assert.Equal(1.0, (RendererControls.withFaceOpacity 2.0 s).faceOpacity)
        Assert.Equal(0.5, (RendererControls.withLineOpacity 0.5 s).lineOpacity)
