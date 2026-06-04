# Architecture critique -- 006.slice-md cycle 1

## Summary

Clean slice. `ResultsView.fs` renders R-1 (schematic + ray on the Canvas
host) and R-2 (multi-system overlay on the AvaPlot host) entirely through the
frozen `Schematic` / `Workspace` / `ChartHosts` seams, recomputing no geometry
and re-implementing no reducer, and it follows the slice-002..005 sub-state +
dispatch view precedent exactly. The only findings are minor: a couple of dead
`open`s and a one-line `FixedInfo` lift duplicated from `SynthesisFitPage` --
nothing that should hold the slice.

## Consistency

The module shape is right: `resultsPanel` takes `(Workspace.Model, SourceSpec,
Workspace.Msg -> unit)` rather than `RootModel`, mirroring
`SourceView.sourcePanel` / `MaterialsView.materialsPanel` and avoiding the
`Shell` cycle, and `Shell.fs:260` wires it with `RootMsg.Workspace >> dispatch`
in the same form as the sibling panels. The `module WS =` alias, the
`ChartHosts` reuse, the `[<Literal>]` layout constants, and the headless test
structure (`HeadlessSession.run` / `mount` / `RunJobs`, AvaPlot-or-placeholder
assertion) all match the established patterns.

One small drift from the project prompt's "no `open` of a project-internal
namespace unless the file uses 3+ symbols" rule: `open Berreman.Dispersion`
(`ResultsView.fs:35`) names nothing in the body -- `system.dispersive` is a
member access that resolves without it -- and `open Analytics.Variables`
(`ResultsView.fs:36`) likewise appears unused, since the only Analytics
reference is the fully-qualified `Analytics.StandardLightVariables.
wavelength200to800Range` at line 172. `open Berreman.Media` and `open
OpticalConstructor.Domain.Units` each contribute a single symbol
(`OpticalSystem`, `Nanometer`). Trimming the two dead opens would tighten the
header; the single-symbol ones are a wash against the `SourceView`/`Workspace`
precedent. Low priority -- the build is green and F# emits no warning by
default.

## Separation of concerns

The `lift` at `ResultsView.fs:162` reconstructs the same
`{ incidentLightInfo = source.light; opticalSystem = system.dispersive }` record
the SoW says `SynthesisFitPage.comparisonOverlay`'s `mkInfo` already builds.
This is a one-liner, so the duplication is cheap, but it is the second copy of
the "edited source over a system's dispersive form" projection. If
`SynthesisFitPage` exposes `mkInfo` (or an equivalent) as a callable seam, a
later part that touches the overlay lift will want a single source of truth;
worth a glance, though this is more the reuse critic's call than an
architecture defect.

## Spec fit

Both ACs are satisfied and scoped tightly. R-1/AC-U6.1: `schematicChildren`
projects `Schematic.layout` bands to `Rectangle`s and `Schematic.rayGeometry`
to the three ray `Line`s, hosted via `ChartHosts.canvasHost` -- no parallel
geometry model. R-2/AC-U6.2: `systemRow` dispatches `WS.ToggleVisible`/
`SetActive` (routed through the frozen `Workspace.update` in `Shell`, which the
SoW correctly notes was already wired in slice 001), and `overlayHost` rebuilds
`WS.renderOverlay` each render into `ChartHosts.scottPlotHost`. No frozen module
is edited; the only `OpticalConstructor.Ui` changes are the new file and the
arc's own `Shell.fs`. The deferrals (representative `materialKey`, fixed
`wavelength200to800Range 12` axis) are documented and match the `ChartView` /
slice-004 representative-inputs precedent -- under-delivery is principled, not
silent.

## Evolvability

The `py` interface-anchor computation
(`topPad + tops.[1]`, `ResultsView.fs:112`) correctly lands the ray at the top
of the first film band (band 0 being the incident half-space), and the `topPad`
offset keeps the upward reflected ray on-canvas. This is sound for the current
band order, but it hard-codes the assumption that band index 1 is the first
film; if a future part inserts a band ahead of the films (or `Schematic.layout`
reorders), the ray anchor silently drifts. Cheap insurance would be to anchor
off a named band rather than a positional index, but that is a forward concern,
not a current defect. U8 reuses the same Canvas adapter this slice now
exercises end-to-end, so the seam is validated ahead of that work as intended.

## Risks

- **Rectangle-count test brittleness.** The R-1 test asserts `rects >= 8`
  (`ResultsPanelTests.fs:65`), reasoning "five films + two half-spaces + a
  substrate band." That couples the test to `Templates.bandpassFilter`'s exact
  composition and to the headless theme contributing no template `Rectangle`s
  of its own. The `>=` lower bound and the green run mitigate it, but a later
  change to the seed template or to headless styling could flip it. Asserting
  on the band count derived from `Schematic.layout seedSystem` rather than a
  magic `8` would make the test self-describing. Minor.
- **Overlay ignores `Workspace.canOverlay`.** `overlayHost` builds the plot
  unconditionally even with a single visible system, whereas `Workspace`
  exposes `canOverlay` (>=2 visible) as the "overlay is meaningful" predicate.
  For this slice that is harmless (a one-curve plot still renders and the toggle
  still refreshes), and the spec does not require gating, so this is an
  observation rather than a fault -- but a future part that wants the AC-J5
  "two or more visible" semantics should reach for the existing predicate
  rather than re-deriving it.

No §0.4 / §0.5 concern: the view introduces no off-thread dispatch (button
clicks dispatch on the UI thread, no `Cmd`/`JobRunner` callback), and the live
`AvaPlot` is created inside `scottPlotHost` in the host layer, never on the
model.

## Bottom line

I would ship this. The slice does exactly what U6 asks, stays inside the frozen
seams, and matches the sibling-view precedent down to the test shape; the SoW
and impl-log accurately describe what landed and the file list matches the
diff. The findings -- two dead opens, a duplicated one-line lift, a positional
ray anchor, and a count-coupled test assertion -- are all cosmetic or
forward-looking and none blocks the gate. The judge can treat this as a
pass-with-optional-cleanup unless a gate result says otherwise.
