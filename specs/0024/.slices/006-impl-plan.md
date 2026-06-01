# 006 impl-plan — Results + schematic + workspace (Part U6)

## Approach

This slice wires the `results` panel. The geometry/overlay builders are frozen
and already exist (§0.1); the round only renders them and routes the workspace
selection through the existing `Workspace.update`.

Two acceptance criteria:

- **AC-U6.1** — render the schematic band layout (`Schematic.layout`) and the ray
  overlay (`Schematic.rayGeometry`) on the slice-002 **Canvas host**
  (`ChartHosts.canvasHost`). MUST NOT recompute geometry.
- **AC-U6.2** — toggling a system's visibility dispatches `Workspace …` (routed
  through the frozen `Workspace.update`, already wired in `Shell.update` since
  slice 001) and refreshes the multi-system overlay (`Workspace.renderOverlay`,
  returns `ScottPlot.Plot`) in the slice-002 **AvaPlot host**
  (`ChartHosts.scottPlotHost`).

## Files

New:
- `OpticalConstructor.Ui/ResultsView.fs` — the panel view. Like the
  slice-002..005 views it takes its sub-state (`Workspace.Model` + the edited
  `SourceSpec`) plus a `Workspace.Msg` dispatch — NOT `RootModel` — so it
  composes under the root without a module cycle.
- `OpticalConstructor.Ui.Tests/ResultsPanelTests.fs` — two headless `ui-tests`.

Edited:
- `OpticalConstructor.Ui/Shell.fs` — wire the `"results"` panel id to
  `ResultsView.resultsPanel` (currently a placeholder). The `Workspace` `RootMsg`
  case already dispatches through `Workspace.update`.
- `OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj` — register `ResultsView.fs`
  (after `SourceView.fs`, before `ConstructionView.fs`/`Shell.fs`).
- `OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` — register
  `ResultsPanelTests.fs`.

## Schematic projection (R-1)

`Schematic.layout Nanometer materialKey activeSystem : Band list` → one
`Rectangle` per band stacked vertically on the Canvas (cumulative tops via
`List.scan`), filled from the band's `SchematicColor`. `Schematic.rayGeometry
source.light.incidenceAngle : RayGeometry` → three `Line`s (incident / reflected
/ transmitted) drawn from the top film interface. All hosted through
`ChartHosts.canvasHost`. A `topPad` offset keeps the upward reflected ray on the
canvas. The active system is `workspace.active` (falling back to the first
system). `materialKey` is a representative per-index key (the §A.7 per-layer
material-id assignment lift is a later part — same representative-inputs
precedent as `ChartView`'s sample sweep / slice-004's `referenceWavelength`).

## Workspace overlay (R-2)

A per-system row with a visibility toggle (`Workspace.ToggleVisible i`) and a
set-active button (`Workspace.SetActive i`). The overlay is built by the frozen
`Workspace.renderOverlay ChartSettings.defaultValue lift OpticalFunction.R
(wavelength200to800Range 12) workspace`, where `lift system = { incidentLightInfo
= source.light; opticalSystem = system.dispersive }` (the same `OpticalSystem ->
FixedInfo` shape `SynthesisFitPage.comparisonOverlay` uses). Hosted via
`ChartHosts.scottPlotHost` (AvaPlot), degrading to the §U1.8 placeholder
headlessly.

## Risks

- Low–medium. Geometry/overlay builders exist; the work is correct Canvas
  projection and reusing the slice-002 adapters rather than re-hosting. The
  FuncUI Shapes DSL (`Rectangle`/`Line`/`Canvas.top`/`Canvas.left`) is confirmed
  present in the pinned `Avalonia.FuncUI` 1.6.0 by reflection.
