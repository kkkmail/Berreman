# 005 impl-log — Sources panel (Part U4)

## Progress

- [x] Read system + project prompts, slice spec, prior SoW (004).
- [x] Surveyed the frozen seams: `SourceEditorView`, `PolarizationPicker`,
      `PolarizationPlots`, `ChartHosts`, `Shell`, `MaterialsView` (closest precedent).
- [x] Wrote impl-plan.
- [x] Implement `SourceView.fs`.
- [x] Wire `Shell.fs` (`source` model field, `Source` RootMsg case, `sources` panel).
- [x] Register `SourceView.fs` in `OpticalConstructor.Ui.fsproj`.
- [x] Add headless `SourcePanelTests.fs` + register in test fsproj.
- [x] Run gates.

## Files modified

- NEW `Berreman/OpticalConstructor/OpticalConstructor.Ui/SourceView.fs`
- NEW `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SourcePanelTests.fs`
- EDIT `Berreman/OpticalConstructor/OpticalConstructor.Ui/Shell.fs`
- EDIT `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`
- EDIT `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`

## Testing state

- `build`: pass — solution builds 0 errors (pre-existing MSB3277 / FS1125 warnings only).
- `unit-tests` (BerremanTests): pass — 84 passed / 5 skipped.
- `constructor-unit-tests`: pass — 204 passed.
- `ui-smoke`: pass — 1 passed; the `sources` panel renders one frame headlessly,
  the AvaPlot ellipse renders and the WebView2 Poincaré degrades to the placeholder.
- `ui-tests`: pass — 13 passed (prior 9 + 4 new sources-panel tests).

## Artifacts

- Gate captures under `specs/0024/.artifacts/` (build/test stdout).

## Decisions / Gotchas

- The polarization presets (`PolarizationPicker.applyPreset`) set `coherence`
  (`Unpolarized`), which no `SourceEditorView.SourceMsg` case can express, so they
  cannot be decomposed into `SourceMsg`. `SourceView` therefore defines a thin local
  `SourceViewMsg = Edit of SourceMsg | ApplyPreset of PolarizationPreset` dispatched
  through the single `Source` `RootMsg` case — `Edit` routes
  `SourceEditorView.update` (R-1), `ApplyPreset` routes `applyPreset` (R-2). This
  mirrors the materials precedent's view-local `MaterialsMsg`.
