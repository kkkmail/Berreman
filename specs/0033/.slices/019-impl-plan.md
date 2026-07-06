# Impl plan — spec 0033, slice 019 (IMPLEMENT)

## Goal

Two moves on the dual-axis chart spine slice 018 delivered:

1. **REAL-MOVE** the pure spectral-axis helpers `axisLabel` / `spectralRange` /
   `axisTicks` out of `OpticalConstructor.Ui/MaterialPreview.fs` into a new
   `OpticalConstructor.Domain.SpectralAxis` module (Domain already references
   Berreman + Analytics, so `Range<WaveLength>` and `Units` are in reach), and
   re-point every caller — no duplication, no re-export shims.
2. **Build the pure n/k dispersion chart builder** in TestWindows:
   `nkDispersionChart : OpticalPropertiesWithDisp -> UnitOfMeasure ->
   Range<WaveLength> -> ExperimentChart` with the n series from
   `Analytics.Variables.calculateN11Re` and the k series from `calculateXi11Im`,
   x-values from the moved `SpectralAxis.axisTicks` (display unit), plus the
   paired style seed `nkDispersionStyle` that flips the k series to the RIGHT
   axis via the 018 `ChartStyle.setSeriesAxisSide` seam (n stays LEFT — exactly
   the host pattern 018's state-of-the-world called out for step 019).

The x-grids line up by construction: `calculateOpticalProp` samples
`s + (e-s)·i/n` linearly in meters (`Variables.fs:317-318` via
`RangedVariable.value`), and `axisTicks` computes the identical grid before
converting to the display unit — so zipping ticks with the calculated y-values
re-labels the axis without touching the data (AC-D7 discipline).

## Files

- **NEW** `Berreman/OpticalConstructor/OpticalConstructor.Domain/SpectralAxis.fs`
  — the three helpers, moved verbatim (docs included).
- `OpticalConstructor.Domain.fsproj` — `<Compile Include="SpectralAxis.fs" />`
  right after `Units.fs` (its only in-project dependency).
- `OpticalConstructor.Ui/MaterialPreview.fs` — helpers deleted; header doc
  notes the move; `plotInput` / `show*` untouched.
- `OpticalConstructor.Ui/MaterialsView.fs` — `MaterialPreview.spectralRange` →
  `SpectralAxis.spectralRange` (the `OpticalConstructor.Domain` open is already
  there); comment re-pointed.
- `OpticalConstructor.Tests/MaterialPreviewTests.fs` — add
  `open OpticalConstructor.Domain.SpectralAxis` so the existing AC-D7 tests
  keep compiling against the moved symbols (mechanical consequence of the
  REAL-MOVE; project not in `touches` — recorded in Gotchas).
- `OpticalConstructor.TestWindows.fsproj` — add the Analytics
  ProjectReference (`..\..\Analytics\Analytics.fsproj`, same relative shape as
  Domain's) and the new compile item.
- **NEW** `OpticalConstructor.TestWindows/NkDispersionChart.fs` —
  `nkDispersionChart` + `nkDispersionStyle` (+ named series indices).
- `OpticalConstructor.Ui.Tests.fsproj` + **NEW** `NkDispersionChartTests.fs` —
  headless model tests (pure, `ui-tests` gate): acceptance shape (2 series,
  n→left / k→right via the style seed, x spans the requested range in the
  display unit), dispersive entry (Silicon built-in) yields a curve,
  non-dispersive (Vacuum built-in) yields a flat line through the SAME builder.

## Risks

- `--warnaserror+:25` — all new signatures concrete; no generics introduced.
- OpticalConstructor.Tests is outside `touches` but its MaterialPreviewTests
  reference the moved symbols unqualified; a one-line open re-point is the
  minimal necessary edit (slice-017 precedent: callers re-point on a REAL-MOVE).
- LF discipline — verify no CRLF churn after edits.
- Baselines: expect BerremanTests 119, constructor 373, ui-smoke 59 unchanged;
  ui-tests 259 → 262 (+3 new model tests).
