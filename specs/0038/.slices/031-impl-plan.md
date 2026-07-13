# Step 031 — impl-plan (out-of-band dispersion diagnostic)

## What the slice asks

Add an OUT-OF-BAND diagnostic, independent of the step-30 bound/unbound cue. A
PURE Domain function: given the materials reachable through an element's binding
(a bound sample's layer material versions; a directly bound material) and the
wavelengths the actual experiment requests (a single λ for a fixed setting, the
full swept range for a wavelength sweep), flag when SOME reachable material is
dispersive AND the requested set falls partly outside the union of that
material's defined dispersion segments (`EditSegment.interval` →
`WaveLengthInterval`). Constant materials NEVER flag. The result carries
per-material defined-vs-requested ranges as typed data. Render a small warning
badge on the flagged element with a hover tooltip naming the offending
material(s) and both ranges; the same text appears in the Details bay.

## Approach

**Domain (new pure module `OutOfBandDiagnostic.fs`).** Elevated types, no naked
primitives:

- `RequestedWavelengths = FixedWavelength of WaveLength | SweptWavelengths of WaveLengthInterval`.
- `ReachableMaterial = { materialName : string; definedSegments : WaveLengthInterval list }`
  — a material's DEFINED dispersion segment intervals (empty for a constant or a
  coded-preset material → never flags).
- `OutOfBandFinding = { materialName; definedRange; requestedRange }` (both ranges
  as `WaveLengthInterval`).
- `DispersionCoverage = InBand | OutOfBand of OutOfBandFinding list`.
- `definedSegmentIntervals : MaterialComplexity -> WaveLengthInterval list` reads
  the eps segment `wavelengthInterval`s (constant eps → `[]`).
- `reachableMaterialOf : MaterialEntry -> ReachableMaterial` (a coded preset,
  `complexity = None`, yields no defined segments).
- Coverage: merge the segment intervals (touching/overlapping fuse), then a
  contiguous request is covered iff it fits inside ONE merged interval. A fixed λ
  is the degenerate interval `{lower = λ; upper = λ}`.
- `checkOutOfBandDispersion` / `checkMaterialsOutOfBand`, plus
  `requestedWavelengthsFor` / `requestedWavelengthsOf` (sweep → nm range through
  the sole `Units.toWaveLength` seam; else the fixed λ), and `findingText` /
  `coverageWarning` for the tooltip + Details text (nm labels via `Units`).

Register after `Experiments.fs` (reuses `VariableParameter` / `VariableRange` /
`Experiment`); place last in the compile list — nothing else depends on it.

**Ui (`TableAndElementRotationView.fs`).** Host-owned, renderer-independent:

- `reachableMaterialEntries` resolves a bound sample's layer material VERSIONS
  through `model.materials.resolveVersion`; non-sample / unbound → none.
- `outOfBandWarningFor model e : string option` = the coverage warning for the
  scene's requested wavelengths (the Experiments-bay draft's wavelength sweep, or
  the fixed source λ via the existing `runWaveLength`).
- `outOfBandBadge` (a "⚠" `TextBlock` at the element's projected centre, an
  indexed AutomationId `UiIds.outOfBandBadge i`, an Avalonia `ToolTip.Tip` = the
  text) + `outOfBandBadges` over the scene, appended to `mainTableCanvas`.
- `detailsState` appends the same warning to the selected element's title.

**Tests.** `OpticalConstructor.Tests/OutOfBandDiagnosticTests.fs` — 600 nm inside
300–700 does not flag; a 200–800 sweep flags naming the material + both ranges;
multi-segment (adjacent + gap) unions; constant materials never flag; the
requested-wavelengths derivation. `OpticalConstructor.Ui.Tests/OutOfBandBadgeTests.fs`
— a flagged element renders the badge headless with its tooltip text; an in-band
scene renders none.

## Files

- add `OpticalConstructor.Domain/OutOfBandDiagnostic.fs` (+ fsproj)
- edit `OpticalConstructor.Ui/TableAndElementRotationView.fs`
- add `OpticalConstructor.Tests/OutOfBandDiagnosticTests.fs` (+ fsproj)
- add `OpticalConstructor.Ui.Tests/OutOfBandBadgeTests.fs` (+ fsproj)

## Risks

- FuncUI tooltip is a NEW pattern (no existing `ToolTip` use) — set the attached
  `ToolTip.TipProperty` via `AttrBuilder`, the AutomationId precedent.
- Order: badge helpers must sit after `runWaveLength`/`boundEntry` yet before
  `mainTableCanvas`; the badge id goes in the early `UiIds` module.
- LF line endings; zero new warnings; every touched `Drawable`/record literal
  stays complete.
