# Step 034 — State of the world

## Where we are

Spec 0038 Part I, step 034 (IMPLEMENT, `depends_on: 25`). The step-25 Experiments domain gave
every experiment a detector kind, a varied parameter, and a numeric range, plus an optional
attached measured-data file path. This step adds the pure PARSERS that turn a measured-data file's
TEXT into elevated series values, and the validator that checks a parsed series against the
experiment it belongs to. It is the Domain half of the measured-data (inverse) flow; a later
storage cycle wires the disk read and the UI attachment on top of these pure functions.

## What's working

- Add the pure `MeasuredData` Domain module: elevated `IntensitySeries` / `EllipsometricSeries`
  over `MeasuredAbscissa` / `MeasuredIntensity` / `Angle` / `WaveLength` point types, and the
  `ExperimentDataError` channel (`MalformedDataFile` / `EmptyDataFile` / `DataRangeMismatch` /
  `DataUnitsMismatch`, each carrying `reason`).
- Parse the STRICT intensity schema v1 (labels row ignored, then invariant-culture `X,Y` rows)
  and ellipsometric schema v1 (`wavelength_nm,psi_deg,delta_deg` with optional `aoi_deg`) from
  file text — no filesystem IO, no exceptions, no delimiter or unit sniffing.
- Reject a malformed row (naming the first bad row), an empty file, and a header-only file with
  typed errors; the ellipsometric header is validated, not sniffed.
- Add `validateAgainstExperiment`: a LOUD typed `DataRangeMismatch` for an abscissa outside the
  experiment's range (never a silent trim) and `DataUnitsMismatch` when the series shape does not
  match the detector kind.
- Cover it with 19 in-memory-string tests; no test performs file IO.

## Tests

- **build** (whole solution, Release): green — 0 errors; no new warnings from this slice.
- **constructor-unit-tests**: green — 652 passed / 0 failed / 0 skipped, including the 19 new
  `MeasuredDataTests` facts.
- **unit-tests** (BerremanTests), **ui-smoke**, **ui-tests**: unaffected by this Domain-only
  change and carried forward from the step-033 baseline; the whole solution (Ui.Tests included)
  compiled clean in the build gate. The arc-runner re-runs every gate as the authority.

```yaml
gates:
  berreman_unit_tests:  119
  constructor_unit_tests: 652
  ui_smoke_tests:       170
  ui_tests:             436
```

## Architecture

- **Text-in parsers, IO at the edge.** Following the `SpectralImport.parseSpectrumCsv` precedent,
  every parser takes the file TEXT and returns `Result<_, ExperimentDataError>`; the disk read
  that yields the text is a future `ExperimentDataProxy` at the composition edge, so the parsers
  stay pure and unit-testable from strings.
- **`MeasuredSeries` tag DU.** An `IntensityData` / `EllipsometricData` tag lets
  `validateAgainstExperiment` reject a wrong-shape file as a `DataUnitsMismatch` rather than
  silently coercing one record into the other.
- **Single unit seam reused.** The ellipsometric `wavelength_nm` column crosses to the engine
  `WaveLength` through the sole `Units.toWaveLength` seam; Ψ/Δ/aoi are engine `Angle`s from
  degrees. The intensity X abscissa stays meaning-agnostic (the experiment's varied parameter
  fixes its meaning) and is interpreted only at validation.
- **Compile order.** `MeasuredData.fs` compiles last in the Domain (after `OutOfBandDiagnostic.fs`)
  — nothing else in the Domain depends on it.

## Deferred

- The disk-backed `ExperimentDataProxy` (the file read/write adapter, §15.4) and the UI attachment
  flow that hands a picked file's text to these parsers — a later storage/UI cycle.
- Future measured-data parsers explicitly OUT of scope here: N/C/S and Mueller-element imports,
  delimiter sniffing, and unit heuristics. Those are new parsers, not schema-v1 changes.
- Interpreting the intensity abscissa into a typed `WaveLength` / `Angle` per the varied parameter
  (only its raw value is range-checked today) — add when a consumer needs the typed abscissa.

## Gotchas

- **Local gate run is advisory.** `implement_worker.system-md` Invariant 6 makes the arc-runner
  the sole gate authority; `CLAUDE.md` makes a green build non-negotiable and mandates running
  tests after every change. Reconciled by running build + constructor tests locally to verify my
  own work only — the arc-runner re-runs and decides.
- **`DataUnitsMismatch` means series-shape ↔ detector-kind.** An intensity file for an
  ellipsometer (or vice versa) is the units mismatch; that is the guard `validateAgainstExperiment`
  enforces.
- **Ellipsometric range check applies only to a wavelength sweep** — for an R1/R2-varied
  experiment the file's wavelength abscissa is not the swept quantity, so the wavelength range
  check is skipped (returns `Ok`), documented by a dedicated test.
- **Strict parsing.** Intensity rows are EXACTLY two invariant-culture floats; extra columns are
  malformed. The ellipsometric header is matched case-insensitively after trimming but otherwise
  exact — no sniffing.

## Changelog

- 2026-07-11 — Step 034 (IMPLEMENT): added the pure `MeasuredData` Domain module (elevated
  series/point types, `ExperimentDataError`, strict intensity + ellipsometric text parsers, and
  `validateAgainstExperiment` with typed range/units mismatches) and 19 in-memory-string tests.
  Build green; constructor suite 652 passing, 0 failing.
