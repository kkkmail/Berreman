# Step 034 — IMPLEMENT — impl-log

## Progress

- [x] `MeasuredData.fs` Domain module (types, parsers, validation)
- [x] Register `MeasuredData.fs` in `OpticalConstructor.Domain.fsproj`
- [x] `MeasuredDataTests.fs` (in-memory-string tests only, no file IO)
- [x] Register `MeasuredDataTests.fs` in `OpticalConstructor.Tests.fsproj`
- [x] Local build + constructor-unit-tests (advisory; see Gotchas)

## Files modified

- **NEW** `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasuredData.fs` — the
  `MeasuredData` module: `ExperimentDataError` (`MalformedDataFile` / `EmptyDataFile` /
  `DataRangeMismatch` / `DataUnitsMismatch`, each `reason : string`); the elevated point types
  `MeasuredAbscissa` / `MeasuredIntensity`; `IntensityPoint` / `IntensitySeries`;
  `EllipsometricPoint` / `EllipsometricSeries`; the `MeasuredSeries` tag DU; the strict
  `parseIntensitySeries` and `parseEllipsometricSeries` text parsers; and
  `validateAgainstExperiment`.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` —
  `<Compile Include="MeasuredData.fs" />` added LAST (nothing else depends on it).
- **NEW** `Berreman/OpticalConstructor/OpticalConstructor.Tests/MeasuredDataTests.fs` — 19
  xUnit facts over in-memory strings (parse good/malformed/empty/header-only/CRLF; ellipsometric
  3- and 4-column + bad header; validation range/units mismatches). No file IO.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` —
  `<Compile Include="MeasuredDataTests.fs" />` added last.

## Testing state

- **Build** (`dotnet build Berreman.slnx -c Release`): **succeeded, 0 errors**. Zero warnings
  originate from the two new files; the 10 warnings shown are all pre-existing (SeriesDataTests
  FS1125, Dispersion.fs FS3873, ChartWindow FS0044, MathNet SYSLIB0051, NU1701 Wolfram) and
  outside this slice's scope.
- **Constructor unit tests** (`dotnet test OpticalConstructor.Tests -c Release`): **652 passed,
  0 failed, 0 skipped**; the 19 new `MeasuredDataTests` are all green. No regressions.
- `berreman_unit_tests` / `ui_smoke_tests` / `ui_tests` gates: untouched by this Domain-only
  change; carried forward from the step-033 baseline (the whole solution, including
  `OpticalConstructor.Ui.Tests`, compiled clean in the build gate).

## Acceptance mapping (slice 034)

- *good / malformed / empty CSV parse or reject with typed errors* — `parseIntensitySeries` and
  `parseEllipsometricSeries`: good → `Ok series`; a malformed row → `MalformedDataFile` naming
  the first bad row; an empty / header-only file → `EmptyDataFile`. Proven for both schemas.
- *a range or units mismatch against the experiment yields the typed validation error* —
  `validateAgainstExperiment` returns `DataRangeMismatch` for an abscissa outside the range
  (never trimmed) and `DataUnitsMismatch` when the series shape does not match the detector kind.
- *no test performs file IO* — every test drives the parsers from an in-memory string
  (`MeasuredDataTests.csv` = `String.Join("\n", …)`); nothing opens a file.

## Artifacts

None captured — pure numerical/domain work with no persistent run output.

## Gotchas

- **Local gate run is advisory (Invariant 6 reconciliation).** `implement_worker.system-md`
  Invariant 6 says the worker "runs no checks" and the arc-runner's gate engine is the sole
  authority. `CLAUDE.md` (project instructions that OVERRIDE defaults) says a green build is
  *non-negotiable* and to "run the relevant tests after every change". I reconciled by running
  `dotnet build` + the constructor tests locally ONLY to verify my own work, treating the result
  as ADVISORY; the arc-runner re-runs every gate after I exit and remains the authority. The
  exit JSON reflects my advisory local result.
- **`DataUnitsMismatch` = series-shape ↔ detector-kind mismatch.** The clean, testable reading of
  a "units mismatch": an intensity file (dimensionless S0) supplied for an ellipsometer (records
  Ψ/Δ, angles), or vice versa. `MeasuredSeries` carries the tag so the guard can fire; the two
  record shapes are never silently coerced.
- **Intensity X is meaning-agnostic at parse time.** The X column MEANS whatever the experiment's
  varied parameter says (nm for wavelength, ° for R1/R2), so it parses to a bare
  `MeasuredAbscissa` and only `validateAgainstExperiment` interprets it. The range check needs no
  unit conversion — the X column and `range.min/max` are in the SAME display unit by the schema.
- **Ellipsometric range check applies only to a wavelength sweep.** The ellipsometric schema's
  abscissa is `wavelength_nm`; when the experiment varies R1/R2 the file's wavelength is NOT the
  swept quantity, so the wavelength range check is skipped (returns `Ok`) rather than comparing a
  nm value against a degree range. Documented by a dedicated test.
- **Strict, no sniffing.** Intensity rows must be EXACTLY two comma-separated invariant-culture
  floats; the ellipsometric header is VALIDATED against `wavelength_nm,psi_deg,delta_deg[,aoi_deg]`
  (case-insensitive after trimming), not sniffed. No delimiter/unit heuristics, no N/C/S or
  Mueller-element imports (future parsers, out of scope).
- **Line endings.** Parsers split on `\n` and trim each line's `\r`/whitespace, so a pasted CRLF
  string parses; blank lines are dropped and the first non-empty line is the header. Source files
  are LF-clean (verified: 0 stray CR bytes).

## Changelog

- 2026-07-11 — Step 034: added the pure `MeasuredData` Domain module (elevated series/point
  types, `ExperimentDataError`, strict intensity + ellipsometric text parsers, and
  `validateAgainstExperiment` with typed range/units mismatches) plus 19 in-memory-string tests.
  Build green; constructor suite 652 passing.
