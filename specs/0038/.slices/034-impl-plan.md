# Step 034 — IMPLEMENT — impl-plan

## Slice

Pure measured-data parsers in `OpticalConstructor.Domain` (text in, values out) plus
`validateAgainstExperiment`, following the text-taking parser precedent
(`SpectralImport.parseSpectrumCsv`) that keeps filesystem IO and exceptions out. Every
primitive elevated; errors are values; every test runs against in-memory strings.

## Approach

Add one new Domain module `MeasuredData.fs` (compiled LAST, after `OutOfBandDiagnostic.fs`
— nothing else depends on it). It reuses:

- `Units.toWaveLength` / `Units.wavelengthToUnit` / `Nanometer` — the SOLE unit seam — for
  the ellipsometric `wavelength_nm` column.
- `Berreman.Geometry.Angle.degree` for `psi_deg` / `delta_deg` / optional `aoi_deg`.
- `Experiments.VariableParameter` / `VariableRange` and `Library.DetectorKind` (step 25) for
  `validateAgainstExperiment`.

### Types (all elevated; no bare primitive in a domain type/signature)

- `ExperimentDataError` = `MalformedDataFile` | `EmptyDataFile` | `DataRangeMismatch` |
  `DataUnitsMismatch`, each carrying `reason : string`.
- `MeasuredAbscissa` / `MeasuredIntensity` — single-case DUs with `.value` (the intensity X
  MEANS whatever the experiment's varied parameter says, so it stays meaning-agnostic at
  parse time).
- `IntensityPoint { x; intensity }`, `IntensitySeries { points }`.
- `EllipsometricPoint { waveLength; psi; delta; aoiOpt }`, `EllipsometricSeries { points }`.
- `MeasuredSeries` = `IntensityData` | `EllipsometricData` — the tag that lets
  `validateAgainstExperiment` catch a wrong-shape file as a LOUD `DataUnitsMismatch`.

### Parsers (strict schema v1, invariant culture, no delimiter/unit sniffing)

- `parseIntensitySeries` — first non-empty line is labels (ignored); each remaining
  non-empty line is EXACTLY two comma-separated invariant-culture floats; first bad row →
  `MalformedDataFile` naming the row; no rows → `EmptyDataFile`.
- `parseEllipsometricSeries` — header MUST be `wavelength_nm,psi_deg,delta_deg` with an
  optional 4th `aoi_deg` (validated, not sniffed); each data row is 3 (or 4) invariant floats;
  first bad row → `MalformedDataFile`; no rows → `EmptyDataFile`.

### Validation

`validateAgainstExperiment (varied) (range) (detector) (series)` →
`Result<MeasuredSeries, ExperimentDataError>`:

- detector-kind vs series-kind mismatch → `DataUnitsMismatch` (never coerced).
- intensity series: any X outside `[range.min, range.max]` (same display unit as the range by
  schema) → `DataRangeMismatch` (LOUD, never trims).
- ellipsometric series against a `VaryWaveLength` experiment: any wavelength (nm) outside the
  range → `DataRangeMismatch`; a non-wavelength varied parameter skips the wavelength range
  check (the swept quantity is not the file abscissa).

## Files

- NEW `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasuredData.fs` (+ fsproj entry).
- NEW `Berreman/OpticalConstructor/OpticalConstructor.Tests/MeasuredDataTests.fs` (+ fsproj entry).

## Risks

- Line-ending handling: split on `\n`, trim `\r`/whitespace per line so pasted CRLF strings
  parse. Blank lines ignored; the FIRST non-empty line is the header.
- Keep the range comparison tolerant to float noise at the endpoints (tiny relative epsilon)
  but still LOUD for genuine overruns.
