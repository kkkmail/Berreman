# 008 — Calculations & analysis layer (Part F) — impl-plan

## Approach

Pure analysis/derivation layer over the engine's existing solver outputs. Predominantly
reuse: the 4×4 solver (`Solvers.fs`), the field model (`Fields.fs`), the Mueller/Jones
constructors, the 1D/3D sweep pipeline (`Variables.fs`) all already exist and MUST NOT be
forked. The net-new work is three small pure-function modules in the engine `Analytics`
project plus one one-line completion of a commented-out member in the engine
`FieldFunctions.fs`.

### Files to create / edit

1. `Berreman/Berreman/FieldFunctions.fs` (edit) — implement the commented-out
   `member em.muellerMatrix` (line 93) as a pure delegation to
   `MuellerMatrix.fromEmFields em em` (the field's own s/p-resolved amplitudes). No
   re-derived 4×4 algebra. (F.3)
2. `Berreman/Analytics/AnalysisFunctions.fs` (new) —
   - `absorptance (s : Solution) : float` = `1 - R - T` via `Solution.func` (F.1)
   - `stokesSystem (s : Solution) : StokesSystem` from `stokesI/stokesR/stokesT` (F.3)
   - `degreeOfPolarization (StokesVector v) : float` = √(s1²+s2²+s3²)/s0 (F.4)
   - `psiDelta (s : Solution)` / `ncs (s : Solution)` from ρ = reflected.amplitudeP /
     amplitudeS (F.5; matches the §G.6 consumer definition ρ = r_p/r_s).
3. `Berreman/Analytics/FieldProfile.fs` (new) —
   - `fieldDepthProfile system info samplesPerLayer : (double<meter> * float) list`
     stepping the transmitted `EmField` through each film via `EmField.propagate` (F.6)
   - `layerAbsorptance system info : (Layer * float) list` from the Poynting-flux drop
     across each film (`EmField.intensity`) (F.7)
   - `totalAbsorbedPower system info : float` anchored to F.1 `absorptance` so it equals
     the system-level A within tolerance (F.7).
4. `Berreman/Analytics/Colorimetry.fs` (new) — `spectrumToXyz` / `xyzToLab` /
   `xyzToSrgb` with embedded CIE 1931 2° CMF + D65/A illuminant tables (10 nm grid). (F.8)
5. `Berreman/Analytics/Analytics.fsproj` (edit) — add the three new files after
   `Charting.fs` in dependency order (AnalysisFunctions → FieldProfile → Colorimetry).
6. `Berreman/BerremanTests/MuellerMatrixTests.fs` (new) — mandatory smoke: completed
   `em.muellerMatrix` equals `MuellerMatrix.fromEmFields em em`.
7. `Berreman/BerremanTests/AnalysisFunctionsTests.fs` (new) — mandatory smokes for
   AC-F1, AC-F4, AC-F6, AC-F9, AC-F2.
8. `Berreman/BerremanTests/BerremanTests.fsproj` (edit) — register the two test files;
   add `Softellect.Sys` package reference for the AC-F2 `.binz` round-trip.

## Risks

- F# `--warnaserror+:25` is on for Analytics and BerremanTests: all matches must be complete.
- AC-F2 round-trips a `(float * Solution)[]` through FsPickler/`BinaryZippedFormat`. If
  FsPickler cannot serialize the `Solution` graph (class + MathNet vectors), fall back to
  asserting fidelity on a derived projection. Round-trip equality is asserted on the float
  keys and the R/T channel values (not reference equality of the class).
- `EmField.propagate` carries the engine's "Does not work properly yet" limitation
  (`BerremanMatrix.fs:221`); F.6/F.7 wire through it as-is — magnitudes NOT asserted.
