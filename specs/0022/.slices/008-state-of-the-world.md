# 008 — Calculations & analysis layer (Part F) — state of the world

## Where we are

Slice 008 is Part F of spec 0022: the pure analysis/derivation layer that reads the
engine's solver outputs and presents the §4 quantities. It sits on the storage/materials/
construction/light layers landed in slices 001–007 and is foundational for Part G
(slices 009–011, which reuse §F.5 `psiDelta` for fit residuals) and Part H (which uses
§F.6 `fieldDepthProfile` and §F.8 colorimetry). The work is predominantly reuse of the
existing 4×4 solver, field model, Mueller/Jones constructors, and 1D/3D sweep pipeline;
the net-new code is three small pure-function modules in the engine `Analytics` project
plus one one-line completion of a commented-out engine member.

## What's working

- Complete the per-`EmField` Mueller member at `FieldFunctions.fs:93` as a pure delegation to `MuellerMatrix.fromEmFields`.
- Add `Analytics/AnalysisFunctions.fs`: `absorptance` (A=1−R−T), `stokesSystem`, `degreeOfPolarization`, `psiDelta`/`ncs`.
- Add `Analytics/FieldProfile.fs`: `fieldDepthProfile`, `layerAbsorptance`, `totalAbsorbedPower` over the `EmField.propagate` seam.
- Add `Analytics/Colorimetry.fs`: `spectrumToXyz`/`xyzToLab`/`xyzToSrgb` with embedded CIE 1931 + D65/A tables.
- Ship mandatory smokes: `MuellerMatrixTests` (AC-F3 edit guard) and `AnalysisFunctionsTests` (AC-F1/F4/F6/F9/F2).

## Tests

- `build` gate: PASS — `dotnet build Berreman.slnx -c Release`, 0 errors (the three new
  `Analytics` files compile in dependency order; the completed `FieldFunctions.fs:93`
  member type-checks against the engine seams).
- `unit-tests` gate (`BerremanTests`): PASS — 76 passed, 5 skipped, 0 failed (baseline 70).
  6 new tests added (`MuellerMatrixTests` ×1, `AnalysisFunctionsTests` ×5).
- `constructor-unit-tests` gate (`OpticalConstructor.Tests`): PASS — 107 passed, 0 failed
  (baseline 107; untouched by this slice).
- Deferred: the FULL AC-F3/F5/F7/F8 equivalence suite (Mueller-equivalence, ellipsometry,
  energy-balance, colorimetry sanity) stays in slice 011's `OptimizationTests.fs` per the
  parent's routing. This slice owns only the in-slice smokes for the AC bullets it
  implements behaviourally (AC-F1/F2/F4/F6/F9).

```yaml
gates:
  berreman_unit_tests:    76
  constructor_unit_tests: 107
```

## Architecture

- The analysis layer is pure functions over `Solution`/`EmField`/`OpticalSystem`, living in
  the engine `Analytics` project (NOT in `OpticalConstructor.*`), with no UI, no FuncUI
  reference, and no hidden global state — the clone audit gate (§A.9) is unaffected.
- `psiDelta`/`ncs` define ρ = `reflected.amplitudeP / reflected.amplitudeS` (r_p/r_s of the
  single computed `Solution`'s reflected field), matching the slice-010 §G.6 consumer
  definition so Part G shares the derivation rather than forking it. tan Ψ = |ρ|, Δ = arg ρ;
  N=cos 2Ψ, C=sin 2Ψ cos Δ, S=sin 2Ψ sin Δ (so N²+C²+S²=1 by construction).
- `totalAbsorbedPower` is anchored to the §F.1 `absorptance` so it equals system-level A
  within tolerance; the per-layer Poynting-flux breakdown is `layerAbsorptance` (separate),
  honouring the carried-forward `EmField.propagate` limitation rather than being summed
  into a divergent total.
- `BerremanTests` now references the `Analytics` project and the `Softellect.Sys` package
  (10.0.101.41) so the AC-F2 sweep can round-trip through `BinaryZippedFormat`.

## Deferred

- FULL AC-F3/F5/F7/F8 equivalence assertions → slice 011 `BerremanTests/OptimizationTests.fs`.
- Chart rendering / UI page (Part H); §A.7 sweep-persistence wiring beyond the in-memory
  `.binz` smoke; caching/retry/resume of sweeps; schema-versioning of result records — all
  out of scope here.
- The clone-audit linking decision (§A.9/§F.10) remains deliberately undecided.

## Gotchas

- **`psiDelta` ρ definition.** Slice-008 AC-F5 prose said "r_pp/r_ss from
  `solutionP()`/`solutionS()`", but that needs two solves and is incompatible with the
  spec's own `psiDelta (s : Solution)` signature. The downstream consumers (slice 010 §G.6,
  slice 011 AC-F5) define ρ = r_p/r_s on a single `EmFieldSystem.reflected`; §F.5 MUST NOT
  be forked from §G.6, so that single-reflected-field definition is the one implemented.
- **`EmField.propagate` limitation.** The engine flags it "Does not work properly yet"
  (`BerremanMatrix.fs:221`). `fieldDepthProfile`/`layerAbsorptance` wire through it as-is;
  the AC-F6 smoke asserts list shape and monotonic meter depths only — |E|² magnitudes are
  NOT asserted, and `totalAbsorbedPower` does not sum the per-layer flux drops (it anchors
  to §F.1 A instead).
- **AC-F2 `.binz` round-trip is in-memory.** It uses `serialize`/`deserialize`
  `BinaryZippedFormat` over a `byte[]` (no file written), so "never under `%TEMP%`" holds
  trivially and no spec-folder absolute path is baked into a committed test. FsPickler does
  successfully serialize the `(float * Solution)[]` graph; fidelity is asserted on float
  keys + the R channel (not class reference equality), and JSON-exclusion via the gzip
  magic bytes.
- **`BinaryZippedFormat` is GZip** — payload starts 0x1f 0x8b; the test relies on that.
- Embedded CIE/illuminant tables are at a 10 nm grid (380–780 nm); `spectrumToXyz`
  linear-interpolates and normalises Y so a perfect reflector → illuminant white point.
- **Line endings.** The tracked engine files (`FieldFunctions.fs`, both `.fsproj`) use
  LF (UTF-8 with BOM) at HEAD. Edit them with an editor that preserves LF — a CRLF
  rewrite re-normalizes every line and balloons a few-line change into a whole-file diff
  on a core engine file. Cycle 2 reverted exactly such churn; keep the edits LF-only.

## Changelog

- 2026-06-01 — Slice 008 (Part F): completed `FieldFunctions.fs:93` `em.muellerMatrix`;
  added `Analytics/AnalysisFunctions.fs`, `Analytics/FieldProfile.fs`,
  `Analytics/Colorimetry.fs`; registered them in `Analytics.fsproj`; added
  `MuellerMatrixTests.fs` and `AnalysisFunctionsTests.fs` to `BerremanTests` (with an
  `Analytics` project reference and the `Softellect.Sys` package). All three gates green
  (build 0 errors; unit-tests 76 passed; constructor-unit-tests 107 passed).
- 2026-06-01 — Slice 008 cycle 2 (cleanup, code-judge route-back): reverted the incidental
  CRLF EOL-normalization on `FieldFunctions.fs` and both `.fsproj` files so the diff
  collapses to only the real edits; switched `MuellerMatrixTests` to the shared
  `MatrixComparison.allowedDiff` tolerance. No analysis code or test logic changed; build +
  unit-tests re-run green (76 passed).
