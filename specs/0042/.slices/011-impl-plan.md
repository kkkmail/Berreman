# Impl-plan — slice 011 (WIRE) — §7.2 reconstruction composition root

## Goal

Wire the composition root of the deliverable in
`Berreman/BerremanTests/MuellerReconstructionTests.fs`: instantiate the REAL
`MuellerDataStore.createFileBacked ()` (Storage) and `createMathNetSvd ()` (Domain)
proxies, load the OPM measured data, run Stage 1 (AIR calibration) → Stage 2 (glue
models) → Stage 3 (reduction + reconstruction) end-to-end, and assert the report
§7.2 / §4 / §8 results. Guard with `[<Fact>]` + `Assert.Skip` (xunit.v3-native dynamic
skip) so the suite SKIPS when the sibling `optics-mueller` checkout is absent — the slice
names `[<SkippableFact>]`, but that package is v2-only and yields zero discovered test
cases under this project's xunit.v3 runner (see Key decisions).

This is a WIRE step: it only assembles already-built Domain/Storage surfaces
(steps 001–010) at the test composition root. No new production code.

## Files

- `Berreman/BerremanTests/MuellerReconstructionTests.fs` — the top-of-file `///` XML-doc
  block (future candidates NOT-yet-asserted) was already present from attempt 01; add one
  new `[<Fact>]` member that calls `Assert.Skip` when the OPM data is absent. No other
  files change (the fsproj already references Domain + Storage).

## Approach (mirrors the reference `matrix_step1_air_fit.py` → `matrix_glue.py` → `matrix_fit_linear.py`)

1. **Locate data** — reuse the existing `tryFindOpmFinalDir ()` walk-up (proven by
   step 008). Load the 5 science CSVs + `darkness_checks.csv` + `bullshit_checks.csv`
   through `createFileBacked ()`.
2. **Stage 1 (calibration)** — reuse `reduceAirTraces` / `freeCosineFit` /
   `retardanceFromVisibility` / `lpFrameCoeffs` / `sourceCplFromLpFrame` (all already
   in the file for step 008) to re-derive `z_LP`, `z_CPL`, analyzer δ, and the source
   `(θ_rel, δ)`.
3. **Stage 2 (glue models)** — build `sourceLp` / `sourceCpl` / `analyzerLp` /
   `analyzerCpl` (`load_step1_models`: analyzer_lp dial −1, analyzer_cpl dial +1 /
   θ_rel 45 / δ negative, source δ positive) and `modelsFor : Family -> …`.
4. **Stage 3 (reduction)** — per-family AIR scalar gain via `scalarGain` over
   `darkSubtract` vs `airIdentityPred` on each file's AIR#0 block; assemble a
   `GainCalibration` (LP-LP / LP-CPL mean-pair, CPL-LP split at the first
   BullshitCheck, CPL-CPL day2 single) and a day1 variant (`Day1Interp` between the
   two AIR blocks' mean capture times). `correctSignal` = dark-subtract / gain.
5. **Reconstruction** — for each kind (`Qz`, `Lr`, `QzLrProduct`), call `buildDesign`
   per science file with that file's gain rule, concatenate designs/targets, then
   `reconstruct (createMathNetSvd ())`. Assert rank 16, matrices within ~2e-3 of §4,
   and the cascade metrics via `cascadeProduct` + `frobeniusDiff`.

## Key decisions / risks

- **Per-file gain, not one bundle.** The two CPL-CPL files share family `CplCpl`
  but use different rules (day1 interp vs day2 single) and share a calendar date, so
  they cannot be told apart row-by-row. `buildDesign` is called once per file with the
  file's own gain calibration, then the (design, target) pairs are concatenated per
  kind (LS solution is row-order-independent).
- **Skip mechanism: `[<Fact>]` + `Assert.Skip`, NOT `[<SkippableFact>]` (verified).**
  `SolverTests.fs`'s `[<SkippableFact>]` members (`basicSolverTest1/2/3`) are silently
  NOT DISCOVERED under this project's xunit.v3 in-proc runner — a filtered run of
  `SolverTests.basicSolverTest*` reports `Total: 2` (only the two `[<Fact>]` siblings);
  the tests that actually skip use `[<Fact (Skip = "…")>]`. So `Xunit.SkippableFact`
  1.5.61 (xunit v2) yields zero test cases here, exactly as step 008 found. The fact is a
  plain `[<Fact>]` that calls `Assert.Skip(reason)` when `tryFindOpmFinalDir ()` finds no
  checkout — same skip-when-absent semantics, and it actually runs under v3.
- **Data-folder resolution.** Reuse `tryFindOpmFinalDir ()` (walk-up) rather than the
  slice's `__SOURCE_DIRECTORY__` + `../../optics-mueller/...` — the latter is off by one
  directory level for the nested `Berreman/Berreman` layout and duplicates existing logic.
- **Argmax of the cascade delta.** The reference §8 delta's two largest |Δ| are at
  (4,4)=0.268065 and (4,2)=0.267826 1-indexed — NOT the slice's "(4,2) and (2,2)"
  ((2,2)=0.193 is only 4th-largest). Assert the reference-faithful positions and record
  the slice discrepancy (base-protocol skepticism rule).
- **Tolerance.** Assert §4 matrices within the slice's ~2e-3 acceptance band (the pure-F#
  reconstruction matches the Python reference to ~1e-9, so ~2e-3 is huge margin over
  6-decimal print rounding).
