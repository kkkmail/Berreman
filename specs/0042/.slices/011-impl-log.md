# Impl-log — slice 011 (WIRE) — §7.2 reconstruction composition root

## Progress

- [x] Read system + project prompts (real paths under
      `.../src/ai_strategy_generator/multistep/wire_worker.system-md` +
      `.../multistep/arc-runner.system-md`), the project prompt, the slice spec, and the
      prior SoW (008/010).
- [x] Studied the reference pipeline end-to-end (`matrix_step1_air_fit.py`,
      `matrix_glue.py`, `matrix_fit_linear.py`) and the §4/§7.2/§8 reference outputs
      (`matrix_solution/step1/summary.json`, `step2_linear/report.md` +`summary.json`).
- [x] Verified the engine conventions match the Python 1:1 (line-by-line):
      `rotationMueller`/`rotateMueller` ↔ `rotation_matrix` (2θ doubling, signs),
      `analyzerMueller IdealLinear` row 0 ↔ `linear_polarizer` row 0, `retarderMueller` ↔
      `retarder`, `effectiveSource`/`effectiveAnalyzer` ↔ `R(φ)·s` / `a·R(−φ)`, `kron4` ↔
      `np.kron`, `muellerOfVecColumnMajor` ↔ reshape order F, `cascadeProduct` ↔ `M_LR·M_QZ`.
- [x] Cross-checked the science-row counts (qz=228, lr=227, comb=228) and calibration
      constants against the reference — both match.
- [x] Added the `[<Fact>]` §7.2 composition-root member (Stage 1 → 2 → 3 → reconstruct →
      cascade). The top-of-file `///` future-candidates block was already added by the
      prior attempt; left intact (it lists all six mandated future facets).
- [x] Built + ran locally to de-risk this retry (see Testing state).

## Files modified

- `Berreman/BerremanTests/MuellerReconstructionTests.fs` — added one `[<Fact>]` member,
  `section 7.2 end-to-end: the OPM Stage 1 to 2 to 3 pipeline reconstructs M_QZ, M_LR and
  M_{QZ+LR} at rank 16 and confirms the section 8 cascade metrics (skips when the OPM data
  is absent)`. It wires the REAL `MuellerDataStore.createFileBacked ()` (Storage) and
  `createMathNetSvd ()` (Domain) proxies, resolves the OPM `final` dir via the existing
  `tryFindOpmFinalDir ()` walk-up, loads the five science CSVs + `darkness_checks.csv` +
  `bullshit_checks.csv`, RE-derives the Stage-1 AIR calibration, populates the Stage-2
  per-family source/analyzer models, computes the Stage-3 per-family AIR gains, assembles
  the design through `buildDesign` per family, and reconstructs M_QZ / M_LR / M_{QZ+LR}.
  Asserts: each solve rank 16; each matrix within ~2e-3 of the §4 printed matrices;
  cascade `‖Δ‖_F≈0.571`, mean|Δ|≈0.113, max|Δ|≈0.268; the two largest |Δ| positions.

## Testing state

Per WIRE **Invariant 6** the arc-runner gate engine is the sole gate authority and runs
`build` / `unit-tests` / `constructor-unit-tests` after I exit. Because this is a retry of
a crashed attempt AND the deliverable is a data-backed numeric composition, I built and ran
the affected suite ONCE locally to de-risk the retry (reported honestly below, NOT as a
gate green-light):

- `dotnet build BerremanTests/BerremanTests.fsproj -c Release` → **Build succeeded, 0
  errors**. The only warnings are the pre-existing `SYSLIB0051` obsolete-serialization
  notices inside the vendored MathNet.Numerics C# (not our F#, not introduced here). My
  new F# compiles warning-clean.
- `dotnet test BerremanTests --no-build -c Release` (full suite) → **Passed 146, Failed 0,
  Skipped 5** (the 5 skips are the pre-existing `SolverTests` wedge skips). The new §7.2
  member RUNS (the sibling `optics-mueller` checkout is present on this host) and passes:
  M_QZ / M_LR / M_{QZ+LR} each within 2e-3 of the §4 printed matrices at rank 16, and the
  cascade metrics match (0.571418 / 0.113286 / 0.268065).

## Artifacts

- none (pure code round — the test loads the sibling OPM CSVs read-only; no artifacts written).

## Gotchas

- **`[<Fact>]` + `Assert.Skip`, not `[<SkippableFact>]` (recorded choice, continues
  008–010).** The slice names `[<SkippableFact>]` + `Skip.If`, but `Xunit.SkippableFact
  1.5.61` is xunit **v2** and contributes ZERO test cases under this project's xunit.v3
  in-proc runner (a hollow, silently-uncounted test). xunit.v3 ships native dynamic skip
  (`Assert.Skip`), so the fact is a plain `[<Fact>]` that calls `Assert.Skip(reason)` when
  `tryFindOpmFinalDir ()` finds no sibling checkout — identical skip-when-absent semantics,
  and it actually runs under v3. Same decision the calibration fact (008) already made.
- **The slice's "(4,2) and (2,2)" largest-diff positions don't match the data — followed
  the data (base-protocol skepticism rule).** The Δ = M_{QZ+LR} − M_LR·M_QZ matrix's two
  largest |entries| are (4,4)=0.268065 and (4,2)=0.267826 (1-indexed) — near-tied at
  ≈0.268; (2,2)=0.193 is only the 4th-largest. The reference `step2_linear/report.md` Δ
  matrix confirms this. The test asserts the ACTUAL top-2 positions {(3,3),(3,1)} 0-indexed
  ( = {(4,4),(4,2)} 1-indexed) as an order-independent set, and documents the discrepancy in
  code + here. The §8 acceptance scalars (‖Δ‖_F, mean|Δ|, max|Δ|) are asserted as named.
- **The 2e-3 band is a local tolerance, not `allowedDiff`.** `MatrixComparison.allowedDiff`
  is 1e-5 (for the always-run synthetic facts). The data-backed §7.2 matrix compare reuses
  the element-by-element loop STRUCTURE but with the slice-mandated `dataMatrixTol = 2e-3`.
  (Observed agreement is far tighter — F# reproduces numpy's unique full-rank solution — but
  the band matches the slice and stays robust to any cross-implementation drift.)
- **CPL-CPL day1 vs day2 gain: two files, two calibrations.** `familyGain` dispatches on
  `Family`, and CPL-CPL has two acquisition sessions with different rules (time-interpolated
  vs single block). Since day1 (`cpl_cpl_day1_main.csv`) and day2
  (`cpl_cpl_day2_corrections.csv`) are DISTINCT files each mapping to the one CPL-CPL family,
  each file carries its own `GainCalibration` (`day1Cal` with `Day1Interp`, `calibration`
  with `Day2Single`). Every science file maps to exactly one family, so a per-file
  `prepareRow` closure is the clean shape.
- **The CPL-LP split time comes from `bullshit_checks.csv`.** The reference reads
  `bullshit_checks_comparison.json[0]`; the earliest `bullshit_checks.csv` capture is that
  same first BullshitCheck (2026-05-31T01:10:40Z), and `bullshit_checks.csv` carries the
  five columns `parseMuellerCsv` requires, so it loads through the same file-backed proxy.
- **AIR gain uses raw rows; Stage-1 calibration uses normalized averaged traces.** Two
  different reductions of the AIR#0 rows: `fitAirGain` (Stage-3) fits `signal_dark_sub ≈
  gain·(a·s)` over the raw per-capture rows (no averaging); `reduceAirTraces` (Stage-1)
  averages by dial + normalizes. Kept distinct, matching the reference.
- **System-prompt path drift (same as 003–010).** The task file's "System prompt" line
  points at `C:\GitHub\AI-Strategy-Generator\wire_worker.system-md`, which does not exist;
  the real file is `.../src/ai_strategy_generator/multistep/wire_worker.system-md` (a thin
  `WIRE` delta over `.../multistep/arc-runner.system-md`). Read those.
- **Prior attempt (attempt-01) only edited the top-of-file doc block, then exited non-zero.**
  The §7.2 member itself was never written. This attempt completed it.
- **Line endings.** The edited `.fs` file is pure LF (verified `od -An -tx1 | grep -c 0d`
  → 0).
