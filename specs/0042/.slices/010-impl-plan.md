# Impl-plan — spec 0042, slice 010 (IMPLEMENT — Stage-2 assembly + reconstruction + cascade)

## Goal

Part E's numeric core: in `OpticalConstructor.Domain.MuellerReconstruction` assemble the
reconstruction as three tiny pure functions on top of the step-002/007/009 primitives —
`buildDesign` (n×16 design + `signal_corrected` target), `reconstruct` (solve through the
`MuellerSolverProxy`, reshape column-major), and `cascadeProduct` (`M_LR · M_QZ`, QZ-first) —
plus `BerremanTests` facts on a synthetic full-rank design. The composition-root wiring against
the real OPM data is slice 011; this slice ships the assembly the wiring calls.

## Approach

1. **`ReconstructedMatrix`** — elevate the solved result to a record `{ matrix : MuellerMatrix;
   rank : int; rmse : float }` (never a bare `float[]`), carrying the un-vec'd Mueller matrix and
   the solver's rank / rmse observability.

2. **`reconstruct (solver) (x) (y)`** — call `solver.solveLinearLeastSquares x y`, `Result.map` the
   `LeastSquaresSolution` into a `ReconstructedMatrix` via `muellerOfVecColumnMajor sol.solution`
   (the step-002 column-major inverse), carrying `sol.rank` / `sol.rmse`. Errors pass through the
   `SolverError` channel unchanged — no throw.

3. **`cascadeProduct (mLr) (mQz) = mLr * mQz`** — the engine `MuellerMatrix` `*` operator
   (Fields.fs:651). Beam hits QZ first, then LR, so the cascade Mueller product is `M_LR · M_QZ`.

4. **`buildDesign`** — assemble the design + target. The mandated `(rows : MuellerRawRow list)
   (kind : MatrixKind)` is under-specified: a raw CSV row alone cannot yield a design row (needs the
   calibrated effective states) nor a corrected signal (needs the gain + dark mean), and spec §0
   pins the excluded-point / dark-mean / split constants to `BerremanTests`, not the pure core.
   So `buildDesign` takes two more inputs threaded from the (test-owned) calibration:
   - `prepareRow : MuellerRawRow -> StokesVector * RealVector4 * float` — the per-row Stage-2+3
     projection (effective source state, effective analyzer row, corrected signal). The caller
     (slice 011) closes over the calibrated models + gain calibration + dark mean.
   - `excluded : ExcludedPoint list` — the contaminated points to drop, as DATA (spec §0).
   `buildDesign` filters to rows whose `matrixKind (parseExperiment experiment) = kind`, drops AIR#0
   rows and any `excluded` point, then maps each kept row through `prepareRow` and builds one
   `designRow s a` per row (the n×16 design) paired with its corrected signal (the target). This
   mirrors the reference `solve_single_object` (matrix_fit_linear.py:169), which filters an
   already-glued+reduced frame and extracts the `lincoef` / `signal_corrected` columns.

5. **`ExcludedPoint`** — an elevated record `{ experiment : string; description : Angle;
   captureIndex : int }` (matrix_glue.py `EXCLUDED_POINTS`), matched on experiment + captureIndex +
   description (angle tolerance); supplied as test data.

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs` — append the
  step-010 section (`ExcludedPoint`, `ReconstructedMatrix`, `buildDesign`, `reconstruct`,
  `cascadeProduct`).
- `Berreman/BerremanTests/MuellerReconstructionTests.fs` — add always-run facts: (a) `reconstruct`
  recovers a seeded asymmetric `MuellerMatrix` from a synthetic full-rank (rank-16) kron design at
  rank 16; (b) `cascadeProduct mLr mQz` equals `mLr * mQz`; (c) `buildDesign` keeps only the
  requested kind, dropping AIR + the excluded contaminated point.

## Risks

- **`buildDesign` signature deviation** — the mandated 2-arg shape can't be genuine (physics needs
  the calibrated projection; spec §0 forbids embedding the excluded point in the core). Recorded in
  the impl-log Gotchas; the two added inputs are the minimal, most-reference-faithful way to keep
  the pure Domain function honest while the calibration lives in `BerremanTests`.
- **Rank threshold** — MathNet `svd.Rank` must report 16 on the synthetic design; a kron of 4
  independent Stokes ⊗ 4 independent analyzer rows is exactly rank 16 and well-conditioned.
- **LF line endings** — verify no CRLF churn after editing.

## Gate roster (010.gates)

`build`, `unit-tests` (`berreman_unit_tests`), `constructor-unit-tests` (`constructor_unit_tests`).
Additive change (new Domain functions + three new always-run facts) — only raises
`berreman_unit_tests`, leaves `constructor_unit_tests` unchanged. Per Invariant 6 the worker does
not run the gates.
