# Impl-log — spec 0042, slice 010 (IMPLEMENT — Stage-2 assembly + reconstruction + cascade)

## Progress

- [x] Read system prompt (`implement_worker.system-md` + base `arc-runner.system-md`), project
      prompt (`arc-runner.user-md`), slice spec (`010.slice-md`), and the dependency context
      (slices 002/004/007/009 SoW, the reference `matrix_fit_linear.py` / `matrix_glue.py`).
- [x] Wrote impl-plan.
- [x] Implement `ReconstructedMatrix`, `ExcludedPoint`, `buildDesign`, `reconstruct`,
      `cascadeProduct` in `MuellerReconstruction.fs`.
- [x] Add always-run `BerremanTests` facts (synthetic full-rank reconstruction, cascade identity,
      buildDesign filtering).
- [x] Verify LF line endings.
- [x] Write state-of-the-world.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs` — appended the
  step-010 section: `ExcludedPoint`, `ReconstructedMatrix`, `buildDesign`, `reconstruct`,
  `cascadeProduct`.
- `Berreman/BerremanTests/MuellerReconstructionTests.fs` — added three always-run `[<Fact>]`s.

## Decisions

- **`buildDesign` takes a per-row projection + excluded-points data** beyond the mandated
  `(rows) (kind)`. A `MuellerRawRow` alone cannot yield a design row (needs calibrated effective
  states) nor a corrected signal (needs gain + dark mean); spec §0 pins the excluded-point /
  dark-mean / split constants to `BerremanTests`, not the pure core. So the calibrated per-row
  physics arrives as `prepareRow : MuellerRawRow -> StokesVector * RealVector4 * float` and the
  contaminated points as `excluded : ExcludedPoint list` (test-owned data). This is the minimal,
  most-reference-faithful shape (mirrors `solve_single_object`, which filters an already
  glued+reduced frame). See Gotchas.
- **`buildDesign` computes `designRow s a` per kept row** (not delegated) so the mandated
  "designRow per science row" is literally in the function; `prepareRow` supplies the effective
  `(s, a)` and the corrected signal only.
- **Synthetic full-rank design** in the acceptance test = 4 independent Stokes states ⊗ 4
  independent analyzer rows = 16 linearly independent `designRow`s (column rank 16), target seeded
  as `y = X · vec_F(M_seed)`, so the unique solution un-vecs back to the asymmetric seed at rank 16
  with ~zero residual. Asymmetric seed catches a column/row-major transpose bug.

## Testing state

Per Invariant 6 (the worker acts; it runs no checks) the deterministic gate engine runs
`build` / `unit-tests` / `constructor-unit-tests` after exit. Change is additive: new Domain
functions + three new always-run facts, raising `berreman_unit_tests`, leaving
`constructor_unit_tests` unchanged. `commit_ready: true`.

## Artifacts

None beyond the source edits and these round outputs.
