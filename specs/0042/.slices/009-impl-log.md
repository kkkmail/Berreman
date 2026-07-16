# Impl-log — spec 0042, slice 009 (IMPLEMENT — Stage-3 signal reduction)

## Progress

- [x] Read protocol (`implement_worker.system-md` + `arc-runner.system-md`),
      project prompt (`arc-runner.user-md`), slice spec `009.slice-md`, prior
      SoW (008).
- [x] Locate reference Stage-3: `matrix_fit_linear.py:assign_gain_model`
      (`identity_prediction` :60, `fit_scalar_gain` :66, the per-glue-file gain
      dispatch :92–:154, `signal_corrected` :160).
- [x] Append the Stage-3 pure functions + `DarkMean` / `GainCalibration` data to
      `MuellerReconstruction.fs`.
- [x] Add the always-run `[<Fact>]` to `MuellerReconstructionTests.fs`.
- [x] Verify LF line endings (`od -An -tx1 … | grep -c ' 0d'` → 0 on both files).
- [x] No name clashes (grep of the new symbols across Domain/Storage/Core — none).
- [x] Write the state-of-the-world.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`
  — new Stage-3 section (after `sourceCplFromLpFrame`): `DarkMean` (elevated
  single-case DU), `darkSubtract`, `airIdentityPred`, `scalarGain`,
  `RepeatPairGain` / `CplCplGain` (`Day1Interp` | `Day2Single`) / `SplitGain` /
  `GainCalibration`, `familyGain`, `correctSignal`.
- `Berreman/BerremanTests/MuellerReconstructionTests.fs` — one new always-run
  `[<Fact>]` pinning all five Stage-3 functions and each per-family `familyGain`
  rule with fixed-literal timestamps.

## Testing state

Per Invariant 6 (the worker acts; it runs no checks) this worker did **not**
execute the gates — the deterministic gate engine runs `build`, `unit-tests`
(`berreman_unit_tests`) and `constructor-unit-tests` (`constructor_unit_tests`)
after exit. The change is additive (new Domain functions + one new fact), so it
only raises the `berreman_unit_tests` count and leaves `constructor_unit_tests`
untouched. `commit_ready: true` — every function the slice enumerates is
implemented and pinned this round.

## Artifacts

None — a pure Domain + test slice (no captured logs/screenshots/dumps).

## Gotchas

- **`familyGain` signature vs the 5-rule reference (recorded choice).** The
  mandated signature is `familyGain (family : Family) (cal : GainCalibration)
  (t : DateTimeOffset)`. `Family` is 4-case but the reference has FIVE gain rules
  (CPL-CPL splits into a day1 interpolation and a day2 single block, dispatched in
  the reference on `source_glue_file`). Resolution: `family` is the dispatch key
  and `GainCalibration` bundles each family's data, with `cplCpl : CplCplGain` a
  `Day1Interp | Day2Single` DU the caller resolves per acquisition session. This
  uses BOTH mandated parameters meaningfully and keeps each rule pure/total.
- **Ports stay TOTAL where the reference raises (CLAUDE.md: no throw across a
  public boundary).** `fit_scalar_gain` raises on a degenerate
  `dot(model,model) ≤ 0`; day1 raises on a non-positive AIR interval. The F#
  `scalarGain` returns the IEEE division result, and `familyGain` uses weight 0
  on a non-positive interval. Neither degeneracy arises on real AIR data (the AIR
  model is well-conditioned; the day1 interval is strictly positive).
- **Gains stay bare `float`; timestamps stay bare `System.DateTimeOffset`.** The
  slice fixes `scalarGain … : float` and `familyGain … : float`, so a gain (a
  dimensionless scale readout) is bare, mirroring slice 008's bare `CosineFit`;
  it is elevated where the corrected signal is consumed downstream. Timestamps
  match `MuellerRawRow.capturedAt` and the mandated `(t : System.DateTimeOffset)`.
  `DarkMean` IS elevated (a single-case DU with `.value`) per the slice.
- **The three procedure-level constants stay as caller data.** `dark_mean`
  (≈ 869.666) flows in as `DarkMean`; the split timestamp as `SplitGain.splitTime`;
  the excluded contaminated point (`LP-(LR#90)-CPL-2`, description 140,
  capture_index 17) is a `buildDesign` concern (slice 010), not a Stage-3
  signal-reduction function — so none of these constants are embedded here.
- **`airIdentityPred` unwraps a single-case DU in a `let` — warning-free.**
  `let (StokesVector sv) = s` is the same pattern `kron4` (line 65) uses; the
  build's `--warnaserror+:25` (incomplete matches, FS0025) does not fire on a
  single-case union let-deconstruct, so this is safe.
- **`family` parameter shadows the `family` classification function.** Harmless —
  `familyGain` never calls the `family` function in its body, and F# does not warn
  on value shadowing (so the zero-warnings rule is not tripped).
- **System-prompt path drift (same as 003–008).** The task file's "System prompt"
  line points at `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md`,
  which does not exist; the real file is
  `.../src/ai_strategy_generator/multistep/implement_worker.system-md` (a thin
  `IMPLEMENT` delta over `.../multistep/arc-runner.system-md`).
- **Line endings LF** — verified on both edited files (zero `0d` bytes).

## Changelog

- 2026-07-16 — slice 009 (IMPLEMENT) attempt 01: port the reference Stage-3 signal
  reduction (`matrix_fit_linear.py:assign_gain_model`) into
  `MuellerReconstruction.fs` as pure functions — `darkSubtract`,
  `airIdentityPred` (a·s), `scalarGain` (Σ(s·m)/Σ(m²)), `familyGain` (per-family
  AIR-gain dispatch: lp_lp/lp_cpl mean, cpl_lp split, cpl_cpl day1-interp/day2),
  and `correctSignal` ((avg − dark)/gain) — with the `DarkMean` elevated DU and
  the `GainCalibration` (`RepeatPairGain` / `CplCplGain` / `SplitGain`) data. Add
  one always-run `[<Fact>]` pinning all five and every per-family rule.
