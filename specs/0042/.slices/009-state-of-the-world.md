# State of the world — spec 0042, slice 009 (IMPLEMENT — Stage-3 signal reduction)

## Where we are

Slice 009 is the ninth slice of arc 0042 and continues Part D (Glue). Slices
001–002 landed the pure primitives (`Retardance` / `retarderMueller`, the
column-major `kron4` / `vecColumnMajor` / `frobeniusDiff` core); 003–004 the
least-squares `MuellerSolverProxy` seam + the real MathNet SVD; 005–006 the
`MuellerDataProxy` CSV-load seam + the real file-backed store; 007 the per-row
glue (label parse, family / matrix-kind, source & analyzer models, effective-state
builders); 008 the Stage-1 AIR calibration fits. This slice ports the reference
**Stage 3** (`matrix_fit_linear.py:assign_gain_model`) — the dark subtraction, the
per-family AIR scalar-gain fit, and the signal correction — into
`OpticalConstructor.Domain.MuellerReconstruction` as tiny pure functions. These
turn a raw averaged intensity into the `signal_corrected` target the Stage-2
`buildDesign` / `reconstruct` assembly (slice 010) will fit; the assembly itself
remains a later slice.

## What's working

- Add `darkSubtract` (`avg − dark_mean`), `airIdentityPred` (`a·s`, the ideal AIR#0
  prediction), and `scalarGain` (`Σ(s·m)/Σ(m²)`, the closed-form 1-parameter AIR
  gain fit) — the three elementary Stage-3 reductions, each a 1:1 mirror of the
  reference.
- Add `familyGain` — the per-family AIR-gain dispatch: `lp_lp` / `lp_cpl` the mean
  of two repeats, `cpl_lp` split at the first BullshitCheck timestamp, `cpl_cpl`
  day1 time-interpolated between its two AIR blocks and day2 the single block.
- Add `correctSignal` (`(avg − dark_mean) / gain`) composing `darkSubtract` with
  the gain divide, plus the `DarkMean` elevated single-case DU and the
  `GainCalibration` data (`RepeatPairGain` / `CplCplGain` / `SplitGain`).
- Add one always-run `[<Fact>]` pinning all five functions on known values and
  `familyGain` firing each per-family rule deterministically (fixed-literal
  timestamps, no ambient clock).

## Tests

Gate roster for this slice (from `009.gates`): `build`, `unit-tests`
(`berreman_unit_tests`), `constructor-unit-tests` (`constructor_unit_tests`).

Per the IMPLEMENT worker's **Invariant 6 (the worker acts; it runs no checks)**,
this worker did NOT execute the gates — the deterministic gate engine runs them
after exit. New coverage in `BerremanTests/MuellerReconstructionTests.fs`:

- **the Stage-3 reduction fact (always-run)** — `scalarGain` on a proportional
  pair (`signal = 2·model` → gain 2) and a general pair (matches the closed form);
  `darkSubtract` and `correctSignal` on known values with the ≈ 869.666 dark mean
  passed in as data; `airIdentityPred` on a known `(s, a)` dot; and `familyGain`
  selecting each per-family rule — `LpLp`/`LpCpl` means, `CplLp` pre/post split at
  the timestamp, `CplCpl` day2 single, and `CplCpl` day1 interpolation with its
  midpoint / endpoint / clamp-below / clamp-above cases.

The change is additive (new Domain functions + one new fact), so it only raises
the `berreman_unit_tests` count and leaves `constructor_unit_tests` unchanged. All
existing MuellerReconstruction facts remain green.

```yaml
gates:
  berreman_unit_tests: 0
  constructor_unit_tests: 0
```

(The gate engine records the real captured counts post-exit; the baseline for a
`count_at_least` gate is the prior green step's checkpoint, not a value the worker
authors — 0 is the safe floor. The new fact only adds to the count.)

## Architecture

- **Five tiny pure functions, one per reference Stage-3 step.** `darkSubtract`
  (↔ `signal_dark_sub`), `airIdentityPred` (↔ `identity_prediction`), `scalarGain`
  (↔ `fit_scalar_gain`), `familyGain` (↔ the `assign_gain_model` per-glue-file
  dispatch), `correctSignal` (↔ `signal_corrected`) — each a concrete elevated
  signature mirroring the reference 1:1. `correctSignal` reuses `darkSubtract` so a
  single dark-mean seam governs both readouts.
- **`familyGain` keyed by `Family`, data in `GainCalibration`.** The mandated
  signature is `familyGain (family : Family) (cal : GainCalibration)
  (t : DateTimeOffset)`. Because `Family` is 4-case but the reference has five gain
  rules (CPL-CPL splits into day1/day2), the CPL-CPL data is a `CplCplGain`
  (`Day1Interp | Day2Single`) DU the caller resolves per acquisition session —
  mirroring the reference's per-`source_glue_file` dispatch. Both mandated
  parameters carry weight: `family` selects the branch, `cal` supplies the data.
- **Elevation where it belongs.** `DarkMean` is elevated (a single-case DU with
  `.value`). Gains stay bare `float` because the slice fixes `scalarGain … : float`
  and `familyGain … : float` (a gain is a dimensionless scale readout, exactly as
  slice 008 fixed `CosineFit`'s bare floats); timestamps stay bare
  `System.DateTimeOffset`, matching `MuellerRawRow.capturedAt` and the mandated `t`.
- **The three procedure-level constants stay as caller data.** `dark_mean`
  (≈ 869.666), the split timestamp, and the excluded contaminated point are DATA
  the test supplies — none is embedded in these pure functions (the excluded point
  is a `buildDesign` concern in slice 010, not a Stage-3 function at all).

## Deferred

- **`buildDesign` / `reconstruct` / `cascadeProduct`** (slice 010) — assembling the
  n×16 design from `designRow` per science row with the `correctSignal` target,
  excluding AIR rows and the one contaminated point, solving through the proxy,
  reshaping column-major, and the QZ-first cascade product — remain the next slice.
- **The composition-root wiring** (slice 011) — instantiating the real proxies,
  resolving the OPM data relative to `__SOURCE_DIRECTORY__`, and running Stage 1 →
  2 → 3 end-to-end against the §7.2 result — remains the final slice.
- **Consuming `familyGain` per science row** — `buildDesign`'s use of `familyGain`
  (which day's `CplCplGain` a given CPL-CPL row takes) is slice 010's wiring
  concern; this slice ships the pure rule, not its per-row application.

## Gotchas

- **`familyGain` signature vs the 5-rule reference (recorded choice).** `Family` is
  4-case, the reference has 5 gain rules; the CPL-CPL day1/day2 split is carried in
  the `cplCpl : CplCplGain` field of `GainCalibration`, resolved by the caller per
  acquisition session (the reference keys on `source_glue_file`). This honours the
  exact mandated signature while covering every rule.
- **Ports stay TOTAL where the reference raises.** `fit_scalar_gain` raises on a
  degenerate `dot(model,model) ≤ 0` and day1 raises on a non-positive AIR interval;
  the F# `scalarGain` returns the IEEE division result and `familyGain` uses weight
  0 on a non-positive interval (CLAUDE.md forbids throwing across a public
  boundary). Neither degeneracy arises on real AIR data.
- **`airIdentityPred` single-case-DU let-unwrap is warning-free** — the same
  `let (StokesVector sv) = s` pattern `kron4` uses; `--warnaserror+:25` (FS0025
  incomplete matches) does not fire on a single-case union let-deconstruct.
- **`family` parameter shadows the `family` classification function** — harmless
  (never called in `familyGain`'s body; F# does not warn on value shadowing, so the
  zero-warnings rule is not tripped).
- **System-prompt path drift (same as 003–008).** The task file's "System prompt"
  line points at `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md`, which
  does not exist; the real file is
  `.../src/ai_strategy_generator/multistep/implement_worker.system-md` (a thin
  `IMPLEMENT` delta over `.../multistep/arc-runner.system-md`). Read those.
- **Line endings.** Both edited `.fs` files are pure LF (verified with
  `od -An -tx1 … | grep -c ' 0d'` → 0).

## Changelog

- 2026-07-16 — slice 009 (IMPLEMENT) attempt 01: port the reference Stage-3 signal
  reduction (`matrix_fit_linear.py:assign_gain_model`) into
  `MuellerReconstruction.fs` as pure functions — `darkSubtract`, `airIdentityPred`
  (a·s), `scalarGain` (Σ(s·m)/Σ(m²)), `familyGain` (per-family AIR-gain dispatch:
  lp_lp/lp_cpl mean, cpl_lp split, cpl_cpl day1-interp / day2-single), and
  `correctSignal` ((avg − dark)/gain) — with the `DarkMean` elevated DU and the
  `GainCalibration` (`RepeatPairGain` / `CplCplGain` / `SplitGain`) data. Add one
  always-run `[<Fact>]` pinning all five and every per-family rule.
