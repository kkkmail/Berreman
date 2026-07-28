# Impl-plan — spec 0042, slice 009 (IMPLEMENT — Stage-3 signal reduction)

## Goal

Port the reference **Stage-3** signal reduction (the AIR-gain / dark-subtract
half of `matrix_fit_linear.py:assign_gain_model`) into
`OpticalConstructor.Domain.MuellerReconstruction` as tiny pure functions, plus
an always-run `BerremanTests` fact that pins each one deterministically.

## Reference mapping (matrix_fit_linear.py)

- `signal_dark_sub = avg_total - dark_mean`      → `darkSubtract (darkMean : DarkMean) (avgTotal : float) : float`
- `identity_prediction` = `einsum(a,s)` per row  → `airIdentityPred (s : StokesVector) (a : RealVector4) : float` = a·s
- `fit_scalar_gain = dot(sig,mdl)/dot(mdl,mdl)`  → `scalarGain (signal : float[]) (model : float[]) : float`
- the per-`source_glue_file` gain dispatch       → `familyGain (family : Family) (cal : GainCalibration) (t : DateTimeOffset) : float`
  - `lp_lp` / `lp_cpl`   → mean of the two AIR repeats
  - `cpl_lp`            → split at the first BullshitCheck timestamp (pre / post)
  - `cpl_cpl` day1      → time-interpolated between its two AIR blocks
  - `cpl_cpl` day2      → the single AIR block
- `signal_corrected = signal_dark_sub / gain`    → `correctSignal (darkMean : DarkMean) (avgTotal : float) (gain : float) : float`

## Files to modify

1. `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`
   — append a Stage-3 section: `DarkMean` (elevated single-case DU), the five
   pure functions above, and the supporting `GainCalibration` data
   (`RepeatPairGain` / `CplCplGain` / `SplitGain`). Gains stay bare `float` by the
   slice's explicit signatures (`scalarGain … : float`, `familyGain … : float`),
   mirroring slice 008's bare `CosineFit`; timestamps stay bare
   `System.DateTimeOffset`, matching `MuellerRawRow.capturedAt` and the mandated
   `familyGain … (t : System.DateTimeOffset)`.
2. `Berreman/BerremanTests/MuellerReconstructionTests.fs` — add one always-run
   `[<Fact>]` pinning `scalarGain` on a known signal/model pair, `darkSubtract` /
   `correctSignal` / `airIdentityPred` on known values, and `familyGain` firing
   each of the five per-family rules deterministically (fixed-literal timestamps,
   no ambient clock).

## Design choice — familyGain signature

The mandated signature is `familyGain (family : Family) (cal : GainCalibration)
(t : DateTimeOffset)`. `Family` is 4-case but the reference has 5 gain rules
(CPL-CPL splits into day1/day2). Resolution: `family` is the dispatch key;
`GainCalibration` is a bundle carrying each family's data, with `cplCpl : CplCplGain`
a `Day1Interp | Day2Single` DU so the caller supplies whichever CPL-CPL session
applies (mirrors the reference's per-glue-file dispatch). This uses both mandated
parameters meaningfully and keeps every rule pure and total.

## Risks

- **Total vs the reference's raises.** `fit_scalar_gain` raises on a degenerate
  `dot(model,model) ≤ 0`; day1 raises on a non-positive AIR interval. CLAUDE.md
  forbids throwing across a public boundary, so the port stays total (the IEEE
  division result / weight 0 on a degenerate interval). Real AIR data never hits
  either. Record in Gotchas.
- **Zero warnings.** `family` (param) shadows the `family` classification
  function but is never called inside `familyGain`; the single-case
  `let (StokesVector sv) = s` unwrap is warning-free (kron4 already does it).
- **Line endings LF.** Verify with `od` after editing.
