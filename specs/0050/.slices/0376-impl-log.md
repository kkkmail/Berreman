# Step 0376 — impl log

## Progress

- Read the task, both system prompts, the project prompt, the slice and its gates snapshot. No operator note or ruling
  is attached.
- Read the seams:
  - `StudyForward.fs`: step 375's `RecordedReading`, `predictedReadingOf` and `StudyForwardError`;
  - `MuellerInverse.fs`: `InversionError`, `ResidualVector`, `normalized`, `observationResidual` and `residualVector`;
  - `MuellerFitting.fs`: `throughputResidualFor` and its local `logThroughput`;
  - `StudyForwardTests.fs`.

  None of the new names (`throughputResidual`, `RowResidual`, `RowResidualErr`, `residualAgainst`) exists in the repo
  or in STL.
- `MuellerInverse.fs`: added `throughputResidual measured model` after `residualVector`, the one entry
  `ln m₀₀(model) − ln m₀₀(measured)`. Its refusal is `normalized`'s own: the measured matrix is checked first, then the
  model. So the non-positive / non-finite test is not copied a third time.
- `MuellerFitting.fs`: `throughputResidualFor` takes each observation's term from `throughputResidual o.measured model`.
  An `Error` gives `[| 1.0e3 |]`, and the per-observation arrays are concatenated. The local `logThroughput` is
  deleted, and the doc names `throughputResidual` and cites S11.2 beside spec 0044's manual. Domain and Optimization
  built with 0 errors.
- `StudyForward.fs`:
  - `RowResidualErr of error : InversionError` joins `StudyForwardError`. It renders
    `the residual cannot be formed: <cause>`, each `InversionError` case's payload rendered inline (the m₀₀; the
    rotation reason; the two counts).
  - After `predictedReadingOf`: the record `RowResidual = { elements : ResidualVector; throughputOpt : ResidualVector option }`
    and `RecordedReading.residualAgainst`.
- `StudyForwardTests.fs`:
  - the error-render fact is widened by `RowResidualErr` over all three `InversionError` cases;
  - three test-local helpers (`scaledBy`, `entriesOf`, `rowResidualOf`) and the four new facts are appended.
- Built the solution, then ran constructor-unit-tests (whole), unit-tests (the selected class), elevation and the
  twenty s37 invariants. All passed (see Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerInverse.fs` (+7 / -0).
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/MuellerFitting.fs` (+7 / -9).
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/StudyForward.fs` (+55 / -0).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyForwardTests.fs` (+109 / -0).

Every edit is inside the step's authorized `edits`, and no project file changed. All four files and this round's
outputs are LF: `git diff --numstat` equals `--ignore-cr-at-eol`, and a byte count finds no CR.
`specs/0050/.manifest.state.json` was already modified when the round started (supervisor state). This round did not
touch it.

## Testing state

All run locally after the final edit (Release x64):

| Gate | Result | Duration | Deadline |
|---|---|---|---|
| build (`dotnet build Berreman.slnx -c Release -nologo -v:m`) | exit 0, `0 Error(s)`, no lowercase `error` in the log; the 12 warning lines are all NU1902 advisories (exempt) | 45 s | 600 s |
| constructor-unit-tests (the gate's command, whole suite) | `Test run summary: Passed!`, 1640 succeeded (checkpoint 375: 1636; +4), 0 failed, 0 skipped; the four new facts and the widened error-render fact are `Passed` in the TRX under exactly the authorized ids | 74 s | 180 s |
| unit-tests (`--filter-method "BerremanTests.AbsorbingBiaxialInverseTests.*" --filter-method "BerremanTests.AbsorbingBiaxialInverseTests"`) | `Test run summary: Passed!`, 7 of 7 succeeded, 0 failed, 0 skipped: the class's count recorded at step 336 | 189 s | 7200 s |
| elevation (`ASG_ARC_BASE_COMMIT` = `.arc-base-json`'s `f395597c`, ELEV004 excluded) | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, exit 0; Domain and Optimization `findings=0` | 219 s | 600 s |
| twenty s37 invariants (the snapshot's `section_invariants --find-nothing` commands) | each exits 0 with `INVARIANT HOLDS` (20 of 20) | a few seconds each | 300 s |

**Red before green.** The four new facts call `residualAgainst`, `RowResidual`, `RowResidualErr` and
`throughputResidual`, which this step creates. Before the production code they could not compile, which is their red.
Each fact makes discriminating checks:

- **signal:** 2.5 against 2 gives exactly [0.5], with no throughput.
- **Ψ and Δ:** Ψ 30° against 28° gives 2°, and Δ 179° against −179° gives −2°, not 358°, both within 1e-12 in
  radians.
- **Mueller:** an ideal LP at 0.8 against the same at 0.5 gives exactly `observationResidual`'s 15 entries, each
  within 1e-15 of zero, and the throughput [ln 0.8 − ln 0.5] within 1e-15. A measured retarder (axis 20°,
  retardance 70°) against the same prediction gives `observationResidual`'s 15 entries, at least one above 0.1, and
  `throughputResidual`'s ratio.
- **refusals:**
  - all six mismatched-kind pairs answer `ReadingMismatchErr`, naming both kinds;
  - a measured m₀₀ of 0 answers `RowResidualErr (NonPositiveIntensityErr 0)`, and a predicted m₀₀ of −0.5 answers it
    with −0.5;
  - with both bad, the measured matrix is named first;
  - `throughputResidual` alone refuses in the same order.

**Bit-identity of `throughputResidualFor`.** `throughputResidualFor` reaches the throughput term only after
`residualVector` succeeded, which normalized both matrices of every observation. So both m₀₀ are positive and finite,
and `throughputResidual` evaluates `log (model.element 0 0) - log (measured.element 0 0)`. That is the same
expression the deleted `predicted - measured` over `logThroughput` evaluated. The width stays 16 per observation,
because each term is one entry, and `1.0e3` stays the penalty. `AbsorbingBiaxialInverseTests` passes 7 of 7,
unchanged.

Every requirement of the slice landed this round; nothing is deferred. Every duration is within its deadline.

## Artifacts

- `specs/0050/.artifacts/0376-build.log`
- `specs/0050/.artifacts/0376-constructor-unit-tests.log`
- `specs/0050/.artifacts/0376-unit-tests.log`
- `specs/0050/.artifacts/0376-elevation.log`
- `specs/0050/.artifacts/0376-invariants.log` (each decoded `git grep` and its outcome)
- `specs/0050/.artifacts/0376-invariant-commands.txt` (the twenty commands from the step's gates snapshot)

## Gotchas

- **The refusal reuses `normalized`.** The how-to asks for `NonPositiveIntensityErr` "when it is not above zero or
  not finite (the refusal `normalized` makes)". `throughputResidual` calls `measured.normalized`, then
  `model.normalized`, and discards both matrices. This keeps one copy of the check, which `normalized` and
  `depolarizationIndex` already repeat. It costs one more 4×4 normalization per observation per residual
  evaluation, which is negligible beside the Berreman solve. Nothing it computes enters the result, so the numbers
  are unchanged.
- **Kind names are rendered by a local closure.** "No other member is added", so `ReadingMismatchErr` names both
  kinds through a `kindOf` closure inside `residualAgainst` ("a signal", "Ψ and Δ", "a Mueller matrix"). No
  `RecordedReading` render member was added.
- **`InversionError` has no render member.** `RowResidualErr` renders each case's payload inline inside
  `StudyForwardError.describe`: the m₀₀; the rotation's own reason; the measured and model counts.
- **The mismatch arms name each first kind.** They are written per first kind, not as a wildcard, so a new
  `RecordedReading` case shows up as an incomplete match.
- **Δ wraps with `IEEERemainder`.** `IEEERemainder (x, 2π)` lands in [−π, π]. At exactly ±π it rounds to even, which
  is still a half-turn of the right size.
- **Test-local helpers.** `MuellerMatrix` has no scalar product, so the tests scale a matrix through
  `RealMatrix4x4.create` (`scaledBy`). Negative literal arguments are parenthesized.
