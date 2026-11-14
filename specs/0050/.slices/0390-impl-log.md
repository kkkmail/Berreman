# Step 0390 — impl log

## Progress

- 2026-11-13: read the task, the system and project prompts, the slice, steps 386–389's records and the code
  (`StudyEngine.fs`, `StagedMuellerPath.fs`, `MuellerReconstruction.fs`, `StudyForward.fs`, `ItemEvaluation.fs`,
  `FitRuns.fs`, `Studies.fs`). Wrote the impl plan.
- `StudyForward.fs`: moved `readingAt`'s per-row rules into `BoundExperiment.waveLengthAt` / `orientationAt`;
  `readingAt` calls them (a move, no behaviour change).
- `StagedMuellerPath.fs`: factored the scalar AIR gain arithmetic into `airAnchorGainOver` and the row projection into
  `projectCaptureRowFrom`; `airAnchorGainOf` / `projectCaptureRow` now call them with the same operands in the same
  order.
- `StudyEngine.fs`:
  - added `SessionGainFitErr`, `ObjectSolveFitErr`, `ParameterValue.airGain`, `MuellerMatrix.rowMajorValues` and
    `StagedStudy.reconstructLinear`;
  - moved `captureRowsOf` out of `calibrateLinear`, and `fittedFollowers` / `bearsUnknown` out of `engineWith`
    (`fittedFollowersIn`, `bearersAmong`).
  - The Optimization project builds with 0 errors.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StudyEngine.fs` (authorized)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs` (authorized)
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StagedMuellerPath.fs` (deviation, see Decisions)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/StudyForward.fs` (deviation, see Decisions)

## Decisions

1. **The projection reads the run law's own per-row rules, so `StudyForward.fs` changes (a deviation from the
   authorized edits).**
   - The brief: the source state and fixed matrices "MUST come from the same SourceEmission, ItemModelChoice.boundWith,
     BoundOpticalElement evaluation and per-row dial/orientation/wavelength rules used by the run law".
   - Those rules were private closures of `BoundExperiment.readingAt`. Copying them into the engine would be a second
     implementation, which AGENTS.md calls a defect ("Reuse, never copy").
   - They are moved, unchanged, into `BoundExperiment.waveLengthAt` / `orientationAt`, and `readingAt` calls them.
   - Precedent: the 0259/0305/0353 judges accepted out-of-list edits that the brief itself forces. Every
     `StudyForwardTests` fact still passes.
2. **The AIR gain arithmetic and the row projection are factored in `StagedMuellerPath.fs`, the Optimization project
   in `touches` (a deviation from the edits list).**
   - `airAnchorGainOver` takes (s, a, row) projections; `airAnchorGainOf` calls it with the family models. The brief
     says "Factor and call that scalar gain calculation", and an AIR run behind any fixed optic must use its run-law
     projection, never an LP family.
   - `projectCaptureRowFrom` takes the arriving state and the analyzer row; `projectCaptureRow` calls it.
   - The brief's two sentences conflict: "every row is projected by `projectCaptureRow`" and "without constructing an
     LP/CPL SourceModel". So every study row goes through the projection `projectCaptureRow` is built on.
     `effectiveSource` / `effectiveAnalyzer` are therefore applied, as the brief requires.
   - Same operands in the same order, so the reference path is unchanged. `MuellerReconstructionTests` (the selected
     unit-tests) pass 25/25.
3. **P and k come from `StudyParameterLayout.assignmentFor` itself.**
   - The assignment is affine in the vector: each item value is a copy of an entry, of a leader, or the item's own.
   - So k is the object's sixteen coefficients at the zero vector, and P's column j is their change at the j-th unit
     vector, all in `vecColumnMajor` order.
   - Equality links (summed columns), cross-object leaders, fixed leaders and the owner rules therefore follow the
     study's actual rules. There is no second layout implementation, as the brief requires.
4. **Systems and solves.**
   - Grouping: fitted runs group by the unknown-bearing items on their detector path in light order (`List.groupBy`,
     first-run order). A run with none, such as an AIR run in a joint fit, joins no group.
   - Merging: lone-item groups whose P columns intersect merge into one system, rows in first-run order and columns
     in layout order.
   - Lone fully free item: when a system is one item fitting all sixteen of its own coefficients, it goes through
     `reconstruct solver` over `ofProjections`.
   - Any other lone system is reduced to `A·P`, `b − A·k` and goes through `solver.solveLinearLeastSquares`. Its
     matrix is `k + P·x`, assembled whole before `columnMajorMueller`. A position no column carries keeps k exactly.
   - Series: a group of two or more items is reconstructed whole by `reconstruct`. Its segment runs from the first
     unknown to the last, φ is the first one's rotation, and it fills no entry.
   - Each group gives one `ObjectMatrixFit`, with the rank and RMSE of the system it was solved in. χ² is Σ rmse² ×
     the group's own rows.
5. **Bound once per run at the start vector.**
   - Fixed items hold their own values there.
   - Unknown steps are never evaluated: the steps before the segment fold the emission (`detectorStokes`), and the
     steps after give the detector's S₀ row (`RunStep.productOf`).
   - A black box fits only sample slots, so it is an `ObjectStep`. `emittedInto`'s reference is therefore the same as
     the whole path's.
6. **Dark and gain.**
   - Dark: the run's readout from `boundWith` with `data.darkOf` of its session, else the detector's `darkOffset`.
   - A free session divides by the readout's held gain (`ParameterValue.airGain`).
   - An anchored or interpolated session builds each named AIR run's anchor from that run's own run-law projection
     and dark (`airAnchorGainOver`), then applies `ruleWith` and `gainAt` the row's capture time.
7. **A fixture deviation: the "CPL" analyzer families put the fixed quarter-wave plate ahead of CPL2.**
   - The shipped circular polarizer is LP → retarder in light order, so as an analyzer it reads no S₃.
   - Measured: with CPL2 alone, the four-family design reaches rank 12, and `reconstructLinear` answers
     `ObjectSolveFitErr (QZ, rank 12)`.
   - The plate, a known retarder model item, makes it the reference's circular analyzer: a retarder, then an LP.
   - Every other item is step 389's, with the five constants published at the truth's.
8. **Test oracle.**
   - Every observation and expectation comes from the ordinary run law (`boundWith` + `readingAt`), probing the
     black box with unit matrices.
   - So "projections MUST equal the ordinary run law at each row" is checked directly: the recorded design rows equal
     the run law's within 1e-12.

## Testing state

These are local runs on the final code. Each finished within its effective deadline.

- `build`: `dotnet build Berreman.slnx -c Release` exit 0, 0 errors, 24 s (deadline 600 s). The only warnings are 12
  NU1902 lines (exempt), and the log holds no `error`.
- `constructor-unit-tests`, whole suite: 1687/1687, 102 s (deadline 180 s).
  - The count is 1684 → 1687 (+3).
  - The TRX shows the three new ids and the widened describe fact `Passed` under their exact names.
- `unit-tests`, selected `BerremanTests.MuellerReconstructionTests`: 25/25, 2 s (deadline 7200 s).
- `elevation`, standalone, all 13 projects: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 225s` (deadline
  600 s).
  - The first run (222 s) found ELEV013P on the private `captureRowsOf`.
  - It is now the private extension member `ExperimentPrefit.captureRows`. The re-run is clean.
- Twenty `invariant-s38-*` greps, decoded from the snapshot and run with `git grep`: all find nothing (`clean: 20`).
- Mutation checks, each restored byte for byte from `.artifacts/0390-scripts/`:
  - A full sixteen-column solve for every lone object fails the main fact (rank 15 expected, 16 got) and the
    rank-deficient fact (the recorded column count).
  - The held gain in place of the session rule fails the main fact (m00 1.058 against 1).
  - Lone objects never merged fails the linked variant (2 solves recorded, not 1).
  - Row-major in place of vec order fails the main and series facts.
  - The CPL analyzer without the plate: `ObjectSolveFitErr … rank 12`.
- LF only: 0 CR bytes in every written file, and `git diff --numstat` equals `--ignore-cr-at-eol`.

Commit-ready: every requirement of the slice is addressed in this round.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0390-build-local.log`, `0390-constructor-unit-tests-local.log`, `0390-unit-tests-local.log`,
  `0390-elevation-local.log`, `0390-invariants-local.log` — the final local gate runs.
- `0390-studyengine-tests-local.log` and `0390-trx/studyengine.trx` — the class run with per-test durations.
- `0390-scripts/` — the inserted source text and the pre-mutation copies of `StudyEngine.fs` / `StudyEngineTests.fs`.
  The mutation checks restored from these.

## Gotchas

- The shipped CPL compound cannot analyze circular light; see Decision 7.
- ELEV013P covers private module functions. The first elevation run refused `captureRowsOf (prefit :
  ExperimentPrefit)`, so it is now the private extension member `ExperimentPrefit.captureRows`. Precedent:
  `CategoryId.unknownEditorRow`.
- `runStepOf` is an `ItemEvaluation` extension, so `StudyEngine.fs` now opens `OpticalConstructor.Domain.ItemEvaluation`,
  together with `Berreman.Fields` and `Berreman.MathNetNumericsMath`. No name clashed.
- The brief contradicts itself: "built with step 388's constructors … projected by `projectCaptureRow`" against
  "without constructing an LP/CPL SourceModel". I followed the MUST text; see Decision 2.
- A run without hand-set sessions (sub-variants (a)–(d), the series and the rank-deficient facts) is in a free session,
  so it uses the Phone's held gain of 1000. The two hand-set sessions with AIR-derived gains are in the main fixture,
  as the brief describes it.
