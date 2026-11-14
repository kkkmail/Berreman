# Step 0390 — state of the world

## Where we are

Spec 0050, section S11.3 (engines, staged and joint fits, append-only fit runs).

- Step 386 chose the engine by the study's shape.
- Step 387 added the nonlinear engine.
- Step 388 made the staged path's pieces callable.
- Step 389 added the linear engine's stage 1.
- This step adds stage 2, which also serves a joint fit of the same shape: `StagedStudy.reconstructLinear`
  reconstructs each black-box object behind calibrated instruments by the reference linear path (001 §L).
  - Every capture row is projected by the study's own run law about its unknown object and divided by its session's
    gain at its capture time.
  - A lone fully free object, or a series' combined matrix, is solved by `reconstruct`. A partly fixed or
    equality-linked system is solved on its free columns through the same SVD proxy.
- Step 391 dispatches `runFit` to it.

## What's working

- Added `StagedStudy.reconstructLinear`. It projects every capture row of the fitted runs through the run law's own
  item-model matrices, orientations and wavelength. It divides each row by its session's dark and its gain at the row's
  time. It solves `reconstruct` or `A·P·x = b − A·k` through the injected SVD proxy.
- Took P and k from the study's own `assignmentFor`, so equality links (summed columns), cross-object and fixed
  leaders, and the owner rules follow the study. Lone objects that share a leader are solved as one system.
- Added `ParameterValue.airGain`, `MuellerMatrix.rowMajorValues`, and the errors `SessionGainFitErr` and
  `ObjectSolveFitErr`.
- Factored the AIR gain arithmetic (`airAnchorGainOver`) and the row projection (`projectCaptureRowFrom`) out of the
  staged path. Moved `readingAt`'s per-row orientation and wavelength rules into `BoundExperiment.orientationAt` /
  `waveLengthAt`, so the engine and the run law share one copy.
- Added three `StudyEngineTests` facts (sessions with interpolated gains, partial and linked variants, the series'
  combined matrix, the rank-deficient refusal) and widened the describe fact.

## Tests

Run locally on 2026-11-13 on the final code. Every run finished within its effective deadline.

- `build`: exit 0, 0 errors, 24 s (deadline 600 s). The only warnings are the exempt NU1902 advisories (12 lines), and
  the log holds no `error`.
- `constructor-unit-tests`, whole suite: 1687/1687, 102 s (deadline 180 s).
  - The count is 1684 → 1687, +3, as the acceptance requires.
  - The TRX shows each of these `Passed` under its exact name:
    - `the object stage reconstructs a black-box matrix behind calibrated instruments, each row divided by its session's
      gain at its time`;
    - `a series of two black-box objects is reconstructed as one combined matrix`;
    - `an object stage whose design is rank-deficient is refused naming its objects`;
    - the widened `every study-fit error renders its condition and its payload`.
  - The three new facts take about 2.4 s together.
- `unit-tests`, selected `BerremanTests.MuellerReconstructionTests`: 25/25, 2 s (deadline 7200 s). No BerremanTests file
  is edited, and the factored staged path is unchanged for them.
- `elevation`, standalone with no `-Base`, so all 13 projects:
  - The first run, 222 s, found one ELEV013P (`captureRowsOf` took only an `ExperimentPrefit`). It is now the private
    member `ExperimentPrefit.captureRows`.
  - The re-run is clean: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 225s` (deadline 600 s).
- The twenty `invariant-s38-*` must-find-nothing greps, decoded from the snapshot and run with `git grep`: all twenty
  find nothing.
- Mutation checks, each with the source restored byte for byte afterwards:
  - a sixteen-column solve for partial variants fails the main and rank-deficient facts;
  - the held gain in place of the session rule fails the main fact;
  - lone objects never merged fails the linked variant (two solves instead of one);
  - row-major in place of vec order fails the main and series facts;
  - the CPL analyzer without the plate is answered `ObjectSolveFitErr … rank 12`.
- Every file this step wrote is LF (0 CR bytes); `git diff --numstat` equals `--ignore-cr-at-eol`.

```yaml
gates:
  constructor_unit_tests: 1687
```

## Architecture

- **One run law.**
  - `readingAt`'s per-row rules (R1, dial or recorded turn, R2, wavelength) are moved, unchanged, into
    `BoundExperiment.orientationAt` / `waveLengthAt`. `readingAt` and the linear engine both call them.
  - The engine binds each run once at the start vector (`assignmentFor` + `boundWith`, its session's dark else the
    detector's offset). Per row, it evaluates the fixed steps around the unknown segment through
    `BoundOpticalElement.muellerAt`.
  - The steps before the segment fold the emission (`detectorStokes`). The steps after give the detector's S₀ row
    (`RunStep.productOf`). φ is the segment's rotation.
- **P and k without a second layout.**
  - The assignment is affine in the vector, so k is an object's sixteen coefficients (vec order) at the zero vector.
    Column j of P is their change at the j-th unit vector.
  - Links and owners come from `assignmentFor` itself.
- **Systems.**
  - Runs group by their unknown-bearing items in light order, in first-run order.
  - Lone groups sharing a column merge, with columns in layout order.
  - A lone item fitting all sixteen of its own goes through `reconstruct`. Any other system is reduced
    (`A·P`, `b − A·k`) through `solveLinearLeastSquares`, and its matrix `k + P·x` is assembled whole before
    `columnMajorMueller`.
  - A series group is reconstructed whole as its combined matrix and fills no entry.
- **The answer.**
  - One `ObjectMatrixFit` per group, with its system's rank and RMSE.
  - Lone groups fill only their own entries.
  - One converged member, whose χ² is Σ rmse² × the group's own rows.
- **Gains.**
  - A free session uses the held detector gain.
  - An anchored or interpolated session takes each named AIR run's anchor from that run's own run-law projection and
    dark, through the factored `airAnchorGainOver`, then `ruleWith` and `gainAt` at the row's time.
- **Two files outside the authorized edits changed**, as the brief's own requirements force:
  - `StudyForward.fs`, the shared per-row rules;
  - `StagedMuellerPath.fs`, the shared gain arithmetic and projection.

  Both are moves with the same operands in the same order. The impl-log records why.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **The shipped circular polarizer cannot analyze circular light.**
  - Its compound is a linear polarizer then a retarder in light order, so as an analyzer its retarder sits after the
    polarizer and S₀ never sees S₃.
  - The brief's four-family fixture with CPL2 analyzing reaches only rank 12 (measured).
  - The fixture's "CPL" analyzer families therefore put the fixed quarter-wave plate ahead of CPL2: a retarder then an
    LP, the reference's circular analyzer.
  - Any later fixture or kit that relies on a CPL analyzer meets the same limit.
- **The brief's sentences pull two ways.** It says "models built with step 388's constructors … projected by
  `projectCaptureRow`", and it also says "without constructing an LP/CPL SourceModel". The MUST statements win:
  - no `AnalyzerModel` or `SourceModel` is built;
  - rows go through `projectCaptureRowFrom`, the projection `projectCaptureRow` now calls.
- **ELEV013P applies to private functions.** A private module function taking only one of our types is refused. It
  becomes a private extension member, as `CategoryId.unknownEditorRow` is.
- **Test fixtures:**
  - The data comes from the run law itself (`boundWith` + `readingAt`, the black box probed with unit matrices), so
    the expected design rows equal the run law's.
  - `contextOver` now has `contextWithin` (any setups) beneath it, and `benchOf` has `benchWith`. `dialSweep` is
    shared with stage one's runs.
- **Shell:** a long Bash heredoc with apostrophes failed again. Inserted text is staged under
  `.artifacts/0390-scripts/` and spliced in with Python.

## Changelog

- 2026-11-13 — Step 0390: the linear engine's object stage.
  - `StagedStudy.reconstructLinear` projects rows through the run law and divides them by each session's gain at the
    row's time.
  - Objects are solved by `reconstruct` or on their free columns `A·P·x = b − A·k`, with P and k taken from the
    study's own assignment.
  - New `ParameterValue.airGain`, `MuellerMatrix.rowMajorValues`, `SessionGainFitErr` and `ObjectSolveFitErr`.
  - Shared run-law rules, AIR gain arithmetic and projection.
  - Three new `StudyEngineTests` facts and the widened describe fact.
