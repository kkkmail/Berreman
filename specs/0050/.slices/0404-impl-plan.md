# Step 0404 — impl plan

## Approach

The fit run stores its results (spec 0050 S11.4; step 383's "S11.4 adds the results").

- `FitRuns.fs`: `FitRun` gains `results : FitRunResults` after `objectMatrices`. `FitRun.currentSchemaVersion` stays
  1, because no fit-run file exists before S12 writes the committed kits.
- `fitrun.schema.json`: under version 1 the root requires sixteen fields, `results` added. `results` is an object
  with exactly `sigmas`, `problems`, `runResiduals`, `realizability`, `productChecks` and `profiles`, each an array.
  The inner shapes stay the serializer's to bind. No converter is added.
- `StudyEngine.fs` `runFit`: after the engine's solution and before the clock read, call
  `staged.resultsWith stagedContext prefits data this.linearSolver solution watch`. Its error is answered as it
  stands, so a cancelled profile pass is `FitCancelledErr` and the job writes nothing. The run's `results` is the
  value it gives. Nothing else in `runFit` changes.

## Tests

- `FitRunFileFixtures.fitRunOf` (the one test record expression that spells out a `FitRun`) gains the slice's
  one-of-each results, because it is the round-trip fact's fit run:
  - QZ m00's σ `Some 0.002` and CPL1 c2.retardance's `None`;
  - one problem per subject, covering each coordinate case, one with a condition and correlation and one with neither;
  - a two-entry run residual, a four-eigenvalue realizability and a product check;
  - a profile holding all three reasons and both outcomes.
- The round-trip fact expects the `results` key, reads back equal and rewrites the same text.
- The refusal fact also refuses a run without `results`, and a `results` without `profiles`, with `SchemaValidationErr`.
- `a fit run records its name, …`: the plate fit's run holds exactly the `resultsWith` results of the same solution,
  namely n_o's and n_e's σ, one `StudyEntriesProblem` and P1's residual.
- New fact `a fit run stores its results, profiles included`: step 403's confounded intensity study goes through
  `runFit`. Its run's results hold the gain's and the scale's profiles, seven points each, equal to what `resultsWith`
  gives. The run also round-trips through its `.fitrun-json` text under the schema.

## Risks

- `objectProblemsWith` re-solves each object group through the solver. The fit-run fact's recording SVD then sees two
  solves per fit, not one, which is a change of expectation inside that fact.
- Under the initial thresholds, a linear fixture's entries may be suspect. Then `runFit` re-fits them under the fit's
  watch, and the "no iteration told" assertion in `linearFitRunOf` changes too.
- Runtime: `StudyEngineTests` was 134.7 s of the 180 s deadline. Each `runFit` now also gathers results and profiles,
  so the suite must be measured.
