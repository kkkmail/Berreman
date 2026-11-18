# Step 0476 — impl plan

Four MOVES so Layer 2 (S14.2) can call the study engine's own algebra; each rule keeps one body and every number is unchanged.

1. `FitQuality.fs`: private `covarianceMatrix` becomes public `covarianceOf`, body unchanged; `reportFrom` calls it.
2. `FittedProblems.fs`: the private `FittedProblem.offsetFrame` tuple member moves, before `FittedProblem`, into
   `type ScaledVector with` as `offsetResidualOf`, `offsetOrigin` and `offsetJacobianOf` (the 100.0 offset a private
   `[<Literal>]`). `jacobian` / `report` keep their `ConditionalWeakTable` caches (R0004: one Jacobian and one report
   per problem) and call the new members inside the cache callbacks.
3. `BenchNoise.fs`: `disturb`'s angle shift moves into `type MeasurementConfiguration with member misalignedBy`,
   declared before `disturb`; `disturb` becomes `c.misalignedBy (wobble ()) (wobble ())` (left-to-right argument
   evaluation keeps the incidence draw first, so seeded experiments are bit-identical).
4. `StudyEngine.fs`: the per-entry unit moves into `type ParameterInterval with member searchUnit`, directly after
   `studySearchBox`; `scaledProblemWith` reads `unknown.bounds.searchUnit.value`.

Tests: `FitResultsTests` gains the offset-frame/covarianceOf fact; `StudyEngineTests` gains the search-unit fact.

Risks: ELEV findings on new members (private float constant -> `[<Literal>]`); evaluation order in `disturb`
(record-field order was incidence first; curried arguments are evaluated left to right, same order).
