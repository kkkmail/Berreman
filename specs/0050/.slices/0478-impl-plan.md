# Step 0478 — impl plan

## Approach

Step 477 gave `PlannerInformation.fs` the Fisher information of stacked candidate rows (`over`, `rank`,
`logDeterminant`, `traceOpt`). This step adds the planner's greedy ranking on top of it, after those declarations:

1. `PlanCriterion` = `DeterminantPlanCriterion` | `TargetTracePlanCriterion`, with a `text` member
   (`D-optimal: most information`, `A-optimal: smallest target variances`). It is window state and carries no wire tag.
2. `PlanOption<'Candidate> = { candidate; rows : CandidateRows }` and
   `RankedOption<'Candidate> = { option; information : PlannerInformation }`.
3. `type PlannerInformation with static member rankWith criterion coordinates baseline options`. Each round, every
   option still left is joined to the baseline and the options already ranked, in that order, and `over` is taken.
   - The best option is ranked next. Better means a higher rank first.
   - At equal rank, D takes the larger `logDeterminant`. A takes the smaller `traceOpt` when both are at full rank,
     and otherwise the larger `logDeterminant`.
   - A strict comparison keeps the earlier option on a tie.
   - The first `over` refusal answers the whole call, because `Result.bind` stops at it.

   The comparison is a local closure, because a member or private function `PlannerInformation -> … -> bool` would
   be ELEV010, as step 477 measured.

Tests (`PlannerInformationTests`, three new facts over literal rows at noise 1, coordinates a and b):

- the confounded-combination fact, where Y outranks X on rank under both criteria;
- the D-against-A fact, with determinants 24 and 16 and traces 1/2 and 1 + 1/24;
- the tie-and-refusal fact.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/PlannerInformation.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerInformationTests.fs`

Neither project file changes: both files are already in their compile lists.

## Risks

- **ELEV010 on members.** No member signature mixes our types with a primitive. `text` is on `PlanCriterion` alone,
  with no parameter.
- **Record-label inference.** `PlanOption.rows` shares a label with `PlannerInformation.rows`, so every access to
  them is annotated.
- **Exact ties.** The tie fact uses identical rows, so the two joined informations are bit-identical.
