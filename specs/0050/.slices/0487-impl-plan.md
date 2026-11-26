# Step 0487 — impl plan (RunPlan on JobRunner)

## Approach

Host the Measurement Planner's run as step 393 hosts a study fit (`StudyFitJob.fs`), over `JobRunner.startBackground`,
`updateJob` and `JobModel`, in a new `OpticalConstructor.Ui/PlanJob.fs` compiled directly after `StudyFitJob.fs`.

1. `MeasurementPlanner.fs`: `PlannerError` gains `PlanRaisedErr` ("the plan raised: <reason>") and
   `UnknownPlannerStudyErr` ("the kit has no such study: <reason>"), each documented, each rendered by `describe`.
2. `PlanJob.fs`, in the brief's order:
   - `PlanJobHandle { cancelPlan }`;
   - `PlanJobHost { startPlan }` with `posting`, `onUiThread` and `inert`; `posting` builds a `CancellationTokenSource`
     and a `Stopwatch` per job, an observer posting each `PlanProgress` as a `JobProgress` with a known total, runs the
     plan through `startBackground` as `RanToCompletion [ plan o ]`, and maps a raise / a result-less outcome to
     `PlanRaisedErr`;
   - `PlanJobState` (`job : JobModel<PlanReport>`, handle, error) with `idle`;
   - `PlanRequest { study; material; criterion }`;
   - `PlanMsg` (`RunPlan`, `CancelPlan`, `PlanProgressed`, `PlanEnded`);
   - `PlanEnvironment { host; planWith }` with `create host kit forward`, planning the named study's material through
     `kit.cachedPlannerRows`;
   - `PlanMsg.updateWith environment state`: one plan at a time, completion commits, cancel keeps, other errors held.
3. Tests: new `PlanJobTests.fs` (`module PlanJobTests`, namespace `OpticalConstructor.Tests`), appended last in the
   test project, five facts as the brief names them, over a `posting` host with an immediate `post`, waiting on a
   `ManualResetEventSlim` with a ten-second limit.

## Files

- `OpticalConstructor.Domain/MeasurementPlanner.fs` (two error cases)
- `OpticalConstructor.Ui/PlanJob.fs` (new), `OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj` (one Compile line)
- `OpticalConstructor.Tests/PlanJobTests.fs` (new), `OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
- Consequential (precedent: step 0486): the exhaustive `PlannerError` or-patterns in `PlannerInputsTests.fs` and
  `PlannerReportTests.fs` gain the two new cases; `PlanRunTests.fs` drops `private` from `planStudy` and
  `contextOver`, so the environment fact runs over "`PlanRunTests`' study" by reuse, not a copy.

## Risks

- ELEV: every new public signature must carry only our types or members on our types; no bare primitive in a record
  (PlanJobHost's functions take our types only). The `posting` / `onUiThread` signatures copy StudyFitJobHost's, which
  is elevation-clean.
- Unique names (ELEV014/015/016/019): the case names `RunPlan`, `CancelPlan`, `PlanProgressed`, `PlanEnded`,
  `PlanRaisedErr`, `UnknownPlannerStudyErr` and members `posting` / `onUiThread` / `inert` / `idle` / `create` /
  `updateWith` — members on different types are fine (StudyFitJob already uses the same member names); grep the DU
  case names for collisions before building.
- The read-only environment run: the same folder opened read-only hits the cache the read-write run wrote, so the
  "writes nothing" check compares the folder's path list before and after.
