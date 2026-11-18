# Step 0487 — impl log (RunPlan on JobRunner)

## Progress

- [x] `PlannerError` gains `PlanRaisedErr` and `UnknownPlannerStudyErr`
- [x] `PlanJob.fs` (handle, host, state, request, messages, environment, `updateWith`) and its Compile line
- [x] `PlanJobTests.fs` (five facts) and its Compile line
- [x] Consequential test edits (exhaustive or-patterns; `PlanRunTests` fixtures made public)
- [x] Build, elevation, filtered tests

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasurementPlanner.fs` — `PlannerError` gains
  `PlanRaisedErr` ("the plan raised: …") and `UnknownPlannerStudyErr` ("the kit has no such study: …"), each
  documented and rendered by `describe`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/PlanJob.fs` (new, `module OpticalConstructor.Ui.PlanJob`) —
  `PlanJobHandle`, `PlanJobHost` (`posting`, `onUiThread`, `inert`), `PlanJobState` (`idle`), `PlanRequest`,
  `PlanMsg`, `PlanEnvironment` (`create`) and `PlanMsg.updateWith`, in the brief's order.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj` — one `<Compile>` line directly
  after `StudyFitJob.fs`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlanJobTests.fs` (new, `module PlanJobTests`) — the five facts.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` — `PlanJobTests.fs` appended
  last.
- Outside `effects.edits` (see Decisions; the same shape step 0486 recorded and the operator committed):
  - `OpticalConstructor.Tests/PlannerInputsTests.fs` (1 ±) and `OpticalConstructor.Tests/PlannerReportTests.fs` (1 ±);
  - `OpticalConstructor.Tests/PlanRunTests.fs` (2 ±).

## Decisions

- **Consequential edit: the exhaustive `PlannerError` or-patterns.** `PlannerInputsTests.unresolvedReasonOf` and
  `PlannerReportTests`' unknown-target fact list every other case; the two new cases join each list, or the
  incomplete match warns (FS0025) and the zero-warnings rule fails. No assertion changes.
- **`PlanRunTests`' study is reused, not copied.** The environment fact runs "over a read-write in-memory kit holding
  `PlanRunTests`' study". AGENTS.md's "Reuse, never copy" rules out re-transcribing it, so `private` is dropped from
  exactly `PlanRunTests.planStudy` and `PlanRunTests.contextOver` (its forward context). Nothing else in that file
  changes.
- **The count conversion is a member on the source type.** The brief's `JobStepCount p.evaluated.value` is the shape
  ELEV001 (NoCallSiteRewrap) reports; `PlanJob.fs` declares `PlanCandidateCount.toJobStepCount` (a private type
  extension, `this.value` inside the source type's member being the sanctioned form), and the observer posts
  `{ completed = p.evaluated.toJobStepCount; total = Some p.total.toJobStepCount; elapsed = JobElapsed stopwatch.Elapsed }`.
- **A refusal other than Cancel** returns the job to `Idle` with `progress = None`, its committed report kept, as
  `StudyFitMsg.updateWith`'s `refusedWith` does (the brief: "as step 393 hosts a study fit").
- **An outcome holding no single result** is `PlanRaisedErr "the plan job answered <n> results instead of one"`, the
  study fit's wording.
- **`UnknownPlannerStudyErr`'s reason** is the requested study's name.
- **The environment fact plans synchronously.** Its first run went through the surface's host and waited ten seconds;
  in the whole suite (with elevation running beside it) the real solve did not end in time. The fact's subject is the
  environment and the cache, the host path is facts 1–4's, so the fact calls `environment.planWith` directly over a
  recording observer (asserting 1 of 2 and 2 of 2) and keeps the ten-second wait for the stubs only.
- **A fresh read-only kit was tried and dropped.** The brief's same-folder read-only run hits the read-write run's
  cache. A second, fresh read-only kit (solving both candidates, writing nothing) made the fact 91 s under the parallel
  suite, its longest test, so it was removed: the read-only miss path is `PlannerCacheTests`' existing fact "a
  read-only kit computes and writes nothing, and reads what a read-write open cached". The fact now does exactly the
  brief's two runs (62 s under the suite, about 6 s alone) and adds one solver-free assertion: a request naming a study
  the kit lacks is `UnknownPlannerStudyErr` with the name as its reason.

## Testing state

Each run uses the gate's own command over the final tree; the supervisor's gate engine is the authority.

- `build`: exit 0, 0 errors; only the exempt `NU1902`s (and, on the first build's MathNet recompile, the vendored
  pre-existing `SYSLIB0051`s). 86 s first, 26 s incremental (deadline 600 s).
- `elevation` (`ASG_ARC_BASE_COMMIT` = the `.arc-base-json` origin): `ELEV-GATE: OK - 0 new finding(s) in 13
  project(s), 237s` (deadline 600 s), known findings 0; Ui 0 and Domain 0 findings. No product file changed after it.
- `constructor-unit-tests` (`all_discovered`): `Passed!`, 1899 of 1899 = 1894 + the five census facts, 139 s
  (deadline 180 s). An earlier run, with elevation beside it, failed only the environment fact on its ten-second host
  wait (see Decisions); `PlanJobTests` alone: 5 of 5, 7 s.
- `ui-smoke` (trait `ui-smoke`): `Passed!`, 268 of 268, 224 s (deadline 600 s).
- `ui-tests` (selected `KitWindowTests`, `StudyViewTests`): `Passed!`, 24 of 24, 34 s (deadline 360 s).
- The 21 `invariant-s46-*` greps find nothing, also with `--untracked` to cover the two new files.
- No run exceeded its effective deadline.

## Artifacts

- `specs/0050/.artifacts/0487-build.log`
- `specs/0050/.artifacts/0487-elevation.log`
- `specs/0050/.artifacts/0487-build-02.log`, `0487-build-03.log`
- `specs/0050/.artifacts/0487-constructor-unit-tests.log` (first run: 1898 of 1899, the environment fact timed out),
  `0487-constructor-unit-tests-02.log` (1899 of 1899, 142 s), `0487-constructor-unit-tests-03.log` (final, 139 s)
- `specs/0050/.artifacts/0487-ui-smoke.log`, `specs/0050/.artifacts/0487-ui-tests.log`

## Gotchas

- A real plan run through the host under the whole parallel suite can exceed the stubs' ten-second wait: plan a real
  kit synchronously in a fact, and keep the host's wait for stubs.
- The constructor suite stands at 139 s of its 180 s deadline; `PlanRunTests` (about 120 s of facts in one class) and
  this step's environment fact (62 s) carry the real solves. A further real-plan fact should reuse a cached kit.
- `PlanRequest` shares the field names `study`, `material` and `criterion` with `PlanReport`; annotate a record
  literal's type where both are in scope.
