# Step 0486 — impl log

## Progress

- Read the arc-runner system prompt, the IMPLEMENT delta, the project prompt and rulings R0001–R0005. None of them
  reaches the planner run; each is preserved untouched. No file they authorized or protect is edited.
- `MeasurementPlanner.fs` (Domain): `PlannerError` gains two cases, and the type's doc now names a run among the
  refusals.
  - `PlanCancelledErr of reason`, described as `the plan was cancelled: <reason>`.
  - `PlannerForwardErr of reason`, described as `the forward model refused a candidate: <reason>`.
- New `PlannerRuns.fs`, `module PlannerRuns` in `namespace OpticalConstructor.Optimization`, compiled directly after
  `PlannerInformation.fs` (one `<Compile>` line, no comment). In the step's order:
  - (a) Progress and Cancel:
    - `PlanCandidateCount of int` with `.value`;
    - `PlanProgress = { evaluated; total }`;
    - `[<ReferenceEquality>] PlanObserver = { reportCandidate : PlanProgress -> unit; cancellation : CancellationToken }`.
  - (b) `PlanCandidate.configurations` iterates each incidence of `incidenceRange.sweptValues` (outermost), then each
    azimuth of `azimuthRange.sweptValues`, through `MuellerFitting.configurationOf`.
  - (c) On `PlannerInputs`:
    - `scalingWith`: the targets' `prior.crystal.parameterAxes` in target order, centred at the prior crystal, its
      scale the crystal with each axis written (`writeValue`) at its unknown's `bounds.searchUnit`. A target the copy
      holds known or calibrated is `UnknownPlannerTargetErr "<name> is not an unknown of '<material>'"`; one it does
      not hold is `"... is not a parameter of ..."`.
    - `forwardWith`: `createBerremanForward (fun c -> c.opticalProperties) solverParameters`, as
      `StudyForward.predictedReadingOf` builds it.
    - `candidateRowsWith`, in order:
      1. no polarimeter is `UnresolvedPlannerItemErr "no detector of the kit records a full Mueller matrix"`, the
         wording of `cacheKeyWith`;
      2. `polarimeter.noise.benchNoise` is resolved first, its `ItemEvaluationError` becoming
         `UnresolvedPlannerItemErr "<detector label>: <describe>"`, whose describe names the axis;
      3. then the scaling;
      4. then `candidateRowsOver`, a `MuellerFitError` becoming `PlannerForwardErr "<caption>: <reason>"`.
    - `priorRows`: for each target with `priorOpt`, in target order, one row of unit noise holding
      `searchUnit / σ` at its own coordinate.
    - `scalingWith` and `priorRows` read one private walk of the targets (`plannedTargets`, over a private
      `PlanTarget = { axis; unknown }` record).
  - (d) `PlanStanding`, `RankedPlanStep` and `PlanReport`, with the fields the step lists.
  - (e) `PlannerInputs.planWith context study material criterion rowsOf observer` works in this order:
    1. `structuralReport`;
    2. `instrumentFindingsWith context.kit.items context.choiceOf context.materials`;
    3. `scalingWith`;
    4. each candidate in order: the token read before it (requested: `PlanCancelledErr "<k> of <n> candidates were
       evaluated"`), its `rowsOf`, then `observer.reportCandidate` of the count so far and the total;
    5. the evaluated candidates split by `coverageIn context.kit.setups context.kit.experiments`;
    6. the baseline: `PlannerInformation.over` the targets of `priorRows`, then the in-kit candidates' rows, with its
       `boundsOver` the scaling;
    7. `PlannerInformation.rankWith criterion` of the open candidates over the same baseline rows, each step with its
       bounds.

    The first error stops the run. No retry, cache or option is added.
- New `PlanRunTests.fs`, `module PlanRunTests`, appended last in the test compile list with no comment. It holds the
  five census facts over `PlannerInputsTests`' kit and a study opening the probe quartz with g11 and g33 freed at
  their values within ±50 %. Its `StudyForwardContext` is test-local: Built-in library and material store,
  `ExperimentDataStore.createFileBacked` over an empty in-memory kit, and default solver parameters. The facts:
  1. **Progress and Cancel.** Literal rows: (1, 2) then (2, 2). A token the first report cancels gives
     `PlanCancelledErr "1 of 2 candidates were evaluated"` with `rowsOf` called once. A token already cancelled
     evaluates nothing ("0 of 2"). A `rowsOf` refusal stops the run unreported.
  2. **In-kit candidate.** E1, built as step 482's fact builds it (`benchHolding qzZDraft`, R2 then R1 sweep,
     transmitted), puts the transmitted candidate in `inKit`. `ranked` is the reflected candidate alone, and the
     baseline's rank is 1, its rows E1's. A planned E1 holds it too.
  3. **Prior.** With g33 under a prior of σ 1e-6, `priorRows` is one row `[0; 1e-7 / 1e-6]` at noise 1, and the
     baseline's rank is 1 before any candidate. Without the prior it is 0. A target held known refuses the scaling.
  4. **Real forward.** Through `candidateRowsWith SolverParameters.defaultValue`:
     - `structural` holds the off-axis-cut finding (g33, x-cut);
     - the last ranked step has rank 2 and a finite, positive bound on g11 and on g33;
     - `instrument` and `unplacedPlates` are empty.

     The detector noise is checked here too, as the step requires:
     - an edited `elementUncertainty` (doubled) leaves the Jacobian unchanged and raises the candidate's noise
       variance by exactly `15/16 · Δ(u²)/3`;
     - a negative `rotationUncertainty` is `UnresolvedPlannerItemErr "Polarimeter: … rotationUncertainty = -0.01 …"`;
     - no polarimeter refuses.
  5. **Cache.** `KitContext.cachedPlannerRows` runs over an in-memory read-write kit, wrapping `candidateRowsWith`
     with a counter. The first plan computes 2; after QZ-X joins the items, the second computes 1, and the kit then
     needs no off-axis cut.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasurementPlanner.fs` (+8 / −2)
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/PlannerRuns.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/OpticalConstructor.Optimization.fsproj` (+1)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlanRunTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (+1)
- Outside `effects.edits` (see Decisions):
  - `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerInputsTests.fs` (8 ±);
  - `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerReportTests.fs` (1 ±);
  - `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerCacheTests.fs` (2 ±).

## Decisions

- **Consequential edit: the exhaustive `PlannerError` or-patterns.** `PlannerInputsTests.unresolvedReasonOf` and
  `PlannerReportTests`' unknown-target fact match `PlannerError` exhaustively. The test project compiles with
  `--warnaserror+:25`, so the two new cases would break the build. Each or-pattern gains
  `PlanCancelledErr _ | PlannerForwardErr _` on its existing line. No assertion or identity changed. This follows
  steps 470, 471, 477, 483 and 485.
- **The kit is reused, not copied.** The facts run "over `PlannerInputsTests`' kit" and build E1 "as step 482's fact
  builds them". AGENTS.md's "Reuse, never copy" rules out a second transcription, so `private` is dropped from exactly
  the bindings `PlanRunTests` reads:
  - in `PlannerInputsTests`: `crystalEntry`, `qzZDraft`, `qzX`, `g11`, `benchHolding`, `sampleAxisOf` and `runOver`;
  - in `PlannerCacheTests`: `newKitFolder` and `openOver`, its in-memory read-write kit.

  Nothing else in those files changed. This is step 485's precedent.
- **No proxy copied for the test context.** The data seam is the product's
  `ExperimentDataStore.createFileBacked` over an empty in-memory kit; the plan never reads it.
- **`MuellerFitError` has no render member.** `PlannerForwardErr` needs one: `%A` is ELEV002, and a free function of
  one of our types is ELEV013. So `PlannerRuns` declares a private extension member,
  `MuellerFitError.plannerReason`. It answers the forward's own reason where the case carries one (the only case
  `candidateRowsOver` can produce), and a sentence for the rest. `MuellerFitting.fs` is not in the step's edits.
- **Coverage split by `List.choose` over a match**, not a `PlanCoverage -> bool` lambda (ELEV012). Each candidate's
  coverage is a pure function of the kit, so deciding it after the rows are evaluated is the step's order to every
  observer.
- **The baseline and the ranking share one row list**: the priors, then the in-kit rows. `PlannerInformation.over`
  takes the first row's noise as the reference, so a prior's unit noise is the reference when a prior exists.
- **The real-forward fact uses the D criterion**, the Plan view's first. The step names no criterion for it, and rank
  2 and the bounds do not depend on the order.

## Testing state

- **Build**: `dotnet build Berreman.slnx -c Release -nologo -v:m` from `Berreman/`, the gate's command. Exit 0, 0 errors;
  the only warnings are the exempt `NU1902` (and the vendored `SYSLIB0051` on a MathNet recompile).
  - The first two builds failed in the new test file only: an open of the `[<RequireQualifiedAccess>]`
    `ExperimentDataStore`, and a missing `PlannerSymmetry` open. Both fixed; the production projects compiled clean
    from the first build.
  - Logs: `.artifacts/0486-build-01.log` to `0486-build-05.log`; `-05` is the final tree.
- **`PlanRunTests`** alone: 5 of 5, 18 s. Log: `.artifacts/0486-plan-run-tests-01.log`.
- **The touched classes** `PlanRunTests`, `PlannerInputsTests`, `PlannerReportTests` and `PlannerCacheTests`: 36 of
  36, on the final tree. The TRX names of the five new facts are exactly the census ids. Logs:
  `.artifacts/0486-planner-classes-tests.log`, `.artifacts/0486-trx/0486-planner-classes.trx`.
- **`constructor-unit-tests`**, the gate's command over the whole suite (selection `all_discovered`): `Passed!`,
  1894 of 1894, 0 failed, 0 skipped, 129 s of 180 s. Checkpoint 485 was 1889, so the count rises by exactly 5. Log:
  `.artifacts/0486-constructor-unit-tests.log`.
- **Elevation**: `Elevation/Run-ElevationGate.ps1 -ExcludeRules ELEV004` from the repo root, no base, so all 13
  projects. Result: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 232s`, known findings 0 (Domain 196 s,
  Optimization 0 findings). Log: `.artifacts/0486-elevation.log`.
- **`kit-acceptance`**, the gate's command with its selection (`KitAcceptanceTests`, trait `kit-acceptance`):
  `Passed!`, 14 succeeded, 1 skipped (AT-2's Explicit five-seed ensemble; skip policy `allow`), 1418 s of 3600 s.
  AT-2's grid-generator fact (the fit) took 789 s, inside R0005's 1800 s. Run alone, so the fit time is uncontended.
  Log: `.artifacts/0486-kit-acceptance.log`.
- **`unit-tests`**, the gate's command with its selection (`MuellerInverseTests`, `BiaxialInverseTests`): `Passed!`,
  31 of 31, 946 s of 7200 s, run after kit-acceptance. Log: `.artifacts/0486-unit-tests.log`.
- **The 21 `invariant-s46-*`** must-find-nothing greps, decoded from `.slices/0486.gates`, find nothing, also with
  `--untracked`, which is checked to see the two new files. Commands: `.artifacts/0486-invariant-commands.txt`.
- No run exceeded its effective gate deadline.
- **Line endings**: every touched file is `w/lf`, the new files hold no CR byte, and `git diff --numstat` equals
  `--ignore-cr-at-eol`.
- **Style**: lines are wrapped to the tree's 120 columns. The one exception is the fourth fact's census name, which is
  its identity.

## Artifacts

- `.artifacts/0486-build-01.log` … `0486-build-05.log`
- `.artifacts/0486-plan-run-tests-01.log`
- `.artifacts/0486-planner-classes-tests.log`, `.artifacts/0486-trx/0486-planner-classes.trx`
- `.artifacts/0486-constructor-unit-tests.log`
- `.artifacts/0486-elevation.log`
- `.artifacts/0486-kit-acceptance.log`
- `.artifacts/0486-unit-tests.log`
- `.artifacts/0486-invariant-commands.txt`

## Gotchas

- `ExperimentDataStore` is `[<RequireQualifiedAccess>]`; a test reaches it with `open OpticalConstructor.Storage`
  and the qualified name.
- `git grep` skips untracked files; the invariants were also checked with `--untracked`, which sees the two new files.
