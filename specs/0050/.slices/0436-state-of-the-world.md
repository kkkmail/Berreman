# Where we are

Step 0436 is AT-2's fifth part (spec 0050 S12.2; 001 §Q AT-2 "3. Fit"). Steps 0432–0435 built the library drop, the
items, the table, one experiment per plate, seed 1's attached observations, and the `AbsorbingTriclinic` study with its
fifteen components freed from `perturbedStart`. This step makes the build end by fitting that study through the shell's
fit host, and adds the fact that judges the recovered crystal by the noisy suite's bands.

Attempt 01 wrote the test and was blocked: the study engine's fit ran past 7170 s. The operator committed that work
(`21962d32`) and repaired the product under R0004 (`85e7efeb`) and R0005 (`2a72a8c0`, `d3cd3274`). They measured AT-2's
fit through the shell at 811 s against R0005's 1800 s bound, with the new fact passing.

This attempt 02 reviewed the committed tree and corrected one comment. Elevation found R0004's memo tables tripping
ELEV003 and ELEV008, and the tables moved into a private class with the behaviour unchanged. The supervisor runs the
unchanged 26-gate roster to publish checkpoint 435.

# What's working

- AT-1 and AT-2 host their study fits through one `runStudyFitThroughShell (model) (request) (deadline)`.
- AT-2's build ends with the study's joint fit, calibrations held fixed, at 2026-06-01T12:01:00Z within 1800 s.
- New fact: the stored fit run's shape, then `assertAbsorbingRecovery` holds the crystal within the noisy suite's bands.
- A fitted problem evaluates its Jacobian and report once; a study residual solves its rows concurrently (R0004).
- The memo tables live in the private class `FittedProblemEvaluations`, not the record, so elevation is clean.
- The Berreman matrix and field components read `rhoT` once; the app and UI test host run the server GC (R0005).

# Tests

Development runs, evidence only and not gate verdicts; the supervisor's gates are authoritative.

- **build** (`dotnet build Berreman.slnx -c Release`): exit 0, 0 errors, in 83 s against 600 s.
  - The warnings are the exempt NU1902 advisory and four `SYSLIB0051` from the vendored MathNet's C#.
  - The MathNet warnings pre-date this step (step 0434's first build has them too).
- **elevation** from the arc origin `f395597c`, ELEV004 excluded, over all 13 projects:
  - on the committed tree: **FAILED**, ELEV003 and ELEV008 on `FittedProblem.init@78` (R0004's `static let` in the
    record), in 228 s;
  - after moving the tables into a class: `ELEV-GATE: OK`, 0 new findings, in 229 s against 600 s.
- **build after the fix:** exit 0, 0 errors, in 49 s; NU1902 only.
- **`FitResultsTests` and `StudyEngineTests`** (`OpticalConstructor.Tests`, the classes exercising the edited code):
  `Passed!`, 47 of 47, in 47 s. They are not in the roster; this is evidence for the edit.
- **The twenty-one s42 invariants:** each decoded `git grep` finds nothing.
- **kit-acceptance, ui-smoke and ui-tests were not re-run in this attempt.**
  - No test identity changed since attempt 01, which measured ui-tests at 18 of 18 in 29 s against 360 s, and ui-smoke at
    264 of 264 in 232 s against 600 s.
  - The operator measured AT-2's fit through `runStudyFitThroughShell` at 811 s, main solve 45 s, inside R0005's 1800 s
    bound, with the step's new fact passing.
  - The worker protocol (Invariant 6) and the operator's standing instruction say a measured fit is not repeated. The
    supervisor's roster runs kit-acceptance over `KitAcceptanceTests`.
  - That gate runs AT-1's builds and AT-2's build, whose fit measured 811 s, under its 3600 s attempt timeout.
- No development run exceeded its gate's effective deadline.

```yaml
gates:
  kit_acceptance_tests: 11
  ui_smoke_tests: 264
  ui_tests: 18
```

The counts are expected, not re-measured in this attempt.

- kit-acceptance: 0435's baseline of 10 plus the one fact this step creates.
- ui-smoke and ui-tests: attempt 01's measured counts. No identity in either selection has changed since.

# Architecture

- **The tests, `KitAcceptanceTests.fs` (attempt 01, `21962d32`):**
  - `runStudyFitThroughShell` is the one fit-hosting composition:
    - `StudyFitJobHost.posting` whose `post` runs each job at once, recording the progress and end messages;
    - a `ManualResetEventSlim` wait bounded by the deadline;
    - the recorded messages through the update, in order;
    - the generic post-conditions.
  - `runAcceptanceFit` is AT-1's wrapper.
  - `assertAbsorbingFitRun` checks the stored run's shape; `assertAbsorbingRecovery` is `promotedMaterialWith`
    followed by `absorbingBandBreaches`. Step 439 reuses it.
- **R0004 (`85e7efeb`):**
  - `FittedProblem.jacobian` and `report` are memoized in two `ConditionalWeakTable`s keyed on the problem's reference
    identity (`[<ReferenceEquality>]`), so a copy is evaluated afresh.
  - Attempt 02 moved the tables from the record's `static let` into `FittedProblemEvaluations`, a private class in
    the same recursive group (`and private`). The record no longer gets a hidden `init@` field.
  - `Study.residualWith` binds its runs in order up to the first refusal. It solves the bound runs' rows with
    `Array.Parallel.map`, then answers the vector in row order, or the first row refusal, else the bind refusal: the
    same answer as the serial walk.
- **R0005 (`2a72a8c0`, `d3cd3274`):**
  - `BerremanMatrix.create` reads `o.rhoT` once for its 92 term reads, and `EmComponent.create` once for its ten.
  - `ServerGarbageCollection` is set in the App and UI test project files.
- R0004's conditional concurrent profile pass is not used: the measured 811 s is under 1800 s. `StudyEngine.fs`,
  `FitResultsTests.fs` and `StudyEngineTests.fs` are unchanged.

# Deferred

Nothing.

# Gotchas

- **A `static let` in an F# record or union fails elevation.** The compiler adds a hidden `init@<line> : int` field,
  which the analyzers read as a bare primitive record field (ELEV003, ELEV008). Hold static state in a class.
- **Keep these through 0437–0494:** the single Jacobian and report per problem, the concurrent residual, the single
  `rhoT` read, the server collector, the 1800 s bound and R0001–R0003 (R0004, R0005).
- At 0460, `StudyForward.fs` keeps the residual concurrent. At 0476, the offset frame's move keeps one Jacobian and one
  report per problem.
- **A missed deadline leaves the fit running on the thread pool until the test process exits.** The late `Set` on the
  disposed event does not throw.
- **The residual's rows after a refusal are now solved and dropped.** The answer is still the first refusal in serial
  order (S11.2 as R0004 restates it).
- `BerremanMatrix.create` and `EmComponent.create`'s terms are generated. A regeneration must keep the single `rhoT`
  local (R0005).

# Changelog

- 2026-11-15/16, attempt 01: AT-1 and AT-2 share `runStudyFitThroughShell`; AT-2's build ends with the study's joint fit;
  `assertAbsorbingRecovery` and the recovery fact were added. Blocked: the engine's fit ran past 7170 s.
- 2026-11-16, operator: R0004 and R0005 repairs committed; AT-2's fit measured at 811 s.
- 2026-11-16, attempt 02:
  - reviewed the committed tree and corrected `Fields.fs`'s comment to the ten `rhoT` reads;
  - fixed the elevation breach R0004's record `static let` caused, moving the memo tables to `FittedProblemEvaluations`;
  - recorded the build, elevation, invariant and fit-class evidence.
