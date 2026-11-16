# Step 0436 — implementation plan

AT-2's fifth part (spec 0050 S12.2; 001 §Q AT-2 "3. Fit"): the build ends by running the `AbsorbingTriclinic` study
through the shell's fit host into one nonlinear fit run, and a new fact judges the recovered crystal by the noisy
suite's own bands. The only authorized edit is
`Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs`.

## Approach

1. **One fit-hosting composition.** AT-1's inline host in `runAcceptanceFit` becomes
   `runStudyFitThroughShell (model) (request) (deadline) : TableAndElementRotationModel`: `StudyFitJobHost.posting`
   whose `post` runs its job at once, the two messages recorded, the wait on a `ManualResetEventSlim` for `deadline`
   (a miss fails the fact naming the request), then the recorded messages passed through the update in order. The
   generic post-conditions (started, one completion, no refusal, finished, handle released, the completed run committed
   and appended) move with it. `runAcceptanceFit` keeps AT-1's request, its sixty seconds and its expected-name check.
2. **AT-2's build ends with the fit.** `buildAbsorbingKit` keeps its clock cell, moves it to 2026-06-01T12:01:00Z and
   runs `{ study = AbsorbingTriclinic; stage = JointStage; calibrationUse = CalibrationHeldFixed }` through
   `runStudyFitThroughShell` with 1800 seconds, checking the committed name `AbsorbingTriclinic@20260601T120100Z`.
   No engine, start count or deviate is configured: the study's defaults (`FitStartCount.single`) and
   `StudyFitEnvironment.create`'s `UniformDeviateProvider.fromSeed (NoiseSeed 1)` are the ones steps 382 and 393 name.
3. **`assertAbsorbingRecovery (study) (run)`**: the `p` of `study.promotedMaterialWith run absorbingMaterialVersion
   absorbingMaterial` answering `CrystalParameterPhysics (AbsorbingTriclinicCrystal p)` (any other answer fails naming
   it), then `absorbingBandBreaches` of `RelativeError.between (q.valueIn p) (q.valueIn absorbing)` must be empty, the
   message naming every breach's `describe` and the run's χ².
4. **The fact** `AT-2's study engine recovers one seed's fifteen constants and derived quantities within the bands
   AbsorbingBiaxialNoisyTests asserts`: `readFitRuns AbsorbingTriclinic` holds exactly that run (name, stamp, study,
   `JointStage`, `CalibrationHeldFixed`, `NonlinearStudyEngine`, the five runs, one converged member, fifteen entries in
   the axes' order owned by the material, each starting at `perturbedStart`'s value, equal to the window's run), then
   `assertAbsorbingRecovery` of the stored study and that run.

## Risks

- **Runtime.** One fifteen-parameter fit is about three minutes (001 §1b), but `resultsWith` also profiles every
  suspect entry (six 14-parameter re-fits each). The build's deadline is 1800 s and kit-acceptance's 3600 s; measure.
- **Recovery.** One seed's relative error, not an ensemble scatter, is judged against bands pinned at ~3× the
  scatter; a breach would be a measured finding, never a loosened band.
- Earlier AT-2 facts read the same Lazy build; the fit must not change the study, experiments or library they check.

## Attempt 02 — the recovery round (2026-11-16)

The step's code is committed: the worker's `KitAcceptanceTests.fs` (`21962d32`), R0004's repair (`85e7efeb`:
`FittedProblems.fs` evaluates one Jacobian and one report per problem; `StudyForward.fs` solves a residual's rows
concurrently) and R0005's (`2a72a8c0`, `d3cd3274`: `BerremanMatrix.create` and `EmComponent.create` read `rhoT` once; the
app and the UI test host run the server collector). The operator measured AT-2's fit through the shell at 811 s, inside
R0005's 1800 s bound, so R0004's conditional concurrent profile pass (3) is not triggered and `StudyEngine.fs`,
`FitResultsTests.fs` and `StudyEngineTests.fs` stay untouched.

1. Review the committed diffs against the slice's how-to (1)–(4), R0004 and R0005, and R0001–R0003's preservation.
2. Correct the one imprecise line found: `Fields.fs`'s new comment says the generated terms read `rhoT` "~90 times";
   `EmComponent.create` reads it ten times (R0005 itself says 10; ~90 is `BerremanMatrix.create`'s 92). Comment only,
   same line, under R0005's authority over `Fields.fs` at 0436.
3. Development evidence: the solution build and the elevation analyzers from the arc origin, because production code
   changed. No test suite is re-run: the operator's 811 s measurement is the fit's evidence, and the supervisor runs the
   unchanged roster.
4. Rewrite the state of the world for the recovered step; append this attempt to the impl-log.
5. (Found while executing 3.) Elevation over the committed tree fails with ELEV003 and ELEV008 on
   `FittedProblem.init@78`, the hidden field a record's `static let` creates. Move R0004's two memo tables into a
   private class in the same recursive group (under R0004's authority over `FittedProblems.fs`). Rebuild, re-run
   elevation, and run the two classes that exercise the code (`FitResultsTests`, `StudyEngineTests`).
