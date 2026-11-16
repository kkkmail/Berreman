# Step 0436 — implementation log

## Progress

- Read the task, both system prompts, the project prompt, rulings R0001–R0003 (`.manual/030`, `031`, `033`), the slice,
  its gate snapshot, and step 0435's impl-log and state of the world.
- Read the APIs the step drives:
  - `StudyFitJobHost.posting`, `StudyFitEnvironment.create` (its fixed `UniformDeviateProvider.fromSeed (NoiseSeed 1)`),
    `StudyFitMsg.updateWith`;
  - `StudyFitParam.runFit`, `StagedStudy.fitNonlinear`, `scaledProblemWith`, `resultsWith`,
    `EngineProblems.suspectsWith`, and `FittedProblem` (`jacobian`, `report`, `sigmas`, `sigmaAt`, `profileAt`);
  - `FitRun`, `FitRunEntry`, `FitRunMember`, `FitMemberOutcome`;
  - `Study.promotedMaterialWith` (`StudyPublishing`), `MaterialPhysics`, `CrystalParameters`;
  - `AbsorbingBiaxialSample.absorbingBandBreaches`, `absorbingReportedQuantities`, `RecoveredQuantity.valueIn`,
    `MuellerFitting.RelativeError.between`, `RecoveryEnsembles.statisticsFor`;
  - `StudyFitSettings.initial` (`FitStartCount.single`, `SuspectThresholds.initial`: correlation 0.95, σ fraction
    0.1), and `StudyMsg`, which has no case setting either.
- Wrote the shared fit host, AT-2's fit at the end of its build, the two assertions and the fact, all in
  `KitAcceptanceTests.fs`.
- First development build: 2 errors. `promotedMaterialWith` is an extension in `OpticalConstructor.Domain.StudyPublishing`,
  which the file did not open. Opened it and rebuilt clean.
- The twenty-one s42 invariants: each decoded `git grep` finds nothing.
- Development runs: the ui-tests selection passed 18/18 (29 s); ui-smoke passed 264/264 (232 s).
- Development kit-acceptance run over the class: **Failed!, 6 of 11 succeeded, in 1887 s**.
  - The six AT-1 facts pass over the shared host.
  - All five AT-2 facts fail on their shared build's fit: `AbsorbingTriclinic@*.fitrun-json: expected the Joint fit
    of AbsorbingTriclinic (fixed) to end within 1800 s; actual no end`.
- Diagnosis. I saved the file first (sha256 `d240960c…`), applied a temporary probe, and restored the file byte for
  byte afterwards; it now has no `PROBE` line. The one later edit deletes the build's "about three minutes" comment, which the measurement contradicts; the binaries were rebuilt from the final file.
  - The probe logged the engine fit's progress stream with timestamps and raised the AT-2 deadline to 7200 s for the
    probe only. It also timed the residuals, and was to dump the run's members, problems, sigmas, profiles and
    recovery errors at the fit's end; the fit never ended, so the dump never ran.
  - Two scratch `dotnet fsi` scripts (`.artifacts/0436-probe/harness/`) ran over the same built assemblies: the
    harness's own seed-1 fit (`fitAbsorbingObservations`) and its suspect count (`FitQuality.reportFrom`).
  - The probe's shell reached its 7500 s background limit at 01:03 and was killed, test process included. The fit
    had not ended (see Testing state).
- Stopped. The step is blocked on authority (see Testing state and Gotchas).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs`, the step's only authorized edit
  (+132 / −18, LF only, no BOM).

## Decisions

- **How-to (1), one composition.** `runAcceptanceFit`'s inline host became
  `runStudyFitThroughShell (model) (request) (deadline) : TableAndElementRotationModel`:
  - `StudyFitJobHost.posting` whose `post` runs its action at once, recording `StudyFit (StudyFitProgressed _)` and
    `StudyFit (StudyFitEnded _)` in a `ConcurrentQueue`;
  - the wait is `ManualResetEventSlim.Wait deadline`, never a sleep; a miss fails the fact naming the request (its
    stage, study and calibration use) and the deadline;
  - the recorded messages pass through the update in order, and the shell's own host is restored on the answer.

  The generic post-conditions moved with it, so AT-1 keeps every assertion: started, one completion message, no
  refusal, finished, handle released, the completed run committed, and the window's fit runs extended by exactly it.
  `runAcceptanceFit` keeps AT-1's request (`studyName`, the stage, `CalibrationHeldFixed`), its sixty seconds and its
  check that the committed run is the expected name. The two AT-1 checks that named `expectedName` (committed and
  appended) are now the completed run's name in the shared function plus the expected-name check in AT-1's wrapper,
  which together are the same statement.
- **How-to (2), AT-2's build.** `buildAbsorbingKit` keeps its clock cell (`let clock, time = …`), moves it to
  `absorbingFitTime` (2026-06-01T12:01:00Z) after `buildAbsorbingStudy`, then runs `absorbingFitRequest`
  (`{ study = AbsorbingTriclinic; stage = JointStage; calibrationUse = CalibrationHeldFixed }`) through
  `runStudyFitThroughShell` with 1800 seconds, and checks the committed run is `AbsorbingTriclinic@20260601T120100Z`.
  Nothing else is configured, because the defaults already are what the steps name:
  - the engine is `engineWith`'s choice (step 386); the fact asserts `NonlinearStudyEngine`;
  - the start count is the study's `FitStartCount.single` (step 382), and the start is the components' starts (step
    387); the fact asserts one member whose start is the entries' starts;
  - the deviates are `StudyFitEnvironment.create`'s `UniformDeviateProvider.fromSeed (NoiseSeed 1)` (step 393).
- **How-to (3), `assertAbsorbingRecovery (study : Study) (run : FitRun) : unit`.**
  - `study.promotedMaterialWith run absorbingMaterialVersion absorbingMaterial`: `absorbingMaterialVersion` is the
    file's existing `MaterialVersionId.firstOf absorbingMaterial.id` and `absorbingMaterial` is step 432's entry.
  - `CrystalParameterPhysics (AbsorbingTriclinicCrystal p)` gives `p`; any other physics fails naming it, and a refusal
    fails naming its `describe`.
  - `absorbingBandBreaches (fun q -> RelativeError.between (q.valueIn p) (q.valueIn absorbing))` must be empty; the
    message names every breach's `describe` and the run's χ² (each converged member's, as stored).
- **How-to (4), the fact** `AT-2's study engine recovers one seed's fifteen constants and derived quantities within the
  bands AbsorbingBiaxialNoisyTests asserts`:
  - `readFitRuns AbsorbingTriclinic` holds exactly one run, equal to the window's completed job (`kitFitRuns`);
  - `assertAbsorbingFitRun` checks:
    - name, study, stamp, `JointStage`, `CalibrationHeldFixed` and `NonlinearStudyEngine`;
    - the five plates' experiments in kit order;
    - fifteen entries in `AbsorbingTriclinicParameters.axes` order, each `MaterialParameterRef (version, axis)`, owned
      by `MaterialOwner version` and starting at the axis's read of `perturbedStart`;
    - one member that converged, starting at the entries' starts;
  - `assertAbsorbingRecovery` of the stored study and that run.
- **Opened `OpticalConstructor.Domain.StudyPublishing`** for `promotedMaterialWith`, as `KitWindowTests`,
  `ScrollExtentTests` and `StudyViewTests` do. `RelativeError` is written `MuellerFitting.RelativeError`, as the file
  already writes `MuellerFitting.throughputResidualFor`.
- **R0001–R0003.**
  - R0001's dispatch is untouched: AT-2's study has one material and no black box, so `engineWith` answers nonlinear
    without reaching the series rule.
  - R0002's analyzer law is untouched: AT-2's setups hold no analyzer slot.
  - R0003's provider still mints only the reflected detector's id, because the fit adds no slot; AT-1's three injected
    ids are unchanged.
- **No workaround in the test.** The profile pass cannot be shortened from `KitAcceptanceTests.fs` through the app's
  messages: `StudyMsg` sets no start count and no suspect thresholds. The host only chooses the watch, never the work.
  Writing the study's settings past the messages, loosening the deadline beyond the how-to, or dropping the profile
  pass would each change what the step was asked to prove, so none was done.

## Testing state

These are development runs, evidence only and not gate verdicts; the supervisor runs the gates.

- `dotnet build OpticalConstructor.Ui.Tests.fsproj -c Release`:
  - 0 errors after the `open`, and again after restoring the probe-free file
    (`.artifacts/0436-attempt-01-dev-build-final.log`);
  - the only warnings are the exempt NU1902 advisory;
  - the first, failing, build also recompiled the vendored MathNet and printed its pre-existing C# `SYSLIB0051`
    warnings, which step 0434's log has as well; they are not from this step's code.
- The twenty-one s42 invariants: each decoded `git grep` finds nothing.
- ui-tests selecting `StudyViewTests` and `DataStepTests`: `Passed!`, 18 of 18, in 29 s against 360 s.
- ui-smoke (`Category=ui-smoke`, every discovered test): `Passed!`, 264 of 264, in 232 s against 600 s.
- elevation was not run: the only edited file is in a test project, outside the ELEV scope (AGENTS.md).
- **kit-acceptance over `OpticalConstructor.Ui.Tests.KitAcceptanceTests`: `Failed!`, total 11, succeeded 6, failed 5, in
  1887 s against the 3600 s deadline.**
  - The new fact is discovered under its census id, `OpticalConstructor.Ui.Tests.KitAcceptanceTests.AT-2's study engine
    recovers one seed's fifteen constants and derived quantities within the bands AbsorbingBiaxialNoisyTests asserts`.
  - It fails with the other four AT-2 facts on the build's 1800 s fit deadline.
- **Measured: the engine's AT-2 fit does not end within 7171 s.** The probe ran the fact alone with the deadline raised
  to 7200 s; the fit started at 23:03:37 (`.artifacts/0436-probe/progress.log`).

  | Phase | Measured |
  |---|---|
  | One study residual (44 configurations) | 1.31 s, against the harness's 1.00 s |
  | Main LM fit (`fitNonlinear`) | ~363 s (last report at 359.6 s); the harness's identical seed-1 fit takes 278.5 s |
  | `resultsWith` before the first profile re-fit | ~593 s, with no progress report (first profile report at 953.5 s) |
  | Profile pass (`profileAt`, four suspects) | still running at 7171.6 s, after 993 reports over more than 6200 s |

  - **The results pass, ~500 residual evaluations.** `FittedProblem.jacobian` is a property, recomputed on every read.
    `sigmas` reads it once for its rank, and `report` computes a second forward-difference Jacobian (16 evaluations
    each). `sigmaAt` re-evaluates `sigmas`, and `suspectsWith` calls it once per coordinate: 15 × 2 Jacobians. Then
    every profile's own `sigmaAt` adds two more.
  - **The suspects**, from the harness's report at its seed-1 solution (`.artifacts/0436-probe/harness-suspects.log`):
    n1, n2 and n3, correlated pairwise at |ρ| 0.9999 (> 0.95), and g23, with σ at 0.107 of its span (> 0.1). Each
    takes six 14-parameter LM re-fits, about 30 s per Jacobian, along the indices' near-degenerate valley.
  - **The recovery is sound.** The harness's own seed-1 fit (same observations, search box and solver) recovers every
    reported quantity within its band (`.artifacts/0436-probe/harness-seed1.log`): n1–n3 ≤ 8.8e-5 (band 1e-3),
    birefringences ≤ 2.8e-4 (1.5e-3), e11–e12 ≤ 5.7e-3 (1.5e-2), isotropic absorption 7.3e-4 (2e-3), gyration ≤ 1.58
    (10). Since the engine's fit never completed, the new fact's other assertions (the stored run's shape and the
    promotion) have not yet run against a finished fit.
- **Blocked; `commit_ready` is false.** The conflicting obligations, and the authority each remedy needs:
  - **Obligations that cannot both hold:**
    - how-to (1)–(2): AT-2's fit through `runStudyFitThroughShell` within 1800 s, on the premise that one
      fifteen-parameter fit takes about three minutes (001 §1b);
    - Acceptance: kit-acceptance MUST pass over the class, under the gate's 3600 s attempt timeout.

    The engine's `runFit` measures over 7170 s for this study. Almost all of it goes to `StagedStudy.resultsWith`
    (steps 391 and 393), which the how-to's premise leaves out.
  - **Missing authority.** Step 0436's effects allow only `KitAcceptanceTests.fs`, and no app message reaches the
    settings that drive the profile pass. Each remedy needs an operator ruling:
    - (a) **Production, `OpticalConstructor.Optimization/FittedProblems.fs` and `StudyEngine.fs`.** Compute each
      problem's Jacobian, report and sigmas once rather than per read (about 10 minutes and ~33 evaluations per
      profile saved). Bound the profile pass's cost: its residual is 44 independent Berreman solves and its Jacobian
      columns are independent, on a 64-core machine. This is the recommended remedy: the 1800 s premise is reachable
      only through it.
    - (b) **Gates.** Raise AT-2's fit deadline and kit-acceptance's attempt timeout above the measured total, which
      is not yet known: more than 7170 s. Later AT-2 builds (steps 0437–0439) multiply that.
    - (c) **A study-settings message or seam** for `SuspectThresholds` (`Studies.fs`, the window). This would change
      what the fit run records, and it is not recommended.

## Artifacts

- `.artifacts/0436-attempt-01-dev-build.log` (the failing first build), `.artifacts/0436-attempt-01-dev-build-2.log`,
  `.artifacts/0436-attempt-01-dev-build-final.log` (the restored, probe-free file).
- `.artifacts/0436-attempt-01-dev-kit-acceptance.log`, `.artifacts/0436-attempt-01-dev-ui-tests.log`,
  `.artifacts/0436-attempt-01-dev-ui-smoke.log`; their TRX reports are in `.artifacts/0436-attempt-01-dev-results/`.
- `.artifacts/0436-probe/` holds the probe material:
  - `KitAcceptanceTests.fs.saved`, the file before the probe, which equals the file now;
  - `apply_probe.py` and `build.log`;
  - `progress.log`, the residual timings and the fit's timestamped progress stream;
  - `kit-acceptance-probe.log`;
  - `harness/harness-seed1.fsx` with `harness-seed1.log`, and `harness/harness-suspects.fsx` with
    `harness-suspects.log`.

## Gotchas

- **The how-to's runtime premise does not hold.** "One fifteen-parameter fit takes about three minutes" (001 §1b) is
  the harness's LM solve alone: 278.5 s measured here. The study engine adds its results pass, with σ, quality and
  profiles of every suspect entry. For this crystal that is four profiles along a 0.9999-correlated index valley, and
  together they take over 95 % of the time.
- `FittedProblem`'s `jacobian`, `report` and `sigmas` are members recomputed on every read. Any caller that asks for
  `sigmaAt` per coordinate pays two Jacobians each time.
- The study residual is 1.31 s against the harness's 1.00 s for the same 44 configurations; the extra is the study's
  forward path (assignment, setups, branches).
- The progress stream counts from 1 again when the profile pass starts (`profilesFrom … (IterationCount 0)`). A report
  every ~1.05 s is one residual evaluation; the ~33 s gaps are Jacobians.
- A missed deadline leaves the fit running on the thread pool until the test process exits. A late `Set` on the
  disposed `ManualResetEventSlim` does not throw, so the process does not crash. In the gate it keeps one core busy
  through AT-1's facts.
- The new fact's name has no `@`, so it needs no `CompiledName` (unlike step 0433's).
