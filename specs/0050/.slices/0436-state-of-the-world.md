# Where we are

Step 0436 is AT-2's fifth part (spec 0050 S12.2; 001 §Q AT-2 "3. Fit"). Steps 0432–0435 built the library drop, the
items, the table, one experiment per plate, seed 1's attached observations, and the `AbsorbingTriclinic` study with its
fifteen components freed from `perturbedStart`. This step makes the build end by fitting that study through the shell's
fit host, and adds the fact that judges the recovered crystal by the noisy suite's bands.

The code is written as the how-to says, but the step is **blocked**: the study engine's fit for this study runs past
7170 s. That breaks the how-to's 1800 s deadline and kit-acceptance's 3600 s. The time goes to the engine's results
pass (σ, quality and χ² profiles of suspect entries), production code outside this step's authorized effects. An
operator ruling is needed.

# What's working

- AT-1 and AT-2 host their study fits through one `runStudyFitThroughShell (model) (request) (deadline)`.
- AT-1's build keeps its sixty seconds and every assertion; its six facts pass over the shared host.
- AT-2's build moves its clock to 2026-06-01T12:01:00Z and runs the joint fit, calibrations held fixed, with 1800 s.
- `assertAbsorbingRecovery` promotes the study's material from the run and asserts `absorbingBandBreaches` is empty.
- New fact: the stored fit run's shape, then `assertAbsorbingRecovery`. It fails today on the 1800 s fit deadline.

# Tests

These are development runs, evidence only and not gate verdicts; the supervisor's gates are authoritative.

- Build of the UI test project: 0 errors; the only warnings are the exempt NU1902 advisory.
- ui-tests selecting `StudyViewTests` and `DataStepTests` (both untouched): `Passed!`, 18 of 18, in 29 s against 360 s.
- ui-smoke over every discovered smoke test: `Passed!`, 264 of 264, in 232 s against 600 s.
- The twenty-one s42 invariants: each decoded `git grep` finds nothing.
- elevation was not run: the only edited file is in a test project, outside the ELEV scope.
- **kit-acceptance over `OpticalConstructor.Ui.Tests.KitAcceptanceTests`: `Failed!`, 6 of 11 succeeded, in 1887 s
  against 3600 s.**
  - The six AT-1 facts pass.
  - The five AT-2 facts fail on the shared build: `AbsorbingTriclinic@*.fitrun-json: expected the Joint fit of
    AbsorbingTriclinic (fixed) to end within 1800 s; actual no end`. The new fact is among them, discovered under its
    census id.
- **The probe: the fit does not end within 7171 s.** The fact ran alone with the deadline raised to 7200 s for the
  probe only; the file was then restored byte for byte.
  - The main LM fit took ~363 s. The harness's identical seed-1 fit takes 278.5 s, and a study residual costs 1.31 s
    against the harness's 1.00 s.
  - `resultsWith` then spent ~593 s before its first profile re-fit: each `sigmaAt` recomputes two Jacobians, once per
    coordinate.
  - Its profile pass over four suspects was still running at 7171.6 s. The suspects are n1, n2 and n3, at |ρ| 0.9999,
    and g23, at σ/span 0.107.
- **The recovery itself is sound.** The harness's own seed-1 fit holds every reported quantity inside its band; the
  worst are e13 at 5.7e-3 against 1.5e-2 and g23 at 1.58 against 10.
- No gate deadline was exceeded by a development run of the gate's own selection (kit-acceptance 1887 s against 3600 s,
  failed). The 7171 s probe is not a gate run. `commit_ready` is false: the step is blocked on authority.

```yaml
gates:
  kit_acceptance_tests: 6
  ui_smoke_tests: 264
  ui_tests: 18
```

These are this round's measured development counts, not a green checkpoint. kit-acceptance's 6 is below its baseline
of 10 because AT-2's shared build fails.

# Architecture

Everything is in `KitAcceptanceTests.fs`.

- **`runStudyFitThroughShell`** is the one fit-hosting composition:
  - `StudyFitJobHost.posting` whose `post` runs each action at once, the two messages recorded in a queue;
  - the wait on a `ManualResetEventSlim` for the given deadline; a miss fails the fact naming the request;
  - the recorded messages passed through the update in order;
  - started, one completion, no refusal, finished, handle released, and the completed run committed and appended.

  `runAcceptanceFit` is AT-1's wrapper: its request, sixty seconds and expected-name check.
- **AT-2's build** ends with `absorbingFitRequest` at `absorbingFitTime`, 1800 s, and checks the committed name
  `AbsorbingTriclinic@20260601T120100Z`. The engine, the single start and the seed-1 deviates are the defaults steps
  382, 386, 387 and 393 already compose, so nothing is configured.
- **`assertAbsorbingFitRun`** checks the stored run:
  - name, stamp, study, `JointStage`, `CalibrationHeldFixed`, `NonlinearStudyEngine`, the five runs;
  - fifteen entries in the axes' order, owned by the material, starting at `perturbedStart`;
  - one converged member starting at those starts.

  **`assertAbsorbingRecovery`** is `promotedMaterialWith` followed by `absorbingBandBreaches`; step 439 reuses it.

# Deferred

Nothing is deferred by choice. The step is blocked, not split: its fact cannot pass until the operator rules on the
fit's runtime. Each remedy needs authority this step lacks:

- (a) **Recommended: production, in `FittedProblems.fs` and `StudyEngine.fs`.** Compute each problem's Jacobian,
  report and sigmas once. Bound the profile pass's cost, for example by evaluating the 44 independent configurations,
  or the Jacobian's columns, in parallel.
- (b) **Gates:** raise AT-2's fit deadline and kit-acceptance's attempt timeout above the measured total, which is
  more than 7170 s and not yet known.
- (c) **A settings message or seam** for the suspect thresholds.

Steps 0437–0439 remain as planned.

# Gotchas

- **The how-to's premise, "one fifteen-parameter fit takes about three minutes" (001 §1b), is the harness's LM solve
  alone.** The study engine's `runFit` adds `resultsWith`. For this crystal that is four χ² profiles, six 14-parameter
  re-fits each, along a 0.9999-correlated index valley, which together take over 95 % of the time.
- **`FittedProblem`'s `jacobian`, `report` and `sigmas` are recomputed on every read.** `suspectsWith` calls
  `sigmaAt` per coordinate, so the results pass costs ~500 residual evaluations before any profile.
- **No app message sets a study's start count or suspect thresholds.** The test therefore cannot shorten the pass
  through the window, and it did not reach past the messages to do so.
- **A missed deadline leaves the fit running on the thread pool until the process exits.** The late `Set` on the
  disposed event does not throw, so the process does not crash.

# Changelog

- 2026-11-15/16: Step 0436, AT-2's fifth part.
  - AT-1 and AT-2 share `runStudyFitThroughShell`.
  - AT-2's build ends with the study's joint fit; `assertAbsorbingRecovery` and the recovery fact were added.
  - Blocked: the engine's fit runs past 7170 s, so it breaks the how-to's 1800 s and kit-acceptance's 3600 s.
    The measurements and the remedies needing authority are in the impl-log.
