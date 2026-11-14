# Step 0403 — impl log

## Progress

- [x] `StagedStudy.resultsWith` in `StudyEngine.fs`, after `realizabilityWith`, before `StudyFitRequest`.
- [x] Malus intensity-study fixtures and five `StudyEngineTests` facts. All five pass (3.8 s together).
- [x] Mutation check: seven planted defects, each caught (see Artifacts).
- [x] Development gate runs (see Testing state).
- [x] Tidied two long test lines and one comment, rebuilt, and re-ran the whole constructor suite (1725/1725).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StudyEngine.fs`: +44 lines, the one member
  `resultsWith`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs`:
  - `open OpticalConstructor.Domain.ItemEvaluation`, for `SetupSlot.runStepOf`;
  - the Malus analyzer draft (draft 18), added to `drafts`;
  - the Malus fixtures and the five facts, before the "one fit, one fit run" section.

## Decisions

- **One member.** `resultsWith` reads the solution's `engineProblemsWith`, then `realizabilityWith`, then each of
  `suspectsWith this.study.settings.suspect solution.layout` in order.
  - Each suspect's `problem.profileAt coordinate earlier watch` is folded through `Result`. `earlier` starts at
    `IterationCount 0` and grows by each `ProfileRun.iterations`.
  - `ProfileCancelledErr reason` answers `FitCancelledErr reason`, and the remaining suspects are not re-fitted.
  - No suspect means no `profileAt` call and so no re-fit. Nothing is cached, retried or written.
- **S₀ by `ExperimentRun.read Intensity`.** The fixture binds M1 at the items' own values and takes the bound path's
  steps at each reading (`muellerAt` at `orientationAt`, then `runStepOf`). It reads them at unit scale, the bound
  emission with `intensityScale` 1.
  - The HeNe's default emission is unpolarized and the leading LP sets the path's reference, so S₀ = cos² θ, not the
    slice's ½ cos² θ. The expected correlation is invariant to that factor, so −√(2/3) holds as the slice states.
- **Malus items.** The HeNe is the file's `laser`, the prep the file's `lp1` (the ideal-LP preset, fixed, at R1 0°),
  and the analyzer a new fixed ideal-LP draft. A fixed analyzer is turned by `VaryR1`; the dial-mounted LP2 would be
  turned by its dial.
  - The camera is the Phone draft's item, with the gain unknown from 1.8 within [1, 4] and the dark offset in the
    fact's state. The scale is the HeNe's `intensityScale`, unknown from 1 within [0.25, 4] in the confounded study.
- **Malus setup.** One setup of the four, through the file's `benchOf` (`SetupSlot.withItem`, as `SetupTests` builds
  slots). The study is one Science run in the default session, its gain model free.
- **Fact 4 also checks a live watch.** Under an uncancelled watch, the pass reports 1, 2, …, N across both profiles,
  at least 14 counts, because ALGLIB's xrep reports each re-fit's start. This covers the "counts on from one" clause,
  which the `earlier-zero` mutant showed no other fact sees.
- **Fact 1 also checks the study's thresholds.** The same solution, its staged study given `tryCreate 0.8 0.1`,
  profiles both entries (|ρ| ≈ 0.816). The `initial-thresholds` mutant survived without it: nothing else in the five
  facts tells the study's thresholds from `SuspectThresholds.initial`.

## Testing state

Development runs on 2026-11-14. These are not gate verdicts. Every run was inside its effective deadline.

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`): exit 0, 0 errors.
  - The only warnings are the exempt `NU1902` advisories, plus the vendored MathNet's already-recorded `SYSLIB0051`
    when it recompiled (first build only).
  - 79 s first, 9 s after the mutation restore, 5 s final, against 600 s.
- **elevation** (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`): `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`,
  with no base, so all 13. 229 s against 600 s.
  - Production code has not changed since; the later edits touched the test file only, which is out of scope.
- **constructor-unit-tests**, whole suite, by the gate's command: 1725/1725, from 1720 + 5.
  - 137 s against 180 s, both before and after the final test-file tidy.
  - The five new facts take 3.2 s together, the series fact 2.8 s of it.
- **unit-tests**, selected `BerremanTests.MuellerReconstructionTests` and `BerremanTests.MuellerInverseTests`: 45/45
  passed. 676 s against 7200 s.
- **invariant-s39-*** (20): every command printed `INVARIANT HOLDS ... found nothing`.

Logs: `specs/0050/.artifacts/0403-dev-*.log`. Nothing blocks commit.

## Artifacts

- `specs/0050/.artifacts/0403-build-dev-1.log`, `0403-dev-build.log` and `0403-dev-build-final.log`: the builds.
- `0403-dev-constructor-unit-tests.log` and `0403-dev-constructor-unit-tests-final.log`: the whole constructor suite.
- `0403-dev-unit-tests.log`, `0403-dev-elevation.log` and `0403-dev-invariants.log`: the other development runs.
- `specs/0050/.artifacts/0403-mutation/`: the mutation runner, its per-mutant logs, `summary.log` and
  `summary-2.log`.
  - First pass: `earlier-zero`, `cancel-swallowed`, `no-realizability`, `no-run-residuals`, `no-product-checks` and
    `profiles-dropped` were each caught. `initial-thresholds` survived.
  - Second pass, with fact 1 extended: `earlier-zero` and `initial-thresholds` were both caught.
  - The source was restored and checked byte-identical (`cmp`).

## Gotchas

- `MalusFit` labels are prefixed (`malusStaged`, …). The bare `staged`, `data` and `solution` labels belong to other
  records, and an unannotated construction after the type would infer the wrong one.
- The scale profile holds the scale at 0.25, which needs a gain of 8. That re-fit ends on the gain's upper bound 4 and
  still answers `ProfileConverged`, so the fact asserts convergence only, not flatness, as the slice does.
- The build prints two `SYSLIB0051` warnings from the vendored MathNet whenever it recompiles. These were already
  recorded (`0185-attempt-01-task.txt`) and are not owned code.
