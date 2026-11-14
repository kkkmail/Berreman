# Step 0403 — state of the world

## Where we are

Spec 0050 S11.4 (fit results) continues. Step 402 gave each engine's problems their σ, suspects and realizability.
This step gathers them into one `FitRunResults` and profiles every suspect entry.

`StagedStudy.resultsWith` reads a solution's:

- σ per entry and quality per solve;
- run residuals, black-box realizability and series product checks;
- a χ² profile for each suspect entry, by re-fits under the fit's own watch.

Synthetic intensity studies test it: an identifiable gain / dark-offset pair, an exactly confounded gain / source-scale
pair, and a nearly confounded pair.

## What's working

- Added `StagedStudy.resultsWith`. It gathers a fit's σ, problem qualities, run residuals, realizability and product
  checks.
- Each suspect entry is profiled by `profileAt` under the fit's watch, counted on across the profiles. A cancel answers
  `FitCancelledErr`, and a fit with no suspect re-fits nothing.
- Added five `StudyEngineTests` facts over test-local Malus intensity studies and step 390's series fixture.
  Constructor tests rose from 1720 to 1725.

## Tests

Development runs on 2026-11-14. These are not gate verdicts; the supervisor re-runs every gate. Every run finished
within its effective deadline.

| Gate | Development outcome | Duration / deadline |
|---|---|---|
| build | exit 0, 0 errors, only the exempt `NU1902` advisories | 79 s (first, MathNet recompiled), 5 s (final) / 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` (no base: all 13) | 229 s / 600 s |
| constructor-unit-tests | 1725/1725 passed, whole suite (1720 + 5) | 137 s / 180 s |
| unit-tests (selected `MuellerReconstructionTests`, `MuellerInverseTests`) | 45/45 passed | 676 s / 7200 s |
| invariant-s39-* (20) | each ran its own command: `INVARIANT HOLDS ... found nothing` | < 1 s each / 300 s |

The five new facts all passed:

- `an identifiable pair is determined, with no suspect and no profile`
- `an exactly confounded pair has no sigma, and its profile is flat along the confounded direction`
- `a nearly confounded pair is suspect by its correlation and profiled across three sigmas`
- `a profile pass under a cancelled watch answers FitCancelledErr`
- `a fit's results gather its sigmas, solves, run residuals, realizability and product checks`

A mutation check planted seven defects in `resultsWith`, and each turned a new fact red (see the impl-log). The new
facts add about 3.2 s to `StudyEngineTests`, now 134.7 s serial and still the suite's critical path.

```yaml
gates:
  constructor_unit_tests: 1725
  berreman_unit_tests: 45
```

`berreman_unit_tests` is the two selected classes' own count, as at step 402.

## Architecture

- **One gathering member.** `resultsWith` reads the problems from `engineProblemsWith`, so the results always come
  from the solves the solution's own engine made. Realizability comes from `realizabilityWith` at `solution.fitted`.
- **Profiles are a fold over suspects.** For each of `suspectsWith this.study.settings.suspect solution.layout`, in
  order, `profileAt` runs under `watch`.
  - `earlier` starts at 0 and grows by each profile's `ProfileRun.iterations`, so one live watch counts 1, 2, …, N over
    the whole pass.
  - The first `ProfileCancelledErr reason` becomes `FitCancelledErr reason`, and no later suspect is re-fitted.
- **Nothing is written, cached or retried.** No suspect means no `profileAt` call and so no re-fit. A forward or
  evaluation error answers through the existing `StudyFitError` cases.
- `resultsWith` sits after `realizabilityWith` and before `StudyFitRequest`, as the slice places it.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **S₀ is cos² θ, not ½ cos² θ.** The HeNe emits unpolarized light, and `SourceEmission.emittedInto` normalizes to the
  leading LP. The slice's correlation −√(2/3) is invariant to that factor and holds within 1e-6.
- **The two extra checks are deliberate.** In the mutation check, two defects survived the slice's literal assertions:
  - `SuspectThresholds.initial` used in place of the study's thresholds;
  - `earlier` never accumulating.

  So fact 1 re-reads the same solution under `tryCreate 0.8 0.1` and gets both entries profiled. Fact 4 checks that a
  live watch counts 1…N over both profiles.
- **The confounded scale profile is not flat.** Holding the scale at 0.25 needs a gain of 8, so that re-fit ends on the
  gain's bound 4. The fact asserts convergence only, as the slice does.
- **New test-file open and label prefixes.** `StudyEngineTests` now opens `OpticalConstructor.Domain.ItemEvaluation`
  for `SetupSlot.runStepOf`. `MalusFit`'s labels are prefixed (`malusStaged`, …) so later record constructions do not
  infer it.
- **Harmless vendored warnings.** A full rebuild of the vendored MathNet prints two `SYSLIB0051` warnings. They were
  already recorded and are not owned code.

## Changelog

- 2026-11-14 — Step 0403: added `StagedStudy.resultsWith` to `StudyEngine.fs`, gathering a fit's results and profiling
  its suspect entries under the fit's watch. Added five `StudyEngineTests` facts over test-local Malus intensity studies
  and step 390's series fixture.
