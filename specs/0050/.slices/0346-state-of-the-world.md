# Step 0346 — state of the world

## Where we are

Spec 0050 section S10.2 ("BerremanTestSupport: the shared ground truth and designs"). Step 0344 created the
`BerremanTestSupport` library and moved `CommittedKits` into it, step 0345 moved the transparent ground truth
`BiaxialSample`, and this step moves its absorbing twin `AbsorbingBiaxialSample` (constants, `buildAbsorbing`, the
cuts, `a1` / `a2` / `a3`, `perturbedStart`, `searchBox`) beside it, so one compiled copy serves every consumer and
`BerremanTests` compiles no ground-truth file.

## What's working

- Moved `AbsorbingBiaxialSample` from `BerremanTests` into `BerremanTestSupport` (namespace `BerremanTestSupport`),
  every declaration and number unchanged; `buildAbsorbing` still calls `AbsorbingTriclinicParameters.opticalProperties`
- Dropped its unused `open BerremanTests.InverseFitHarness`; the module doc gains one S10.2 sentence
- `BerremanTestSupport` compiles `AbsorbingBiaxialSample.fs` after `BiaxialSample.fs`; `BerremanTests` no longer does
- Both absorbing suites open `BerremanTestSupport.AbsorbingBiaxialSample`

## Tests

- build: green — exit 0, 0 errors, 78 s (deadline 600 s); warnings only NU1902 (exempt) and the pre-existing
  vendored-MathNet SYSLIB0051, the same six as steps 0344 and 0345.
- elevation: green — `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 223 s (deadline 600 s).
- unit-tests (selected `BerremanTests.AbsorbingBiaxialInverseTests`): green — `Test run summary: Passed!`, 7 of 7,
  193 s (deadline 7200 s). No test added or removed; no assertion changed.
- The twenty s34 invariants: every `git grep` finds nothing (also run with `--untracked` to cover the moved file).
- No run exceeded its deadline. Nothing deferred.

```yaml
gates:
  berreman_unit_tests: 7
```

## Architecture

- Exactly one `module AbsorbingBiaxialSample` exists, in `Berreman/BerremanTestSupport/AbsorbingBiaxialSample.fs`,
  and it opens no `BerremanTests` module, so `BerremanTestSupport` still references no test project. No new
  project reference was needed: Domain and Optimization were already referenced in step 0345.
- No product project references `BerremanTestSupport`; only `BerremanTests.fsproj` does. The physics stays in the
  product kernel (`AbsorbingTriclinicParameters.opticalProperties`); the sample only names it.

## Deferred

Nothing.

## Gotchas

- The brief says `git mv`; the worker protocol forbids git mutation, so the file was moved with a plain `mv`
  (as in steps 0344 and 0345). Git sees `D Berreman/BerremanTests/AbsorbingBiaxialSample.fs` plus an untracked new
  file until commit, where the rename is detected.
- The S10.2 sentence names `OpticalConstructor.Ui.Tests` as a consumer; that project does not yet reference
  `BerremanTestSupport` and is outside this step's edits.

## Changelog

- 2026-11-12 — step 0346: `AbsorbingBiaxialSample` moved to `BerremanTestSupport`; build, elevation, the selected
  `AbsorbingBiaxialInverseTests` (7/7) and the s34 invariants green.
