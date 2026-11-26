# Step 0345 — state of the world

## Where we are

Spec 0050 section S10.2 ("BerremanTestSupport: the shared ground truth and designs"). Step 0344 created the
`BerremanTestSupport` library and moved `CommittedKits` into it; this step moves the transparent triclinic ground
truth `BiaxialSample` there whole, so one compiled copy can serve every biaxial suite. Step 0346 moves
`AbsorbingBiaxialSample` next.

## What's working

- Moved `BiaxialSample` from `BerremanTests` into `BerremanTestSupport` (namespace `BerremanTestSupport`),
  every declaration, number and open unchanged; `buildTriclinic` still calls `TriclinicParameters.opticalProperties`
- `BerremanTestSupport` now compiles `BiaxialSample.fs` and references `OpticalConstructor.Optimization`
  (for `RecoveredQuantity` / `QuantityValue`)
- `BerremanTests` no longer compiles `BiaxialSample.fs`; dropped its comment and the stale `InverseFitHarness.fs` one
- The five biaxial consumers open `BerremanTestSupport.BiaxialSample`
- Module doc: "every biaxial suite shares", one S10.2 sentence, and "in test code"

## Tests

- build: green — exit 0, 0 errors, 77 s (deadline 600 s); warnings only NU1902 (exempt) and the pre-existing
  vendored-MathNet SYSLIB0051, the same six as step 0344.
- elevation: green — `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 217 s (deadline 600 s).
- unit-tests (selected `BerremanTests.BiaxialInverseTests`, `BerremanTests.FeasibleBiaxialInverseTests`): green —
  `Test run summary: Passed!`, 16 of 16 (11 + 5), 1575 s (deadline 7200 s). No test added or removed; no
  assertion changed.
- The twenty s34 invariants: every `git grep` finds nothing (also run with `--untracked` to cover the moved file).
- Nothing deferred.

```yaml
gates:
  berreman_unit_tests: 16
```

## Architecture

- `BerremanTestSupport` now depends on `OpticalConstructor.Domain` and `OpticalConstructor.Optimization`
  (Optimization references only Domain, so no cycle). No product project references `BerremanTestSupport`;
  only `BerremanTests.fsproj` does.
- Exactly one `module BiaxialSample` exists, in `Berreman/BerremanTestSupport/BiaxialSample.fs`. The consumers
  are unchanged apart from the namespace in their `open`; no qualified `BiaxialSample.x` use exists in code.

## Deferred

Nothing.

## Gotchas

- The brief says `git mv`; the worker protocol forbids git mutation, so the file was moved with a plain `mv`
  (as in step 0344). Git sees `D Berreman/BerremanTests/BiaxialSample.fs` plus an untracked new file until commit,
  where the rename is detected.
- The brief's doc line numbers (`:13`, `:23`) are two lines off the committed file (15, 25); the quoted text
  located each edit.
- The mandated S10.2 doc sentence names `OpticalConstructor.Ui.Tests` as a consumer; that project does not yet
  reference `BerremanTestSupport` and is outside this step's edits.

## Changelog

- 2026-11-12 — step 0345: `BiaxialSample` moved to `BerremanTestSupport`; build, elevation, the selected
  BerremanTests classes (16/16) and the s34 invariants green.
