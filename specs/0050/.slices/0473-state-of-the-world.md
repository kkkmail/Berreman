# Step 0473 — state of the world

## Where we are

Spec 0050 section S14.1 (the Measurement Planner: kit inputs and the structural layer). Steps 465–472 built the
Planner's inputs, its structural report and its looks.

This step is the MOVE that prepares step 475's acceptance facts. Spec 0044's quartz ground truth and its C1-C4
measurement design leave the `MuellerInverseTests` class and become `BerremanTestSupport.QuartzSample`. The Planner's
facts can then read C1, C3 and the full set instead of transcribing them (S14.1 brief; 001 §M, §Q; AGENTS.md "Reuse,
never copy").

## What's working

- Moved spec 0044's quartz ground truth and C1-C4 design into the new `BerremanTestSupport.QuartzSample` module:
  - `waveLength`, the indices and gyration constants, `quartz`;
  - the plates, `configuration`, `c1`–`c4`, `fullConfigurations`.
- `MuellerInverseTests` now opens `BerremanTestSupport.QuartzSample` and declares none of the moved names. Every fact
  and every other binding is unchanged.
- The moved bodies are byte-identical. Only the three banner wordings the step names changed.
- `BerremanTestSupport.fsproj` compiles `QuartzSample.fs` last. No reference was added.

## Tests

Development runs only; the arc-runner's gate engine is the authority.

- **build:** 0 errors in 81 s (deadline 600 s).
  - Warnings are only `NU1902` (exempt) and the vendored-MathNet `SYSLIB0051` (pre-existing).
  - No FS warning.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` in 227 s (deadline 600 s).
  - ELEV004 is excluded and the known findings are 0.
  - The suppressed count is 1767, the same as step 0472.
- **unit-tests, filtered to the selected `BerremanTests.MuellerInverseTests`:** `Passed!`, 20 of 20 (failed 0, skipped 0)
  in 361 s (deadline 7200 s). The count is unchanged: the class declares 20 facts, the step's 20 `change_expectation`
  identities. No test was added or removed.
- **kit-acceptance (selected `OpticalConstructor.Ui.Tests.KitAcceptanceTests`):** not run locally; left to the gate
  engine.
  - `KitAcceptanceTests` reads none of the moved names.
  - Its only contact with this step is compiling against the changed `BerremanTestSupport`, which the build covered.
  - The run takes about 30 minutes.
- **21 × invariant-s45-\*:** every decoded `git grep` finds nothing, also with `--untracked` so the new file is
  searched. The positive control finds 3.
- Line endings: LF only. No BOM, as in the sibling support files.

```yaml
gates:
  berreman_unit_tests: 20
```

## Architecture

- `QuartzSample` follows `BiaxialSample.fs`'s form exactly:
  - `namespace BerremanTestSupport` + `module QuartzSample =`;
  - the module doc cites spec 0044 and spec 0050 S14.1;
  - it opens only the engine, Domain and Optimization modules the bodies read, and never the `BerremanTests`
    namespace.
- `MuellerInverseTests` opens `QuartzSample` as its LAST `open`. That reproduces the scope the class-level `let`s had,
  because they shadowed every earlier `open`.
- The values become module values computed once, not class values computed per instance. Every expression is unchanged,
  so the numbers are bit-identical (.spec-md §4).
- `solverParameters` and `forward` stay in the class, as the step names. The design does not depend on the solver.

## Deferred

Nothing. The step's three edits (the new file, the project entry and the test file's move-out) all landed.

## Gotchas

- **The step's wording.** It writes the module as `module BerremanTestSupport.QuartzSample` and also says "declared as
  `BiaxialSample.fs` is". The namespace + nested-module form gives the same fully qualified name.
- **The `double` parameters.** `QuartzSample.configuration` keeps `incidenceDeg : double` and `azimuthDeg : double`,
  unchanged as the step requires. That is ELEV-clean only because the `BerremanTestSupport` assembly sits outside
  `inScopePatterns` (`^Softellect\.Berreman\.`). Moving it into a product assembly would need elevated angles.
- **`git grep` and new files.** `git grep` does not search untracked files. An invariant check before the commit
  therefore needs `--untracked` to see a new file.

## Changelog

- 2026-11-17 — Step 0473: moved MuellerInverseTests' quartz ground truth and its C1-C4 design into
  `BerremanTestSupport.QuartzSample` (S141-QUARTZ-DESIGN-MOVE). Every value is unchanged and no test was added or
  removed.
