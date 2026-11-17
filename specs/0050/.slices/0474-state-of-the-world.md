# Step 0474 — state of the world

## Where we are

Spec 0050 section S14.1 (the Measurement Planner: kit inputs and the structural layer). Step 473 moved the quartz
ground truth and its C1-C4 design into `BerremanTestSupport.QuartzSample`.

This step is its biaxial twin, and the second MOVE that prepares step 475's acceptance facts. Spec 0044's biaxial
measurement design leaves the `BiaxialInverseTests` class and becomes `BerremanTestSupport.BiaxialDesign`, beside its
ground truth `BiaxialSample`. The Planner's facts can then read the normal-incidence and full designs instead of
transcribing them (S14.1 brief; 001 §M; AGENTS.md "Reuse, never copy").

## What's working

- Moved `BiaxialInverseTests`' measurement design into the new `BerremanTestSupport.BiaxialDesign` module: the
  measurement-set banner, `plate`, `configuration`, `principalCuts`, `b1`–`b3` and `fullConfigurations`.
- Promoted the ablation fact's local `normalOn` to `BiaxialDesign.normalIncidenceOn`. `b1` is now
  `principalCuts |> List.collect normalIncidenceOn`, so the azimuth list is written once.
- `BiaxialInverseTests` now opens `BerremanTestSupport.BiaxialDesign` and declares none of the moved names. Its
  ablation fact builds `zOnly`, `zAndX` and `everyCutNormal` from `normalIncidenceOn`. Every fact and every other
  binding is unchanged.
- `BerremanTestSupport.fsproj` compiles `BiaxialDesign.fs` last. No reference was added.

## Tests

Development runs only; the arc-runner's gate engine is the authority.

- **build:** 0 errors in 85 s (deadline 600 s).
  - Warnings are only `NU1902` (exempt) and the vendored-MathNet `SYSLIB0051` (pre-existing).
  - No FS warning.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` in 224 s (deadline 600 s).
  - ELEV004 is excluded and the known findings are 0.
  - The suppressed count is 1767, the same as step 0473.
- **unit-tests, filtered to the selected `BerremanTests.BiaxialInverseTests`:** `Passed!`, 11 of 11 (failed 0, skipped 0)
  in 792 s (deadline 7200 s). The count is unchanged: the class declared 11 facts before and after, the step's 11
  `change_expectation` identities. No test was added or removed. The fits' pinned bands all held, so the configurations
  feed them the same data.
- **kit-acceptance (selected `OpticalConstructor.Ui.Tests.KitAcceptanceTests`):** not run locally; left to the gate
  engine, as at step 0473.
  - `KitAcceptanceTests` reads none of the moved names.
  - Its only contact with this step is compiling against the changed `BerremanTestSupport`, which the build covered.
  - The run takes about 30 minutes.
- **21 × invariant-s45-\*:** every decoded `git grep` finds nothing, also with `--untracked` so the new file is
  searched. The positive control (`BiaxialDesign`) finds 6.
- Line endings: LF only (0 CR bytes in all three files). No BOM, as in the sibling support files.

```yaml
gates:
  berreman_unit_tests: 11
```

## Architecture

- `BiaxialDesign` follows the form of `BiaxialSample.fs` and `QuartzSample.fs`:
  - `namespace BerremanTestSupport` + `module BiaxialDesign =`;
  - the module doc cites spec 0044 and spec 0050 S14.1;
  - it opens only the engine, Domain, Optimization and `BiaxialSample` modules the bodies read, and never the
    `BerremanTests` namespace.
- The design sits in its own module rather than inside `BiaxialSample`. `FeasibleBiaxialInverseTests` and the absorbing
  suites measure the same crystal with different designs, keep their own, and do not open it.
- `configuration` reads `BiaxialSample.referenceWaveLength` directly. The class's `waveLength` alias held the same value.
- `BiaxialInverseTests` opens `BiaxialDesign` as its LAST `open`. That reproduces the scope the class-level `let`s had,
  because they shadowed every earlier `open`.
- The values become module values computed once, not class values computed per instance. Every configuration list holds
  the same configurations in the same order, so the numbers are bit-identical (.spec-md §4).

## Deferred

Nothing. All three of the step's edits landed: the new file, the project entry, and the test file's move-out and
switch to `normalIncidenceOn`.

## Gotchas

- **Re-pointed comments.** Three comments left in the class named the moved banner by position ("the thickness
  commentary above", "the measurement set above"). They now name `BiaxialDesign`, so they do not dangle. No other
  comment changed.
- **The unread alias.** The class alias `waveLength` stays, as the step requires, although no remaining binding reads it.
  FS1182 is not enabled, so it raises no warning.
- **Same-named designs.** `AbsorbingBiaxialSample` (in the same assembly) and `FeasibleBiaxialInverseTests` declare
  their own `plate` / `configuration` / `fullConfigurations`. A file that opened `BiaxialDesign` together with
  `AbsorbingBiaxialSample` would have the later `open` win silently. Step 475's Planner facts should qualify or open
  exactly one design.
- **The `double` parameters.** `BiaxialDesign.configuration` keeps `incidenceDeg : double` and `azimuthDeg : double`,
  unchanged as a MOVE requires. That is ELEV-clean only because the `BerremanTestSupport` assembly sits outside
  `inScopePatterns` (`^Softellect\.Berreman\.`).
- **`git grep` and new files.** `git grep` does not search untracked files. An invariant check before the commit
  therefore needs `--untracked` to see the new file.

## Changelog

- 2026-11-17 — Step 0474: moved BiaxialInverseTests' measurement design (plate, configuration, principal cuts,
  `normalIncidenceOn`, B1-B3, the full set) into `BerremanTestSupport.BiaxialDesign` (S141-BIAXIAL-DESIGN-MOVE). Every
  configuration is unchanged and no test was added or removed.
