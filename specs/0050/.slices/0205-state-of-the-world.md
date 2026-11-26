# Step 0205 — state of the world (IMPLEMENT: the parameter vocabulary moves to `ItemModels`)

## Where we are

Spec 0050, section S6 (send and receive kits; copy experiments between kits), closing on its S7 prerequisite.

- Steps 196–204 built the send side and the copy's pure halves and byte-exact read.
- This step gives items, item models and the fitter one home for their shared parameter vocabulary, compiled early
  enough for the library entries and the kit (S7.1). `ParameterName` and `DialSign` moved from
  `MuellerInverse.fs` / `MuellerReconstruction.fs` (which compile last in Domain) into the new `ItemModels.fs`,
  compiled directly after `ElementId.fs` and before `LibraryEntryFiles.fs`.
- No behaviour changed. S7 builds the items and item models on the moved types.

## What's working

- Add `OpticalConstructor.Domain/ItemModels.fs` (`module ItemModels`), compiled after `ElementId.fs` and before
  `LibraryEntryFiles.fs`.
- Move `ParameterName` (from `MuellerInverse`) and `DialSign` (from `MuellerReconstruction`) into it unchanged;
  no alias, re-export or second declaration remains.
- Open `ItemModels` in `MuellerInverse.fs`, `MuellerReconstruction.fs`, `DesignParameters.fs` and five
  BerremanTests files where the compiler asked; nothing else in them changed.
- Add fact `the moved parameter name and dial sign keep their values` (constructor unit tests 1264 → 1265).

## Tests

These are development runs, recorded in `0205-impl-log.md`. The gate engine gives the verdict (Invariant 6).

- **build:** exit 0, 0 warnings, 0 errors.
- **constructor-unit-tests:** 1265 passed, 0 failed, 0 skipped (1264 → 1265, +1). The new test identity is
  `Passed` in the TRX.
- **unit-tests** (selected: `BerremanTests.MuellerReconstructionTests`, `BerremanTests.OptimizationTests`): 37
  passed, 0 failed, 0 skipped (29 + 8, the same facts HEAD declares; no fact changed).
- **elevation** (from the arc origin): `ELEV-GATE: OK`, 0 new findings in 12 of 12 projects (ELEV004 excluded).
- **The fourteen `invariant-s22-*` greps:** each finds nothing.

```yaml
gates:
  constructor_unit_tests: 1265
  berreman_unit_tests: 37
```

`berreman_unit_tests` is the count under this step's class filter.

## Architecture

- **`ItemModels` is the one home of the parameter vocabulary** shared by items, item models and the fitter
  (spec 0050 S7.1; .spec-md §4: a reuse is a move). It depends on nothing, so any Domain file from
  `LibraryEntryFiles.fs` on (the library entries, `Kit.fs`) can name `ParameterName` and `DialSign`.
- **The module is not `[<AutoOpen>]`.** A consumer opens `OpticalConstructor.Domain.ItemModels` explicitly, as it
  opened `MuellerInverse` / `MuellerReconstruction` before.

## Deferred

Nothing in this step's scope.

- The items and item models that use the moved types are S7 steps, by the spec's plan.

## Gotchas

- **`ItemModels.fs` and `ItemModelsTests.fs` are untracked until the operator's commit**, so a plain `git grep`
  does not see them; the declaration check needs `--untracked` until then.
- **Three authorized BerremanTests files needed no edit** (`AbsorbingBiaxialInverseTests.fs`,
  `AbsorbingBiaxialNoisyTests.fs`, `FeasibleBiaxialInverseTests.fs`): they only read `….name.value`, which needs no
  `open`. They are unchanged.
- **Building `OpticalConstructor.Tests` alone recompiles the vendored `MathNetNumerics/Numerics`** and prints two
  `SYSLIB0051` warnings from `Exceptions.cs`; the solution build (the gate's command) prints none.

## Changelog

- 2026-11-09 — step 0205: `ParameterName` and `DialSign` moved unchanged into the new `ItemModels.fs` (after
  `ElementId.fs`); consumers open it; one new fact.
