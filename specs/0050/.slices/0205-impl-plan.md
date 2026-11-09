# Step 0205 — impl-plan (IMPLEMENT: the parameter vocabulary moves to `ItemModels`)

## Approach

A pure move, no behaviour change (.spec-md §4: a reuse is a move). Items, item models and the fitter share
`ParameterName` and `DialSign`; both are declared today in `MuellerInverse.fs` / `MuellerReconstruction.fs`, which
compile last in Domain, after `LibraryEntryFiles.fs` and `Kit.fs` that must name them (S7.1).

1. New `OpticalConstructor.Domain/ItemModels.fs`: `module ItemModels` in `namespace OpticalConstructor.Domain`, a
   one-to-three-line module doc (the parameter vocabulary of items and item models, spec 0050 S7.1). One
   `<Compile Include="ItemModels.fs" />` line in the Domain fsproj, directly after `ElementId.fs` and before
   `LibraryEntryFiles.fs`, no comment.
2. `ParameterName` moves into it unchanged; its doc keeps its text and adds that spec 0050 S7.1 moved it here.
   `MuellerInverse.fs` opens `OpticalConstructor.Domain.ItemModels` and no longer declares it.
3. `DialSign` moves after it unchanged, doc likewise. `MuellerReconstruction.fs` opens `ItemModels`.
4. Consumers open `OpticalConstructor.Domain.ItemModels` only where the compiler asks; nothing else changes. By
   `git grep -w`, the files naming either type are `DesignParameters.fs` (Optimization) and, in BerremanTests,
   `InverseFitHarness.fs`, `MuellerInverseTests.fs`, `BiaxialInverseTests.fs`, `OptimizationTests.fs` and
   `MuellerReconstructionTests.fs`. The other authorized BerremanTests files (`AbsorbingBiaxialInverseTests.fs`,
   `AbsorbingBiaxialNoisyTests.fs`, `FeasibleBiaxialInverseTests.fs`) do not name either type; they get an `open`
   only if the build asks for one.

## Tests

- New `OpticalConstructor.Tests/ItemModelsTests.fs` (`module ItemModelsTests` in `namespace
  OpticalConstructor.Tests`), appended last in the compile list with no comment, opening
  `OpticalConstructor.Domain.ItemModels`. One fact, `the moved parameter name and dial sign keep their values`:
  `(ParameterName "c2.retardance").value` = `c2.retardance`, `DialPositive.value` = 1.0, `DialNegative.value` = −1.0.
- Red first: the test is written before `ItemModels.fs` exists, so the test project fails to compile (FS0039 on
  `OpticalConstructor.Domain.ItemModels`).
- No BerremanTests fact changes; `OptimizationTests` and `MuellerReconstructionTests` run unchanged.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemModels.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerInverse.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/DesignParameters.fs`
- `Berreman/BerremanTests/` consumers listed above
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemModelsTests.fs` (new) and the test fsproj

## Risks

- ELEV014/016: the moved types must exist exactly once — no alias or re-export left behind.
- A module name `ItemModels` must not collide (checked: no `ItemModels` anywhere in the tree).
- Opening `ItemModels` in a file that does not need it would be an unrequested change; open only where the compiler
  reports FS0039.
