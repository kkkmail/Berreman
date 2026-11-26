# Step 0465 — impl plan

The Measurement Planner's layer-1 structural table as pure Domain data (spec 0050 S14.1).

## Approach

1. New `OpticalConstructor.Domain/PlannerSymmetry.fs` (`namespace OpticalConstructor.Domain`,
   `module PlannerSymmetry`), appended last in the Domain compile list:
   - `ComponentCount of int` with `.value`;
   - extension `Anisotropy.independentIndexCount` (1 / 2 / 3), reusing the editor's `Anisotropy`;
   - `Centrosymmetry` and `Gyrotropy`, two-case derived DUs, no wire tag;
   - `PointGroup`, the 32 classes named by crystal class, with complete-match members `symbol`,
     `anisotropy`, `centrosymmetry`, `gyrotropy`, `orientationAngleCount` and `static member all`;
   - `CrystalSymmetry = { anisotropy; pointGroups }` with `independentIndexCount` and
     `orientationAngleCount` (largest of its groups', 0 for none);
   - extension `CrystalParameters.symmetry`, reading `CrystalParameters.anisotropy` (LibraryFacets)
     and naming the uniaxial record's six gyrotropic classes (3/32/4/422/6/622), the triclinic
     records' class 1.
2. New `OpticalConstructor.Tests/PlannerSymmetryTests.fs` (`namespace OpticalConstructor.Tests`,
   `module PlannerSymmetryTests`), appended last in the test compile list, with the three facts the
   step's effects name. The crystal records come from the test project's existing
   `CrystalMaterialFixtures` (literal records with exactly the values the slice describes) — reuse,
   not a second copy.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/PlannerSymmetry.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerSymmetryTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Risks

- ELEV016 / ELEV014 name collisions: checked by grep, none of the 32 case names, the four
  class cases or the five type names exist in the tree.
- ELEV005 in `CrystalSymmetry.orientationAngleCount`: fold over `ComponentCount` with `max`
  (structural comparison), never unwrap and re-wrap.
