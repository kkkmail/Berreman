# Step 0465 — impl log

## Progress

- Read the system prompts, project prompt, rulings R0001–R0005 (all confined to 0427/0428/0436;
  this step touches none of their files, and their preserved behaviour is untouched) and the slice.
- Checked the reused seams: `Anisotropy` (`MaterialComplexityEditor.fs:36`),
  `CrystalParameters.anisotropy` (`LibraryFacets.fs:177`), the three crystal records
  (`CrystalPhysics.fs`), and the test project's `CrystalMaterialFixtures` (`MaterialProxyTests.fs`).
- Grep for collisions (ELEV014/016/019): none of `PointGroup`, `CrystalSymmetry`, `ComponentCount`,
  `Centrosymmetry`, `Gyrotropy`, the four class cases, the 32 point-group cases, or a `symmetry` /
  `independentIndexCount` / `orientationAngleCount` member existed in `Berreman/`.
- Wrote `PlannerSymmetry.fs` (items 1–7 of the brief) and appended it last in the Domain compile list.
- Wrote `PlannerSymmetryTests.fs` (the three facts of the step's effects) and appended it last in the
  test compile list.
- Build green; the new class 3/3; the whole constructor suite 1840/1840 (baseline 1837, +3).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/PlannerSymmetry.fs` (new): `module PlannerSymmetry`
  — `ComponentCount`, `Anisotropy.independentIndexCount`, `Centrosymmetry`, `Gyrotropy`, `PointGroup`
  (32 cases; `symbol`, `anisotropy`, `centrosymmetry`, `gyrotropy`, `orientationAngleCount`, `all`, each a
  complete match without a wildcard), `CrystalSymmetry` (`independentIndexCount`,
  `orientationAngleCount`), `CrystalParameters.symmetry`.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`: one
  `<Compile Include="PlannerSymmetry.fs" />` line, last, no comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerSymmetryTests.fs` (new): `module PlannerSymmetryTests`
  in `namespace OpticalConstructor.Tests` with the three facts.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: one
  `<Compile Include="PlannerSymmetryTests.fs" />` line, last, no comment.

## Testing state

- Build (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`): exit 0, 0 errors, 84 s.
  Warnings: NU1902 (exempt advisory) and SYSLIB0051 ×4 in the vendored
  `MathNetNumerics/Numerics/Exceptions.cs` — pre-existing (present in earlier steps' captures), outside
  this step's touches, none from our F#. Log: `.artifacts/0465-build.log`.
- `OpticalConstructor.Tests.PlannerSymmetryTests` (`--filter-class`): 3/3 passed.
- Whole constructor suite (the gate's selection is `all_discovered`, `whole_suite: light`): 1840 passed,
  0 failed; checkpoint 463 recorded 1837, so the count rises by exactly 3. Duration 116 s (run beside the
  elevation analyzer), inside the 180 s deadline.
- Section invariants: none of the 21 patterns occurs in the two new files; the edits to the project files
  add only a `Compile` line each.
- Elevation (`Elevation/Run-ElevationGate.ps1 -ExcludeRules ELEV004`, from the repo root, no base so all
  13 projects): `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 224s`; Domain findings=0. No
  boundary or carrier mark was added. Inside the 600 s deadline. Log: `.artifacts/0465-elevation.log`.

## Artifacts

- `specs/0050/.artifacts/0465-build.log`
- `specs/0050/.artifacts/0465-elevation.log`

## Gotchas

- The brief asks for "test-local records written as literals". The test project already holds exactly
  those literal records — `CrystalMaterialFixtures` (`MaterialProxyTests.fs`): the uniaxial probe
  n_o 1.544, n_e 1.553, g11 3.0e-5, g33 -1.0e-5; a triclinic crystal with distinct indices; the absorbing
  one with fifteen distinct values — and `StudyMaterialTests` already reuses it. Per AGENTS.md "Reuse,
  never copy", the facts open that fixture module rather than re-transcribing the same literals; they are
  still test-local (in the test project) literal records.
- The brief lists `MuellerInverse` and `Studies` among the modules the file "reads" (to justify its place at
  the end of the compile list). The table needs only `MaterialComplexityEditor` (`Anisotropy`),
  `CrystalPhysics` (`CrystalParameters`) and `LibraryFacets` (`CrystalParameters.anisotropy`), so only those
  are opened; the file still sits last, after all five.
- `CrystalSymmetry.orientationAngleCount` folds `max` over `ComponentCount` itself (a single-case union
  compares structurally) from `ComponentCount 0`, so no `.value` unwrap and re-wrap (ELEV005) is needed.
