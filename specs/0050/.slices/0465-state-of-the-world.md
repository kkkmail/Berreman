# Step 0465 — state of the world

## Where we are

Spec 0050, section S14.1 (the Measurement Planner: kit inputs and the structural layer). Step 0465
lands the Planner's layer-1 structural table as pure Domain data: the 32 crystallographic point groups,
what each allows (anisotropy, centrosymmetry, gyrotropy, free orientation angles), and the symmetry each
crystal-parameter record states. Later S14.1 steps read it (the Planner's prior reads a kit's crystal
through its record at step 471).

## What's working

- Add `PlannerSymmetry` to the Domain: the 32 point groups with symbol, anisotropy, centrosymmetry, gyrotropy and orientation-angle count
- Add `ComponentCount` and `Anisotropy.independentIndexCount` (1 / 2 / 3 independent principal indices)
- Add `CrystalSymmetry` and `CrystalParameters.symmetry`: uniaxial record → classes 3/32/4/422/6/622, triclinic records → class 1
- Add `PlannerSymmetryTests` (3 facts): 11 centrosymmetric / 21 non-centrosymmetric / 18 gyrotropic, index and angle counts, the records' symmetry

## Tests

- build: green locally (exit 0, 0 errors, 84 s; only the exempt NU1902 and the pre-existing vendored
  MathNet SYSLIB0051 warnings).
- constructor-unit-tests: 1840 passed, 0 failed (checkpoint 463: 1837; +3, the three new facts).
  116 s, run beside the elevation analyzer, inside the 180 s deadline.
- elevation: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 224 s (all 13 projects, no base),
  inside the 600 s deadline; no boundary or carrier mark added.
- The 21 s45 invariants: none of their patterns occurs in the two new files; the project files gain only
  one `Compile` line each.

```yaml
gates:
  constructor_unit_tests: 1840
```

## Architecture

- `PlannerSymmetry` is a module in `namespace OpticalConstructor.Domain`, compiled last in the Domain
  project. It REUSES the editor's `Anisotropy` as the optical class and `CrystalParameters.anisotropy`
  (LibraryFacets) as a record's class; no second class type is declared.
- `Centrosymmetry` and `Gyrotropy` are derived, never stored: no wire tag, store or conversion exists for
  them or for `PointGroup`.
- Every `PointGroup` member is a complete match with no wildcard, so a new case cannot compile without its
  row in every table.

## Deferred

Nothing. Every item of the step's brief landed.

## Gotchas

- The facts reuse the test project's literal `CrystalMaterialFixtures` (the uniaxial probe n_o 1.544,
  n_e 1.553, g11 3.0e-5, g33 -1.0e-5; distinct triclinic indices; the fifteen-value absorbing record)
  instead of re-transcribing them (AGENTS.md: reuse, never copy).
- `CrystalSymmetry.orientationAngleCount` takes `max` over `ComponentCount` directly (structural
  comparison), avoiding a `.value` round trip.

## Changelog

- 2026-11-17 — step 0465: point-group table (`PlannerSymmetry.fs`) and `PlannerSymmetryTests`.
