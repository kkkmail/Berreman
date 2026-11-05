# Step 0010 — IMPLEMENT — impl plan

## Approach

Langasite's dispersive gyration becomes data. The three kernels (`g11La3Ga5SiO14`, `g33La3Ga5SiO14`,
`rhoLa3Ga5SiO14`) and the helper they share (`realIndexSum`, step 8) MOVE from the `Langasite` class in
`OpticalProperties/Dispersive.fs` into the core, as members of a new one-case union
`PublishedGyrationLaw = LangasiteGyrationLaw` in `Berreman/Dispersion.fs` (after `GyrotropicValue`,
before `RhoWithDispValue`). `RhoWithDispValue` gains `PublishedLawRho of PublishedGyrationLaw`.
The engine preset reads `LangasiteGyrationLaw.rhoAt`, so one implementation exists and every engine
number is unchanged (same expressions, same evaluation order).

Before moving anything, the start tree's `rhoLa3Ga5SiO14` is sampled at 400 / 500 / 600 / 800 /
1000 nm as G17 literals (by reflection on the private let, cross-checked against the public
`langasiteOpticalProperties.rhoWithDisp`), so the new fact pins the moved law to the old numbers.

Every match follows the new case:

- `Active.fs` `toRhoWithDisp`: `PublishedLawRho law -> RhoWithDisp law.rhoAt`.
- `MaterialComplexityEditError` gains `PublishedGyrationNotEditableErr of reason : string`;
  `ofComplexity`'s active seeding returns a `Result` and answers the error naming the law; the
  `caption` arm is added in `ConstructorUiIdOfDomain.fs`.
- `MaterialComplexity.isEditable` answers false when the active aspect is a published law.
- `LibraryFacets.fs` gyration-class and handedness facets still apply but extract `[]`.
- `NkDispersionChart.gyrationComponentsOfComplexity` answers `allGyrationComponents`.
- Docs: `langasiteComplexity` (keeps its constant uniaxial gyration, names the published law as the
  engine preset's ρ) and the `PropagationTests.fs` comment.

## Files

- `Berreman/Berreman/Dispersion.fs`, `Berreman/OpticalProperties/Active.fs`,
  `Berreman/OpticalProperties/Dispersive.fs`
- `OpticalConstructor.Domain/MaterialComplexityEditor.fs`, `LibraryFacets.fs`, `MaterialLibrary.fs`
- `OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`, `NkDispersionChart.fs`
- Tests: `BerremanTests/RhoWithDispValueTests.fs` (2 facts), `MaterialComplexityTests.fs`,
  `LibraryFacetsTests.fs`, `PropagationTests.fs` (comment only), `MaterialEditorWindowTests.fs`,
  `NkDispersionChartTests.fs` (1 fact each)

## Risks

- `realIndexSum` is used only by g11 / g33, so it moves with them (otherwise the core law would
  need to reach back into OpticalProperties, which it cannot).
- `ofComplexity`'s active seeding sits inside a `Result.map`; it becomes a `Result.bind` so the
  new error can surface. Its doc ("seeding never fails") must change.
- ELEV: the new union is fieldless with an `int` tag (ELEV017 clean); the case name
  `PublishedLawRho` / `LangasiteGyrationLaw` / `PublishedGyrationNotEditableErr` must be unique
  across our DU cases (ELEV016) — checked by grep before the build.
