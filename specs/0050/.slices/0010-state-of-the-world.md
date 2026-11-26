# Step 0010 — IMPLEMENT — state of the world

## Where we are

This is step 10 of spec 0050, in section S1.2 ("A material's physics is its complexity"). After
step 9, every eps axis is data, but Langasite's dispersive gyration was still a closure in
`OpticalProperties/Dispersive.fs`, which `RhoWithDispValue` could not state. This step moves those
kernels into the core as the published law `PublishedGyrationLaw.LangasiteGyrationLaw`, and
`RhoWithDispValue` gains `PublishedLawRho` (obligation `S12-GYRATION-LAW`). Step 13 derives
`properties` from the complexity, and it can now state the engine Langasite preset as a
`MaterialComplexity`.

## What's working

- Moved Langasite's g₁₁ / g₃₃ / ρ kernels into the core as `PublishedGyrationLaw.rhoAt`, with wire tag 1 and `tryOfWire`
- Added `RhoWithDispValue.PublishedLawRho`; `toRhoWithDisp` and the engine `Langasite` preset now read the law, and every engine number is unchanged
- A published gyration law opens view-only: `isEditable` is false, and `ofComplexity` answers `PublishedGyrationNotEditableErr`, which has a caption
- The activity facets still apply to a published law but extract no value, and the gyration chart draws every component for it
- Added six facts, including one that pins the law to the start tree's G17 literals at 400–1000 nm

## Tests

These runs are local observations, made to confirm the change. The arc-runner's gate engine runs
and records the gates.

- `build`: 0 errors. The only warnings were the two vendored MathNet SYSLIB0051 ones that were
  already on the start tree, and the final rebuild showed 0 warnings.
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 11 project(s)`.
- `unit-tests` (RhoWithDispValueTests and EpsWithDispValueTests): 38/38 succeeded, including the two new
  facts.
- `constructor-unit-tests` (whole suite): 736/736 succeeded, which is 734 plus the two new facts.
- `ui-tests` (MaterialEditorWindowTests, NkDispersionChartTests and FsxAcceptanceTests): 57/57 succeeded,
  including the two new facts and both LangasiteOnSilicon facts.
- `ui-smoke`: 208/208 succeeded.
- The four s02 invariants (`elev-list-empty`, `no-vstest`, `no-kit-kit-json` and
  `no-eps-axis-evaluated`) each find nothing.

```yaml
gates:
  berreman_unit_tests:     38
  constructor_unit_tests: 736
  ui_smoke_tests:         208
  ui_tests:                57
```

`berreman_unit_tests` and `ui_tests` are the counts under this step's class filters.

## Architecture

- **`PublishedGyrationLaw` is the gyration analogue of `PublishedIndexLaw`.**
  - It is a fieldless closed set with an `int` wire tag (`LangasiteGyrationLaw` = 1).
  - `rhoAt` holds the closed form once, in the core. It reads the two Langasite index laws step 8
    created.
- **`RhoWithDispValue` has three cases:**
  - `RhoWithDispValue`: per-component `DispersionFormula`
  - `RhoWithoutDispValue`: constant `RhoValue`
  - `PublishedLawRho`: a published law. It carries no class or handedness, because the law fixes both.
- **Editability covers the active aspect.** `MaterialComplexity.isEditable` = eps editable AND the
  active aspect is not a published law. `ofComplexity` refuses a published law with a typed error
  rather than inventing a class.

## Deferred

- Nothing in this step's scope is deferred.

## Gotchas

- **The library Langasite keeps its constant uniaxial ρ, so no library number moves.** Only the
  engine preset reads the law.
- **The UI never reaches `ofComplexity` for a published law.** The editor and the Materials window
  both gate on `isEditable` first, so the entry opens view-only with the generic caption.
- **The slice's `Dispersive.fs` line numbers predate step 8.** The kernels were at `:23-49`.
- **One stale comment outside this step's effects was left alone.** `BerremanTests/LangasiteInverseTests.fs:104,109`
  still cites the old let names. `g33La3Ga5SiO14` survives as a local inside `rhoAt`.

## Changelog

- 2026-11-05 — step 0010:
  - Langasite's dispersive gyration became the core published law `LangasiteGyrationLaw`
    (`RhoWithDispValue.PublishedLawRho`), and the engine preset reads it.
  - The Domain and Ui matches follow the case: view-only, a typed seeding error, empty activity
    facets and every chart component.
  - Six facts added.
