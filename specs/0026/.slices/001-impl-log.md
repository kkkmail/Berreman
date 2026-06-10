# 001 — impl-log: Element geometry, placement & rotation domain model

## Progress

- [x] Placement.fs — Vector3, BoundingBox + normals, CatalogueKind, Emission, ElementPlacement, geometry, toConstructorElement
- [x] Project.fs — add `placements` to `OpticalConstructorProject`
- [x] schema — placement `$defs` + root `placements` array
- [x] Update existing `OpticalConstructorProject` constructions (+`placements = []`, 8 sites)
- [x] fsproj wiring (Placement.fs, PlacementTests.fs)
- [x] PlacementTests.fs — AC-A1..A7 (7 tests)
- [x] gates green (build, unit-tests, constructor-unit-tests)

## Files modified

New:
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Placement.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlacementTests.fs`

Edited:
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Project.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/optical-constructor-project.schema.json`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/Templates.fs` (+`placements = []`)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SystemView3DTests.fs` (+`placements = []`)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ProjectJsonRoundtripTests.fs` (+`placements = []`)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/RoundTripTests.fs` (+`placements = []`)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/HistoryTests.fs` (+`placements = []`)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExportImportTests.fs` (+`placements = []`)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StackEditTests.fs` (+`placements = []`)

## Decisions / issues encountered

- **`Vector3` is a local pure `{x;y;z}` record, not the engine `RealVector3`.**
  The placement (incl. the box's two normals) MUST JSON-round-trip and pass
  schema validation (AC-A6). The engine `RealVector3` (`Geometry.fs:72`) wraps a
  Math.NET vector that System.Text.Json cannot round-trip without a custom
  converter; a plain 3-float record serializes with no converter and keeps the
  geometry provable headless (constraint 0.3). Minimal-invention choice — angles
  still reuse the engine `Angle` (R-9). (Spec ambiguity resolved per
  arc-runner.user-md "Don't ask the user".)
- **`toConstructorElement Sample` carries a placeholder vacuum `OpticalSystem`.**
  `ConstructorElement.Sample` is parametric (`Sample of OpticalSystem`,
  `BeamTree.fs:38`) but the A.5.2 mapping is `CatalogueKind -> ConstructorElement`
  with no system input. A freshly-mapped Sample therefore gets a vacuum stack;
  the concrete material is bound later via `valueId` (None now, R-6 / Part F).
  Keeps the mapping total and pure.
- **Rest-pose frame convention.** `centralRayDirection = +X`, `tableNormal = +Z`,
  third axis `N1 x N2 = -Y`. `orientedBasis` applies R3 (about N3, Rodrigues),
  then R2 (about the table normal), then R1 (about the oriented face normal). R1
  leaves N1 unchanged (it is the spin axis). This makes rest pose return
  `(centralRay, tableNormal)` (AC-A1) and R3 != 0 tip N1 out of plane so
  `r1Axis . r2Axis != 0` (AC-A4).
- **`tableFrameOrientation` takes the incident-ray ANGLE.** A.4.3 phrases the
  input as "the incident-ray direction"; AC-A3 phrases it as "an incident ray at
  angle r2". Implemented as `Angle -> Angle -> Angle` returning `incident + r2`
  (the required additive law), the representation that the headless AC checks.
- **`displayUnit` defaults to `Millimeter`** — a readable bench-scale length for
  the 0.01–0.08 m box dimensions; display metadata only (mirrors
  `BeamNode.defaultUnit`), never affects the stored SI value.
- **`placements` added as a new mandatory field on `OpticalConstructorProject`.**
  This broke the 8 literal construction sites (found via a `beamTree =` grep);
  each got `placements = []`. `{ p with ... }` copies (e.g.
  `ConstructionPage.fs:150`) are unaffected. The schema lists `placements` as an
  OPTIONAL root property (not in `required`), matching the existing
  `sources`/`materials` pattern. The TABLE field stays for slice 004.

## Testing state

All gates pass locally (logs under `C:\GitHub\Berreman\specs\0026\.artifacts\`):

- `build` — exit 0, "Build succeeded.", 0 Error(s), 0 lowercase `error` matches
  (the gate's `stdout_match` veto). Log `001-build.log`.
- `unit-tests` — exit 0, Passed 84 / Skipped 5 / Total 89; `BerremanTests`
  untouched. First-slice baseline `berreman_unit_tests = 0`, so the count holds;
  new baseline recorded = 84. Log `001-unit-tests.log`.
- `constructor-unit-tests` — exit 0, Passed 212 / Failed 0 / Total 212 (includes
  the 7 new `PlacementTests`). Log `001-constructor-unit-tests.log`.
- `impl-log-structure`, `state-of-world-structure` — built-in structural checks;
  this impl-log and the sibling state-of-the-world carry all required ATX
  headings.

Coverage: AC-A1 (rest-pose normals), AC-A2 (default locks), A.4.5 (lock
respected/ignored), AC-A3 (`r2 + R2` additivity), AC-A4 (R3 breaks R1⟂R2),
AC-A5 (emission invariant), AC-A6 (placement round-trip + schema validation),
AC-A7 (catalogue-kind mapping; never `Analyzer`). None deferred.

## Artifacts

- `001-build.log`, `001-unit-tests.log`, `001-constructor-unit-tests.log`
