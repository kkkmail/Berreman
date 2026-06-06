# 001 — State of the world: Element geometry, placement & rotation domain model

## Where we are

Slice 001 is the CRITICAL, land-first slice of Spec 0026 (Optical Constructor UI):
it realises **Part A**, the pure, headless-testable element geometry / placement /
rotation domain. It adds a new `OpticalConstructor.Domain/Placement.fs` module on
top of the existing beam-tree topology (`BeamTree.fs`) and units spine
(`Units.fs`), reserves the schema and `OpticalConstructorProject` slots for
per-element placement, and proves the geometry headless. It introduces NO drawing
(Part C / slice 004) and NO ray routing (Part B / slice 002) — slices 002 and 004
consume the `ElementPlacement` / `BoundingBox` / `CatalogueKind` / `Emission`
shapes defined here.

## What's working

- Add `Placement.fs` with the `ElementPlacement` record (box centre, three `Angle`
  rotations, three per-axis locks, catalogue kind, `valueId` slot, bounding box,
  emission metadata, display unit) — R3 locked by default, R1/R2 unlocked.
- Add the `BoundingBox` (A×A×B extents + two normals N1/N2) with named per-role
  default dimensions, and the rest-pose frame (N1 = central ray, N2 = table normal).
- Add the three-state `Emission` smart constructor that makes "both groups off"
  unrepresentable and re-enables the other group when one is turned off.
- Add the pure geometry: `tableFrameOrientation` (the `r2 + R2` additive law),
  `orientedBasis`/`orientedNormals`/`r1Axis`/`r2Axis` (R3 tips N1 out of plane).
- Add `toConstructorElement`, mapping both polarizers to `Polarizer` and never to
  `Analyzer`, leaving the engine `ConstructorElement` DU untouched.
- Extend `OpticalConstructorProject` and the JSON schema with per-element
  placement; prove a placement round-trips through `saveProject`/`openProject`
  and passes schema validation. Add 7 headless tests (AC-A1..A7).

## Tests

- `build` — PASS (exit 0, "Build succeeded.", 0 Error(s); no lowercase `error`). Log `.artifacts/001-build.log`.
- `unit-tests` — PASS (exit 0, Passed 84 / Skipped 5 / Total 89; `BerremanTests` untouched). First-slice baseline `berreman_unit_tests = 0` held; new baseline 84. Log `.artifacts/001-unit-tests.log`.
- `constructor-unit-tests` — PASS (exit 0, Passed 212 / Total 212; +7 new `PlacementTests`). Log `.artifacts/001-constructor-unit-tests.log`.
- `impl-log-structure`, `state-of-world-structure` — PASS (required ATX headings present).

Coverage: AC-A1 rest-pose normals; AC-A2 default locks; A.4.5 lock respected;
AC-A3 `r2 + R2` additivity; AC-A4 R3 non-orthogonality; AC-A5 emission invariant;
AC-A6 placement round-trip + schema validation; AC-A7 catalogue-kind mapping.
None deferred.

## Architecture

- **Placement is a distinct module from beam-tree topology (R-9).** Placement is
  a separate concern; `BeamNode` is not overloaded with a flag-heavy spatial
  payload. The project aggregate carries `placements : ElementPlacement list`
  alongside the beam tree.
- **`Vector3` is a local pure `{x;y;z}` record, not the engine `RealVector3`.**
  The Math.NET-backed `RealVector3` cannot round-trip through System.Text.Json
  without a custom converter; a plain 3-float record serializes for free and keeps
  the geometry provable headless (constraint 0.3). Angles still reuse the engine
  `Angle` type (R-9), and box/point magnitudes stay canonical SI meters (0.2).
- **Rest-pose frame: N1 = +X (central ray), N2 = +Z (table normal), N3 = N1×N2.**
  `orientedBasis` composes R3 (about N3, Rodrigues), then R2 (about the table
  normal), then R1 (about the oriented face normal). R1 leaves N1 unchanged.
- **Emission is a 3-state DU.** `EmitReflectedOnly | EmitTransmittedOnly |
  EmitBoth` cannot represent both-off; `withReflected`/`withTransmitted` re-enable
  the opposite group when one is turned off (R-7). Mirror default = reflected-only
  (consistent with `BeamNode.attach`, `BeamTree.fs:74`).
- **Schema extension is additive.** New `$defs` (`vector3`, `tablePoint`,
  `boundingBox`, `catalogueKind`, `emission`, `elementPlacement`) and an OPTIONAL
  root `placements` array; the nine reserved anchors and the permissive
  `constructorElement` (`true`) are untouched (A.8.1). FSharp.SystemTextJson
  serializes `Angle` (single-case union) as a bare number, fieldless DUs as bare
  strings, options as value-or-null — the schema matches that shape exactly.

## Deferred

- The TABLE representation and the `OpticalConstructorProject` table field — slice
  004 (Part C). The placement anchors here are left stable for that sibling edit.
- Ray routing / snapping that consumes `orientedNormals` and `emission` — slice
  002 (Part B / RayModel).
- Binding a concrete device/material behind `valueId` — Part F / slice 006
  (`valueId` is `None` for every element this build creates, R-6).
- No drawing code, no caching/scene-graph, no schema migration (0.6).

## Gotchas

- **`ConstructorElement.Sample` is parametric**, so `toConstructorElement Sample`
  carries a placeholder vacuum `OpticalSystem`. Treat the system as a stand-in
  until `valueId` binding (Part F); do not read physics from it.
- **A freshly-mapped Sample's placeholder system is vacuum/empty films.** Solving
  it is meaningful only after Part F binds a real material.
- **Adding a mandatory record field breaks every literal `OpticalConstructorProject`
  construction.** 8 sites were updated with `placements = []`; future top-level
  fields (e.g. slice 004's table) will need the same sweep. `{ p with ... }`
  copies are unaffected.
- **`r3` starts locked (A.1.2).** `withR3` is inert until `setR3Locked false`;
  tests that exercise the R3 geometry copy the record directly to bypass the lock.
- **`tableFrameOrientation` takes the incident-ray ANGLE** (`Angle -> Angle ->
  Angle`), not a direction vector — the additive `r2 + R2` form the AC checks.

## Changelog

- 2026-06-05 (slice 001): Land Part A — the element geometry / placement / rotation
  domain. New `Placement.fs` (`ElementPlacement`, `BoundingBox` + two normals,
  `CatalogueKind`, three-state `Emission` smart constructor, `orientedBasis` /
  `r1Axis` / `r2Axis` / `tableFrameOrientation` geometry, `toConstructorElement`).
  Extend `OpticalConstructorProject` with `placements` and the JSON schema with the
  placement `$defs` + root array. Add `PlacementTests.fs` (7 tests, AC-A1..A7).
  Build green at Release/x64; all gates pass (constructor-unit-tests 212,
  berreman_unit_tests 84 held).

```yaml
gates:
  berreman_unit_tests:    84
  constructor_unit_tests: 212
```
