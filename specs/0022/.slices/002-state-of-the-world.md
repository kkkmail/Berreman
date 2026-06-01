# 002 — State of the world: Domain foundation (units spine, beam-tree types & project aggregate)

## Where we are

Slice 002 fills the core immutable domain model every later part of the 0022 arc
consumes. It lands the units spine (`Units.fs`), the NET-NEW beam-tree topology
with mirror/branch validation, propagate-driven routing and gradient
discretization (`BeamTree.fs`), and the root `OpticalConstructorProject`
aggregate plus the nine schema `$defs` anchor names (`Project.fs`) — all on top
of the slice-001 scaffold projects. No engine type is forked: every quantity
reduces through `Berreman.Constants`/`WaveLength.value`, and every solve/route
goes through the existing `OpticalSystemSolver`, `BaseOpticalSystemSolver`, and
`EmField.propagate` seams. Serialization of the aggregate is deferred to slice 003.

## What's working

- Add the `UnitOfMeasure` boundary DU and the sole unit-conversion seam (`toMeters`/`toWaveLength`/`fromMeters`/`wavelengthToUnit`) reusing the `Constants.fs` factors, with `1239.84` bound once and no new `[<Measure>]` types.
- Add the NET-NEW `BeamBranch`/`ConstructorElement`/`BeamNode`/`BeamTree`/`BeamTreeError` types with a `Result`-returning `attach` enforcing mirror-reflected-only.
- Route beams through the reused engine seams: `solve` via `OpticalSystemSolver`, child incident field via `EmField.propagate`, child solve via `BaseOpticalSystemSolver`.
- Auto-discretize gradient-index layers into exactly `n` equal-thickness reused `Layer` records assignable straight into `OpticalSystem.films`.
- Define the `OpticalConstructorProject` aggregate and document the nine reserved `$defs` anchor names; defer serialization to slice 003.
- Add 17 unit tests (BeamTree/BeamRouting/Units/GradientDiscretize); build green at Release/x64 with all four roster gates passing.

## Tests

- `build` — PASS (exit 0, "Build succeeded.", 0 Error(s); no lowercase `error`). Log `.artifacts/002-build.log`.
- `unit-tests` — PASS (exit 0, Passed 70 / Skipped 5 / Total 75; `BerremanTests` untouched). Baseline `berreman_unit_tests = 70` held. Log `.artifacts/002-unit-tests.log`.
- `constructor-unit-tests` — PASS (exit 0, Passed 18 / Total 18; up from slice-001 baseline of 1). Log `.artifacts/002-constructor-unit-tests.log`.

Coverage: AC-A3/AC-B4 (BeamRoutingTests — engine-solver path, propagate-driven
child field, BaseOpticalSystemSolver child solve, independent branches);
AC-A4/AC-B2/AC-B3 (BeamTreeTests — mirror reject/accept, both branches, linear
chain, AC-B8 default-unit hook); AC-A5/AC-A7/AC-D1/AC-D2 + cm⁻¹ + R-8
(UnitsTests); AC-B11 (GradientDiscretizeTests). None deferred.

## Architecture

- **`IncidentLightInfo` is the node's local beam state.** `BeamNode` carries the engine's own `IncidentLightInfo` (`Fields.fs:338`) as its solver inputs (direction/energy/polarization/ellipticity) rather than a new beam-state type — reuse over invention (§A.3).
- **Children as `Map<BeamBranch, BeamNode>`.** `BeamBranch` is a plain comparable DU so it keys the attachment map; only the key needs comparison, so the engine's non-comparable Math.NET-backed payloads on the node are unaffected.
- **Routing reuses three engine seams unchanged:** `OpticalSystemSolver` (root solve), `EmField.propagate` (inter-element advance), `BaseOpticalSystemSolver(emf, ShortOpticalSystem)` (downstream solve). No parallel solver, no Jones/Stokes accumulator.
- **Aggregate is minimal by design.** `OpticalConstructorProject` carries only `beamTree` + `systems` (types that already exist); later slices extend the same record and the schema. The nine `$defs` anchors are documented as `Project.schemaDefAnchors` for slice 003 to fill.

## Deferred

- JSON serializer, `optical-constructor-project.schema.json`, and validate-on-load — slice 003 (storage core).
- Curved-element local-AOI physics — only the §A.5 contract is fixed here; implementation is slice 006 (Part C).
- Dispersion models / material library / sources / chart settings — extend the aggregate in slices 004/007/012.
- The Part J §J.9 validation seam for `subLayerCount` — guarded inline here with `invalidArg`; the shared seam lands later.

## Gotchas

- **`EmField.propagate` is an OPTIONAL type extension in `module BerremanMatrix`** (`type EmField with`, BerremanMatrix.fs:191). Any caller MUST `open Berreman.BerremanMatrix` or the member is invisible — done in `BeamTree.fs` and `BeamRoutingTests.fs`.
- **`Berreman.Fields.RT` also has `Reflected`/`Transmitted` cases** (Fields.fs:61-63), colliding with `BeamBranch`. In-module use resolves to the locally-defined `BeamBranch`; tests qualify as `BeamBranch.Reflected`/`.Transmitted` to stay unambiguous.
- **Math.NET-backed engine records have no reliable structural equality.** Routing/solve tests assert on a scalar projection (`emf.e.value.norm`) and never compare whole `EmField`/`OpticalSystem` records with `=`.
- **`--warnaserror+:25`** (incomplete matches = error): every DU match in the new code is exhaustive; verified by the green build.
- **`BeamNode.system` vs `ConstructorElement.Sample of OpticalSystem`.** The spec defines both ("BeamNode carries its OpticalSystem" §A.4/R-10, and `Sample (carrying an OpticalSystem)`). For a `Sample` node the two coincide; `BeamNode.system` is the authoritative stack the solver consumes, and the `Sample` payload preserves the literal DU shape the spec mandates.

## Changelog

- 2026-05-31 (slice 002): Land the units spine (`Units.fs`), the NET-NEW beam-tree topology with mirror/branch validation + propagate-driven routing + gradient discretization (`BeamTree.fs`), and the `OpticalConstructorProject` aggregate + nine `$defs` anchors (`Project.fs`). Reuse the engine `OpticalSystemSolver`/`BaseOpticalSystemSolver`/`EmField.propagate`/`WaveLength`/`Constants` seams unchanged. Add 17 unit tests; build green at Release/x64; all four gates pass (constructor-unit-tests 18, berreman_unit_tests 70 held).

```yaml
gates:
  berreman_unit_tests:    70
  constructor_unit_tests: 18
```
