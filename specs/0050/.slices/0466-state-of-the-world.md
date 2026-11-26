# Step 0466 — state of the world

## Where we are

Spec 0050 section S14.1 (the Measurement Planner: kit inputs and the structural layer). Step 465 gave
`PlannerSymmetry` the point-group table and each crystal record's symmetry; this step adds layer 1's
"what each cut sees" vocabulary: which tensor component each crystal-record parameter is, and which principal
axis a plate puts on its normal, for a `SampleCut` (through `orientForCut`) and for a layer's
`CrystalOrientation` (through the rotation the system builder applies). Step 467 builds the transverse-sight
rule (`PlannerLook`, `sightOf`) on top of these.

## What's working

- Add `CrystalAxis` with a diag(1, 4, 9) probe `marker` and `normalOf`, which names the principal axis a
  rotation carries onto the plate normal
- Add `CrystalAxisPair`, `TensorComponent` (principal index, gyration and absorption components, with `axes`)
  and `CrystalComponent`
- Add `CrystalParameters.components`, which pairs every record parameter, in its axes' order, with its tensor
  component and reuses the `parameterAxes` names
- Add `SampleCut.normalAxisOpt` (via `orientForCut`) and `CrystalOrientation.normalAxisOpt` (via
  `OpticalProperties.rotate`), so the Planner restates no rotation
- Add three `PlannerSymmetryTests` facts covering the components, the cut normals and the layer orientations

## Tests

Development runs (the gate engine's own run is authoritative):

- build: exit 0 (85 s full, 19 s incremental after the elevation fix); 0 errors; the only warnings are the
  pre-existing vendored-MathNet `SYSLIB0051` and the exempt `NU1902` advisory, the same as step 465.
- constructor-unit-tests (`all_discovered`, whole suite): 1843 passed, 0 failed, 108 s against the 180 s
  deadline. That is 1840 + 3, the rise the acceptance requires; `PlannerSymmetryTests` alone 6/6.
- elevation (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`, all 13 projects): `ELEV-GATE: OK - 0 new finding(s)`,
  238 s against the 600 s deadline. The first run found ELEV003P on a plain `let private markerTolerance : float`;
  it is now a `[<Literal>]`, as the tree's other private constants are.
- the 21 s45 invariants: none of their patterns occurs in the change; no project file, props, targets or ELEV
  list was touched.

```yaml
gates:
  constructor_unit_tests: 1843
```

## Architecture

- All new declarations sit in `PlannerSymmetry.fs` after step 465's, in the brief's order. The cut and layer
  geometry reuse `MuellerInverse.orientForCut` and `CrystalOrientation.toRotation` →
  `OpticalProperties.rotate` (`Layer.rotate`, as `ResolvedLayer.getLayer` applies it). No rotation is
  re-derived.
- An axis is recognised by value. The marker is a `TriclinicParameters` probe (n = 1, 2, 3, every g = 0) built
  through its own `opticalProperties`. `normalOf` reads Re ε₃₃ against the marker's own diagonal, with
  |ε₁₃| and |ε₂₃| below the module's one private tolerance (1e-9).
- `components` uses `List.map2` over `parameterAxes` and the case's component list, so parameter names are
  never written twice.

## Deferred

Nothing. Every declaration and fact the brief lists landed in this step.

## Gotchas

- `component` is reserved in F# (warning FS0046). The `CrystalComponent` field is therefore declared and read as
  `` ``component`` ``, following `EntryFileJson.fs`'s `` ``sealed`` ``. Step 467's `(component : TensorComponent)`
  parameter needs the same treatment or another name.
- A private numeric constant must be a `[<Literal>]`: a plain `let private x : float` is ELEV003P.
- `CrystalAxis.marker` is a static property that is rebuilt on every read. It is cheap, but avoid reading it in a
  hot loop.
- A new crystal-record axis without a matching component makes `List.map2` throw. The first new fact catches
  that.

## Changelog

- 2026-11-17 — step 0466: tensor components of the crystal records and the principal axis on a plate's normal,
  for cuts and layer orientations (spec 0050 S14.1); 3 facts added (constructor suite 1840 → 1843).
