# 002 — impl-plan: Domain foundation (units spine, beam-tree types, project aggregate)

## Approach

This slice fills three of the Domain placeholder modules created by slice 001 and
adds one new aggregate module, proving the contractful invariants (mirror
reflected-only, SI storage, propagate-driven routing, gradient discretization,
unit round-trips) by unit test. No engine type is forked — every physical
quantity reduces through `Berreman.Constants`/`Berreman.Fields.WaveLength` and
every solve goes through the existing `OpticalSystemSolver` /
`BaseOpticalSystemSolver` / `EmField.propagate` seams.

### Units.fs (R-5..R-9, AC-A5/A7/D1/D2)

`module OpticalConstructor.Domain.Units`:
- `UnitOfMeasure = Meter | Millimeter | Micrometer | Nanometer | Angstrom | ElectronVolt | Wavenumber` — the sole place a non-SI unit name appears; no new `[<Measure>]` types (reuse `meter`/`mm`/`mkm`/`nm` from `Constants.fs`).
- `evNmProduct = 1239.84` — the literal appears exactly once, doc-commented `E[eV] = 1239.84 / λ[nm]`.
- `toMeters : UnitOfMeasure -> float -> float<meter>` — multiplies by `mmToMeter`/`mkmToMeter`/`nmToMeter` (Constants.fs:27,30,33); `1.0e-10<meter>` per Å; eV→nm→m and cm⁻¹→nm→m via `1.0e7`.
- `toWaveLength : UnitOfMeasure -> float -> WaveLength` — returns engine `Nm`/`Mkm` (Fields.fs:294,295); `Nanometer`→`Nm`, `Micrometer`→`Mkm`, all others reduce to nm and return `Nm` (R-8: no Å/eV/cm⁻¹ case added to `WaveLength`).
- `fromMeters : UnitOfMeasure -> float<meter> -> float` and `wavelengthToUnit : UnitOfMeasure -> WaveLength -> float` — inverses; `wavelengthToUnit` reads the canonical value via `WaveLength.value` (Fields.fs:284).

### BeamTree.fs (R-2/R-3/R-10..R-14, AC-A3/A4/B2/B3/B4/B8/B11)

`module OpticalConstructor.Domain.BeamTree` (opens `Berreman.*` engine modules incl. `BerremanMatrix` for the `propagate` seam):
- `BeamBranch = Reflected | Transmitted` (comparable → usable as a `Map` key).
- `ConstructorElement = Source | Polarizer | Sample of OpticalSystem | Lens | CurvedMirror | FlatMirror | Analyzer | Detector`; mirror cases carry the reflected branch only.
- `BeamTreeError = MirrorBranchMustBeReflected` (NET-NEW, traced to §B.3/R-11).
- `BeamNode` record: `element`, `system : OpticalSystem` (the stack solved here), `incident : IncidentLightInfo` (the engine local-beam-state, Fields.fs:338), `children : Map<BeamBranch, BeamNode>`, `defaultUnit : UnitOfMeasure` (B.8 hook — display only).
- `BeamNode.isMirror`, smart constructor `BeamNode.attach branch child parent : Result<BeamNode, BeamTreeError>` rejecting `Transmitted` on a mirror, allowing `Reflected`; both branches on a non-mirror.
- Orchestration seams: `solve` (via `OpticalSystemSolver`), `branchEmField`, `childIncidentField` (parent branch field advanced by `EmField.propagate`), `routeAndSolve` (via `BaseOpticalSystemSolver(emf, ShortOpticalSystem)`).
- `GradientLayer = { totalThickness; subLayerCount; indexProfile }` + pure `discretize : GradientLayer -> Layer list` slicing into `subLayerCount` equal-thickness reused `Layer`s sampled at mid-depth; `>= 1` guarded.

### Project.fs (R-4, §A.7 aggregate + nine `$defs` anchors)

`module OpticalConstructor.Domain.Project`:
- `BeamTree` aggregate type wrapping a root `BeamNode`.
- `OpticalConstructorProject = { beamTree : BeamTree; systems : OpticalSystem list }` — only fields whose types already exist; later slices extend.
- `schemaDefAnchors` — the nine reserved `$defs` names documented (`opticalSystem`, `layer`, `opticalProperties`, `beamNode`, `beamBranch`, `constructorElement`, `materialEntry`, `sourceSpec`, `unitOfMeasure`). Serialization itself is slice 003.

## Files to modify

- New: `Units.fs` (fill), `BeamTree.fs` (fill), `Project.fs` (new).
- New tests: `BeamTreeTests.fs`, `BeamRoutingTests.fs`, `UnitsTests.fs`, `GradientDiscretizeTests.fs`.
- Edit: `OpticalConstructor.Domain.fsproj` (insert `Project.fs` after `BeamTree.fs`), `OpticalConstructor.Tests.fsproj` (register the four test files).

## Risks

- `EmField.propagate` is an optional type extension in `module BerremanMatrix` — must `open Berreman.BerremanMatrix` wherever it is called.
- Math.NET-backed engine records don't give reliable structural equality — routing/solve tests assert on scalar projections (`emf.e.value.norm`), never whole-record `=`.
- `--warnaserror+:25`: every DU match must be exhaustive.
- `1239.84`-once and no-shadow-`[<Measure>]` rules are easy to violate — checked explicitly.
