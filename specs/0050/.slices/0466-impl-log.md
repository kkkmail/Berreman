# Step 0466 — impl log

## Progress

- Read the task, both system prompts, the project prompt, rulings R0001–R0005 (all confined to 0427/0428/0436;
  nothing here touches the dispatch, analyzer, identity, residual or solver code they protect, and every one of
  them is preserved) and the slice.
- Read `PlannerSymmetry.fs` / `PlannerSymmetryTests.fs` (step 465), `MuellerInverse.fs` (`SampleCut`,
  `orientForCut`, the three `axes` lists), `CrystalPhysics.fs`, `Studies.fs:parameterAxes`,
  `ElementId.fs:CrystalOrientation`, `Propagation.fs:ResolvedLayer.getLayer`, `Media.fs:Layer.rotate`,
  `MaterialProperties.fs:OpticalProperties.rotate` / `Eps.Item` and `Geometry.fs:Rotation`.
- Implemented the seven declarations in `PlannerSymmetry.fs` after step 465's, in the brief's order.
- Added the three facts to `PlannerSymmetryTests` with exactly the census identities.
- Built, ran the touched class, the whole constructor suite (its gate selects `all_discovered`), and the
  elevation script.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/PlannerSymmetry.fs` — opens for
  `Berreman.MaterialProperties`, `Library`, `ItemModels`, `MuellerInverse`, `Studies`; then
  `markerTolerance` (private), `CrystalAxis` (+ `marker`, `normalOf`), `CrystalAxisPair`, `TensorComponent`
  (+ `axes`), `CrystalComponent`, `CrystalParameters.components`, `SampleCut.normalAxisOpt`,
  `CrystalOrientation.normalAxisOpt`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerSymmetryTests.fs` — opens, module doc widened to
  the new scope, three private helpers (`tensorComponentsOf`, `axisPair`, `eulerOrientation`) and the three facts.

No project file changed (both files were already compiled, step 465).

## Decisions

- **`component` field written `` ``component`` ``.** `component` is reserved by F#: as a bare field name it
  raises FS0046 ("reserved for future use") at the declaration and at every use (measured with `dotnet fsi`,
  `.artifacts/0466-scratch/kw.fsx`); the zero-warnings rule binds. The backtick-escaped name keeps the brief's
  field name with no warning (`kw2.fsx`), following the repo's precedent `EntryFileJson.fs:154`
  `` member this.``sealed`` ``. Later steps that read the field write it the same way.
- **Marker values read back, not re-written.** `normalOf` compares Re ε₃₃ with `CrystalAxis.marker`'s own
  diagonal, so 1, 4 and 9 exist once (the marker's n = 1, 2, 3).
- **Tolerance** is the module's one private value, `[<Literal>] let private markerTolerance : float = 1e-9`.
  The first elevation run flagged the plain `let private markerTolerance : float` as ELEV003P (bare `float` in a
  private signature, `PlannerSymmetry.fs:336`; log `.artifacts/0466-elevation-run1-elev003p.log`). The tree's
  private constants are `[<Literal>]`s (`RayModel.fs:grazingCosine`, `HenkeCompounds.fs:classicalElectronRadius`),
  which the analyzers accept, so the fix is the same mark (no ELEV list or analyzer edit). `normalOf` now also reads
  `CrystalAxis.marker` once.
- **`components`** builds each case's component list from one principal-axes list and one off-diagonal pair list
  (23, 13, 12 — the records' order), then `List.map2`s it with `this.parameterAxes`, so no parameter name is
  written a second time. The uniaxial list is the brief's four literally.
- **Tests go beyond the brief's minimum, within the same three identities:** `TensorComponent.axes` is checked
  (uniaxial n_o, g33; absorbing e11; triclinic off-diagonal pairs); an x-cut at azimuth 40° still answers the
  1-axis; `PrimaryAxes` equals `ZCut`'s answer and the θ = 90° Euler rotation equals `XCut`'s.

## Testing state

- `dotnet build Berreman.slnx -c Release` (from `Berreman/`), first build: exit 0, 85 s, 0 errors; the 8
  warnings are the pre-existing vendored-MathNet `SYSLIB0051` (4) and the `NU1902` package advisory (exempt),
  identical to step 465's build log. After the ELEV003P fix: exit 0, 19 s (incremental, Domain recompiled),
  0 errors, only `NU1902`. None from our code. Log: `.artifacts/0466-build.log` (the second build).
- `--filter-class OpticalConstructor.Tests.PlannerSymmetryTests`: 6/6 passed (3 from 465 + 3 new).
  Log: `.artifacts/0466-planner-symmetry-class.log`.
- Whole constructor suite (the gate's `all_discovered` selection): 1843/1843 passed, 112 s on the first build
  (concurrent with elevation) and again 1843/1843, 108 s alone, after the fix; deadline 180 s. Previous
  step's baseline 1840, so +3 as the acceptance requires. Log: `.artifacts/0466-constructor-unit-tests.log`.
- Elevation (`Elevation/Run-ElevationGate.ps1 -ExcludeRules ELEV004`, from the repo root, no base: all 13
  projects): run 1 failed on ELEV003P `PlannerSymmetry.markerTolerance` (stopped after the Domain report);
  run 2 after the `[<Literal>]` fix: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 238 s (deadline
  600 s). Logs: `.artifacts/0466-elevation-run1-elev003p.log`, `.artifacts/0466-elevation.log`.
- The 21 s45 invariants: none of their patterns occurs in the two edited files; no project file, props,
  targets or ELEV list changed.
- LF only (`git ls-files --eol`: `i/lf w/lf`); `git diff --numstat` equals `--ignore-cr-at-eol`.

## Artifacts

- `.artifacts/0466-build.log`, `.artifacts/0466-planner-symmetry-class.log`,
  `.artifacts/0466-constructor-unit-tests.log`, `.artifacts/0466-elevation.log`,
  `.artifacts/0466-elevation-run1-elev003p.log`
- `.artifacts/0466-scratch/kw.fsx`, `kw2.fsx` — the FS0046 measurement behind the `` ``component`` `` decision.

## Gotchas

- A private numeric constant must be a `[<Literal>]`: a plain `let private x : float` is ELEV003P.
- `component` is F#-reserved (FS0046): see Decisions. Step 467's brief also names a parameter
  `(component : TensorComponent)`; it needs the same escaping or another name.
- `CrystalAxis.marker` is a static property: it is rebuilt on each read (a 3×3 builder, cheap).
- `List.map2` relies on each case's component list having its record's axis count (4, 9, 15); the first fact
  asserts the names line up, so a new axis without its component fails there.
