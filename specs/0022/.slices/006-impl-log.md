# 006 — Impl log: Lenses & non-flat (curved) mirror elements (Part C)

## Progress

- [x] Wrote `CurvedElements.fs` (CurvedSurface, SurfaceFigure, LocalZone, sag, sampleZones, solveZone, attachCurvedElement, gradeCoating). Replaced the slice-001 placeholder module.
- [x] Added `CurvedElementsTests.fs` (AC-C1, C2, C3, C5, C6, C7, C8) and registered it in the test fsproj.
- [x] Extended `BeamTreeTests.fs` for AC-C4 (curved fan: CurvedMirror Reflected-only, Lens both branches).
- [x] Ran all gates: build, unit-tests, constructor-unit-tests — all green.

## Files modified

- **New** `OpticalConstructor/OpticalConstructor.Domain/CurvedElements.fs` — the curved-element domain. Already registered in `OpticalConstructor.Domain.fsproj` (compile order after `BeamTree.fs`), so no Domain fsproj edit was needed.
- **New** `OpticalConstructor/OpticalConstructor.Tests/CurvedElementsTests.fs` — AC-C1/C2/C3/C5/C6/C7/C8.
- **Edit** `OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` — registered `CurvedElementsTests.fs` (after `BeamTreeTests.fs`).
- **Edit** `OpticalConstructor/OpticalConstructor.Tests/BeamTreeTests.fs` — added the AC-C4 curved-fan test and opened `OpticalConstructor.Domain.CurvedElements`.

## Decisions made

- **Three engine seams reused unchanged.** `solveZone` calls `OpticalSystemSolver(localInfo, zone.coating, parameters)` (`Solvers.fs:199`) with `parameters` passed through untouched; the local AOI is formed by `IncidentLightInfo.rotateY` (`Fields.fs:411`); inter-element propagation is the existing `EmField.propagate` seam (`BerremanMatrix.fs:224`, already wrapped by `BeamTree.childIncidentField`). No second solver, propagator, or ABCD module added.
- **Exact conic sag.** `sag` uses `z = c r² / (1 + sqrt(1 − (1+k) c² r²))`, `c = 1/R`, which is the exact spherical sag at `k = 0` and carries the sign of the curvature directly — so the §C.4 sign convention (concave = negative R → negative sag; convex = positive R → positive sag) falls out of the formula with NO override flag.
- **`gradeCoating` factor.** Computed from `localAoiShift`, normalized by the edge shift at `semiAperture`, so the axis zone is exactly 1.0 (unchanged) and the edge zone hits the ~5% default (`depthGradingFraction = 0.05`). Applied only to `Thickness.Thickness d` films; `Thickness.Infinity` passes through untouched; all results stay `double<meter>`.
- **Coating sharing.** `sampleZones` assigns `surface.coating` to every zone by value, so all zones share one immutable `OpticalSystem` instance (AC-C8 verifies `ReferenceEquals`). Grading is opt-in and per-zone: a non-graded element simply never calls `gradeCoating`.

## Testing state

All gates passed in the local run (the supervisor re-runs them):

- **build** — `dotnet build Berreman.slnx -c Release -nologo -v:m` → `Build succeeded. 0 Error(s)`. No lowercase `error` token in the log. Artifact `.artifacts/006-build.log`.
- **unit-tests** — `dotnet test --no-build -c Release` in `Berreman/BerremanTests/` → Passed 70 / Skipped 5 / Total 75. Baseline `berreman_unit_tests = 70` held (this slice does not touch `BerremanTests`). Artifact `.artifacts/006-unit-tests.log`.
- **constructor-unit-tests** — `dotnet test OpticalConstructor.Tests.fsproj -c Release` → Passed 77 / Failed 0 / Total 77, up from the slice-005 baseline 66 (+11: 10 in `CurvedElementsTests.fs`, 1 AC-C4 in `BeamTreeTests.fs`). Artifact `.artifacts/006-constructor-unit-tests.log`.

Mid-round fix: the first constructor-test run had 3 failures (AC-C3 ×2, AC-C7) — the shared test coating included a `Thickness.Infinity` film, which the engine solver cannot propagate (`BerremanMatrix.fs:125` "TODO: Implement infinite thickness"). Split the fixtures: a finite-only `coating` for the solver-driven tests and a separate `gradedCoating` (with the `Infinity` guard film) for the AC-C6 grading test, since `gradeCoating` is a pure thickness map and is never solved. Re-ran → all green.

## Gotchas

- **`attachCurvedElement` signature vs `Map`-keyed children.** `BeamNode.children` is `Map<BeamBranch, BeamNode>` (≤ two entries, one per branch), owned by slice 002 and not redefinable here. A single parent therefore cannot hold an N-zone fan. So `attachCurvedElement element node` shapes ONE zone node's two branches (Lens → both; CurvedMirror → Reflected only, transmitted dropped via the slice-002 `BeamNode.attach` mirror rule), and the aperture fan ("one node per LocalZone") is realized by the orchestration caller mapping it across the per-zone nodes from `sampleZones`. This is the only reading consistent with the imported types.
- **Aspheric polynomial units.** The spec types `evenPolynomial` as `double<meter> list`, which is dimensionally inconsistent for raw even-asphere coefficients. To keep `sag : … -> double<meter>` well-typed, the polynomial is evaluated in the dimensionless ratio `(r / R)` (each meter-valued coefficient × a dimensionless power → a meter-valued term). The committed [Core] tests exercise the spherical path; the asphere path is type-correct and reduces to the conic base when the polynomial is empty.
- **`Reflected`/`Transmitted` name clash.** Both `Berreman.Fields.RT` and `BeamTree.BeamBranch` define `Reflected`/`Transmitted`. `CurvedElements.fs` opens both `Berreman.Fields` and `BeamTree`, so all branch references are written `BeamBranch.Reflected` / `BeamBranch.Transmitted` to disambiguate.
- **`EmField.propagate` is a `BerremanMatrix` augmentation.** Tests touching propagation must `open Berreman.BerremanMatrix` (as `BeamRoutingTests` does) or the `.propagate` member is invisible.
