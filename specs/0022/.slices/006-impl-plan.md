# 006 — Impl plan: Lenses & non-flat (curved) mirror elements (Part C)

## Approach

All curved-element domain code lands in the single new file
`OpticalConstructor/OpticalConstructor.Domain/CurvedElements.fs`
(already registered in `OpticalConstructor.Domain.fsproj` after
`BeamTree.fs`). It is confined to the Domain project and reuses three
engine seams unchanged: `OpticalSystemSolver` (`Solvers.fs:199`),
`IncidentLightInfo.rotateY` (`Fields.fs:411`), and `EmField.propagate`
(`BerremanMatrix.fs:224`). No engine type is forked; the slice-002
`BeamTree`/`BeamNode`/`BeamBranch`/`ConstructorElement` are imported, never
redefined.

Symbols added (all in `module CurvedElements`):

- `SurfaceFigure` DU — `Spherical | Aspheric of conic:double * evenPolynomial:double<meter> list` (R-2).
- `CurvedSurface` record — `radiusOfCurvature:double<meter>` (signed, XML-doc'd sign convention), `semiAperture:double<meter>`, `figure:SurfaceFigure`, `coating:OpticalSystem` (R-1/R-4/R-8).
- `LocalZone` record — `radialPosition:double<meter>`, `localAoiShift:Angle`, `coating:OpticalSystem` (R-3).
- `sag : SurfaceFigure -> double<meter> -> double<meter> -> double<meter>` — exact conic/spherical sag carrying the sign of R (R-2/R-4).
- `sampleZones : CurvedSurface -> int -> LocalZone list` — n zones uniform in aperture radius 0..semiAperture, axis shift = `Angle.zero`, coating shared by value (R-3/R-8/R-8-share).
- `solveZone : IncidentLightInfo -> LocalZone -> SolverParameters -> Solution` — `rotateY` the local-AOI shift then run `OpticalSystemSolver(localInfo, zone.coating, parameters).solution` (R-5).
- `attachCurvedElement : ConstructorElement -> BeamNode -> BeamNode` — shapes a zone node's branch fan: `Lens` → both `Reflected`+`Transmitted`; `CurvedMirror` → `Reflected` only (transmitted dropped) (R-6).
- `gradeCoating : CurvedSurface -> LocalZone -> OpticalSystem` — per-zone multilayer-period rescale of finite `Thickness.Thickness d` films only, `Infinity` untouched, ~5% center-to-edge default computed from `localAoiShift` (R-9).

Inter-element propagation (R-7/AC-C7) reuses the existing
`EmField.propagate` seam (already wrapped by `BeamTree.childIncidentField`);
no new propagation routine is added — the AC-C7 test exercises the seam
directly over a gap `Layer`.

## Files

- New: `OpticalConstructor.Domain/CurvedElements.fs` (replaces the slice-001 placeholder).
- New: `OpticalConstructor.Tests/CurvedElementsTests.fs` (AC-C1,C2,C3,C5,C6,C7,C8).
- Edit: `OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` — register the new test file.
- Edit: `OpticalConstructor.Tests/BeamTreeTests.fs` — add AC-C4 curved-fan test.

## Risks / decisions

- **`attachCurvedElement` signature vs Map-keyed children.** `BeamNode.children`
  is `Map<BeamBranch,BeamNode>` (≤ 2 entries), so a single parent cannot hold an
  N-zone fan. The aperture fan is therefore realized as the orchestration caller
  mapping `attachCurvedElement` across per-zone nodes; the function shapes one
  zone node's two branches. Recorded in impl-log Gotchas.
- **Aspheric polynomial units.** `evenPolynomial` coefficients are typed
  `double<meter>` by the spec; to keep `sag` dimensionally `double<meter>` the
  polynomial is evaluated in the dimensionless ratio `(r / R)`. Recorded in Gotchas.
- **Grading factor.** Computed from `localAoiShift` normalized by the edge shift at
  `semiAperture` so the edge zone hits the ~5% default exactly and the axis zone is
  1.0 (unchanged). Finite `Thickness` only; `Infinity` passes through.
