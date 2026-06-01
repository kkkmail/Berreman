# 006 — State of the world: Lenses & non-flat (curved) mirror elements (Part C)

## Where we are

Slice 006 completes the Part A–D foundation by landing Part C: the curved-element
domain. It sits on the slice-002 beam tree and the engine 4×4 solver, turning one
curved surface into many solver runs. All code is confined to the new
`OpticalConstructor.Domain/CurvedElements.fs`; the single admitted physics
extension (§A.5) is the per-zone local-AOI fan, and three engine seams are reused
unchanged — `OpticalSystemSolver` (`Solvers.fs:199`), `IncidentLightInfo.rotateY`
(`Fields.fs:411`), and `EmField.propagate` (`BerremanMatrix.fs:224`). No engine
type is forked, and the slice-002 `BeamTree`/`BeamNode`/`BeamBranch`/
`ConstructorElement` types are imported, never redefined. Later slices (E sources,
F results, G synthesis/fit, H optimizers, I persistence, J builders) build on the
now-complete foundation.

## What's working

- Add the curved-surface domain in `CurvedElements.fs`: `CurvedSurface` (signed `radiusOfCurvature`, `semiAperture`, `figure`, reused `OpticalSystem` coating — all canonical meters), the `SurfaceFigure` DU (`Spherical`/`Aspheric`), and the pure `sag` function (exact conic/spherical sag carrying the §C.4 sign convention with no override flag).
- Add `sampleZones`: decompose a surface into N local flat zones uniform in aperture radius (axis zone `localAoiShift = Angle.zero`), each sharing one immutable coating instance by value.
- Add `solveZone`: form the local incident light via `IncidentLightInfo.rotateY` and run the unforked `OpticalSystemSolver(localInfo, coating, parameters)`, returning the engine `Solution` reflected/transmitted split.
- Add `attachCurvedElement`: shape a zone node's beam fan — a `Lens` exposes both `Reflected` and `Transmitted`, a `CurvedMirror` exposes `Reflected` only and drops the transmitted field (mirror special case).
- Add `gradeCoating`: per-zone EUV multilayer-period rescale of finite `Thickness.Thickness d` films only (~5% center-to-edge default from `localAoiShift`), leaving `Thickness.Infinity` untouched and all values `double<meter>`.
- Add `CurvedElementsTests.fs` and extend `BeamTreeTests.fs`: constructor-unit-tests rise 66 → 77; build green at Release/x64; `BerremanTests` held at 70.

## Tests

- `build` — PASS (`Build succeeded.`, `0 Error(s)`; no lowercase `error` token). Log `.artifacts/006-build.log`.
- `unit-tests` — PASS (Passed 70 / Skipped 5 / Total 75; `BerremanTests` untouched). Baseline `berreman_unit_tests = 70` held. Log `.artifacts/006-unit-tests.log`.
- `constructor-unit-tests` — PASS (Passed 77 / Failed 0 / Total 77; up from slice-005 baseline 66). Log `.artifacts/006-constructor-unit-tests.log`.

Coverage: AC-C1 (radius/aperture entered in nm/µm/mm stored as `double<meter>`
matching the `Constants.fs` factors, no non-SI value left); AC-C2 (`sampleZones n`
returns n uniform zones, axis `localAoiShift = Angle.zero`, edge lands on the
aperture); AC-C3 (axis zone reproduces the flat `OpticalSystemSolver`, off-axis
zone shifts `incidenceAngle` via `rotateY` and differs from the unshifted solve);
AC-C4 (`CurvedMirror` fan Reflected-only, `Lens` both branches); AC-C5
(concave → negative / convex → positive radius, `sag` signed and equal to the exact
spherical sag); AC-C6 (`gradeCoating` axis unscaled, edge scales finite `Thickness`
by the ~5% factor, `Infinity` untouched, all `double<meter>`); AC-C7 (inter-element
gap carried by the reused `EmField.propagate` over a `Layer` whose thickness is the
axial separation); AC-C8 (grading disabled → all zones share one immutable
`OpticalSystem` coating, no rescale). None deferred.

## Architecture

- **Local-interface evaluation, not a new engine.** A curved surface is `sampleZones`d into local flat zones; each zone is `solveZone`d at its local AOI through the existing `OpticalSystemSolver`. `solveZone` returns the engine `Solution` directly — reflectance/transmittance is never re-implemented. This is the only admitted §A.5 extension; there is no second physics engine, no ABCD matrix module, no Gaussian-beam path (that stays the FFT scaffolding, out of scope).
- **Sign convention is intrinsic to the sag formula.** `sag` uses the exact conic form `c r² / (1 + sqrt(1 − (1+k) c² r²))` with `c = 1/R`. The result inherits the sign of `c`, so concave (negative `R`) → negative sag and convex (positive `R`) → positive sag without any per-element flag or convention selector. A concave vs. convex mirror is distinguished solely by the sign of `radiusOfCurvature`.
- **Fan shape is capped by the slice-002 `Map`-keyed children.** `BeamNode.children : Map<BeamBranch, BeamNode>` holds at most two entries, so `attachCurvedElement` shapes one zone node's two branches; the N-zone aperture fan is the orchestration caller mapping the function across per-zone nodes. The mirror reflected-only drop reuses the slice-002 `BeamNode.attach` smart constructor rather than a parallel rule.
- **Coating reuse and grading.** A coating is an ordinary `OpticalSystem` (`films`/`Substrate`/`lower` verbatim); zones share it immutably by value. Grading is a per-zone copy that rescales only finite multilayer `Thickness` values — no graded-layer type, no period-vs-radius table, no schema versioning.

## Deferred

- The aspheric polynomial's exact physical scaling — the committed [Core] tests exercise the spherical path; the asphere path is type-correct (evaluated in the dimensionless `r/R` ratio to keep `sag` in `double<meter>`) and reduces to the conic base when the polynomial is empty. A later slice may revisit the coefficient convention if a directive pins it.
- Wavefront diffraction, full Gaussian-beam waist/divergence, aberration ray tracing, image formation, adaptive aperture-sampling refinement, and zone-solve caching/retries — all out of scope per §C.0/§C.7.
- The `constructorElement` schema `$def` fill for the curved-element fields and any `.binz` sidecar for per-zone solver outputs — storage (Part A §A.7); this slice keeps fields in canonical SI so they serialize through the existing model, adding no FsPickler path.
- Source/cone (Part E), results (Part F), synthesis/fit (Part G), optimizers (Part H), persistence (Part I), builders (Part J) — later slices.

## Gotchas

- **`attachCurvedElement` is per-zone, not per-aperture.** Because `BeamNode.children` is branch-keyed (≤ 2), it cannot itself hold an N-zone fan; it shapes one zone node's branches. The caller maps it across `sampleZones` output. This is the only reading consistent with the un-redefinable slice-002 types — do not try to make one node carry all zones.
- **A `Thickness.Infinity` film cannot be solved.** The engine propagator raises `"TODO: Implement infinite thickness"` (`BerremanMatrix.fs:125`) on an `Infinity`-thickness film. Solver-driven tests (and any real curved-element coating that gets solved) must use finite films; `Infinity` is only safe in `gradeCoating`, which is a pure thickness map and never solved. The tests split fixtures accordingly.
- **`Reflected`/`Transmitted` are defined in both `Fields.RT` and `BeamTree.BeamBranch`.** `CurvedElements.fs` opens both namespaces, so all branch references are qualified `BeamBranch.Reflected`/`BeamBranch.Transmitted`.
- **`EmField.propagate` needs `open Berreman.BerremanMatrix`.** It is a type augmentation in that module, not a member on the `Fields` definition; tests touching propagation must open it.
- **`OpticalSystemSolver` has a 2-arg and a 3-arg ctor.** `solveZone` uses the 3-arg `(info, system, parameters)` form so `SolverParameters` flows through unchanged; the `.solution` member yields the `Solution`.

## Changelog

- 2026-06-01 (slice 006): Add the Part C curved-element domain — `CurvedElements.fs` with `CurvedSurface`/`SurfaceFigure`/`LocalZone`, the `sag`/`sampleZones`/`solveZone`/`attachCurvedElement`/`gradeCoating` functions, the signed-radius convention intrinsic to the sag formula, immutable coating reuse, and EUV depth-graded period scaling — all over the reused `OpticalSystemSolver`/`rotateY`/`EmField.propagate` seams and the slice-002 `BeamTree`, unit-tested. Add `CurvedElementsTests.fs` (AC-C1/C2/C3/C5/C6/C7/C8) and extend `BeamTreeTests.fs` (AC-C4). All three gates green: build 0 errors, unit-tests 70 held, constructor-unit-tests 66 → 77.

```yaml
gates:
  berreman_unit_tests:    70
  constructor_unit_tests: 77
```
