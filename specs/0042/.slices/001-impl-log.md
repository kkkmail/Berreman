# 001 IMPLEMENT — impl-log

## Progress

- [x] New module `MuellerReconstruction.fs` (Retardance DU + retarderMueller)
- [x] Wire it into `OpticalConstructor.Domain.fsproj` (compiled last)
- [x] Add explicit Domain ProjectReference to `BerremanTests.fsproj`
- [x] New `MuellerReconstructionTests.fs` + fsproj compile entry

## Files modified

- **ADD** `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`
  — `namespace OpticalConstructor.Domain`, `module MuellerReconstruction`.
  - `Retardance` single-case DU (`| Retardance of double`) with `.value` (radians),
    `.degrees = value / degree`, and `static member degree`. Mirrors the engine's
    `Angle` shape (Geometry.fs:34) but is a PHASE, deliberately distinct from the
    azimuth `Angle`.
  - `retarderMueller (axis : Angle) (retardance : Retardance) : MuellerMatrix` =
    `Propagation.rotateMueller axis (Propagation.muellerOfRows core)` with the
    fast-axis-0 diagonal retarder core `[1,0,0,0],[0,1,0,0],[0,0,c,s],[0,0,-s,c]`
    (`c = cos d`, `s = sin d`, `d = retardance.value`). Built ONLY from the existing
    `Propagation.muellerOfRows` (Propagation.fs:35) and `Propagation.rotateMueller`
    (Propagation.fs:128) — no new 4×4 algebra. Local-binding `let c`/`let s` + `-s`
    matches the precedent in `Propagation.rotationMueller` (Propagation.fs:117-124).
- **EDIT** `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
  — `<Compile Include="MuellerReconstruction.fs" />` appended AFTER `ExperimentDataLoad.fs`
  (the last Domain file), so `Propagation`, `Fields`, `Geometry`, `Experiments` are in scope.
- **EDIT** `Berreman/BerremanTests/BerremanTests.fsproj`
  — appended `<Compile Include="MuellerReconstructionTests.fs" />`; added an explicit
    `<ProjectReference>` to `..\OpticalConstructor\OpticalConstructor.Domain\OpticalConstructor.Domain.fsproj`.
- **ADD** `Berreman/BerremanTests/MuellerReconstructionTests.fs`
  — 5 xUnit facts. Reuses the element-by-element Mueller compare loop from
    `MuellerMatrixTests.fs:20` and the shared `allowedDiff` (MatrixComparison.fs:13);
    no hand-rolled epsilon.

## Testing state

Per the `IMPLEMENT` worker's **Invariant 6 (act only; run no checks)**, the worker
did NOT execute `dotnet build` / `dotnet test` — the arc-runner's deterministic gate
engine runs the `build`, `unit-tests`, and `constructor-unit-tests` gates after exit.

Tests authored (BerremanTests / `MuellerReconstructionTests`):

- `zero retardance is the identity Mueller matrix (arbitrary azimuth)` — axis 37°,
  d = 0 ⇒ `Propagation.identityMueller` (acceptance #1). R(−θ)·I·R(θ) = I for any θ.
- `zero retardance at zero azimuth is the identity Mueller matrix` — axis 0, d = 0.
- `quarter-wave retarder at azimuth 0 is the standard QWP Mueller form` — axis 0,
  d = 90° ⇒ `[[1,0,0,0],[0,1,0,0],[0,0,0,1],[0,0,-1,0]]` (acceptance #2).
- `quarter-wave retarder at azimuth 45 is the standard rotated QWP Mueller form` —
  axis 45°, d = 90° ⇒ `[[1,0,0,0],[0,0,0,-1],[0,0,1,0],[0,1,0,0]]` (exercises `rotateMueller`).
- `Retardance degrees and value round-trip` — `Retardance.degree 90.0` ⇒ `.degrees ≈ 90`,
  `.value ≈ π/2`.

Hand-verification of the sign convention (below) confirms both acceptance forms and the
azimuth-45° rotated form fall within `allowedDiff` (1e-5); residuals are ~1e-16
(`cos(π/2) ≈ 6.1e-17`).

## Artifacts

None — pure numerical code, no captured logs/screenshots/traces.

## Gotchas

- **Invariant 6 vs. base step 5.** The shared base's step 5 ("Run the project's gates
  locally") is explicitly superseded by the `IMPLEMENT` delta's Invariant 6 ("act only;
  run no checks; the gate engine is the sole gate authority"). Decision: did NOT run
  `dotnet build`/`dotnet test`; the gate engine runs them post-exit. Consequently the
  SoW `gates:` YAML carries the first-slice zero baseline (0 is explicitly valid on the
  first slice of an arc) rather than a locally-captured count.
- **Sign convention.** `retarderMueller` reuses `Propagation.rotateMueller`'s
  R(−θ)·M·R(θ) convention. Verified by hand that the fast-axis-0 core at azimuth 0
  gives the standard QWP form and at azimuth 45° gives `[[1,0,0,0],[0,0,0,-1],[0,0,1,0],[0,1,0,0]]`
  — the standard rotated QWP. No independent rotation code was written.
- **`Propagation` unqualified.** `MuellerReconstruction` names `Propagation.*` unqualified
  because it lives in the same `OpticalConstructor.Domain` namespace and compiles after
  `Propagation.fs`; no `open` is needed for it.
- **Explicit Domain reference.** `BerremanTests.fsproj` already reaches Domain
  transitively via `OpticalConstructor.Optimization`; the slice mandates the EXPLICIT
  Domain `ProjectReference` regardless. MSBuild dedupes by path (same project), so no
  duplicate-reference / MSB3277 version conflict is introduced.
