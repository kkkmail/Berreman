# 001 IMPLEMENT — impl-plan

## Slice goal

Add a pure `MuellerReconstruction` domain module carrying an elevated
`Retardance` phase-delay type and a `retarderMueller` builder that produces the
Mueller matrix of an ideal (lossless) retarder at a given azimuth, plus a
BerremanTests suite proving the two acceptance identities (zero retardance =
identity; quarter-wave = standard QWP form).

## Approach

1. **New Domain module** — `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`,
   `namespace OpticalConstructor.Domain`, `module MuellerReconstruction`.
   - `open Berreman.MathNetNumericsMath` (for `degree`), `Berreman.Geometry` (for
     `Angle`), `Berreman.Fields` (for `MuellerMatrix`).
   - Elevated single-case DU `Retardance` (`| Retardance of double`) mirroring the
     engine's `Angle` shape (Geometry.fs:34): `.value`, `.degrees = value / degree`,
     `static member degree`. It is a *phase delay*, deliberately distinct from the
     azimuth `Angle`.
   - `retarderMueller (axis : Angle) (retardance : Retardance) : MuellerMatrix` built
     as `Propagation.rotateMueller axis (Propagation.muellerOfRows core)` where the
     diagonal retarder core (fast axis at 0°) is
     `[1,0,0,0],[0,1,0,0],[0,0,cos d,sin d],[0,0,-sin d,cos d]`, `d = retardance.value`.
     Reuses the EXISTING `Propagation.muellerOfRows` (Propagation.fs:35) and
     `Propagation.rotateMueller` (Propagation.fs:128) — no new matrix algebra.
   - Compiled AFTER `ExperimentDataLoad.fs` (the last Domain file) so `Propagation`,
     `Fields`, `Geometry` (and `Experiments`) are all in scope.

2. **fsproj wiring** —
   - Add `<Compile Include="MuellerReconstruction.fs" />` as the last entry of
     `OpticalConstructor.Domain.fsproj`'s compile list.
   - Add an explicit `<ProjectReference>` to `OpticalConstructor.Domain` in
     `BerremanTests.fsproj` (currently only Optimization/Analytics/OpticalProperties
     are referenced) and add `MuellerReconstructionTests.fs` to its compile list.

3. **Tests** — `Berreman/BerremanTests/MuellerReconstructionTests.fs`. Reuse the
   element-by-element compare loop from `MuellerMatrixTests.fs:20` and the shared
   `allowedDiff` tolerance from `MatrixComparison.fs:13` (do not hand-roll epsilons):
   - zero retardance at an arbitrary azimuth == `Propagation.identityMueller`;
   - quarter-wave (d = 90°) at azimuth 0° == the standard QWP form
     `[[1,0,0,0],[0,1,0,0],[0,0,0,1],[0,0,-1,0]]`;
   - quarter-wave at azimuth 45° == the standard rotated QWP form (exercises
     `rotateMueller`);
   - `Retardance` degrees/value round-trip (exercises the elevated accessor).

## Files to modify / add

- ADD `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`
- EDIT `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
- EDIT `Berreman/BerremanTests/BerremanTests.fsproj`
- ADD `Berreman/BerremanTests/MuellerReconstructionTests.fs`

## Risks

- **Sign convention.** The QWP form depends on `rotateMueller`'s R(−θ)·M·R(θ)
  convention. Verified by hand: fast-axis-0 core + azimuth 0 reproduces
  `[[..],[..],[0,0,0,1],[0,0,-1,0]]`, and azimuth 45° reproduces the standard
  `[[1,0,0,0],[0,0,0,-1],[0,0,1,0],[0,1,0,0]]`. The rotation is a pure rename of
  the existing polarizer-rotation seam, so it matches the module's own precedent.
- **Zero-warning rule.** Concrete signatures only, no generics, single-case DU
  patterns are exhaustive — no FS#### expected.
