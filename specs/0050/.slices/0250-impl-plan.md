# Step 0250 — impl plan (IMPLEMENT: cut plates through the spec 0044 Berreman forward)

## Approach

Add the cut plate as one more case of `BoundOpticalElement` in
`Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemEvaluation.fs`. It reuses the spec 0044 forward
(`MuellerInverse.createBerremanForward`, which orients through `orientForCut`) and copies nothing from it.

1. `ItemEvaluationError.CutPlateSolveErr of reason : string`, rendered
   `the cut plate's Berreman solve failed: <reason>`.
2. On `ElementOrientation`, two intrinsic members:
   - `sampleAzimuth : MuellerInverse.SampleAzimuth`, the rotation as the plate's azimuth about its normal;
   - `observable : MuellerInverse.Observable`, `TransmittedMueller` on `BranchTransmitted` and
     `ReflectedMueller` on `BranchReflected`.
3. `[<ReferenceEquality>] type CutPlate = { cut; thickness; predict }` with
   `static member ofForward forward parameters cut thickness`. `predict` is `forward.muellerOf` with the
   parameters applied. `[<ReferenceEquality>]` keeps `BoundOpticalElement`'s value equality, as step 249 found.
4. `BoundOpticalElement.BoundCutPlate of CutPlate`. `muellerAt` handles it with an or-pattern over both branches and
   no wildcard. It builds the `MeasurementConfiguration` from the plate, the orientation and λ, calls `predict`, and
   maps `ForwardSolveFailedErr (_, reason)` to `CutPlateSolveErr reason`.
5. The docs on the touched types and members are widened, one to three lines each, citing spec 0050 S8.1.

## Tests (`ItemEvaluationTests`)

The tests use a test-local builder: `UniaxialParameters` (n_o 1.544, n_e 1.553, g = 0) →
`OpticalProperties.fromRefractionIndex (n_o, n_o, n_e)`, under `createBerremanForward … SolverParameters.defaultValue`.

- `a cut plate is predicted through the Berreman forward at the element's spin, incidence, branch and wavelength`:
  - an XCut 1 mm plate at rotation 30°, incidence 20° and 600 nm equals the forward's `muellerOf` exactly;
  - the reflected branch equals the forward's `ReflectedMueller` result;
  - at rotation 60° the transmitted matrix is different.
- `a cut plate whose forward fails answers CutPlateSolveErr`: a test-local failing proxy makes `muellerAt` answer
  `CutPlateSolveErr "degenerate"`.
- The render fact gains a `CutPlateSolveErr` row, so the distinct count goes from 6 to 7.

## Risks

- **Record-label resolution for `MeasurementConfiguration` / `ForwardModelProxy` without opening `MuellerInverse`.**
  These are type-annotated. If inference fails, qualify the labels.
- **The field `thickness` also appears on `SampleLayer`.** `CutPlate` is declared after `withThicknessValues`, and
  the constructions are annotated.
- **ELEV (ELEV001 / 005 / 010 / 014 / 016).** `CutPlate`, `BoundCutPlate` and `CutPlateSolveErr` must be unique names
  across `Softellect.*`. Elevation is run after the build.
- **1 mm at Δn 0.009 and 600 nm is 15 whole waves at normal incidence.** At 20° the path differs, so turning the
  azimuth still changes the matrix. The fact asserts a measurable difference.
