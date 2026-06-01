# 009 — impl-plan (Part G §G.1–G.3 optimization infrastructure)

## Approach

Three net-new files in `OpticalConstructor.Optimization`, in compile order
**interface → adapter → mapping**, plus one new test module in
`OpticalConstructor.Tests`.

1. **`OptimizationInterface.fs`** (§G.1) — the library-agnostic abstraction in
   our own F# records/DUs only, NO ALGLIB type in its surface: `Residual`,
   `Jacobian`, `ParameterBounds`, `InequalityTarget`, `OptimizationMethod`,
   `OptimizationRequest`, `TerminationReason`, `OptimizationResult`. The public
   `optimize` entry point's signature (`OptimizationRequest -> Result<…,string>`)
   is realised in the adapter (the interface file cannot reference ALGLIB, and
   §0 constraint 6 forbids an extra indirection layer / `IOptimizer`).

2. **`AlglibAdapter.fs`** (§G.2) — the ONLY ALGLIB-referencing file. `optimize`
   dispatches each `OptimizationMethod` to its ALGLIB routine:
   `LevenbergMarquardt → minlm` (`minlmcreatevj` when an analytic Jacobian is
   supplied and there are no penalty rows, else `minlmcreatev` numerical-diff;
   `minlmsetbc` for bounds), `QuasiNewtonLbfgs → minlbfgs`,
   `BoundedLinearConstrained → minbleic`, `NonlinearConstrained → minnlc`.
   `NelderMead` is surfaced as `Failed "simplex backend unresolved"` — its
   simplex backend is the carried-forward deferred decision and MUST NOT be
   bound to a guessed routine. Completion codes → `TerminationReason`; the whole
   body is wrapped in `try/with → Result.Error` so no ALGLIB exception escapes.
   `InequalityTarget`s become hinge penalty rows for `minlm`, nonlinear
   inequality constraints for `minnlc`.

3. **`DesignParameters.fs`** (§G.3) — `DesignParameter` record (`name`, a
   `getSys : OpticalSystem -> double -> OpticalSystem` closure mirroring the
   `ArbitraryVariable.getSys` seam, and the SI bound pair). Smart constructors
   for the three supported variables: `layerThickness` (rebuilds the film's
   `Thickness (v * 1.0<meter>)`, lower bound fixed 0 → negative-thickness
   prohibition as a bound), `wedgeAngle` (sets `WedgeLayer.angle`),
   `dispersionCoefficient` (substitutes into the supplied
   `OpticalPropertiesWithDisp` then resolves at a wavelength). `applyVector`
   folds the ordered list left over a base `OpticalSystem` in vector order;
   `bounds` assembles the G.1 `ParameterBounds`. No reflection — every mapping
   is an explicit closure.

4. **`OptimizationInterfaceTests.fs`** — AC-G1 (LM returns an `OptimizationResult`
   within `maxIterations`; NelderMead → `Failed "simplex backend unresolved"`;
   an in-callback exception stays behind the `Result` boundary) and AC-G2 (the
   fold lands canonical-meter `Thickness` values, lower bound 0).

## Files

- New: `OptimizationInterface.fs`, `AlglibAdapter.fs`, `DesignParameters.fs`,
  `OpticalConstructor.Tests/OptimizationInterfaceTests.fs`.
- Edited: `OpticalConstructor.Optimization.fsproj` (register the 3 files in
  compile order — the `alglib.net` PackageReference is already present from
  slice 001), `OpticalConstructor.Tests.fsproj` (register the test file).

## Risks

- ALGLIB `minlmoptimize` overload resolution from F# (fvec vs jac arity) and
  byref callback params (`minlbfgs`/`minbleic` use `ndimensional_grad` with a
  `byref<float>` objective). Probed: F# accepts a byref lambda coerced to the
  delegate. minlm/minnlc use array-only `ndimensional_fvec`/`ndimensional_jac`.
- The contractful invariant is that NO ALGLIB type/exception escapes the G.1
  boundary; exercised explicitly by the boundary test.
