# 009 — Optimization infrastructure (Part G §G.1–G.3) — state of the world

## Where we are

Slice 009 opens Part G of spec 0022: the optimization infrastructure every later
fitting/synthesis slice depends on. It sits on the domain/storage/materials/
construction/light/analysis layers from slices 001–008 and is almost wholly
net-new orchestration above the existing forward solver. It lands three things —
the library-agnostic optimization interface (§G.1), the single ALGLIB adapter
behind it (§G.2), and the flat-`float[]` ⇄ `OpticalSystem` design-variable
mapping (§G.3) — in the gated, UI-less `OpticalConstructor.Optimization` /
`OpticalConstructor.Tests` projects. Slice 010 builds the merit/target editor,
local-refinement entry point, ellipsometric Ψ/Δ targets and inverse fitting over
this; slice 011 adds fit-quality, synthesis, the Synthesis/Fit page, and the
Wolfram-reference validation.

## What's working

- Add `OptimizationInterface.fs` (§G.1): `Residual`/`Jacobian`/`ParameterBounds`/
  `InequalityTarget`/`OptimizationMethod`/`OptimizationRequest`/`TerminationReason`/
  `OptimizationResult` — our own F# types, no ALGLIB type in the surface.
- Add `AlglibAdapter.fs` (§G.2): the sole ALGLIB-referencing file; `optimize`
  maps each method to minlm/minlbfgs/minbleic/minnlc, traps every exception into
  `Result.Error`, and surfaces `NelderMead` as the deferred
  `Failed "simplex backend unresolved"`.
- Add `DesignParameters.fs` (§G.3): `DesignParameter` + `layerThickness`/
  `wedgeAngle`/`dispersionCoefficient` smart constructors reusing the
  `ArbitraryVariable.getSys` seam; `applyVector` folds a vector over a base
  `OpticalSystem`; `bounds` assembles the G.1 `ParameterBounds`.
- Register the three files (interface → adapter → mapping) in
  `OpticalConstructor.Optimization.fsproj` and add `OptimizationInterfaceTests.fs`
  to `OpticalConstructor.Tests.fsproj`.
- Prove AC-G1 (LM returns an `OptimizationResult` within `maxIterations`, no
  ALGLIB type/exception crosses the boundary, NelderMead deferred) and AC-G2
  (the fold lands canonical-meter `Thickness` values, lower bound 0).

## Tests

- `build` gate: PASS — `dotnet build Berreman.slnx -c Release`, 0 errors. The
  three new Optimization files compile in dependency order; the `alglib.net`
  3.19.0 `PackageReference` (the GPL free edition, already on the project from
  slice 001) resolved under the .NET 10 / x64 SDK; no new C# project.
- `constructor-unit-tests` gate (`OpticalConstructor.Tests`): PASS — 113 passed,
  0 failed, 0 skipped (baseline 107 → +6 in the new `OptimizationInterfaceTests`).
- `unit-tests` gate (`BerremanTests`): PASS — 76 passed, 5 skipped, 0 failed
  (baseline 76; this slice does not touch `BerremanTests`).

```yaml
gates:
  berreman_unit_tests:    76
  constructor_unit_tests: 113
```

## Architecture

- **Boundary isolation (§A.8).** The §G.1 interface is expressed only in our F#
  records/DUs; `AlglibAdapter.fs` is the single file that references ALGLIB. The
  public `optimize` entry point lives in the adapter (the interface file cannot
  reference ALGLIB, and §0 constraint 6 forbids an extra `IOptimizer` indirection
  layer just to declare it) but keeps the exact
  `OptimizationRequest -> Result<OptimizationResult, string>` signature over the
  G.1 types. The whole adapter body runs under `try/with → Result.Error`, so no
  ALGLIB type or exception ever crosses the boundary.
- **Method → routine map.** `LevenbergMarquardt → minlm` (`minlmcreatevj` with an
  analytic Jacobian and no penalty rows, else `minlmcreatev` numerical-diff;
  `minlmsetbc` for bounds), `QuasiNewtonLbfgs → minlbfgs`,
  `BoundedLinearConstrained → minbleic`, `NonlinearConstrained → minnlc`. The
  scalar minimizers (lbfgs/bleic/nlc) minimize the SSR `‖r(x)‖²` with a
  forward-difference gradient/Jacobian; minlm keeps the vector residual.
- **`InequalityTarget` handling.** A hinge penalty row on the residual L2 norm
  for minlm; the same expression as a nonlinear `c(x) ≤ 0` constraint for minnlc.
- **No engine linear algebra through ALGLIB and no optimization through the math
  seam (§A.8).** The adapter routes no matrix op through `MathNetNumericsMath.fs`
  and does not reference `ExtremeNumericsMath.fs`; ALGLIB's own LA does the LM
  step.
- **Design-variable mapping reuses the seam.** `DesignParameter.getSys` mirrors
  `ArbitraryVariable.getSys : OpticalSystem -> double -> OpticalSystem`
  (`Variables.fs:37`); thickness rebuilds `Thickness (v * 1.0<meter>)`
  (`Media.fs:12`, `Constants.fs:6`), wedge angle sets `WedgeLayer.angle`
  (`Media.fs:34`), dispersion substitutes into `OpticalPropertiesWithDisp`
  (`Dispersion.fs:53`). Every mapping is an explicit closure — no reflection.
  The negative-thickness prohibition is the lower-bound-0 `ParameterBounds`, not
  a separate validator.

## Deferred

- **The `NelderMead` simplex backend remains an OPEN deferred decision** (G.2).
  010 §II.5 requires a [Standard] Nelder–Mead simplex method but 010 §III.4 names
  no ALGLIB routine for a simplex solver. Its concrete backend — an ALGLIB
  routine if one is later confirmed to exist, OR an explicit in-file F# Nelder–
  Mead loop over the G.1 `Residual` closure — is deliberately undecided and MUST
  remain undecided until that decision is taken, traceable to the 010-required
  simplex feature. Until then the adapter surfaces
  `Failed "simplex backend unresolved"` and MUST NOT bind it to a spec-invented
  routine.
- Merit/target-function editor, local-refinement entry point, ellipsometric Ψ/Δ
  targets, inverse fitting → slice 010 (§G.4–G.7).
- Fit-quality reporting, needle/tunnelling synthesis, global optimization,
  Synthesis/Fit UI page, Wolfram-reference validation in
  `BerremanTests/OptimizationTests.fs` → slice 011 (§G.8–G.11).
- Progress-callback registry — G.1 stays callback-free; G.10 wires progress via
  a separate channel (slice 011).

## Gotchas

- **`optimize` is in `AlglibAdapter`, not the interface file.** Consumers call
  `OpticalConstructor.Optimization.AlglibAdapter.optimize` with the G.1 types;
  the interface file owns only the type definitions.
- **`InequalityTarget` carries only a bound**, not a reference to a model
  quantity. The adapter penalizes the residual L2 norm against the bound. A
  consumer needing a target on a specific scalar should compose it into the
  `Residual` closure rather than expecting the bound to bind to one residual.
- **`QuasiNewtonLbfgs` ignores `bounds`** — ALGLIB's minlbfgs is unconstrained
  (no `minlbfgssetbc`). Use `BoundedLinearConstrained`/`NonlinearConstrained` for
  box-bounded fits.
- **F# byref-in-delegate works** for `ndimensional_grad`'s `byref<float>`
  objective (minlbfgs/minbleic); minlm/minnlc use array-only callbacks.
- **ALGLIB termination codes →** `5 → MaxIterationsReached`,
  `7 → StepTooSmall`, other positive → `Converged`, negative → `Failed`.
- **Only `LevenbergMarquardt` and `NelderMead` are exercised by tests**; the
  other three routines are wired and compile-checked but not asserted (their
  end-to-end validation arrives with the slice-011 Wolfram suite).

## Changelog

- 2026-06-01 — Slice 009 (Part G §G.1–G.3): added `OptimizationInterface.fs`,
  `AlglibAdapter.fs`, `DesignParameters.fs` to `OpticalConstructor.Optimization`
  and `OptimizationInterfaceTests.fs` to `OpticalConstructor.Tests`; registered
  all four in their `.fsproj`s (compile order interface → adapter → mapping). The
  library-agnostic §G.1 interface, the single ALGLIB §G.2 adapter (NelderMead
  deferred as `Failed "simplex backend unresolved"`), and the §G.3 design-
  variable fold are implemented and unit-tested. All three gates green (build 0
  errors; constructor-unit-tests 113 passed; unit-tests 76 passed).
