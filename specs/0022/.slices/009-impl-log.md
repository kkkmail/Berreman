# 009 — impl-log (Part G §G.1–G.3 optimization infrastructure)

## Progress

- [x] Read system prompt, project prompt, slice spec, gate roster, prior SoW (008).
- [x] Surveyed reuse seams: `Variables.fs` `ArbitraryVariable.getSys`, `Media.fs`
      `Thickness`/`Layer`/`WedgeLayer`/`OpticalSystem`, `Dispersion.fs`
      `OpticalPropertiesWithDisp`, `Constants.fs` `meter`.
- [x] Reflected the `alglib.net` 3.19.0 public API (minlm/minlbfgs/minbleic/minnlc
      routines, delegate signatures, report properties).
- [x] Wrote `OptimizationInterface.fs` (§G.1).
- [x] Wrote `AlglibAdapter.fs` (§G.2).
- [x] Wrote `DesignParameters.fs` (§G.3).
- [x] Wrote `OptimizationInterfaceTests.fs` (AC-G1, AC-G2).
- [x] Registered files in the two `.fsproj`s.
- [x] Ran gates: build, constructor-unit-tests, unit-tests.

## Files modified

New:
- `OpticalConstructor/OpticalConstructor.Optimization/OptimizationInterface.fs`
- `OpticalConstructor/OpticalConstructor.Optimization/AlglibAdapter.fs`
- `OpticalConstructor/OpticalConstructor.Optimization/DesignParameters.fs`
- `OpticalConstructor/OpticalConstructor.Tests/OptimizationInterfaceTests.fs`

Edited:
- `OpticalConstructor/OpticalConstructor.Optimization/OpticalConstructor.Optimization.fsproj`
- `OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Decisions

- **`optimize` lives in `AlglibAdapter`, not the interface file.** The G.1 file
  MUST NOT reference ALGLIB, and §0 constraint 6 forbids an extra `IOptimizer`
  indirection layer just to "declare" the entry point. So the interface file
  owns the types and the adapter owns the single `optimize` function with the
  exact `OptimizationRequest -> Result<OptimizationResult, string>` signature
  over those types. The public surface a consumer touches is still the G.1 types.
- **ALGLIB `PackageReference` already present** (`alglib.net` 3.19.0, the GPL
  free edition) from slice 001's project creation — R-2's "add the
  PackageReference" was already satisfied; no duplicate added.
- **`NelderMead` deferred** — surfaced as `Failed "simplex backend unresolved"`
  verbatim (not dispatched to a guessed routine), per the carried-forward G.2
  decision.
- **Inequality-target penalty interpretation** — see Gotchas/Testing state.

## Gotchas

- **Inequality-target → penalty-row interpretation.** `InequalityTarget` carries
  only a bound, not a reference to a model quantity (the G.1 surface gives no
  hook tying it to a specific residual). For `minlm` each target becomes one
  hinge penalty row on the achieved residual L2 norm `‖r(x)‖`
  (`LessOrEqual b → max(0, ‖r‖-b)`, `GreaterOrEqual b → max(0, b-‖r‖)`); for
  `minnlc` the same expression becomes a nonlinear inequality constraint
  `c(x) ≤ 0`. This is the minimum mechanism that satisfies R-2 ("residual
  penalty rows for minlm / nonlinear constraints for minnlc"); the AC tests use
  empty `inequalityTargets`. A later slice that needs a target on a specific
  scalar (e.g. R at one wavelength) can compose it into the `Residual` closure.
- **`minlbfgs` is unconstrained** — ALGLIB exposes no `minlbfgssetbc`, so
  `QuasiNewtonLbfgs` ignores `bounds`. Constrained fits use
  `BoundedLinearConstrained` (minbleic) / `NonlinearConstrained` (minnlc), which
  do bound via `minXsetbc`. Recorded here per the project prompt's
  "pick-a-sensible-default-and-note-it" rule.
- **Scalar minimizers (lbfgs/bleic/nlc) minimize the SSR `‖r(x)‖²`** with a
  forward-difference gradient/Jacobian; `minlm` keeps the vector residual and
  uses `minlmcreatevj` only when an analytic Jacobian is supplied AND there are
  no penalty rows (the hinge-of-norm penalty rows have no clean analytic
  Jacobian), else `minlmcreatev` numerical-diff with a 1e-6 step.
- **`optimize` is in `AlglibAdapter`, not the interface file** — see Decisions.
  The G.1 file cannot reference ALGLIB and §0 constraint 6 forbids an extra
  `IOptimizer` indirection; the public `optimize` still has the exact
  `OptimizationRequest -> Result<OptimizationResult, string>` signature over the
  G.1 types.
- **F# byref-in-delegate** — `minlbfgs`/`minbleic` use `ndimensional_grad` whose
  objective parameter is `byref<float>`. F# accepts a byref lambda coerced to the
  delegate type (probed before writing). `minlm`/`minnlc` use the array-only
  `ndimensional_fvec`/`ndimensional_jac`, no byref.

## Testing state

All three slice-009 gates green in the local run:

- **build** (`dotnet build Berreman.slnx -c Release`): 0 errors. The three new
  Optimization files compile in order (interface → adapter → mapping); the
  `alglib.net` 3.19.0 PackageReference resolved (already present from slice 001).
- **constructor-unit-tests** (`OpticalConstructor.Tests`): 113 passed, 0 failed,
  0 skipped (baseline 107 → +6: the new `OptimizationInterfaceTests` module — 4
  AC-G1 facts incl. the analytic-Jacobian and exception-boundary cases, 2 AC-G2
  facts).
- **unit-tests** (`BerremanTests`): 76 passed, 5 skipped, 0 failed (baseline 76;
  untouched by this slice).

`commit_ready: true` — every R-1..R-3 requirement is addressed in this round
(NelderMead deferred exactly as the spec directs, not as a self-declared split).
