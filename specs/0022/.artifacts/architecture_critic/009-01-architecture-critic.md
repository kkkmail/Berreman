# Architecture critique -- 009.slice-md cycle 1

## Summary

Clean, well-isolated slice with one substantive design concern. The three
files land exactly where the spec places them, the §A.8 ALGLIB boundary is
respected to the letter (one referencing file, every body under
`try/with → Result.Error`), and the design-variable mapping reuses the real
`ArbitraryVariable.getSys` seam without reflection. The one finding worth the
judge's attention is forward-looking: the fixed *absolute* finite-difference
step in the scalar minimizers sits at the same magnitude as canonical-SI
thicknesses (~1e-7 m), a latent numerical hazard the canonical-units rule
makes systematic rather than incidental.

## Layering

No violation. `OpticalConstructor.Optimization` depends downward on the
Berreman core (`Media`, `Dispersion`, `Geometry`, `Constants`, `Fields`,
`MaterialProperties`) and on `alglib`; nothing reaches back up. `AlglibAdapter.fs`
is verifiably the sole ALGLIB-referencing file — `OptimizationInterface.fs` is
pure F# records/DUs and `DesignParameters.fs` touches only domain types. The
adapter routes no matrix op through `MathNetNumericsMath.fs` and never opens
`ExtremeNumericsMath.fs`, satisfying the §A.8 "optimization only, not linear
algebra" directive.

The one structural wrinkle is that the public `optimize` entry point lives in
`AlglibAdapter` (the ALGLIB-referencing module), not in `OptimizationInterface`.
A consumer therefore names `AlglibAdapter.optimize` to invoke optimization.
Because the signature is `OptimizationRequest -> Result<OptimizationResult,string>`
over G.1 types only, no ALGLIB type leaks transitively, so the §A.8 contract
("physics/fitting code never depends on ALGLIB directly") holds at the type
level — but the *module reference* a caller writes is the ALGLIB module's name.
This is defensible (see Spec fit) and the worker documented it; flagging it so
the judge weighs it consciously rather than missing it.

## Separation of concerns

The adapter's per-method runners (`runLm`/`runLbfgs`/`runBleic`/`runNlc`) each
co-locate residual assembly, hinge-penalty construction, finite-difference
gradient/Jacobian, and ALGLIB state wiring. For an adapter this concentration
is appropriate — the shared seams (`sumSq`, `terminationReason`, `result`,
`ssrGrad`) are already factored out, which is the right amount of extraction
for a minimum-impl slice. No push-behind-an-existing-seam opportunity is being
missed here.

## Consistency

Idiomatic throughout: PascalCase modules/types, camelCase locals,
`Unchecked.defaultof<...>` for ALGLIB out-state structs (the standard alglib.net
F# pattern), smart-constructor style in `DesignParameters` matching the rest of
the tree. One mild asymmetry: `dispersionCoefficient` takes a `setCoeff`
closure plus a fixed `model` and `waveLength`, so its arity and shape diverge
from the closure-returning `layerThickness`/`wedgeAngle`. This is justified —
dispersion resolution genuinely needs the model and a wavelength to produce an
`OpticalProperties` — but the three "design variable" constructors no longer
read as siblings. Not worth a change this slice; noting it as a small
consistency cost a future reader will feel.

## Spec fit

Strong. R-1's interface is exactly the named record/DU set; R-2's method→routine
map (`minlm`/`minlbfgs`/`minbleic`/`minnlc`) is correct and `NelderMead`
surfaces as `Failed "simplex backend unresolved"` verbatim, not a guessed
routine; R-3's fold reuses `ArbitraryVariable.getSys`'s exact signature and
rebuilds `Thickness (v * 1.0<meter>)` with lower bound 0 as the
negative-thickness prohibition. AC-G1 and AC-G2 are both exercised.

Two scope notes, neither a defect. (1) R-1 says the *interface* MUST expose the
single `optimize` entry point; the worker placed it in the adapter and cites §0
constraint 6 (no extra indirection layer) as why it cannot live in the
ALGLIB-free interface file. That is the most defensible reading of two directives
in tension, and it is recorded in the impl-log — accept as written. (2)
`wedgeAngle` and `dispersionCoefficient` are implemented but unit-tested only
indirectly (no fold test drives them); only `layerThickness` is asserted in
AC-G2. The slice's acceptance criteria only require thickness, so this is
spec-conformant under-coverage, not under-delivery — but it means two of the
three design-variable constructors land unverified.

## Evolvability

The `InequalityTarget` shape is the decision most likely to corner slice 010.
The DU carries only a bound (`LessOrEqual b` / `GreaterOrEqual b`) with no
selector tying it to a model quantity, so the adapter penalizes the achieved
residual *L2 norm* `‖r(x)‖` against the bound. But 010 §II.5's motivating
target is "R ≤ 0.5%" — a constraint on one specific physical scalar, not the
global fit-residual norm. When 010 wires real targets it will likely need a
target bound to a named quantity, and the worker's own gotcha already concedes
the workaround is "compose it into the `Residual` closure." That workaround
pushes target semantics out of the typed interface and into opaque closures,
which is exactly the kind of locked-in shape this head exists to flag. It is
not wrong for this slice (AC tests use empty targets), but the judge should
note that the `InequalityTarget`↔residual binding is unresolved and may force a
G.1 surface change in 010.

## Risks

**Finite-difference step vs. canonical-SI magnitudes.** `ssrGrad` uses a fixed
absolute step `h = 1.0e-7` (and `runLm` a `1.0e-6` numerical diffstep). Per §0
constraint 3 every parameter is stored in canonical SI, so a layer thickness is
~1e-7 m (100 nm). An absolute FD step of 1e-7 m is then the same order as the
parameter itself — a near-100% perturbation that will produce a poor gradient
for `minlbfgs`/`minbleic`/`minnlc` and a marginal one for `minlm`. These paths
are only compile-checked (the SoW concedes "only LevenbergMarquardt and
NelderMead are exercised by tests"), so the hazard is latent and will first bite
the slice-011 Wolfram suite on a thickness fit. A relative or
per-parameter-scaled step would be the durable fix; worth surfacing now because
the canonical-units rule makes small-magnitude parameters the norm, not the
exception.

**`applyVector` silent precondition.** The fold indexes `vector.[i]` with no
length guard; a vector shorter than `parameters` throws `IndexOutOfRange`
rather than a typed error. The comment documents "MUST be at least as long,"
which is acceptable for a minimum-impl slice, but it is an unguarded
precondition on the public mapping entry point. Minor.

## Bottom line

I would ship this. The boundary invariants the slice exists to establish are
correct and proven, the reuse seam is genuine, and the deferred `NelderMead`
decision is carried exactly as directed. The two real findings — the
absolute-FD-step/canonical-units interaction and the `InequalityTarget`-binds-to-norm
shape — are both forward-looking liabilities for slices 010/011, not defects in
what this slice claims to deliver, and both are at least partially acknowledged
in the worker's own notes. My recommended posture is **pass with the FD-step and
`InequalityTarget`-binding concerns recorded for slice 010 to confront**; the
judge holds the verdict.
