# Step 0380 — impl plan

## Approach

A long fit needs per-iteration progress and Cancel (spec 0050 S11.3). The Levenberg–Marquardt solve runs whole inside
one ALGLIB call, so the adapter, the one file allowed to name ALGLIB, is where the watch lands.

1. `OptimizationInterface.fs`, after `OptimizationRequest`:
   - `[<ReferenceEquality>] OptimizerIterationObserver = { reportIteration : IterationCount -> unit; cancellation :
     CancellationToken }`, the shape of `KitArchiveObserver`;
   - `IterationWatch = UnwatchedIterations | WatchedIterations of OptimizerIterationObserver`;
   - `SearchCancelled`'s doc and the module doc are reworded as the slice prescribes.
2. `AlglibAdapter.fs`:
   - `CompletionCode.terminationReason` gains `| CompletionCode 8 -> SearchCancelled` before the `c > 0` arm;
   - `runLm (watch : IterationWatch)`: unwatched makes exactly today's calls (no `minlmsetxrep`, a null report);
     watched calls `minlmsetxrep(state, true)` and passes an `ndimensional_rep` that numbers its calls, reports each and
     calls `minlmrequesttermination` once the observer's token is cancelled;
   - `optimizeWatched (watch : IterationWatch)` holds today's `optimize` body, its LM arm calling `runLm watch`;
     `optimize ()` becomes `this.optimizeWatched UnwatchedIterations`.
3. `MuellerInverseSolver.fs`: `type IterationWatch with member this.levenbergMarquardtSolver : NonlinearSolverProxy`
   holds today's `createAlglibLevenbergMarquardt` body with `optimizeWatched this`; the free function becomes
   `UnwatchedIterations.levenbergMarquardtSolver`, its doc kept.
4. Two facts in `OptimizationInterfaceTests`, named exactly as the effects list.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/OptimizationInterface.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/AlglibAdapter.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/MuellerInverseSolver.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OptimizationInterfaceTests.fs`

## Risks

- A closure cannot capture a `let mutable` (FS0407): the ALGLIB state is bound to an immutable local before the report
  closure captures it, and the counter is a `ref` cell local to `runLm`.
- ALGLIB may report the start point as its first call; the slice counts calls, not ALGLIB's iterations, so that only
  changes how many reports there are, never the 1, 2, … numbering.
- The unwatched path must stay bit-identical: `FeasibleBiaxialInverseTests` is the check.
