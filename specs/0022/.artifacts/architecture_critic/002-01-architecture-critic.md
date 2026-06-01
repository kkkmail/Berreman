# Architecture critique -- 002.slice-md cycle 1

## Summary

Clean, spec-faithful slice with two notable design smells worth the judge's
attention. Every cited engine seam (`OpticalSystemSolver` `Solvers.fs:199`,
`BaseOpticalSystemSolver(emf, system)` `:149`, `EmField.propagate`
`BerremanMatrix.fs:224`, `WaveLength.value` `Fields.fs:284`, the `Constants.fs`
factors) resolves exactly as written, reuse-over-invention is honoured, and the
mirror/branch and SI invariants are proven by test. The single most important
finding is the **dual source of truth on a `Sample` node** (`element` payload
vs `system` field), which the worker itself flagged in Gotchas but left
unresolved for slice 003 to trip over.

## Layering

No violations. `OpticalConstructor.Domain` depends only on the engine
(`Berreman`/`Analytics`); nothing reverses the arrow. Compile order in the
`.fsproj` (`Units.fs` → `BeamTree.fs` → `Project.fs`) matches the dependency
direction: `BeamTree.BeamNode.defaultUnit` consumes `Units.UnitOfMeasure`, and
`Project.OpticalConstructorProject` consumes `BeamTree.BeamTree`. The beam-tree
aggregate sits strictly above the solver and *calls* it (`solve`,
`routeAndSolve`) rather than re-typing `BaseOpticalSystemSolver` — exactly the
"orchestration must not embed solver internals" rule of R-2.

## Separation of concerns

`BeamTree.fs` carries four concerns (topology DUs, single-node solve, branch
routing, gradient discretization), but R-10/R-11/R-12/R-14 each explicitly name
`OpticalConstructor.Domain/BeamTree.fs` as the home, so this is spec-directed,
not a worker choice. `Units.fs` is correctly the sole conversion seam. No
finding.

## Consistency

- The placeholders were `module OpticalConstructor.Domain.X`; the worker
  rewrote all three files to `namespace OpticalConstructor.Domain` + nested
  `module X`. This is applied uniformly across `Units`/`BeamTree`/`Project` and
  the tests open `OpticalConstructor.Domain.Units`/`.BeamTree` accordingly —
  internally consistent and idiomatic F#.
- Mirror validation uses `Result<BeamNode, BeamTreeError>`, not exceptions, per
  R-11. Good. By contrast `discretize` rejects `subLayerCount < 1` with
  `invalidArg` (an exception). This is a reasonable interim for an
  argument-precondition (it is idiomatic F#, and the SoW Deferred section names
  the Part J §J.9 `Result` validation seam as the eventual home), but it is a
  second, divergent validation idiom in the same module — worth the judge noting
  the inconsistency is acknowledged and intentional.
- **`1239.84` once-rule (R-7).** Production has exactly one code literal, the
  `evNmProduct` binding (`Units.fs:31`); the rest are doc-comments. `UnitsTests.fs:22`
  re-introduces the literal in `(1239.84 / 13.5)`. If R-7 is read solution-wide
  this is a duplicate; however an independent literal in the test is defensible —
  binding the test to `evNmProduct` would make it tautological and unable to
  catch a typo in the constant. I read the production constraint as satisfied;
  flagging only so the judge weighs the scope reading.

## Spec fit

- AC-A3/AC-B4/AC-A5/AC-A7/AC-D1/AC-D2/AC-B8/AC-B11 each have a matching test and
  the seams are reused as the ACs demand. `discretize` samples `indexProfile` at
  `(i + 0.5) * dz` mid-depth and sums to `totalThickness` — correct.
- **Seam vs. orchestration gap (under-delivery to watch).** `attach` populates
  `BeamNode.children`, but no function in `BeamTree.fs` ever *traverses*
  `children` to drive a multi-node solve. `solve` evaluates one node;
  `routeAndSolve` solves one child from loose `EmFieldSystem`/`Layer`/`ShortOpticalSystem`
  values, not from a parent node's `children` map. The only tree-walk lives in
  `BeamTreeTests`. R-2 says "the orchestration MUST evaluate each node," and a
  recursive solve over the tree is the obvious connective tissue. This is a
  defensible reading — the slice's ACs target seam existence/reuse, and
  live-recalculation is Part F — but the judge should confirm the missing
  whole-tree evaluator is intentional deferral, not an omission, since SoW
  Deferred does not mention it.

## Evolvability

- **Dual source of truth on `Sample` (highest-value finding).** `BeamNode` holds
  both `element : ConstructorElement` (where `Sample of OpticalSystem`) and a
  separate `system : OpticalSystem`. For a `Sample` node the same stack is stored
  twice and can diverge; `solve` reads `node.system` and silently ignores the
  `OpticalSystem` inside the `Sample` payload. SoW Gotchas calls `system`
  "authoritative," but nothing enforces that, and slice 003's serializer will
  have to either double-persist the stack or pick one and drop the other —
  exactly the kind of locked-in shape the Risks section of the spec warns about.
  The spec does mandate both phrasings (§A.4 "BeamNode carries its OpticalSystem"
  and `Sample (carrying an OpticalSystem)`), so this is not a spec violation, but
  the smallest hardening — make `ConstructorElement.Sample` carry no payload and
  let `system` be the sole stack, or drop `system` and read the stack from the
  `Sample` case — would remove the divergence before slice 003 cements it in
  JSON.
- `GradientLayer.indexProfile` is a function field (unserializable), but it is
  correctly never reachable from the `OpticalConstructorProject` aggregate (only
  expanded `films` persist, per B.9), so JSON is unaffected. No action.

## Risks

- **Case-name collision relying on shadowing.** `open Berreman.Fields` brings
  `RT.Reflected`/`Transmitted` (`Fields.fs:61-63`) into the same scope as the
  locally-defined `BeamBranch.Reflected`/`Transmitted`. `branchEmField` matches
  these cases *unqualified*; it resolves to `BeamBranch` today (build green,
  tests pass), but the resolution depends on shadowing order. Since both DUs
  share identical case names, a future reorder of `open`s or a refactor could
  silently rebind to `RT` without a compiler error. Qualifying as
  `BeamBranch.Reflected`/`.Transmitted` inside `branchEmField` (the tests already
  do this) would make the binding robust.
- `fromMeters`/`toMeters` for `ElectronVolt`/`Wavenumber` divide by the input and
  produce `infinity` at `x = 0` rather than erroring. Physically wavelengths are
  never zero, so low priority, but these are advertised as total conversions and
  silently yield non-finite values on degenerate input.

## Bottom line

I would ship this with a non-blocking note. The slice is correct, reuse-disciplined,
and the contractful invariants (mirror reflected-only via `Result`, SI storage,
propagate-driven routing, n-slice discretization) are all proven by passing
tests against real engine seams. The two items I'd want the worker to weigh
before the shape hardens — the `Sample` `element`/`system` duplication and the
absence of a tree-walking evaluator — are evolvability concerns that get more
expensive to fix once slice 003 serializes the aggregate, not defects in what
this slice delivers. The judge has gate-green plus these suggestions; none rises
to a re-spawn trigger on its own.
