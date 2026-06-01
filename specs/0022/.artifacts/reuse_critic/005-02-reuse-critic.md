# Reuse critique -- 005.slice-md cycle 1

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` — `OpticalConstructor.{Domain,Storage,Ui,Tests}` and the `Berreman/Berreman` engine core (`Media.fs`, `Fields.fs`, `Solvers.fs`, `MaterialProperties.fs`, `Constants.fs`).
- Files inspected: ~22/200 (cap not reached).
- Extensions: `.fs`, `.json`, `.md`.

## Findings

### F1: Beam-tree navigation primitives live in the UI page, not in the `BeamTree` domain that owns the tree

- **Worker added:** `tryGetNode`, `updateNodeAt`, `removeNodeAt`, `descendantCount`, `walk`, `subtreePaths`, and `solveSubtree` — all generic, pure, immutable traversals over `BeamNode.children` — in `OpticalConstructor.Ui/ConstructionPage.fs:28-87`.
- **Existing helper:** `OpticalConstructor.Domain/BeamTree.fs` is the established home of beam-tree operations: `BeamNode.attach` (`BeamTree.fs:74`), `solve` (`BeamTree.fs:91`), `branchEmField` (`BeamTree.fs:98`), `childIncidentField` (`BeamTree.fs:107`), `routeAndSolve` (`BeamTree.fs:117`). Every existing operation that reads or rebuilds the `BeamNode`/`children` spine sits in that module, next to the types it manipulates.
- **Why it matters:** This is pattern divergence, not duplication of a single symbol — there is no pre-existing `tryGetNode`/`descendantCount`, so nothing is *re-implemented* today. But these are domain operations on the domain aggregate, and placing them in a UI page module means the next consumer that needs tree navigation (slice 006 curved elements, `OpticalConstructor.Optimization`, or the deferred FuncUI view itself) cannot reach them without either depending on `OpticalConstructor.Ui` or re-rolling its own walk/path traversal — which is exactly how reuse erodes. `solveNode` already correctly delegates to `BeamTree.solve`; the path/walk/descendant helpers around it belong beside `solve`, not above it.
- **Suggested action:** Lift the Avalonia-free navigation core (`NodePath`, `tryGetNode`, `updateNodeAt`, `removeNodeAt`, `descendantCount`, `walk`, `subtreePaths`, `solveSubtree`) into `BeamTree.fs` and have `ConstructionPage` consume it, leaving only the MVU `Model`/`Msg`/`update`/presentation helpers in the UI module. Advisory only; the code judge decides, and the architecture critic may also be weighing the same altitude question.

### F2: `StackEditTests` re-rolls the per-module test fixtures (`vacuumSystem`/`light`/node-builder) a third time

- **Worker added:** private `baseSystem` (`StackEditTests.fs:34`), `light` (`StackEditTests.fs:43`), and `mkNode` (`StackEditTests.fs:164`) fixtures.
- **Existing helper:** the same shapes already exist privately in `BeamTreeTests.fs:17,26,28` (`vacuumSystem`/`light`/`node`), `ProjectJsonRoundtripTests.fs:25,34,36` (`vacuumSystem`/`light`/`node`), and `RoundTripTests.fs:22,31` (`vacuumSystem`/`light`). `light = IncidentLightInfo.create (WaveLength.nm 600.0<nm>)` is byte-identical to two of them; `mkNode` is the same `BeamNode` record-builder as `node`.
- **Why it matters:** Near-miss duplication of test scaffolding — a `vacuumSystem`/`light` change (e.g. a new required `BeamNode` field) must now be edited in four modules. The cost is low and the divergence is genuine (`StackEditTests` needs a films-bearing `baseSystem` plus `glass`/`eNorm`/`refEq` that the others don't), so this is a soft finding.
- **Suggested action:** Optionally extract a single internal `OpticalConstructor.Tests.Fixtures` module (`vacuumSystem`, `light`, a `node`/`mkNode` builder, `okOr`) and have all four modules open it. **But note:** the worker *followed* the established precedent here — every prior test module rolls its own private fixtures, so this is consistent, not divergent, and the consolidation is a cross-cutting refactor outside this slice's `Files in scope`. Leave-as-is with the duplication documented is a defensible call.

## Bottom line

The substantive engine-reuse obligations are met cleanly: per-node solving delegates to `BeamTree.solve`/`OpticalSystemSolver` (`ConstructionPage.fs:78`), the medium selectors call `OpticalProperties.fromRefractionIndex` and the slice-004 `resolveMaterial` seam rather than re-deriving tensors (`StackEditor.fs:108,113`), orientation reuses `Layer.rotate`/`OpticalSystem.rotate`, thickness display routes through the sole `Units.fromMeters` seam, and saving reuses `ProjectJson.serializeProject` with no `.binz` path — no forked solver, tensor builder, or serializer. F1 (domain navigation parked in the UI module) is the only finding with real future-reuse weight and is worth a re-spawn-or-fold decision; F2 is low-priority and arguably out of scope. I have no gate authority — the judge decides routing.
