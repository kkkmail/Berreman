# Step 0249 — state of the world

## Where we are

Spec 0050, section S8.1 ("The run law walks every object; the source is real"), step 249 (IMPLEMENT).

- Steps 244–248 built the item-model evaluator:
  - `ParameterValues` with `checkedFor`;
  - its source arm;
  - its element arm `BoundOpticalElement.muellerAt`, for polarizer presets, the empty slot and the parameterised
    optical forms.
- This step adds the stack samples, solved through Berreman as the table solves them: `resolveSampleMaterials` once,
  then `sampleMuellerSpun` per evaluation.
  - A sample's thickness parameters (step 207) are applied to its layers when it is bound.
  - An unresolved material is a typed `SampleMaterialErr`.
- Step 250 adds the cut plates, and step 251 binds every item-model choice through these constructors.

## What's working

- Added `ParameterValue.thickness` (metres) and `Sample.withThicknessValues`, which applies `film<k>` / `film<k>.layer<j>` / `substrate` thickness parameters to the layers they name, a repeated cell once for every period
- Added `BoundStackSample` and `BoundOpticalElement.ofStackSample`, which resolve a sample's materials once when it is bound; `muellerAt` solves it by `Propagation.sampleMuellerSpun` on both branches at the incidence and spin
- Added `ItemEvaluationError.SampleMaterialErr` (the store's `MaterialError`), rendered with the error's reason
- Kept `BoundOpticalElement`'s value equality by holding the resolved sample in a `[<ReferenceEquality>]` `ResolvedStackSample`
- Added three `ItemEvaluationTests` facts and widened the render fact (constructor tests 1392 → 1395)

## Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster: build, elevation,
constructor-unit-tests over everything discovered, and the fourteen s25 invariants.

The worker's development runs over the final tree, with logs under `specs/0050/.artifacts/0249-*`:

- **build:** exit 0, `0 Warning(s)`, `0 Error(s)`.
- **constructor-unit-tests,** the gate's command: `Passed!`, 1395 succeeded, 0 failed, 0 skipped, in 66 s
  (estimate 180 s).
  - That is checkpoint 248's 1392 plus the three declared facts:
    - `a stack sample at its own thicknesses is solved as the table solves it`
    - `a stack sample's thickness parameters change the layers they name`
    - `a sample whose material does not resolve answers SampleMaterialErr`
  - The widened `every evaluation error renders its condition and its payload` also passes.
  - All four are `Passed` in the TRX under their declared identities.
- **elevation** from the arc origin `f395597`: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, in 196 s
  (estimate 600 s). Known findings: 0.
- **The fourteen s25 invariant greps,** untracked files included: none finds anything.
- **Red, measured by mutation.** Solving every branch as transmitted, and leaving the substrate unread, turned the
  two solve and thickness facts red (2 of 17). The mutation was reverted (`cmp` identical) and everything above was
  re-run.

No run came near a deadline. No test is removed or renamed, and no assertion is removed or weakened.

```yaml
gates:
  constructor_unit_tests: 1395
```

## Architecture

- **A stack sample is one more case of `BoundOpticalElement`.**
  - `ofStackSample` applies the thicknesses (`withThicknessValues`), then resolves the materials once
    (`Propagation.resolveSampleMaterials`).
  - `muellerAt` calls `Propagation.sampleMuellerSpun` as the host's `chartForRun` does.
  - No cache, catch or option is added.
  - The arm is an or-pattern over both branches, so `muellerAt` still has no wildcard.
- **`BoundStackSample` holds a `[<ReferenceEquality>] ResolvedStackSample`, not the bare `ResolvedSample`** (a
  recorded deviation). `ResolvedSample` carries dispersion functions. With the bare payload, F# dropped
  `BoundOpticalElement`'s generated value equality, and step 248's binding fact went red. Step 250's brief uses the
  same wrapper pattern for its `CutPlate`.
- **`withThicknessValues` reads a layer exactly when `parameterAxes` publishes one for it.** Both decide by
  `Thickness.parameterValueOpt`, so the two cannot drift apart.
- **The Result-list traversal is one private module-level `sequenced`,** moved out of `muellerAt` and shared with
  `withThicknessValues`.

## Deferred

Nothing. Every requirement of the step landed in this round. The cut plates and the choice binding are steps 250 and
251's own work, scheduled steps rather than deferrals from this one.

## Gotchas

- **A union case whose payload holds a function costs the whole union its value equality.** Wrap such a payload in
  a `[<ReferenceEquality>]` record.
- **Two bound stack samples are never `=`, even over one sample.** Compare stack samples by their evaluated matrices.
- **`MaterialProxy.createInMemoryEmpty`, which the brief names, does not exist.** The empty in-memory store is
  `MaterialProxy.createInMemory (fun _ -> [])`.
- **`ParameterValue.thickness` round-trips `Thickness.parameterValueOpt` exactly.** A sample bound at its own
  defaults solves bit for bit as the table's.

## Changelog

- 2026-11-10 — Step 0249:
  - added `ParameterValue.thickness`, `Sample.withThicknessValues` and `SampleMaterialErr`;
  - added `BoundStackSample` / `ofStackSample`, solved by `sampleMuellerSpun` on both branches;
  - added three `ItemEvaluationTests` facts (constructor tests 1392 → 1395).
