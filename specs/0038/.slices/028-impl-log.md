# Step 028 — impl-log (ADD_CONTRACT STORE_XDUO_0005 ExperimentCollectionProxy)

## Progress

- **Read** the cross-repo system prompts (`add_contract_worker.system-md` +
  `arc-runner.system-md`, found under
  `AI-Strategy-Generator/src/ai_strategy_generator/multistep/`, not the repo root
  the task-file line implied), the Berreman project prompt, and the slice spec.
  The `## Operator note` section is empty.
- **Surveyed** the seam precedents: `Scene.SceneProxy` (`Scene.fs`) for the
  ADD_CONTRACT shape (error → name+tryCreate → snapshot → `[<ReferenceEquality>]`
  proxy), `SceneProxyTests.fs` for the inline map-backed mock + mock-driven test,
  and `Experiments.fs` (step 025) for the full-setup `Experiment` (ordered
  `ElementDescriptor` setup + optional `DataFilePath` attachment) the snapshot
  reuses. Confirmed no pre-existing `CollectionName` /
  `ExperimentCollection{Snapshot,Proxy,Error}` type (the only hit is a forward
  reference in an `AppContext.fs` comment).
- **Declared** the seam in `ExperimentCollectionStore.fs` (Domain):
  `ExperimentCollectionError`, `CollectionName` (+`value`/`tryCreate`),
  `ExperimentCollectionSnapshot`, `[<ReferenceEquality>]
  ExperimentCollectionProxy`. Registered it last in the Domain fsproj (after
  `Scene.fs`).
- **Added** the inline mock + mock-driven test in
  `ExperimentCollectionProxyTests.fs` (Tests), registered after
  `SceneRoundTripTests.fs`.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ExperimentCollectionStore.fs`
  — **new**. The experiment-collection persistence seam (types only; DECLARED
  lifecycle — no store body / no `createInMemory`).
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
  — registered `ExperimentCollectionStore.fs` after `Scene.fs`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentCollectionProxyTests.fs`
  — **new**. Inline map-backed mock + the mock-driven test exercising every proxy
  field through its exact signature.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
  — registered `ExperimentCollectionProxyTests.fs` after `SceneRoundTripTests.fs`.

## Testing state

`commit_ready: true`. Per the ADD_CONTRACT **Invariant 6 — act only** (and the
step 024/025/026 precedent in this arc), the worker authors the contract, mock,
and test and runs NO gates; gate execution belongs to the arc-runner's
deterministic gate engine. Static verification only this round:

- **Compile order** verified — `ExperimentCollectionStore.fs` compiles after
  `Experiments.fs` (line 128 in the Domain fsproj), the only module it names
  (`Experiments.Experiment`); appended after `Scene.fs` (last). Sibling-module
  qualified reference (`Experiments.Experiment`) needs no `open` (the `Scene.fs` /
  `Project.fs` precedent).
- Every `match` is exhaustive (no FS0025), `CollectionName` carries
  `.value`/`tryCreate` (blank/null → typed `InvalidCollection` with a non-blank
  reason), the error carries a `reason`, and the proxy is `[<ReferenceEquality>]`.
- The mock-driven test drives `saveCollection`, `tryLoadCollection` (hit + miss),
  and `listCollections` through their EXACT signatures — the acceptance. The
  snapshot fixture carries two step-25 full-setup experiments, the first WITH a
  `DataFilePath` attachment and the second WITHOUT, and the round-trip asserts the
  optional attachment rides along per experiment. `ref Map` mutation matches the
  `SceneProxyTests` / `MaterialStore` idiom.
- LF line endings verified on all four touched files (no CRLF churn).
- Record-inference ambiguity handled: fixtures are return-type-annotated
  (`: Experiment`, `: ExperimentCollectionSnapshot`), and the `VariedElement`
  literal resolves via the `varied` field's expected type (the ExperimentProxyTests
  precedent).

Expected gate roster (run by the engine): `build`, `unit-tests`,
`constructor-unit-tests`, `ui-smoke`, `ui-tests`. Only `ExperimentCollectionStore.fs`
(Domain) and `ExperimentCollectionProxyTests.fs` (Tests) change;
`constructor-unit-tests` gains 6 cases (a 2-case `[<Theory>]` + four `[<Fact>]`),
612 → 618; the other counts are untouched.

## Artifacts

None produced this round (a pure contract-declaration slice — no captured logs,
screenshots, or traces). The per-arc artifacts folder is
`C:\GitHub\Berreman\specs\0038\.artifacts`.

## Gotchas

- **Module name is `ExperimentCollectionStore`, NOT `ExperimentCollection`.** The
  `Experiments` module already declares an `ExperimentCollection` TYPE (the
  editable draft-plus-list collection); a top-level `ExperimentCollection` module
  would shadow / collide with it. `ExperimentCollectionStore` is unambiguous and
  parallels `SampleStore`/`MaterialStore`; the file name matches the module.
- **`saveCollection` takes the snapshot alone**, not `name -> snapshot` like
  `SceneProxy.saveScene`. The slice pins that exact signature and the snapshot
  embeds its own `name : CollectionName`; the mock keys the map off
  `snapshot.name.value`.
- **DECLARED, not implemented.** No disk format, no `.ocproj` wiring, no
  `createInMemory` this round — the real file-backed `create` is a later
  `IMPLEMENT_CONTRACT STORE_XDUO_0005` in `OpticalConstructor.Storage`, which will
  also wire the live `ExperimentCollection` as the app-scope `VersionsInUse` source
  (the `AppContext.fs` §0.2 forward reference).
- **System-prompt path drift.** The task file lists the system prompt at
  `C:\GitHub\AI-Strategy-Generator\add_contract_worker.system-md`, but it actually
  lives at `.../src/ai_strategy_generator/multistep/add_contract_worker.system-md`
  (with its `arc-runner.system-md` base alongside). Read both from the real
  location; no scope impact.
