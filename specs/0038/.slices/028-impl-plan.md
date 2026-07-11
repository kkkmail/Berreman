# Step 028 — impl-plan (ADD_CONTRACT STORE_XDUO_0005 ExperimentCollectionProxy)

## Goal

Declare, in `OpticalConstructor.Domain`, the experiment-collection persistence
seam at **DECLARED** lifecycle: the elevated `CollectionName` key, the pure
`ExperimentCollectionSnapshot` over the step-25 full-setup `Experiment` list
(each experiment carrying its optional `DataFilePath` attachment), the
`ExperimentCollectionError` channel, and the `[<ReferenceEquality>]`
`ExperimentCollectionProxy` functional-proxy record. Supply an inline mock and a
mock-driven test exercising all three proxy fields through their exact
signatures. NO real disk-backed store this round — that is the later
`IMPLEMENT_CONTRACT` cycle.

## Approach

This is the direct analogue of step 026 (ADD_CONTRACT STORE_XDUO_0004,
`SceneProxy` in `Scene.fs`). Mirror that file's shape:

- error channel declared first (`ExperimentCollectionError = InvalidCollection of reason`),
- `CollectionName` single-case DU with `.value` + blank-rejecting `tryCreate`
  returning the typed `InvalidCollection`,
- `ExperimentCollectionSnapshot = { name : CollectionName; experiments : Experiment list }`
  reusing the step-25 `Experiments.Experiment` (which already carries `dataFileOpt`),
- `[<ReferenceEquality>] ExperimentCollectionProxy` with the three exact fields.

`saveCollection` takes the snapshot directly (the snapshot embeds its own
`name`), unlike `SceneProxy.saveScene` which takes name + snapshot separately —
follow the signature the slice pins verbatim.

## Files

- **NEW** `Berreman/OpticalConstructor/OpticalConstructor.Domain/ExperimentCollectionStore.fs`
  — the `ExperimentCollectionStore` module (distinct from the existing
  `Experiments.ExperimentCollection` *type*). Registered in the Domain `.fsproj`
  AFTER `Experiments.fs` (it references `Experiments.Experiment`) — appended
  after `Scene.fs`.
- **EDIT** `OpticalConstructor.Domain.fsproj` — add the `<Compile Include>` entry.
- **NEW** `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentCollectionProxyTests.fs`
  — inline map-backed mock + mock-driven test (mirrors `SceneProxyTests.fs`).
- **EDIT** `OpticalConstructor.Tests.fsproj` — add the `<Compile Include>` entry
  after `SceneRoundTripTests.fs`.

## Risks

- Record-field ambiguity: `VariedElement` shares `elementId`/`variable` labels
  with `ExperimentDraft`. Inside an `Experiment` literal the `varied` field's
  expected type disambiguates (the ExperimentProxyTests precedent), so no
  qualification is needed there.
- Module vs. type name clash: the module is `ExperimentCollectionStore`, NOT
  `ExperimentCollection`, so it never collides with the existing
  `Experiments.ExperimentCollection` type.
- LF line endings; zero warnings; declared-only (no `createInMemory`).
