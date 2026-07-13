# Step 022 — IMPLEMENT — impl-plan

## Goal

Mirror step 021 (the versioned `MaterialProxy`) onto the registered `SampleProxy`
write-seam (STORE_XDUO_0002), IN PLACE, AND re-type the material references a
sample carries so they point at a **version** (spec-md Part H).

## Two pieces

### A. Version the sample store (mirror step 021)

- `SampleProxy` record re-typed IN PLACE (`ElementId.fs`) into the versioned
  surface: `listSamples : InactiveVisibility -> …`, `resolveVersion :
  SampleVersionId -> …` (by-version, ignoring lifecycle), `saveSample` through the
  SAME shared `decideVersioning` rule + `VersionsInUse`, `markSampleInactive` /
  `markSampleActive` / `supersedeSample`, `removeSample` hard-block extended to
  used versions (`SampleVersionInUse`). `addSample`/`updateSample` collapse into
  `saveSample` (no compatibility overload — the step-021 precedent).
- Move `SampleVersionId` from `Lifecycle.fs` into `ElementId.fs`'s `Library`
  module next to `SampleId` (so the in-place record can name it) — the exact
  move step 021 made for `MaterialVersionId` (recorded there as the principle
  "a version-id type lives with its identity type"). `Lifecycle.fs` keeps it in
  scope through its existing `open …Library`.
- The store body `SampleProxy.createInMemory` moves out of `ElementId.fs` into a
  new `SampleStore.fs` (after `Lifecycle.fs`/`MaterialStore.fs`) because it now
  applies `decideVersioning` and takes the `VersionsInUse` seam. It takes ONLY
  `versionsInUse` (no `samplesReferencing`-analogue — nothing structural
  references a sample; a sample is "used" only when a live experiment binds a
  version, the `VersionsInUse` seam).

### B. Sample references → material VERSIONS

- `SampleLayer.materialId : MaterialId` → `MaterialVersionId`; `SampleStructure.lower
  : MaterialId option` → `MaterialVersionId option` (every material reference a
  sample carries points at a version, spec-md Part H).
- Seeds pin version one via a new `MaterialVersionId.firstOf : MaterialId ->
  MaterialVersionId` helper (`MaterialLibrary.fs`).
- Resolution flows through step-021 by-version resolve: `Propagation.resolveSampleMaterials`
  takes a `MaterialProxy` and resolves each pinned `MaterialVersionId` through
  `resolveVersion` (new `resolveMaterialVersion` helper) — a pinned layer keeps
  resolving its version after the material mints a newer one.
- `referencedMaterials` keeps returning `Set<MaterialId>` (removal block is per
  identity), extracting `.materialId` from each version.
- Update consumers mechanically: `SampleStackEditor` (`SetMaterialOfSelected` /
  `SetLower` carry `MaterialVersionId`; `SelectByMaterial` stays `MaterialId`,
  compared by identity), the sample editor (pin chosen material at v1 via
  `firstOf`), the facet extractors (`LibraryFacets`), the workbench band view.

## Files

Domain: `Lifecycle.fs`, `MaterialLibrary.fs`, `ElementId.fs`, new `SampleStore.fs`,
`Propagation.fs`, `SampleStackEditor.fs`, `LibraryFacets.fs`, the Domain `.fsproj`.
Ui: `AppContext.fs`, `TableAndElementRotationView.fs`, `SampleEditorView.fs`,
`LibraryWindowView.fs`. Domain tests: `SampleProxyTests.fs` (full rewrite mirroring
`MaterialProxyTests` + the version-pin round-trip), `SampleStackEditorTests.fs`,
`PropagationTests.fs`, `LibraryFacetsTests.fs`. Ui tests: every
`SampleProxy.createInMemory ()` / `.listSamples ()` call site + the sample-editor
stubs and Save acceptance assertions.

## Risks

- Compile-order: `SampleVersionId` must precede the in-place `SampleProxy` record
  (move to `ElementId.fs`); the store body must follow `Lifecycle.fs` (new file).
- `saveSample` replacing `addSample`/`updateSample` ripples into the editor Save
  and its stubs/assertions.
- Editor pins v1 (`firstOf`): correct at this step (materials never advance past
  v1 — mints need a used version; `VersionsInUse` is empty until step 25). Recorded
  as an interpretation; step 25 revisits pin-latest.
