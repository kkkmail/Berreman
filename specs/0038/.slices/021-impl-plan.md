# Step 021 — IMPLEMENT — impl-plan

## Slice

Spec 0038 Part H (AC-H2, the material store). Re-shape the registered
`MaterialProxy` (STORE_XDUO_0001) IN PLACE into a **versioned** in-memory store:
versions per `MaterialId`, latest-active default listing with an include-inactive
switch, by-`MaterialVersionId` resolve that ignores lifecycle, a `saveMaterial`
verb driven by the step-20 shared `decideVersioning` rule against an injected
`VersionsInUse`, lifecycle verbs (`markMaterialInactive` / `markMaterialActive` /
`supersedeMaterial`), and a `removeMaterial` that keeps the `MaterialStillReferenced`
hard-block AND additionally refuses any material carrying a used version. Update
every consumer mechanically; keep the whole solution building and every gate's
tests green.

## Compile-order constraint (the load-bearing decision)

`MaterialProxy`'s new surface must reference `Lifecycle` types, but `MaterialLibrary.fs`
(where the record lives, :506) compiles BEFORE `ElementId.fs` (where `SampleId` lives)
and BEFORE `Lifecycle.fs` (which needs `SampleId`). The record only needs
`MaterialVersionId` (for `resolveVersion`) and a listing switch; the store body
(`createInMemory`) needs the full `Lifecycle` surface plus `Sample`. So:

- **Keep the `MaterialProxy` record IN PLACE at `MaterialLibrary.fs:506`** — the spec's
  literal requirement. To let it type-check there, move the two version-id primitives
  `VersionNumber` and `MaterialVersionId` from `Lifecycle.fs` into `MaterialLibrary.fs`
  (right before the record — `MaterialVersionId` only needs `MaterialId`, already
  defined at :27), and add the new listing switch there too. `Lifecycle.fs` keeps
  everything else and references the moved types through its existing
  `open …MaterialLibrary`. `LifecycleTests` opens both modules, so its unqualified
  references still resolve — no test churn there.
- **Move `MaterialProxy.createInMemory` out of `ElementId.fs` into a new
  `MaterialStore.fs` after `Lifecycle.fs`** (it needs `decideVersioning`/`VersionRef`/
  `VersionsInUse`). `createInMemory` was already a type augmentation living in a
  different file than its record (`ElementId.fs`), so this is a relocation of an
  already-separate augmentation, not a shim. Consumers that call it add
  `open …MaterialStore`.
- `materialsReferencingCategory` stays in `MaterialLibrary.fs` (it references the
  record at :506 and the switch, both local).

## New / changed proxy surface

```
listMaterials       : InactiveVisibility -> Result<MaterialEntry list, MaterialError>   // latest per material, scoped
searchMaterials     : MaterialQuery      -> Result<MaterialEntry list, MaterialError>    // over latest-ACTIVE (offers)
tryGetMaterial      : MaterialId         -> Result<MaterialEntry option, MaterialError>  // latest version's entry
resolveVersion      : MaterialVersionId  -> Result<MaterialEntry option, MaterialError>  // exact version, IGNORES lifecycle
saveMaterial        : MaterialEntry      -> Result<unit, MaterialError>                   // decideVersioning: mutate / mint / keep
markMaterialInactive: MaterialId         -> Result<unit, MaterialError>
markMaterialActive  : MaterialId         -> Result<unit, MaterialError>
supersedeMaterial   : MaterialId         -> Result<unit, MaterialError>
removeMaterial      : MaterialId         -> Result<unit, MaterialError>                   // sample-ref block + used-version block
```

- `InactiveVisibility = ActiveOnly | IncludeInactive` (new, MaterialLibrary.fs).
- `MaterialError` gains `MaterialVersionInUse of reason : string` (the used-version block).
- Store representation: `ref Map<MaterialId, VersionRecord list>`, versions ascending,
  non-empty; latest = highest version. Seeded from `builtInEntries` as version 1 active.
- Physics = `MaterialComplexity option` (the only `(=)`-comparable physics view; `properties`
  carries functions); metadata = `(name, category, description)`. `saveMaterial` on a new id
  inserts version 1 active; on an existing id runs `decideVersioning` over
  (latest payload, incoming, usage-of-latest) and applies mutate / mint-next / keep.
- Supersession is automatic (a non-latest version never surfaces in `listMaterials`, only
  the latest does; older versions stay resolvable by `resolveVersion`). `supersedeMaterial`
  and `markMaterialInactive` both retire the latest version to `InactiveEntry` (step 020
  defines `InactiveEntry` as "superseded / soft-deleted"); they are distinct proxy entry
  points for distinct UI intents.

## Consumers (mechanical)

- `listMaterials ()` → `listMaterials ActiveOnly` (offers/facets/editors) or
  `IncludeInactive` (`materialsReferencingCategory`, so no referencing material is missed).
- `addMaterial`/`updateMaterial` → `saveMaterial` (MaterialEditorView).
- Every `MaterialProxy.createInMemory (samplesReferencing s)` call → add the injected
  `VersionsInUse` (empty at composition/most tests via `VersionsInUse.empty`), add
  `open …MaterialStore` / `open …Lifecycle` where needed.
- Stub `MaterialProxy` record literals (in Domain + Ui tests) → the new field set.

## Files to modify

- Domain: `MaterialLibrary.fs`, `Lifecycle.fs`, `ElementId.fs`, new `MaterialStore.fs`,
  `OpticalConstructor.Domain.fsproj`.
- Domain tests: `MaterialProxyTests.fs` (full rewrite for the versioned surface),
  `CategoryProxyTests.fs` (save/createInMemory arity), `OpticalConstructor.Tests.fsproj`
  (no new files, but confirm).
- Ui: `AppContext.fs`, `TableAndElementRotationView.fs` (DefaultStores),
  `MaterialEditorView.fs`, `MaterialsWindowView.fs`, `SampleEditorView.fs`,
  `SampleEditorWindow.fs`, `LibraryWindowView.fs`.
- Ui tests: `MaterialEditorWindowTests.fs`, `MaterialsWindowTests.fs`,
  `SampleEditorWindowTests.fs`, `WindowLauncherTests.fs`, `EmbeddedChartTests.fs`,
  `MainWorkbenchTests.fs`, `LibraryWindowTests.fs`, `AppContextTests.fs`.

## Risks

- `MaterialComplexity` structural `(=)` — its constituents are value trees (records/DUs of
  primitives/Complex/arrays/lists), no function fields, so equality both compiles and is
  meaningful; `properties` is NOT compared (it carries function cases).
- `--warnaserror+:25` — exhaustive matches only.
- Baselines must not regress (constructor-unit-tests 586, ui-tests 414, ui-smoke 153).
- Supersede vs mark-inactive representation overlap — documented in Gotchas.
