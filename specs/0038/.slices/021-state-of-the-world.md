# State of the world — Step 021 (Spec 0038 Part H, AC-H2)

## Where we are

Step 021 lands the second piece of Part H (Lifecycle): the registered materials
write-seam `MaterialProxy` (STORE_XDUO_0001) is re-typed IN PLACE into a
**versioned** in-memory store. It builds directly on step 020's shared
`decideVersioning` rule and the `VersionsInUse` seam. The material store now keeps
a per-`MaterialId` version history, offers a latest-active default listing with an
explicit include-inactive switch, resolves any exact version ignoring lifecycle,
saves through the shared decision rule, exposes the active/inactive/supersede
lifecycle verbs, and un-deletably hard-blocks both sample-referenced and
version-used materials. Step 022 mirrors this for the sample store; step 25 wires
the real `VersionsInUse` over live experiment descriptors.

## What's working

- Re-type `MaterialProxy` in place into the versioned surface: `listMaterials`
  takes an `InactiveVisibility` switch, `resolveVersion` serves reference
  resolution ignoring lifecycle, `saveMaterial` applies `decideVersioning`, and
  `markMaterialInactive` / `markMaterialActive` / `supersedeMaterial` retire and
  revive the latest version.
- Add the versioned store `MaterialStore.createInMemory` (history per material,
  seeded active at version 1; mutate-in-place / mint-next / keep per the shared
  rule; latest-only editing with older versions view-only).
- Block removal of a used material with a new typed `MaterialVersionInUse`
  refusal, alongside the retained `MaterialStillReferenced` sample-reference block.
- Update every consumer mechanically: composition roots inject the seam
  (`VersionsInUse.empty` until step 25), editors/windows/facets list latest-active,
  the editor's save collapses to one `saveMaterial`.
- Rewrite `MaterialProxyTests` (23 facts) over the versioned surface: the full
  decision table on a stubbed seam, latest-active listing, inactive-hidden-yet-
  resolvable, supersede-as-inactive, and both removal blocks.

## Tests

Gate execution remains the arc-runner's (IMPLEMENT Invariant 6 — the worker acts;
the deterministic gate engine is the sole authority). Locally confirmed to avoid
wasting the single supervisor retry, not self-reported as authoritative:

- `build`: 0 errors; no new warnings (only the two pre-existing exempt ones —
  `SYSLIB0051`, `FS3873`).
- `unit-tests` (BerremanTests): 119 passed (core solver untouched).
- `constructor-unit-tests`: 588 passed — a strict addition over the 586 baseline
  (the rewritten `MaterialProxyTests` net +2 facts; no existing test dropped).
- `ui-smoke`: 153 passed. `ui-tests`: 414 passed. Both at baseline.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 588
  ui_smoke_tests: 153
  ui_tests: 414
```

## Architecture

- **The record stays in place; the store moved by compile-order necessity.** The
  spec pins `MaterialProxy` at `MaterialLibrary.fs:506` and demands re-typing in
  place. The record's new surface only needs `MaterialVersionId` + the listing
  switch, so those (plus `VersionNumber`, which `MaterialVersionId` needs) moved
  from `Lifecycle.fs` into `MaterialLibrary.fs` right before the record — the
  record is genuinely re-typed in place. The STORE body (`createInMemory`) needs
  the full `decideVersioning` foundation from `Lifecycle.fs` (compiled AFTER
  `ElementId.fs`, which it needs for `SampleId`), so it moved out of `ElementId.fs`
  into a new `MaterialStore.fs` after `Lifecycle.fs`. This is the same
  compile-order split that already put `createInMemory` in a different file than
  its record; no shim, no parallel type, no compatibility overload.
- **One rule, applied by the store.** `saveMaterial` reads the latest version's
  usage through the injected seam and calls the ONE shared `decideVersioning`;
  physics is the `(=)`-comparable `MaterialComplexity option` (the engine
  `properties` carries function cases and cannot be compared, and equal complexity
  ⇒ equal properties for editable entries), metadata is name/category/description.
- **Supersession is structural.** `listMaterials` surfaces only the latest version
  per material, so a minted-over (non-latest) version is automatically hidden from
  offers yet resolvable by `resolveVersion` — "superseded behaves as inactive
  automatically" with no extra state and no third `EntryLifecycle` case.
- **Elevated primitives.** The listing switch is the two-case `InactiveVisibility`
  (never a bool); the used-version refusal is the typed `MaterialVersionInUse`
  carrying a reason.

## Deferred

- Wiring the material store into step 022's sample-store analogue (it will face the
  same compile-order split and can reuse `VersionNumber` / `InactiveVisibility` /
  `VersionsInUse.empty`).
- The real `VersionsInUse` over the in-memory experiment store's descriptors,
  injected at composition in place of `VersionsInUse.empty` (step 25).
- The Materials/Library window show-inactive TOGGLE that flips `listMaterials` to
  `IncludeInactive` (a later UI step — every consumer defaults to `ActiveOnly` now).
- A dedicated editor / distinct store representation for supersede vs mark-inactive
  (see Gotchas): the two share `InactiveEntry` at this in-memory step.

## Gotchas

- **`supersedeMaterial` and `markMaterialInactive` share a representation here.**
  Step 020's `EntryLifecycle` doc defines `InactiveEntry` as "superseded /
  soft-deleted", and there is no replacement-entry linkage or persistence yet, so
  both verbs retire the latest version to `InactiveEntry` through one shared helper.
  They are kept as DISTINCT proxy fields so the UI's "Supersede" and "Mark inactive"
  intents bind separately and a future step can diverge them (e.g. make supersede
  irreversible) without a signature change. This is the interpretation chosen per
  the "pick a sensible default and record it" rule.
- **`VersionNumber` / `MaterialVersionId` now live in `MaterialLibrary.fs`, not
  `Lifecycle.fs`.** They had to precede the in-place `MaterialProxy` record.
  `Lifecycle.fs` still owns everything else and reaches them through its existing
  `open …MaterialLibrary`; `LifecycleTests` opens both modules, so its unqualified
  references were unaffected. The principle going forward: a version-id type lives
  with its identity type (`MaterialVersionId` by `MaterialId`), which also unblocks
  step 022's `SampleVersionId` near `SampleId`.
- **`saveMaterial` replaced `addMaterial` + `updateMaterial`.** A new id inserts
  version 1; an existing id runs the decision rule. The editor's freshness split
  collapsed to one call. Any future stub of `MaterialProxy` must supply the full
  new field set (`resolveVersion`, `saveMaterial`, the three lifecycle verbs) —
  the old two write fields are gone (no compatibility overload, per §0.2).
- **Physics equality is over `MaterialComplexity`, deliberately not `properties`.**
  `properties : OpticalPropertiesWithDisp` carries function cases (`EpsWithDisp`
  etc.) and does not support `(=)`; a coded preset (`complexity = None`) therefore
  never mints (its physics cannot change through editing), which is correct.
- **`materialsReferencingCategory` lists `IncludeInactive`.** A category is held
  even by a material whose latest version is retired, so removing it would
  otherwise orphan the inactive entry.

## Changelog

- 2026-07-11 — Step 021 (Part H, AC-H2): re-type `MaterialProxy` in place into the
  versioned store (listMaterials scope, resolveVersion, saveMaterial via the shared
  `decideVersioning`, active/inactive/supersede verbs, the used-version removal
  block); move `VersionNumber`/`MaterialVersionId` to `MaterialLibrary.fs` and the
  store body to a new `MaterialStore.fs`; add `VersionsInUse.empty`; update every
  consumer; rewrite `MaterialProxyTests` (23 facts). Build 0 errors; unit 119,
  constructor 588, ui-smoke 153, ui-tests 414 — all green locally, no regressions.
