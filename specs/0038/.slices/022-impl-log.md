# Step 022 — IMPLEMENT — impl-log

## Progress

- [x] Domain: move `SampleVersionId` into `ElementId.fs` (Library); trim from `Lifecycle.fs`.
- [x] Domain: add `MaterialVersionId.firstOf` (`MaterialLibrary.fs`).
- [x] Domain: re-type `SampleLayer.materialId` / `SampleStructure.lower` to versions; seeds pin v1.
- [x] Domain: `SampleError` + `SampleVersionInUse`; re-type `SampleProxy` record (versioned surface).
- [x] Domain: new `SampleStore.fs` (versioned `SampleProxy.createInMemory versionsInUse`); register in fsproj.
- [x] Domain: `Propagation.resolveSampleMaterials` via by-version resolve; `SampleStackEditor` message re-types; `LibraryFacets` reads.
- [x] Ui: `AppContext` / `DefaultStores` wiring; `SampleEditorView` (pin v1, saveSample, error case); `LibraryWindowView` (ActiveOnly, error case); band view.
- [x] Domain project + Ui project + App project all build (0 errors, no new warnings).
- [x] Domain tests: rewrite `SampleProxyTests` (versioned surface + version-pin round-trip); fix `SampleStackEditorTests`, `PropagationTests`, `LibraryFacetsTests`, `LibraryProxyTests`, `MaterialProxyTests`.
- [x] Ui tests: `createInMemory VersionsInUse.empty`, `listSamples ActiveOnly`, sample-editor stubs + Save assertions.
- [x] Full solution builds; all four test projects run green locally.

## Files modified

Domain:
- `ElementId.fs` — `SampleVersionId` moved in (from `Lifecycle.fs`, next to `SampleId`);
  `SampleLayer.materialId` → `MaterialVersionId`; `SampleStructure.lower` →
  `MaterialVersionId option`; `referencedMaterials` projects each version to its `MaterialId`;
  `SampleError` + `SampleVersionInUse`; `SampleProxy` record re-typed (versioned surface);
  `filmStructure`/`plateStructure` + every inline seed pin v1 via `MaterialVersionId.firstOf`;
  old `SampleProxy.createInMemory` removed (→ `SampleStore.fs`); `validateSample` made public;
  `samplesReferencing` lists `ActiveOnly`.
- `MaterialLibrary.fs` — `MaterialVersionId.firstOf` (the shared "pin version one" helper).
- `Lifecycle.fs` — `SampleVersionId` removed (moved to `ElementId.fs`); comment updated.
- `SampleStore.fs` — NEW: the versioned `SampleProxy.createInMemory versionsInUse` (mirror of
  `MaterialStore.fs`; physics = `SampleStructure`, metadata = name/description/substrate kind;
  used-version removal block only).
- `Propagation.fs` — `resolveMaterialVersion`; `resolveSampleMaterials` now takes a `MaterialProxy`
  and resolves each pinned `MaterialVersionId` through `resolveVersion`.
- `SampleStackEditor.fs` — `SetMaterialOfSelected`/`SetLower` carry `MaterialVersionId`;
  `SelectByMaterial` stays `MaterialId` (per identity); `positionsOfMaterial` compares `.materialId.materialId`.
- `LibraryFacets.fs` — the two `materialName … layer.materialId` reads project to `.materialId.materialId`.
- `OpticalConstructor.Domain.fsproj` — register `SampleStore.fs` after `MaterialStore.fs`.

Ui:
- `AppContext.fs`, `TableAndElementRotationView.fs` (DefaultStores) — `SampleProxy.createInMemory
  VersionsInUse.empty` + `open …SampleStore`; the workbench resolution now passes the LIVE
  `model.materials`; band specs project `.materialId.materialId`.
- `SampleEditorView.fs` — pin the chosen material at v1 (`firstOf`) on Add/Set/Substrate/Lower;
  Save collapses to one `saveSample`; `sampleErrorReason` + `SampleVersionInUse`; layer reads
  project `.materialId.materialId`.
- `LibraryWindowView.fs` — `listSamples ActiveOnly`; `SampleVersionInUse` in the refusal map.

Domain tests:
- `SampleProxyTests.fs` — full rewrite (22 facts) over the versioned surface + the version-pin
  round-trip.
- `SampleStackEditorTests.fs`, `LibraryFacetsTests.fs`, `LibraryProxyTests.fs` — `layerOf`/layer
  helpers pin v1; assertions read `.materialId.materialId`; `SetMaterialOfSelected` wraps `firstOf`.
- `PropagationTests.fs` — `standardProxy` (versioned material store); `resolveSampleMaterials`
  takes it; layer construction pins v1.
- `MaterialProxyTests.fs` — `composedProxies` uses `SampleProxy.createInMemory VersionsInUse.empty`;
  `listSamples ActiveOnly`.

Ui tests:
- `SampleProxy.createInMemory VersionsInUse.empty` + `open …SampleStore` and `.listSamples ActiveOnly`
  across `AppContextTests`, `EmbeddedChartTests`, `LibraryWindowTests`, `MainWorkbenchTests`,
  `MaterialEditorWindowTests`, `MaterialsWindowTests`, `WindowLauncherTests`, `SampleEditorWindowTests`.
- `SampleEditorWindowTests.fs` — both `SampleProxy` stubs re-typed to the versioned surface;
  Save assertions `add:`/`update:` → `save:`; layer helpers pin v1; reads project `.materialId.materialId`.

## Testing state

Gate execution is the arc-runner's (IMPLEMENT Invariant 6 — the worker acts; the gate engine is
the sole authority). These are LOCAL confirmations run to avoid wasting the single supervisor retry.

- `build`: `dotnet build Berreman.slnx -c Release` → **0 errors**. The only warnings are the two
  pre-existing exempt ones (`SYSLIB0051` in MathNet, `FS3873` in `Berreman/Dispersion.fs`) plus the
  pre-existing `FS1125` in `OpticalConstructor.Tests/SeriesDataTests.fs` — a file this slice does
  NOT touch (confirmed via `git diff --name-only`). Slice 022 introduces none.
- `unit-tests` (BerremanTests): **119 passed**, 0 failed (baseline 119; core solver untouched).
- `constructor-unit-tests`: **589 passed**, 0 failed (baseline 588 → strict +1; the rewritten
  `SampleProxyTests` net +1 fact, no existing test dropped).
- `ui-smoke`: **153 passed**, 0 failed (baseline 153).
- `ui-tests`: the full `Ui.Tests` project runs **567 passed**, 0 failed (= 153 ui-smoke + 414
  ui-tests; both at baseline — no Ui test added or removed).

All modified `.fs`/`.fsproj` files verified LF-clean (`git diff --ignore-cr-at-eol` shows no
CR-only churn; the new `SampleStore.fs` is LF).

## Artifacts

No captured logs needed beyond the console runs above; the `.artifacts/` folder is available.

## Gotchas

- **`saveSample` replaced `addSample` + `updateSample`** (no compatibility overload, §0.2 — the
  step-021 precedent). Any future stub of `SampleProxy` must supply the full new field set
  (`resolveVersion`, `saveSample`, the three lifecycle verbs, `listSamples : InactiveVisibility -> …`);
  the two old write fields are gone. The editor's freshness split collapsed to one call.
- **Sample physics = `SampleStructure`; metadata = name / description / substrate kind.** Unlike a
  material's engine `properties` (function cases, not `(=)`-comparable), `SampleStructure` is fully
  structurally comparable, so the store passes `(=) (=)` to `decideVersioning` directly. The
  `SubstrateKind` (ThinFilm/Plate/Wedge) is treated as METADATA: it is a display/search facet the
  engine mapping (`resolveSampleMaterials`/`sampleToSystem`) never reads, so it never versions.
- **`SampleStructure.lower` was versioned too**, not only the film/substrate `SampleLayer`s. The
  task calls out "substrate/film material ids", but the lower half-space is also a material
  reference a sample carries, and Part H says references point at a version — versioning all three
  keeps `referencedMaterials`/resolution uniform. Recorded interpretation.
- **The editor pins version one (`MaterialVersionId.firstOf`).** At this step every material lives
  at v1 (an unused version's edit mutates in place; a mint needs a USED version, and `VersionsInUse`
  is `empty` until step 25), so `firstOf` IS the current latest version. Centralised in one helper
  (the seeds and the editor both pin through it) so step 25's real `VersionsInUse` has ONE place to
  revisit pin-latest. `MaterialEntry` carries no version number, so the view cannot pin latest
  through the proxy surface today anyway.
- **Resolution moved to the LIVE store.** `resolveSampleMaterials` now takes a `MaterialProxy` and
  resolves each pinned version through `resolveVersion` (the round-trip proof). The workbench call
  site now passes `model.materials` (the live versioned store) instead of the former static
  `MaterialLibrary.standard`; the `materialLibrary` binding survives only for band-LABEL display.
- **`samplesReferencing` lists `ActiveOnly`.** A retired sample no longer offers new use, so it no
  longer holds a material from removal; the material's own used-version block still guards any
  version a live experiment binds.
- **No `samplesReferencing`-analogue for the sample store.** Nothing structural references a sample
  (samples are leaf library entries), so `SampleProxy.createInMemory` takes only `versionsInUse`;
  removal is blocked purely by the used-version rule (`SampleVersionInUse`), the mirror of the
  material store's `MaterialVersionInUse`.
- **`SampleVersionId` moved to `ElementId.fs` (Library).** It had to precede the in-place versioned
  `SampleProxy` record; `Lifecycle.fs` reaches it through its existing `open …Library` (the exact
  move step 021 made for `MaterialVersionId`).

## Changelog

- 2026-07-11 — Step 022 (Part H, AC-H2): mirror step 021's versioned store onto the registered
  `SampleProxy` in place (listSamples scope, resolveVersion, saveSample via the shared
  `decideVersioning`, active/inactive/supersede verbs, the used-version removal block); move
  `SampleVersionId` to `ElementId.fs` and the store body to a new `SampleStore.fs`; re-type sample
  material references (film/substrate/lower) to `MaterialVersionId`, seeds pinning v1 via
  `MaterialVersionId.firstOf`, resolution flowing through the by-version resolve; update every
  consumer; rewrite `SampleProxyTests` (22 facts incl. the version-pin round-trip). Build 0 errors;
  unit 119, constructor 589, ui-smoke 153, ui-tests 414 — all green locally, no regressions.
