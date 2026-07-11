# State of the world — Step 022 (Spec 0038 Part H, AC-H2)

## Where we are

Step 022 completes Part H's store lifecycle by mirroring step 021 onto the registered sample
write-seam `SampleProxy` (STORE_XDUO_0002): it is re-typed IN PLACE into a **versioned** in-memory
store built on step 020's shared `decideVersioning` rule and the `VersionsInUse` seam. It ALSO
re-types the material references a sample carries — film, substrate, and lower-half-space material
ids — into **versioned** references (`MaterialVersionId`), so a sample layer keeps resolving its
pinned material version after the material mints a newer one. Step 25 wires the real `VersionsInUse`
over live experiment descriptors.

## What's working

- Re-type `SampleProxy` in place into the versioned surface: `listSamples` takes an
  `InactiveVisibility` switch, `resolveVersion` serves reference resolution ignoring lifecycle,
  `saveSample` applies `decideVersioning` (replacing `addSample`/`updateSample`), and
  `markSampleInactive` / `markSampleActive` / `supersedeSample` retire and revive the latest version.
- Add the versioned store `SampleStore.createInMemory versionsInUse` (history per sample, seeded
  active at version 1; mutate-in-place / mint-next / keep per the shared rule; used-version removal
  block `SampleVersionInUse`).
- Re-type sample material references to versions: `SampleLayer.materialId` and `SampleStructure.lower`
  become `MaterialVersionId`, the seeds pinning version one via `MaterialVersionId.firstOf`, and
  resolution flowing through step 021's by-version `resolveVersion`.
- Update every consumer mechanically: the composition roots inject the seam, the stack editor and
  sample editor carry versioned references (pinning v1), the facet extractors and workbench band
  view project each version to its identity.
- Rewrite `SampleProxyTests` (22 facts) over the versioned surface and add the version-pin
  round-trip (a layer keeps resolving its pinned material version after the material evolves).

## Tests

Gate execution remains the arc-runner's (IMPLEMENT Invariant 6 — the worker acts; the deterministic
gate engine is the sole authority). Locally confirmed to avoid wasting the single supervisor retry,
not self-reported as authoritative:

- `build`: 0 errors; no new warnings (only the pre-existing exempt `SYSLIB0051`, `FS3873`, and the
  pre-existing `FS1125` in the untouched `SeriesDataTests.fs`).
- `unit-tests` (BerremanTests): 119 passed (core solver untouched).
- `constructor-unit-tests`: 589 passed — a strict addition over the 588 baseline (the rewritten
  `SampleProxyTests` net +1 fact; no existing test dropped).
- `ui-smoke`: 153 passed. `ui-tests`: 414 passed. Both at baseline (no Ui test added or removed).

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 589
  ui_smoke_tests: 153
  ui_tests: 414
```

## Architecture

- **The record stays in place; the store moved by compile-order necessity** — the exact split
  step 021 made. `SampleProxy` is re-typed in place in `ElementId.fs`; its `createInMemory` body
  moved to a new `SampleStore.fs` after `Lifecycle.fs` because it needs the shared `decideVersioning`
  rule and the `VersionsInUse` seam. `SampleVersionId` moved from `Lifecycle.fs` into `ElementId.fs`
  next to `SampleId` so the in-place record can name it (a version-id type lives with its identity
  type). No shim, no parallel type, no compatibility overload.
- **One rule, applied by both stores.** `saveSample` reads the latest version's usage through the
  injected seam and calls the ONE shared `decideVersioning`. The sample's physics is the
  `(=)`-comparable `SampleStructure` (no function cases, unlike a material's engine `properties`), so
  the store passes `(=) (=)` directly; metadata is name / description / substrate kind (a
  display/search facet the engine mapping never reads).
- **References point at a version, resolved late.** Every material reference a sample carries —
  film, substrate, and lower half-space — is a `MaterialVersionId`; `Propagation.resolveSampleMaterials`
  takes the `MaterialProxy` and resolves each pinned version through `resolveVersion`, so a later
  mint never rewrites a bound sample's physics. The workbench resolves against the LIVE store.
- **Supersession is structural** (the step-021 shape): `listSamples ActiveOnly` surfaces only the
  latest-active version per sample, so a minted-over or retired version is automatically hidden from
  offers yet resolvable by version.
- **Elevated primitives.** The pin is the `MaterialVersionId`; the used-version refusal is the typed
  `SampleVersionInUse`; the listing switch is the shared two-case `InactiveVisibility`.

## Deferred

- The real `VersionsInUse` over the in-memory experiment store's descriptors, injected at
  composition in place of `VersionsInUse.empty` (step 25) — at which point a sample/material version
  can actually be "in use" and a mint can fire through the UI path.
- Pin-LATEST in the editor: today it pins version one (correct while every material lives at v1);
  step 25 (real usage → mints) is when a "resolve the material's current latest version id" plumbing
  may be needed. Centralised in `MaterialVersionId.firstOf` so it is a one-site change.
- The Materials/Library window show-inactive TOGGLE that flips `listSamples` to `IncludeInactive`
  (a later UI step — every consumer defaults to `ActiveOnly` now).
- A dedicated editor / distinct representation for supersede vs mark-inactive (they share
  `InactiveEntry` at this in-memory step, as in step 021).

## Architecture decisions

- **Sample physics = `SampleStructure`, metadata = name/description/substrate kind.** The
  `SubstrateKind` never versions because the engine mapping does not read it — it is a search facet.
- **`lower` versioned alongside films/substrate.** All three are material references a sample
  carries; Part H says references point at a version, so versioning only two of three would leave a
  wart in `referencedMaterials` and resolution.
- **The sample store has no `samplesReferencing`-analogue.** Nothing structural references a sample,
  so removal is guarded purely by the used-version block, the mirror of the material store's
  `MaterialVersionInUse`.

## Gotchas

- **`saveSample` replaced `addSample`/`updateSample`** — a new id inserts version 1; an existing id
  runs the decision rule. Any future `SampleProxy` stub must supply the full new field set; the two
  old write fields are gone (no compatibility overload).
- **The editor pins version one (`MaterialVersionId.firstOf`).** Correct at this step (mints need a
  used version, and `VersionsInUse` is empty until step 25); `MaterialEntry` carries no version
  number, so the view cannot pin latest through the proxy surface today. Centralised in one helper
  shared by the seeds and the editor.
- **`samplesReferencing` now lists `ActiveOnly`** — a retired sample no longer holds a material from
  removal; the material's own used-version block still guards any bound version.
- **`SampleVersionId` now lives in `ElementId.fs` (Library), not `Lifecycle.fs`** — it had to
  precede the in-place versioned record; `Lifecycle.fs`'s `VersionRef` reaches it through its
  existing `open …Library`.
- **Pre-existing `FS1125` warnings** in `OpticalConstructor.Tests/SeriesDataTests.fs` are NOT from
  this slice (that file is untouched — confirmed via `git diff`); they surface only on a full
  rebuild.

## Changelog

- 2026-07-11 — Step 022 (Part H, AC-H2): mirror step 021's versioned store onto `SampleProxy` in
  place (listSamples scope, resolveVersion, saveSample via the shared `decideVersioning`,
  active/inactive/supersede verbs, the used-version removal block); move `SampleVersionId` to
  `ElementId.fs` and the store body to a new `SampleStore.fs`; re-type sample material references
  (film/substrate/lower) to `MaterialVersionId`, seeds pinning v1 via `MaterialVersionId.firstOf`,
  resolution flowing through the by-version resolve; update every consumer; rewrite `SampleProxyTests`
  (22 facts incl. the version-pin round-trip). Build 0 errors; unit 119, constructor 589, ui-smoke
  153, ui-tests 414 — all green locally, no regressions.
