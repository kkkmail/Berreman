# Step 021 — IMPLEMENT — impl-log

## Progress

- [x] Domain: moved `VersionNumber` / `MaterialVersionId` into `MaterialLibrary.fs`
      (so the re-typed record can name them in place); added `InactiveVisibility`;
      re-typed the `MaterialProxy` record into the versioned surface; added the
      `MaterialVersionInUse` error case; updated `materialsReferencingCategory`.
- [x] Domain: trimmed those two types out of `Lifecycle.fs` (kept the rest of the
      foundation intact) and added `VersionsInUse.empty`.
- [x] Domain: removed the old `MaterialProxy.createInMemory` from `ElementId.fs`.
- [x] Domain: new `MaterialStore.fs` (the versioned in-memory store) after
      `Lifecycle.fs`; registered it in the Domain fsproj.
- [x] Domain tests: rewrote `MaterialProxyTests.fs` for the versioned surface;
      fixed `CategoryProxyTests.fs` (save + createInMemory arity).
- [x] Ui consumers: `AppContext`, `DefaultStores`, `MaterialEditorView`,
      `MaterialsWindowView`, `SampleEditorView`, `SampleEditorWindow`,
      `LibraryWindowView`, and the four `materialErrorReason`/`materialErrorText`
      match sites (`+ MaterialVersionInUse`).
- [x] Ui tests: proxy stubs + `createInMemory` call sites + `listMaterials`
      scope + save recorder.
- [x] Built the whole solution (0 errors, no new warnings); ran all four test
      projects locally to confirm green.

## Files modified

Domain:
- `OpticalConstructor.Domain/MaterialLibrary.fs` — `VersionNumber`,
  `MaterialVersionId`, `InactiveVisibility` (moved-in / new); `MaterialProxy`
  record re-typed (versioned surface); `MaterialVersionInUse` error case;
  `materialsReferencingCategory` now lists `IncludeInactive`.
- `OpticalConstructor.Domain/Lifecycle.fs` — removed the two moved types; added
  `VersionsInUse.empty`.
- `OpticalConstructor.Domain/ElementId.fs` — removed the old
  `MaterialProxy.createInMemory` augmentation (kept `samplesReferencing`).
- `OpticalConstructor.Domain/MaterialStore.fs` — NEW: the versioned
  `MaterialProxy.createInMemory` (history per `MaterialId`, `decideVersioning`
  applied, lifecycle verbs, the two removal blocks).
- `OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` — registered
  `MaterialStore.fs` after `Lifecycle.fs`.

Domain tests:
- `OpticalConstructor.Tests/MaterialProxyTests.fs` — full rewrite (23 facts).
- `OpticalConstructor.Tests/CategoryProxyTests.fs` — `saveMaterial` +
  `createInMemory` arity + opens.

Ui:
- `OpticalConstructor.Ui/AppContext.fs`,
  `OpticalConstructor.Ui/TableAndElementRotationView.fs` (DefaultStores +
  `materialErrorText`), `OpticalConstructor.Ui/MaterialEditorView.fs`
  (save + error map), `OpticalConstructor.Ui/MaterialsWindowView.fs`
  (`ActiveOnly` + error map), `OpticalConstructor.Ui/SampleEditorView.fs`
  (`ActiveOnly` + error map), `OpticalConstructor.Ui/SampleEditorWindow.fs`
  (`ActiveOnly`), `OpticalConstructor.Ui/LibraryWindowView.fs` (`ActiveOnly`).

Ui tests:
- `EmbeddedChartTests.fs`, `LibraryWindowTests.fs`, `MainWorkbenchTests.fs`,
  `MaterialEditorWindowTests.fs`, `MaterialsWindowTests.fs`,
  `SampleEditorWindowTests.fs`, `WindowLauncherTests.fs` — `createInMemory`
  arity + opens, stub field sets, `listMaterials` scope, save recorder.
  (`AppContextTests.fs` needed no change — `removeMaterial` kept its signature
  and `AppContext.create` injects the empty seam internally.)

## Testing state

Gate execution is the arc-runner's (IMPLEMENT Invariant 6 — the worker acts, the
gate engine is the sole authority). These are LOCAL confirmations run to avoid
wasting the single supervisor retry; they are not self-reported as authoritative.

- `build`: `dotnet build Berreman.slnx -c Release` → **0 errors**. The only
  warnings are the two pre-existing exempt ones (`SYSLIB0051` in MathNet,
  `FS3873` in `Berreman/Dispersion.fs`); slice 021 introduces none.
- `unit-tests` (BerremanTests): **119 passed**, 0 failed (baseline 119; the core
  solver is untouched).
- `constructor-unit-tests`: **588 passed**, 0 failed (baseline 586; a strict
  addition — the rewritten `MaterialProxyTests` net +2 facts, no existing test
  removed a count).
- `ui-smoke`: **153 passed**, 0 failed (baseline 153).
- `ui-tests`: **414 passed**, 0 failed (baseline 414).

All modified `.fs` files verified LF-clean.

## Artifacts

No captured logs needed beyond the console runs above; the `.artifacts/` folder
is available for any follow-up captures.
