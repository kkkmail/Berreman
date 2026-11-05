# Step 0020 — CLOSE_RED — impl log

## Progress

- Read the protocol (`arc-runner.system-md`, `close_red_worker.system-md`), the project prompt,
  `AGENTS.md`, the slice and its `.gates` snapshot, `.spec-md` §3.3, and step 19's ADD_RED outputs.
- Survey before editing:
  - the slice's grep (`src-600`, `det-intensity`, `pol-lp`, …) over `Berreman/*.fs` found only the
    33 `PresetIds` lines of `ElementId.fs`; the tests already name presets through `PresetIds`;
  - the only product site that tells a sample id from another by parsing it as a GUID is
    `liveLibrary`'s probe (`ElementId.fs`, `tryGetEntry`); `SampleStore.tryGetSample` answers
    `Ok None` for an unknown id, so a preset GUID falls through to the presets. The
    `Guid.TryParse` calls in `ConstructorUiIdOfDomain.fs` read category / material codes only;
  - product sorts (`LibraryWindowView`, `MaterialsWindowView`, `Facets`) sort by label with a
    stable sort, never by entry id; no `Map` / `Set` keyed by `ElementValueId` is enumerated into
    an order the UI shows.
- Minted 33 GUIDs once with `[guid]::NewGuid()` (saved, in member order, to
  `.artifacts/0020-minted-guids.txt`) and wrote them into `Library.PresetIds` in the lowercase
  hyphenated form, member order unchanged. Each GUID occurs exactly once in the tree outside
  `specs/` (`git grep -c -i -F` per GUID), so none equals a `MaterialIds`, `CategoryIds` or
  `SampleIds` literal, and no two are equal.
- `newSourceId` now mints `ElementValueId (string (Guid.NewGuid ()))`; its doc says the id is a plain
  GUID string, as a sample's is.
- `LibraryEntry.entryId`'s doc says every entry id is the string of a GUID: a sample's `SampleId`,
  a preset's `PresetIds` member, a user source's minted `newSourceId`. `PresetIds`' doc says each is
  minted once. `liveLibrary`'s code is unchanged.
- `SourceLibraryTests.fs` gained `the live Library resolves a seeded preset, a user source and a
  sample by their GUID ids`.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs` (+44 / −42): the 33
  `PresetIds` literals, the `PresetIds` / `LibraryEntry.entryId` / `newSourceId` docs, and
  `newSourceId`'s body.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SourceLibraryTests.fs` (+21 / −0): the new fact.

Both are LF: `git diff --numstat` equals `git diff --ignore-cr-at-eol --numstat`, and
`git ls-files --eol` shows `i/lf w/lf` for both. (`specs/0050/.manifest.state.json` was already
modified by the supervisor when the round started; this round did not touch it.)

## Testing state

These are local observations, made to confirm the change. The arc-runner's gate engine runs and
records the gates.

- `build` (`dotnet build Berreman.slnx -c Release -nologo -v:m`): exit 0, 0 errors. The only
  warnings are the two vendored `MathNetNumerics/Numerics/Exceptions.cs` SYSLIB0051 warnings that
  were already on the start tree. Log: `.artifacts/0020-build.log`.
- `constructor-unit-tests` (whole suite, as the gate runs it): `Passed!`, `total: 739, failed: 0,
  succeeded: 739` (738 on step 19's tree, plus the new fact). The TRX shows all three passing:
  `every seeded Library entry id is the canonical string of a GUID, and no two are equal` and
  `newSourceId mints the canonical string of a fresh GUID` (step 19's reds) and
  `the live Library resolves a seeded preset, a user source and a sample by their GUID ids`.
  Log: `.artifacts/0020-constructor-unit-tests.log`.
- `ui-tests` with the gate's 46-class selection: `Passed!`, `total: 560, failed: 0, succeeded: 560`.
  The whole suite without a class filter also gives 560/560. The TRX shows
  `Add source opens the editor on a freshly minted source id` (step 19's red) passing.
  Logs: `.artifacts/0020-ui-tests-selected.log`, `.artifacts/0020-ui-tests.log`.
- `ui-smoke`: `Passed!`, `total: 208, succeeded: 208`. Log: `.artifacts/0020-ui-smoke.log`.
- `elevation`: exit 0, `ELEV-GATE: OK - 0 new finding(s) in 8 project(s)`. Domain changed, so the
  run covered Domain plus its dependents; the test file is outside the ELEV scope. Log:
  `.artifacts/0020-elevation.log`.
- The five s03 invariants each ran their `git grep` and found nothing (exit 1):
  `s03-elev-list-empty`, `s03-no-vstest`, `s03-no-kit-kit-json`, `s03-no-eps-axis-evaluated`,
  `s03-complexity-mandatory`.
- The slice's grep (`(src-(600|hene-|…)|det-(intensity|ellipsometer)|pol-(lp|cp-left|cp-right))`
  over `Berreman/*.fs`) finds nothing (exit 1). S1.4's invariant `s04-no-seeded-string-preset-ids`
  will run this same grep.
- No fact failed because of the old ids' alphabetical order, so no fact was made order-independent.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0020-minted-guids.txt` — the 33 minted GUIDs, in `PresetIds` member order.
- `0020-build.log`
- `0020-constructor-unit-tests.log`
- `0020-ui-tests.log`, `0020-ui-tests-selected.log`
- `0020-ui-smoke.log`
- `0020-elevation.log`

## Gotchas

- The 33 `PresetIds` literals are frozen from this step: S1.5 exports them byte-for-byte. Never
  re-mint them.
- `liveLibrary`'s `Guid.TryParse` probe now succeeds for a preset id too. It is still correct,
  because the samples store answers `Ok None` for a GUID it does not hold and the presets resolve
  next. A future store that answered an *error* for an unknown id would still fall through (the
  probe maps `Error _` to `None`).
- The new fact builds the user source as a `SourcePreset` record rather than through the private
  `userSource` helper, because that helper takes a bare string id and the fact needs the minted
  `newSourceId ()`. Its "names nothing" id is a fresh `System.Guid.NewGuid ()` string, written
  qualified like step 19's helper, as the file does not open `System`.
- Test-local user-source ids (`src-user-1`, `src-mine`, …) stay text. The stores take any id; the
  rule covers only seeded and minted ids, and the slice's grep does not match them.
