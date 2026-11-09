# Step 0198 — impl log

## Progress

- Read the task, both system prompts, the project prompt, the slice and its gates (build, elevation,
  constructor-unit-tests, fourteen s22 invariants). No operator note was in flight.
- Read `KitArchive.fs` (steps 196–197), `KitArchiveProxyTests.fs`, `Kit.fs` (`KitFile.create`, `KitFile.fileName`,
  `KitFolder.tryKitPathOf`, `KitRelativePath.pathRole`) and `KitFolderPicker` (`KitFolders.fs`, step 145). Also read the
  briefs of steps 199 and 200 to check that the new members fit how they will be called.
- **Domain (`KitArchive.fs`).** Added two members to the existing `type KitArchiveProxy with` extension, beside
  `createInMemory`:
  - `sendKit kitFile kit zip observer`: if `kit.tryKitPathOf zip` is `Some _`, it answers
    `KitZipInsideKitErr '<zip>'` and lists or writes nothing. Otherwise it runs `listKitTree kit`, then
    `writeKitZip { kit; entries = kitFile.sentEntries tree; zip }`.
  - `receiveKit zip target observer`: runs `listZipEntries zip`. A zip with no root `KitTreeFile` whose path equals
    `KitFile.fileName` (ignoring case) is `KitZipHoldsNoKitErr '<zip>'`, and nothing is extracted. Otherwise it runs
    `extractKitZip { zip; entries = <the entries whose pathRole is KitContentPath>; target }`.
  - After the extension: `[<ReferenceEquality>] KitZipPicker = { pickZipToWrite : FileName -> Async<FileName option>;
    pickZipToOpen : unit -> Async<FileName option> }`. It has a one-line doc on each field, and
    `static member none : KitZipPicker` answers `None` to both.
- **Tests (`KitArchiveProxyTests.fs`).** Added four fixtures to `KitArchiveProxyTestData`:
  - `sampleKitWithAppCache`: `sampleKit`, plus `kit/.oc/lock` and `kit/.oc/planner/c.json`.
  - `dngExcludingKitFile`: `KitFile.create … with sendExclude = [ **/*.dng ]`.
  - `fourFiles`.
  - `sendSampleKit` and `zipKitEntries`.

  Added five facts to `KitArchiveProxyContract`, which `KitArchiveProxyTests` inherits. They follow the slice's
  wording exactly. Added `open OpticalConstructor.Domain.LibraryEntryFiles` for `KitId`.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitArchive.fs`: +64 lines (`sendKit`, `receiveKit`,
  `KitZipPicker`).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitArchiveProxyTests.fs`: +115 lines (one `open`, the
  fixtures, five facts).

Both files are in the slice's authorized `edits`. No project file changed.

## Testing state

These are development runs only. Under Invariant 6, the gate verdict belongs to the arc-runner's gate engine.

- **First build:** it failed on `KitId` not being in scope in the test file (FS0039), because `KitId` lives in
  `LibraryEntryFiles`. I added the `open`.
  - This is not a TDD red: the facts and the production members were written together. A separate red build on the
    undefined `sendKit` / `receiveKit` was not captured.
- **Build:** `dotnet build Berreman.slnx -c Release -nologo -v:m` exits 0, with 0 warnings and 0 errors
  (`0198-build.log`).
- **constructor-unit-tests:** 1233 passed, 0 failed, 0 skipped (`0198-constructor-unit-tests.log`).
  - That is checkpoint 197's 1228 + 5, the five declared facts.
  - The TRX (`0198-testresults/constructor-unit-tests.trx`) lists all five
    `OpticalConstructor.Tests.KitArchiveProxyTests.*` identities as Passed.
- **Elevation**, with `ASG_ARC_BASE_COMMIT=f395597…` (`.arc-base-json`): `ELEV-GATE: OK`, 0 new findings in 12
  projects, 0 known findings, ELEV004 excluded (`0198-elevation.log`).
- **The fourteen s22 invariant greps:** none finds anything (`0198-invariants.log`).
- **Line endings:** both touched files are `i/lf w/lf` with no BOM. `git diff --numstat` matches
  `--ignore-cr-at-eol`.

## Artifacts

- `specs/0050/.artifacts/0198-build.log`
- `specs/0050/.artifacts/0198-constructor-unit-tests.log`
- `specs/0050/.artifacts/0198-testresults/constructor-unit-tests.trx`
- `specs/0050/.artifacts/0198-elevation.log`
- `specs/0050/.artifacts/0198-invariants.log`

## Gotchas

- **ELEV012 (no two-case union mapped to `bool`).** `KitTreeEntry` and `KitPathRole` both have two cases.
  - The kit-file lookup is a `List.tryPick` that answers the path.
  - The app-cache filter is a `List.choose`, as in `KitFile.sentEntries`.
  - Neither is a `bool` lambda.
- **Record-literal inference.** `KitZipJob` and `KitUnzipJob` share `zip` and `entries`, so each job is bound with a
  type annotation before it is passed.
- **The first, full-recompile build printed two SYSLIB0051 warnings from the vendored
  `MathNetNumerics/Numerics/Exceptions.cs`.**
  - Neither this step nor the arc touches that file.
  - The incremental rebuild that followed is warning-free.
- **`receiveKit` answers the `listZipEntries` error unchanged.** A missing or unreadable zip is still
  `KitArchiveIoErr`, and a zip entry that escapes is still `KitZipEntryEscapesErr`. The slice asks for no other
  mapping.
