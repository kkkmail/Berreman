# Step 0196 — impl log

## Progress

- Read the protocol, the IMPLEMENT delta, the project prompt, the slice and the gate snapshot.
  - Manual 009 names step 0196: the shared lock guard must keep its repaired contract.
  - Manual 013 applies: elevation runs from the arc origin.
  - No operator note is in flight.
- `KitRelativePath.kitCachePath` (`.oc`) already existed. Its only readers were `KitFolders.fs` and `KitFolderStore.fs`,
  both authorized. It is the member the slice calls `appCacheFolder`, so it was renamed rather than duplicated
  (Gotchas). `kitLockPath` stays, because `KitProxyTests` and `KitOpeningTests` read it.
- Wrote the tests first: `KitArchiveTests.fs` (four facts) and the `KitTests` fact. A test-project build was red on
  the undefined symbols (`KitPathRole`, `parentOpt`, `leafName`, `appCacheFolder`, namespace `KitArchive`,
  `GlobPathMatch`, `matchOf`, ...). The red build is `0196-red-build.log`.
- Wrote the production code: `Kit.fs`, `KitFolders.fs`, `KitFolderStore.fs`, the new `KitArchive.fs` and its
  `<Compile>` line.
- The whole solution built with 0 warnings and 0 errors. Constructor suite: 1220 passed, which is 1215 + 5.
- Elevation from the arc origin: `ELEV-GATE: OK`, 0 new findings in 12 of 12 projects. No repair was needed.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`:
  - New `KitPathRole` (`KitAppCachePath` | `KitContentPath`), directly before `KitRelativePath`.
  - `KitRelativePath.kitCachePath` is renamed `appCacheFolder`. It is the one `.oc` in product code, with a doc
    citing S6 and 001 §A.
  - `kitLockPath` is now `$"{appCacheFolder.value}/lock"`, so its answer is unchanged (`.oc/lock`).
  - New members on `KitRelativePath`:
    - `pathRole`: compares the first `/` segment with `appCacheFolder` by `OrdinalIgnoreCase`;
    - `parentOpt`: the text before the last `/`, `None` at the root;
    - `leafName : FileName`: the text after the last `/`.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitFolders.fs`:
  - `InMemoryKitFolder`'s acquire adds `KitRelativePath.appCacheFolder` as the `.oc` folder. The `.oc/lock` entry
    already went through `kitLockPath`, which is derived from it.
  - The private `parentFolders` walker now recurses over `parentOpt`, so the last-`/` split is written once. Its
    answers are unchanged.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitFolderStore.fs`:
  - The proxy's `cacheFolder` is named through `KitRelativePath.appCacheFolder`. The lock file stays on `kitLockPath`.
  - The bounded guard (`KitLockGuardProxy`, retries, delete-on-close, ownership rechecks) is untouched, so 0050
    .manual/009's contract holds.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitArchive.fs` (new). It is `module KitArchive` and opens
  `Kit`, `LibraryEntryFiles` and `Softellect.Sys.Primitives`. In order:
  - `GlobPathMatch`.
  - `SendExcludeGlob.matchOf`:
    - recursive over the glob's and the path's `/` segments (the glob's `\` become `/`), then over characters;
    - `**` spans whole segments, and a final `**` needs at least one (Gotchas);
    - `*` and `?` never cross a `/`;
    - other characters match by `OrdinalIgnoreCase`;
    - no regex, and no `bool` anywhere.
  - `KitSendChoice`.
  - `KitTreeEntry`, with `.path`.
  - `KitFile.sendChoiceOf`: the app's cache is `LeftOutOfSend`. Otherwise the path is checked against each glob, then
    each folder up its `parentOpt` chain.
  - `KitFile.sentEntries`: `List.choose` over `SentWithKit`, in the tree's order.
  - `KitName.zipFileName`: `this.fileStem.withExtension (FileExtension ".zip")`.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`: one
  `<Compile Include="KitArchive.fs" />` directly after `KitHistory.fs`, with no comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitArchiveTests.fs` (new). It is `module KitArchiveTests`;
  every path goes through `KitRelativePath.tryCreate` via a private `kitPath`. It holds the four declared facts:
  - The glob fact covers the exact cases of the slice, through a helper that names the glob and path on failure.
  - The send-choice fact uses a kit file with `sendExclude = [ **/*.dng; data/scratch ]`. It covers the slice's six
    left-out paths and six sent paths.
  - The sent-entries fact uses the slice's nine-entry tree. It asserts the tree is in ordinal order, then the
    answers under `**/*.dng` and under no glob.
  - The zip fact: `(KitName "Optics: QZ/LR").zipFileName = FileName "Optics_ QZ_LR.zip"`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitTests.fs`: new fact
  `a kit path knows its folder, its leaf and whether it is the app's cache`, placed after the kit-relative path fact.
  It holds the slice's assertions exactly.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: `KitArchiveTests.fs` is
  appended at the end of the compile list, with no comment.

## Testing state

These are development runs only. Under Invariant 6, the gate verdict belongs to the arc-runner's gate engine.

- **Red first:** the test project failed to build on the undefined symbols before the production code existed
  (`0196-red-build.log`).
- **Build:** `dotnet build Berreman.slnx -c Release -nologo -v:m` exits 0, with 0 warnings and 0 errors
  (`0196-build.log`). Domain, Storage and every dependent recompiled.
- **constructor-unit-tests:** 1220 passed, 0 failed, 0 skipped (`0196-constructor-unit-tests.log`). That is
  checkpoint 194's 1215 + 5, the five declared facts. The TRX (`0196-testresults/constructor-unit-tests.trx`) lists
  all five identities as Passed. The kit-lock guard facts of `KitProxyTests` (manual 009) are among the passes.
- **Elevation**, with `ASG_ARC_BASE_COMMIT=f395597…` (`.arc-base-json`): `ELEV-GATE: OK`, 0 new findings in 12 of 12
  projects, 0 known findings, ELEV004 excluded (`0196-elevation.log`).
  - Domain's suppressed (ELEV004-only) count moved from 313 to 314, because of `path.value` in
    `KitArchive.fs:52`.
  - The slice forbids a new member that could avoid that use. ELEV004 is measured, not gated (0048 .manual/002 R7).
- **The fourteen s22 invariant greps:** none finds anything.
- **Line endings:** every touched file is LF with no BOM (`git ls-files --eol`, and 0 CR bytes in the two new files).
  `git diff --numstat` matches `--ignore-cr-at-eol`.

## Artifacts

- `specs/0050/.artifacts/0196-red-build.log`
- `specs/0050/.artifacts/0196-build.log`
- `specs/0050/.artifacts/0196-constructor-unit-tests.log`
- `specs/0050/.artifacts/0196-testresults/constructor-unit-tests.trx`
- `specs/0050/.artifacts/0196-elevation.log`

## Gotchas

- **Interpretation: `appCacheFolder` replaces `kitCachePath`.** The slice adds `appCacheFolder` (`.oc`) and wants
  `.oc` written once in product code, but `kitCachePath` already held `.oc`. Keeping both would copy the constant,
  which AGENTS.md's reuse rule forbids. So the existing member was renamed, and its two product readers moved to the
  new name. No test read `kitCachePath`. `kitLockPath` keeps its name and answer, because tests outside this step's
  effects read it.
- **Interpretation: a final `**` needs at least one segment.** The slice says `**` "matches zero or more whole path
  segments". It also requires `data/raw/big/**` not to match `data/raw/big`, and `**` alone to match `.kit-json`.
  - A leading or inner `**` takes zero or more segments (`**/*.dng` matches `IMG_0001.DNG`).
  - A final `**` takes one or more, so `a/**` is what lies inside `a`, never `a` itself. This is the gitignore
    reading, and the doc comment of `matchOf` states it.
- **A glob is taken literally.** A trailing `/` (e.g. `data/scratch/`) leaves an empty segment that no path has, so
  it matches nothing. The slice adds no normalisation, and none was added.
- `specs/0050/.manifest.state.json` was already modified by the supervisor before this worker started, and it was not
  touched.
