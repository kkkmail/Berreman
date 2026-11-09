# Step 0218 — impl log (IMPLEMENT: a kit's item files through `KitProxy`)

## Progress

- [x] Read the protocol (arc-runner and implement_worker system prompts), the project prompt, AGENTS.md, the slice and
  its `.gates` snapshot, and step 0217's SoW and impl-log. No operator note was in flight.
- [x] `KitContentFiles.fs`, on step 148's `type KitProxy with` extension:
  - `member this.writeItem (item : Item) : Result<unit, KitError>` renders `item.itemFile.fileText` (step 217) and
    writes it whole with `writeKindFile ItemKitFile item.label.fileStem`. A rendering `StorageError` is
    `KitFileRejectedErr "the file '<label>.item-json': <describe>"`.
  - `member this.readItems () : Result<Item list, KitError>` reads every stem `listKindFiles ItemKitFile` answers,
    with `readKindFile`, and parses each with `parseItemFile`. The whole read answers `KitFileRejectedErr`, naming
    the file, for:
    - a read or parse refusal;
    - a label whose `fileStem` is not the file's stem, ignoring case;
    - an `ItemId` already read.

    The answer is sorted by `label.value`, ordinal.
  - Each member has a doc comment of two lines citing spec 0050 S7.1. No delete, cache, retry or other member was
    added.
- [x] **Reuse.** The list → read → parse → refuse-by-file fold, which `readExperiments` and `readSceneNames` each
  carried, is now one private `readEveryKindFile`. The render → refuse-by-file → write pipeline of `writeExperiment`
  and `writeScene` is now one private `writeRenderedKindFile`.
  - All three readers and all three writers call them.
  - Each existing member keeps its exact calls, order, error texts and sort.
  - Both are private module functions beside `contentStorageRejectedErr` / `contentReadRejectedErr`, not members,
    so `KitProxy` gains only the two members the slice names.
  - `open OpticalConstructor.Domain.LibraryEntryFiles` was added for `FileStem` in their signatures.
- [x] `OpticalConstructor.Tests/KitItemFileTests.fs` (new): `module KitItemFileTests` in
  `namespace OpticalConstructor.Tests`, appended last in the compile list with no comment. It holds the four
  declared facts under their exact identities.
- [x] Development runs: build, class tests, mutations, whole constructor suite, invariants, elevation (below).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitContentFiles.fs` (+69 / −35)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitItemFileTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (+1 compile line, last)

Every one is in the slice's authorized `edits`. All are LF only: 0 CR bytes, counted with `tr -cd '\r' | wc -c`.
`git diff --numstat` equals `--ignore-cr-at-eol`.

## Decisions

- **The kit fixture is the shared `KitContentRoundTripFixture.createContentTestKit`**, reached through
  `open KitContentRoundTripFixture`.
  - It makes `Path.Combine(AppContext.BaseDirectory, "KitItemFileTests", <new GUID "N">)` from
    `FolderName "KitItemFileTests"`, builds `KitProxy.create` over it, and calls `initialiseKit` with a kit file it
    defines.
  - That is exactly the brief's recipe, and exactly what the named precedent, `ExperimentCollectionRoundTripTests`,
    does.
  - A test-local copy of that body would be the duplicated function body AGENTS.md forbids ("Reuse, never copy").
- **The items are made through `ItemDraft.toItem`**, as `LibraryModelChoice` over the built-in models:
  - CPL1: circular polarizer, `DialMounted DialPositive`;
  - LP1: real linear polarizer, `DialMounted DialNegative`;
  - Phone: intensity detector, `FixedMounting`.

  The ids are fresh test GUIDs. Step 217's CPL1 / LP1 fixtures are private to `KitContentFileJsonTests`, and LP1
  there is the ideal-polarizer preset form, not the real LP the brief names.
- **The revision-2 record is constructed directly** (`cpl1Revision2`): CPL1 with `revision = ItemRevision 2`,
  `history = [ { revision = ItemRevision 1; parameters = cpl1.parameters } ]` and its `dialZero` re-measured to
  0.25. Step 235's publication behaviour is not used.
- **A repeated id names both of its files.** `listKindFiles` answers stems in ordinal order, so in the brief's case
  `CPL9.item-json` (LP1's id) is read *before* `LP1.item-json`, and the repeat is met on `LP1.item-json`. The refusal
  therefore reads "the file 'LP1.item-json' repeats the id <guid> of item 'CPL9' in 'CPL9.item-json'". It names both
  files and the id, whichever the listing reached first, so "naming that file" holds for `CPL9.item-json`. The
  label rule runs before the id rule, so `Other.item-json` (CPL1's text, which also repeats CPL1's id) is refused as
  "holds item 'CPL1' instead of 'Other'".
- **Assertions beyond the brief's minimum, within the four facts:**
  - **Round-trip fact:**
    - each stored text is the item's canonical `fileText`;
    - a lower-case `lp2.item-json` holding `LP2` is accepted, because the stem rule ignores case (the
      `ExperimentCollectionRoundTripTests` e10 precedent).
  - **Replace fact:**
    - an unrenderable item (a NaN known value) refuses the write as `KitFileRejectedErr` naming `CPL1.item-json`;
    - after it, the folder's bytes are unchanged. This is the brief's "a `StorageError` of the rendering is
      `KitFileRejectedErr` naming the file", and the experiment-replace precedent.
  - **Refusal fact:**
    - the refusal for `Other.item-json` names `'CPL1'` and `'Other'`;
    - the refusal for `CPL9.item-json` names LP1's id;
    - after the bad file is deleted, the kit reads CPL1, LP1 and Phone again.
  - **Read-only fact:** both a rewrite (CPL1 at revision 2) and a new item (LP2) are refused.

## Testing state

Development runs only. The arc-runner's gate engine gives the verdict (Invariant 6).

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`):
  - The first build failed, because `FileStem` was not in scope (`0218-dev-build-01.log`). It also showed the two
    pre-existing vendored-MathNet `SYSLIB0051` warnings.
  - After `open …LibraryEntryFiles`: exit 0, 0 warnings, 0 errors (`0218-dev-build-02.log`).
  - Rebuild after the first mutation run: 0 / 0 (`0218-dev-build-03.log`).
  - Final source, with the helpers as module functions: 0 warnings, 0 errors (`0218-dev-build-04.log`).
  - Rebuild after the final mutation run, so `bin/` holds the restored source: 0 warnings, 0 errors
    (`0218-dev-build-05.log`).
- **Class tests** for `KitItemFileTests`, `ExperimentCollectionRoundTripTests`, `SceneRoundTripTests` and
  `KitContentFileJsonTests`: 32 passed, 0 failed (`0218-dev-class-tests-01.log`). `--list-tests` discovers exactly the
  four declared identities.
- **The facts bite.** Five code mutations of `readItems` / `writeItem` were each rebuilt and run against
  `KitItemFileTests`, and each was caught. Afterwards the source was restored byte for byte (`cmp`).
  `0218-mutations/code-mutations.log` holds the run on the final source.
  - The label sort was dropped: three facts failed.
  - The repeated-id rule was dropped: the refusal fact failed.
  - The label-stem rule was dropped: the refusal fact failed, on `'Other'`.
  - The stem rule was made case-sensitive: the round-trip fact failed, on `lp2.item-json`.
  - The item was written under a lower-case stem: the round-trip and replace facts failed.
- **constructor-unit-tests** (the gate's command, whole suite, final source after build 05): 1308 passed, 0 failed,
  0 skipped. That is 1304 + 4, in 65 s against the 180 s estimate. The TRX shows all four new identities `Passed`.
  Log: `0218-dev-constructor-unit-tests.log`.
- **The fourteen `invariant-s22-*`:** each holds. `git grep` finds nothing, and the untracked new test file holds
  none of the patterns either. Log: `0218-dev-invariants.log`.
- **elevation** (`ASG_ARC_BASE_COMMIT=f395597c…`, `-ExcludeRules ELEV004`, final source): `ELEV-GATE: OK - 0 new
  finding(s) in 12 project(s)`, 196 s against a 600 s deadline. Storage shows `findings=0` (suppressed: 150; 146 at
  step 217). Suppressed findings are ELEV004 or third-party `T` namesakes, never enforced.
  Log: `0218-dev-elevation.log`.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0218-dev-build-01.log` … `0218-dev-build-05.log`
- `0218-dev-class-tests-01.log`
- `0218-dev-constructor-unit-tests.log`
- `0218-dev-invariants.log`
- `0218-dev-elevation.log`
- `0218-dev/KitContentFiles.fs.orig` (the pre-step source)
- `0218-mutations/`, which holds:
  - `mutate_code.py`
  - `code-mutations.log`
  - `KitContentFiles.fs.orig` (the final source the mutations restore)

## Gotchas

- **Listing order decides which file meets a repeated id.** Stems come back in ordinal order. A later reader that
  wants "the newcomer" named must not assume it is the second file read, so the reason names both files.
- **`Other.item-json` holding CPL1's text breaks two rules.** The label rule is checked first. A fact that wants to
  see the id rule alone needs a file whose label matches its stem (the `CPL9` case).
- **The shared fold accumulates newest-first.** Every reader sorts its answer, by id, by name or by label. A new
  reader on `readEveryKindFile` must sort too.
- **The mutation script leaves the last mutant's binaries in `bin/` until the next build.** The source is restored
  (checked with `cmp`), and this round rebuilt afterwards (build 05). The gate engine's `build` gate always runs first.
