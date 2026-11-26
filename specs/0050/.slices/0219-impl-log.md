# Step 0219 — impl log (IMPLEMENT: `KitLibraryContext.recordItem`)

## Progress

- [x] Read the protocol (the arc-runner and implement_worker system prompts), the project prompt, AGENTS.md, the slice
  and its `.gates`, and step 0218's SoW and impl-log. No operator note was in flight.
- [x] `KitContentFiles.fs`, on step 149's `type KitLibraryContext with` extension, after `recordScene`:
  - adds `member this.recordItem (item : Item) : Result<unit, KitLibraryError>`;
  - it calls `this.snapshotReferences item.libraryReferences` (steps 139, 215), then `this.kit.proxy.writeItem item`
    (step 218), mapping a `KitError` to `KitLibraryKitErr`;
  - its doc comment is three lines citing spec 0050 S7.1;
  - no other member was added.
- [x] `KitLibraryTests.fs`: the four declared facts, under their exact identities, appended after `a read-only kit
  records nothing`. One private helper was extracted (see Decisions).
- [x] Development runs: build, class tests, mutations, whole constructor suite, invariants, elevation (below).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitContentFiles.fs` (+7 / −0)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitLibraryTests.fs` (+247 / −30)

Both are in the slice's authorized `edits`. Both are LF only: 0 CR bytes, counted with `tr -cd '\r' | wc -c`, and
`git diff --numstat` equals `--ignore-cr-at-eol`. The `.fsproj` files are untouched.
`specs/0050/.manifest.state.json` was already modified before this round; it is the supervisor's, not this step's.

## Decisions

- **`recordItem` is the exact shape of `recordExperiment` and `recordScene`.**
  - The read-only refusal comes from `snapshotReferences`, which answers `KitLibraryKitErr (KitReadOnlyErr
    "snapshotReferences")` before any IO. That is why the recorder stays empty.
  - The snapshots come first, so a failing write keeps them, and the next record finds them held.
- **The fixtures are the class's own.**
  - `freshSnapshotFixture` / `snapshotFixture` provide the Built-in library (`BuiltinCatalogue.freshStores ()`), the
    pin recorder and the in-memory kit, read-write or read-only.
  - `catalogueEnvelope` gives the expected envelopes.
  - `assertSnapshots` checks `listSnapshots`.
  - `observeContentWrites` asserts, at the moment `writeKindFile` runs, that every expected snapshot is listed and
    every pin was taken, pin then write per version. That is the "before it writes the item's file" obligation,
    measured at the write.
- **Every item is made through `ItemDraft.toItem`, over a shipped step-210 model from `BuiltinCatalogue.itemModel`.**
  No model is manufactured, and no preset or sample stands as a model.
  - CPL1: `LibraryModelChoice`, circular polarizer, `DialMounted DialPositive`.
  - QZ: `StackSampleChoice` over the stack model, `SampleVersionId.firstOf glassPlate1mmId` and the Built-in glass
    plate, `DialMounted DialPositive`.
  - Laser: `SourcePresetChoice` over the source-binding model and `BuiltinCatalogue.source PresetIds.heNe633`,
    `FixedMounting`.
  - LP1: `PolarizerPresetChoice` over the ideal-polarizer model and the `polarizerLp` preset, `DialMounted
    DialNegative`. The preset is decoded from its catalogue envelope (`content.toPolarizerPreset`), so no third copy
    of the private `polarizerPresetOf` helper that two other classes hold is written.
- **What the brief's closing obligations add, inside the four facts:**
  - **Source, ideal-polarizer and stack items are all recorded and snapshotted.** The preset fact records LP1 after
    the laser.
  - **The model and the full physics closure are pinned and snapshotted.** The QZ expectation is the stack model,
    the sample, `glass152` and the `glass` category, in that order, pinned in that order.
  - **Reopening against an empty library.** `assertItemsReopenAgainstEmptyLibrary` opens a second, read-only
    in-memory proxy over the same folder with an empty `CatalogueImportProxy.createInMemory ()`, then:
    - reads the items back;
    - finds each item's model by its exact `.item-model-json` snapshot and decodes it with `toItemModel` (step 210),
      equal to the shipped model and version;
    - resolves the binding through the overlaid stores of `openOverlay []` (step 142): a preset by
      `library.tryGetEntry`; a sample by `samples.resolveVersion`, each of its materials by
      `materials.resolveVersion`, and each material's category in `listCategories`.
    - The overlaid *item-model* store is step 222's `ItemModelProxy.overlaidWith`, which this step does not add
      ("No other member is added").
  - **A missing model is a typed error, never a preset fallback.** A library holding HeNe 633 but not the
    source-binding model answers `UnheldEntryErr` naming the model's GUID for the laser. It snapshots nothing, pins
    nothing, writes no file and reads no item.
- **Assertions beyond the brief's minimum, within the four facts:**
  - **CPL1:**
    - the model is version 1;
    - `cpl1.libraryReferences` is exactly that version;
    - `model.snapshotPath` is the literal `library/<GUID>@v1.item-model-json`;
    - the folder's new paths are exactly `CPL1.item-json` and that snapshot. The `library` folder already exists
      after `initialiseKit`, measured.
  - **Recording CPL1 again** rewrites its file and pins nothing more.
  - **A refused write** keeps the snapshot, writes no item, and a retry then records it. This is the step-139
    contract the doc comment states.
  - **QZ2**, a second item over the same sample version, snapshots and pins nothing more, so each closure member is
    held once.
  - **The read-only fact** loops over all four kinds of item.
- **Reuse: `refusingContentWrites`.** The experiment and scene recording facts each built the same 16-line
  "refuse every content write after asserting the snapshots" context inline. The item fact needs it a third time.
  - It is now one private helper beside `observeContentWrites`.
  - Both existing facts call it with exactly their previous arguments: `expected`, and `recordingClosure ()`.
  - Their assertions are unchanged, and so are their identities.

## Testing state

Development runs only. The arc-runner's gate engine gives the verdict (Invariant 6).

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`):
  - First build: exit 0, 0 errors (`0219-dev-build-01.log`). Its two warnings are the pre-existing vendored-MathNet
    `SYSLIB0051` warnings that appear whenever `MathNetNumerics/Numerics` recompiles. Step 0218's build 01 recorded
    them too.
  - Final build, over the restored source after the mutation runs: 0 warnings, 0 errors (`0219-dev-build-02.log`).
- **Class tests** (`KitLibraryTests`): 30 passed, 0 failed (`0219-dev-class-tests-01.log`). That is 26 before, plus
  4. `--list-tests` discovers the four declared identities.
- **The facts bite.** Seven code mutations of `recordItem` were each rebuilt and run against `KitLibraryTests`, and
  each was caught. The source was then restored byte for byte (`cmp`).
  - The item is written before the snapshot: the three recording facts failed.
  - Only the model reference is snapshotted: the sample and preset facts failed.
  - Only the binding reference is snapshotted: the three recording facts failed.
  - Nothing is snapshotted: the three recording facts failed.
  - Nothing is written: the three recording facts failed.
  - The item is written despite an `UnheldEntryErr`: the preset fact failed, on the missing-model case.
  - The refusal is swallowed as `Ok ()`: the read-only fact and the preset fact failed.

  Log: `0219-mutations/code-mutations.log`.
- **constructor-unit-tests** (the gate's command, whole suite, after build 02): 1312 passed, 0 failed, 0 skipped.
  That is 1308 + 4, in 66 s against the 180 s estimate. The TRX shows all four new identities `Passed`.
  Log: `0219-dev-constructor-unit-tests.log`.
- **The fourteen `invariant-s22-*`:** each `git grep` finds nothing (exit 1). Log: `0219-dev-invariants.log`.
- **elevation** (`ASG_ARC_BASE_COMMIT=f395597c…`, `-ExcludeRules ELEV004`, final source):
  - `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 197 s against a 600 s deadline.
  - The whole scope ran because `Berreman.slnx` changed since the arc origin.
  - Storage shows `findings=0` (suppressed: 150). That is the same as step 0218, so `recordItem` adds no finding,
    ELEV004 included.

  Log: `0219-dev-elevation.log`.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0219-dev-build-01.log`, `0219-dev-build-02.log`
- `0219-dev-class-tests-01.log`
- `0219-dev-constructor-unit-tests.log`
- `0219-dev-invariants.log`
- `0219-dev-elevation.log`
- `0219-dev/KitContentFiles.fs.orig`, `0219-dev/KitLibraryTests.fs.orig` (the pre-step sources)
- `0219-mutations/`, which holds:
  - `mutate_code.py`
  - `code-mutations.log`
  - `KitContentFiles.fs.orig` (the final source the mutations restore)

## Gotchas

- **`observeContentWrites` asserts cumulatively.** Its pin and operation checks compare against everything the
  fixture recorded since it was made. A second item recorded on the same fixture with new snapshots needs a new
  observed context over the cumulative expectation, as the preset fact does for LP1 (`laserExpected @ …`).
- **`readItems` answers in ordinal label order.** `"LP1"` sorts before `"Laser"`, because `'P'` (0x50) comes before
  `'a'` (0x61).
- **The reopen check decodes the model snapshot directly with `toItemModel`.** Once step 222 lands
  `ItemModelProxy.overlaidWith`, that store is the session's model resolver. This step's check is the
  snapshot-level guarantee that store will read.
- **The new private `presetEntryId` is the named form of a one-liner the file already inlines six times** (pre-existing
  facts, each with its own failure message). Those facts were left as they are: rewriting them is outside this step's
  four facts and gains no coverage.
- **The mutation script leaves the last mutant's binaries in `bin/` until the next build.** This round rebuilt
  afterwards (build 02), and the gate engine's `build` gate always runs first.
