# Step 0222 — impl-log (IMPLEMENT, S72-ITEM-MODEL-OVERLAY)

## Progress

### Survey

- Read the system prompts, the project prompt and the slice.
- Surveyed:
  - `KitLibrary.fs`: step 142's private `resolveOverlayReference` and `overlayListingSnapshots`, and the four store
    extensions;
  - `ItemModels.fs`: `ItemModelProxy` and its in-memory double;
  - `LibraryEntryFiles.fs`: `toItemModel` (step 210), `libraryVersionRef` (step 137) and
    `ItemModelVersionId.entryReference`;
  - `KitLibraryTests.fs`: the step 142 overlay facts and the step 219 item facts.
- An item model references no entry (`LibraryEntryContent.references` is `[]` for `ItemModelPayload`). A
  snapshotted item model is therefore its own whole closure.

### Domain (`OpticalConstructor.Domain/KitLibrary.fs`)

- Added `open OpticalConstructor.Domain.ItemModels`. Nothing was shadowed, and the build is clean.
- After step 142's extensions, at the end of the module, added `type ItemModelProxy with member this.overlaidWith
  (overlay : KitLibraryOverlay) : ItemModelProxy`. The type doc cites spec 0050 S7.2 and 001 §E; the member doc
  states the contract.
  - **`resolveItemModelVersion v`** is
    `resolveOverlayReference overlay v.entryReference (store read) (decode >> Result.map Some)`. That is step 142's
    helper, reused unchanged. For an exact-version reference it answers, in order:
    1. the held envelope of a re-bound id;
    2. the kit's snapshot of exactly that `libraryVersionRef`;
    3. the store.
  - **`listItemModels ()`** answers the store's listing, then the kit's snapshot-only item models:
    - they come from `overlayListingSnapshots overlay [ ItemModelEntryKind ]` (one per id at its highest version,
      re-bound ids left out);
    - store-held ids are filtered out;
    - each is decoded, and the result is sorted by `(entry.name, entry.id)`, the in-memory store's own order.
  - **`saveItemModel`** passes through. The extension is a `{ this with … }` copy, so the field is untouched.
  - **`decode`** goes only through `content.toItemModel` and builds `ItemModelVersion { version = content.version;
    entry }`. Its refusal is `ItemModelStoreUnavailableErr $"snapshot {content.describe}: {error.describe}"`, the
    sibling stores' wording.
- `KitLibraryStores` is unchanged. No cache, write-back or other member was added.

### Tests (`OpticalConstructor.Tests/KitLibraryTests.fs`)

- New private helper `userItemModel id name model`: a test-local, user-managed `ItemModelVersion` at v1. It is a
  standalone `OpticalElementModel`, never a preset or sample binding.
- **New fact** `a kit's item models resolve from its snapshots against an empty library`:
  - `snapshotReferences` of the retarder-plate and circular-polarizer v1 references (their `entryReference`) writes
    exactly those two Built-in envelopes;
  - the kit reopens read-only over the same folder against `CatalogueImportProxy.createInMemory ()`;
  - `(ItemModelProxy.createInMemory ()).overlaidWith (openOverlay [])` resolves each version to
    `BuiltinCatalogue.itemModel` of its id and answers `None` for v2;
  - `listItemModels` answers `[ circular; retarder ]`, by name; the references were given in the other order;
  - the underlying store stays empty, the folder is unchanged and the empty seam holds nothing.
- **New fact** `the overlaid item-model store answers the kit's divergent version and passes a save through`, over
  `BuiltinCatalogue.freshStores ()`:
  - the kit holds three snapshots: the circular polarizer v1 with another description, and two kit-only user
    models, "A kit-only wave plate" (id …0003) and "B kit-only polarizer" (id …0001), whose name order is the
    reverse of their id order;
  - `resolveItemModelVersion` of circular v1 answers the kit's description, while the store still answers the
    shipped one;
  - `listItemModels` is the store's listing (= `BuiltinCatalogue.itemModels ()` sorted by name and id, all eleven)
    followed by `[ A; B ]`, with exactly one circular polarizer, the store's;
  - `saveItemModel` of a test-local user model through the overlay is held by `held.itemModels`. Saving the circular
    polarizer through it is the store's own `DuplicateItemModelErr`. The kit's snapshots are unchanged.
  - Re-binding (S4.2 rule): the kit-only wave plate re-bound to the circular polarizer resolves to the shipped model
    and leaves the listing.
  - Refusal: a kit snapshot without a description is
    `ItemModelStoreUnavailableErr "snapshot <describe>: <refusal describe>"`, from both `resolveItemModelVersion` and
    `listItemModels`.
- **Extended step-219 helper** `assertItemsReopenAgainstEmptyLibrary` (assertions added, none removed): the reopened
  kit now resolves each item's model through `empty.itemModels.overlaidWith overlay` to the shipped model. That
  covers the section's "reopening against an empty library MUST resolve the model from its .item-model-json
  snapshot" for the CPL, stack-sample, source-preset and ideal-polarizer items.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitLibrary.fs` (+40)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitLibraryTests.fs` (two new facts, one helper, one
  extended helper)

## Testing state

These are development runs over the final source. The gate engine gives the verdict (Invariant 6). Logs are in
`specs/0050/.artifacts/0222/`.

- **build** (`dotnet build Berreman.slnx -c Release`):
  - first build: exit 0, 0 errors; the only warnings were the two existing vendored MathNet `SYSLIB0051`;
  - final rebuild, after the mutants were restored: **0 warnings, 0 errors** (`build-final.log`).
- **constructor-unit-tests** (all discovered): **1321** passed, 0 failed, 0 skipped. That is 1319 + 2, the two new
  `KitLibraryTests` facts.
  - In the TRX, both new identities are `Passed`.
  - So are the three step-219 item facts whose reopen helper was extended.
- **elevation** (`ASG_ARC_BASE_COMMIT` = arc origin `f395597`, ELEV004 excluded):
  - `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 183 s;
  - suppressed findings stay at 1311, unchanged from step 0221 (measured, not gated);
  - `KitLibrary.fs` is byte-identical to the analyzed version. Only the test file, outside ELEV scope, changed
    afterwards.
- **The fourteen `invariant-s22-*` must-find-nothing greps:** each finds nothing.
- **The facts bite.** Each mutant was applied to `KitLibrary.fs`, then `KitLibraryTests` (32 facts) was run. The
  file was restored byte for byte (checked with `cmp`) before the final build. Logs are in `.artifacts/0222/mutants/`.
  - **m1:** `resolveItemModelVersion` answering only the store fails both new facts and the three extended
    step-219 reopen facts.
  - **m2:** the snapshot-only listing sorted by id alone fails the divergent-version fact, where B …0001 sorts
    before A …0003.
  - **m3:** without the store-held filter, the listing shows a second circular polarizer and fails the
    divergent-version fact.
  - **m4:** listing the snapshot-only models before the store's fails the divergent-version fact.
- Not in this step's roster: `unit-tests` (no solver code changed), `ui-smoke` and `ui-tests` (no UI code changed).

## Artifacts

- `specs/0050/.artifacts/0222/`:
  - `build.log` and `build-final.log`;
  - `constructor-unit-tests.log` and `constructor-unit-tests-final.log`;
  - `elevation.log`.
- `specs/0050/.artifacts/0222/mutants/`: `mutate.py`, the original-source backup, and each mutant's build and test
  logs.

## Gotchas

- **"the store's eight item models" is stale.** The slice says the Built-in store holds eight item models. The
  Built-in catalogue has shipped **eleven** since the operator's `0050-210-manual` commit (90ab50b). That commit
  added the three binding forms: light source, ideal polarizer and sample by stack.
  - The fact therefore asserts the store's own listing, equal to `BuiltinCatalogue.itemModels ()` sorted by name and
    id (eleven), followed by the kit-only models.
  - The spec's intent is kept: the store's models first, and no second circular polarizer.
- **Exact-version semantics.** `resolveItemModelVersion` of a re-bound id answers the held entry's latest envelope,
  whatever version was asked for. This is step 142's rule, shared with `MaterialProxy` and `SampleProxy`.
