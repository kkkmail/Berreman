# Step 0209 — impl log (ADD_CONTRACT STORE_BigBoy_0006 ItemModelProxy)

## Progress

- [x] Read the protocol (`arc-runner.system-md`, `add_contract_worker.system-md`), the project prompt, AGENTS.md,
  slice 0209 and its `.gates`, step 0208's SoW / impl-log, and the 0210 / 0211 briefs that consume this seam.
- [x] Wrote `ItemModelProxyTests.fs` first and appended it to the test project; red build (below).
- [x] Added the seam to `ItemModels.fs`.
- [x] Development runs: build, constructor unit tests, elevation, the fourteen s22 invariant greps.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemModels.fs`
  - The module doc also names the item-model store's seam.
  - After step 208's forms, in the brief's order, each doc citing spec 0050 S7.1:
    - `ItemModelId of Guid` with `value : Guid`;
    - `ItemModelVersionId = { itemModelId : ItemModelId; version : VersionNumber }`;
    - `ItemModelEntry = { id; name : EntryName; description : EntryDescription; model : ItemModelForm;
      protection : EntryProtection }`;
    - `ItemModelVersion = { version : VersionNumber; entry : ItemModelEntry }` with
      `member this.versionId : ItemModelVersionId`;
    - `module ItemModelIds`: the eleven frozen GUIDs, each `Guid.Parse "…" |> ItemModelId`. The eight from the brief's
      list come first, then `sourcePresetItemModelId`, `idealPolarizerItemModelId` and `stackSampleItemModelId`.
    - `ItemModelError` = `DuplicateItemModelErr | InvalidItemModelErr | ItemModelStoreUnavailableErr`, each
      `of reason : string`, with `describe` in step 39's `<condition>: <reason>` form;
    - `[<ReferenceEquality>] ItemModelProxy = { listItemModels; resolveItemModelVersion; saveItemModel }` and
      `static member createInMemory () : ItemModelProxy`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemModelProxyTests.fs` (new)
  - `module private ItemModelProxyTestData`: `Lab retarder` (`RetarderPlateModel ConstantRetardance`, version 2) and
    `Lab polarizer` (`RealPolarizerModel DiattenuationLeakage`, version 1). Both are `UserManaged` and use test-local
    GUIDs.
  - `[<AbstractClass>] ItemModelProxyContract` with `abstract freshItemModels : unit -> ItemModelProxy` and three facts:
    - `an item model is saved at its version and resolved by exactly that version`;
    - `the store lists the latest version of every item model, by name`;
    - `saving an item model the store already holds is DuplicateItemModelErr and keeps the held version`.
  - `ItemModelProxyTests` inherits the contract over `ItemModelProxy.createInMemory ()` and adds
    `every item-model error renders its condition and its payload`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: one `<Compile>` line at the
  end of the compile list, no comment.

## Decisions

- **The test file is a namespace, not a module.** The brief asks for "`module ItemModelProxyTests` in
  `namespace OpticalConstructor.Tests`" and also for `type ItemModelProxyTests () = inherit ItemModelProxyContract ()`.
  - One name cannot be both the module and the class.
  - The census and `ADD_TEST` name the class `OpticalConstructor.Tests.ItemModelProxyTests`, and every other contract
    suite here is a namespace holding the contract and its classes (`SourceLibraryTests.fs`,
    `LibraryBackupProxyTests.fs`, `KitHistoryProxyTests.fs`).
  - So the file is `namespace OpticalConstructor.Tests`, the test data lives in a private module, and the inherited
    facts report under `OpticalConstructor.Tests.ItemModelProxyTests` (all four confirmed in the TRX).
- **The in-memory list orders by `(entry.name, entry.id)`**, the order `SourceProxy.createInDatabase` uses
  (`List.sortBy (fun s -> s.name, s.id)`). Step 211's SQLite store can then pass the same contract.
  - The in-memory store keeps one version per id, because save refuses a held id. Its latest version is the one it
    holds.
- **The fixtures pin the ordering.** `Lab retarder`'s GUID sorts before `Lab polarizer`'s and is saved first, so the
  "by name" fact fails if a store lists in insertion order or by id. The fact also asserts that a fresh store lists
  nothing, which is the contract's `freshItemModels` precondition.
- **`DuplicateItemModelErr`'s reason names the held item model: `'<name>' (<id>) at version <n>`.** The contract fact
  asserts only the id, as the brief does. That leaves step 211 free to phrase its "id and latest version".
- **The in-memory double never answers `InvalidItemModelErr` or `ItemModelStoreUnavailableErr`.** Its doc says so, as
  `SourceProxy`'s does. It holds built-ins too, because step 210's in-memory import pushes the shipped item models
  through it.
- **`ItemModelId` has only `value`.** The brief asks for no `create` or `tryCreate`, and nothing in this step mints
  or parses one.
- **The error-render fact checks each exact text and that the three texts are distinct**, as
  `CatalogueImportProxyTests`' render facts do.

## Testing state

These are development runs only. The gate engine gives the verdict (Invariant 6).

- **Red:** before the domain change, `dotnet build OpticalConstructor.Tests.fsproj -c Release` failed with FS0039:
  `'ItemModelId'`, `'ItemModelVersion'`, `'ItemModelError'`, `'DuplicateItemModelErr'` … not defined. Log:
  `.artifacts/0209-red-build.log`.
- **build:** `dotnet build Berreman.slnx -c Release -nologo -v:m` → exit 0, 0 warnings, 0 errors (51 s).
- **constructor-unit-tests:** 1279 passed, 0 failed, 0 skipped (1275 → 1279, +4). The four new identities are
  `Passed` in the TRX under `className="OpticalConstructor.Tests.ItemModelProxyTests"`.
- **elevation**, run from the arc origin (`ASG_ARC_BASE_COMMIT=f395597c…`, per `.arc-base-json`):
  - `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, exit 0, 192 s;
  - ELEV004 excluded, known findings 0;
  - the arc's `Berreman.slnx` change widens the scope to all 12 projects.
- **The fourteen `invariant-s22-*` greps:** each answers `INVARIANT HOLDS … found nothing`.
- **LF:** `git diff --numstat` equals `--ignore-cr-at-eol`, and neither changed file holds a CR.

## Artifacts

- `specs/0050/.artifacts/0209-red-build.log`
- `specs/0050/.artifacts/0209-build.log`
- `specs/0050/.artifacts/0209-constructor-unit-tests.log`
- `specs/0050/.artifacts/0209-dev-elevation.log`
- `specs/0050/.artifacts/0209-invariant-commands.txt`, `specs/0050/.artifacts/0209-invariants.log`

## Gotchas

- **`.contracts-json` records `STORE_BigBoy_0006` as `declared` with `declaringStep: 216`.** This step (209) declares
  it, and step 211 implements it.
  - The registry is not among this step's authorized edits.
  - Steps 165 and 197 also left it to the arc-runner.
  - So it is not edited. The lifecycle (`declared`) is already right; only the step number differs.
- **`ItemModelVersionId` and `ItemModelVersion` both have a `version` field.** A bare record expression needs its type
  from context (an annotation or a return type), as the tests and `versionId` give it.
- **`saveItemModel` never replaces.** An item model is never edited in this spec. A later version arrives through
  the import seam (step 95), not through this proxy.
