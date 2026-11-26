# Step 0211 — impl log (IMPLEMENT_CONTRACT STORE_BigBoy_0006 ItemModelProxy)

## Progress

- [x] Read the protocol, the project prompt, the slice, slices 0209 and 0210, and the store pattern (steps 76 and 79:
  `SampleDatabaseStore`, `SourceDatabaseStore`, `CategoryDatabaseStore`, `PresetDatabaseStore`). No operator note is
  in flight.
- [x] `ItemModelDatabaseStore.fs` and its `<Compile>` line, directly after `PresetDatabaseStore.fs`, with no comment.
- [x] `ItemModelProxySqliteTests`: 3 inherited contract facts and 3 own facts.
- [x] `.contracts-json`: `STORE_BigBoy_0006` set to `implemented`, `implementStep` 211.
- [x] Development runs: build, constructor-unit-tests, elevation and the fourteen s22 invariants.
- [x] State of the world.

## Files modified

- **New:** `Berreman/OpticalConstructor/OpticalConstructor.Storage/ItemModelDatabaseStore.fs`
  - `module ItemModelDatabaseStore` in `namespace OpticalConstructor.Storage`.
  - `type ItemModelProxy with static member createInDatabase (database : LibraryDatabase) : ItemModelProxy`.
  - It carries the slice's exact `ElevationBoundary(StorageBoundary, …)` mark and a two-line doc citing spec 0050
    S7.1.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`: one `<Compile>` line.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemModelProxyTests.fs`: four `open`s and the new
  `ItemModelProxySqliteTests` class. The existing contract and `ItemModelProxyTests` are unchanged.
- `specs/0050/.contracts-json`: `STORE_BigBoy_0006` lifecycle `declared` → `implemented`, `implementStep` `null` →
  `211`. `declaringStep` (216), consumers and temporaries are preserved.

Every edited file is within the step's authorized effects, apart from the arc's own registry, which the
`IMPLEMENT_CONTRACT` role updates.

## Decisions

1. **The store's fields, as the slice specifies.**
   - Each field is one `database.run`. The only write calls `SaveChanges ()` once.
   - Every `StorageError` and every `toItemModel` `LibraryEntryError` becomes `ItemModelStoreUnavailableErr` with its
     `describe`.
   - `listItemModels`: `latestStoredOfKind ItemModelEntryKind`, with no lifecycle filter, decoded into
     `{ version = content.version; entry }`. It is sorted by `(entry.name, entry.id)`, as the in-memory double sorts.
   - `resolveItemModelVersion`: `tryStoredVersion v.itemModelId.toLibraryEntryId v.version`.
     - `None` for no row, or for a row of another kind.
     - Otherwise, that version decoded.
2. **The order of `saveItemModel`'s checks follows the slice's sentence.**
   1. Latest row is an item model: `DuplicateItemModelErr`. The text is
      `'<held name>' (<id>) at version <latest version>`, the same as the in-memory double's.
   2. Latest row is another kind: `InvalidItemModelErr`. The text is
      `item model id '<id>' is held by a <extension> entry`.
   3. `ofItemModel v.version UserProvenance v.entry` refuses (a `ProtectedBuiltIn` entry, say): `InvalidItemModelErr`
      carrying the builder's `describe`.
   4. Otherwise: `stageNewEntry`, then `SaveChanges`.

   So a `ProtectedBuiltIn` entry under a held built-in id answers `DuplicateItemModelErr`, because the held-id check
   comes first. No cache, retry, pin, removal or edit was added.
3. **The tests.**
   - **Contract facts.** Each `freshItemModels ()` opens its own empty `LibraryDatabase.openOrCreate (ConnectionString
     "Data Source=:memory:")`. The class records it in a `ResizeArray`, and its `IDisposable.Dispose` disposes every
     one.
   - **The three own facts** each open `BuiltinCatalogue.freshLibraryDatabase ()`. Two release it with `use`. The
     failure fact disposes it explicitly before calling the fields.
   - **The listing fact also asserts that the Built-in list holds the empty slot.** Otherwise an empty catalogue
     would pass vacuously.
   - **The refusal fact also covers what the slice's title promises**, inside the same fact:
     - the refused `ProtectedBuiltIn` leaves no row;
     - the same entry as `UserManaged` saves under `UserProvenance` and resolves;
     - the Silicon-GUID version answers `None` from `resolveItemModelVersion`, which exercises the
       "row of another kind" branch;
     - Silicon's latest row is still the catalogue's envelope;
     - the empty slot still resolves to the catalogue's item model.
4. **The Silicon id.** The slice asks for "an entry under the GUID of `MaterialIds.silicon`". The test builds it as
   `ItemModelId MaterialIds.silicon.value`. No product member converts a material id into an item-model id, and
   adding one would invent a false bijection (ELEV006). Test projects are outside the ELEV analyzers' scope, and the
   collision is the point of the fact.
5. **TDD order.** The contract facts already existed (step 209). The store and the new class landed in one edit, so
   the "red" state here was only the missing `createInDatabase` symbol (a compile error), which was not captured as a
   separate run.

## Testing state

These are development runs. The arc-runner's gate engine gives the verdict (Invariant 6).

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`):
  - exit 0, 0 errors, 70.7 s;
  - its 2 warnings are the vendored MathNet `SYSLIB0051`, as in manual 005;
  - log: `.artifacts/0211-build-dev.log`.
- **constructor-unit-tests** (the gate command, whole suite):
  - `Test run summary: Passed!`, total 1287, succeeded 1287, failed 0, skipped 0, 66 s;
  - 1281 → 1287 (+6);
  - in the TRX, each of the six slice identities under `OpticalConstructor.Tests.ItemModelProxySqliteTests` is
    `Passed`;
  - log: `.artifacts/0211-constructor-unit-tests-dev.log`.
- **elevation** (run with `ASG_ARC_BASE_COMMIT` set to the `.arc-base-json` origin):
  - `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 197 s, known findings 0;
  - log: `.artifacts/0211-elevation-dev.log`.
- **The fourteen `invariant-s22-*` gates**, each run with its snapshot command:
  - every one reports `INVARIANT HOLDS`;
  - the untracked new store file was grepped directly against the same patterns and holds none of them;
  - log: `.artifacts/0211-invariants-dev.log`.

## Artifacts

- `specs/0050/.artifacts/0211-build-dev.log`
- `specs/0050/.artifacts/0211-constructor-unit-tests-dev.log`
- `specs/0050/.artifacts/0211-elevation-dev.log`
- `specs/0050/.artifacts/0211-invariants-dev.log`

## Gotchas

- **The registry's step numbers disagree.** `.contracts-json` records `declaringStep` 216 for `STORE_BigBoy_0006`,
  although slice 0209 declared it. I kept that declaration metadata as the role requires and set only `lifecycle` and
  `implementStep` (211).
- **`git grep` does not see untracked files**, so the s22 invariants cannot see a new file until it is committed.
  The new store file was checked directly.
- `specs/0050/.manifest.state.json` was already modified by the supervisor when the round started. It was not
  touched.
