# Step 0165 — impl plan (ADD_CONTRACT STORE_BigBoy_0005 KitHistoryProxy)

## Approach

Declare the kit history's seam in `OpticalConstructor.Domain/KitHistory.fs`, after the `KitHistory` record
(step 164), in the same `KitHistory` module:

1. `KitFolderPresence` = `KitFolderPresent` | `KitFolderMissing` — never stored, so no wire tag.
2. `KitHistoryError` = `KitHistoryWriteErr of reason : string`, with `describe` rendering
   `the kit history could not be written: <reason>` (step 39's fixed-phrase, colon, reason form).
3. `[<ReferenceEquality>] KitHistoryProxy` with exactly `readHistory`, `writeHistory`, `probeFolder`, each with a
   one-line doc (the `SourceProxy` convention, `ElementId.fs`).
4. `type KitHistoryProxy with static member createInMemory (presentFolders : KitFolder list)`: a closure-private
   `ref` starting at `KitHistory.empty`; write keeps the history and answers `Ok ()`; read answers the last kept;
   the probe answers `KitFolderPresent` for a folder equal to one of `presentFolders`, else `KitFolderMissing`.

Tests:

- New `OpticalConstructor.Tests/KitHistoryProxyTests.fs` (appended last in the compile list, no comment):
  `[<AbstractClass>] KitHistoryProxyContract` (abstract `freshHistory`, `presentFolder`, `missingFolder`) holding
  the four store facts over a test-local two-entry history with tick-carrying instants; `KitHistoryProxyTests`
  inherits it over `KitHistoryProxy.createInMemory [ presentFolder ]`.
- `KitHistoryTests` gains `a kit history error renders its condition and its reason`.

Count rises by 5 (4 inherited facts under `KitHistoryProxyTests` + 1 in `KitHistoryTests`).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitHistory.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitHistoryProxyTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitHistoryTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Risks

- ELEV uniqueness (014/016): the new type and case names were grepped and are unused elsewhere.
- ELEV013: `createInMemory` is a static member, not a free function.
- The probe's equality is structural (`KitFolder` equality), as the brief says "equal to"; the history's own
  case-insensitive folder match stays in `changeEntry` and is not reused here.
- The contract registry (`.contracts-json`) already records `STORE_BigBoy_0005` declared at step 165; it is not in
  the authorized edits and is left untouched.
