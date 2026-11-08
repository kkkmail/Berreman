# Step 0169 — IMPLEMENT — impl-plan

## Approach

Opening a kit bumps the kit history; the 16-entry `recent.json` store goes (spec 0050 S5.1; 001 §F, §R).

1. **The move.** The pure MRU update (drop the case-insensitive duplicate, put the new one first, truncate) moves
   from Storage's recent-files module into `KitHistory` as
   `member this.bump (limit : KitHistoryLimit) (clock : ClockProvider) (folder : KitFolder) (kitFile : KitFile) : KitHistory`,
   generalised from file names to entries:
   - the matching entry's star is read (the one folder comparison, ordinal ignoring case);
   - `forget folder` removes every entry of that folder;
   - a new entry `{ folder; kitId = kitFile.id; name = kitFile.name; lastOpened = clock.now (); star }` goes first,
     the clock read once, the star the removed entry's or `UnstarredKit`;
   - `evict limit` runs where the truncation ran.
   The single folder comparison becomes a private static `tryMatchFolder` answering the entry option (no `bool`
   beside our types), used by both `changeEntry` and `bump`, so the comparison still lives in one place. The
   `changeEntry` doc stops naming the deleted module.
2. **The deletion.** `OpticalConstructor.Storage/RecentFiles.fs` and its `<Compile>` line go. The comment above that
   line keeps only the words true of `Export.fs` and the material-library module that now follow it.
3. **The docs.** `UserEnvironment.fs` (module doc and `EnvironmentSettings` doc) and `EnvironmentRoundTripTests.fs`
   (`sample` doc and the old-file section comment) say recently opened kits are kept by the kit history
   (`KitHistoryProxy`, spec 0050 S5.1), not here. No behaviour or assertion changes.
4. **Tests.** `HistoryTests.fs` and its `<Compile>` line go (DELETE_TEST → KitHistoryTests). `KitHistoryTests` gains
   the migrated fact `bump puts the opened kit first, matches its folder in any case and evicts beyond the limit`
   (stepping clock, `KitFile.create` per folder) and the three new facts named by the slice. Net +3.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitHistory.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/RecentFiles.fs` (deleted)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/UserEnvironment.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/HistoryTests.fs` (deleted)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitHistoryTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/EnvironmentRoundTripTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Risks

- ELEV rules on the new member: it is a member on our type (primitive-signature rules exempt), takes four of our
  types, and returns ours. The private comparison answers `KitHistoryEntry option`, not `bool`.
- The `git grep` for the retired names must find nothing across `Berreman/`, docs included; the test file must not
  name `maxRecent` either.
- Act-only role (invariant 6): no gate is run here; the arc-runner measures the build and tests.
