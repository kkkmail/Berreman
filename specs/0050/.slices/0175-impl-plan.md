# Step 0175 — impl plan

## Approach

The app scope composes the kit history once, over the configured preferences folder (S5.1 mapper interpretation 12;
.spec-md §0.2, §3.9).

- `OpticalConstructor.Ui/AppContext.fs`:
  - the record gains `kitHistory : KitHistoryProxy` as its LAST field, with a one-line doc;
  - the private `compose` sets it to `KitHistoryProxy.create locations.preferencesFolder` (Storage's
    `KitHistoryStore`, step 168), so `create` and `recreateLibrary` both carry it;
  - two opens: `OpticalConstructor.Domain.KitHistory` (the proxy type) and `OpticalConstructor.Storage.KitHistoryStore`
    (its `create` extension).

  No other field, option or cache is added. The cap stays `settings.kitHistoryLimit`.
- `OpticalConstructor.Ui.Tests/AppContextTests.fs` gains
  `create composes the kit history over the configured preferences folder`, over `TestAppScope.fileLocations owner`:
  - `ctx.kitHistory.readHistory ()` answers `KitHistory.empty`;
  - a one-entry history written through `ctx.kitHistory.writeHistory` answers `Ok ()`;
  - the locations' `preferencesFolder` then holds `kit-history.json`;
  - a new `KitHistoryProxy.create` over that folder reads it back equal.

`TestAppScope` is unchanged: every test composition already carries `testLocations`' `Preferences` folder under the
test output (step 83).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/AppContext.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/AppContextTests.fs`

## Risks

- `compose` is the only full `AppContext` record literal (every other site copies with `{ ctx with … }`), so adding a
  field breaks no other construction. A grep confirmed it.
- `KitHistoryProxy.create` touches nothing when built, so composing it cannot fail or write. The test asserts the
  preferences folder is still absent before the first write.
- The test fixture's kit folder comes from `TestAppScope.freshKitFolder` (GUID, uncreated). Writing never probes it.
