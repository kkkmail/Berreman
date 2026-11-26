# Step 0178 — impl plan (IMPLEMENT: the kit start screen's view)

## Approach

Step 0176/0177 left `KitStartView.fs` holding the model, the messages and `updateKitStart`; this step adds
the pure projection `kitStartView` and its ids, captions and headless tests (obligation S52-VIEW).

1. `OpticalConstructor.Controls/UiIds.fs` — a new group after the product-launcher group
   (`// --- The kit start screen (KitStartView, KitStartWindow). ---`): nine fixed ids
   (`newKitButton` … `kitHistoryList`) and four ids over a row's `ConstructorUiPosition`
   (`kitHistoryRow`, `kitHistoryStarToggle`, `kitHistoryLocateButton`, `kitHistoryRemoveButton`), each with a
   one-line doc. The brief cites `UiIds.fs:588`; the launcher group now ends at line 654, so the group goes
   right after it (before the diagnostic test launcher group).
2. `OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs` — `KitHistoryEntry.lastOpenedCaption`
   (`Last opened yyyy-MM-dd HH:mm UTC`, invariant culture as `KitLockHolder.describe`) and
   `KitFolder.notAKitCaption` (`<folder> holds no kit.`).
3. `OpticalConstructor.Ui/KitStartView.fs` — `kitStartView`: a `ScrollViewer` (both bars Auto) over a
   `MinWidthPanel` (520) over one vertical `StackPanel`: the New kit… / Open kit… row (`verbButton`s calling
   `m.context.picker.pickThen`), the status, the offer (text + one horizontal positive/negative
   `actionButton` pair), the filter `TextBox`, and the history list (a `DockPanel` per row: star toggle,
   Locate… / Remove from history for a missing folder only, on the right; the keyed `Border` with name,
   folder, last-open in the fill; a missing row's Border at opacity 0.5 and inert).
4. `OpticalConstructor.Ui.Tests/KitStartWindowTests.fs` (+ fsproj, last in the compile list, no comment) —
   the seven facts the brief names: five without a trait (ui-tests), two `ui-smoke`.

## Risks

- `actionButton` subscribes its press only on a change of its id (`SubPatchOptions.OnChangeOf id`), so a
  second `MakeKitOffer` of another folder would keep the first folder's handler. `MaterialsWindowView.fs` is
  outside this step's edits, so the offer block is keyed by its offer and folder (`View.withKey`): a new
  folder recreates the pair.
- The picker answers through `Async.StartImmediate`; `kitPickerOf` answers synchronously, so a press
  dispatches before `RunJobs` returns.
- Real kit folders in tests go under the test output (`TestAppScope.freshKitFolder`); every kit shown is
  closed at the fact's end.
- ELEV: no bare primitive in a signature; positions elevated at origin (`ConstructorUiPosition index` bound
  in a `let`).

## Attempt 2 (retry after build exit 1)

- Open `OpticalConstructor.Domain.LibraryEntryFiles` (`KitId`) and `OpticalConstructor.Storage.CatalogueImportStore`
  (`CatalogueImportContext.create`) in `KitStartWindowTests.fs`; these were the three FS0039 errors.
- Compile and run the touched class, and run the ELEV analyzers over the touched projects. Attempt 1's code was never
  compiled, so a second blind repair could fail again.
- Fix any finding inside the authorized files only.
