# Step 0309 — impl plan

## Approach

Step 308 left the column-mapping wizard as a pure MVU (`ImportWizardView.fs`) with no view. This step adds its
projection, `importWizardView`, under fifteen stable `ConstructorUiId` members, with the Domain-derived keys and
captions it needs as extension members in `ConstructorUiIdOfDomain.fs`. It follows step 107's `importDialogView`:

- a `DockPanel`;
- Attach and Cancel docked at the bottom in one horizontal row, using `MaterialsWindowView.actionButton` with the
  positive and negative fills;
- the refusal `TextBlock` docked above that row;
- the heading and the choice blocks docked at the top;
- a `ScrollViewer` filling the rest.

1. **`UiIds.fs`.** A group after the import dialog's, under one comment line citing spec 0050 S9.2, with the fifteen
   members and their exact texts. Each member has a one-line doc.
2. **`ConstructorUiIdOfDomain.fs`.** Add these extension members, each with a one-line doc:
   - `DataDelimiter.uiKey` and `.caption`;
   - `HeaderRowChoice.uiKey` and `.caption`;
   - `ByteOrderMark.caption`;
   - `ImportProfileName.uiKey` and `.caption`;
   - `DataColumnNumber.uiPosition`;
   - `ImportProfileError.caption`;
   - `ColumnRoleChoice.caption`;
   - `ScanColumnUnit.caption` and `ScanReading.caption`.

   They are qualified with `Kit.` and `ImportProfiles.`, not opened, so nothing in the file is shadowed.
3. **`ImportWizardView.fs`.** Append `importWizardView`:
   - **Top `StackPanel`.** The heading (`Import <leaf>` plus the "Nothing is attached…" line). Then the `Layout:` and
     `Profile:` blocks. Each block is a `DockPanel` with a left label and a `WrapPanel` fill, because AGENTS.md
     forbids a WrapPanel in a horizontal StackPanel. Under `NewColumnMapping`, the `Profile name:` row follows.
   - **Choice boxes.** Each chosen one is an `actionButton` on `positiveBackground`; the others are `verbButton`s.
   - **Preview.** A two-way `ScrollViewer` holds a `Grid` with one `Auto` column per preview column. Its rows are the
     names, then the role row (under `NewColumnMapping` only), then the data rows. A preview error shows its caption
     in place of the grid.
   - **Role, unit and reading choices.** These are `ComboBox`es over caption strings. Each handler is resubscribed on
     every patch (`SubPatchOptions.Always`), so it reads the current role. It dispatches only when the picked index
     differs from the shown one, so a programmatic selection never loops back as a message.
4. **`ImportWizardTests.fs`.** Add the four facts the slice names. They mount the view headless as `ImportDialogTests`
   does, with `Program.mkSimple` and `Program.withHost` on a `HostWindow`. They drive it by pressing boxes and setting a
   ComboBox's `SelectedIndex`, so the view's own dispatch is exercised.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ImportWizardView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ImportWizardTests.fs`

## Risks

- **No FuncUI `ComboBox` precedent in this repo.** The FuncUI 2.0 DSL names (`dataItems`, `selectedIndex`,
  `onSelectedIndexChanged`) are checked by the build.
- **Name collisions (ELEV016 / ELEV019).** Every new member name was checked against the existing extensions on each
  receiver. None is held.
- **`actionButton` subscribes its press once per id** (`OnChangeOf id`). Every wizard box sends a fixed message per id,
  so a stale closure cannot send a wrong message.
