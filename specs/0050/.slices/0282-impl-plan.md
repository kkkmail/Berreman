# Step 0282 — impl plan

## Approach

The grid panel's edits become semantic Elmish messages over the window model, delegating every rule to step 281's
`SetupGridDraft` members (spec 0050 S8.4; 001 §J, §P). Nothing is drawn: the panel is step 283's.

In `OpticalConstructor.Ui/TableAndElementRotationView.fs`:

1. `TableAndElementRotationModel.setupGridOpt : SetupGrids.SetupGridDraft option`, after `slotChipOpenOpt`, with a
   one-line doc; `initWith` sets it to `None` (the only full-record construction).
2. `member model.setupGrid : SetupGrids.SetupGrid option` on the extension holding `slotChipItems`: the open draft's
   `gridOver model.elements model.kitItems model.tryItemModelChoice`, `None` while closed.
3. Six message cases after `ToggleSlotChip`, each with a one-line doc: `OpenSetupGrid`, `CloseSetupGrid`,
   `ToggleSetupGridItem`, `AddSetupGridAngle`, `RemoveSetupGridAngle`, `ToggleSetupGridCombination`.
4. Their arms after the `ToggleSlotChip` arm. None writes, so no `guardKitWrite`. Open keeps a draft already open,
   close forgets it, and the four edits map the open draft through one private helper and leave the model as it is
   while the panel is closed. `AddSetupGridAngle` converts with `BarDegrees.wrappedAngle` (the table's convention).
   `GenerateSetupGrid` is unchanged.

In `OpticalConstructor.Tests/SetupModelTests.fs`, three facts over step 272's `templateTable` (LP1, QZ at 45°, LPd):
the draft's product and "writes nothing" by kit bytes; untick / re-tick and close-forgets; and generation of the three
ticked setups and experiments with the draft left open.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SetupModelTests.fs`

## Risks

- The slice names `RotationControls.BarDegrees`; `BarDegrees` is declared at namespace level in
  `OpticalConstructor.Controls` (RotationControls.fs:45), not inside the `RotationControls` module, so the qualified
  name does not resolve. The case uses `BarDegrees`, which the file already opens.
- The fixture's sample sits at R1 45°, so a grid of QZ at 45° matches the active table; generation must still list
  three setups (3 new) because the table is not yet a kit setup.
- ELEV: the edit helper is private and takes the model plus a function; keep annotations explicit, names unique.
