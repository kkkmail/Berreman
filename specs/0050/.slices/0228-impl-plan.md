# Step 0228 — impl plan

## Approach

Put the copy panel's item mapping into the kit window's semantic messages (spec 0050 S7.2, obligation
`S72-COPY-MESSAGES`), over step 221's `ItemCopyRow` / `ItemCopyChoice` and step 227's
`copyExperimentsFrom … itemRows`.

1. `OpticalConstructor.Ui/TableAndElementRotationView.fs`
   - `KitCopySource` gains `items : Item list` (read when the source opens) and `itemRows : ItemCopyRow list`
     (the mapping table), one-line docs citing S7.2.
   - `TableAndElementRotationMsg` gains `ChooseCopiedItem of ItemId * ItemCopyChoice`, one-line doc.
   - `OpenCopySource` reads `readItems ()` after the experiments; an `Error` releases the source and shows step 225's
     text. The source opens with its items and no row.
   - `ToggleCopiedExperiment` toggles, then resolves `Experiment.itemsUsedBy` over the toggled choice's experiments
     against `items`. `Ok used` sets `itemRows = ItemCopyRow.copyRowsFor used model.kitItems itemRows`; `Error` keeps
     the previous copy source (selection and rows) and reports the typed `ItemError` in `kitStatus`.
   - `ChooseCopiedItem (id, choice)` sets the choice of the row whose `sourceItem.id` is `id`; without such a row or a
     source the model is returned unchanged.
   - `CopyChosenExperiments` passes `itemRows`; an `Ok` that added items says
     `Copied <n> experiment(s) and added <m> item(s) from <name>.`, otherwise step 225's text; an `Error` keeps the
     panel with step 225's text.
2. Tests
   - `KitWindowModelTests`: refactor step 225's `copySourceUnder` into a shared folder-kit maker so the new item
     source reuses it; add the item fixtures (Laser600 / QZ in the source, the target's Laser600, a dial-mounted
     same-preset bench piece) built from step 210's shipped item models through `ItemDraft.toItem`; add the four
     census facts; the read-only copy fact's `KitCopySource` record gains `items = []` and `itemRows = []`.
   - `ScrollExtentTests.tallestKitBar` (step 226) is the other record expression that builds a `KitCopySource`; it
     gains the two empty fields, its assertions unchanged (the brief's rule covers it; see the impl-log).

## Risks

- The brief names "KitWindowTests' open-source fact" as a record expression; it builds none (it dispatches
  `OpenCopySource`). The actual second record expression is in `ScrollExtentTests.fs`, outside the listed edits, and
  the build cannot pass without it. Recorded as a deviation.
- A stale `kitStatus` from a refused toggle is left as is on a later good toggle: the brief adds no status for a
  good toggle, and step 225's toggle never touched the status.
