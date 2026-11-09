# Step 0225 — impl plan

## Approach

Put Copy experiments from another kit… behind four semantic messages of the kit window, over step 224's
`KitContext.copyExperimentsFrom`. Model and update only; the panel and the verb are step 226's.

In `OpticalConstructor.Ui/TableAndElementRotationView.fs`:

1. `KitWindowShell.kitPicker : KitFolderPicker`, the folder dialog that chooses the source kit. `inert` answers
   `None` at once.
2. `KitCopySource = { source : KitContext; experiments; chosen }`, declared before the model, with a `toggled`
   member: it chooses or unchooses an id, keeping the source's order. An id the source does not hold is never
   chosen.
3. `TableAndElementRotationModel.kitCopyOpt : KitCopySource option` after `kitSend`; `initWith` sets `None`.
4. `TableAndElementRotationMsg` gains `OpenCopySource of KitFolder`, `ToggleCopiedExperiment of ExperimentId`,
   `CopyChosenExperiments` and `CloseCopySource`.
5. Update arms:
   - **`OpenCopySource`:** behind `guardKitWrite`.
     - It refuses the window's own folder by `canonical`.
     - It closes any open source, then opens the folder read-only with `KitContext.openKit (KitProxy.create
       folder) ReadOnlyKitAccess model.kit.clock` and reads its experiments.
     - An error closes what was opened and tells `The kit could not be opened to copy from — <e>`.
   - **`ToggleCopiedExperiment`:** goes through `KitCopySource.toggled`; it does nothing without a source.
   - **`CopyChosenExperiments`:** behind `guardKitWrite`, and only with a source and a non-empty `chosen`.
     - It calls `model.kit.copyExperimentsFrom`, then `readExperiments`.
     - On success it updates the content through `withKitContent` (the share of `loadKit` and Reload) over
       `model.kit`, the experiments read, `model.kitItems` and `model.savedScenes`. It closes the source and tells
       `Copied <n> experiment(s) from <name>.`
     - An error keeps the panel and tells `The experiments could not be copied — <e>`.
   - **`CloseCopySource`:** closes the source and sets `kitCopyOpt` to `None`.
   - A private model member closes the open source; a failed close is logged. A read-only close releases nothing,
     so the failure path never fires in practice.
   - The file needs `open OpticalConstructor.Storage.KitFolderStore` for `KitProxy.create`.

In `OpticalConstructor.Tests/KitWindowModelTests.fs`, five facts over folder kits loaded into `initMain ()`:

- The source is a second folder kit (`openKitAt`, read-write) under the fact folder. It holds E1, a dark line, and
  E2, with a data file attached through `attachLabFile` from outside the kit.
- The source is closed, and its `.oc` app-cache folder (left by the read-write lock) is removed. The facts can then
  see that a read-only open creates none.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`
- `KitWindowTests.fs` is authorized but needs no change: every shell there is `KitWindowShell.inert with …`.

## Risks

- ELEV:
  - new names (`KitCopySource`, the four cases) are unique (git grep);
  - the helper that closes the source is a member, not a free single-parameter function (ELEV013);
  - no bare primitives in signatures.
- A full-model equality in an existing fact could break if the update touches `kitCopyOpt`. The existing arms never
  touch it.
- `withKitContent` clears `kitStatus` and `kitChanges`. The copy's status is set after it.
