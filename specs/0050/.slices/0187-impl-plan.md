# Step 0187 — impl plan

## Approach

Step 0187 adds the kit window's reload offer to the model (spec 0050 S5.3; 001 §E, §O). The step-130 stamps already
answer which kit files changed outside the app (`changedKitFiles`). The offer is model state and three semantic
messages; the kit bar that renders it is step 188, and the activation hook is step 189.

1. `KitCheckOrigin` (`KitCheckOnActivation` | `KitCheckOnRequest`) before the model.
2. `TableAndElementRotationModel.kitChanges : FileName list` after `kitShell`; `initWith` sets `[]`.
3. `TableAndElementRotationMsg`: `CheckKitChanges of KitCheckOrigin`, `ReloadKitChanges`, `KeepMyKitChanges`, after
   `CloseKit`.
4. One private function `withKitContent` (the kit context, its experiments, its scene names). It sets:
   - `kit`;
   - `experimentCollection` (`ExperimentCollection.ofLoaded`), `savedScenes` and `experimentDataStatus` (through
     `validateExperimentAttachment`);
   - `kitStatus = None` and `kitChanges = []`.
   `loadKit` sets `kitFolderOpt` and `experimentData` itself, then calls it with `opened`'s content. So a reload sets
   exactly what an open sets, and the status check reads the opened kit's `experimentData`, as before.
5. One private guard `guardKitFolder`, the `guardKitWrite` precedent: a model with no kit folder answers the three
   messages unchanged. The arms follow the how-to:
   - check: `changedKitFiles`; `Ok []` clears the offer and, on request only, says so; `Ok files` replaces the offer;
     `Error` tells the check's failure.
   - reload: `readKitFile`, `readExperiments`, `readSceneNames`, then `recordStamps`, in a `Result.bind` chain; `Ok`
     goes through `withKitContent` with `{ model.kit with kitFile = … }`; the first `Error` keeps the model and offer
     and tells it.
   - keep mine: `recordStamps`; `Ok ()` clears the offer; `Error` keeps it and tells the check's failure.
6. The five declared facts in `KitWindowModelTests`. The other writer is `KitProxy.create folder` (needs
   `open OpticalConstructor.Storage.KitFolderStore`). `E2` is the experiment committed through the model, with
   `id = ExperimentId 2`. The kit file is renamed `Renamed` through `readKitFile` / `writeKitFile`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`

## Risks

- **`loadKit` callers.** `InverseConstructorTests`, `SceneSaveLoadTests`, `MainWorkbenchTests`, `KitWindowTests` and
  `Program.fs` call `loadKit`. Two `InverseConstructorTests` facts pass `{ opened with experimentData = … }`, so the
  validation must read the model's `experimentData` after `loadKit` has set it. It does, because `loadKit` sets
  `experimentData` before calling the shared function.
- **No exhaustive match elsewhere.** No other code matches `TableAndElementRotationMsg` exhaustively (checked by grep),
  so the three new cases break no match site.
- **Act-only.** No gate is run (Invariant 6). Compile-safety rests on reading every API's existing use.
