# Step 0186 — impl plan (IMPLEMENT, S53-KIT-MENU-MESSAGES)

## Approach

The Kit menu's verbs and the forward / inverse switch become semantic Elmish messages over an
injected shell, all in `OpticalConstructor.Ui/TableAndElementRotationView.fs`:

1. After `EditorLaunchers`, a `[<ReferenceEquality>]` record `KitWindowShell = { explorer :
   ExplorerProxy; requestClose : unit -> unit }` (what the Kit menu reaches outside the model), with
   `static member inert` = `ExplorerProxy.createInMemory ignore` and a `requestClose` that does nothing.
2. `TableAndElementRotationModel.kitShell : KitWindowShell` after `kitFolderOpt`; `initWith` sets it
   to `KitWindowShell.inert` (the windows compose the real shell in step 189).
3. `TableAndElementRotationMsg` gains `SwitchConstructorMode of ConstructorMode`, `ShowKitInExplorer`
   and `CloseKit`. `WindowMode.fs`'s `LibraryWindowMode` is a catalogue window's Browse / Select state
   and does not fit, so the switch carries the existing two-case `ConstructorMode`.
4. Arms: `SwitchConstructorMode mode` → `{ model with constructorMode = mode }` only (the chart and
   Data-files gating already follow `constructorMode`); `ShowKitInExplorer` asks
   `model.kitShell.explorer.showFolder` for the kit folder's `FolderName` — `Ok ()` answers the model
   unchanged, `Error e` sets `kitStatus` to `The kit folder could not be shown — {e.describe}`; with no
   folder it answers the model unchanged; `CloseKit` calls `model.kitShell.requestClose ()` and answers
   the model unchanged (the window's `Closed` handler releases the kit, step 154; every kit write is
   already on disk, .spec-md §10.5).

Tests: five facts in `KitWindowModelTests` (step 184's class), exactly the five identities the
census declares, over loaded models whose `kitShell` records the folders shown and the closes asked.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`

## Risks

- Model equality: `KitWindowShell` is reference-equal, so "the model equals the one before" holds only
  for the same shell instance — every arm keeps the model's own shell, and tests compare models built
  from one another.
- New DU case names must be unique across all `Softellect.*` DUs (ELEV016): grep found no
  `SwitchConstructorMode`, `ShowKitInExplorer`, `CloseKit` or `KitWindowShell` anywhere.
- No exhaustive match on `TableAndElementRotationMsg` exists outside the update function.
