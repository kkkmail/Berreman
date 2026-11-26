# Step 0223 — IMPLEMENT — impl plan

## Approach

The constructor window holds its kit's items (spec 0050 S6 closure for 001 §H / §S6, S7.1 mapper request).

1. `OpticalConstructor.Ui/KitOpening.fs`
   - `OpenedKit` gains `items : Item list` after `experiments`, one-line doc.
   - The private `openedOver` reads `context.proxy.readItems ()` right after `readExperiments ()`;
     a refusal flows into the existing failure branch, which closes the kit (releasing `.oc/lock`)
     and answers the error (`KitLibraryKitErr`).
   - `withRebindings` is a `{ this with overlay = ... }` copy, so it keeps `items` unchanged.
2. `OpticalConstructor.Ui/TableAndElementRotationView.fs`
   - `TableAndElementRotationModel` gains `kitItems : Item list` beside `savedScenes` (the kit's loaded
     content), one-line doc; `initWith` sets `[]`.
   - The private `withKitContent` takes `items` after `experiments` and sets `kitItems`.
   - `loadKit` passes `opened.items`; its doc says it loads the items.
   - `ReloadKitChanges` reads `proxy.readItems ()` after `readExperiments ()`; an `Error` keeps the model
     and the offer with the existing "The kit could not be reloaded — …" text.
3. Tests (`OpticalConstructor.Tests`)
   - `KitOpeningTests.opening a kit reads its items, and an item file that does not read refuses the open`.
   - `KitWindowModelTests.a loaded kit's items are the window's, and Reload reads an item another writer wrote`.
   Both build CPL1 / LP1 through `ItemDraft.toItem` over `BuiltinCatalogue.itemModel`, as `KitItemFileTests` does
   (that file compiles after these two and its fixtures are private, so each class keeps its own, per the
   existing per-class fixture pattern).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitOpening.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitOpeningTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`

## Risks

- A name collision on `Item` in the view's opened namespaces (checked: only `Domain.Kit.Item` exists among ours).
- The existing `withRebindings` assertions use `Assert.Same` on library / experimentData; `items` is carried by the
  record copy, so nothing changes there.
- `changedKitFiles` already covers `.item-json` (`KitFileKind.all` holds `ItemKitFile`), so the reload offer needs
  no change; the new window-model fact proves it.
- ELEV: `items : Item list` / `kitItems : Item list` are elevated; no new primitives in signatures.
