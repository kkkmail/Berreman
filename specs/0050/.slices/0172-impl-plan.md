# Step 0172 — impl plan

## Approach

`KitOpening.openKitAt` (step 150) opens or initialises a kit, then reads its experiments and scenes, resolves
snapshot decisions and opens the session overlay, closing the kit on any failure. Open kit… (spec 0050 S5.2) must
open only a folder that is already a kit, so it can offer "Make this a kit" for one that is not.

1. Move the post-open body of `openKitAt`, unchanged, into one private
   `openedOver (context : KitContext) (imports : CatalogueImportContext) : Result<OpenedKit, KitLibraryError>`.
   `openKitAt` keeps its open (initialise-or-open read-write, open-only read-only) and binds into `openedOver`,
   so every answer it gives is unchanged.
2. Add `openExistingKitAt (folder : KitFolder) (clock : ClockProvider) (imports : CatalogueImportContext)`:
   `KitContext.openKit (KitProxy.create folder) ReadWriteKitAccess clock`, its `KitError` mapped to
   `KitLibraryKitErr`, then `openedOver`. It takes no author because it never makes a kit. `openKitAt` gets no
   flag parameter: two functions over one body (.spec-md §4).
3. Add the two `KitOpeningTests` facts the slice names, reusing the class's `withOpeningFolder`,
   `withOpenedKit`, `folderBytes`, clock, author and import context.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitOpening.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitOpeningTests.fs`

## Risks

- `openKit` on a missing folder or one without `.kit-json`: `KitProxy.create`'s `readKitFile` answers
  `NotAKitFolderErr` before any lock or stamp is taken, so nothing is written (step 131).
- A kit held by another proxy in this process: `acquireLock` answers `KitLockedErr` with the live holder, as the
  existing `openKitAt` fact already shows.
- Under invariant 6 this worker runs no gates, so the code is checked by careful reading of the types
  (`KitLibraryContext`, `KitLibraryKitErr`, `KitContext.openKit`).
