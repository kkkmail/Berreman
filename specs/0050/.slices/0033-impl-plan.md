# Step 0033 — IMPLEMENT_CONTRACT STORE_BigBoy_0001 CatalogueProxy — impl plan

## Approach

Step 32 declared `CatalogueProxy` in Domain with its in-memory double. This step adds the real folder implementation in
Storage, as `.spec-md` §5 pins it: the record stays in Domain, and `create` is a static extension in Storage.

1. **`StorageError.describe`** (`Errors.fs`): one line per case. It is the honest render a Domain error's reason carries,
   so no caller formats a `StorageError` with `%A` (ELEV002).
2. **`CatalogueFolderStore.fs`**, compiled directly after `EntryFileJson.fs`:
   - `LibraryFileText.writeAtomically`: write a `.tmp` beside the file, then `File.Move(tmp, file, true)`; a failure
     deletes the `.tmp` and answers `FileIoExn`.
   - `CatalogueProxy.create (folder : FolderName)`: read and write in exactly the slice's order.
3. **`CatalogueFolderTests.fs`**, appended after `CatalogueProxyTests.fs`: the slice's six facts over real folders under
   `AppContext.BaseDirectory`, with the catalogue sealed from the code seeds.
4. **`.contracts-json`**: `STORE_BigBoy_0001` becomes `implemented` at step 33 (the role prompt's order).

## Design choices

- **Nothing in a signature is a bare primitive, so no `[<ElevationBoundary>]` mark is needed.** The slice asks for a mark
  only on a declaration whose signature carries one.
- **The read and write bodies are private extension members on `FolderName`, not free functions.** The analyzers treat
  every assembly but FSharp.Core as "declared here", so `let private f (folder : FolderName)` would be ELEV013P, and the
  gate enforces `P` findings. `SchemaValidation.tryLoadLibrarySchema` is the precedent.
- **Comparing names returns a `FileName option`, not a `bool`.** `FileName -> FileName -> bool` would be ELEV010.
  `tryFindUnnamed` answers both "a row the folder lacks" and "a file no row names".
- **A row's file is matched against the folder's listing.** A manifest row naming `..\x.material-json` matches no
  top-level name, so it is `MissingEntryFileErr` and is never opened.
- **Every text is rendered before the first write.** A serialization failure then leaves the folder untouched.
- **Test fixtures are functions, not module-level values.** A test assembly's module values run at start-up, so a seed
  lookup that failed there would take down all of its facts instead of one.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/Errors.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/CatalogueFolderStore.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/CatalogueFolderTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
- `specs/0050/.contracts-json`

## Risks

- **This round runs no checks** (the role prompt's "you act; you run no checks"), so a compile error, a failing fact or an
  ELEV finding first appears at the gate engine. To reduce that, every API call used here was matched to a compiled
  precedent in the repo, and the ELEV rules were read from STL's analyzer sources.
- **Windows-specific file names.** The fixture names hold `+`, `,`, an en dash, and a `/` that `FileStem.ofText` maps to
  `_`. Those are legal on NTFS, and they exercise the name rule on real files.
