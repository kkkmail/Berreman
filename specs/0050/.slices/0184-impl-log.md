# Step 0184 — IMPLEMENT — impl-log

## Progress

- [x] Read the protocol (the base and the `IMPLEMENT` delta), the project prompt, the slice and its gate snapshot.
  Also read:
  - `KitOpening.fs`, `TableAndElementRotationView.fs` (the model, `initWith`, the `init*` seeds, `loadKit`);
  - `App/Program.fs`;
  - `Domain/Kit.fs` (`KitName`, `KitFolder`, `KitAccess`) and `Domain/KitFolders.fs` (`KitContext`);
  - STL's `FolderName` (`Sys/Primitives.fs`);
  - the existing headless constructor-window facts (`WireUiFinalCompositionTests`, `LauncherTests`,
    `AppContextTests`, `TableAndElementRotationTests`) and `TestAppScope`;
  - `.manual/013` (arc-origin elevation scope).
- [x] `KitOpening.fs`: `OpenedKit.folder`.
- [x] `TableAndElementRotationView.fs`: `kitFolderOpt`, `kitCaption` and `kitWindowTitle`.
- [x] `Program.fs`: the title follows the model.
- [x] Tests: `KitOpeningTests`, `KitWindowModelTests` and `KitWindowTests`.
- [x] State of the world.

### Attempt 02 — build repair

The supervisor's `build` gate failed on one error:
`KitWindowModelTests.fs(28,46): error FS0039: The type 'CatalogueImportContext' does not define a field,
constructor, or member named 'create'`.

- Cause: `CatalogueImportContext.create` is a type extension in `OpticalConstructor.Storage.CatalogueImportStore`
  (`CatalogueImportStore.fs:222`). `KitWindowModelTests.fs` never opened that module; `KitOpeningTests.fs` does.
- Repair: `KitWindowModelTests.fs` gains `open OpticalConstructor.Storage.CatalogueImportStore`. Nothing else
  changed. The open is inside the step's authorized effects and keeps every fact and assertion.
- The failed build compiled every production project and `OpticalConstructor.Ui.Tests` (so `KitWindowTests.fs`
  compiles). `OpticalConstructor.Tests` reported only this error. Every other name in `KitWindowModelTests.fs` was
  re-checked against its declaration:
  - `ClockProvider` and its `system` extension (`Domain/Kit.fs:19`, `Ui/KitOpening.fs:25`);
  - `KitAuthor`, `KitError` and `KitFolder` (`Domain/Kit.fs`);
  - `openKitAt`'s argument order (`Ui/KitOpening.fs:122`);
  - `initMain`, which returns `Result<_, KitError>`, and `loadKit`, which returns the model;
  - `InverseConstructor` (`TableAndElementRotationView.fs:255`) and `KitContext.close` (`Domain/KitFolders.fs:443`).
- `initMain ()` seeds the kit from `scratchKit ()`, which is named `Scratch kit`, read-write and holds no lock. Its
  `close ()` succeeds (`KitOpeningTests.the scratch kit is an empty in-memory kit opened read-write` closes two).
- No production code or test outside the step's effects still names the old literal titles. The repo grep finds
  `Constructor — Main` and `Constructor — Inverse` only in `KitWindowModelTests.fs`.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitOpening.fs`
  - `OpenedKit` gains `folder : KitFolder` as its first field, with the one-line doc "The folder the kit was opened
    from (spec 0050 S5.3)."
  - The private `openedOver` takes `(folder : KitFolder)` first and sets `folder = folder` in the record it builds.
  - `openKitAt` and `openExistingKitAt` pass their own `folder` parameter, which is the folder they opened.
  - `withRebindings` is unchanged: `{ this with overlay = overlay }` already keeps `folder`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
  - `TableAndElementRotationModel` gains `kitFolderOpt : KitFolder option` directly after `kit`, documented in one
    line.
  - `initWith` sets `kitFolderOpt = None`, and `loadKit` sets `kitFolderOpt = Some opened.folder`.
  - The extension that holds `loadKit` gains two members, each documented in one or two lines that cite spec 0050
    S5.3:
    - `member model.kitCaption : ControlCaption` is `kit.kitFile.name.value`, then ` (read-only)` for
      `ReadOnlyKitAccess`, then ` — ` and `folder.value.value` when `kitFolderOpt` is `Some`.
    - `member model.kitWindowTitle : ControlCaption` is `kitCaption`, then ` — Optical Constructor — `, then `Main`
      (`ForwardConstructor`) or `Inverse` (`InverseConstructor`).
- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`
  - `ConstructorScene.mount` runs a local `titledView`. It sets `window.Title <- model.kitWindowTitle.value` and then
    answers `TableAndElementRotationView.mainView model dispatch`. `Program.mkSimple` takes `titledView` in place of
    `mainView`.
  - A one-line comment above `titledView` cites spec 0050 S5.3.
  - `MainConstructorWindow` and `InverseConstructorWindow` no longer set literal titles. Each type doc gains one
    line: "Its title names the kit, its access, its folder and the constructor (spec 0050 S5.3)."
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitOpeningTests.fs`
  - New fact `an opened kit carries the folder it was opened from`, using three fresh `withOpeningFolder` folders,
    one for each open:
    1. `openKitAt` read-write answers `folder`.
    2. The kit is created read-write and closed, then `openKitAt` read-only answers `folder`; the fact also asserts
       the access.
    3. The kit is created and closed, then `openExistingKitAt` answers `folder`.
  - Every open is closed: the first two by `withOpenedKit`, the third in a `finally`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs` (new)
  - `module KitWindowModelTests` in `namespace OpticalConstructor.Tests`.
  - `withTitledFolder`: the kit folder is `<BaseDirectory>/KitWindowModelTests/<guid>/Optics QZ`, uncreated. The
    import context is over `BuiltinCatalogue.freshStores ()`. The GUID folder is deleted at the end.
  - `withLoadedScene`: the model is `initMain ()` loaded with `loadKit opened`. Both the scene's scratch kit and the
    opened kit are closed in a `finally`.
  - Kits are opened through `openKitAt` with `ClockProvider.system` and `KitAuthor "test"`.
  - It opens `OpticalConstructor.Storage.CatalogueImportStore` for `CatalogueImportContext.create`. Attempt 02
    added that open.
  - Facts:
    - `the window title names the kit, its folder and the constructor` asserts
      `Optics QZ — <folder> — Optical Constructor — Main`. With `constructorMode = InverseConstructor`, it asserts
      `… — Inverse`. It also asserts `kitFolderOpt = Some folder`.
    - `a kit opened read-only is marked in the title`: the kit is created read-write and closed, then opened again
      read-only. The fact asserts `Optics QZ (read-only) — <folder> — Optical Constructor — Main`.
    - `a scene that opened no folder is titled by its kit alone`: `initMain ()` is titled
      `Scratch kit — Optical Constructor — Main`, and its `kitFolderOpt` is `None`. Its scratch kit is closed.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: `KitWindowModelTests.fs` is
  appended after `ExplorerProxyTests.fs`, with no comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs` (new)
  - `module KitWindowTests` in `namespace OpticalConstructor.Ui.Tests`. Its one fact has no trait:
    `a constructor window is titled by its kit`.
  - Inside `HeadlessSession.run`, the fact builds `TestAppScope.freshAppContext (FolderName "KitWindowTests")`.
  - A `MainConstructorWindow` is built over `TestAppScope.openedKitIn owner ctx.catalogueImports`, shown, and the
    dispatcher jobs are run. Its `Title` must equal `<kit name> — <folder text> — Optical Constructor — Main`.
  - An `InverseConstructorWindow` is built over a second such kit and checked the same way, ending `— Inverse`.
  - Each window is closed in a `finally`. The fact then asserts that the kit's `.oc/lock` is gone, which shows the
    close released the kit (step 154).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`: `KitWindowTests.fs`
  is appended after `KitStartWindowTests.fs`, with no comment.

These authorized test files are untouched:

- `AppContextTests.fs`, `LauncherTests.fs` and `TableAndElementRotationTests.fs`;
- `WireUi0040CompositionTests.fs`, `WireUiCompositionTests.fs` and `WireUiFinalCompositionTests.fs`.

No fact asserted the old literal titles (`grep "Constructor — Main|Constructor — Inverse"` finds nothing in tests).
Every other fact that builds a constructor window keeps its code and its assertions.

## Decisions

- **The folder text is the folder exactly as opened**, `folder.value.value`. It is not `canonical`, which is a key
  that is upper-cased and never shown (`Kit.fs`). So the title shows the path the user picked, as the slice's
  example `C:/kits/Optics QZ` does.
- **The title is set inside the view function.** The slice prescribes this, and it means the title follows every
  model change. Nothing caches or compares the title, and no setting or format option was added.
- **`titledView` is a local closure inside `mount`.** It closes over the `window` that `mount` receives, so both
  windows share it. It is fully annotated, including `Avalonia.FuncUI.Types.IView` as the return type, because
  Program.fs does not open `Avalonia.FuncUI.Types`.
- **"over one folder each"** (the `KitOpeningTests` fact) was read as one fresh folder for each of the three opens.
  The read-only and existing-kit opens first create the kit read-write and close it, because neither initializes a
  folder.
- **The model facts close the `initMain ()` scratch kit too.** The brief says "every kit a fact opens is closed at
  its end". The scratch kit's close is a lock release over an in-memory kit, which `KitOpeningTests` already shows
  succeeds.
- **The UI fact asserts the whole title**, not just its ending. That covers "ends `— Inverse`" and the
  `<name> — <folder>` prefix in one assertion. Checking the lock release after close is a direct observation of
  "both windows are closed, which releases their kits".
- **ELEV.** These checks were done by reading the analyzer rules and the diff. No analyzer was run.
  - The record fields `folder` and `kitFolderOpt` are elevated (ELEV008).
  - `kitCaption` and `kitWindowTitle` are members, which may speak their type's primitives. They are new names on
    this receiver (ELEV019).
  - `openedOver`'s added parameter is a `KitFolder` (ELEV003P).
  - `.value` appears only where text reaches Avalonia's `Title` or an interpolated caption. That is ELEV004, which
    the gate excludes.
  - No public module-level name, type or DU case was added (ELEV014/015/016).

## Testing state

No build, test, analyzer or invariant gate was run in either attempt. The `IMPLEMENT` role is act-only
(Invariant 6), so these belong to the arc-runner's gate engine after this worker exits. Attempt 02 repaired the one
error the supervisor measured in `build` (see Progress, Attempt 02). The gates are:

- build and elevation;
- constructor-unit-tests (all discovered);
- ui-smoke (all discovered);
- ui-tests (`AppContextTests`, `InverseConstructorTests`, `KitWindowTests`, `LauncherTests`, `SceneSaveLoadTests`,
  `TableAndElementRotationTests`);
- the fourteen s21 invariants.

Checked without running a gate:

- Every edited and new file is LF (byte count: zero CRLF), and `git diff --check` is clean.
- No added line contains any s21 invariant's word.
- Every API used was checked against its source or an existing use:
  - `OpenedKit` is built only in `openedOver`.
  - `TableAndElementRotationModel` is built in full only in `initWith`.
  - `CatalogueImportContext.create (…).proxy` has the same shape as `initWith`.
  - `try … finally` matches `KitOpeningTests.withOpenedKit`.
  - `#Window` matches `WireUiFinalCompositionTests.showAssertClose`.
  - The lock-file path matches `LauncherTests`.

Test identities created, all declared by the step:

- `OpticalConstructor.Tests.KitOpeningTests.an opened kit carries the folder it was opened from`
- `OpticalConstructor.Tests.KitWindowModelTests.the window title names the kit, its folder and the constructor`
- `OpticalConstructor.Tests.KitWindowModelTests.a kit opened read-only is marked in the title`
- `OpticalConstructor.Tests.KitWindowModelTests.a scene that opened no folder is titled by its kit alone`
- `OpticalConstructor.Ui.Tests.KitWindowTests.a constructor window is titled by its kit`

None is removed or renamed. Constructor-unit-tests is expected to rise by 4, from checkpoint 182's 1191 to 1195.
Ui-tests over the selected classes is expected to rise by 1, and ui-smoke is unchanged (227).

## Artifacts

None. This round captured no logs.

## Gotchas

- `CatalogueImportContext.create` is a Storage type extension, not a member of the Domain record. A file that
  calls it must open `OpticalConstructor.Storage.CatalogueImportStore`. Missing that open is what failed attempt 01's
  build.
- In Git Bash, counting `\r` with `grep -c $'\r'` reports every line. Count carriage returns with
  `tr -cd '\r' < file | wc -c` instead. All edited and new files measure 0.
- FuncUI's host may run the view through the dispatcher, so a constructor window's `Title` is guaranteed only
  after its first render. The UI fact shows the window and runs `Dispatcher.UIThread.RunJobs()` before it reads the
  title.
- A kit opened through `TestAppScope.openedKitIn` is named after its GUID folder leaf, not `Optics QZ`. The UI fact
  therefore builds its expected title from `opened.library.kit.kitFile.name` and `opened.folder`.
