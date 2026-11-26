# Step 0233 — impl log

## Progress

- **Reading.** I read the system prompts (base and `WIRE_UI`), the project prompt, AGENTS.md, the slice and its gates
  snapshot, manual 015 (the ui-smoke 600 s deadline), and slice 0232's state of the world and impl-log. No operator
  note is in flight.
- **Seams read.**
  - `KitWindowShell` and its `inert` (`TableAndElementRotationView.fs`).
  - `KitZipPicker.ofWindow`, `KitArchiveJobHost.onUiThread` (`KitArchiveJob.fs`), `KitFolderPicker.ofWindow`
    (`KitOpening.fs`) and `KitArchiveProxy.create` (`KitArchiveStore.fs`; building it touches nothing).
  - The kit bar (`KitWindowView.kitMenuRow`). Send kit… shows whenever the window has a kit folder and no send
    runs, whatever the access. Copy experiments… shows only while the kit takes writes and no source is open.
  - `ConstructorScene.mount` and the two windows in `Program.fs`, and `TestAppScope`.
  - Every `AppContext` built outside `compose` is a `{ … with … }` copy, so the new field breaks no construction.
- **AppContext.fs.**
  - Added `locations : DataLocations` after `settings`. Its one-line doc: the locations the app scope was composed
    over, where its windows' dialogs start (spec 0050 §3.9, S6).
  - `compose` sets it from its `locations` argument, so `create` and `recreateLibrary` both carry it.
  - The type doc's list of what it shares gains "locations".
- **Program.fs.**
  - `ConstructorScene.mount` takes the `AppContext` after the window. Its shell is the slice's record, over
    `KitWindowShell.inert`:
    - `explorer = ExplorerProxy.system`;
    - `requestClose = window.Close`;
    - `zipPicker = KitZipPicker.ofWindow window context.locations.exportsFolder`;
    - `archive = KitArchiveProxy.create ()`;
    - `archiveJobs = KitArchiveJobHost.onUiThread`, posting `SendKitProgressed` / `SendKitEnded` through
      `hostDispatch.Value`, the dispatch the init command stores. It is dereferenced when the job reports, not when
      the shell is built;
    - `kitPicker = KitFolderPicker.ofWindow window context.locations.kitsFolder`.
  - `MainConstructorWindow` and `InverseConstructorWindow` pass `context`. Each doc gains one line: "Its kit bar
    sends and copies through the real dialogs (spec 0050 S6)."
  - The mount doc names the new shell members.
  - Added `open OpticalConstructor.Storage.KitArchiveStore`.
- **Tests.**
  - `AppContextTests`, pure contract section (no trait, gate `ui-tests`): `the app scope carries the locations it was
    composed over`. It checks that `TestAppScope.freshAppContext owner` holds `locations` equal to
    `TestAppScope.testLocations owner`, where `owner` is the module's `FolderName "AppContextTests"`.
  - `KitWindowTests`, trait `ui-smoke`: `a constructor window's kit bar offers Send kit, and Copy experiments only to
    a kit opened read-write`. It runs over `TestAppScope.freshAppContext owner` (`FolderName "KitWindowTests"`).
    - **Read-write:** a shown `MainConstructorWindow` over `TestAppScope.openedKitIn` holds `sendKitButton`
      ("Send kit…") and `kitCopyExperimentsButton` ("Copy experiments from another kit…").
    - **Read-only:** the window is closed, which releases the kit. Over the same folder opened read-only through
      `openedAt`, the window holds `sendKitButton` and no `kitCopyExperimentsButton`.
    - Nothing is pressed.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/AppContext.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/AppContextTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`

Five effect files need no edit: `LauncherTests.fs`, `TableAndElementRotationTests.fs`, `WireUi0040CompositionTests.fs`,
`WireUiCompositionTests.fs` and `WireUiFinalCompositionTests.fs`.

- The windows' public constructors are unchanged: `MainConstructorWindow(context, opened)` and
  `InverseConstructorWindow(context, opened)`. `mount` is private to `Program.fs`.
- Every `AppContext` those files build is a `with` copy.
- None of them presses Send kit… or Copy experiments… on a real constructor window.

They keep their assertions unchanged.

## Testing state

These are development runs over the final source. The gate engine gives the verdict (Invariant 6).

- **build:** `dotnet build Berreman.slnx -c Release -nologo -v:m` exit 0, 0 errors, 70 s. The only warnings are the
  vendored MathNet's two `SYSLIB0051`, the same as step 0232. Nothing comes from our code.
- **The two new facts alone:** 2 of 2 passed.
- **constructor-unit-tests** (whole suite): 1353 passed, 0 failed, 0 skipped. That equals checkpoint 231.
- **ui-smoke** (whole suite, `Category=ui-smoke`): 242 passed, 0 failed, 0 skipped. That is checkpoint 231's 241
  plus the declared fact. It took 201 s against the 600 s `attempt_timeout_seconds`; no deadline was exceeded.
- **ui-tests** over the four selected classes (the gate's filters, without `ui-smoke` / `kit-acceptance`): 63
  passed, 0 failed, 0 skipped, in 55 s. By class, from the TRX:
  - AppContextTests 17 (16 + the declared fact);
  - KitWindowTests 13;
  - LauncherTests 8;
  - TableAndElementRotationTests 25.
- **elevation** (`ASG_ARC_BASE_COMMIT` set to the `.arc-base-json` origin `f395597`): `ELEV-GATE: OK`, 0 new findings
  in 12 of 12 projects, 195 s. Known findings: 0.
- **The fourteen s22 invariant greps** (the decoded `git grep` of each `invariant-s22-*` gate): none finds anything.
- **Line endings:** every edited file is LF, with 0 CR bytes, and `git diff --numstat` equals the
  `--ignore-cr-at-eol` diff.
- **Red-before-green.**
  - The locations fact cannot compile without `AppContext.locations`, so it is red by name.
  - The kit bar fact shows the two verbs on real windows over both accesses. By the slice's design it presses
    neither, so it cannot observe which dialogs the shell holds (see Gotchas).

No test is removed or renamed, and no assertion is removed or weakened.

## Artifacts

Under `specs/0050/.artifacts/`:

- `0233-build.log`
- `0233-new-facts.log`
- `0233-constructor-unit-tests.log`
- `0233-ui-smoke.log`
- `0233-ui-tests.log`
- `0233-elevation.log`

## Gotchas

- **The shell's dispatch is a reference.** The shell is built before `Program.run` calls `init`, and `init` is what
  stores the Elmish dispatch in `hostDispatch`. The job host's two callbacks therefore read `hostDispatch.Value`
  when a job reports. A dispatch captured when the shell is built would be `ignore`, and the send's progress and end
  would be lost.
- **The new smoke fact cannot see the shell.** The slice says neither button is pressed, because a real window's
  dialogs are the system's (the S5.3 Show in Explorer precedent). Send kit… depends only on the kit folder and on no
  running send, and Copy experiments… only on the kit's writes. So a shell reverted to `KitWindowShell.inert` would
  still pass the fact. The shell's composition is pinned by the diff in `ConstructorScene.mount`.
- **Every `KitWindowShell` field is now set** in `mount`. The record still starts from `KitWindowShell.inert`, as the
  slice prescribes, so a field added later starts inert until it is wired.
- **`LauncherWindow` still takes its own `locations`, and needs them.** When the library fails to open there is no
  `AppContext`, yet the Data locations link and Recreate library still need the locations. The slice changes no
  other seam.
