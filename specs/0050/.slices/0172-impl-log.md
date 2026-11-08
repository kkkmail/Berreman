# Step 0172 — impl log

## Progress

- [x] Read the protocol (`arc-runner.system-md`, `implement_worker.system-md`), the project prompt, the slice and its
  `.gates`, manual 013, `KitOpening.fs`, `KitOpeningTests.fs`, `KitContext.openKit` / `openOrInitialise`
  (`Domain/KitFolders.fs`) and `KitProxy.create` (`Storage/KitFolderStore.fs`).
- [x] Moved the post-open body of `openKitAt` into the private
  `openedOver (context : KitContext) (imports : CatalogueImportContext) : Result<OpenedKit, KitLibraryError>`.
  `openKitAt` now binds its open into `openedOver`, so its open and every answer are unchanged.
- [x] Added `openExistingKitAt (folder : KitFolder) (clock : ClockProvider) (imports : CatalogueImportContext)`:
  `KitContext.openKit (KitProxy.create folder) ReadWriteKitAccess clock`, with `KitError` mapped to
  `KitLibraryKitErr`, then `openedOver`.
- [x] Added the two `KitOpeningTests` facts the slice names, plus one private helper, `assertNotAKitFolder`, which is
  not a fact.
- [x] Checked line endings. Both files are `i/lf w/lf` and contain no CR bytes. `git diff --numstat` is the same with
  and without `--ignore-cr-at-eol`.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitOpening.fs`: added `openedOver` (private) and
  `openExistingKitAt`; `openKitAt` now calls `openedOver`. +47 / −32.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitOpeningTests.fs`: added `assertNotAKitFolder` (private)
  and two facts. +56.

## Decisions

- **No flag on `openKitAt`.** Initialise-or-open and open-only are two functions over one body (.spec-md §4, "A
  reuse is a move or a call").
- **`openExistingKitAt` takes no author.** It never makes a kit, so it never needs an author.
- **Log prefix renamed.** The moved body's close-failure log line now says `KitOpening.openedOver:` instead of
  `KitOpening.openKitAt:`. The prefix names the function that logs, and that function is now shared with
  `openExistingKitAt`. No answer (`Result`) changed, and no test reads that text.
- **First fact: one fresh `withOpeningFolder` per case.** The fact has three cases: the kit, the folder holding only
  `notes.txt`, and the missing folder. Each runs in its own fresh folder under
  `AppContext.BaseDirectory/KitOpeningTests/<new GUID>`, with the class's clock, author and import context.
- **"Still holds exactly `notes.txt`".** Two checks:
  - `Directory.GetFileSystemEntries(..., AllDirectories)` must equal the single `notes.txt` path, so no directory
    such as `.oc`, `data` or `library` appears;
  - the folder's file bytes are compared before and after.
- **Second fact: the kit is held open by `withOpenedKit`.** That is the class's `openKitAt` read-write helper. The
  holder's process id, machine and user are asserted the same way as in the existing `openKitAt` lock fact. The whole
  folder's file bytes are compared before and after the refused open.

## Testing state

No gates were run. Under invariant 6 this worker does not run checks; the arc-runner's gate engine runs the step's
roster after the worker exits:
- build;
- elevation;
- constructor-unit-tests, all discovered;
- ui-smoke, all discovered;
- ui-tests, `AppContextTests`;
- the fourteen s20 invariants.

Instead, the types were checked by reading the code:
- `KitLibraryContext { kit; imports }`;
- `KitLibraryKitErr : KitError -> KitLibraryError`;
- `KitContext.openKit : KitProxy -> KitAccess -> ClockProvider -> Result<KitContext, KitError>`;
- `KitProxy.create : KitFolder -> KitProxy`.

`openedOver` is declared after `OpenedKit` and before both of its callers. `openExistingKitAt` and `openedOver` do
not occur anywhere else in `Berreman/`.

The constructor count should rise by 2 over step 0171's.

## Artifacts

None. The slice produced no run captures.

## Gotchas

- **What a refused open writes.** `KitProxy.create`'s `readKitFile` answers `NotAKitFolderErr` before any lock,
  guard or stamp is taken. A missing folder or a folder without `.kit-json` is therefore refused with nothing written
  (step 131).
- **`KitLockedErr` within one process.** The holder is found by the second proxy's `acquireLock`. Each
  `KitProxy.create` has its own held-lock state, so a second proxy in the same process sees the first proxy's live
  lock.
- **`specs/0050/.manifest.state.json` belongs to the supervisor.** It was already modified when this worker
  started, and it was not touched.
