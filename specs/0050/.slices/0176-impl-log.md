# Step 0176 — impl log

## Progress

- [x] Read the protocol (arc-runner + implement worker), the project prompt, the slice, the `0176.gates` snapshot and
  manual 013 (arc-origin elevation, binding from step 0160 onward). No operator note was in flight.
- [x] `OpticalConstructor.Ui/KitStartView.fs` (new). It is compiled directly after `TableAndElementRotationView.fs`:
  one `<Compile>` line, no comment.
- [x] `KitHistoryError.notSavedCaption` in `ConstructorUiIdOfDomain.fs`, beside the start screen's other captions
  (`openFailedCaption`, `openElsewhereCaption`).
- [x] `OpticalConstructor.Tests/KitStartTests.fs` (new, `module KitStartTests`). It is appended last in the compile
  list, no comment (ADD_TEST `OpticalConstructor.Tests.KitStartTests`).
- [x] Static review: name uniqueness, record-label resolution, LF endings, test identities against `.spec-jsonl`
  step 176.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitStartView.fs` (new):
  - `KitStartContext`, `KitStartOffer`, `KitRelocation`, `KitStartModel`, `KitStartMsg`;
  - `initKitStart`, `updateKitStart`;
  - two private helpers: `kitStartRows`, the one listing rule, and `kitStartChanged`, the one write-and-answer path.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`: +1 Compile line.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`: +`KitHistoryError.notSavedCaption`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitStartTests.fs` (new): the five facts the slice names.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: +1 Compile line.

These are exactly the five paths in the slice's authorized `edits`.

## Decisions

- **One listing rule.** The rows are always `history.rowsMatching filter context.kitHistory.probeFolder` (step 170).
  This expression lives once, in the private `kitStartRows`, which `initKitStart`, the write path and the filter all
  call.
- **One write path.** StarKit, UnstarKit, ForgetKit and LocateKit each pass a domain verb (`star`, `unstar`, `forget`,
  `relocate r.missing r.found`) to `kitStartChanged`. That helper calls `changeHistory` (step 171):
  - `Ok history`: the model holds that history, with its rows re-listed and `status = None`;
  - `Error e`: history and rows stay as they were, and `status = Some e.notSavedCaption`.
- **The filter leaves the status alone.** `FilterKitHistory` holds the filter, re-lists the rows and leaves `status` as
  it was. The slice says only "holds the filter and recomputes the rows, writing nothing", so a previous failure stays
  visible.
- **Helpers are free functions with two or three parameters, not members.** A `member private` is visible only inside
  its type, so the module-level `updateKitStart` could not call one. A one-parameter free function over our types would
  be ELEV013P. Neither helper is public.
- **Docs.** The module doc and every type, case and field doc is one to three lines and cites spec 0050 S5.2, as the
  slice requires.
- **The filter fact proves "writes nothing" two ways.** Its proxy is the in-memory store with a recording
  `writeHistory`, so the fact asserts that no write happened at all, as well as asserting that `readHistory ()` is
  unchanged.

## Testing state

No gate was run. Under invariant 6 the `IMPLEMENT` worker acts and runs no checks, and step 0175 did the same. The
arc-runner's gate engine runs the roster from `0176.gates`:
- build;
- elevation;
- constructor-unit-tests, all discovered;
- ui-smoke, all discovered;
- ui-tests, selected `OpticalConstructor.Ui.Tests.AppContextTests` and `OpticalConstructor.Ui.Tests.LauncherTests`;
- the fourteen s20 section invariants.

Expected counts:
- constructor-unit-tests: rises by 5, the five new `KitStartTests` facts, from checkpoint 174's 1,175 to 1,180.
- ui-smoke and ui-tests: no test was added or changed.

Checked by reading, not by running:
- **Unique names.** No type, DU case or public name collides anywhere in `Berreman/` or STL (`C:\GitHub\Softellect`).
  This covers `KitStartContext`, `KitStartOffer`, `KitRelocation`, `KitStartModel`, `KitStartMsg`, `NoKitStartOffer`,
  `MakeKitOffer`, `ReadOnlyKitOffer`, `StarKit`, `UnstarKit`, `ForgetKit`, `LocateKit`, `FilterKitHistory`,
  `initKitStart`, `updateKitStart` and `notSavedCaption`.
- **Extension scope.** `error.notSavedCaption` resolves through the `[<AutoOpen>]` `ConstructorUiIdOfDomain` in
  namespace `OpticalConstructor.Ui`. `LibraryWindowView` uses `sampleError.caption` the same way, without an explicit
  open.
- **Record labels.** Every record construction has a known type: a return annotation, a binding annotation or
  copy-and-update. So the labels opened from `TableAndElementRotationView` and `KitHistory` cannot misresolve.
  `KitStartModel`'s labels are also the latest declared in their file.
- **No new public primitives.** No primitive appears in a public signature or record field. The only string is the
  caption's interpolation, which wraps `describe`, the same shape as `openFailedCaption`.
- **Equality.** `KitStartModel` holds a `[<ReferenceEquality>]` context, the same shape as `LibraryWindowModel`, which
  builds warning-free.
- **Test identities.** All five match `.spec-jsonl` step 176 character for character, including the ASCII apostrophe
  in "folder's".
- **Fixture expectations.** Display order `d, b, c, a, e` and `b`, `e` missing follow from `displayOrder`: starred first,
  newest first, with equal times in stored order. After StarKit `a` the order is `d, a, b, c, e`. The filter `kit c`
  matches only `Kit c`, because no folder `C:/kits/…` holds "kit c".
- **Line endings.** All five touched files are LF, with zero CR bytes.

## Artifacts

None. The round captured no logs.

## Gotchas

- **A duplicated test helper.** `KitStartTests.steppingClock` repeats `KitHistoryTests.steppingClock`, which is private
  to its module. `KitHistoryTests.fs` is outside this step's authorized edits, so the helper could not be moved into a
  shared fixture. `entryOf` likewise follows the KitHistoryTests shape. Each one is a few lines of test-local code.
- **`.manifest.state.json` belongs to the supervisor.** It was already modified before this worker started, and this
  round did not touch it.
- **A misleading CR count.** In this Git Bash, `grep -c $'\r'` sometimes counts every line. Use `tr -cd '\r' | wc -c`
  or `git ls-files --eol` to check line endings.
