# Step 0186 — impl log

## Progress

- [x] `KitWindowShell` + `static member inert` after `EditorLaunchers`
- [x] `TableAndElementRotationModel.kitShell` after `kitFolderOpt`, set to `KitWindowShell.inert` by `initWith`
- [x] `SwitchConstructorMode of ConstructorMode`, `ShowKitInExplorer`, `CloseKit` messages and their arms
- [x] five `KitWindowModelTests` facts (exactly the five declared identities)
- [x] static review: API precedents, name collisions, LF, invariant words

Attempt 01 (2026-11-08):

1. Read the slice, the gate snapshot, manuals 013 and 014, and the code the slice names. The slice's line
   references have drifted: `EditorLaunchers` is at `TableAndElementRotationView.fs:150` (slice: 143),
   `ConstructorMode` at 253 (slice: 246) and `experimentChartVisible` at ~3025 (slice: 2907).
2. Added `KitWindowShell` (`[<ReferenceEquality>]`, `explorer : ExplorerProxy`, `requestClose : unit -> unit`)
   with `inert` = `ExplorerProxy.createInMemory ignore` and `requestClose = ignore`.
3. Added `kitShell : KitWindowShell` to the model after `kitFolderOpt`; `initWith` (the model's only full
   construction) sets it to `KitWindowShell.inert`.
4. Added the three messages after `SceneLoad` and their arms after the `SceneLoad` arm:
   - `SwitchConstructorMode mode` → `{ model with constructorMode = mode }`, nothing else;
   - `ShowKitInExplorer` → `Some folder`: `model.kitShell.explorer.showFolder folder.value`; `Ok ()` answers the
     model unchanged, `Error error` sets `kitStatus` to `The kit folder could not be shown — {error.describe}`;
     `None` answers the model unchanged;
   - `CloseKit` → `model.kitShell.requestClose ()`, then the model unchanged.
5. Added the five facts to `KitWindowModelTests`, with two private helpers: `recordingShell` (records the folders
   shown and the closes asked) and `lockFile` (the `.oc/lock` path, as `KitOpeningTests` spells it).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs` (+46): `KitWindowShell`,
  `kitShell`, `initWith`, the three messages, the three arms.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs` (+111): `open
  OpticalConstructor.Domain.KitFolders`, `recordingShell`, `lockFile`, the five facts.

## Decisions

- **The switch carries `ConstructorMode`.** `WindowMode.fs`'s `LibraryWindowMode` is a catalogue window's
  Browse / Select state and does not fit (as the slice records), so the existing two-case union is the payload.
- **The switch arm changes `constructorMode` only.** The chart and Data-files gating read `constructorMode`
  (`experimentChartVisible`), so they follow without any extra field.
- **`inert` is a static property, re-evaluated per access.** Each `initWith` model gets its own inert shell. The
  model compares the shell by reference, and every arm keeps the model's own shell, so "the model is unchanged"
  holds by equality.
- **The facts check slightly more than the slice's minimum, without new identities:**
  - the Explorer fact also checks the model is unchanged and no close was asked;
  - the refusal fact checks the status prefix and that nothing else changed;
  - the Close fact checks no folder was shown, and that the lock is gone once the fact's `withLoadedScene`
    closes the kit ("until the fact closes the kit").
- **The switch fact's committed experiment.** It comes from a pre-bound linear polarizer committed in a read-write
  folder kit. `SceneSaveLoadTests` proves that snapshotting this polarizer's preset works in a folder kit.

## Testing state

No build, test, analyzer or invariant gate was run. The `IMPLEMENT` role is act-only (Invariant 6), as at
steps 0184 and 0185, so the arc-runner's gate engine runs the frozen roster after this worker exits:

- build and elevation;
- constructor-unit-tests (all discovered);
- ui-smoke (all discovered);
- ui-tests (`OpticalConstructor.Ui.Tests.KitWindowTests`);
- the fourteen s21 invariants.

Checked without running a gate:

- Both edited files are LF (zero CR bytes); `git diff --check` is clean; `--numstat` equals `--ignore-cr-at-eol`.
- No added line contains any s21 invariant's word.
- `SwitchConstructorMode`, `ShowKitInExplorer`, `CloseKit`, `KitWindowShell` and `kitShell` occur nowhere else,
  in Berreman or in STL's sources (ELEV014 / ELEV016).
- `TableAndElementRotationMsg` has no other exhaustive match. The `SceneSaveLoadTests` reducer matches
  `SceneSave` with a wildcard.
- Every API the tests use has a precedent in the suites:
  - `ExplorerProxy.createInMemory shown.Add` (`ExplorerProxyTests`);
  - `closes.Add ()` over `ResizeArray<unit>` (`ImportDialogTests`);
  - `Assert.StartsWith` / `Assert.EndsWith` / `Assert.Empty(...)` with these argument shapes;
  - `ExpCommit` in a folder kit (`InverseConstructorTests`, `SceneSaveLoadTests`).

Test identities created, all declared by the step (none removed or renamed):

- `OpticalConstructor.Tests.KitWindowModelTests.switching the constructor keeps the kit, the table and the experiments`
- `OpticalConstructor.Tests.KitWindowModelTests.Show in Explorer asks the explorer for the kit's folder`
- `OpticalConstructor.Tests.KitWindowModelTests.a kit folder the explorer cannot show is told in the kit status`
- `OpticalConstructor.Tests.KitWindowModelTests.a scene that opened no folder shows nothing in Explorer`
- `OpticalConstructor.Tests.KitWindowModelTests.Close kit asks the window to close and changes nothing else`

constructor-unit-tests is expected to rise by exactly 5, from checkpoint 184's 1197 to 1202. ui-smoke is unchanged
at 227. The selected `KitWindowTests` class is untouched.

## Artifacts

None. No run was captured, because the role runs no gate.

## Gotchas

- The slice's line numbers lag the file by about 7 lines (`EditorLaunchers` 150, `ConstructorMode` 253) and by
  about 120 for `experimentChartVisible` (~3025). The symbols are the anchors.
- `KitWindowShell` compares by reference. A test that replaces `kitShell` must compare against the model that
  already holds the replacement shell, never against the pre-replacement `loaded` model.
- `CloseKit` releases nothing itself. The lock stays on disk until the hosting window's `Closed` handler
  (step 154) closes the kit; in the facts, `withLoadedScene` does it.
- No operator note was in flight for this attempt.
