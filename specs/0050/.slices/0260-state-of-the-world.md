# Step 0260 — state of the world

## Where we are

Spec 0050 section S8.2 (setups as slots), step 0260 (IMPLEMENT). Steps 0256–0259 made `Setups.Setup` / `SetupSlot`
the Domain shape and the table's elements slots. This step makes `<name>.setup-json` hold a `Setup` (schema version
1), replacing S4.3's scene file. `Scene.fs` keeps `SnapMode` alone. The constructor's scene save and load become
`CreateSetup` (the table becomes a new kit setup) and `ActivateSetup` (a kit setup is drawn on the table). Step 0261
moves item assignments into `SlotContent.ItemInSlot`.

## What's working

- Made `<name>.setup-json` hold a `Setup`, read and written by `KitProxy.readSetups` / `writeSetup` and recorded,
  with every revision's library snapshots, by `KitLibraryContext.recordSetup`.
- Deleted `SceneName`, `SceneElement`, `SceneSnapshot`, `SceneFile`, `SceneStoreError`, and `captureScene` /
  `restoreScene`; `Scene.fs` now holds `SnapMode` alone.
- Replaced the scene messages with `SetSetupName`, `CreateSetup` and `ActivateSetup`; the model holds `setupName`,
  `kitSetups` and `activeSetupOpt`, and loading keeps the window's view, snap mode and pivot.
- Replaced the Experiments bay's scene row with the setup row: `Setup name`, `Create setup` and `Saved setups`.
- Renamed `SceneRoundTripTests` → `SetupRoundTripTests` and `SceneSaveLoadTests` → `SetupSaveLoadTests`, and added
  `SetupModelTests` (three facts).

## Tests

No gate verdict is claimed (Invariant 6); the supervisor runs the frozen roster. Development runs over the final tree
used each gate's own command, with logs in `specs/0050/.artifacts/0260-dev-*.log`.

- **build:** exit 0, 0 warnings, 0 errors (deadline 600 s).
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, 199 s (deadline 600 s).
- **constructor-unit-tests:** `Passed!`, 1420 succeeded, 0 failed, 0 skipped, 67 s (deadline 180 s). All 18 census
  identities (renamed, changed and new) are `Passed` in the TRX.
  - The count falls 1421 → 1420 by design (`.manual/006`, step 0260): three SceneName methods (four cases) are
    retired and three facts are added.
- **ui-smoke:** `Passed!`, 245 succeeded, 0 failed, 0 skipped, 220 s while elevation ran beside it (deadline 600 s).
  `the setup row renders its name field, Create setup and the saved setups` passed, as did the launcher acceptance
  fact.
- **ui-tests (47 selected classes):** `Passed!`, 643 succeeded, 0 failed, 0 skipped, 185 s (deadline 360 s). The
  three renamed `SetupSaveLoadTests` facts and `a read-only window withholds every write control` passed.
- **The fifteen s26 invariants** each find nothing, tracked and untracked.
- **The acceptance grep** `git grep -n -E "SceneSnapshot|SceneName|SceneFile|recordScene|readSceneNames" -- Berreman/`
  finds nothing.
- No run exceeded its deadline.

```yaml
gates:
  constructor_unit_tests: 1420
  ui_smoke_tests: 245
  ui_tests: 643
```

## Architecture

- **The setup file is the setup.** `SetupFile = { schemaVersion; setup }` with `Setup.setupFile` in one direction
  only (ELEV006). It goes through the step-146 stack (`nodeOf`, `canonicalFileText`, `parseVersionedText`,
  `validateWith`); no second parser or writer exists.
- **Every revision's entries are snapshotted.** `Setup.libraryReferences` walks the current slots and then each
  history record's, so any revision an experiment references resolves in the kit. It shares the revision walk with
  `heldItems` through a private member.
- **A setup is the optical path.** The table view, the snap mode and the rotation pivot that the scene file held are
  the window's: `ActivateSetup` replaces only the elements (each drawn through the kit's items), the active setup, the
  selection, the drag and the menu.
- **`CreateSetup` creates; it never overwrites.** A name whose file the kit holds (ignoring case) is refused by
  `Setup.create` before anything is written. A storage refusal is told as `Not saved to the kit — …`.

## Deferred

Nothing in this step's scope. Item assignment through `SlotContent.ItemInSlot` is step 0261 by the spec's plan.

## Gotchas

- **`LauncherTests.fs` (outside the edit list) changed.** It referenced the retired `sceneNameField` /
  `saveSceneButton`. Only those ids and the matching wording changed; `.manual/014`'s
  visible-but-disabled / no-status / unchanged-files assertions are all kept.
- **The setup row adds no `KitStatus` line.** The kit bar's single line carries it (S5.3; `KitWindowTests` asserts
  it is single), although brief (4)(d) lists one there.
- **The storage-refusal fixture types a fresh name.** In `SetupSaveLoadTests`, `CreateSetup` refuses a held name
  before storage, so the read-only-proxy fixture (`.manual/014`) uses `Protected bench` to reach the refusing proxy.
- **Bash heredocs on this host drop a backslash even when quoted.** Write JSON or regex text with the Write / Edit
  tools.

## Changelog

- 2026-11-10: Step 0260 made `<name>.setup-json` hold a `Setup`, retired the scene file and the scene save/load, and
  added `CreateSetup` / `ActivateSetup` with the setup row. Constructor 1420 (−1 by `.manual/006`), smoke 245,
  selected UI 643.
