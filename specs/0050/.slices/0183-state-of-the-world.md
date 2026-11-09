# Step 0183 — state of the world

# Where we are

Spec 0050 section S5.3 builds the kit window and the Data locations window. The kit window has its title, Kit
menu, lock, reload and mode. Step 182 declared contract `SVC_BigBoy_0001`, `ExplorerProxy`: the seam that shows a
folder in the system's file browser. This `IMPLEMENT_CONTRACT` step builds the real construction,
`ExplorerProxy.system`, in `OpticalConstructor.Ui/KitOpening.fs`, beside `KitFolderPicker.ofWindow`, and moves the
contract to `implemented`. Two later steps wire it: the Kit menu's Show in Explorer (step 186) and the Data
locations window's Open folder (step 193).

# What's working

- Add `ExplorerProxy.system`. It resolves the folder's full path and starts `explorer.exe` on it, without waiting.
- A missing folder answers `MissingFolderErr`, which names the folder. Nothing is started and nothing is created.
- Any exception from resolving the path or starting Explorer becomes `ExplorerLaunchErr` with the exception's
  message.
- Add the fact `the system explorer refuses a folder that does not exist and starts nothing`.
- `SVC_BigBoy_0001` is `implemented` at step 183 in `.contracts-json`.

# Tests

This worker ran no build, test, analyzer or invariant gate. The `IMPLEMENT_CONTRACT` protocol (Invariant 6) gives
gate execution to the gate engine, which runs:

- build and elevation;
- constructor-unit-tests, all discovered;
- ui-smoke, all discovered;
- ui-tests, `KitStartWindowTests`;
- the fourteen s21 invariants.

Checked without running a gate:

- every edited file is LF, and `git diff --check` is clean;
- no added line contains an s21 invariant's word;
- every API used was checked against its source or an existing use in the repo (impl-log, Testing state).

Test identities:

- One fact is created: `OpticalConstructor.Tests.ExplorerProxyTests.the system explorer refuses a folder that
  does not exist and starts nothing`.
- None is removed or renamed.

The counts below come from the latest checkpoints (181 for constructor-unit-tests, 179 for ui-smoke). They were
not measured this round. With the new fact, the constructor-unit-tests count is expected to measure 1191. Ui-smoke
is unchanged.

```yaml
gates:
  constructor_unit_tests: 1190
  ui_smoke_tests: 227
```

# Architecture

- The real explorer is a static member, `type ExplorerProxy with static member system`, in Ui's `KitOpening.fs`.
  That is where the app's other shell seams live (`ClockProvider.system`, `KitFolderPicker.ofWindow`). The Domain
  keeps the seam's type and the in-memory double. Ui owns the IO edge.
- `showFolder` is the boundary. It converts every exception into an `ExplorerError` value, so nothing above it
  handles one.
- It checks for a missing folder with `Directory.Exists`; it does not catch an error for it. A location that does
  not exist yet is reported, never created (`.manual/001 §G`).
- Explorer is started with `UseShellExecute = false`, and the full path is its single `ArgumentList` entry, so the
  path is never shell-quoted by hand. The returned `Process` is disposed at once, and nothing waits for the
  browser.

# Deferred

Nothing. Every requirement of the step landed this round. Steps 186 (Show in Explorer) and 193 (Open folder) will
wire the proxy into the UI, as the arc plans.

# Gotchas

- No fact asks `ExplorerProxy.system` to show a folder that exists, because that would open an Explorer window on
  the machine running the tests. Starting Explorer on an existing folder is therefore covered only by reading the
  code and by the later wiring steps' doubles.
- `MissingFolderErr`'s reason is the resolved full path (`Path.GetFullPath`), not the caller's raw text. A relative
  input is reported as the absolute path that was checked.

# Changelog

- 2026-11-08 — Step 0183: `ExplorerProxy.system` implemented in `KitOpening.fs`. It refuses a missing folder with
  `MissingFolderErr` and otherwise starts `explorer.exe`; exceptions become `ExplorerLaunchErr`. One
  `ExplorerProxyTests` fact added; `SVC_BigBoy_0001` set to `implemented`.
