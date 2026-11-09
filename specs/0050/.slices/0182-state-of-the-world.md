# Step 0182 — state of the world

# Where we are

Spec 0050 section S5.3 builds the kit window (title, Kit menu, lock, reload and mode) and the Data locations window.
This `ADD_CONTRACT` step declares contract `SVC_BigBoy_0001`, `ExplorerProxy`: the seam that shows a folder in the
system's file browser. The Kit menu's Show in Explorer (step 186) and the Data locations window's Open folder
(step 193) both reach Explorer through it. The contract stays `declared`; step 183 implements the real
`ExplorerProxy.system` in `OpticalConstructor.Ui/KitOpening.fs`.

# What's working

- Add `ExplorerError` (`MissingFolderErr`, `ExplorerLaunchErr`) with `describe` to `KitFolders.fs`.
- Add `ExplorerProxy = { showFolder : FolderName -> Result<unit, ExplorerError> }`, after `KitFolderPicker`.
- Add the double `ExplorerProxy.createInMemory`, which records every folder it is asked to show and starts nothing.
- Add `ExplorerProxyTests` with two facts: the double records folders in order, and each error renders its condition
  and its reason.

# Tests

This worker ran no build, test, analyzer or invariant gate: the `ADD_CONTRACT` protocol (Invariant 6) gives gate
execution to the gate engine. The engine runs:

- build and elevation;
- constructor-unit-tests, all discovered;
- the fourteen s21 invariants.

Checked without running a gate:

- every edited or created file is LF;
- no added line matches an s21 invariant's pattern;
- the facts use only idioms the suite already compiles.

Test identities:

- two facts are created, `ExplorerProxyTests.the in-memory explorer hands every folder it is asked to show to its
  recorder` and `ExplorerProxyTests.every explorer error renders its condition and its payload`;
- none is removed or renamed.

The count below is inherited from the latest checkpoint and was not measured this round. With the two new facts, the
constructor-unit-tests count is expected to measure 1190.

```yaml
gates:
  constructor_unit_tests: 1188
```

# Architecture

- The seam lives in `OpticalConstructor.Domain/KitFolders.fs`, beside `KitFolderPicker`, the other shell seam the kit
  window reaches. It follows the same functional-proxy shape: a `[<ReferenceEquality>]` record of one
  `Result`-returning function.
- `showFolder` takes the elevated `FolderName`, so Show in Explorer (a `KitFolder`'s folder) and Open folder (a data
  location's folder) share one parameter type.
- The double lives on the type (`createInMemory`), as every proxy in the arc keeps its in-memory construction
  (.spec-md §4). The real construction is the Ui's (step 183), because starting `explorer.exe` is the IO edge.
- Contract registry: `.contracts-json` already holds `SVC_BigBoy_0001` as `declared` at step 182, so it is unchanged.

# Deferred

Nothing. Every requirement of the step landed this round. The real proxy is step 183's, by the arc's plan.

# Gotchas

- The double never answers an error. `MissingFolderErr` comes only from the real proxy's existence check (step 183).
- Count carriage returns over raw bytes. In this Git Bash, `grep -c $'\r'` counted every line of an LF file.

# Changelog

- 2026-11-08 — Step 0182: `ExplorerError`, `ExplorerProxy` and `ExplorerProxy.createInMemory` declared in `KitFolders.fs`; `ExplorerProxyTests` added with two facts.
