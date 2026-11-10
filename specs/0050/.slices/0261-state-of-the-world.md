# Step 0261 — state of the world

## Where we are

Spec 0050 section S8.2 (setups as slots), step 0261 (IMPLEMENT). Steps 0256–0260 made `Setups.Setup` / `SetupSlot`
the Domain shape, the table's elements slots and `<name>.setup-json` the kit's setup file. This step makes an
experiment reference one revision of a kit setup (`Experiment.setup : SetupReference`) and retires the captured
descriptor chain (`ElementDescriptor`, `ElementBinding`, `PlacementSummary`, `VersionRef`). A commit records the
table as the active setup, minting the next revision when an experiment references the current one; recording and
copying experiments resolve, and bring, the setups they reference.

## What's working

- Made `Experiment.setup` a `SetupReference` and gave `VariedElement` the varied slot's `label`; deleted
  `ElementDescriptor`, `ElementBinding`, `PlacementSummary` and `VersionRef`.
- Added `Setup.pinFor`, `Experiment.slotsIn` and the slot-based `Experiment.itemsUsedBy` (`Setups.fs`), with
  `SetupItemReferenceErr` and `KitLibrarySetupErr` carrying a missing setup, revision or piece as a typed error.
- `ExpCommit` records the table as the active setup (the first free `Setup <n>` when there is none) and commits over
  its reference; `recordExperiment` refuses a revision or piece the kit lacks and snapshots nothing.
- `copyExperimentsFrom` brings the referenced setups with their slots' items mapped onto the kit's, skips an
  identical held setup and renames a clashing one (`Bench (2)`), its experiments following.
- The solver hand-off and the attach validation read the experiment's slots; the experiment schema's `setup` is
  `{ name, revision }`.

## Tests

No gate verdict is claimed (Invariant 6); the supervisor runs the frozen roster. Development runs over the final tree
used each gate's own command, with logs in `specs/0050/.artifacts/0261-dev-*.log`.

- **build:** exit 0, 0 warnings, 0 errors, 17 s incremental (deadline 600 s).
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, 216 s (deadline 600 s).
- **constructor-unit-tests:** `Passed!`, 1421 succeeded, 0 failed, 0 skipped, 70 s (deadline 180 s). The count rises
  1420 → 1421 as the acceptance states: seven new facts, six deleted. Every census identity (changed, renamed and
  new) is `Passed` in the TRX, and the six deleted identities are gone.
- **ui-smoke:** `Passed!`, 245 succeeded, 0 failed, 0 skipped, 202 s (deadline 600 s). The three census smoke facts
  passed.
- **ui-tests (the seven selected classes):** `Passed!`, 100 succeeded, 0 failed, 0 skipped, 114 s (deadline 360 s).
  `an experiment whose setup the collection lacks yields UnspecifiedElements naming the setup` is new and passed;
  `recommitting a copied experiment preserves its assigned physical ids` (`.manual/016`) passed.
- **Whole non-smoke UI suite** (beyond the gate, to catch fallout from commits now recording setups): 677 passed,
  0 failed, 217 s.
- **The fifteen s26 invariants** each find nothing, tracked and untracked; `ExperimentCollectionProxyTests.fs`
  stays deleted.
- **The acceptance grep** `git grep -n -w -E "ElementDescriptor|ElementBinding|PlacementSummary|VersionRef" --
  Berreman/` finds nothing.
- No run exceeded its deadline.

```yaml
gates:
  constructor_unit_tests: 1421
  ui_smoke_tests: 245
  ui_tests: 100
```

## Architecture

- **An experiment is a run over a setup revision.** Its slots are the setup's (`Experiment.slotsIn`), so a commit
  never captures elements; editing a referenced setup mints its next revision through the one version rule
  (`Setup.revisedWith` with `Setup.pinFor`), and the earlier experiment keeps its own.
- **One resolver for items.** `Experiment.itemsUsedBy experiments setups items` answers the revision's error first,
  then a missing piece as `SetupItemReferenceErr (UnknownKitItemErr _)` naming the slot by its light-order role;
  `recordExperiment`, the copy (before and after mapping) and `ToggleCopiedExperiment` all call it. Storage wraps it
  once in `KitLibrarySetupErr`.
- **A copy writes snapshots, lab files, added items, setups, then experiments.** Its closure walks the mapped setups'
  references, then the added items'; a reference only the target kit holds (a slot rebound to a mapped item's entry)
  resolves among the target's snapshots and is not written.
- **A slot's identity and binding agree.** A copied slot mapped onto another kit item takes that item's binding
  (`slotBinding`), and `BindValueIdTo` empties a slot whose entry changes.
- **`KitCopySource` holds the source's setups**, read on open after the items, so the toggle resolves items without
  IO.

## Deferred

Nothing in this step's scope.

## Gotchas

- **`recordExperiment` also checks the slots' items.** Brief (7)(b) names the revision check; the acceptance keeps
  missing item identities typed failures before writes, and the S6 fact `recording an experiment refuses a missing
  physical item before writing` (not in the census) exercises it. Both come from one `itemsUsedBy` call.
- **Test files outside the edit list changed only to compile:** `KitTests.fs` (one match arm for the new
  `SetupError` case), `KitOpeningTests.fs`, `KitStartTests.fs`, `KitStartWindowTests.fs` and `LauncherTests.fs`
  (`setup = []` → a test-local `SetupReference`). Their assertions are unchanged.
- **Two assertions follow the new behaviour:** `KitWindowModelTests.Reload reads the kit's files in and clears the
  offer` (not in the census) now expects the commit's `Setup 1` before reload; `copying the chosen experiments …`
  (census) finds the copied `Dark` in `kitSetups`, as brief (8)(d) makes the copied setups the window's.
- **The missing-piece reason names a slot by role** (`'Prep 1' (lp)`): slots have no label and `kindName` lives in
  the UI, so the S6 fact asserts the role in place of the old `'Linear polarizer #2' (lp)`.
- **`slotsIn` / `itemsUsedBy` are optional extensions in `Setups`:** callers open `OpticalConstructor.Domain.Setups`.
- **The Domain and Tests `.fsproj` comments** that named the retired types were reworded (no comment added), since
  the acceptance grep covers `Berreman/`.
- **Bash heredocs on this host fail on some quoted content;** the larger edits went through scripts written with the
  Write tool (`specs/0050/.artifacts/0261-scratch/`).

## Changelog

- 2026-11-10: Step 0261 made experiments reference a setup revision, retired the descriptor chain and `VersionRef`,
  recorded the table as the active setup on commit, and made copies bring their setups. Constructor 1421 (+1),
  smoke 245, selected UI 100.
