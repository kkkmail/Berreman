# Step 0261 — impl log

## Progress

- Read the protocol, the project prompt, the slice, `.manual/016` and step 0260's state of the world.
- Domain done: `Experiments.fs` (descriptor types gone, `VariedElement.label`, `setup : SetupReference`, `commit`
  over a reference, dark-line description), `Lifecycle.fs` (`VersionRef` gone, its unused opens dropped), `Kit.fs`
  (`SetupItemReferenceErr`), `Setups.fs` (`pinFor`, `slotsIn`, `itemsUsedBy`), `ExperimentDataLoad.fs` (slots),
  `ExperimentCollectionStore.fs` (`setups`), `KitLibrary.fs` (`KitLibrarySetupErr`). Domain builds.
- Storage done: schema `setup` is `{ name, revision }`; `recordExperiment` resolves the revision and the slots'
  items through `Experiment.itemsUsedBy` and snapshots nothing; `copyExperimentsFrom` keeps, maps, places (skip /
  rename) and writes the setups; report gains `setups`. Storage builds.
- UI done: `ExpCommit` records the table as the active setup; attach validation resolves slots; `KitCopySource`
  holds the source's setups; toggle / copy use the new `itemsUsedBy` and re-read setups; solver hand-off reads
  slots in light order; `BindValueIdTo` empties a slot whose entry changes. UI builds with no warnings.
- AGENTS.md gotcha "The table is the active setup." added after "An experiment runs on the light's real path".
- Tests: census renames / deletions / new facts done; fixtures moved onto `SetupReference` / `SetupSlot`.
  Constructor suite 1421 passed (1420 → 1421: seven new facts, six deleted).

## Files modified

- Domain: Experiments.fs, Lifecycle.fs, Kit.fs, Setups.fs, ExperimentDataLoad.fs, ExperimentCollectionStore.fs,
  KitLibrary.fs, OpticalConstructor.Domain.fsproj (comments only).
- Storage: schema/experiment.schema.json, KitContentFiles.fs.
- UI: TableAndElementRotationView.fs, SolverHandoffView.fs.
- AGENTS.md.
- Constructor tests (census): ExperimentProxyTests, LifecycleTests, ExperimentCollectionRoundTripTests,
  KitContentFileJsonTests, KitLibraryTests, ExperimentCopyTests, OutOfBandDiagnosticTests, SetupTests,
  SetupModelTests, KitWindowModelTests; OpticalConstructor.Tests.fsproj (comment only).
- Constructor tests outside the edit list (fixture compile repairs only): KitTests.fs (one match arm),
  KitOpeningTests.fs and KitStartTests.fs (`setup = []` → a test-local `SetupReference`).
- UI tests (census): InverseConstructorTests, SolverHandoffWindowTests, ScrollExtentTests,
  WireUiFinalCompositionTests, KitWindowTests.
- UI tests outside the edit list (fixture compile repairs only): KitStartWindowTests.fs and LauncherTests.fs
  (`setup = []` → a test-local `SetupReference`).

## Decisions

- **`KitCopySource.setups`** (read on open, after the items): `ToggleCopiedExperiment` must call the new
  `Experiment.itemsUsedBy experiments setups items`, and the source's setups are otherwise not in the model. The
  brief's (8)(d) names only `CopyChosenExperiments`; this is the least change that gives the toggle its setups.
- **A no-op `ExpCommit` writes nothing.** The setup is built first (pure); the collection commits over its
  reference; only if the collection changed are the setup (unless equal to the active one) and the experiment
  recorded. `Setup.create`'s impossible refusal is carried as `KitLibrarySetupErr`, never thrown.
- **`recordExperiment` also resolves the slots' items.** Brief (7)(b) names the revision check; the acceptance
  requires "missing setup/item identities MUST remain typed failures before writes", and the S6 fact `recording an
  experiment refuses a missing physical item before writing` (not in the census) exercises it. One call,
  `Experiment.itemsUsedBy [ experiment ] setups items`, answers the revision error first and then
  `SetupItemReferenceErr (UnknownKitItemErr _)`, wrapped once in `KitLibrarySetupErr`. It still snapshots nothing.
- **The resolver names a missing piece's slot by its light-order role** (`'Prep 1' (lp)`): a slot has no label, and
  the table's `<kind> #<n>` label lives in the UI (`Catalogue.kindName`). The S6 fact `a missing physical item is
  refused instead of guessed from a shared preset` keeps its assertion's shape (the missing id, `'<name>' (<id>)`,
  `E24`, not the held piece's id).
- **`BindValueIdTo` empties a slot whose entry changes.** The retired `experimentSetup` dropped the item of a rebound
  element; `.manual/016` keeps "the later glass-film rebind still clears the sample's physical assignment". Binding
  the same entry keeps the item.
- **A mapped slot's binding** is regenerated from the target item's model (`slotBinding`: preset id, sample id, or
  none for a standalone model) only when the target is another item; emission is kept (Storage has no library to
  resolve it).
- **Copy closure fallback:** a mapped setup's reference the source lacks (a slot rebound to the kit item's entry)
  resolves among this kit's snapshots, and held versions are filtered out as before.
- **Copy rename** also avoids names given earlier in the same copy (`nextFreeAmong` over the kit's and the given
  names), so a renamed setup cannot be overwritten by a later one.
- **`.fsproj` comments:** the Domain and Tests project comments naming `ElementDescriptor` / `VersionRef` were
  reworded (no new comment), as the acceptance grep covers `Berreman/` whole.
- **Non-census assertion adapted:** `KitWindowModelTests.Reload reads the kit's files in and clears the offer`
  asserted the window held no setup after a commit; a commit now records `Setup 1` (acceptance), so it asserts
  `Setup 1` before reload and `Bench` added on reload. `KitWindowModelTests.copying the chosen experiments …` (census)
  now finds `Dark` in `kitSetups`, as (8)(d) makes the copied setups the window's.

## Testing state

Final tree, each gate's own command (logs `specs/0050/.artifacts/0261-dev-*.log`):

- build: exit 0, 0 warnings, 0 errors (17 s incremental).
- elevation: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 216 s.
- constructor-unit-tests: 1421 passed, 0 failed, 0 skipped, 70 s (1420 → 1421).
- ui-smoke: 245 passed, 0 failed, 0 skipped, 202 s.
- ui-tests (seven selected classes): 100 passed, 0 failed, 0 skipped, 114 s.
- whole non-smoke UI suite: 677 passed, 0 failed (beyond the gate).
- fifteen s26 invariants: nothing found, tracked and untracked; acceptance grep: nothing found.
- Every census identity checked in the TRX files: changed / renamed / new `Passed`, deleted gone.

`ConstructorUiIdOfDomain.fs` needed no change: its only use of `KitLibraryError` renders `describe`, which carries
the new case; no match over the error lists cases there.

## Artifacts

- `specs/0050/.artifacts/0261-dev-*.log` — development runs of each gate's command.
- `specs/0050/.artifacts/0261-scratch/` — the edit scripts used for the larger rewrites.

## Gotchas

- Bash heredocs on this host fail on some quoted content; larger edits went through scripts written with the
  Write tool.
- `Experiment.slotsIn` / `itemsUsedBy` are optional extensions in `Setups`; a caller must `open
  OpticalConstructor.Domain.Setups` (the UI view, the hand-off view and several test files now do).
