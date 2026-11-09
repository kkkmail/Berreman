# Step 0227 — impl log

## Progress

- [x] KitLibrary: `KitLibraryItemErr`
- [x] KitContentFiles: report `items`, `copyExperimentsFrom` item mapping, `recordExperiment` item resolution
- [x] TableAndElementRotationView: `CopyChosenExperiments` passes `[]` and re-reads items
- [x] ExperimentCopyTests: fixtures with items, existing facts pass `[]`, seven new facts
- [x] KitLibraryTests: render fact, the missing-physical-item fact
- [x] Development runs: build, constructor-unit-tests (1344), ui-tests selected (40), ui-smoke (238)
- [x] Development runs: elevation, invariants, mutants, final rebuild and re-runs

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitLibrary.fs`: `KitLibraryError` gains
  `KitLibraryItemErr of ItemError`, rendered `the items could not be copied: <e.describe>`. The type's doc and
  `describe`'s doc now cite S7.2.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitContentFiles.fs`:
  - New private members and helper:
    - `ItemModelInstance.assignedBinding` (private extension) gives the binding a descriptor takes from its item:
      `BoundByEntryId <preset>`, `BoundByVersion (SampleVersionRef <version>)` or `Unbound`.
    - `Experiment.unassignedReferences` (private extension) gives the bindings of descriptors with `itemIdOpt = None`.
    - `resolveAmong` (private, module level) is the snapshot resolver step 224 inlined, lifted so the source and the
      target share it.
  - `recordExperiment` first does `readItems`, then `Experiment.itemsUsedBy [experiment] items`; a missing id is
    `KitLibraryItemErr` before any pin, snapshot or write. It then snapshots the unassigned bindings, the used items'
    `libraryReferences` and the experiment's own bindings (distinct), and writes the file.
  - `ExperimentCopyReport` gains `items : Item list` after `experiments`.
  - A private `ExperimentCopyPlan` holds `added`, `mappedOnto` and `mapped`.
  - `copyExperimentsFrom source ids itemRows`, in order:
    1. read-only refusal;
    2. read and keep the experiments;
    3. `planItems`, which reads the source's and the target's items and runs `itemsUsedBy`, `copyRowsFor` and
       `itemsToAdd`. It rewrites each assigned descriptor onto its target (`itemIdOpt` and `assignedBinding`) and
       checks the result with `itemsUsedBy mapped (held @ added)`. Every refusal is `KitLibraryItemErr`;
    4. the target's `listSnapshots`;
    5. `resolveMappedOnto`: each held item a row maps onto resolves its `libraryReferences` closure through the
       TARGET's snapshots. A missing one is `UnheldEntryErr` naming the reference and the item's label;
    6. `lackedSnapshots`: the source closure over the unassigned bindings, then the added items' references, keeping
       what the target lacks;
    7. the lab files, which are the data paths then the added items' photo paths, distinct;
    8. renumber;
    9. the writes, in order: snapshots, lab files, `writeItems` (`photoOpt` moved to the copied photo's path), then
       the experiments.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`: the `CopyChosenExperiments` arm
  passes `[]`. On `Ok` it reads the kit's experiments and its items and sets both through `withKitContent`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentCopyTests.fs`:
  - Fixtures:
    - Laser600 (`SourcePresetChoice` of `source600`), ML (`StackSampleChoice` of the multilayer v1), QZ (the glass
      plate v1) and CPL1 (the circular-polarizer model), each from `BuiltinCatalogue.itemModel`;
    - the source records the items through `recordItem` before its experiments (`sourceRecording`);
    - E1's source and sample descriptors name Laser600 and ML, and E2's plate names QZ;
    - `HeldContent` gains `items`;
    - `holdItems` writes the catalogue snapshots of an item's closure, then `writeItem`;
    - `reopened` closes a kit and opens it again read-only.
  - Every existing fact passes `[]`.
  - Seven new facts.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitLibraryTests.fs`:
  - the render fact covers `KitLibraryItemErr` (3 → 4 distinct renders);
  - the new fact `recording an experiment refuses a missing physical item before writing`;
  - the partial-closure fixture's two exhaustive matches on `KitLibraryError` gain `KitLibraryItemErr` beside
    `UnheldEntryErr`. That is needed to compile (FS0025), and its assertions are unchanged.

## Decisions

- **The existing copy facts' expected values widen; their assertions stay.**
  - The brief makes E1 name Laser600 and ML and E2 name QZ, so a default copy of E1 now adds those two items.
  - The first fact's expected report gains `items = [Laser600; ML]`.
  - Its expected paths gain `Laser600.item-json` and `ML.item-json`.
  - `closureOfE1` now leads with the two item models: source-preset model, preset, stack model, multilayer, materials,
    categories. That is the brief's walk order: the unassigned bindings (none: E1's detector is unbound), then the
    added items' references.
  - No assertion was removed or weakened.
- **`recordExperiment` snapshots the assigned descriptors' own bindings too, after the unassigned ones and the items'
  references.**
  - The brief names "their exact item/model closure as well as unassigned library bindings".
  - The extra bindings keep step 149's contract that a written experiment never names a version the kit lacks.
  - For an experiment without items the reference list is exactly `experiment.libraryReferences`, as before, so the
    existing pin-order facts are unchanged.
  - The copy never walks a source's assigned bindings, as the brief requires.
- **`ExperimentCopyReport.dataFiles` lists every lab file copied in: the experiments' data, then the added items'
  photos.** A photo is a kit file under `data/` like a data file, and the report records what the kit gained. The photo
  fact asserts `[run; photo]`.
- **A held target whose closure the target lacks is `UnheldEntryErr` naming the reference and the item.** The brief
  says only "refuse the copy before writes"; `UnheldEntryErr` is the copy's existing typed error for a missing
  snapshot. The source's model is never taken instead.
- **Two authorized facts carry extra branches.**
  - `a copied experiment reopens using the explicitly selected target physical piece` also refuses a target piece
    written without its model snapshot. It also gives the standalone branch's element a polarizer-preset binding,
    which the copy must replace with `Unbound`.
  - `the user's choice overrides the default…` also maps ML onto the kit's glass-plate ML. The element must take
    the plate's sample version, and no multilayer snapshot may arrive.
  - Without these branches, a copy that kept the source's binding, or imported the source's model, would pass every
    fact, because every other mapped target shares its source's binding.
- **The refusal branches start from a fully valid kit and source.** After each refusal the same target copies E1 by
  default, which proves that only the choice was refused.
- **KitWindowModelTests is unchanged.**
  - The census lists no fact of that class for this step.
  - Its existing copy fact still holds: its E2 uses no item, so the kit's items re-read equal the loaded ones.
  - Step 228's `the user's choice for an item is kept and copied` asserts the window's re-read items.

## Testing state

Development runs over the final source (Invariant 6: the gate engine gives the verdict).

- **build:** `dotnet build Berreman.slnx -c Release -nologo -v:m` exit 0, 0 warnings, 0 errors.
  - The log has no lowercase `error`.
  - The final build after the mutants is `.artifacts/0227-dev-build-final.log`; the first is `.artifacts/0227-dev-build.log`.
  - The first build failed once on FS0025: `KitLibraryTests`' partial-closure fixture matched `KitLibraryError`
    exhaustively. Fixed by listing `KitLibraryItemErr` beside `UnheldEntryErr`, with no assertion changed.
- **constructor-unit-tests** (all discovered): **1344** passed, 0 failed, 0 skipped, in 66 s.
  - That is 1336 + 8: the seven new `ExperimentCopyTests` facts and `recording an experiment refuses a missing
    physical item before writing`.
  - Each of the nine new or changed identities is `Passed` in the TRX.
  - Log: `.artifacts/0227-dev-constructor-unit-tests.log`.
- **ui-tests** over `KitWindowTests` and `ExperimentControlsTests` (non-smoke, non-kit-acceptance): **40** passed,
  0 failed, 0 skipped, in 20 s. Log: `.artifacts/0227-dev-ui-tests.log`.
- **ui-smoke** (all discovered): **238** passed, 0 failed, 0 skipped. It took 197 s against the 600 s
  `attempt_timeout_seconds`, so no deadline was exceeded. Log: `.artifacts/0227-dev-ui-smoke.log`.
- **elevation**, with `ASG_ARC_BASE_COMMIT=f395597c…` (the `.arc-base-json` origin) and ELEV004 excluded:
  - `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 198s`;
  - suppressed findings 1315 (the excluded ELEV004; unchanged from step 226), known findings 0;
  - log: `.artifacts/0227-dev-elevation.log`.
- **The fourteen `invariant-s22-*` greps:** each finds nothing.
- **Line endings:** `git diff --numstat` equals `--ignore-cr-at-eol`, and all five source files are `i/lf w/lf`.
- **Mutants** (`.artifacts/0227-mutants/`, driver `run_mutants.py`). Each was applied alone to `KitContentFiles.fs`,
  rebuilt through the test project, and run over `ExperimentCopyTests` and `KitLibraryTests`:
  - m1, rows ignored (`copyRowsFor … []`), was caught by three facts:
    - `the user's choice overrides…`;
    - `a copied experiment reopens using the explicitly selected target physical piece`;
    - `copying two same-model pieces…`.
  - m2, the source's binding kept on a mapped descriptor, was caught by two facts:
    - `the user's choice overrides…` (the glass-plate branch);
    - `a copied experiment reopens…` (the standalone branch).
  - m3, no target-closure check, was caught by `a copied experiment reopens…` (the unmodelled target branch).
  - m4, the walk taking the rewritten experiments' assigned bindings too, was caught by two facts:
    - `a copy brings exactly…`;
    - `a snapshot the kit already holds…` (the walk order).
    - Because the walk runs over the rewritten experiments, it can never reach a source's assigned binding.
  - m5, the photo not moved, was caught by `an added item's photo is copied with it`.
  - m6, `recordExperiment` without the item check, was caught by `recording an experiment refuses a missing physical
    item before writing`.
  - m7, the descriptor id not rewritten, was caught by four facts:
    - `an item the kit holds by id…`;
    - `the user's choice overrides…`;
    - `a copied experiment reopens…`;
    - `copying two same-model pieces…`.
  - The driver restored the source byte for byte (`restored: True`). The solution was then rebuilt, and every run
    above is over the restored source.
- **Baselines:** `constructor_unit_tests` 1344, `ui_smoke_tests` 238, `ui_tests` 40 over this step's selected
  classes (KitWindowTests 11 + ExperimentControlsTests 29).

## Artifacts

- `.artifacts/0227-dev-build.log`, `.artifacts/0227-dev-build-final.log`
- `.artifacts/0227-dev-copy-library-tests.log`
- `.artifacts/0227-dev-constructor-unit-tests.log`
- `.artifacts/0227-dev-ui-tests.log`
- `.artifacts/0227-dev-ui-smoke.log`
- `.artifacts/0227-dev-elevation.log`
- `.artifacts/0227-mutants/run_mutants.py`, `.artifacts/0227-mutants/m{1..7}-*-{build,tests}.log`

## Gotchas

- **`recordExperiment` now reads the kit's items first.**
  - A source fixture must record its items (`recordItem`) before an experiment that names them, or
    `recordExperiment` answers `KitLibraryItemErr (UnknownKitItemErr _)`.
  - Under `ReadOnlyKitAccess` the item read passes and the refusal stays `KitReadOnlyErr` from `snapshotReferences`.
- **A held item a copy maps onto must have its whole closure in the target's own snapshots.**
  - A kit item written by `writeItem` alone, with no snapshot, makes the copy answer `UnheldEntryErr`, and nothing is
    written.
  - Test fixtures use `holdItems`, which writes the catalogue snapshots first.
- **The descriptor's binding comes from its target item, not from the source.** For a standalone model (e.g. the
  circular polarizer) the binding is `Unbound`, whatever the source element was bound to.
- **The window's copy passes no rows** until step 228 passes the panel's choices. Every used item therefore takes its
  default: mapped by id, then by model and label, otherwise added under the next free label.
- `ExperimentCopyPlan` is private to `KitContentFiles`. The derived binding (`assignedBinding`) and the unassigned
  references (`unassignedReferences`) are private extensions there, because `Experiments.fs` is outside this step's
  authorized edits.
