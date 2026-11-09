# Step 0227 — state of the world

## Where we are

Spec 0050, section S6 ("Send and receive kits; copy experiments between kits"), step 0227 (IMPLEMENT).

- Steps 224–226 copied experiments between kits, with their data files and snapshots, behind the kit window's copy
  panel.
- This step closes the item half of 001 §H in storage: the copy now brings the items the copied experiments use.
  Each item is mapped onto a target-kit piece or added, and every assigned descriptor is rewritten onto the chosen
  physical piece.
- Step 228 puts the mapping choices into the window's messages, and step 229 draws the mapping table.

## What's working

- Copy experiments from another kit… now brings the items the copied experiments use:
  - each is mapped by its row, by id, or by model and label, and otherwise added under the next free label;
  - an added item comes with its model snapshots and its photo.
- A copied descriptor now names the chosen target piece and takes that piece's binding; a mapped piece's closure must
  resolve from the target kit's own snapshots.
- `KitLibraryError` gains `KitLibraryItemErr`. A missing piece or a refused choice stops the copy, and the same
  error stops `recordExperiment`, before anything is written.
- `ExperimentCopyReport` gains `items`.
- The kit window's copy passes no rows yet and re-reads the kit's items after a copy.
- Added seven `ExperimentCopyTests` facts and one `KitLibraryTests` fact; constructor-unit-tests went from 1336 to
  1344.

## Tests

Development runs over the final source, recorded in `0227-impl-log.md`. The gate engine gives the verdict
(Invariant 6).

- **build:** exit 0, 0 warnings, 0 errors, and no lowercase `error` in the log.
- **constructor-unit-tests** (all discovered): **1344** passed, 0 failed, 0 skipped.
  - That is 1336 + 8.
  - Each new or changed identity is `Passed` in the TRX:
    - the seven new `ExperimentCopyTests` facts;
    - `recording an experiment refuses a missing physical item before writing`;
    - the changed `every kit library error renders its condition and its payload`.
- **ui-tests** over `KitWindowTests` and `ExperimentControlsTests`: **40** passed, 0 failed, 0 skipped.
- **ui-smoke** (all discovered): **238** passed, 0 failed, 0 skipped. It took 197 s against the 600 s
  `attempt_timeout_seconds`; no deadline was exceeded.
- **elevation** (from the arc origin `f395597`, ELEV004 excluded): `ELEV-GATE: OK - 0 new finding(s) in 12
  project(s)`, 198 s, with 1315 suppressed findings and 0 known findings.
- **The fourteen `invariant-s22-*` gates:** each grep finds nothing.
- **The facts bite.** Seven mutants of the new storage logic were each caught by the facts meant to guard them:
  - rows ignored;
  - the source's binding kept;
  - no target-closure check;
  - the walk order changed;
  - the photo not moved;
  - `recordExperiment` without its item check;
  - the descriptor id not rewritten.

  The source was restored byte for byte and rebuilt before the runs above.

```yaml
gates:
  constructor_unit_tests: 1344
  ui_smoke_tests: 238
  ui_tests: 40
```

## Architecture

- **Reads before writes, as in step 224.** The copy runs in this order:
  1. the kept experiments;
  2. the source's and the target's items;
  3. `Experiment.itemsUsedBy`, `ItemCopyRow.copyRowsFor` and `ItemCopyRow.itemsToAdd`; every refusal is
     `KitLibraryItemErr`;
  4. the descriptors are rewritten, then checked with `itemsUsedBy` over the held plus added items;
  5. the target's snapshots, which must resolve each mapped-onto held item's closure (else `UnheldEntryErr`);
  6. the source closure, over the unassigned bindings, then the added items' references;
  7. the lab files, which are the data paths, then the added items' photo paths;
  8. renumbering.

  The writes follow: snapshots, then lab files, then added items (photo moved to its copied path), then
  experiments.
- **A descriptor's binding follows its physical piece.**
  - The binding is derived from the target item's `ItemModelInstance`: preset → `BoundByEntryId`, sample →
    `BoundByVersion`, standalone → `Unbound`.
  - The source's assigned bindings are never walked, so a source model is never imported in place of a target piece.
  - No relation is inferred from shared presets: identity is the `ItemId` alone.
- **`recordExperiment`** resolves explicit ids through `readItems` first. It then snapshots the unassigned bindings,
  the used items' closures and the remaining bindings, in that order, distinct. For an experiment without items the
  order is exactly as before.
- **`ExperimentCopyReport.dataFiles`** lists every lab file copied in: the data first, then the photos.

## Deferred

Nothing in this step's scope is deferred. The window's own mapping choices (step 228) and the mapping table
(step 229) are later steps of S6, as this step's brief names them.

## Gotchas

- **A source fixture must record its items before the experiments that name them.** Otherwise `recordExperiment`
  answers `KitLibraryItemErr (UnknownKitItemErr _)`.
- **A kit item written by `writeItem` without its snapshots cannot be mapped onto.** The copy refuses with
  `UnheldEntryErr` and writes nothing. Fixtures use `holdItems`, which writes the catalogue snapshots first.
- **The existing copy facts' expected values widened; their assertions stay.**
  - E1 and E2 now name items, as the brief requires.
  - E1's expected closure leads with its two item models.
  - The first fact's report and paths include `Laser600` and `ML`.
- **Two authorized facts carry extra branches**, so that keeping the source's binding or importing the source's
  model cannot pass:
  - a glass-plate mapping in `the user's choice overrides…`;
  - an unmodelled target and a preset-bound element on a standalone piece in `a copied experiment reopens…`.
- **KitWindowModelTests is unchanged.**
  - The census lists none of its facts.
  - Its copy fact's E2 uses no item, so the re-read items equal the loaded ones.
  - Step 228 asserts the window's re-read items.
- `KitLibraryTests`' partial-closure fixture matches `KitLibraryError` exhaustively. It now lists `KitLibraryItemErr`
  beside `UnheldEntryErr`, which compiles under FS0025 with no assertion changed.

## Changelog

- 2026-11-09 — Step 0227:
  - copying experiments now maps or adds the items they use, rewrites each descriptor onto its chosen target piece,
    and copies the added items' snapshots and photos;
  - added `KitLibraryItemErr` and `ExperimentCopyReport.items`;
  - `recordExperiment` now refuses a missing piece before writing;
  - the window's copy passes `[]` and re-reads the kit's items;
  - constructor-unit-tests went from 1336 to 1344, with ui-smoke at 238 and ui-tests at 40 over the selected classes.
