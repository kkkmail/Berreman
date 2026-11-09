# Step 0228 — state of the world

## Where we are

Spec 0050, section S6 ("Send and receive kits; copy experiments between kits"), step 0228 (IMPLEMENT), obligation
`S72-COPY-MESSAGES`.

- Step 227 made the storage copy map or add the items the copied experiments use, but the window passed no rows.
- This step carries the mapping into the kit window's semantic messages:
  - the copy source holds its items and the mapping table;
  - choosing experiments builds the rows at their defaults;
  - `ChooseCopiedItem` sets a row's choice;
  - Copy passes the rows.
- Step 229 draws the mapping table over these messages.

## What's working

- The copy source now reads its items when it opens and keeps a mapping table (`KitCopySource.items`, `itemRows`).
- Choosing experiments lists every item they use at its default, keeps the user's choice while the item is used, and
  drops the row when no chosen experiment uses it.
- A dangling item id in a chosen experiment is reported with its id; the selection, the rows and the files stay as
  they were.
- Added `ChooseCopiedItem`; Copy now applies the chosen rows and says how many items it added.
- Added four `KitWindowModelTests` facts; constructor-unit-tests went from 1344 to 1348.

## Tests

Development runs over the final source, recorded in `0228-impl-log.md`. The gate engine gives the verdict
(Invariant 6).

- **build:** exit 0, 0 warnings, 0 errors, and no lowercase `error` in the log.
- **constructor-unit-tests** (all discovered): **1348** passed, 0 failed, 0 skipped (1344 + the four census facts).
- **ui-tests** over `KitWindowTests`: **11** passed, 0 failed, 0 skipped.
- **ui-smoke** (all discovered): **238** passed, 0 failed, 0 skipped. It took 195 s against the 600 s
  `attempt_timeout_seconds`; no deadline was exceeded. It includes `ScrollExtentTests`, whose fixture gained the two
  fields.
- **elevation** (from the arc origin `f395597`, ELEV004 excluded): `ELEV-GATE: OK - 0 new finding(s) in 12
  project(s)`, 204 s, with 1315 suppressed findings (unchanged from step 227) and 0 known findings.
- **The fourteen `invariant-s22-*` gates:** each grep finds nothing.
- **The facts bite.** Eight mutants of the view's arms were each caught, and the source was restored byte for byte
  and rebuilt:
  - no items read;
  - rows not rebuilt;
  - choices forgotten;
  - a refused toggle applied;
  - a choice ignored;
  - no rows passed to Copy;
  - the status without its item count;
  - a silent refusal.

  The source was restored byte for byte. After that, only a comment's wording and the copy status's local binding
  changed, with behaviour unchanged, and every run above is over that final text.

```yaml
gates:
  constructor_unit_tests: 1348
  ui_smoke_tests: 238
  ui_tests: 11
```

## Architecture

- **The window keeps the table; storage decides.**
  - `KitCopySource.itemRows` is `ItemCopyRow.copyRowsFor` over the items the chosen experiments use. It is rebuilt
    on every toggle from the previous rows, so a choice survives while its item is used.
  - `copyExperimentsFrom` receives exactly those rows and applies the same `copyRowsFor`, `itemsToAdd` and descriptor
    rewrite as step 227. No second mapping logic exists in the UI.
- **Identity is the `ItemId` alone.**
  - A row is found by `sourceItem.id`.
  - A dangling assigned id is refused through `Experiment.itemsUsedBy`, the same resolver the copy and
    `recordExperiment` use, and is never matched by preset or model.
- **Messages:**
  - `ChooseCopiedItem` is the only new message, and it writes nothing.
  - The toggle and the choice write nothing.
  - Copy is the one write, behind `guardKitWrite`, as in step 225.

## Deferred

Nothing in this step's scope is deferred. Drawing the mapping table and its buttons is step 229, as the brief names it.

## Gotchas

- **`ScrollExtentTests.fs` was edited, outside the listed edits.**
  - The brief requires every `KitCopySource` record expression in the tests to gain the two fields.
  - It names "`KitWindowTests`' open-source fact (step 226)", which builds none.
  - Step 226's record expression is `ScrollExtentTests.tallestKitBar`, and the build needs the fields there.
  - Only `items = []` and `itemRows = []` were added. `KitWindowTests.fs` needed no change.
- **A refused toggle's status stays after a later good toggle.** Only a successful open and the copy's end rewrite
  `kitStatus`.
- **The bench branch tells the same-preset pieces apart by id and by dial.** `dialLaser600` is dial-mounted, so its
  parameters differ from both Laser600s. ExperimentCopyTests' private `withParameter` was not copied.
- The first build of the round recompiled the vendored MathNet and printed its long-standing `SYSLIB0051` warnings.
  The final incremental build printed none.

## Changelog

- 2026-11-09 — Step 0228:
  - the copy panel's item mapping is now part of the kit window's messages: `KitCopySource.items` / `itemRows`, and
    `ChooseCopiedItem`;
  - the toggle builds the rows and refuses a dangling item id;
  - Copy passes the rows and counts the added items;
  - added four `KitWindowModelTests` facts.
