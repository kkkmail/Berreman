# Step 0228 — impl log

## Progress

- [x] TableAndElementRotationView: `KitCopySource.items` / `itemRows`, `ChooseCopiedItem`, the four arms
- [x] KitWindowModelTests: item source fixture, four new facts, the read-only fact's record gains the fields
- [x] ScrollExtentTests: `tallestKitBar`'s record gains the fields (deviation, see Decisions)
- [x] Development runs: build, KitWindowModelTests, mutants, final rebuild, constructor-unit-tests, ui-tests
  (KitWindowTests), ScrollExtentTests, ui-smoke, elevation, invariants

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
  - `KitCopySource` gains `items : Item list` (the source's items, read when it opens) and
    `itemRows : ItemCopyRow list` (the mapping table), one-line docs citing S7.2.
  - `TableAndElementRotationMsg` gains `ChooseCopiedItem of ItemId * ItemCopyChoice`, one-line doc.
  - `OpenCopySource`: reads `readItems ()` after `readExperiments ()`; an `Error` from either releases the source
    and shows step 225's `The kit could not be opened to copy from — …`. The source opens with its items and no row.
  - `ToggleCopiedExperiment`: toggles, then resolves `Experiment.itemsUsedBy` for the toggled choice's experiments
    against the source's `items`.
    - `Ok used` sets `itemRows = ItemCopyRow.copyRowsFor used model.kitItems copySource.itemRows`.
    - `Error e` keeps the copy source as it was (selection and rows) and sets `kitStatus` to
      `The experiment could not be chosen to copy — <e.describe>`. Nothing is written.
  - `ChooseCopiedItem (id, choice)`: sets the choice of the row whose `sourceItem.id` is `id`. Without an open
    source, or without such a row, it answers the model unchanged. Nothing is written.
  - `CopyChosenExperiments`: passes `copySource.itemRows` (step 227 passed `[]`). On `Ok`, with items added:
    `Copied <n> experiment(s) and added <m> item(s) from <name>.`; with none, step 225's text. `Error` is unchanged
    (the panel stays, step 225's text). Step 227's "no rows yet" comment is deleted.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`
  - Step 225's `copySourceUnder` is split: `sourceKitUnder` makes `Copy source` through its library, closes it and
    removes `.oc`; `copySourceUnder` and the new `itemSourceUnder` both go through it.
  - `experimentFilesIn` now goes through the new `rootFilesIn folder pattern`, which the item facts reuse for
    `*.item-json`.
  - The read-only copy fact's `KitCopySource` record gains `items = []` and `itemRows = []`; its assertions are
    unchanged.
  - Fixtures (all from step 210's shipped item models through `ItemDraft.toItem`, as `windowCpl1` is):
    - `sourceLaser600` (`SourcePresetChoice` of `PresetIds.source600`) and `sourceQz` (`StackSampleChoice` of
      `SampleIds.glassPlate1mmId` at version 1);
    - `targetLaser600`, the same preset under another id;
    - `dialLaser600`, the same preset under a third id, dial-mounted, so its parameters differ from both Laser600s;
    - `itemE1` (light source bound `BoundByEntryId PresetIds.source600`, assigned `sourceLaser600.id`) and `itemE2`
      (sample bound `BoundByVersion` of the plate's version 1, assigned `sourceQz.id`);
    - `itemSourceUnder` records the two items through `recordItem`, then E1 and E2 through `recordExperiment`;
    - `openedHolding` makes the target holding items through `recordItem` (full model snapshots), then reopens it;
    - `copySourceOf`, and `reopenedCopy`, which reopens a kit read-only and resolves its experiments through
      `Experiment.itemsUsedBy` (record `ReopenedCopy`).
  - Four new facts, exactly the census identities:
    - `choosing experiments lists the items they use, each at its default`;
    - `the user's choice for an item is kept and copied`, with the MapOntoKitItem bench branch;
    - `a choice the copy refuses keeps the panel and writes nothing`;
    - `choosing an experiment with a dangling physical binding reports its id and writes nothing`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ScrollExtentTests.fs`: `tallestKitBar`'s `KitCopySource`
  record gains `items = []` and `itemRows = []`; nothing else changes.

## Decisions

- **`ScrollExtentTests.fs` is edited, outside the listed edits.**
  - The brief: "Every record expression that builds a `KitCopySource` in the tests gains the two fields
    (`KitWindowModelTests`' read-only copy fact and `KitWindowTests`' open-source fact, steps 225, 226)".
  - `KitWindowTests`' open-source fact builds no `KitCopySource`: it dispatches `OpenCopySource`.
  - The step-226 record expression is `ScrollExtentTests.tallestKitBar` (commit 644fe46, step 0226). An F# record
    expression must name every field, so the test project does not compile without the two fields there.
  - The rule ("every record expression … gains the two fields") governs, and the parenthetical misnames the file.
    The edit is the two empty fields only; no assertion changed. `KitWindowTests.fs` needed no change.
- **The toggle's refusal text** is `The experiment could not be chosen to copy — <ItemError.describe>`.
  - The brief asks for "the typed diagnostic" without naming a text.
  - `ItemError` is the typed error `itemsUsedBy` answers. Its `UnknownKitItemErr` reason names the missing id, the
    element and the experiment.
  - `KitLibraryItemErr` was not used: it renders "the items could not be copied", which is wrong for a choice.
  - The fact asserts the exact caption built from `UnknownKitItemErr` with the missing id.
- **A good toggle leaves `kitStatus` as it was.** Step 225's toggle never touched the status, and the brief adds none.
- **The rows resolve against the toggled choice.** "The chosen experiments" are those chosen after the toggle;
  `copyRowsFor` keeps the previous rows' choices, so a choice survives while its item is used.
- **The mapped branch distinguishes the pieces by id, dial and parameters.**
  - `dialLaser600` is dial-mounted, so it carries a `dialZero` parameter that the Laser600s lack.
  - Test modules here keep their own private fixtures. ExperimentCopyTests' `withParameter` is private to that
    module, and its file is outside this step's edits. The dial difference needs no copy of that helper.
- **The add branch is also reopened.** Its E1 resolves `Laser600 (2)` under the source's id, never the target's
  same-preset Laser600. That is the acceptance's "added source identity".
- **`ChooseCopiedItem` with no source, or no row of that id, returns `model` itself.** The facts assert equality
  before a source opens, and for an id with no row.

## Testing state

Development runs; the gate engine gives the verdict (Invariant 6).

- **build** (first): exit 0, 0 errors. Two `SYSLIB0051` warnings came from the vendored
  `MathNetNumerics/Numerics/Exceptions.cs`. That project recompiled this time; the warning is in every earlier full
  build since step 0001 (`.artifacts/0001-build-tests-project.log` …). Log: `.artifacts/0228-dev-build.log`.
- **KitWindowModelTests:** 31 passed (27 + 4). The four census identities are listed by `--list-tests`. Log:
  `.artifacts/0228-dev-kitwindowmodel-tests.log`.
- **Mutants** (`.artifacts/0228-mutants/run_mutants.py`, `summary.txt`). Each was applied alone to the view and the
  test project rebuilt; every one was caught:
  - m1, the open reads no items: all four new facts;
  - m2, the toggle keeps the old rows: all four;
  - m3, the toggle forgets the choices: `the user's choice…`;
  - m4, a refused toggle still toggles: `…dangling physical binding…`;
  - m5, the choice ignored: `the user's choice…`, `a choice the copy refuses…`;
  - m6, Copy passes no rows: `the user's choice…`, `a choice the copy refuses…`;
  - m7, the status omits the items: `the user's choice…`;
  - m8, a refused toggle is silent: `…dangling physical binding…`.
  - The driver restored the view byte for byte (`restored: True`).
- **Edits after the mutants, behaviour unchanged.**
  - One comment's wording changed.
  - The Copy arm's kit name went back into the interpolation holes, bound as `sourceName : KitName`.
  - A first elevation run counted 1316 suppressed findings, against step 227's 1315. The `KitName.value` that a
    `let sourceName = ….name.value` binding unwrapped is an ELEV004 (excluded, measured) that the interpolation
    hole is not.
  - Every run below is over the final text.
- **build** (final): exit 0, **0 warnings, 0 errors**, no lowercase `error` in the log. Log:
  `.artifacts/0228-dev-build-final.log`.
- **constructor-unit-tests** (all discovered): **1348** passed, 0 failed, 0 skipped, in 66 s (1344 + 4). Log:
  `.artifacts/0228-dev-constructor-unit-tests.log`.
- **ui-tests** over `KitWindowTests` (non-smoke, non-kit-acceptance): **11** passed, 0 failed, 0 skipped, in 18 s.
  Log: `.artifacts/0228-dev-ui-tests.log`.
- **ui-smoke** (all discovered): **238** passed, 0 failed, 0 skipped. It took 195 s against the 600 s
  `attempt_timeout_seconds`, so no deadline was exceeded. Log: `.artifacts/0228-dev-ui-smoke.log`.
- **ScrollExtentTests** (the edited fixture, inside ui-smoke): 14 passed in its own run too. Log:
  `.artifacts/0228-dev-scroll-extent-tests.log`.
- **elevation**, with `ASG_ARC_BASE_COMMIT=f395597c…` (the `.arc-base-json` origin) and ELEV004 excluded:
  - `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 204 s;
  - suppressed findings 1315 (the excluded ELEV004, unchanged from step 227; `OpticalConstructor.Ui` 482);
  - known findings 0;
  - log: `.artifacts/0228-dev-elevation.log`.
- **The fourteen `invariant-s22-*` greps:** each finds nothing.
- **Line endings:** `git diff --numstat` equals `--ignore-cr-at-eol`; the three source files are `i/lf w/lf`.
- **Baselines:** `constructor_unit_tests` 1348, `ui_smoke_tests` 238, `ui_tests` 11 (this step's selection is
  `KitWindowTests` alone).

## Artifacts

- `.artifacts/0228-dev-build.log`, `.artifacts/0228-dev-build-final.log`
- `.artifacts/0228-dev-kitwindowmodel-tests.log`
- `.artifacts/0228-mutants/run_mutants.py`, `.artifacts/0228-mutants/summary.txt`,
  `.artifacts/0228-mutants/m{1..8}-*-{build,tests}.log`
- `.artifacts/0228-dev-constructor-unit-tests.log`
- `.artifacts/0228-dev-ui-tests.log`, `.artifacts/0228-dev-scroll-extent-tests.log`
- `.artifacts/0228-dev-ui-smoke.log`
- `.artifacts/0228-dev-elevation.log`

## Gotchas

- **A `KitCopySource` record expression must now name `items` and `itemRows`.** There are two in the tests:
  `KitWindowModelTests`' read-only fact and `ScrollExtentTests.tallestKitBar`.
- **A refused toggle leaves its status after a later good toggle.** Only `OpenCopySource` (success) and the copy's end
  rewrite `kitStatus`.
- **The rows' default reads the window's `kitItems`, while the copy reads the kit's items afresh.** Both are the
  kit's items as last read or written, so they agree unless another writer changed the kit. In that case the copy's
  own `copyRowsFor` keeps the chosen rows and defaults any new ones.
