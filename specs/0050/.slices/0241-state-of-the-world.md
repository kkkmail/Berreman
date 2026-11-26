# Step 0241 — state of the world

## Where we are

Spec 0050, section S7.2 ("The Items panel, the flow strip and item mapping on copy"), step 0241 (IMPLEMENT),
obligation `S72-ITEMS-PANEL-VIEW`.

- Steps 239–240 gave the Items panel its model state and messages.
- This step adds the panel's view, `ItemsPanelView.itemsPanelView`, a surface in a file of its own. It shows:
  - the kit's items;
  - the "Add item from library model…" form;
  - the selected item's label, model, dial, notes, photo, parameter grid with its editor, and revisions.

  Every control carries its `ConstructorUiId`.
- No window mounts the panel yet:
  - the flow strip's view is 0242's;
  - the App's item-model wiring is 0243's.

## What's working

- Added the Items panel view. It lists the kit's items and offers the add form, with the models grouped under Item
  models, Light sources, Polarizers and Samples. It shows the selected item's label, model, dial, notes, photo,
  parameter grid, parameter editor and revisions.
- Added the panel's 25 fixed `ConstructorUiId`s and five derived ones. Every offer id and caption carries its item
  model's GUID and version, and a bound offer also carries its preset or sample.
- Added the grid's cells (`Parameter.gridCells`), the item-model caption (`itemModelCaption`) and the captions of a
  dial and a revision. The caption shows `(model not in the library)` when a store lacks the model.
- `pickDataFile` is public and takes a title. The item photo picker uses it with "Choose item photo".
- A kit opened read-only offers no item write in the panel.
- Added six `ItemsPanelTests` facts: ui-tests over the selection 7 → 12 and ui-smoke 242 → 243.

## Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster:

- build;
- elevation;
- constructor-unit-tests;
- ui-smoke;
- ui-tests, selected to `DataFilePickerFolderTests` and `ItemsPanelTests`;
- the fourteen s24 invariants.

The worker's development runs over the final tree, with logs under `specs/0050/.artifacts/0241-*`:

- **build:** exit 0, `0 Error(s)`.
  - The incremental solution build had `0 Warning(s)`.
  - The final `--no-incremental` rebuild shows only two `SYSLIB0051` warnings. They come from vendored
    `MathNetNumerics/Numerics/Exceptions.cs`, which this step does not touch. Earlier steps' builds were
    incremental and never recompiled it. See Gotchas.
- **constructor-unit-tests:** `Passed!`, 1377 succeeded, 0 failed, 0 skipped, in 68 s (estimate 180 s). This is
  checkpoint 239's 1377, unchanged.
- **ui-smoke:** `Passed!`, 243 succeeded, 0 failed, 0 skipped, in 202 s (attempt timeout 600 s). That is checkpoint
  239's 242 plus `pressing Add item, a model, a label and Add records the item in the kit`, `Passed` in the TRX.
- **ui-tests (the two selected classes):** `Passed!`, 12 succeeded, 0 failed, 0 skipped, in 7 s. That is
  `DataFilePickerFolderTests`' 7 plus five facts, each `Passed` in the TRX under its declared id:
  - `the Items panel UiIds are the stable intent-named ids`;
  - `the Items panel lists the kit's items and shows the selected item's model, dial, notes, photo, parameters and
    revisions`;
  - `the add form lists the library models by kind and offers Add and Cancel on one row, Cancel to the right`;
  - `the parameter editor offers Known and Unknown, and Set and Cancel on one row`;
  - `a kit opened read-only offers no item write in the Items panel`.
- **elevation** from the arc origin `f395597`: exit 0, `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, in
  196 s, on the final rebuilt tree. Known findings: 0. The Ui project's suppressed count rose 494 → 509; all 15 are
  the excluded, measure-only ELEV004.
- **The fourteen s24 invariant greps,** untracked files included: none finds anything.
- **Mutation check:** twelve mutations of the new code were each caught by a declared fact
  (`0241-mutations.py`, `0241-mutations-run.log`). The source was then restored byte for byte (same SHA-256) and
  rebuilt. The mutations were:
  - a preset id without its binding;
  - a caption without the model GUID;
  - the calibration provenance;
  - revisions oldest first;
  - Choose photo, the notes and Set under a read-only kit;
  - Cancel left of Add;
  - an Add row not keyed by its draft (caught by the smoke fact);
  - a preset-only bound caption;
  - an unfilled selected row;
  - a hidden refusal.

No run came near a deadline. No test is removed or renamed, and no assertion is removed or weakened.

```yaml
gates:
  constructor_unit_tests: 1377
  ui_smoke_tests: 243
  ui_tests: 12
```

## Architecture

- **The view lives in its own file.** `ItemsPanelView.fs` holds the whole surface and sits between `KitStartView.fs`
  and `KitWindowView.fs`. Its private helpers are typed: `itemsPanelChoice`, `itemsPanelListBox`, `itemsPanelField`,
  and two `ControlCaption` members.
- **Offer identity comes from the choice's own `instance`**, the identity the new item will pin, so the id cannot
  drift from what is recorded. The caption carries the same identities:
  - the model's name, version and GUID;
  - a preset's name and entry id;
  - a sample's name, version and GUID.
- **`itemModelCaption` resolves through the window's stores.**
  - The model always comes from `itemModels.resolveItemModelVersion`, at its stored version.
  - The bound entry comes from `library.tryGetEntry` or `samples.resolveVersion`.
  - A store's `None` or error is the private typed `MissingItemModelLookup`.
- **Action rows are keyed by what they send.** `MaterialsWindowView.actionButton` subscribes its press once per id.
  Add and Set send a payload that changes with typing, so their rows are keyed by the draft or texts.
- **Read-only.** The panel withholds every write control under a read-only kit: Add item, Add, Set, Choose photo and
  Remove photo, with the label and notes disabled. This holds even when a form or editor is open.

## Deferred

Nothing in this step's scope is deferred. Mounting the panel behind the flow strip's Items step (0242) and threading
the App's item models (0243) are later slices' own scope.

## Gotchas

- **Headless fonts are wide.** A 140-character light-source caption measures about 2150 px headless. A list box that
  stretched to its panel put a short offer's centre off the 1000 px window, and the first smoke run missed its
  press. List boxes now keep their caption's width (`itemsPanelListBox`).
- **The offer captions are long.** The slice's MUST asks for the item-model GUID and version in every caption, and
  for the binding's identity in a bound one. A preset offer therefore names two GUIDs. The panel scrolls sideways.
- **The panel's wrapping lines do not wrap.** The slice's `ScrollViewer` has both scroll bars `Auto`, which gives its
  content unbounded width.
- **A from-scratch build warns in vendored MathNet.** It raises `SYSLIB0051` in `MathNetNumerics/Numerics`. The
  warning is pre-existing and outside this step's touches. Incremental builds, the build gate's included, do not
  recompile it.
- **Selected ui-tests counts are not checkpointed.** The checkpoint records only `constructor_unit_tests` and
  `ui_smoke_tests`. The 12 is this step's selection: 7 + 5.

## Changelog

- 2026-11-10 — Step 0241:
  - added `ItemsPanelView.itemsPanelView`;
  - added the Items panel's ids and captions, `ParameterGridCells` / `gridCells` and `itemModelCaption`;
  - made `pickDataFile` public with a title;
  - added six `ItemsPanelTests` facts (ui-tests selection 7 → 12, ui-smoke 242 → 243).
