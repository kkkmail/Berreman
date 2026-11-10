# Step 0241 — impl log

## Progress

1. Read the protocol, the project prompt, the slice, AGENTS.md and the 0239 / 0240 state of the world.
2. `UiIds.fs`: added the Items panel group after the kit-bar group. It holds 25 fixed ids, each with a one-line doc.
3. `ConstructorUiIdOfDomain.fs`:
   - added `kitItemRow`, `addItemOffer`, `newItemDialChoice`, `itemParameterRow` and `itemRevisionRow`;
   - added the `caption` members of `ItemModelChoice`, `ItemDial` and `ItemRevisionRecord`;
   - added `ParameterName.caption`;
   - added `ParameterGridCells` and `Parameter.gridCells`.
4. `TableAndElementRotationView.fs`:
   - `pickDataFile` is public and takes a title;
   - `attachDataFileButton` passes `Attach measured-data file`;
   - added the private `ItemModelLookup` and `itemModelCaption` beside `kitWrites`.
5. New `ItemsPanelView.fs`, compiled after `KitStartView.fs` with one `<Compile>` line.
6. New `ItemsPanelTests.fs`, last in the Ui.Tests compile list: six facts, five without a trait and one `ui-smoke`.
7. **First smoke run: red.** The offer press missed its box, because every offer box stretched to the widest caption
   and headless fonts are wide. A 2182 px box put the circular polarizer's centre at x = 1091, past the 1000 px
   window.
   - Fix: list boxes (offers, item rows, the Add verb) sit in a left-aligned holder, `itemsPanelListBox`, at their
     own caption's width.
   - The tests' `childIds` reads through that holder.
8. Extended fact 2 with the bound and missing-model captions:
   - a source-preset item and a stack-sample item;
   - the same model over an empty item-model store, which shows `(model not in the library)`.

   It also checks the revisions newest first, over an item published twice.
9. Strengthened the smoke fact. It types the label in two steps, so an Add box shown at the first step must send the
   label as it reads at the press.
10. Elevation over the final tree; a twelve-mutation check; the final `--no-incremental` rebuild; the gate suites.
11. Shortened four of the new `ConstructorUiIdOfDomain` docs to one line each, as part (2) asks. The edit is to
    comments only.
    - After it, the incremental build gave `0 Warning(s)` and `0 Error(s)`.
    - `DataFilePickerFolderTests` and `ItemsPanelTests` with both traits passed: 13 succeeded, 0 failed.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ItemsPanelView.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ItemsPanelTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`

All seven are in the slice's authorized `edits`.

## Decisions

- **Offer ids and captions follow the slice's MUST**, which overrides the earlier how-to shapes. Every id and every
  caption carries the item model's GUID and version.
  - The id is built from the choice's own `instance`, the identity the item will pin:
    - `AddItemOffer_<model GUID>_v<n>`;
    - a preset adds `_<entry id>`;
    - a sample adds `_<sample GUID>_v<k>`.
  - The caption:
    - an item model: `<name> v<n> (<model GUID>)`;
    - a preset: `<preset name> (<entry id>) — <model caption>`;
    - a sample: `<sample name> v<k> (<sample GUID>) — <model caption>`.

  So two offers that share a model differ in both id and caption, even when two presets share a name.
- **`itemModelCaption` resolves through the window's stores.**
  - The model always resolves through `itemModels.resolveItemModelVersion`, its stored version, and is never taken
    from the preset or sample.
  - A preset's name comes through `library.tryGetEntry`, and a sample's `<name> v<k>` through
    `samples.resolveVersion`.
  - Captions: an item model is `<name> v<n>`; a bound item is `<bound caption> (<name> v<n>)`.
  - Any store that answers `None` or an error gives the private typed `MissingItemModelLookup`, rendered
    `(model not in the library)`. A bound item whose model is missing never shows its preset's name alone, as the
    slice's MUST requires.
- **`pickDataFile`** sets `options.Title` after `buildDataFilePickerOptions`. That builder, and the
  `DataFilePickerFolderTests` that pin its title, are unchanged.
- **Every write control is withheld under a read-only kit**, not only those the how-to names: Add item, Add, Set,
  Choose photo and Remove photo, plus the disabled label and notes. A form or editor opened by its message still
  offers no Add or Set. The slice's acceptance asks for this ("every write control withheld").
- **Stale press handlers.** `MaterialsWindowView.actionButton` subscribes its press once per id
  (`SubPatchOptions.OnChangeOf id`). An Add or Set box kept across renders would therefore send the draft or state of
  its first render. The two action rows are keyed by the draft or texts they send, so a changed draft makes a new
  box. Every other choice box sends a payload fixed by its id.
- **Text boxes.**
  - The selected item's label and notes subscribe with `SubPatchOptions.Always`, so a commit compares against the
    current item.
  - The selected item's block is keyed by its id.

## Testing state

Development runs over the final tree, using the gates' own commands. Every run below is green.

- **Build.**
  - The solution's incremental build gave exit 0 with `0 Warning(s)` and `0 Error(s)`.
  - The final `--no-incremental` rebuild (`0241-build-final.log`) gave exit 0 and `0 Error(s)`. It has two
    `SYSLIB0051` warnings from vendored `MathNetNumerics/Numerics/Exceptions.cs`, untouched by this step.
- **constructor-unit-tests.** `Passed!`: 1377 succeeded, 0 failed, 0 skipped, in 68 s (`0241-constructor-unit-tests.log`
  and `.trx`).
- **ui-smoke.** `Passed!`: 243 succeeded, 0 failed, 0 skipped, in 202 s against the 600 s timeout (`0241-ui-smoke.log`
  and `.trx`). That is checkpoint 239's 242 plus the new smoke fact.
- **ui-tests, selected to `DataFilePickerFolderTests` and `ItemsPanelTests`.** `Passed!`: 12 succeeded, 0 failed,
  0 skipped, in 7 s (`0241-ui-tests.log` and `.trx`). That is 7 plus the five declared facts.
- **TRX check.** All six declared test ids appear verbatim with outcome `Passed`.
- **elevation.** The ELEV gate ran from the arc origin `f395597`: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`
  (`0241-elevation.log` before the test-only edits, `0241-elevation-final.log` after the final rebuild).
  - The Ui project's suppressed findings rose 494 → 509. All are ELEV004, which the gate command excludes (R7).
- **s24 invariants.** All fourteen greps, untracked files included, find nothing.
- **Mutations.** Twelve of twelve were caught (`0241-mutations.py`, `0241-mutations-run.log`). The source's SHA-256 was
  identical before and after (`0241-source-before-mutations.sha256`).
- **Line endings.** Every touched file is LF; `git ls-files --eol` shows `i/lf w/lf`.

`commit_ready: true`: every part (1)–(4) of the slice and every declared test is delivered.

## Artifacts

- `specs/0050/.artifacts/0241-build-ui-1.log` — the first Ui project build.
- `specs/0050/.artifacts/0241-build.log` — the solution build.
- `specs/0050/.artifacts/0241-items-panel-class.log`, `0241-items-panel-smoke.log` — class runs.
- `specs/0050/.artifacts/0241-elevation.log`, `0241-elevation-final.log` — the ELEV gate from the arc origin.
- `specs/0050/.artifacts/0241-build-final.log` — the final `--no-incremental` solution build.
- `specs/0050/.artifacts/0241-constructor-unit-tests.log` / `.trx`, `0241-ui-smoke.log` / `.trx`,
  `0241-ui-tests.log` / `.trx` — the gate suites.
- `specs/0050/.artifacts/0241-mutations.py`, `0241-mutations-run.log`, `0241-mutations.log`,
  `0241-source-before-mutations.sha256` — the mutation check.

## Gotchas

- **Headless fonts are wide.** The 140-character light-source captions measure about 2150 px headless. A list box
  that stretches to its panel's width therefore has its centre off screen. Keep list boxes at their caption's width
  (`itemsPanelListBox`).
- **The offer caption's GUIDs.** "The addItemOffer id and caption MUST always include the item-model GUID/version" is
  read literally: the caption carries the GUID as well as `v<n>`. A bound offer's caption adds the preset's entry id,
  or the sample's GUID and version, so captions stay distinct even for two presets of one name. This overrides the
  how-to's earlier preset caption, "its entry name".
- **A pre-existing MathNet warning.** A from-scratch build warns `SYSLIB0051` in vendored MathNet. Earlier steps'
  "final" builds were incremental and never recompiled it; the build gate's incremental build does not either.
- **The panel's wrapping lines do not wrap.** The slice asks for a `ScrollViewer` with both scroll bars `Auto`, which
  gives its content unbounded width. The step follows the slice. The notes box wraps within its own `MaxWidth`.
