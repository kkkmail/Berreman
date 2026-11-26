# Step 0309 — state of the world

## Where we are

Spec 0050 section S9.2 covers import profiles and the column-mapping wizard (001 §K).

- Step 308 gave the wizard its pure MVU.
- This step adds the wizard's view, `importWizardView`, under fifteen stable `ConstructorUiId` members. The view is
  driven by the same semantic messages as the model tests.
- Next:
  - step 310 has the kit window open the wizard and act on `ImportWizardApplied`;
  - step 311 adds `ImportWizardWindow` (which uses `importWizardWindow`) and joins `ScrollExtentTests`;
  - step 321 makes the heading and Attach caption read the wizard's target (attach or split).

## What's working

- Add `importWizardView`, the column-mapping wizard's view (spec 0050 S9.2; 001 §K):
  - a heading that names the file;
  - delimiter and header boxes and the byte-order mark;
  - every built-in and kit profile, and New column mapping with its name field;
  - a preview grid that scrolls both ways;
  - Attach beside Cancel.
- Under a new mapping, the preview's second row holds one role list per column. A scan role adds its unit and reading
  lists. Each pick sends `SetColumnRole`.
- A refused Attach shows its reason in `importWizardMessage`, and so does an unread file. An unread file offers Cancel
  alone.
- Add the fifteen `ConstructorUiId.importWizard*` ids, and the Domain-derived keys and captions they and the view read.
- Add four headless facts to `ImportWizardTests`.

## Tests

These were development runs of each gate's own command over the final tree. The arc-runner's gate engine runs the gates
themselves. No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0; `0 Warning(s)`, `0 Error(s)`; no `error` in stdout | 38 s, then 17 s after a final one-line comment edit (Ui recompiled, verified by the dll's timestamp) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded | 217 s | 600 s |
| constructor-unit-tests | Passed!, 1522 of 1522, 0 failed, 0 skipped | 73 s | 180 s |
| ui-smoke | Passed!, 251 of 251, 0 failed, 0 skipped | 231 s | 600 s |
| ui-tests (`ImportWizardTests`) | Passed!, 10 of 10, 0 failed, 0 skipped | 7 s | 360 s |
| invariant-s30-\* (16) | each exit 0, `INVARIANT HOLDS` | 0.2 s each | 300 s |

- **ui-tests: 10.** These are the selected class `OpticalConstructor.Ui.Tests.ImportWizardTests` under the gate's exact
  command. The count rises by 4 over step 308's 6, as the slice says. All ten TRX identities are `Passed`, and the four
  new ones carry the slice's exact names:
  - `the import wizard UiIds are the stable intent-named ids`
  - `the wizard shows the sniffed layout, every profile and the preview, with Attach and Cancel in one row`
  - `a new column mapping shows a role per column, and a scan role its unit and reading`
  - `a refused Attach shows its reason in the wizard`

  It was re-run, 10 of 10, after the final comment edit.
- **constructor-unit-tests: 1522** and **ui-smoke: 251** are each equal to step 308's checkpoint. No constructor or smoke
  test changed.
- **The last edit was a comment.** Elevation and ui-smoke ran before it, and the edit changes no code.
- **Elevation** analyzed all 12 product projects (a standalone run with no comparison base). The suppressed ELEV004
  count rose from 1416 to 1426. These are the view's `.value` reads at the control-text and id lines, which the gate
  measures but does not enforce (R7).
- **Red by construction.** Every id, key, caption and the view are new at this step.
- **Mutation check: twelve mutations over the final tree, all killed** (`.artifacts/0309-mutation-checks.py` / `.log`).
  The mutations were:
  - Attach over an unread file;
  - Cancel before Attach;
  - a scan role without its unit and reading lists;
  - role lists under a picked profile;
  - a refusal not shown;
  - the chosen box not filled;
  - the mark's caption ignoring the model;
  - data rows shifted;
  - a reading pick dropping the unit;
  - the heading naming the full path;
  - a scan role's caption listing its unit and reading;
  - the header keys swapped.

  Each restore was verified by SHA-256. The guard history is in Architecture.

```yaml
gates:
  constructor_unit_tests: 1522
  ui_smoke_tests: 251
  ui_tests: 10
```

## Architecture

- **The shape of step 107's import dialog.**
  - The view is a `DockPanel`. Attach and Cancel are docked at the bottom in one horizontal row, using
    `MaterialsWindowView.actionButton` with the positive and negative fills. The `actionButton` right margin is the
    gap. Attach is present only while the source read.
  - The refusal `TextBlock` is docked above that row, in `messageColor`.
  - The heading and choice blocks are docked at the top in a vertical `StackPanel`.
  - A two-way `ScrollViewer` fills the rest.
- **Labelled blocks are a `DockPanel`:** a left label and a `WrapPanel` fill, never a WrapPanel in a horizontal
  StackPanel (AGENTS.md).
- **One choice-box rule for all three groups** (layout, header, profile): the chosen id is an `actionButton` on
  `positiveBackground`, every other id a `verbButton`. Every box sends a fixed message per id, so `actionButton`'s
  once-per-id press subscription is safe.
- **The preview grid.**
  - It has one `Auto` column per preview column. Row 0 holds the names. Under `NewColumnMapping` only, row 1 holds a
    keyed `StackPanel` per column with its role list and, for a scan role, its unit and reading lists. The data rows
    follow, each row's cells truncated to the column count.
  - A preview error shows its caption in place of the grid.
- **The role list** is `wizardRoleChoices` (a private module value), in the slice's order. Its shown item is found by
  caption, so every scan role selects the one `scan variable` item.
- **The lists bind to strings** through `ComboBox.dataItems`, `selectedIndex` and `onSelectedIndexChanged`. This is the
  first FuncUI `ComboBox` in the repo.
  - The handlers resubscribe on every patch (`SubPatchOptions.Always`), so a unit or reading pick keeps the column's
    current other half.
  - The name field uses the repo's text-field pattern (`TextBox.onTextChanged` through `ControlCaption`).
- **Keys and captions** live in `ConstructorUiIdOfDomain.fs` as extension members, with their receivers qualified
  (`Kit.`, `ImportProfiles.`). The keys are explicit matches, not `text`, so an id never moves with a display text.
- **Guard history.** The first version guarded both bindings against echoing a value the view sets (`picked <> shown`,
  `name <> m.profileName`).
  - Both guard mutants survived, including after the facts gained a round trip that re-creates the lists and the field
    over held, non-default values.
  - So FuncUI 2.0 never sends back a programmatic value on creation, on a patch or on re-creation, and the guards were
    dead code. They were removed, and the round-trip assertions stay as the evidence.
- Every new shape is UI-only. No stored format, schema, version, migration or package changed.

## Deferred

Nothing. Every requirement of the slice landed this round. These are later steps' slices, not deferrals:

- the kit window's messages (step 310);
- the wizard's window and `ScrollExtentTests` entry (step 311);
- the split target (step 321).

## Gotchas

- **`importWizardWindow` is declared, but no control carries it yet.** Step 311's `ImportWizardWindow` sets it as the
  window's `Name` and `AutomationId`. The slice adds it here with the other fourteen ids.
- **The fact helpers are private copies.** `ImportWizardTests` has its own small set: `tryControl`, `controlOf`,
  `pressControl` and the others.
  - `ImportDialogTests`' helpers are private.
  - `SlotBarTests`' internal ones compile after this file.
  - Making either set visible would edit a file outside this step's authorized effects.
  - The other window test classes each keep their own copies too.
- **The facts drive the mounted view by meaning.** They press boxes and set a list's `SelectedIndex` or a field's `Text`,
  and they record every message the view sends (a wrapping `update` in `Program.mkSimple`). An extra or missing message
  fails them.
- **Extensions to the slice's four facts, no new identities:**
  - fact 1 also pins every delimiter and header key;
  - fact 3 adds the leave-and-return round trip and unit/reading picks;
  - fact 4 adds the field clearing the refusal, a second refusal (the unmapped signal), the name surviving a round trip,
    and an unread file's message with no Attach and no grid.
- **The heading uses `FileName.leafName`** (Softellect.Sys, `Path.GetFileName`). A test file named
  `C:/Lab/2026-07-15/phone-roi.csv` shows `Import phone-roi.csv`.
- A shell `grep -c $'\r'` inside `$(…)` miscounts CRs (it matched every line). The files were checked with Python: 0
  CR bytes in all four.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11 — Step 0309: the column-mapping import wizard's view and stable ids (S9.2).
  - `importWizardView`.
  - Fifteen `ConstructorUiId.importWizard*` ids.
  - The `DataDelimiter`, `HeaderRowChoice`, `ByteOrderMark`, `ImportProfileName`, `DataColumnNumber`,
    `ImportProfileError`, `ColumnRoleChoice`, `ScanColumnUnit` and `ScanReading` keys and captions.
  - Four headless `ImportWizardTests` facts.
