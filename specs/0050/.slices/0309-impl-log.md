# Step 0309 — impl log

## Progress

- Read these, in order:
  - the task;
  - both system prompts and the project prompt;
  - AGENTS.md;
  - the slice and its `.gates`;
  - step 308's SoW and impl-log.
- Read what the view reuses:
  - `ImportWizardView.fs` (step 308);
  - `ImportDialogView.fs` and `ImportDialogTests.fs` (step 107);
  - `MaterialsWindowView.actionButton` and `verbButton`;
  - `Kit.fs` and `ImportProfiles.fs` (the types the captions are for);
  - `KitFileText.previewWith`;
  - `ConstructorUiIdOfDomain.fs`;
  - the import dialog's group in `UiIds.fs`.
- Wrote `0309-impl-plan.md`.
- **`UiIds.fs`.** Added the fifteen `importWizard*` members after the import dialog's group, under one comment line
  citing spec 0050 S9.2. Each has a one-line doc, and the texts are exactly the slice's.
- **`ConstructorUiIdOfDomain.fs`.** Appended these members, each with a one-line doc citing S9.2:
  - `DataDelimiter.uiKey` and `.caption`;
  - `HeaderRowChoice.uiKey` and `.caption`;
  - `ByteOrderMark.caption`;
  - `ImportProfileName.uiKey` and `.caption`;
  - `DataColumnNumber.uiPosition`;
  - `ImportProfileError.caption`;
  - `ColumnRoleChoice.caption`;
  - `ScanColumnUnit.caption` and `ScanReading.caption`.

  The receivers are qualified (`Kit.`, `ImportProfiles.`), not opened.
- **`ImportWizardView.fs`.**
  - Added the Avalonia, FuncUI and `MaterialsWindowView` opens, and changed the module doc to "MVU and projection".
  - Appended `wizardRoleChoices`, the role list in the slice's order: nine roles, then m00…m33.
  - Appended `importWizardView`.
- The Ui project built: exit 0, with no warning other than the vendored `SYSLIB0051`. The FuncUI 2.0 `ComboBox` DSL
  names (`dataItems`, `selectedIndex`, `onSelectedIndexChanged`) resolved.
- **`ImportWizardTests.fs`.** Added the Avalonia and Elmish opens and a private set of headless helpers. Added the four
  facts, each under the slice's exact name.
- The Ui.Tests project built: exit 0. The selected class passed 10 of 10: the six of step 308 and the four new ones.
  Each TRX identity matches the slice's name.
- **Mutation checks, first run** (`.artifacts/0309-mutation-checks.py`). This run was over the first version of the
  view, whose lists and name field carried echo guards. 12 of 14 mutants were killed
  (`.artifacts/0309-mutation-checks-guarded.log`). The two survivors were the guards themselves:
  - a list's `if picked <> shown`;
  - the name field's `when name <> m.profileName`.
- **Extended two facts** with a round trip that re-creates the lists and the name field over held, non-default values,
  and asserts the exact messages sent:
  - the scan column at radians and a raw dial reading;
  - the name `Bench A`;
  - Phone ROI CSV, then New column mapping.

  Fact 4 also presses Attach a second time, showing the unmapped-signal refusal. Both guard mutants still survived
  (`.artifacts/0309-mutation-checks-echo.log`). So, in every reachable path, FuncUI 2.0's `ComboBox` and `TextBox`
  never send back a value the view sets, whether on creation, on a patch or on re-creation. The guards were dead code.
- **Removed both guards.**
  - The name field now uses the repo's existing text-field pattern (`KitWindowView`'s naming-template field).
  - The lists keep `SubPatchOptions.Always`, because a unit or reading pick must read the column's current other half.
    The killed mutant "a reading pick forgets the unit" proves that.
  - Rebuilt; the class passed 10 of 10 again.
- Dropped the two guard mutants from the script. Re-ran the remaining twelve over the final view: all twelve were
  killed (`.artifacts/0309-mutation-checks.log`), and every restore matched its SHA-256.
- **Final runs over the final tree:**
  - the build gate command (`.artifacts/0309-build.log`): exit 0, `0 Warning(s)`, `0 Error(s)`, 38 s;
  - elevation (`.artifacts/0309-elevation.log`): OK, 0 new findings in 12 projects, 217 s;
  - constructor-unit-tests: 1522 of 1522, 73 s;
  - the ui-tests gate command over `ImportWizardTests`: 10 of 10, 7 s;
  - ui-smoke: 251 of 251, 231 s;
  - the 16 invariants: all hold.
- **Last edit.** Reworded one code comment in the view (`choiceBox`). Rebuilt (exit 0, `0 Warning(s)`; the Ui dll is
  newer than the edit) and re-ran the class: 10 of 10.
- Checked line endings with Python: 0 CR bytes in all four source files and in this round's outputs.
  `git diff --numstat` equals `--ignore-cr-at-eol`.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs` (+32 lines)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs` (+82 lines)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ImportWizardView.fs` (the opens, the module doc, the view)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ImportWizardTests.fs` (the opens, the helpers, four facts)

## Testing state

Green and commit-ready. These were development runs over the final tree (`0309-state-of-the-world.md`, Tests):

- build: exit 0;
- elevation: OK, 0 new findings in 12 projects;
- constructor-unit-tests: 1522 of 1522;
- ui-smoke: 251 of 251;
- ui-tests over `ImportWizardTests`: 10 of 10, rising by 4;
- the 16 `invariant-s30-*`: all hold.

No run exceeded its deadline. Every requirement of the slice landed: the fifteen ids, the domain keys and captions, the
view, and the four facts.

## Gotchas

- **Private test helpers.** `ImportWizardTests` holds its own private headless helpers. The reusable ones are
  `ImportDialogTests`' (private) and `SlotBarTests`' (internal, but compiled after this file). Making either visible
  would edit a file outside this step's authorized effects.
- **`importWizardView` has no echo guards.** The mutation evidence above shows they were dead code under FuncUI 2.0.
  If a later FuncUI starts replaying property values on subscribe, the facts' exact-message assertions will fail and
  name the echo.
