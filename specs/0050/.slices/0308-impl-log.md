# Step 0308 — impl log

## Progress

- Read the task, both system prompts, the project prompt, AGENTS.md and the slice.
- Read the domain the wizard reuses:
  - `ImportProfiles.fs` (steps 300 and 307);
  - `Kit.fs` (steps 291 and 299);
  - `LabFileImport.fs` (`KitFileText.previewWith`, step 307);
  - step 107's `ImportDialogView.fs`, for the context's shape.
- Before coding: found step 107's recorded ELEV013 deviation (`0107-impl-log.md`). Its free single-parameter
  `initImportDialog` became a member of `ImportDialogContext`, and this step's `initImportWizard` has the same shape.
  Read the analyzer (`C:\GitHub\Softellect\Analyzers\NoSingleParameterFunction.fs`, read-only) to confirm the rule
  fires. The module `ImportWizardView` is not named for `ImportWizardContext`, so the module-of-the-type exemption does
  not apply.
- Wrote `ImportWizardView.fs`:
  - the six types;
  - `ImportWizardContext.initImportWizard`, as a member;
  - `updateImportWizard`;
  - a private two-parameter helper, `wizardColumnsOver`, the one rule that remakes the columns over a preview.
- Added one `<Compile>` line after `SetupGridView.fs`, with no comment.
- Wrote `ImportWizardTests.fs` (six facts) and added one `<Compile>` line after `ImportDialogTests.fs`, with no
  comment.
- First build: exit 0, 0 errors. The only 2 warnings are the vendored MathNet `SYSLIB0051`, the same as the prior
  steps.
- The selected class passed 6 of 6. Then ran:
  - constructor-unit-tests: 1522 of 1522;
  - ui-smoke: 251 of 251;
  - elevation: OK, 0 new findings in 12 projects;
  - the 16 invariants: all hold.
- Mutation checks: ten mutations of `ImportWizardView.fs`, run by `.artifacts/0308-mutation-checks.py`. All ten were
  killed, and the source's SHA-256 was verified restored.
- Final build over the restored tree: exit 0, `0 Warning(s)`, `0 Error(s)`. The Ui assembly was recompiled from the
  restored source.
- Final runs:
  - the exact ui-tests gate command: 6 of 6, every TRX identity matching the slice's names;
  - ui-smoke: 251 of 251.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ImportWizardView.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj` (+1 `<Compile>` line)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ImportWizardTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` (+1 `<Compile>` line)

All four are LF, with 0 CR bytes (checked with Python).

## Testing state

Green; commit-ready. These were development runs over the final tree (`0308-state-of-the-world.md`, Tests):

- build: exit 0;
- elevation: OK, 0 new findings in 12 projects;
- constructor-unit-tests: 1522 of 1522;
- ui-smoke: 251 of 251;
- ui-tests over `ImportWizardTests`: 6 of 6;
- the 16 `invariant-s30-*`: all hold.

No run exceeded its deadline. No requirement of the slice is left open.

## Artifacts

- `.artifacts/0308-build-01.log` and `0308-build-02.log`, `0308-ui-tests-01.log` and `0308-ui-tests-02.log`, `0308-constructor-unit-tests-01.log`, `0308-ui-smoke-01.log` and `0308-ui-smoke-02.log`,
  `0308-elevation-01.log`, `0308-invariants-01.log` (with `0308-invariant-commands.txt`)
- `.artifacts/0308-mutation-checks.py` and `0308-mutation-checks.log`

## Gotchas

- **Spec deviation, ELEV013: `initImportWizard` is a member of `ImportWizardContext`, not a free function.** The
  slice names `initImportWizard (context : ImportWizardContext) : ImportWizardModel`. That signature takes one of our
  types and nothing else, which ELEV013 refuses, and AGENTS.md says the analyzers win. Step 107 resolved
  `initImportDialog` the same way.
  - Callers write `context.initImportWizard`.
  - Its behaviour is the slice's.
  - The contract symbol lives at `OpticalConstructor.Ui.ImportWizardView.ImportWizardContext.initImportWizard`.
- **A kit profile's pick remakes the columns.** It changes the layout, so it remakes the columns by the same rule as
  `SetImportDelimiter` / `SetImportHeader`, keeping "one choice per preview column" true. The slice says only that it
  sets the layout. With `Lab scan` over the fixture, the columns are the same five, so no fact depends on this choice.
- **Messages that are "inert" leave the model equal**, `lastError` included:
  - a pick of an unknown profile;
  - `SetColumnRole` on a column the model lacks;
  - `ApplyImportWizard` over an unread file.

  Every other message clears `lastError`, as the slice says.
- **Attach's error order** is the slice's:
  1. a blank name (`BlankProfileNameErr "the profile name is empty"`);
  2. then `availableAmong` (built-in, then duplicate);
  3. then `ColumnRoles.tryOfChoices`.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.
