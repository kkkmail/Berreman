# Step 0296 — impl log

## Progress

- 2026-11-11: read the task, the system prompts, the project prompt, the slice, manuals 013 / 015 and the prior SoW
  (0295). Wrote the impl plan.
- **Controls.**
  - `UiIds.fs` gained the seven ids.
  - `ExperimentControls.fs` gained `ExperimentRoleMark`, `ExperimentRoleOption`, the row's `badge`, the six state
    fields and the five handlers.
  - The view now draws the scan summary under the range, Add variable / Add block beside Add / New, the keyed
    bookkeeping block (role row, Session, Notes), and each row's badge text.
- **Ui.**
  - `ConstructorUiIdOfDomain.fs` gained `ExperimentRole.uiKey` and `.caption`, and the source-side captions
    `SessionName.caption` and `ExperimentNotes.caption`.
  - `TableAndElementRotationView.fs`: `experimentState` fills the fields, and `experimentHandlers` sends
    `ExpAddScanAxis`, `ExpAddScanBlock`, `SetExperimentRole`, `SetExperimentSession` and `SetExperimentNotes`.
- **Tests.**
  - Four ui-tests and one ui-smoke fact in `ExperimentControlsTests`.
  - The handler records in `ExperimentControlsTests` and `WorkbenchInfoAndCaptureTests` gained the five fields as
    `ignore`.
  - The `ScrollExtentTests` Inverse fact loads E1 and holds its axis.
- **First elevation run: one finding.** ELEV013P: the private `scanSummary` took only `ExperimentControlsState`. It
  is now `member internal this.scanSummaryPanel` on the state, the `ItemsPanelView` pattern. The rerun follows.
- **Mutation check.** A constant bookkeeping key fails the loaded-experiment fact (`Expected: DarkRun, Actual:
  CalibrationRun`: the press went to the previously loaded E2). The file was restored byte for byte (SHA-256
  `11c3dd7e…9122` before and after) and rebuilt.
- **Elevation rerun:** OK, 0 new findings in 12 projects. ui-smoke and constructor-unit-tests were rerun over the
  final build: green.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/ExperimentControls.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ExperimentControlsTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/WorkbenchInfoAndCaptureTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ScrollExtentTests.fs`

`ElementMenuTests.fs` and `InverseConstructorTests.fs` are unchanged, as the slice says.

## Decisions

See the SoW's Architecture and Gotchas.

## Testing state

The final runs are over the final build, with the gates' own commands:

- **build:** exit 0, 0 warnings.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 206 s.
- **constructor-unit-tests:** 1495 / 1495, 69 s.
- **ui-smoke:** 250 / 250, 206 s.
- **ui-tests:** 124 / 124 over the seven selected classes, 133 s.
- **The 16 s29 invariants:** each finds nothing.

No run went over its deadline. Commit-ready.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0296-build-dev-0{1,2,3}.log` and `0296-gate-build.log`;
- `0296-gate-elevation.log`;
- `0296-gate-constructor-unit-tests.log`, `0296-gate-ui-smoke.log` and `0296-gate-ui-tests.log`;
- `0296-trx/` (the gate TRX files);
- `0296-mutation/` (the original file and the failing run's log);
- `0296-dev-*.log`.

## Gotchas

See the SoW.
