# Step 0185 — IMPLEMENT — impl-plan

## Approach

A kit opened read-only refuses its four write messages, and the window withholds the controls that send them
(spec 0050 S5.3). The model decides once, through a new `kitWrites : ControlAvailability` member on the extension
that holds `kitCaption`. The update guard and the view both read that member.

1. **Model (`OpticalConstructor.Ui/TableAndElementRotationView.fs`).**
   - Add `member model.kitWrites`: `ReadWriteKitAccess` maps to `ControlOffered`, and `ReadOnlyKitAccess` to
     `ControlWithheld`.
   - Add one private guard, `guardKitWrite`, between the extension and `updateTableAndElementRotation`.
     - Under `ControlWithheld` it answers the model with only `kitStatus` set to
       `The kit is open read-only: nothing is saved.`
     - Under `ControlOffered` it runs the arm.
   - The `ExpCommit`, `ExpRemove`, `AttachDataFileTo` and `SceneSave` arms call the guard around their unchanged
     bodies. No other arm changes.
2. **Controls (`OpticalConstructor.Controls/ExperimentControls.fs`).**
   - `ExperimentControlsState.removalAvailability` goes after `addAvailability`. It is `ControlOffered` in
     `emptyExperimentControls`.
   - `rowActionButton` takes a `ControlAvailability` after its background. It sets `isEnabled` and the 0.4 opacity
     the way `actionButton` does.
   - `collectionRow` takes the Remove availability: View passes `ControlOffered`, and `collectionBlock` passes
     `state.removalAvailability`.
3. **View (`TableAndElementRotationView.fs`).**
   - `experimentState`: `addAvailability` is withheld under `kitWrites = ControlWithheld` and follows today's rule
     otherwise. `removalAvailability` is `model.kitWrites`.
   - `attachDataFileButton` takes the availability. When withheld it is disabled, at 0.4 opacity, and opens no
     picker.
   - `workbenchButton` calls a new private `availableWorkbenchButton` with `ControlOffered`. `Save scene` calls
     `availableWorkbenchButton model.kitWrites`.
4. **Tests.**
   - `KitWindowModelTests` gets two facts: the refusal, which also checks the folder's bytes, and the access
     mapping.
   - `KitWindowTests` gets the headless read-only and read-write control check.
   - `InverseConstructorTests.a kit write that fails…`: only fixture lines change. The access stays read-write, so
     the write reaches the `readOnlyView` proxy and fails there.

5. **Approved fixture amendment (manual 014).** Delete only the scene-save protected fixture's read-only access
   override; retain its refusing proxy and all assertions. Preserve both refusal paths through 0187, 0188 and 0260.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Controls/ExperimentControls.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/InverseConstructorTests.fs`

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SceneSaveLoadTests.fs` (manual 014)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LauncherTests.fs` (additional operator approval)

## Risks

- **The inverse fact's first refusal reason changes.** Under `ReadWriteKitAccess`, `snapshotReferences` no longer
  short-circuits. The inverse scene's elements are all unbound, so the experiment has no library references, and
  the refusal comes from the proxy's `writeKindFile`. The expected `refusal` value is a fixture line, and it must
  name that reason.
- **Worker gate ownership.** The IMPLEMENT worker delegates execution to the gate engine. The approved operator
  recovery below has now run the full frozen roster through that engine.
- **The headless controls are disabled `Border`s.** The UI fact reads `IsEnabled` from each one, found by its
  AutomationId.

## Approved operator recovery — completed

1. Recorded manual 014 and its project-prompt pointer, including the explicit
   normal profile-1 resume requirement.
2. Applied the single fixture-line deletion, preserving the five worker files
   and every scene-save assertion/test identity. After the operator approved
   the measured smoke failure's repair, updated the launcher fact to assert
   disabled-Save behavior, preserving its other assertions and file/lock checks.
3. Passed all 19 frozen gates, with 1197 constructor cases,
   227 smoke cases and 47 selected UI cases, all exact
   censuses and zero failures/skips. Release build: zero owned-code warnings; two existing SYSLIB0051 warnings in vendored MathNet. Elevation:
   all 12 projects from the arc origin. Validated all 494 steps and refreshed
   the receipt without changing any of the 988 compiled artifacts.
4. Recorded measured results and preserved inherited checkpoint-183 floors.
   The arc remains stopped, awaiting the operator's commit.

Wait for the operator's commit and explicit relaunch request. Manual 014 overrides
the generic committed-green acceptance rule for this recovery: resume normally
with an operator reply and `--executability-profile 1`, without
`--accept-current-slice`, and reattach the monitor. The supervisor must review
the repaired slice, rerun its frozen gates and publish measured checkpoint 184
and case evidence before 0186. Do not write that checkpoint by hand, lower the
inherited floors or reset budgets.

Evidence: `.artifacts/0185-operator-repair/verification-summary.json`,
`gate-results.json`, `test-inventory.json`, `elevation-coverage.json`,
`contract-validation.json`, the archived TRX reports and per-gate logs.
`final-scope-check.json` records the final protected-file and Git checks.
