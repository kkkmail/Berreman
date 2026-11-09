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

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Controls/ExperimentControls.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/InverseConstructorTests.fs`

## Risks

- **The inverse fact's first refusal reason changes.** Under `ReadWriteKitAccess`, `snapshotReferences` no longer
  short-circuits. The inverse scene's elements are all unbound, so the experiment has no library references, and
  the refusal comes from the proxy's `writeKindFile`. The expected `refusal` value is a fixture line, and it must
  name that reason.
- **No local compile.** Under Invariant 6 nothing compiles locally, so every API used must be checked against its
  declaration.
- **The headless controls are disabled `Border`s.** The UI fact reads `IsEnabled` from each one, found by its
  AutomationId.

## Operator recovery — not commit-ready

The approved scene-save repair is complete under manual 014: only the
`access = ReadOnlyKitAccess` fixture line was removed. Every assertion and
identity remains. The five worker product/test files are unchanged from operator
entry. The project prompt points to the amendment; all 494 steps validate and
the receipt is refreshed with all 988 compiled artifacts byte-identical.

Measured gates: build passed with zero warnings/errors; elevation passed over
all 12 product projects from the immutable arc origin; constructor-unit-tests
passed all 1,197 cases, exact census, zero failures/skips. Smoke ran all 227
accepted cases: 226 passed, one failed, none skipped. The engine short-circuited,
so the selected UI gate and fourteen invariants did not run.

The additional failure is
`LauncherTests.acceptance: a kit open elsewhere offers Open read-only and Cancel in one row, and Open read-only opens it without writing`,
at `LauncherTests.fs:959`: `Missing control KitStatus`. The test clicks Save
scene in a genuinely read-only window and expects the old storage-error prefix.
Step 0185 now disables that control, so no write message or status is produced.
This is a second omitted test adaptation; production must keep Save disabled.

The proposed narrow repair asserts Save remains visible but disabled, clicking
it produces no status, and all kit bytes/locks remain unchanged. It preserves
the test identity and all other assertions. The old storage-error assertions
remain in SceneSaveLoadTests; direct model refusal is covered by the new
KitWindowModelTests fact. Replacing the launcher's two old status assertions
exceeds the approved assertion-preservation constraint, so operator agreement
was requested. `LauncherTests.fs` remains unchanged. The concrete patch is
`.artifacts/0185-operator-repair/launcher-read-only-proposal.patch`.

Evidence: `.artifacts/0185-operator-repair/verification-summary.json`,
`gate-results.json`, per-gate logs and archived constructor/smoke TRX reports.
The original worker handoff is archived alongside them. No ASG/STL change,
commit, restart, checkpoint creation or budget/floor edit was performed.

After agreement, extend manual 014 to the named launcher fact, apply the narrow
repair and complete verification. Keep waiting for the operator's commit. A
later explicitly requested relaunch still requires normal
`--executability-profile 1`, without `--accept-current-slice`, and measured
checkpoint 184 before 0186. The arc remains stopped.
