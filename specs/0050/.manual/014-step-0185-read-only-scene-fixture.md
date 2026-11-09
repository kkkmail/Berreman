# Step 0185: preserve the scene-save storage-refusal fixture

The operator approved the four recovery steps in
`.slices/0185-01.postmortem-review-md` on 2026-11-08. This ruling supplies the
missing fixture-migration authority for S53-READ-ONLY. The published spec,
ledger, acceptance oracle and compiled gate contracts remain unchanged.

## Authorized repair

At step 0185, extend edit authority to
`Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SceneSaveLoadTests.fs`,
only in the existing fact
`OpticalConstructor.Ui.Tests.SceneSaveLoadTests.a saved scene is a setup file in the kit and loads back onto the table`.
Delete only `access = ReadOnlyKitAccess` from its `protectedModel` fixture.
Retain `proxy = ordered.kit.proxy.readOnlyView` and every assertion.

The fixture inherits read-write access from `openSceneKit`. Its save therefore
reaches the refusing storage proxy and preserves the existing failure-prefix,
unchanged-model and unchanged-disk assertions. Actual read-only model access
is covered separately by the two declared constructor facts and the new UI
fact: all four write messages must refuse before proxy calls, and their controls
must remain visible but disabled. Preserve the worker's five-file implementation,
the authorized inverse-test adaptation and all three declared test additions.

No test is added, renamed or removed by this fixture repair, so it requires no
operator test-census row. Do not weaken assertions, production guards, gates,
counts or budgets. At steps 0187, 0188 and 0260 preserve the distinction between
model access refusal and storage refusal, including when 0260 renames the scene
test class to `SetupSaveLoadTests`. This is specific authority for the named fact,
not a general permission to change fixtures outside a step's effects.

## Verification and handoff

Run all 19 unchanged gates: Release build, actual elevation from the immutable
arc origin, complete constructor and smoke suites, the four selected UI classes,
and fourteen s21 invariants. Require zero failures/skips, exact accepted test
identities, the two constructor additions (1,195 to 1,197 cases), unchanged
227 smoke cases, and the one selected UI addition. No owned-code warning is
allowed. Validate all 494 steps and refresh the profile-1 receipt for the prompt
amendment without changing any of the 988 compiled slice/gate artifacts.

Record measured results in this ruling and the slice's plan, log and state of
the world. Keep the inherited YAML floors at 1,195 and 227 until supervisor
measurement. Evidence belongs in `.artifacts/0185-operator-repair/`.

Wait for the operator's commit and explicit relaunch request. This recovery
overrides the prompt's general committed-green acceptance rule: resume normally
with the operator reply and `--executability-profile 1`, without
`--accept-current-slice`. Reattach the monitor. Normal supervisor review and all
19 gates must publish measured checkpoint 184 and case evidence before step
0186. Do not manufacture that checkpoint or reset consumed budgets. No ASG or
STL source change is part of this recovery.

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
