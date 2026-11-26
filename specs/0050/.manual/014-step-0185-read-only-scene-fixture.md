# Step 0185: preserve read-only UI and storage-refusal coverage

The operator approved the four recovery steps in
`.slices/0185-01.postmortem-review-md` on 2026-11-08. This ruling supplies the
missing fixture-migration authority for S53-READ-ONLY. The published spec,
ledger, acceptance oracle and compiled gate contracts remain unchanged.

After verification exposed the launcher expectation below, the operator also
approved additional test repairs needed to complete this recovery. The earlier
repair is committed as `6607a493b05cfe41b86947ad44905a35d383d228`.

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
not a general permission to change unrelated fixtures outside a step's effects.

The additional approval extends edit authority to
`Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LauncherTests.fs`, in
`OpticalConstructor.Ui.Tests.LauncherTests.acceptance: a kit open elsewhere offers Open read-only and Cancel in one row, and Open read-only opens it without writing`.
Replace its two obsolete storage-error text assertions after a disabled Save
click with assertions that Save remains visible but disabled and that clicking
it leaves `KitStatus` absent. Preserve the unchanged-file/lock checks, test
identity, launcher behavior and all other assertions. The scene-save fact keeps
its storage-error assertions; the new model fact verifies direct write-message
refusal. These three tests cover distinct paths without weakening the guard.

For any other failure caused by the same read-only behavior change, the
operator authorized the necessary test adaptation during this recovery. Record
each concrete change and its preserved obligation here. Do not use that
authority to waive failures, retire tests, lower counts or alter frozen gates.

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

## First verification and additional approval

The first verification attempt passed build, elevation across all 12 projects
and all 1,197 constructor cases. Smoke passed 226 of 227 cases and failed only
the launcher expectation described above; the gate engine stopped there. That
failure and the original entry snapshot are preserved under
`.artifacts/0185-operator-repair/initial-verification/`.

The operator approved the additional test repair. The launcher now checks
visible-but-disabled Save and unchanged status after a click, retaining all
file/lock checks. The revised full frozen roster passed as recorded below.
The arc remains stopped; no checkpoint or budget was changed.

## Measured implementation

The scene fixture deletion, launcher expectation repair and project-prompt
pointer are complete. Every scene-save assertion remains; only the two approved
obsolete launcher status assertions were replaced. All other launcher assertions
and the five worker product/test files are preserved. Both storage-refusal
fixtures, the new read-only model/control tests and launcher acceptance passed.

All **19 frozen gates passed**. Release build: zero owned-code warnings; two existing SYSLIB0051 warnings in vendored MathNet; zero errors. Elevation:
all **12 of 12** product projects from immutable origin
`f395597c071910aaa215518626563f6c784bfb62`, zero findings with the existing
ELEV004 exclusion. All fourteen s21 invariants passed.

| Test gate | Cases | Method identities | Failed | Skipped | Accepted census |
|---|---:|---:|---:|---:|---|
| constructor-unit-tests | 1197 | 1172 | 0 | 0 | exact match |
| ui-smoke | 227 | 227 | 0 | 0 | exact match |
| ui-tests | 47 | 47 | 0 | 0 | exact match |

Constructor cases increased by exactly two; smoke stayed at 227. The four
selected UI classes include exactly the one declared new method. No test was
retired, renamed or skipped. All 494 steps validate and the refreshed profile-1
receipt validates; all 988 compiled artifacts are byte-identical.

Published contracts, census and supervisor/checkpoint records, budgets and
version locks were preserved. Inherited YAML floors remain 1,195 and 227;
checkpoint 184 remains absent. No ASG/STL source change, Git mutation or arc
restart was performed. The slice handoff is ready for the operator's commit.

Evidence: `.artifacts/0185-operator-repair/verification-summary.json`,
`gate-results.json`, `test-inventory.json`, `elevation-coverage.json`,
`contract-validation.json`, the archived TRX reports and per-gate logs.
`final-scope-check.json` records the final protected-file and Git checks.

Wait for the operator's commit and explicit relaunch request. Manual 014 overrides
the generic committed-green acceptance rule for this recovery: resume normally
with an operator reply and `--executability-profile 1`, without
`--accept-current-slice`, and reattach the monitor. The supervisor must review
the repaired slice, rerun its frozen gates and publish measured checkpoint 184
and case evidence before 0186. Do not write that checkpoint by hand, lower the
inherited floors or reset budgets.
