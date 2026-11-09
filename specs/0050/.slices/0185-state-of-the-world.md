# Step 0185 — state of the world

# Where we are

Step 0185 implements read-only kit guards and disabled write controls. The
approved scene-save fixture repair is complete, but verification found another
obsolete UI expectation. **Not commit-ready.** The arc remains stopped and
escalated at index 184. Manual 014 supplies the scene-fixture authority; the
proposed additional launcher-test amendment awaits operator agreement.

# What's working

- `kitWrites` projects access once for the model guard and view.
- `ExpCommit`, `ExpRemove`, `AttachDataFileTo` and `SceneSave` refuse read-only
  access before proxy calls with `The kit is open read-only: nothing is saved.`
- Add, Remove, Attach data file and Save scene remain visible but disabled.
- Two new constructor facts cover access mapping and refusal with unchanged
  bytes; one new UI fact covers withheld/enabled controls.
- The inverse and scene-save storage-refusal fixtures keep read-write model
  access and a refusing proxy. The scene repair deletes only one line and keeps
  every assertion. The five worker product/test files remain unchanged.

# Measured tests

| Gate | Result |
|---|---|
| build | PASS, zero warnings/errors |
| elevation | PASS, all 12 product projects from immutable arc origin |
| constructor-unit-tests | PASS, 1,197 cases, exact census, zero failures/skips |
| ui-smoke | FAIL, 226 passed / 1 failed / 0 skipped, exact 227-case census |
| ui-tests and fourteen invariants | Not run: first failure short-circuited |

The failing launcher acceptance fact clicks Save in a read-only window, then
looks for the old storage-error status. Save is now disabled, so the click
correctly produces no message and no `KitStatus` control. The test must check
the new disabled-control behavior instead. The proposed patch preserves its
identity, all other launcher assertions, unchanged-file/lock checks, and the
separate scene-save storage-refusal assertions. It has NOT been applied because
it replaces two assertions the approved plan explicitly preserved.

The inherited checkpoint-183 floors below stay unchanged. Local verification
has not accepted the slice; checkpoint 184 remains absent.

```yaml
gates:
  constructor_unit_tests: 1195
  ui_smoke_tests: 227
```

All 494 steps validate, the refreshed profile-1 receipt validates, and all 988
compiled artifacts remain byte-identical. Published/supervisor records and
budgets remain unchanged. No ASG/STL change, commit or restart was performed.

# Next action

Await agreement on `.artifacts/0185-operator-repair/launcher-read-only-proposal.patch`.
Then extend manual 014 to the named launcher fact, apply the repair and finish
verification before waiting for the operator's commit. Only after commit and an
explicit relaunch request: normal `--executability-profile 1`, without
`--accept-current-slice`, reattach the monitor, and require measured checkpoint
184 before 0186. Do not manufacture checkpoints, lower floors or reset budgets.

Evidence: `.artifacts/0185-operator-repair/verification-summary.json`,
`gate-results.json`, per-gate logs, constructor/smoke TRX reports and the proposed
patch. The impl-log and manual 014 record the completed repair and new blocker.

# Gotchas

- Model access refusal and storage refusal are separate paths. A proxy-refusal
  fixture retains read-write access; actual read-only access stops earlier.
- Disabled controls do not dispatch click messages. Direct model-message
  refusal remains covered by KitWindowModelTests.
- Valid live `.specrc` changes and all `.manual/` descendants remain supported;
  the primary postmortem's contrary advice is obsolete.

# Changelog

- 2026-11-08: worker implemented the read-only behavior and declared tests;
  escalated over missing scene-fixture authority.
- 2026-11-08: operator approved the scene repair, now applied. Build, elevation
  and constructor tests passed; smoke exposed the launcher expectation above.
