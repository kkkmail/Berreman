# Step 0185 — state of the world

# Where we are

Step 0185 implements S5.3 read-only kit behavior. The omitted scene-save fixture
migration and obsolete launcher click expectation have been authorized by
manual 014 and repaired. **Commit-ready:** all
19 frozen gates passed. The arc remains stopped and escalated at index 184;
normal supervisor acceptance is still required after the operator's commit.

# What's working

- `TableAndElementRotationModel.kitWrites` projects kit access once to
  `ControlOffered` or `ControlWithheld` for the update guard and view.
- `ExpCommit`, `ExpRemove`, `AttachDataFileTo` and `SceneSave` refuse read-only
  access with `The kit is open read-only: nothing is saved.`, before proxy calls.
  Read-write bodies retain their existing behavior.
- Add experiment, Remove, Attach data file and Save scene remain visible and
  are disabled/dimmed when writes are withheld.
- Two new constructor facts cover access mapping and refusal with unchanged
  kit bytes; the new UI fact covers the disabled/enabled controls.
- The inverse and scene-save proxy-refusal fixtures retain read-write model
  access. The one-line scene fixture amendment preserves every assertion,
  including storage error text and unchanged model/disk contents.
- The launcher read-only acceptance fact checks visible-but-disabled Save and
  no status after a click, retaining all kit-file/lock and launcher checks.

# Tests

All 19 frozen gates passed in the operator recovery: build, elevation, the three
test gates below, and all fourteen s21 invariants. Build: zero owned-code warnings; two existing SYSLIB0051 warnings in vendored MathNet; zero errors.
Elevation: all 12 product projects analyzed from immutable origin
`f395597c071910aaa215518626563f6c784bfb62`, zero findings with the existing
ELEV004 exclusion.

| Test gate | Cases | Method identities | Failed | Skipped | Accepted census |
|---|---:|---:|---:|---:|---|
| constructor-unit-tests | 1197 | 1172 | 0 | 0 | exact match |
| ui-smoke | 227 | 227 | 0 | 0 | exact match |
| ui-tests | 47 | 47 | 0 | 0 | exact match |

UI selection: `ExperimentControlsTests`, `InverseConstructorTests`,
`KitWindowTests` and `SceneSaveLoadTests`. All three declared additions are
present; none is removed or renamed. This fixture repair adds no census effect.

The YAML below deliberately retains checkpoint 183's inherited floors. The
local measurement above is not a supervisor checkpoint; checkpoint 184 remains
absent until normal verification and acceptance.

```yaml
gates:
  constructor_unit_tests: 1195
  ui_smoke_tests: 227
```

All 494 steps and the refreshed profile-1 receipt validate. All 988 compiled
slice/gate artifacts, published contracts, supervisor records and budgets are
unchanged. ASG and STL need no source change for this recovery.

Evidence: `.artifacts/0185-operator-repair/verification-summary.json`,
`gate-results.json`, `test-inventory.json`, `elevation-coverage.json`,
`contract-validation.json`, the archived TRX reports and per-gate logs.
`final-scope-check.json` records the final protected-file and Git checks.

# Next action

Wait for the operator's commit and explicit relaunch request. Manual 014 overrides
the generic committed-green acceptance rule for this recovery: resume normally
with an operator reply and `--executability-profile 1`, without
`--accept-current-slice`, and reattach the monitor. The supervisor must review
the repaired slice, rerun its frozen gates and publish measured checkpoint 184
and case evidence before 0186. Do not write that checkpoint by hand, lower the
inherited floors or reset budgets.

# Gotchas

- Read-only model access refuses before the storage proxy; a test of storage
  refusal keeps read-write access and replaces only the proxy with `readOnlyView`.
- Manual 014 preserves this distinction through 0187, 0188 and 0260, when the
  scene test class becomes `SetupSaveLoadTests`.
- Valid live `.specrc` changes and all `.manual/` descendants remain supported;
  the primary postmortem's contrary configuration advice is obsolete.

# Changelog

- 2026-11-08, attempt 01: added the four-message guard, withheld controls and
  three declared tests; escalated because the scene fixture was outside effects.
- 2026-11-08, approved operator recovery: recorded manual 014, removed the single
  conflicting fixture line, then measured a launcher smoke failure. With the
  operator's additional approval, adapted that existing fact, passed all 19
  gates and prepared the commit handoff.
