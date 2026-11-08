# Implementation plan — slice 0160

## Normal resume after manual 013 — 2026-11-08

Resume the committed repair at `37cb549` under the task's operator note,
[manual 012](../.manual/012-step-0160-material-schema-recovery.md) and
[manual 013](../.manual/013-arc-origin-elevation-recovery.md). Preserve the
completed codec/aggregate retirement, effective material-metadata schema,
caller-owned category membership, all retained assertions and declared test
effects. No further product or test edit is indicated by the source inspection.

Read the committed origin record, elevation driver and operator evidence.
Elevation must use `f395597c071910aaa215518626563f6c784bfb62` from
`.arc-base-json`, supplied to the frozen command as `ASG_ARC_BASE_COMMIT`.
Neither accepted slice 0159 nor entry HEAD is the comparison base. Preserve
coverage of committed and uncommitted arc changes and dependent projects.

Refresh only the supplied `0160-impl-plan.md`, `0160-impl-log.md` and
`0160-state-of-the-world.md` paths. Replace the stale pre-commit handoff,
retain the historical repair trail in the log and restore every required
state-of-world heading. Attribute prior measurements to the existing operator
artifacts; leave the implementation, frozen contracts, receipts, compiled
artifacts and supervisor records untouched.

## Verification and handoff

The IMPLEMENT worker's Invariant 6 reserves execution of checks and gates to
the supervisor. Run no build, tests, analyzer, scope harness, invariants or
validation command. Inspect source and recorded evidence only; return
`gates_run: []` and `gate_results: {}` without claiming a fresh verdict.

Keep the inherited YAML floor at checkpoint 158's 1142. The operator evidence
records 1136 cases after seven deletions, one unchanged migration and one
addition. Normal supervisor review and all fifteen frozen gates must publish
checkpoint 159 and measured case evidence before step 0161. Retain
`--executability-profile 1`; do not use `--accept-current-slice`, manufacture
a checkpoint, narrow the origin, lower a floor or reset budgets. No Git
mutation, relaunch or ASG/STL edit belongs to this worker round.
