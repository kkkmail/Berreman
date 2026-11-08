# Implementation plan — slice 0160

Resume the completed repair at commit `4160378` under
[manual 012](../.manual/012-step-0160-material-schema-recovery.md) and the task's
operator note. The ruling resolves the original exact-copy/category conflict:
material metadata validation checks structure, and the caller's category list
owns membership. The schema extraction, codec/aggregate retirement, seven fact
deletions, unchanged fresh-table migration and numeric-category refusal fact
are already implemented and recorded in the operator's verification evidence.

Read the implementation and existing evidence without rerunning checks. Preserve
all production code, schema constraints, test identities and retained assertions.
Refresh only `0160-impl-plan.md`, `0160-impl-log.md` and
`0160-state-of-the-world.md` at the task's supplied paths. Replace the stale
blocked/pre-commit handoff with the current normal-resume state, retaining the
historical repair trail and links to `.artifacts/0160-operator-repair/`.

## Verification and handoff

The IMPLEMENT worker's Invariant 6 reserves checks to the supervisor. Run no
gates and report no new gate results. Attribute the prior 1136 passed cases,
exact census and fifteen passing gates to the operator evidence. Keep the
state-of-world YAML at checkpoint 158's inherited 1142 floor; the supervisor
must publish measured checkpoint 159 and its case evidence before advancing.

Manual 012 explicitly requires normal review and all fifteen unchanged gates
after the operator's commit, with `--executability-profile 1` and without
`--accept-current-slice`. The committed repair can hide product changes from
working-tree elevation selection; preserve coverage of affected projects and
dependents against accepted 0159 (`8d82da6`). Do not alter published requirements,
compiled artifacts, gate snapshots, receipts, checkpoints or supervisor state.
The three supervisor-owned files already dirty on entry stay untouched.
