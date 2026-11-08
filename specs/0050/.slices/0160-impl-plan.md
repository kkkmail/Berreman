# Implementation plan — slice 0160

## Approved arc-origin recovery — manual 013

ASG now persists the immutable arc origin in `.arc-base-json`, validates its
history, exports it as `ASG_ARC_BASE_COMMIT` and supports `{arc_base}` in gate
commands. Existing arcs without a record require an explicit verified origin;
restarts never silently substitute HEAD. BRM's unchanged frozen command uses
that context through the driver, including committed and uncommitted changes
and dependent projects. Scope-only output never claims an analyzer verdict.

BRM's verified origin is `f395597c071910aaa215518626563f6c784bfb62`. The existing
reflog and first accepted commit's parent confirm the bootstrap. Existing
supervisor state, counters, checkpoint/case-evidence and expected-red records
were not rewritten. The committed material-schema repair and every product
test remain unchanged.

All **15 frozen gates passed**. Elevation actually analyzed **12 of 12 product
projects**, with zero new findings, in **193.7 seconds** under the existing
gate budget and ELEV004 exclusion. Constructor tests passed **1,136 cases / 1,111
method identities**, exact census, zero failures/skips. The build has no owned-code
warnings; two existing SYSLIB0051 warnings originate in vendored MathNet. All 494
steps validate, the profile-1 receipt validates, and all 988 compiled artifacts
are byte-identical.

All **18 PowerShell scope regressions** pass. The focused ASG origin/deadline
run passed 29 tests; the runtime/origin run passed 30. The subsequent broad run
includes the added boot-refusal test and has **1,207 passed, 12 skipped and 14
failures**. All 14 failures were independently reproduced with committed ASG
source `94aa431`: twelve assert obsolete three-digit labels; two path-validation
fixtures encounter ignored artifact paths. They are not introduced by this
repair, and their assertions were not weakened. The new origin module also
passes strict mypy. CLI parsing accepts the origin bootstrap and profile 1
together without invoking the supervisor.

Evidence is in `.artifacts/0160-02-operator-repair/`: verification-summary.json,
gate-results.json, elevation-coverage.json, the archived TRX and per-gate logs,
scope-tests-2/results.json, both broad ASG logs and asg-baseline-comparison.json.
Final scope and Git checks are recorded in final-scope-check.json.

Awaiting the operator's commits in ASG and BRM. The arc remains stopped;
checkpoint 159 is still absent. No Git mutation, STL change or process restart
was performed. ASG-ERRORS.md and ASG spec 0090 manual 032 record the fixed defect.

After both commits and an explicit relaunch request, verify the committed
arc-origin scope, then resume with the operator reply and
`--executability-profile 1`. Preserve manual 012's normal-resume exception:
do not use `--accept-current-slice`. Normal supervisor review and all fifteen
gates must publish measured checkpoint 159 and case evidence before 0161.
The inherited floor stays at 1,142 until supervisor measurement; declared
credits still account for seven deletions and one addition. Never manufacture
the checkpoint, lower the floor or reset consumed budgets.

## Previous attempt history

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
