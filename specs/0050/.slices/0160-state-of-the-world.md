# Where we are

The operator-approved origin/coverage repair is implemented and verified under
[manual 013](../.manual/013-arc-origin-elevation-recovery.md). The product repair
under manual 012 is preserved. Awaiting the operator's commits and relaunch.

# Implementation and tests

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

The inherited floor below is unchanged; it is not a newly authored checkpoint.

```yaml
gates:
  constructor_unit_tests: 1142
```

# Required next action

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
