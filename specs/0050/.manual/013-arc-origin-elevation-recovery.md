# Step 0160: elevation coverage from the arc origin

The operator approved the recovery for postmortem 0160-02 on 2026-11-08 and
clarified that ASG must preserve the beginning-of-arc commit. User commits on
escalation are supported; they must not remove code from analyzer coverage.
The operator authorized ASG + BRM changes and prohibited all STL changes.
ASG records the repair in spec 0090 manual 032 and ASG-ERRORS.md.

## Authority and scope

Preserve manual 012's material-schema repair, all product/test assertions and
declared test effects. Extend 0160 authority to `Elevation/Run-ElevationGate.ps1`,
the focused `Elevation/Test-ElevationScope.ps1` harness, elevation usage guidance
in AGENTS.md and the project prompt, and the new `.arc-base-json` record.

ASG captures and preserves the immutable arc origin and supplies it as
`ASG_ARC_BASE_COMMIT` to gate processes. BRM's frozen gate command consumes that
context through the existing driver; no descriptor rewrite is required. The
driver analyzes committed changes since that origin plus staged, unstaged and
untracked changes, and expands to dependent projects. Retain explicit `-Base`,
`-All`, `-Project`, exclusions and analyzer failure handling. Reject conflicting
explicit/ASG bases. With no comparison context, standalone calls analyze every
in-scope project. Scope-only inspection is never an analysis verdict.

This supersedes the secondary review's all-project workaround as the arc's
normal mechanism. The fallback remains useful for standalone calls. No STL
change is needed: its existing driver already accepts a supplied `-Base`.

## Verified starting commit

Initialize `.arc-base-json` at
`f395597c071910aaa215518626563f6c784bfb62`, the last commit before the arc began.
It was committed at 2026-11-05 11:24:16 UTC; the first boot is logged at 11:30.
First accepted slice 0001, `ad55d280630c443691fea244be6fae90775ed2ec`, has exactly
that parent. The first-parent history has no intervening commit. This is an
explicit historical bootstrap, not a fallback to current HEAD, the handoff's
earlier source commit, or the last accepted slice.

The new record is the sole authorized new supervisor metadata in this repair.
Preserve every existing supervisor state/counter, checkpoint/case-evidence and
expected-red file. Preserve the committed product, published requirements,
oracle, effects, registries, versions and lock. The entry receipt was already
dirty after the previous supervisor boot; refresh it for this prompt change.

## Verification and handoff

Run ASG origin, gate and lifecycle regressions, plus scope tests covering
committed-only, report-only, mixed, untracked, invalid-base and dependent cases.
Keep the PowerShell harness outside MTP discovery. Store evidence only under
`.artifacts/0160-02-operator-repair/`.

Run all fifteen frozen 0160 gates using the real ASG evaluator and arc-origin
context. Require actual analysis of the selected product projects and readable
reports, zero new findings under the existing ELEV004 exclusion, exactly 1,136
constructor passes, the exact census, zero failures/skips and no owned-code
warnings. Measure the resulting scope against all twelve current product
projects and the current gate budget. Do not change the budget to mask a failure.

Validate all 494 steps and refresh the receipt, preserving all 988 compiled
files byte-for-byte. Update this log and the slice handoff with measured results,
keeping the inherited YAML floor at 1,142. Wait for the operator to commit ASG
and BRM; do not commit or relaunch as part of this repair.

## Eventual resume

After both commits and an explicit relaunch request, verify the committed
tree still selects the arc's changes from the recorded origin. Resume normally
with `--operator-reply <recovery-note> --executability-profile 1`, preserving
manual 012's exception. Do not use `--accept-current-slice`: normal review and
verification must publish checkpoint 159 and measured case evidence before
0161. The seven deletions and one addition yield `1142 - 7 + 1 = 1136`.

The origin applies throughout the remaining arc, including after additional
operator commits. No speculative narrowing to the latest accepted slice is
permitted. No STL write, build, package bump or process change is authorized.

## Measured implementation

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
