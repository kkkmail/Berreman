# Implementation log — slice 0160

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

## Normal worker resume — 2026-11-08

This round follows the task's operator note and manual 012's explicit recovery
exception. Entry HEAD is `4160378` (`0050-0160-manual`), which contains the
completed repair. The earlier waiting-for-commit statements below describe the
operator repair session; they are no longer the current handoff state.

### Progress

- Read the shared worker protocol, IMPLEMENT delta, project prompt, manual 012,
  slice and unchanged fifteen-gate snapshot, plus the existing handoff files.
- Inspected the committed repair and the recorded operator evidence. The live
  schema keeps category as a string without a fixed allowlist; Report.fs uses
  lazy `tryLoadSchema` and shared `validateWith`; the declared refusal fact uses
  numeric category `12` and requires nonempty diagnostics. The retained category
  round-trip and membership-refusal assertions remain present.
- Before handoff edits: preserve the complete implementation at `4160378` and
  replace the stale blocked implementation plan and pre-commit state-of-world.
  Limit this round's writes to the three supplied worker handoff paths.
- Completed the handoff refresh: the plan reflects manual 012, the log retains
  the historical repair evidence, and the state-of-world names the committed
  repair and required supervisor measurement. All required headings and the
  inherited 1142 YAML floor are retained. No source or test changes were needed.

### Files modified

This round updates only `0160-impl-plan.md`, `0160-impl-log.md` and
`0160-state-of-the-world.md` beside the slice. Production, tests, schemas and
project entries need no further edits under the operator's preservation request.

### Testing state

No build, test, analyzer, invariant, retirement-search or plan-validation command
is run by this resumed IMPLEMENT worker, per Invariant 6. Prior measured results
remain attributed to the operator's repair evidence below; they are not new
worker gate results or supervisor acceptance. This round reports `gates_run: []`
and `gate_results: {}`.

The implementation and handoff are ready for normal supervisor review. The
supervisor must run all fifteen unchanged gates, measure the exact declared
test effects, and publish checkpoint 159 and case evidence before advancing.
Retain `--executability-profile 1`; do not use `--accept-current-slice`.
The state-of-world YAML keeps checkpoint 158's inherited floor of 1142.

### Artifacts

Read existing evidence in `.artifacts/0160-operator-repair/`, including
`verification-summary.json`, `gate-results.json`, `source-review.json`,
`contract-validation.json`, `build-warnings.json` and `final-scope-check.json`.
No new runtime evidence is generated by this handoff-only round.

### Gotchas

- `.executability-json`, `.manifest.state.json` and `.supervisor-lock.diag` were
  already modified on entry. They belong to the supervisor and remain untouched
  by this worker, as do checkpoints, counters, receipts and compiled artifacts.
- The repair is committed, so a working-tree-only elevation selection may hide
  the product changes. Manual 012 requires coverage against accepted 0159
  (`8d82da6`) of all affected product projects and dependents; the operator's
  prior evidence covers seven projects. Do not alter the gate roster or snapshot.
- Seven single-case deletions and one addition change 1142 to 1136; the migrated
  fresh-table fact changes identity without changing the count. Step 0161 has
  no deletion credit, so the supervisor's measured checkpoint is required.
- Manual 012 resolves the historical schema conflict and authorizes the two
  additional documentation cleanups. There is no unresolved authority request
  or outstanding product work in this resume.

## Approved operator recovery — 2026-11-08

The operator approved the secondary review's four-step plan, including the
0160-only normal-resume exception. Manual 012 records that authority. Entry HEAD
`8f2deca` contains the stopped reports; product code is accepted 0159 (`8d82da6`).
The stopped worker history below remains historical.

Before implementation: captured all tracked input hashes, Git HEAD/index and
the 988 compiled artifacts under `.artifacts/0160-operator-repair/`. Confirmed
checkpoint 159 is absent and the arc is stopped. The repair will move the four
effective schema definitions, reuse shared validation, complete the declared
retirement and exact test effects, then measure all fifteen gates. No passing
post-repair result is claimed yet. Wait for the operator's commit after verification.

## Measured operator result

Implemented manual 012 and the complete 0160 retirement. The four schema
definitions moved into material-metadata.schema.json; their only semantic change
is removal of the obsolete JSON category allowlist. Category remains a string,
all other constraints remain, and the F# category types are byte-identical.
Report.fs now uses the existing lazy FileName.tryLoadSchema and validateWith
seams. Its DTO, conversion and import/export membership behavior are unchanged.

Deleted the old codec, project schema, Domain aggregate and obsolete round-trip
test file, with their project entries. Migrated the fresh-table fact byte-for-byte
to TableViewTests, retired exactly seven declared facts, and added the declared
numeric-category refusal with nonempty diagnostics. All six existing export/import
fact bodies and all nine retained placement fact bodies are unchanged. The required
retirement search is empty. The two additional storage files have documentation-only
changes; already-retired history/project-file code remains retired.

All fifteen unchanged gates passed: build, elevation, the whole constructor suite
and twelve s18 invariants. The measured constructor report contains **1,136 passed
cases / 1111 method identities**, zero failures/skips, and an exact census
match. It includes all seven ExportImportTests facts and the migrated table fact.
The evaluator consumed checkpoint 158's 1,142 floor and the seven verified deletion
credits; no floor was edited. The new fact restores one case, giving the declared
net reduction of six.

The Release build has zero errors and no owned-code warnings. It reports the two
known SYSLIB0051 warnings in vendored MathNet Numerics/Exceptions.cs (lines 24 and
77); these are recorded in build-warnings.json, not suppressed. Elevation passes
all seven affected product projects and dependents with zero new findings under
the existing ELEV004 exclusion. Full-plan validation passes all 494 steps; the
refreshed profile-1 receipt validates and all 988 compiled files are byte-identical.

Published requirements, test effects, gate snapshots, supervisor state/counters,
expected-red records, checkpoints/case evidence, contract registries, versions
and the version-update lock remain unchanged. Git HEAD/index are unchanged;
no ASG/STL source was edited. The complete verified repair awaits the operator's
commit. The arc remains stopped and checkpoint 159 remains absent.

After the operator commits and explicitly requests relaunch, manual 012 requires
normal resume with `--operator-reply <absolute-recovery-note> --executability-profile 1`,
without `--accept-current-slice`. Normal supervisor review and all fifteen gates
must record checkpoint 159 and its measured case evidence before advancing.
Local verification does not publish supervisor acceptance. Do not reset budgets
or fabricate a checkpoint; step 0161 has no deletion credit for this retirement.

Evidence: [.artifacts/0160-operator-repair/](../.artifacts/0160-operator-repair/),
including verification-summary.json, gate-results.json, source-review.json,
contract-validation.json, test-inventory.json, 0160-constructor-unit-tests.trx,
the per-gate stdout/stderr logs, and final-scope-check.json. The future resume
note is operator-reply.md in that folder.

## Worker progress (historical)

- Read the task, shared worker protocol, IMPLEMENT delta, project prompt,
  AGENTS.md, naming conventions, slice and resolved gate snapshot.
- Inspected the retirement census, material loader and validator, original
  schema, affected tests, prior slice handoff and immutable acceptance oracle.
- Before source implementation: traced the material schema extraction against
  the existing ExportImportTests assertions. Found a direct conflict between
  exact schema preservation and the retained renamed-category regression.
- Consulted step 0046's implementation log, state-of-the-world, accepted code
  review and recorded failing test output. These establish that the current
  enum removal intentionally repairs `S17-CATEGORY-LIST`; it is not an accidental
  discrepancy or an unmeasured concern about a future gate.
- Wrote the implementation plan and this log. Stopped before product/test edits
  because satisfying either conflicting requirement literally violates the
  other. No partial retirement or intentionally failing implementation was left.
- Wrote the state-of-the-world at the exact supplied path, with all required
  sections and the inherited checkpoint count. The handoff records the authority
  conflict; implementation and supervisor verification remain incomplete.

## Worker files modified (historical; superseded by manual 012)

- Created `specs/0050/.slices/0160-impl-plan.md`.
- Created `specs/0050/.slices/0160-impl-log.md`.
- Created `specs/0050/.slices/0160-state-of-the-world.md`.
- No production, test, schema or project file was changed.

## Worker testing state (historical; superseded by manual 012)

Blocked on a requirement decision, not on a newly measured gate failure.
No checks or gates ran under the IMPLEMENT worker's Invariant 6. No gate outcome
is asserted. `commit_ready` is false because the slice is not implemented.
The supervisor's roster remains build, elevation, all-discovered
constructor-unit-tests and the twelve s18 invariants.

Checkpoint 158 records 1142 constructor cases. The requested completed slice
migrates one fact, deletes seven and creates one, for a net reduction of six
(1136 expected cases). This round changes no tests and claims no measured count.
No census, baseline, oracle, checkpoint or gate was edited.

## Worker blocking requirement and missing authority (historical; superseded by manual 012)

Owner: spec 0050 operator/spec owner. Step: 0160.
Obligations: `S44-PROJECT-CODEC` and accepted `S17-CATEGORY-LIST` from step 0046.

1. `0160.slice-md:23` requires the four definitions to move **exactly as the
   project schema holds them**, a lazy `tryLoadSchema`, and
   `validateEntryElement = validateWith materialEntrySchema element`.
   `schema/optical-constructor-project.schema.json:84-87` defines category as
   one of Glass, Metal, Semiconductor, Crystal or Vacuum. The new fact must
   reject the otherwise valid category `NotACategory`.
2. `0160.slice-md:25` requires every other ExportImportTests fact to pass.
   The existing accepted fact
   `OpticalConstructor.Tests.ExportImportTests.0035 category round-trips through export then import back to the matching CategoryId`
   (`ExportImportTests.fs:112`, renamed-category assertions at lines 146-179)
   supplies `Glass from the caller`, requires successful export, checks that
   exact persisted name, calls `validateEntryElement` successfully, and requires
   import to preserve the id, CategoryId and description.
3. `Report.fs:59` removes only the inherited category enum in memory. Export
   checks membership against the caller's categories before calling
   `validateEntryElement` (`Report.fs:142-158`). Replacing this loader with the
   required unchanged file restores precisely the rejection that step 0046
   repaired; both the export and direct validation cannot accept the retained
   fixture under the specified enum.

Required decision: authorize preserving caller-owned categories by moving the
effective structural definition without its enum and changing the new invalid
example to an actual structural violation (for example a non-string category),
or explicitly supersede the caller-category behavior and its accepted regression.
The former is recommended because it preserves the accepted library contract.
The owner must supply the ruling; the worker may not edit the published slice
or oracle, silently change the requested negative case, change the renamed
fixture to a built-in category, or remove existing assertions.

The whole-tree absence requirement additionally includes two stale docs outside
the explicit edit list: `OpticalConstructor.Storage/CanonicalJson.fs:128`
(`OpticalConstructorProject`) and `EntryFileJson.fs:87`
(`ProjectJson.serializeProject`). Authorize documentation-only cleanup in those
two files to complete the retirement census. No implementation change is needed
there.

## Worker artifacts (historical; superseded by manual 012)

No new runtime artifacts were produced. Historical evidence read, not rerun:

- `.artifacts/0046-retry-red.log:2-4`: renamed-category export failed with
  `SchemaValidationErr` before enum removal.
- `.slices/0046-impl-log.md`, Attempt 02: documents the reproduced failure and
  repair, retaining structural constraints and caller-list membership checks.
- `.slices/0046-state-of-the-world.md`: documents folding the regression into
  the accepted 0035 fact while retaining all assertions and test identities.
- `.artifacts/code_judge/0046-01-code-judge.md`, cycle 4: review-green explicitly
  accepts the schema repair and renamed-category regression as satisfying
  `S17-CATEGORY-LIST`.

## Worker gotchas (historical; superseded by manual 012)

- This is a concrete contradictory requirement, not missing permission for
  ordinary implementation or a request to expand the gate roster.
- Adding `Glass from the caller` to the enum would both violate exact schema
  preservation and encode a test fixture into the production schema. Special
  casing `NotACategory` would likewise be incorrect.
- No new test identity is missing authority: the new fact is already declared.
  The unresolved authority concerns its prescribed example, the category schema
  semantics and preservation of existing accepted assertions.
- `.manifest.state.json` was modified on entry and belongs to the supervisor;
  it was not edited. No git mutation, version bump, cross-repository edit,
  notebook change or analyzer configuration change was made.
