# Implementation log

## Current recovery status (2026-11-15)

The resumed attempt 01 starts at 0931367d. The operator committed the original
implementation and R0003 identity-provider repair in 926d4cbd, then committed
the regenerated reference in 0931367d. The earlier attempt-02 authority
blocker below is historical and resolved by R0003. No new product change is
planned unless source review identifies an unmet obligation.

### Recovery progress

- Read the active task, both worker protocols, project instructions, AGENTS.md,
  naming conventions, the slice and gate snapshot, manuals 013/015 and all
  three binding rulings. Read the 0428 post-mortem and its review.
- Before source review: preserve both operator commits and all test identities;
  trace the provider through initialization and both add messages, review the
  comparison and reference provenance, then replace the stale blocked handoff.
- Existing dirty files on entry are .manifest.state.json and
  .supervisor-lock.diag. They belong to the active supervisor and are untouched.
- Follow IMPLEMENT Invariant 6: no build, tests, builder invocation or gates
  in this worker round. Normal review, the unchanged 26 gates and measured
  checkpoint 427 belong to the supervising process after this worker exits.
- Source review confirms the one mint at TableAndElementRotationView.fs:634,
  the production default at :945, mkElement's use at :828 and addElement's
  injected use at :1734. Both add messages converge at :3106-3110. The old
  freshId declaration is deleted in the operator's commit.
- KitAcceptanceTests.fs:112 declares three distinct elevated slot identities;
  :711 creates a fresh consumption list inside each build, :719 refuses a
  fourth request and :729 asserts full consumption. The three AddElement
  messages in buildAcceptanceSetups remain unchanged.
- The comparison at KitAcceptanceTests.fs:1076 uses the existing builder,
  recursive inventory and byte reader. It compares the sorted union of all
  paths, excludes only root .oc/, reports the built absolute path and first
  missing/extra/different file, closes the kit in finally and retains failed
  output. No identity normalization or alternate construction path was added.
- Read S121-COMMITTED-KIT in the immutable oracle and manual 001 section Q.
  The committed four-line AGENTS.md bullet and the implementation satisfy the
  source obligations. Read operator manual 034 for the regeneration and
  independent subsequent comparison provenance.
- Commit 0931367d updates exactly the 42 setup and 53 experiment files that
  carry the three identities. The operator's recorded generation copied all
  116 root/library files; unchanged files naturally have no second diff.
  Git history shows no lab-data changes since accepted 0427 and no changes
  to StudyEngine.fs, ItemEvaluation.fs or StudyForward.fs, preserving R0001/R0002.
- Read the existing operator gate-results.json and saved build, elevation and
  kit-acceptance logs. These are prior operator measurements, not new worker
  gate results. They corroborate the recovery note; no contradiction or
  remaining implementation defect was found.
- Before final handoff: replace the obsolete blocked state-of-the-world with
  the committed recovery status, historical evidence, unchanged inherited
  baseline block and supervisor verification requirement. Keep the original
  failed-attempt log below as history.
- Wrote the canonical state-of-the-world with all required sections. Final
  diff review shows only the three intended worker documents alongside the
  two pre-existing supervisor edits; normal and ignore-CR-at-EOL numstats
  agree. The committed repair and reference data remain untouched.

### Recovery testing state

No gate, build, test or builder was executed in this worker recovery. All
implementation obligations are present in the operator's commits; no scope
question remains. The unchanged 26-gate roster is pending the supervisor's
normal review and measurement. commit_ready=false means that this worker
does not certify gate success; it is not the old implementation blocker.

Historical operator evidence: .artifacts/0428-operator-repair/gate-results.json
records build 79.7 s, elevation 218.1 s, ui-smoke 264/264 in 240.0 s, selected
ui-tests 18/18 in 28.4 s, kit-acceptance 4/4 in 46.5 s and all 21 invariants
in 0.2 s each. All records passed, within the active task's deadlines, with
zero test failures/skips. Elevation's saved log uses immutable arc origin
f395597c071910aaa215518626563f6c784bfb62, analyzes 13 projects and reports no
new finding with ELEV004 excluded. The saved build has only NU1902 advisories
and the two SYSLIB0051 warnings in vendored MathNet; no owned-code warning.
These records do not publish checkpoint 427 or replace the supervisor's run.

### Recovery architecture and gotchas

The only production addition is the injected identity provider already
committed under R0003; stored shapes and messages are unchanged. Normal app
additions still mint fresh GUIDs. AT-1's deterministic identities enter before
its existing semantic messages. At 0432, AT-2 must use the same provider for
its added reflected detector, preserving the injection through 0494.

The operator note's normal review/verification instruction addresses the
supervisor lifecycle. IMPLEMENT's act-only protocol still governs this worker;
do not launch a nested supervisor, use --accept-current-slice, edit checkpoints
or replace the immutable arc origin with an operator commit. Preserve profile 1.

### Recovery files

Only 0428-impl-plan.md, 0428-impl-log.md and
0428-state-of-the-world.md are intended worker edits in this recovery.

## Historical implementation and failed attempts

## Progress

- Read the task, worker/project protocols, slice, frozen gate snapshot,
  AGENTS.md, naming conventions, manuals 013/015 and binding rulings 030/031.
- Inspected the existing AT-1 builder, its three facts and shared file helpers.
  The committed kit currently contains only data/.
- Before implementation: add one comparison fact reusing the full builder and
  existing byte reader; preserve failed kit output and all earlier assertions.
- Added the single authorized comparison fact. It traverses the sorted union
  of both recursive inventories, excludes root .oc/, reuses assertSameBytes,
  and closes the kit before retaining a failure or deleting a successful run.
- Added the requested four-line AGENTS.md gotcha, without changing other text.
- Before generation: invoke the unchanged buildOpticsMuellerKit from the
  existing Release assembly, with its application base and run directory
  under .artifacts/0428-kit-generation/. Copy only generated root/library
  files into the committed kit; raw lab files remain read-only.
- The first generation completed all application operations and wrote the kit,
  but the artifact helper treated KitContext.close as a function field rather
  than its existing method. Corrected only that helper and regenerated into
  a different fresh artifact folder so close can complete normally.
- Generation completed with exit code 0 through the unchanged compiled
  buildOpticsMuellerKit and real application messages/KitProxy. Its base folder,
  catalogue/schema copies, app locations and output all live under .artifacts/.
- Copied 116 files without rewriting bytes: .kit-json, nine items, 42 setups,
  53 experiments, one study, two fit runs and eight library snapshots. The
  copy script refuses existing destination files and excludes data/ and .oc/.
- Reviewed the final diff: 28 added lines in KitAcceptanceTests.fs and four
  in AGENTS.md; earlier facts and the builder are unchanged. Normal and
  ignore-CR-at-EOL numstats agree. No tracked lab file changed, none of the
  generated paths is ignored, their JSON has no CR bytes, and .oc/ stays ignored.
- Wrote the state-of-the-world handoff. All requested implementation and data
  effects are present; supervisor verification remains pending.

## Files modified

- 0428-impl-plan.md and this log: canonical worker handoffs.
- KitAcceptanceTests.fs: one new fact; no earlier assertion or builder change.
- AGENTS.md: one reference-kit maintenance bullet.
- data/optics-mueller/: 108 new root kit files and eight new library snapshots.
- 0428-state-of-the-world.md: implementation and verification handoff.

## Attempt 01 testing state (superseded by attempt 02)

No gates run. Invariant 6 assigns their execution and verdicts to the supervisor.
The authorized census adds one kit-acceptance fact; no existing identity changes.
No new measured count or checkpoint is authored by the worker.
The new fact has not been compiled or run by this worker. Neither its prescribed
red/green bootstrap runs nor any gate are claimed. The data-generation invocation
is an implementation action using existing binaries, not a gate result. No gate
duration or deadline overrun exists to report. commit_ready=false indicates
pending supervisor verification, not an outstanding implementation requirement.

## Architecture

Reuse the message-driven AT-1 build and real folder KitProxy. No durable shape
changes. R0001/R0002 remain intact.

## Gotchas

- The slice's test-run bootstrap recipe conflicts with IMPLEMENT's act-only
  protocol. Generate through the existing compiled builder directly and record
  the provenance; leave comparison and gate verdicts to the supervisor.
- specs/0050/.manifest.state.json was already dirty and belongs to the supervisor.

## Artifacts

Generation scripts, generated kit and provenance go beneath specs/0050/.artifacts/.

- 0428-kit-generation/generate.fsx: invokes the existing AT-1 builder and closes
  its KitContext; no copied builder implementation or production changes.
- 0428-kit-generation/generate.log: first generation and artifact-helper close error.
- 0428-kit-generation/generate-final.log: successful fresh generation and close.
- 0428-kit-generation/KitAcceptanceTests/reference-final/optics-mueller/: source kit.
- 0428-kit-generation/copy-reference.ps1: one-time byte-preserving copy operation.
- 0428-kit-generation/copy-provenance.json: source/target, assembly SHA-256 and
  relative path, length and SHA-256 for each of the 116 copied files.

## Attempt 02 progress

- Read the retry task, worker/project instructions, binding rulings, slice,
  gate snapshot and immutable S121-COMMITTED-KIT requirement. Preserved the
  existing implementation, generated reference and supervisor-owned changes.
- Read the failed file from the retained supervisor run. Its analyzer slot ID
  differs from the reference at byte 596; the surrounding scan is unchanged.
- Traced AddElement to AddElementBoundTo to addElement, which assigns freshId
  at TableAndElementRotationView.fs:1722; freshId calls Guid.NewGuid at :813.
  There is no ElementIdProvider or explicit-ID add message. Kit/item IDs and
  the clock are injected already, so their repeatability does not cover slots.
- Before diagnostic capture: inventory the retained failure's byte differences
  and JSON field paths, without running the builder or a gate. Save the
  evidence only under the supplied .artifacts folder. Then record whether the
  required repair fits the authorized effects; preserve exact-byte acceptance.
- Saved 0428-attempt-02-byte-diagnosis.json. Both inventories contain 141 files,
  excluding .oc/, with none missing or extra. Exactly 95 files differ: all
  42 setups and all 53 experiments. Every differing JSON value is one of the
  three AddElement slot IDs. All other bytes, including both fit runs and
  all lab files, match the retained failure's reference.
- Confirmed the binding requirement in .manual/001 §Q: the kit is built
  "ONLY through the semantic messages" and compared file by file. The frozen
  S121-COMMITTED-KIT oracle repeats this requirement. A fixture-side record
  rewrite of persisted slot IDs would bypass those messages; normalization
  during comparison would weaken the exact-byte obligation. Recopying a failed
  run changes the reference's random IDs but cannot make the next run repeatable.
- Missing authority: edit OpticalConstructor.Ui/TableAndElementRotationView.fs
  to inject element identity generation into the existing AddElement path.
  0428 authorizes KitAcceptanceTests.fs, the narrow AGENTS.md bullet and listed
  kit files, not that production file. R0001/R0002 explicitly confine their
  additional production-edit authority to step 0427.
- The proposed minimum repair is an ElementIdProvider on the table model,
  defaulting to fresh production GUIDs and consumed by addElement, with AT-1
  supplying a fixed sequence for its prep, second object and analyzer slots.
  This can be implemented in the UI file plus the already-authorized fixture;
  no durable format or test identity needs to change. The kit must then be
  regenerated through the real builder and copied without editing JSON.
- Before handoff: report the conflict to the supervisor/operator, leaving all
  source, reference data, acceptance assertions and gate metadata unchanged.
- Wrote the canonical state-of-the-world with the measured failure, exact
  missing authority, proposed repair and commit_ready=false. This is an
  unresolved S121-COMMITTED-KIT implementation obligation, not merely pending
  verification. No implementation is deferred to another slice.

## Testing state

No gates or tests were executed by this worker in attempt 02, per Invariant 6.
The read-only diagnostic script inspects existing supervisor output; it is
neither a builder invocation nor a gate result. No development-run deadline
overrun exists to report.

The saved supervisor console reports build passed (83.5 s, then 3.9 s),
elevation passed (229.6 s), ui-smoke passed (230.2 s), selected ui-tests passed
(28.7 s), and kit-acceptance failed after selective retries (105.4 s total).
The final filtered run reports one failure in 18.561 s. These are historical
supervisor measurements, all within the task's deadlines. The remaining
21 invariant gates were not reached in that verification phase.

commit_ready=false: S121-COMMITTED-KIT remains red, and its conforming repair
requires additional edit authority for the UI's element-ID creation seam.
No existing fact, assertion, census, floor, deadline or acceptance obligation
was changed. Do not import synthetic results or add a gate allowlist entry.

## Attempt 02 files and artifacts

- Updated only 0428-impl-plan.md, 0428-impl-log.md and
  0428-state-of-the-world.md in their canonical .slices locations.
- Added .artifacts/0428-attempt-02-byte-diagnosis.json: complete differing
  field/value pairs and SHA-256 hashes for the 95 files, relative inventories,
  and gate-result records extracted from the saved supervisor console.
- Preserved prior source/data edits and supervisor-owned
  .expected-red-observed-json and .manifest.state.json.
