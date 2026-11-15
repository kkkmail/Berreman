# Implementation log

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
