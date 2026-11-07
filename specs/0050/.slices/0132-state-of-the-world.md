# Where we are

Slice 0132 is returning from the normal recovery worker for supervisor
verification. Entry HEAD `66ac13c` contains the operator-approved S41-KIT-LOCK
repair; the S42-ATTACH-SHAPES implementation is preserved. Manual 009 resolves
the earlier authority conflict and explicitly requires this normal profile-1
resume. This worker reviewed both obligations and updated only its three
reports. No additional implementation change was needed.

# What's working

- Preserved data-folder validation, first-free collision suffixes and lexical
  kit containment using the existing relative-path validator.
- Preserved lab-file sources, attachment paths and hashes, and
  unchanged/changed/missing status.
- Preserved the shared bounded guard for acquisition and release, typed IO
  diagnostics, ownership retention and temporary-file cleanup.
- Preserved all four prescribed attachment facts and existing lock coverage.

# Tests

This IMPLEMENT worker ran no gates, as invariant 6 requires. Build, elevation,
all-discovered constructor-unit-tests and all eleven s16 invariants remain for
normal supervisor execution after worker exit. No gate result is asserted by
this worker's structured handoff.

The existing operator evidence is indexed by
`../.artifacts/0132-operator-repair/final-review.md`. Its `gate-results.json` and
`verification-summary.json` record the earlier 14-gate run and 1,076 constructor
cases with zero failures/skips and exact census, completed on 2026-11-07. The
review also records a zero-warning final build, seven-project elevation run,
58 focused kit cases and three independent-process contention scenarios.
Those are inherited operator measurements, not a fresh worker run or a
substitute for supervisor acceptance.

Source review covered the four attachment facts, scripted retry bounds and
permanent failures, interrupted waits, failed release and recovery, foreign
ownership, and repeated synchronized empty/stale/malformed contention. No
test identity, assertion, selection or expected-red record was changed.

The following count is inherited from supervisor checkpoint 130. Checkpoint
131 is absent at handoff; only successful normal supervisor verification may
publish it and the newly measured constructor floor (expected 1,076).

```yaml
gates:
  constructor_unit_tests: 1072
```

# Architecture

Attachment behavior stays pure in Domain/Kit.fs and reuses KitRelativePath,
ContentHash and STL FileByteContent. Data-folder validation is ordinal and
case-sensitive; collisions and lexical containment ignore case. There is no
new disk operation or durable-format change in the attachment shapes.

Storage's KitLockGuardProxy owns one guard-open retry policy shared by
acquisition and release: native 5/32/33 failures allow at most 101 openings and
100 ten-millisecond waits. Other failures return immediately. Exhaustion
preserves exception type, HRESULT and guard path in KitIoErr. FileShare.None,
DeleteOnClose, ownership rechecks, non-overwriting publication and finally-based
cleanup remain. Failed release retains ownership for a subsequent retry.

# Deferred

No implementation work is deferred. The completed implementation is returned
for the supervisor's required review and all 14 unchanged gates. This worker
does not advance the arc; only successful normal acceptance publishes
checkpoint 131, updates the measured baseline and completes expected-red
bookkeeping. No direct acceptance or state reset is authorized.

# Gotchas

- `.executability-json`, `.manifest.state.json` and `.supervisor-lock.diag`
  were already modified on entry. This worker leaves them and all checkpoints,
  counters, baselines, expected-red records, oracle and compiled artifacts
  untouched.
- Manual 009 overrides the standing committed-green direct-accept rule for
  this recovery. Retain `--executability-profile 1`; do not use
  `--accept-current-slice` or fabricate a checkpoint.
- The retry limit bounds attempts and requested waits; scheduling and native
  IO may add elapsed time. The existing process evidence covers local Windows,
  not remote filesystem behavior.
- The original failure omitted its HRESULT. The operator reproduction observed
  native 5 and 32 without claiming the missing original measurement.
- Future shared-guard edits must preserve manual 009's contract. No changes to
  ASG, STL, version rings, production source or tests were made in this round.

# Changelog

- 2026-11-07: Added attachment shapes and four prescribed facts.
- 2026-11-07 (attempt 02): Reported the storage-guard repair authority conflict.
- 2026-11-07 (operator recovery): Authorized, repaired and verified the shared
  guard, refreshed the receipt and prepared the normal-resume handoff.
- 2026-11-07 (normal recovery resume): Reviewed the committed S41/S42 behavior
  and existing coverage, preserved the implementation, and refreshed worker
  reports for normal supervisor verification without running gates.
