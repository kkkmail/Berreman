# Where we are

Slice 0132's S42-ATTACH-SHAPES implementation is preserved. The operator-approved
recovery in `.manual/009-step-0132-lock-guard-recovery.md` resolves the earlier
S41-KIT-LOCK repair authority conflict and the measured guard failure. The repair
is verified and ready for the operator's commit. The arc remains stopped and
escalated at 0132; accepted 0131 is `6d45e90`, and repair entry HEAD is `90b0649`.

# What's working

- Validated data-folder selection, collision naming, lexical kit containment,
  lab-file sources, attachment hashes and unchanged/changed/missing status.
- Shared bounded filesystem guard for acquisition and release, including
  transient native access-denied and sharing/lock violations.
- Exclusive coordination, winner diagnostics, stale/malformed recovery,
  ownership retention after a failed release and temporary-file cleanup.
- All four prescribed attachment facts and every existing lock assertion and
  fact identity are preserved.

# Tests

The operator-authorized recovery ran all 14 unchanged 0132 gates successfully:
build (zero warnings/errors), elevation (seven projects, zero new findings),
all-discovered constructor-unit-tests (1,076 passed, zero failures/skips, exact
census), and all eleven s16 invariants. The earlier 1,075/1,076 result is retained
in the original diagnostic; it has been superseded by this measured passing run.

Additional checks passed: 58 focused kit cases; deterministic retry exhaustion,
immediate failure and release recovery; 25 synchronized contention rounds each
for empty, stale and malformed filesystem locks; and three separate-process
scenarios with three contenders each, one winner and clean release. These last
checks cover local Windows processes, not a remote filesystem deployment.

All 494 steps validated and all 988 compiled artifacts are byte-identical. The
refreshed profile-1 receipt validates. Evidence and logs are under
`.artifacts/0132-operator-repair/`, indexed by `final-review.md`.

The YAML is the inherited supervisor-owned baseline from checkpoint 130, not
the new operator measurement. Checkpoint 131 does not yet exist. Only a normal
successful supervisor run may publish the new floor of 1,076.

```yaml
gates:
  constructor_unit_tests: 1072
```

# Architecture

Attachment behavior remains pure in Domain/Kit.fs; its only recovery edit is a
reserved-local rename with unchanged rendering. `KitTests.fs` is unchanged.

Storage's `KitLockGuardProxy` replaces the private unbounded spin loop with the
same exclusive delete-on-close primitive and a deterministic IO seam. It permits
101 opens with at most 100 ten-millisecond waits for native codes 5, 32 and 33.
Other errors return immediately; exhaustion preserves exception type, HRESULT
and guard path in `KitIoErr`. This is an attempt bound, not a hard wall-clock
deadline for operating-system IO. Acquisition and release share the policy and
retain the ownership recheck under the guard. No durable format changed.

# Deferred

No repair work is deferred. Await the operator's commit and explicit relaunch
instruction. Resume normally with the recovery `operator-reply.md` and
`--executability-profile 1`, as manual 009 directs. Do not accept the slice
directly: normal supervisor verification must publish checkpoint 131, raise the
floor from 1,072 to 1,076 and finish expected-red bookkeeping.

# Gotchas

- State, counters, checkpoints, baselines, expected-red records, published
  requirements, oracle and compiled artifacts were preserved. No commit or
  relaunch was performed; ASG needs no change for this incident.
- The original incident omitted its native HRESULT. A direct reproduction of
  the same guard primitive produced both native errors 32 and 5; this supports
  the mechanism without inventing the original missing measurement.
- Future guard edits must preserve manual 009's bounded retry and ownership
  contract. Blanket unbounded retries can hang on real permission failures.
- Data-folder validation remains ordinal and case-sensitive; collision and kit
  containment checks remain ordinal and case-insensitive.

# Changelog

- 2026-11-07: Added attachment shapes and four prescribed facts.
- 2026-11-07 (attempt 02): Diagnosed the supplied storage guard failure and
  reported the required out-of-scope repair authority.
- 2026-11-07 (operator recovery): Recorded that authority, repaired and verified
  the shared guard, refreshed the receipt and prepared the normal-resume handoff.
