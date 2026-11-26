# Where we are

Step 0150 implements S43-OPENING. The partial implementation is committed at
`7addce1`; accepted 0149 (`aa5ff97`) is the complete-slice comparison base.
The operator-approved recovery in manual 011 is now committed at `70254b5`.
This normal-resume worker reviewed and preserved it against the slice, immutable
acceptance oracle and manuals 008–011, then updated the three supplied handoff
documents. Normal supervisor review and all sixteen gates are the next action.

Eight inherited JSON-index expressions are now explicit `.[]` accesses, clearing
all ten FS3369 diagnostics without changing their malformed values or assertions.
The three existing picker facts directly exercise the new kit adapter using the
existing isolated fixture. Every original assertion and test identity remains.
The kit-opening implementation and its six declared facts required no change.

# What's working

- Read-write kit initialization/opening, experiment and scene reads, snapshot
  decisions and session overlays compose the existing Domain/Storage seams.
- Foreign-lock errors propagate; failed opens close their contexts without
  replacing the first error. Read-only opening writes nothing.
- Snapshot decisions, including SameContentHash, stay pending until confirmation;
  opening does not mutate the library or import log.
- The actual window picker has direct coverage for resolved/unresolved suggestion, title,
  single-selection, first-local-folder, cancellation, non-local-first selection
  and provider acquisition/resolution/dialog failure tests. It creates no folder.
- Scratch construction preserves manual 010's Result-returning Storage factory,
  independent backing folders and typed initialization failures.
- Preserved all six declared facts, approved JSON/picker repairs and original
  assertions; refreshed the worker handoff without changing product code.

# Tests

This resumed IMPLEMENT worker ran no checks or gates under Invariant 6. The
source review found the six declared identities, prescribed compile positions
and approved manual 011 changes intact. This is source review, not a new test
discovery or execution result. All sixteen gates remain for the supervisor.

Operator verification in `.artifacts/0150-operator-repair/` passed all sixteen
unchanged gates: build, elevation, constructor-unit-tests, ui-smoke, selected
ui-tests and all eleven s17 invariants. Measured totals are 1,157 constructor
cases, 215 smoke cases and exactly fourteen non-smoke AppContextTests identities,
with zero failures/skips and exact profile-1 census. The selected UI count is
not a whole-suite floor.

The forced Release rebuild reports no owned-code warning and no FS3369. Its two
SYSLIB0051 warnings are in vendored MathNet `Numerics/Exceptions.cs`. The normal
build gate reports zero warnings/errors. Nine focused JSON facts and three
extended picker facts also pass. The preliminary unsupported wildcard invocation
ran zero tests and was corrected to the three exact names; its log is retained.

Supplemental elevation against accepted 0149 covers the committed Ui project
and all three affected dependents: zero findings under the existing ELEV004
exclusion. All 494 steps validate, all 988 compiled artifacts are byte-identical,
and the refreshed profile-1 receipt validates.

Only the supervisor publishes new checkpoint counts. Preserve the inherited
floors of 1,151 constructor cases (checkpoint 148) and 215 smoke cases (143):

```yaml
gates:
  constructor_unit_tests: 1151
  ui_smoke_tests: 215
```

# Architecture

KitOpening.fs composes existing Domain and Storage seams. Read-write opening
uses KitContext.openOrInitialise; read-only opening uses KitContext.openKit.
The opened context supplies content reads, snapshot decisions, an initial
overlay and ExperimentDataStore.createFileBacked. A downstream failure closes
the context; a failed close is logged while the original error remains the answer.
withRebindings replaces only the overlay through KitLibraryContext.openOverlay.

KitFolderPicker.ofWindow captures the TopLevel and suggested folder in the
existing async proxy. It returns only a local first selection and catches
provider failures at that boundary. ClockProvider.system reads UTC on every call.
scratchKit reuses Storage's fallible createInMemory under manual 010. No durable
shape, serializer, cache, retry, confirmation policy or UI initializer changed.

# Deferred

No 0150 implementation requirement is deferred. The approved repair has been
committed and this worker was dispatched with its recovery note. Return the
preserved implementation to the current normal supervisor review and full
verification flow under `--executability-profile 1`. Manual 011 explicitly
requires this flow, without `--accept-current-slice`.

Only successful supervisor acceptance publishes checkpoint index 149, measured
case evidence and expected-red bookkeeping. This worker does not launch another
supervisor, reset counters, publish a baseline or fabricate checkpoint records.
Its empty gate results and commit_ready false indicate pending supervisor
acceptance, not an unresolved implementation or authority conflict.

# Gotchas

- Manuals 008–010 remain binding. Later edits retain the corrected indexing,
  existing picker identities and their expanded assertions.
- An incremental zero-warning summary cannot clear an earlier diagnostic;
  require source repair and actual recompilation.
- Each test fixture stays under the test output directory; picker tests use
  separate providers/windows and observe assertions outside production's catch.
- This worker did not edit published requirements, oracle, gate snapshots, test
  effects, state, baselines, checkpoints, expected-red records, analyzer rules,
  version lock or receipts. Entry changes to `.executability-json`,
  `.manifest.state.json` and `.supervisor-lock.diag` belong to the supervisor.
- No version bump, Git mutation, ASG source change or arc restart was performed
  by this worker.

# Changelog

- 2026-11-07: Implemented kit opening and six facts; retry attempts repaired the
  pattern and list-equality syntax. Review escalated over inherited warnings
  and missing direct picker coverage outside the original edit authority.
- 2026-11-08: Applied approved manual 011, repaired both test files, completed
  forced recompilation and full verification, and prepared the commit handoff.
- 2026-11-08 (normal resume): Reviewed and preserved committed recovery
  `70254b5`; refreshed worker handoffs for supervisor verification without
  running checks or changing product/test code or protected records.
