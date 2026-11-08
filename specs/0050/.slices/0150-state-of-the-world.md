# Where we are

Step 0150 implements S43-OPENING in section S4.3. The UI now has the kit-opening
composition needed by subsequent constructor-window wiring, with the six
authorized KitOpeningTests facts. The four authorized source/project edits and
worker handoff documents are complete. Executable verification belongs to the
supervisor after this worker exits.

Attempt 03 repairs the two ambiguous Assert.Equal calls reported by the
supervisor, preserving attempt 02's material-error pattern repair. The existing
production implementation, six test identities and all assertions are preserved.
The reported FS3369 warnings in KitContentFileJsonTests.fs require authority
outside this slice's edit list; no warning-free build is claimed.

# What's working

- Added kit opening with experiment and scene reads, snapshot decisions and session overlays.
- Preserved foreign-lock errors and closed failed opens without replacing their first error.
- Kept every snapshot decision pending, including unchanged content, without applying imports.
- Added the local-folder picker and current UTC clock provider at the UI boundary.
- Added fallible, independent scratch-kit construction under manual 010.
- Added the six declared facts and repaired their pattern and equality typing, preserving every assertion.

# Tests

The supervisor's latest build failed with FS0041 at KitOpeningTests.fs:276-277.
Attempt 03 specifies CatalogueImportLogEntry list and HeldEntry list on the
two equality assertions so each compares a complete list. Attempt 02's earlier
parser repair remains intact. No build or test result after this repair has
been measured by this worker.

The supplied build also reports ten FS3369 indexing warnings in
KitContentFileJsonTests.fs around lines 175-177, 190-194 and 368-369. Steps
0146-0147 own that file; step 0150 does not authorize editing it. The supervisor
must resolve that scope conflict with the zero-owned-warnings obligation before
claiming completion. No assertion, test identity or warning policy was weakened.

No build, test, analyzer or invariant gate was executed by this worker, as
required by IMPLEMENT Invariant 6. No passing result is claimed. The supervisor
must run the unchanged 0150.gates roster: build, elevation, all-discovered
constructor-unit-tests, all-discovered ui-smoke, ui-tests selecting
OpticalConstructor.Ui.Tests.AppContextTests, and the eleven s17 invariants.

The six facts retain the exact authorized identities. Their assertions cover
initializing a nonexistent folder, refusing read-only initialization, reading
persisted experiments and scene names, preserving the process lock diagnostic,
byte-for-byte read-only preservation with and without another writer, malformed
experiment/setup cleanup, snapshot-resolution cleanup, ordered mixed and
all-Reuse decisions, no library/import-log writes, replacement overlays, and
scratch metadata, empty content, lock absence and isolation.

The inherited constructor baseline is 1151 from checkpoint 148. The inherited
smoke baseline is 215 from checkpoint 143. Six added constructor facts imply
1157 cases if the supervisor discovers and passes them; that is an expectation,
not a measured result or a new baseline. Only the supervisor publishes new
checkpoint counts.

```yaml
gates:
  constructor_unit_tests: 1151
  ui_smoke_tests: 215
```

# Architecture

KitOpening.fs composes existing Domain and Storage seams. Read-write opening
uses KitContext.openOrInitialise; read-only opening uses KitContext.openKit.
The opened context supplies content reads, snapshot decisions, the initial
overlay and ExperimentDataStore.createFileBacked. Any downstream error closes
the context; a close error is logged while the original error remains the
answer. Pending decisions are the original list, and withRebindings replaces
only the overlay through KitLibraryContext.openOverlay.

KitFolderPicker.ofWindow captures the Avalonia TopLevel and suggested folder in
the existing async picker proxy. It accepts only local folder paths and catches
provider exceptions at that boundary. ClockProvider.system reads UTC on each
invocation. scratchKit reuses Storage's Result-returning createInMemory, as
mandated by specs/0050/.manual/010-step-0145-scratch-kit-construction.md. Each
scratch kit gets a fresh identity and private backing folder.

No durable shape, serializer, cache, retry, import confirmation policy or
existing UI initializer changed. No migration is needed.

# Deferred

No authorized KitOpening implementation requirement is deferred. Gate execution
remains pending with the supervisor by protocol. The reported warnings outside
the authorized edits are an unresolved scope conflict, not an accepted deferral.
Commit readiness is not claimed.

# Gotchas

- Manual 010 supersedes the slice's infallible scratchKit sketch. Callers must
  handle Result<KitContext, KitError>; there is no unsafe unwrap in production.
- SameContentHash rows require confirmation just like other snapshot decisions.
  Opening or rebinding an overlay does not apply an import.
- Each disk fixture lives under the test output's KitOpeningTests/GUID folder
  and closes its contexts before removing its own fixture directory.
- The two project-file edits each add one compile line, with identical numstats
  when CR at line ends is ignored. New source and worker documents use LF.
- The pre-existing supervisor manifest-state modification was preserved. No
  gates, census, oracle, checkpoints, analyzer configuration, version rings,
  notebooks, sibling repositories or git index were changed by this worker.

# Changelog

- 2026-11-07: Added kit-opening composition, folder picker, UTC clock, fallible
  scratch kits and the six authorized facts; handed verification to the supervisor.
- 2026-11-07 (attempt 02): Repaired the reported test parser error, preserved all
  assertions and identities, and returned the unchanged gate roster to the supervisor.
- 2026-11-07 (attempt 03): Disambiguated two list equality assertions, preserved
  acceptance, and recorded the reported out-of-scope indexing warnings.
