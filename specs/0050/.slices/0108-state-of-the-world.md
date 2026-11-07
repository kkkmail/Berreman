# Where we are

Slice 0108 completes S32-DIALOG-WINDOW in S3.2: the import-dialog host and shared file-drop adapter. Attempt 03 repairs the supervisor's test-identity mismatch while preserving the reviewed adapter coverage. Only ImportDialogTests.fs changes in this retry; formal acceptance and commit remain supervisor-owned.

# What's working

- Added ImportDialogWindow over the existing MVU with stable automation identity and screen fitting.
- Added the guarded EntryFileDrop adapter with complete ordered FileName delivery for nonempty drops.
- Added real-window Apply acceptance and small-screen reachability over all built-in catalogue rows and a wide collision row.
- Preserved all seven registered-handler scenarios as private helpers executed by the declared smoke acceptance.
- Restored the authorized test identities while preserving every original acceptance and adapter assertion.

# Tests

Attempt 03 measurements:

- Release solution build passed in 12.93 seconds with zero warnings/errors.
- Selected ImportDialogTests and ScrollExtentTests passed 18/18 cases in 7.659 seconds, with zero failures/skips.
- All-discovered smoke passed 212/212 cases in 61.821 seconds, with zero failures/skips.
- Both TRX identity multisets exactly match the retained attempt-01 reports: no missing or unexpected identities. Each declared acceptance identity passed once in its assigned selection.
- All seven adapter helper bodies match their attempt-02 fact bodies verbatim. Each is called once by the passing declared smoke acceptance, before its window action dispatches to the UI thread. The original window acceptance body follows those calls verbatim; the reachability body is unchanged.
- Source files remain LF-only; whitespace checks pass and ordinary/ignore-CR diff statistics agree.

The supervisor rejected attempt 02's 25-case ui-tests observation because seven identities exceeded the declared effects, despite all cases passing. This retry retains their behavior checks within the authorized smoke identity. No oracle, effect, test selection, checkpoint or baseline was changed.

Attempt 01's elevation evidence (four projects, zero new findings, ELEV004 excluded) and eleven s13 invariant passes are retained for unchanged product/policy files. Those checks were not rerun in this test-only retry. Formal supervisor verification is pending.

```yaml
gates:
  ui_smoke_tests: 212
  ui_tests: 18
```

These are measured selection counts, not edits to supervisor baselines. Fresh evidence lives under specs/0050/.artifacts/: 0108-attempt-03-build.log, 0108-attempt-03-ui-tests.log, 0108-attempt-03-ui-smoke.log, 0108-attempt-03-tests/{ui-tests,ui-smoke}.trx, 0108-attempt-03-evidence.py, 0108-attempt-03-evidence.log and 0108-attempt-03-verification-results.json. Earlier artifacts remain intact.

# Architecture

ImportDialogWindow composes the existing ImportDialogContext, update and view through Program.mkProgram. Only Apply reaches the import seam. The initializer is the context member established by slice 0107's ELEV013 repair.

EntryFileDrop remains the platform IO edge: file-only Copy feedback and complete ordered path materialization before dispatching one nonempty drop, with platform failures contained. Its tests install the actual handlers and observe attempted reads for controlled format, item-list, item-value and later-path failures. The retry changes only test composition: private assertion helpers preserve coverage without creating unauthorized discovered identities. No durable shape, migration, package, version or production behavior changed.

# Deferred

None within slice 0108 or this retry.

# Gotchas

- A selected class does not authorize new test identities. Step 0108 declares exactly the window smoke acceptance and reachability fact. Keep all seven adapter helpers called from the declared acceptance; do not add Fact attributes without new authority.
- The helpers dispatch their own headless actions and must remain outside the window acceptance's HeadlessSession.run callback.
- Library and Materials wiring belongs to slices 0110 and 0111; this slice provides the shared seam.
- Apply closes on PointerPressed. The headless click helper sends MouseUp only while the window remains visible.
- Session-level verification requirements take precedence over the worker prompt's act-only wording; formal acceptance and checkpoints remain supervisor-owned.
- Pre-existing supervisor changes to .manifest.state.json and .expected-red-observed-json remain untouched.

# Changelog

- 2026-11-07, attempt 01: Added the dialog host, drop seam and two acceptance facts; retained passing build, elevation, 212 smoke cases, 18 selected UI cases and eleven invariants.
- 2026-11-07, attempt 02: Added seven adapter facts with passing runtime results; the supervisor subsequently rejected their unexpected identities.
- 2026-11-07, attempt 03: Preserved all seven adapter scenarios inside the declared smoke acceptance; passed warning-free Release build, 18 selected UI cases and 212 smoke cases, with exact identity-set parity.
