# Where we are

Step 0141 implements S42-RESOLUTION after the snapshot storage, closure and kit import-origin steps. `KitLibraryContext.resolveSnapshots` now reads every snapshot through the shared import decision seam. Attempt 02 repairs the supervisor-reported indentation failure in the no-write test's nested record update. The implementation and five authorized facts are in place; executable verification belongs to the arc-runner after worker exit.

# What's working

- Resolve every snapshot with the named kit origin in listing order through the existing import decision function.
- Preserve typed kit and library read failures without writing or restricting read-only kits.
- Document mandatory confirmation of every snapshot row, including all-Reuse lists, with only empty kits bypassing the dialog.
- Add the five authorized facts covering reuse, missing entries, divergence, collisions and unchanged kit/library state.
- Repair the nested failure-fixture record indentation while preserving every assertion and test identity.

# Tests

No build, test, analyzer or invariant gate was run by this worker, as required by IMPLEMENT Invariant 6. The supervisor's attempt-01 build failed on the nested `failingKitContext` record's strict indentation; attempt 02 repairs that source defect. The supervisor owns build, elevation, all-discovered constructor-unit-tests and the eleven S16 invariants in `0141.gates`. The repair awaits those gates; no passing outcome or commit readiness is claimed.

Exactly five facts were added, with no theory cases or changes to existing assertions. They cover the required decision statuses and defaults, origins and collision names, retained snapshots, all-row confirmation, both access modes, listing/read order, empty kits, typed failures and short-circuiting. The no-write fact guards library and kit mutation seams and inspects the Built-in-filled SQLite library's envelopes, latest metadata, lifecycles, pins and import log. The expected suite count is 1,121; this is not measured.

The following repeats the supervisor's existing checkpoint `139` baseline, not a new worker-authored measurement. No checkpoint, test census, acceptance oracle or gate definition was edited.

```yaml
gates:
  constructor_unit_tests: 1116
```

# Architecture

The only new product member lists kit snapshots once, maps storage errors to `KitLibraryKitErr`, and traverses the list through `CatalogueImportProxy.decideEntry` with `KitSnapshotOrigin kit.kitFile.name`. Import errors map to `KitLibraryImportErr`. Accumulating then reversing preserves the listing order without filtering Reuse rows. An empty list returns `Ok []`; a failure stops further decisions.

Existing kit import defaults and the shared decision table are reused unchanged. No storage format, cache, option, proxy field, migration, UI wiring or other product member is added. The opener's mandatory dialog behavior is documented on the member as this slice requires.

# Deferred

None of the requested implementation is deferred. Supervisor gate execution remains pending under the worker protocol.

# Gotchas

- `specs/0050/.manifest.state.json` was already modified at entry and remains untouched by the worker.
- Source review does not establish compilation, analyzer compliance or passing tests. The structured exit leaves commit readiness false pending supervisor gates.
- The required test identities are exact, including punctuation and spaces; the suite increment is five.
- No version files, shared storage guards, analyzer settings, known findings, git index or history are changed.

# Changelog

2026-11-07 — Added ordered read-only snapshot resolution through the shared import decision seam, its confirmation contract and the five authorized facts.

2026-11-07 — Attempt 02 repaired the supervisor-reported F# record indentation failure without changing production behavior or test assertions; supervisor verification remains pending.
