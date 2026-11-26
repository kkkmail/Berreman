# Where we are

Step 0148 implements S43-KIT-CONTENT-VERBS in section S4.3. The canonical
experiment and scene adapters from steps 0146-0147 now back six typed KitProxy
operations. Both persistence round-trip modules use initialized kit folders.
All three authorized source paths and all declared test effects are implemented;
the supervisor owns gate execution and acceptance.

# What's working

- Added typed experiment write, delete and ordered read operations over kit files.
- Added typed scene write, read and ordinal name listing over setup files.
- Rejected unreadable content and experiment identity mismatches with filename diagnostics.
- Moved both round-trip suites onto fresh folders beneath the test output directory.
- Preserved scene restoration and experiment-shape assertions across the storage change.
- Covered replacement, name collisions, ordering, missing files and read-only byte preservation.

# Tests

No build, test, analyzer or invariant gate was run by this IMPLEMENT worker,
as required by Invariant 6. No passing gate result is claimed. The supervisor
must run the unchanged 0148.gates roster: build, elevation, all-discovered
constructor-unit-tests and the eleven s17 invariants.

The declared effects rename seven facts, delete two obsolete blank-name store
facts and add three facts, leaving four SceneRoundTripTests facts and six
ExperimentCollectionRoundTripTests facts. Existing applicable assertions remain.
Additional assertions within those identities cover fresh-reader persistence,
original-name ordering independent of sanitized filenames, colliding scene
stems, numeric experiment ordering, case-insensitive stems, rendering refusal
without replacement, individual malformed scene reads, exclusive-handle IO
refusals, and successful reads after those handles close. Read-only coverage
compares the complete file set and every byte after each refused write and
also exercises all three read members.

Checkpoint 146 records the inherited constructor baseline below. The authorized
effects imply an expected increase of one, to 1147 cases; this is not a measured
result or a new baseline. Only the supervisor publishes checkpoints.

```yaml
gates:
  constructor_unit_tests: 1146
```

# Architecture

KitContentFiles.fs has one KitProxy extension containing exactly the six
requested members. Writes use the existing fileText adapters and writeKindFile;
deletion uses deleteKindFile. Reads use listKindFiles/readKindFile and the
existing versioned parsers. No proxy field, serializer, cache, retry, format or
migration was introduced.

Serialization and parsing failures map their StorageError.describe into
KitFileRejectedErr with the filename. Per-file IO refusals also become named
KitFileRejectedErr values; list and mutation failures retain their proxy
channels, and readScene preserves MissingKitFileErr for an absent scene.
Experiment stems compare ordinally ignoring case; successful experiment lists
sort by id. Scene names come from parsed envelopes and sort ordinally, so names
sharing a sanitized stem replace the same file and retain the last written name.
All writes flow through the proxy verbs and inherit readOnlyView's refusal.

The two test modules share one internal, test-local initialized-folder helper
in SceneRoundTripTests.fs, which already compiles first. Each invocation uses a
fresh GUID folder under AppContext.BaseDirectory and the calling class name.
Manual 010 explicitly leaves step 0148's existing scene initializer signatures
unchanged; the later scratch-kit changes were not pulled into this step.

# Deferred

No implementation requirement is deferred. Gate execution is pending with the
supervisor under the mandated worker protocol. Commit readiness is not claimed
before those results exist.

# Gotchas

- A malformed, misnamed or inaccessible file refuses the whole corresponding
  list; successful neighboring files are never returned as a partial result.
- Scene identity is its safe filename stem, while its stored name remains the
  original display name. Reading through either colliding name reaches the
  same latest scene.
- Exclusive test handles use FileShare.None and lexical disposal; no sleep or
  timing assumption is involved. Test kits never write to Documents or data/.
- Source diff inspection showed matching product numstats with and without
  ignoring CR at line ends. Edited files use LF.
- The pre-existing supervisor manifest-state modification is untouched. No
  checkpoint, census, oracle, gate, version ring, analyzer rule, notebook,
  sibling repository or git index was changed by this worker.

# Changelog

- 2026-11-07: Added six typed kit-content operations and repointed the ten
  authorized round-trip facts to real kit folders; prepared supervisor handoff.
