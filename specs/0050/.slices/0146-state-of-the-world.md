# Where we are

Step 0146 implements S43-EXPERIMENT-FILE in section S4.3. Experiment files now
have their own versioned Domain envelope, schema and Storage text extensions,
with the four declared test identities. Implementation is complete for this
slice. Attempt 02 parenthesized two chained JSON indexer receivers to address
the supervisor-reported FS0597 errors. Execution and acceptance of its fourteen
gates belong to the supervisor.

# What's working

- Added E<n> experiment file stems and the version-1 ExperimentFile envelope.
- Added the conditional experiment JSON schema and cached shared validator.
- Reused canonical serialization and versioned parsing for experiment files.
- Added four facts covering naming, round-trips, versions and malformed input.
- Registered the new source files and build-copied experiment schema.
- Parenthesized the two attachment-field removal receivers in the invalid-file fact.

# Tests

No build, test, analyzer, invariant or other executable check was run by this
IMPLEMENT worker, as required by Invariant 6. No gate result is claimed.
The supervisor must run build, elevation, all-discovered constructor-unit-tests
and all eleven s17 invariants from the unchanged 0146.gates snapshot.

The retry task supplied build failures at KitContentFileJsonTests.fs lines 177
and 178. Both expressions are repaired without changing their assertions;
the supervisor's rerun is pending.

The new class declares exactly the four authorized facts, without changing any
existing test identity. Its local E1 fixture includes BoundByVersion for the
version-one 1 mm glass plate, BoundByEntryId for PresetIds.source600, an Unbound
polarizer varied over R1, a data/run 1.csv attachment with a lowercase 64-digit
hash and a 632.8 nm wavelength. E3 has no setup, varied element, attachment or
wavelength. Assertions cover:

- E3.experiment-json naming and equal round-trips for both fixtures.
- LF-only canonical text, one final newline, ordered root keys and stable
  rewriting of parsed content.
- Unsupported schema version 2 with intact, absent or malformed bodies and
  exact typed extension/found/expected diagnostics.
- Required root/experiment fields, extra keys, invalid ids/versions, attachment
  shape/hash/path validation and non-object roots.
- Malformed JSON and serializer-owned descriptor, binding, varied, measurement,
  range and wavelength payload failures with nonblank diagnostic reasons.

Checkpoint 144 supplies the inherited 1137-case constructor baseline below.
The four new facts imply an expected increase of four cases, not a measured
count. Only the supervisor may publish the next baseline. Source diff review
showed identical tracked product numstats with and without ignoring CR at line
ends; the existing supervisor-owned manifest CRLF warning was left untouched.

```yaml
gates:
  constructor_unit_tests: 1137
```

# Architecture

ExperimentFile contains only FileSchemaVersion and Experiment. Its current
schema version is 1; Experiment.experimentFile creates that envelope, and reads
access its experiment field directly. ExperimentId.fileStem delegates naming
to FileStem.ofText. No reverse conversion or migration is introduced.

The schema requires a positive schemaVersion unconditionally. Its version-1
branch closes the root and experiment records and validates attachment paths
and hashes, reusing library-common.schema.json definitions by $id. Other
positive versions reach the existing typed version check. Descriptor, binding,
range and wavelength payload binding remains the shared serializer's job.

SchemaValidation loads the common definitions before the new schema through
the existing lazy loading path. KitContentFiles delegates serialization to
nodeOf/canonicalFileText and parsing to parseVersionedText. The live parser
takes JsonText rather than the slice's shorthand string; a private conversion
member on KitFileText supplies that existing elevated type without changing
the parser or widening KitFolderStore's private member. The new source compiles
directly after KitFolderStore and its schema is copied with PreserveNewest.

# Deferred

No implementation requirement is deferred. All gate execution is pending with
the supervisor under the mandated worker protocol. Commit readiness is not
claimed before those results exist; there is no implementation authority block.
No later slice's storage orchestration or UI behavior was added.

# Gotchas

- Keep the schema's version-1 restrictions conditional: version-only future
  files must produce UnsupportedSchemaVersionErr rather than SchemaValidationErr.
- The fixture's wavelength uses the engine's 632.8<nm> unit annotation.
- Do not add test identities beyond the four declared effects or publish a
  baseline from the source-level test count.
- Supervisor state, checkpoints, oracle, census, analyzer rules and version
  rings were not edited. The pre-existing manifest-state change remains owned
  by the supervisor. No git mutation or sibling-repository edit was performed.

# Changelog

- 2026-11-07: Added the versioned experiment file contract, canonical text,
  schema validation and four authorized facts; prepared supervisor handoff
  without executing gates.
- 2026-11-07 (attempt 02): Repaired the two FS0597 fixture expressions while
  preserving all four test identities and acceptance assertions; updated handoff.
