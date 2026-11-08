# Where we are

Step 0147 implements S43-SCENE-FILE in section S4.3. Scenes now have a versioned
Domain envelope, setup schema and canonical text adapters using step 0146's
shared stack. All six authorized product paths and five declared test identities
are implemented. The supervisor owns execution and acceptance of the gates.

# What's working

- Added filesystem-safe scene stems through the shared FileStem name rule.
- Added ordered, distinct latest library references to SceneSnapshot.
- Added the version-1 SceneFile envelope with the original name and snapshot.
- Added the conditional setup schema, lazy validator and copied content item.
- Reused canonical serialization and versioned parsing for setup files.
- Added five facts for naming, round-trips, versions, invalid input and references.

# Tests

No build, test, analyzer, invariant or other executable check was run by this
IMPLEMENT worker, as required by Invariant 6. No passing result is claimed.
The supervisor must run build, elevation, all-discovered constructor-unit-tests
and the eleven s17 invariants from the unchanged 0147.gates snapshot.

The five new facts use pure Domain records: a source bound to source600, a
polarizer bound to polarizerLp, an unbound detector on the polarizer's reflected
beam, a panned/zoomed view, SnapToBeam and PivotAtIncidentFace. Assertions cover
the required names and safe stems; equal file round-trip and preserved original
name; LF-only canonical text with one final newline and ordered root keys;
unsupported version diagnostics with intact, absent and malformed bodies;
required/extra fields, invalid names/versions, malformed serializer-owned
values and malformed JSON; and ordered distinct references, including unbound
and non-entry value ids. Existing experiment facts and assertions remain.

Checkpoint 145 records the inherited 1141-case constructor baseline below.
The five added facts imply an expected increase of five, not a measured count.
Only the supervisor may publish the next checkpoint.

```yaml
gates:
  constructor_unit_tests: 1141
```

# Architecture

SceneFile contains schemaVersion, name and the existing SceneSnapshot shape.
SceneName.fileStem delegates to FileStem.ofText. SceneSnapshot.libraryReferences
uses ElementValueId.tryToLibraryEntryId, keeps the first occurrence of each id
in element order and returns EntryLatestReference values.

The setup schema unconditionally requires a positive schemaVersion through the
shared library-common definition. Only version 1 closes and validates the root
and scene records; elements must be an array and name must contain a non-space
character. Scene field binding remains the canonical serializer's job. Other
positive versions reach UnsupportedSchemaVersionErr before binding.

SchemaValidation loads the schema once through the existing common-schema
loader and delegates to validateWith. SceneFile.fileText calls nodeOf and
canonicalFileText; parseSceneFile calls parseVersionedText. The existing private
JsonText conversion was renamed to toContentJsonText and shared by both content
parsers. There is no new parser, serializer, migration or public conversion.

# Deferred

No implementation requirement is deferred. Gate execution is pending with the
supervisor under the mandated worker protocol. Commit readiness is not claimed
before those results exist; there is no implementation authority block.

# Gotchas

- Keep the body validation conditional so a future version with no scene body
  yields UnsupportedSchemaVersionErr rather than SchemaValidationErr.
- JsonText is the shared parser's live parameter type; the slice's string
  shorthand is adapted by the existing private source-type conversion.
- The setup schema must ship beside the assembly in schema/; the content item
  uses PreserveNewest, just like the experiment schema.
- Source diff review showed matching product numstats with and without ignoring
  CR at line ends. New files were written with LF line endings.
- The supervisor-owned manifest-state change, checkpoints, test census, oracle,
  version rings and analyzer rules were not edited. No git mutation or sibling
  repository edit was performed.

# Changelog

- 2026-11-07: Added canonical versioned scene files, setup validation, safe naming,
  library references and five authorized facts; prepared supervisor handoff.
