# Where we are

Slice 0124 implements spec 0050 S4.1's `S41-KIT-FILE-TEXT` obligation: canonical kit metadata text, schema validation before binding and typed rejection of unsupported schema versions. The implementation and three authorized tests are written. Attempt 03 addresses the supervisor-reported ELEV010 by using the existing `Softellect.Sys.Primitives.JsonText` for the shared parser's input, with private source-type conversions in the catalogue and kit readers. The arc-runner's post-worker verification remains pending.

# What's working

- Added canonical kit-file rendering and schema-validated parsing through the existing typed kit metadata.
- Consolidated complete-file rendering for entry, catalogue and kit files in `JsonNode.canonicalFileText`.
- Moved catalogue versioned parsing into `CanonicalJson.parseVersionedText` and reused it for kit files.
- Added the version-conditional kit schema and once-only common-schema-first loading, with compile and content wiring.
- Authored the three authorized facts covering canonical text, unsupported versions and invalid input; preserved existing entry tests.
- Replaced the shared parser's raw-string input with `JsonText` and source-type conversions, addressing the reported ELEV010.

# Tests

No build, test, analyzer or invariant gates were executed by this worker. The IMPLEMENT protocol assigns their execution exclusively to the arc-runner after worker exit. The resolved roster is build, elevation, all discovered constructor-unit-tests and the eleven s15 invariant gates. No pass, measured count increase or commit-readiness result is claimed.

The attempt-03 task reports that review and build passed before this repair and supplies one ELEV010 finding in `CanonicalJson.parseVersionedText`. These are supervisor measurements, not this worker's results. The repair elevates that parameter and updates its two callers; all test identities and assertions remain unchanged. The repair awaits supervisor verification.

The round-trip fact covers the specified GUID, Unicode name, notes, exclusion globs and UTC instants with seven fractional-second digits. It asserts equal metadata, ordinal keys, indentation, LF-only text, exactly one final newline, readable Unicode, UTC timestamp kinds and stable second-write text. It also covers empty author, notes and exclusions.

The unsupported-version fact covers version 2 with the intact body, the version alone and an invalid body. Every case requires the kit extension, found version 2 and expected version 1. The invalid-input fact covers every required field, unexpected keys, blank names, invalid GUIDs, unconditional version constraints, field/item types and non-object roots. Malformed JSON, an invalid timestamp string and an overflowing version must return diagnostic `MalformedJsonErr` values.

Exactly three fact identities were added. The following is the inherited value in checkpoint 122, not a result measured by this worker or a newly authored baseline. The current checkpoint file has no checkpoint 123 entry; the supervisor owns subsequent measured counts.

```yaml
gates:
  constructor_unit_tests: 1021
```

# Architecture

`CanonicalJson.parseVersionedText` catches platform exceptions around the full read sequence: parse the text, validate the root, compare its top-level schema version and only then bind with the existing shared serializer options. Schema errors retain their validation messages; unknown positive versions yield `UnsupportedSchemaVersionErr`; parsing, version decoding and binding exceptions yield `MalformedJsonErr`. The catalogue manifest's old parsing body was removed in the same change.

Attempt 03 adjusts the slice's illustrative `text : string` parameter to `text : JsonText`: the measured ELEV010 rule rejects the former even at a marked boundary, and AGENTS.md gives the analyzers precedence. `JsonText` already exists in the referenced STL primitives, so no new type or package is needed. Each source text type owns a private `toJsonText` member, and the parser unwraps at `JsonDocument.Parse`. The three edited Storage files are authorized effects; the parser symbol, shared body, acceptance obligations and durable formats are preserved.

`JsonNode.canonicalFileText` owns indented canonical text plus one final LF. Entry, catalogue and kit writers all call it. The former entry and catalogue newline bodies were removed. Compact canonical rendering and content hashes are unchanged.

`kit.schema.json` always requires a positive integer schema version. Only version 1 requires the exact eight metadata keys and validates their types, shared GUID shape, nonblank name and nonempty exclusion strings. Other positive versions reach the typed version check without binding their bodies. The schema references the existing common definitions and is loaded lazily through the same common-first loader as library schemas.

The serializer preserves UTC ticks and the UTC suffix without a new date converter. `KitFolderStore.fs` provides the requested text extensions and their private conversion; no filesystem or proxy operations are introduced. This introduces the kit-file format at version 1 and preserves existing entry/catalogue formats. There are no migrations, up-converters or fallback readers, and no deviation from the slice's acceptance contract.

# Deferred

No implementation requirement is deferred. Gate execution is the supervisor's pending stage under IMPLEMENT invariant 6.

# Gotchas

- A storage-boundary annotation does not exempt a mixed raw/elevated signature from ELEV010. Keep `parseVersionedText`'s input elevated and the conversion on each source type.
- Elevation attributes and `StorageBoundary` are in `Softellect.Sys`; importing only `Softellect.Sys.Primitives` does not expose them.
- Common schema registration must remain behind its existing shared lazy value; the kit schema must not independently register that id.
- Timestamp strings bind through System.Text.Json after schema and version checks; schema validation deliberately does not add an ISO date format restriction.
- Existing `EntryFileJsonTests` remain unchanged so the shared-parser extraction is exercised by the same catalogue assertions.
- Tracked source diff numstats agree with and without ignoring CR at line ends; added files use LF.
- The pre-existing modifications to `.manifest.state.json` and `.expected-red-observed-json` remain supervisor-owned and untouched. No gate, census, checkpoint, analyzer, version or reference-data file was edited.

# Changelog

- 2026-11-07: Implemented canonical kit-file text and validation, consolidated shared rendering and versioned parsing, and authored the three prescribed tests for supervisor verification.
- 2026-11-07 (attempt 02): Added the missing `Softellect.Sys` import for the versioned parser's boundary annotation; preserved acceptance coverage and returned the repair for supervisor verification.
- 2026-11-07 (attempt 03): Replaced the parser's raw-string parameter with the existing `JsonText` and private source conversions to address ELEV010; preserved all tests and returned the repair for supervisor verification.
