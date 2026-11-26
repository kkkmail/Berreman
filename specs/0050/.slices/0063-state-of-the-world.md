# Step 0063 state of the world

## Where we are

Slice 0063 implements S21-ROW-MAPPING on the existing S2.1 database handle
and lifecycle tags. Stored versions now map through the existing entry-file
format, with typed rejection of unsupported row data. The implementation and
both declared tests are written; acceptance belongs to the supervisor's gate
engine under the IMPLEMENT worker protocol.

## What's working

- Added stored entry versions with typed lifecycle and pin state.
- Mapped database payloads through the canonical entry-file writer and parser.
- Rejected unknown tags and mismatched columns with diagnostic storage errors.
- Added catalogue-wide byte comparisons and row-corruption regression coverage.

## Tests

No build, test, analyzer or invariant gates were executed by this worker.
The supervisor must run the resolved 0063.gates roster: build, elevation,
constructor-unit-tests and the nine s08 invariants. No pass or new test count
is claimed, and commit readiness is not asserted before verification.

Both declared LibraryDatabaseTests facts are present. The round trip uses
SQLite :memory:, two separate run contexts, alternating lifecycle/pin states,
every shipped envelope, all library kinds, and File.ReadAllBytes comparisons
against Catalogue/Builtin under AppContext.BaseDirectory. The rejection fact
covers row metadata, all three unknown tags before parsing, first-column
reporting, noncanonical text, tampered content and unsupported schema versions.

This YAML copies the supervisor's latest available baseline from checkpoint
61; it is neither authored nor newly measured by this worker:

```yaml
gates:
  constructor_unit_tests: 801
```

## Architecture

EntryVersionState holds only EntryLifecycle and PinState. StoredEntryVersion
holds that state beside LibraryEntryEnvelope. EntryProvenance projects its
query tag and optional built-in catalogue id; the complete provenance is
always read from the file payload.

Both mapping members carry StorageBoundary. Writes reuse envelope.fileText
without rewriting its text. Reads decode kind/lifecycle/pin, parse through
LibraryFileText.parseEntry, then regenerate a versionRow and compare all eleven
columns in declaration order. Parser failures, including unsupported schema
versions and content hash mismatches, propagate unchanged. The first differing
column yields LibraryRowInvalidErr with entry identity, version and column;
an unknown tag also names its value.

No durable shape, schema version, migration, serializer or cache is added.
The existing EF row and file contract are reused without format changes.

## Deferred

No slice implementation is deferred. Gate execution is assigned to the
supervisor by the worker protocol.

## Gotchas

- PayloadText itself must equal the canonical file text, even when alternative
  JSON whitespace would preserve the content hash.
- Lifecycle and pin belong to the stored version state; protection, provenance
  kind and catalogue id derive from its payload.
- The existing dirty specs/0050/.manifest.state.json belongs to the supervisor
  and was left untouched.
- No gates, oracle, checkpoints, analyzer configuration, manual notes or version
  files were edited; edited source diff sizes show no CRLF churn.

## Changelog

- 2026-11-06: Implemented entry-version row mapping, diagnostic row rejection
  and both declared facts; handed verification to the supervisor.
