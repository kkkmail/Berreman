# Where we are

Step 0138 implements S42-SNAPSHOT-FILES: immutable library-version snapshots in a kit's `library/` folder. The five authorized source files and the prescribed worker documents are complete. Supervisor verification is pending.

# What's working

- Name snapshots by lowercase entry GUID, invariant version and kind extension under library.
- Add immutable snapshot writes, ordered validated reads and read-only refusal to both kit proxies.
- Reuse canonical UTF-8 serialization and atomic non-overwriting publication for filesystem snapshots.
- Reject invalid or misnamed snapshots with their relative filename and diagnostic reason.
- Add the ten authorized test identities and extend the existing kit-error rendering coverage.

# Tests

No build, test, analyzer or invariant gate was run, per IMPLEMENT Invariant 6. No passing result or newly measured count is claimed. The supervisor owns build, elevation, all-discovered constructor-unit-tests and the eleven s16 invariants in `0138.gates`.

The shared contract covers Silicon and the quarter-wave sample, ordinal listing, reopening, versions 2 and 10, duplicate and divergent writes, unchanged paths and read-only access. It also exercises schema-kind, schema-version and hash validation against both implementations. Filesystem coverage includes exact catalogue bytes, missing library creation, UTF-8 BOM removal, case-insensitive filename matching, malformed/schema/version/hash refusals, wrong identity/version/kind names, and ignored non-entry and nested files.

Exactly ten discovered tests are added: one domain fact, three inherited facts in each of the two concrete proxy classes, and three filesystem-only facts. The expected new total is 1,106, not measured. This YAML repeats only the existing supervisor baseline from checkpoint 136; no checkpoint or census was edited.

```yaml
gates:
  constructor_unit_tests: 1096
```

# Architecture

`LibraryEntryEnvelope.snapshotFileName` and `snapshotPath` centralize naming. `KitProxy` gains only `listSnapshots` and `writeSnapshot`; `readOnlyView` passes the former through and refuses the latter. `InMemoryKitFolder` stores envelopes by filename and includes their relative paths in its existing path view.

Storage supplies the in-memory reader through the existing injection seam so both implementations use `LibraryFileText.parseEntry` and the same filename validation. Filesystem listing decodes through the existing UTF-8 byte helper and ignores files without a library-entry extension. Writing uses `fileText` and `writeNewBytesAtomically`, preserving canonical bytes and refusing replacement, including when another writer publishes first. No new durable format, migration, fallback reader or cache is introduced.

# Deferred

None of the requested implementation is deferred. Executable verification belongs to the supervisor under the worker protocol.

# Gotchas

- The existing supervisor edit to `specs/0050/.manifest.state.json` remains untouched.
- The shared lock guard from `.manual/009` and the atomic byte-writer implementation remain unchanged.
- Source review is not a passing build, test or analyzer result; the exit summary does not claim commit readiness.
- The test census, oracle, gates, checkpoints, version files, git history and index were not mutated by this worker.
- The five source files and three handoff documents use LF endings; normal and ignore-CR-at-EOL source diff counts agree.

# Changelog

2026-11-07 — Implemented immutable kit snapshots, canonical filesystem publication, shared validated listing, read-only routing and the ten authorized regression cases.
