# Step 0025 — IMPLEMENT — state of the world

## Where we are

Spec 0050, section S1.4: "Library entry files: envelope, provenance, canonical JSON, content hash,
schemas". Step 24 gave `OpticalConstructor.Domain/LibraryEntryFiles.fs` the entry vocabulary: kinds,
schema version, entry id, provenance and the file-name rule. This step adds the entry content
itself, in the same file (001 §B; .spec-md §5):
- the payload that holds what the envelope does not;
- the content record, which is the file without its hash, and the envelope that pairs it with its
  `ContentHash`;
- the builders and decoders that join the content to the six domain values.

The content hash (step 27) and the file writer and reader (step 30) build on these shapes.

## What's working

- Added `LibraryEntryPayload` (material complexity, sample, source, polarizer, detector kind,
  category visibility) with its `kind` and `currentSchema`.
- Added `LibraryEntryContent` (the entry file without its hash), its `fileName` (the name made safe,
  plus the kind's extension), and `LibraryEntryEnvelope` (content + `contentHash`).
- Added builders from each domain value to content. The preset and category builders refuse a
  non-GUID id or a provenance that disagrees with the value's protection or origin.
- Added `ElevationBoundary`-marked decoders back to each domain value. They answer
  `EntryKindMismatchErr` for another kind, and `MissingEntryFieldErr` for a material without a
  category or a sample without a description.
- Tightened `ElementValueId.tryToLibraryEntryId` to canonical GUID text, so builder ∘ decoder is the
  identity. A test now shows that an upper-case GUID preset id is refused (`EntryIdNotGuidErr`).
- Added five `LibraryEntryFilesTests` facts; the constructor suite is 751/751.

## Tests

The arc-runner's gate engine is the gate authority. These are local observations:

- `build`: exit 0, `0 Warning(s)`, `0 Error(s)` (`.artifacts/0025-build.log`).
- `constructor-unit-tests`, whole suite: 751 total, 751 succeeded, 0 failed, 0 skipped. That is 746
  plus the five new facts, each discovered under its slice id
  (`.artifacts/0025-constructor-unit-tests-whole.log`).
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 8 project(s)`, Domain plus its dependents, 0
  known findings (`.artifacts/0025-elevation-local.log`).
- The six s04 invariants: each grep finds nothing (re-run in attempt 02).
- Attempt 02 added one assertion to an existing fact, so the count stays at 751.

```yaml
gates:
  constructor_unit_tests: 751
```

## Architecture

- **Envelope, content and payload are three layers.**
  - The envelope keeps the hash apart from what it covers, so step 27 hashes `content` and nothing
    else.
  - The content carries the identity fields every kind shares: id, version, name, category,
    description and provenance.
  - The payload carries only the kind's own physics or behaviour.
  - Facets derive from the payload (`LibraryFacets`) and are never stored. A preset's protection and
    a category's origin derive from the provenance and are never stored.
- **One assembly point.** The private `LibraryEntryContent.ofPayload` builds every content record,
  and `LibraryEntryPayload.currentSchema` decides every schema, so no builder states a kind or a
  version by hand.
- **The file seam is the decoders.** Only the six decoders, plus the private `presetId` they share,
  turn the kind-neutral entry GUID into a kind's own id, and each carries the `StorageBoundary` mark.
  A measured probe showed ELEV001 returns without the mark. No public reverse conversion exists
  (ELEV006).
- **A preset id must be canonical.** `ElementValueId.tryToLibraryEntryId` accepts only the
  lower-case `D` text, the only text the decoder can reproduce.

## Deferred

Nothing. Every requirement of the slice landed this round: (1)–(5) and tests (a)–(e). The content
hash, the JSON schemas and the file reader and writer are later steps of S1.4 by the spec's own
plan, so they are not part of this step.

## Gotchas

- `LibraryEntryContent` adds the record labels `id`, `name`, `version` and `schema`. After it, an
  unannotated `{ id = …; name = … }` infers the last record with those labels. Annotate domain
  records in any file that opens `LibraryEntryFiles`.
- The decoders' `[<ElevationBoundary>]` marks are load-bearing (ELEV001); keep them.
- Run `Elevation/Run-ElevationGate.ps1` from a shell that is not sitting in `Elevation/bin/reports`;
  otherwise the driver crashes on a folder lock.
- `specs/0050/.manifest.state.json` shows as modified in the working tree. It is the supervisor's
  file, and this worker did not touch it.

## Changelog

- 2026-11-05 — Step 0025 (IMPLEMENT): `LibraryEntryFiles.fs` gains the entry payload, content
  (`fileName`), envelope and `LibraryEntryError`, `CategoryName.toEntryName`, six builders and six
  `StorageBoundary`-marked decoders. Preset ids must be canonical GUID text. Five new
  `LibraryEntryFilesTests` facts; the constructor suite is 751/751.
- 2026-11-05 — Step 0025 attempt 02: fact `a preset or category filed under a provenance that
  disagrees is refused` also asserts that an upper-case GUID preset id answers `EntryIdNotGuidErr`.
  This covers the tightened `tryToLibraryEntryId`. The suite is still 751/751.
