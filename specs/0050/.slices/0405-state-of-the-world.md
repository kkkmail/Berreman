# Step 0405 — state of the world

## Where we are

Spec 0050 S11.5 (publishing, the study view) gives a promoted library entry a provenance that says where it was
fitted. None of the five cases fit: `UserProvenance` carries nothing, and `KitImportProvenance` names only the kit.
So `EntryProvenance` gains a sixth case, `FitProvenance of EntryFitInfo`. It names three things:

- the kit;
- the study's file stem;
- the fit run's file stem.

The stems name the `.study-json` and `.fitrun-json` that reproduce the fitted values (001 §I). The case is stored,
written and read, but nothing builds it yet: step 407's Promote to library does.

## What's working

- Added `FitProvenance of EntryFitInfo` (`kit : KitId`, `studyStem : FileStem`, `fitRunStem : FileStem`) to
  `EntryProvenance`. It is user-managed, has a user category and carries no attribution.
- The library database stores a fitted entry under provenance kind 6, with no catalogue id.
- Added the `fitProvenance` shape (kit GUID, two non-empty stems) to `library-common.schema.json`, still at
  version 1.
- Widened three facts to cover a fit: the protection / category fact, the attribution fact and the entry-file
  round trip. Constructor tests stay at 1726.

## Tests

Development runs on 2026-11-14. These are not gate verdicts; the supervisor re-runs every gate. Every run finished
within its effective deadline.

| Gate | Development outcome | Duration / deadline |
|---|---|---|
| build | exit 0, 0 errors, only the exempt `NU1902` advisories (plus vendored MathNet's recorded `SYSLIB0051` on recompile) | 81 s first, 25 s final / 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` (no base: all 13), known findings 0 | 224 s / 600 s |
| constructor-unit-tests | 1726/1726 passed, whole suite (count unchanged) | 156 s, 159 s final / 180 s |
| invariant-s40-* (20) | each ran its own command: `INVARIANT HOLDS ... found nothing` | < 1 s each / 300 s |

The three widened facts passed:

- `a built-in provenance is protected and every other provenance is user-managed`
- `a pulled entry and its forks carry its licence's attribution, and every other provenance none`
- `every provenance round-trips through an entry file`

A mutation check made three mutants in one build:

- a fit answering `ProtectedBuiltIn`;
- a fit answering an attribution;
- `fitProvenance` dropped from the schema's `oneOf`.

Exactly those three facts went red, and the round trip failed with `SchemaValidationErr`. The sources were restored
byte-identical (sha256-checked), and the Tests project was recompiled before the final build and suite.

```yaml
gates:
  constructor_unit_tests: 1726
```

## Architecture

- **One more provenance case, user-owned.**
  - `FitProvenance` falls in with `UserProvenance` in every complete match: `protection`, `categoryOrigin` and
    the store's `catalogueIdOpt`.
  - `attributionOpt` answers `None`, because a fit carries no licence.
  - `kindValue` gives it the next tag, 6.
  - No other match on `EntryProvenance` lists its cases, so FS0025 reported nothing else.
- **The study and fit run are named by file stem, not by `StudyName` / `FitRunName`.**
  - Those types are declared after `LibraryEntryFiles.fs` (steps 214, 358). Their `fileStem` members are how a kit
    names their files, so the provenance stores the stems and nothing is moved across files.
- **`FileStem` moved above `EntryProvenance` in its own file** (impl-log D1).
  - The record cannot name a type declared later.
  - The type moved unchanged; it depends only on `Softellect.Sys` primitives.
- **Schema, no migration.**
  - `$defs/fitProvenance` is the serializer's case form (`Case` / `Fields`, one object, keys `kit`, `studyStem`,
    `fitRunStem`). The single-case `KitId` / `FileStem` are written unwrapped.
  - The entry schemas keep version 1: no entry file has been released (.spec-md §7).

## Deferred

Nothing. The step's brief is complete. Building the case on Promote to library is step 407's own brief, not
deferred work.

## Gotchas

- `FileStem` now sits above `EntryProvenance` in `LibraryEntryFiles.fs`. A type the provenance group names must be
  declared above the group.
- The step authorizes no `LibraryDatabaseTests` identity, so nothing asserts kind 6 / no catalogue directly. That
  row is first written by step 407's promotion.
- The constructor suite now runs 159 s of its 180 s deadline. This is close, but unchanged in shape from step 0404
  (154 s), whose critical path is `StudyEngineTests`.

## Changelog

- 2026-11-14 — Step 0405: `EntryProvenance.FitProvenance of EntryFitInfo` (kit, study stem, fit-run stem).
  - It is user-managed, with no attribution, stored as kind 6 with no catalogue.
  - `library-common.schema.json` gained `fitProvenance`.
  - Three facts widened; constructor tests stay at 1726.
