# Where we are

Step 0443 opens section S13.1 (`MaterialSourceProxy` and refractiveindex.info) by declaring contract
`SVC_BigBoy_0002` at `declared` lifecycle. It adds the online-source seam in Domain: its error, query,
hit, fetched record, snapshot progress, observer and outcome, the five-field proxy, the in-memory
double, and seven facts that pin the surface. Later steps build on it:

- step 444 writes what a pull produces and carries the sources in the import context;
- step 447 is the real refractiveindex.info adapter;
- S13.2's PTB and Henke sources are further `MaterialSourceProxy` adapters.

# What's working

- Added `OpticalConstructor.Domain/MaterialSources.fs`, compiled directly before `CatalogueImport.fs`.
  It holds `MaterialSourceProxy` with exactly `sourceName`, `snapshotVersion`, `searchRecords`,
  `fetchRecord` and `updateSnapshot`.
- Added `MaterialSourceQuery.filter`, the one search rule for every adapter: every white-space word
  must appear in the hit's name or record, ignoring case; a blank query keeps every hit.
- Added `MaterialSourceError`: seven `*Err` cases, each with a reason and a `describe` text.
- Added the snapshot types: `SourceSnapshotObserver` (with `unobserved`), `SourceSnapshotProgress`,
  `SourceSnapshotInfo` and `SourceSnapshotOutcome`.
- Added the double, `InMemoryMaterialSource` with `MaterialSourceProxy.createInMemory`. An update
  reads the cancellation before each record and reports after each, and a cancelled update keeps the
  held snapshot.
- Added `MaterialSourceProxyTests` with the census's seven facts.

# Tests

These are development runs with each gate's own command and selection. The supervisor's gate engine
re-runs the roster and is the authority.

- **build:** exit 0, 85 s against a 600 s deadline. The only warnings are the exempt NuGet advisories
  and the vendored MathNet's existing SYSLIB0051.
- **elevation** (ELEV004 excluded, all 13 projects, no comparison base):
  `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 234 s against a 600 s deadline. Domain and every
  downstream project have zero findings.
- **constructor-unit-tests** (`all_discovered`, the whole suite as the gate selects it): `Passed!`,
  1772 of 1772, 112 s against a 180 s deadline.
  - The count rises by 7 from checkpoint 441's 1765, as the acceptance requires.
  - TRX: all seven new `OpticalConstructor.Tests.MaterialSourceProxyTests` identities passed.
- **The twenty-one s43 invariants:** each decoded `git grep` finds nothing. A direct scan of the two
  new, still untracked files for the same patterns also finds nothing.
- No development run exceeded its gate's effective deadline.

```yaml
gates:
  constructor_unit_tests: 1772
```

# Architecture

- **One proxy is one adapter.** `MaterialSourceProxy` is a `[<ReferenceEquality>]` record of
  pre-curried, `Result`-returning functions plus `sourceName`, which is data.
  - It does IO only; what a pull writes stays with the import (step 444).
  - The adapters to come (refractiveindex.info at step 447, then PTB and Henke) each construct one.
- **The search rule lives on the query, not in the adapters.** `MaterialSourceQuery.filter` is the only
  matching rule. Each adapter feeds its held catalog's hits through it, so every source searches alike.
- **Progress and cancellation follow the archive jobs.**
  - `SourceSnapshotObserver` copies `KitArchiveObserver`'s shape: report after each file, read the
    token before each.
  - `SourceSnapshotOutcome` copies `KitArchiveOutcome`'s: updated, or cancelled with nothing changed.
  - The UI can therefore host a snapshot update the same way it hosts a kit archive job (.spec-md §10.2).
- **The double follows `InMemoryArchiveDisk`.**
  - `InMemoryMaterialSource` is a sealed class with a private constructor and its state behind a lock.
  - Its state is reached only through its own members, some internal; the proxy's closures call those
    members.
  - A test publishes a version and records with `publish`; the next `updateSnapshot` installs them.
- The contract registry already listed `SVC_BigBoy_0002` as `declared` at step 443, so it was not
  edited.

# Deferred

Nothing.

# Gotchas

- **No `open System`.** The how-to names exactly three opens (`MaterialLibrary`, `LibraryEntryFiles`,
  `System.Threading`), so `filter` writes `System.StringSplitOptions` and `System.StringComparison` in
  full.
- **`git grep` cannot see the new files until they are committed.** The invariant gates search tracked
  files only. Both new files were scanned directly for the same patterns and hold none of them.
- **A cancelled update needs a later record to stop before.** The fact cancels inside the first
  progress report. The second record's check then stops the update, so it publishes two records.
- **The double keeps what it published after an update.** Updating again re-installs the same
  snapshot, as a real source still serving that version would.
- **The `SourceSnapshotUpdated` case names its field `snapshot`,** following `KitArchiveFinished of files`.
  It changes no construction or match site.

# Changelog

- 2026-11-16, step 0443:
  - declared `SVC_BigBoy_0002` `MaterialSourceProxy`, with its errors, query rule, snapshot types and
    in-memory double, in `MaterialSources.fs`;
  - added `MaterialSourceProxyTests` (seven facts); constructor-unit-tests went from 1765 to 1772.
