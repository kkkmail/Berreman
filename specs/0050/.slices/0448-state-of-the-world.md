# Where we are

Step 0448 is S13.1's end-to-end proof of the pulls (obligation S131-PULLS-SQLITE). The earlier steps built what it
drives:

- step 443 declared the `MaterialSourceProxy` seam;
- step 444 added the pulls (`pullRecord`, `updateSourceSnapshot`) and proved them over the in-memory double;
- step 447 added the real refractiveindex.info adapter.

This step proves them over the real adapter and the real library database, with SQLite `:memory:` and a fixture
snapshot under the test output, as .spec-md §9 asks of each library proxy's suite. It is tests only; no product
file changed.

# What's working

- Added `MaterialSourcePullSqliteTests`: four end-to-end facts running the pulls over the real refractiveindex.info
  adapter and the SQLite library database (`:memory:`).
- Proved that a pulled page is a user material at version 1 with its pull provenance, its `PulledSources` row and
  its CC0 attribution line.
- Proved that an update of an unchanged page mints nothing, and an update of a changed page mints version 2 and
  keeps version 1.
- Proved that pulling the same page twice writes it once.

# Tests

These are development checks; the gate engine runs the step's roster after this session and is the authority.

- **Build of the test project:** exit 0, 25 s against 600 s, with 0 warnings and 0 errors. The first try failed
  with FS0039 ×3 (`MaterialError` has no `describe`) and was fixed in the test file.
- **`OpticalConstructor.Tests.MaterialSourcePullSqliteTests`, filtered:** `Passed!`, 4 of 4, in 4.9 s against 180 s:
  - `a pulled refractiveindex.info page is a user material at version 1 with its provenance and its pulled-source row`;
  - `an update whose page is unchanged mints no version`;
  - `an update whose page changed mints version 2 and keeps version 1`;
  - `pulling the same page twice writes it once`.
- **The twenty-one s43 invariants:** each decoded `git grep` finds nothing in the tracked tree. The same patterns,
  run over the new, untracked file and the edited project file, find nothing too.
- **Not run here:** elevation, the whole-solution build and the whole constructor-unit-tests suite. Under
  Invariant 6 those are the gate engine's. The test project is outside elevation's scope.
- **The expected count** is 1795: step 447's 1791 plus these four facts. It is arithmetic, not a measurement here.
- No development run exceeded its gate's effective deadline.

```yaml
gates:
  constructor_unit_tests: 1795
```

# Architecture

- **The facts compose the real seams exactly as the brief names them.** Each fact:
  - opens `BuiltinCatalogue.freshLibraryDatabase ()` and disposes it with `use`;
  - composes `CatalogueImportContext.create (CatalogueImportProxy.createInDatabase database)` and
    `MaterialProxy.createInDatabase database`;
  - builds the adapter as `MaterialSourceProxy.createRefractiveIndexInfo` over `MaterialSourceFixture`'s fresh
    sources folder, a `writeSnapshot` of `siPage ()`, a `copyingDownloader` of a `writeArchive` zip beside it and
    `fixedClock` of 2026-11-04.

  Nothing reaches the network.
- **The pulled entry's id is fixed.** The context's `newEntryId` is set to one fixed GUID, as in
  `MaterialSourcePullTests`, so every expected pull result is exact.
- **`PulledSources` is read through `LibraryDatabase.run`.** Each row is mapped at that read into elevated values
  (`LibraryVersionRef`, source, record, version, licence, citation) and compared structurally, in version order.
- No product file, stored format, schema, package or gate changed. The R0001–R0005 behaviour is untouched.

# Deferred

Nothing.

# Gotchas

- **`MaterialError` has no `describe`.** A test that renders one matches its cases for the reason, as this file's
  private `materialRefusal` and `BuiltinCatalogue.materialStore` do.
- **The installed snapshot has 3 files:** the catalog and the two pages of `writeArchive`'s archive. A fact that
  expects a whole `SourceRefreshed` must say so.
- **The literals must use the fixtures' exact code points:**
  - in the name, `–` is U+2013 and `µ` is U+00B5;
  - in the citation, the em dash is U+2014.
- **Materialise rows inside `run`.** The `LibraryDbContext` is disposed when `run` returns.

# Changelog

- 2026-11-16, step 0448: added `MaterialSourcePullSqliteTests`, four end-to-end facts of the refractiveindex.info
  pull and update over the real adapter and the SQLite library database; tests only. The expected
  constructor-unit-tests count goes from 1791 to 1795.
