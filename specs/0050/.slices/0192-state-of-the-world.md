# Step 0192 — state of the world

# Where we are

Step 0192 (`IMPLEMENT`) is part of section S5.3, the Data locations work (001 §G: "a Launcher Data locations panel
shows them, each with Open folder"). It states what that window lists, as data:
- `DataLocationKind` names each of the seven locations in `DataLocations`, and `DataLocationKind.all` lists them in
  field order;
- `LibraryDatabase.tryFileOf` is the one rule for where a library's file is.

Step 0193 builds the Data locations screen's rows and view over both.

# What's working

- Add `DataLocationKind` (seven cases, one per field of `DataLocations`) and `DataLocationKind.all`, in field order.
- Add `LibraryDatabase.tryFileOf`. It answers a library's full file path from its connection string, or `None` for
  an in-memory library, and touches no file.
- `recreate` now calls `tryFileOf`, and its own in-memory test and file read are deleted.
- Add the constructor facts `tryFileOf answers the library's file, and nothing for an in-memory library` and `every
  location is a data location kind, in the order of its field`.

# Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster:

- build and elevation;
- constructor-unit-tests (all discovered);
- the fourteen s21 invariants.

The worker's development runs are in the impl-log's `Testing state`, with logs under
`specs/0050/.artifacts/0192-*.log`:

- build: 0 errors, no owned-code warning;
- the two new facts and `recreate`'s two facts: 4 passed;
- constructor-unit-tests: 1211 passed, 0 failed, 0 skipped;
- elevation from the arc origin: `ELEV-GATE: OK`, 0 new findings in 12 of 12 projects;
- the fourteen s21 invariant greps: none finds anything.

Expected count: constructor-unit-tests +2 (1209 → 1211). No test is removed or renamed, and no assertion is
weakened. The YAML carries the latest recorded checkpoint's floors (checkpoint 190); the supervisor measures this
step's.

```yaml
gates:
  constructor_unit_tests: 1209
  ui_smoke_tests: 232
```

# Architecture

- **One rule for a library's file.** `LibraryDatabase.tryFileOf` (Storage, `LibraryBackupStore.fs`) owns the
  in-memory test and the file read. `recreate` matches on it, and step 0193's Data locations rows will read it.
  `openOrCreate` keeps its own `isFileDatabase` test: it is compiled earlier, in `LibraryDatabaseStore.fs`, so it
  cannot call `tryFileOf`.
- **`DataLocationKind` is in-memory only.** It is never stored or sent, so it has no `value` tag and no
  `tryOfWire`. Its order is the record's field order, which a fact pins by reflection.
- **`tryFileOf` never throws.** Malformed text and an empty `DataSource` name no file and answer `None`. The platform
  exception is caught at this `StorageBoundary`-marked member.

# Deferred

Nothing. Every requirement of the slice is in this round.

# Gotchas

- A malformed connection string given to `recreate` now fails in `openOrCreate`, with `LibraryDatabaseExn`, instead
  of in `recreate`'s old builder, with `FileIoExn`. That matches `AppContext.create`. Nothing is copied or deleted
  either way, and the only app caller passes the error through unread.
- A relative `DataSource` resolves against the process's current directory, as SQLite and the old `File.Exists`
  check do.
- `DataLocationKind`'s order is the contract for step 0193's rows. Adding a field to `DataLocations` means adding a
  case in the same position. The reflection fact fails until both agree.

# Changelog

- 2026-11-09 — attempt 01:
  - `DataLocationKind` with `all`;
  - `LibraryDatabase.tryFileOf`, with `recreate` calling it;
  - the two declared constructor facts.
