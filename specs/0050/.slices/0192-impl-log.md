# Step 0192 — impl log

## Progress

- Read the protocol, the project prompt, the slice and the gate snapshot. Also read manual 013, whose arc-origin
  elevation scope applies from 0160 on. No other manual ruling names step 0192, and no operator note is in flight.
- Read step 0193's brief, which consumes this step. Its `DataLocations.rows` shows `tryFileOf`'s full path, or
  `In memory (<the connection string>)` on `None`, so `tryFileOf` must answer for every connection string and never
  throw.
- Grepped `Berreman/` and `Softellect/` for the new names. `DataLocationKind`, its seven cases and `tryFileOf` have
  no prior use, so the ELEV014/015/016/019 uniqueness rules hold.
- Wrote the two declared facts first. They were red by construction: they name `DataLocationKind` and `tryFileOf`,
  which did not exist yet.
- Implemented (1) and (2) of the slice. Built, then ran the development checks under Testing state.

## Files modified

- `OpticalConstructor.Domain/WorkbenchSettings.fs`, after `DataLocations` (step 81):
  - `DataLocationKind` = `LibraryDatabaseLocation` | `LibraryBackupsLocation` | `PreferencesLocation` |
    `KitsLocation` | `ExportsLocation` | `CataloguesLocation` | `ShippedCatalogueLocation`;
  - its doc is two lines citing spec 0050 S5.3 and saying it is never stored, so it has no tag;
  - each case has a one-line doc naming its field (`The location in \`libraryConnection\`.`, …);
  - `static member all : DataLocationKind list` lists the seven in the order of the record's fields.
- `OpticalConstructor.Storage/LibraryBackupStore.fs`, in `type LibraryDatabase with`, before `recreate` (step 87):
  - New `static member tryFileOf (connectionString : ConnectionString) : FileName option`. It reads the string
    through `SqliteConnectionStringBuilder` and touches no file. It answers `None` when `DataSource` is `:memory:`,
    when `Mode` is `Memory`, or when `DataSource` is empty. Otherwise it answers
    `Some (FileName (Path.GetFullPath DataSource))`.
  - A platform exception from the builder or `GetFullPath` (malformed text) is caught here and answers `None`.
  - Its doc is two lines citing spec 0050 S5.3. It carries a `StorageBoundary` mark for the caught platform calls.
  - `recreate` now matches `LibraryDatabase.tryFileOf connectionString`:
    - `Some file` when the file exists runs the copy, delete and sidecar deletion unchanged;
    - `Some _` and `None` copy nothing.

    Its own builder, in-memory test and `FileName builder.DataSource` read are deleted, so one rule says where a
    library's file is. No other member is added.
- `OpticalConstructor.Tests/LibraryDatabaseTests.fs` gains `tryFileOf answers the library's file, and nothing for an
  in-memory library`:
  - `Data Source=:memory:` and `Data Source=lib;Mode=Memory;Cache=Shared` answer `None`;
  - `Data Source=<fixture folder>/library.db` (a GUID folder under `AppContext.BaseDirectory`, written with a forward
    slash) answers `Some` of that file's full path;
  - the file is not created, and the fixture folder stays empty.

  `recreate`'s two facts (step 87) are unchanged, and every assertion is kept.
- `OpticalConstructor.Tests/WorkbenchSettingsTests.fs` (adds `open Microsoft.FSharp.Reflection`) gains `every location
  is a data location kind, in the order of its field`:
  - the record's field names, read by `FSharpType.GetRecordFields` in declaration order, equal the seven expected
    field names;
  - `DataLocationKind.all` equals the seven paired kinds in that same order;
  - the seven kinds are distinct.

## Testing state

These are development runs only. Under Invariant 6, the gate verdict belongs to the arc-runner's gate engine. Logs
are under `specs/0050/.artifacts/`.

- **Build:** `dotnet build Berreman.slnx -c Release` exits 0 with 0 errors (`0192-build.log`). The only warnings
  are the two `SYSLIB0051` warnings from vendored `MathNetNumerics/Numerics/Exceptions.cs`, which were already
  there. No owned-code warning.
- **The new facts and `recreate`'s two facts:** 4 passed (`0192-constructor-new-facts.log`).
- **constructor-unit-tests:** 1211 passed, 0 failed, 0 skipped (`0192-constructor-unit-tests.log`). That is
  checkpoint 190's 1209 + 2, the two declared facts. Step 191 left the count unchanged.
- **Elevation:** run with `ASG_ARC_BASE_COMMIT=f395597…` (`.arc-base-json`). Result: `ELEV-GATE: OK`, 0 new
  findings in 12 of 12 projects, 183 s, ELEV004 excluded, 0 known findings (`0192-elevation.log`).
- **The fourteen s21 invariant greps:** none finds anything.
- **Line endings:** no CR in any touched file. `git diff --numstat` matches `--ignore-cr-at-eol`.

## Artifacts

- `specs/0050/.artifacts/0192-build.log`
- `specs/0050/.artifacts/0192-constructor-new-facts.log`
- `specs/0050/.artifacts/0192-constructor-unit-tests.log`
- `specs/0050/.artifacts/0192-testresults/` (the constructor run's TRX)
- `specs/0050/.artifacts/0192-elevation.log`

## Gotchas

- **Interpretation: a connection string that names no file answers `None`.** The slice names `None` for an
  in-memory library and `Some` otherwise. Two more cases also answer `None`, because the signature is fixed at
  `FileName option` and a public member must not throw (AGENTS.md: errors are values):
  - malformed text, where the builder or `GetFullPath` throws;
  - an empty `DataSource`, which is SQLite's temporary database, not a file.

  For the empty `DataSource`, `recreate` behaves as before: it copied nothing (`File.Exists ""` is false) and opened
  through `openOrCreate`.
- **A malformed string's error case moves.** Before, `recreate`'s own builder threw and its outer catch answered
  `FileIoExn`. Now `tryFileOf` answers `None`, and `openOrCreate`'s builder throws and answers `LibraryDatabaseExn`.
  That is the same error `AppContext.create` gives for that string. Nothing is copied or deleted on either path.
  The only app caller, `AppContext.recreateLibrary`, passes the error through unread, and no fact asserts this case.
- **`openOrCreate` keeps its own `isFileDatabase` test** (`LibraryDatabaseStore.fs`). That file is compiled before
  `LibraryBackupStore.fs`, so `openOrCreate` cannot call `tryFileOf`, and the file is outside this step's authorized
  edits. The slice moves only `recreate`'s copy.
- **A relative `DataSource` resolves against the process's current directory** in `tryFileOf`, as SQLite and the old
  `File.Exists` check both do.
