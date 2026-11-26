# Step 0192 — impl plan (IMPLEMENT)

## Approach

Step 0192 states what the Data locations window lists, as data. Step 0193 builds the window over it.

1. `OpticalConstructor.Domain/WorkbenchSettings.fs`: add `DataLocationKind` after `DataLocations` (step 81). It has
   seven cases, one per field of `DataLocations`, in field order:
   - `LibraryDatabaseLocation`, `LibraryBackupsLocation` and `PreferencesLocation`;
   - `KitsLocation`, `ExportsLocation` and `CataloguesLocation`;
   - `ShippedCatalogueLocation`.

   Each case has a one-line doc naming its field. The kind is never stored, so it has no `value` tag. Its
   `static member all : DataLocationKind list` lists the seven in that order.
2. `OpticalConstructor.Storage/LibraryBackupStore.fs`: add `LibraryDatabase.tryFileOf (connectionString :
   ConnectionString) : FileName option` beside `recreate`. It reads the connection string through
   `SqliteConnectionStringBuilder` and answers:
   - `None` for an in-memory library (`DataSource` `:memory:` or `Mode` `Memory`);
   - otherwise `Some` of `Path.GetFullPath DataSource`.

   This is `recreate`'s in-memory test and file read, moved. `recreate` now matches on `tryFileOf`, and its own copy
   is deleted. `tryFileOf` touches no file. No other member is added.
3. Tests, both in existing classes:
   - `LibraryDatabaseTests` gains `tryFileOf answers the library's file, and nothing for an in-memory library`.
     `:memory:` and the named shared-memory string answer `None`. `Data Source=<fixture folder>/library.db` answers
     that file's full path, and no file is created.
   - `WorkbenchSettingsTests` gains `every location is a data location kind, in the order of its field`.
     `DataLocationKind.all` holds the seven cases once each, in order, and its kinds pair one-to-one with the
     record's fields, read by reflection in declaration order.

   `recreate`'s two existing facts keep every assertion. They now cover the moved rule through `recreate`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/WorkbenchSettings.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/LibraryBackupStore.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryDatabaseTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/WorkbenchSettingsTests.fs`

## Risks

- **Errors as values.** `SqliteConnectionStringBuilder` and `Path.GetFullPath` throw on malformed text. A public
  member must not throw, and the signature is fixed at `FileName option`. So:
  - the platform exception is caught at this boundary and answers `None` (a connection string that names no file);
  - an empty `DataSource` (SQLite's temporary database) also answers `None`.

  Both keep `recreate`'s old behaviour for those strings: it copied nothing and handed them to `openOrCreate`, which
  answers its own typed error. This is recorded as an interpretation in the impl-log.
- **`openOrCreate` keeps its own `isFileDatabase` test.** It is compiled earlier (`LibraryDatabaseStore.fs`), so it
  cannot call a member declared in `LibraryBackupStore.fs`. The slice moves only `recreate`'s copy, and that file is
  outside the authorized edits.
- **ELEV uniqueness.** A grep over `Berreman/` and `Softellect/` finds no prior use of the new type, case or member
  names.
