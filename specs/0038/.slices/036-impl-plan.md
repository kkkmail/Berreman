# Step 036 — impl-plan (IMPLEMENT_CONTRACT STORE_XDUO_0006 ExperimentDataProxy)

## Goal

Bring the DECLARED `ExperimentDataProxy` (step 035, Domain
`ExperimentDataProxy.fs`, module `ExperimentData`) to `implemented` lifecycle by
supplying the ONE real file-read adapter this spec permits (spec-md §0.3c): a
thin `createFileBacked` in `OpticalConstructor.Storage` whose two proxy fields
read the data file's TEXT at the IO boundary and delegate to the step-34 pure
`MeasuredData` parsers. No parsing logic of its own; exceptions never cross into
Domain.

## Approach

1. **New Storage file `ExperimentDataStore.fs`** (module `ExperimentDataStore`):
   - A private `loadThrough (parse : string -> Result<'Series, ExperimentDataError>) (kind) (path : DataFilePath)`
     helper that reads the file text via `File.ReadAllText path.value` wrapped in
     `try/with` — any .NET IO exception (missing file, access failure) is caught
     AT this boundary and mapped to a typed `ExperimentDataError`, then the text
     is handed unchanged to `parse`. Mirrors `ProjectFile.openProject`'s
     `try Ok(File.ReadAllText path) with e -> Error(FileIoError e)` boundary.
   - `createFileBacked () : ExperimentDataProxy` wires the two fields to
     `loadThrough parseIntensitySeries` / `loadThrough parseEllipsometricSeries`.
   - Register it in `OpticalConstructor.Storage.fsproj` (before the `Storage.fs`
     anchor; it depends only on the Domain project reference).

2. **New test file `ExperimentDataStoreTests.fs`** in `OpticalConstructor.Tests`:
   - Round-trip a known intensity file and a known ellipsometric file written
     under the test output directory (`AppContext.BaseDirectory`), asserting the
     real store's parsed series equals the step-34 parser's own output.
   - A missing path yields a typed `ExperimentDataError` from BOTH fields (never
     a throw).
   - A delegation guard: an EMPTY (but existing) file yields the parser's
     `EmptyDataFile` — distinct from the IO-failure case — proving the adapter
     reads then delegates and adds no parsing logic.
   - Register after `ExperimentDataProxyTests.fs`.

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/ExperimentDataStore.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj` (register)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentDataStoreTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (register)

## Risks / decisions

- **Error case for IO failures.** The step `touches: [OpticalConstructor.Storage,
  OpticalConstructor.Tests]` — Domain is out of scope, so I do NOT grow the
  `ExperimentDataError` DU with a new missing-file case. IO exceptions map to the
  existing `MalformedDataFile` (reason names the path + underlying message) —
  the SAME case the step-035 mock already used for a missing file. The step-035
  SoW's "MAY grow the channel" is permissive, not required; keeping the four-case
  channel honours the touches list. Recorded in the impl-log Gotchas.
- **Test scratch location.** Round-trip files are ephemeral test scratch written
  under `AppContext.BaseDirectory` (the bin test-output tree), not `%TEMP%` and
  not the arc `.artifacts/` folder (which is for durable audit artifacts).
- **Generic helper + `--warnaserror+:25`.** `loadThrough` is a private generic
  combinator (`'Series`), its only match is exhaustive over `Result`, so no
  incomplete-match warning.
