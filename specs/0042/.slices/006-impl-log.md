# 006 impl-log — IMPLEMENT_CONTRACT STORE_XDUO_0009 MuellerDataProxy

## Progress

- [x] Add `MuellerDataStore.fs` (pure `parseMuellerCsv` + file-backed `createFileBacked`)
- [x] Wire it into `OpticalConstructor.Storage.fsproj` compile order (after `ExperimentDataStore.fs`)
- [x] Add `OpticalConstructor.Storage` ProjectReference to `BerremanTests.fsproj`
- [x] Add BerremanTests facts driving the real `parseMuellerCsv` + `createFileBacked`

## Files modified

- **ADD** `Berreman/OpticalConstructor/OpticalConstructor.Storage/MuellerDataStore.fs`
  — `module MuellerDataStore`: the pure `parseMuellerCsv` and the file-backed
  `createFileBacked` behind the step-005 DECLARED `MuellerDataProxy`.
- **EDIT** `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`
  — added `<Compile Include="MuellerDataStore.fs" />` between `ExperimentDataStore.fs`
  and the `Storage.fs` anchor.
- **EDIT** `Berreman/BerremanTests/BerremanTests.fsproj` — added the explicit
  `ProjectReference` to `OpticalConstructor.Storage`.
- **EDIT** `Berreman/BerremanTests/MuellerReconstructionTests.fs` — `open
  OpticalConstructor.Storage`; two committed CSV-text fixtures (a BOM-prefixed
  multi-column capture family; a malformed `capture_index` row) bound as class-level
  `let`s ahead of the members; three new facts driving the REAL parser/store.

## Decisions made

- **`parseMuellerCsv` reuses `CsvFile.Parse` exactly as `SpectralImport.parseSpectrumCsv`**
  (SpectralImport.fs:46) — parse TEXT, read `csv.Headers` / `csv.Rows` / `r.Columns`.
  Column access is BY NAME via a trimmed header→index map, so the file's many unread
  columns (`iso`, `exposure_ns`, `avg_R/G/B`, …) and any re-ordering are tolerated.
- **BOM-tolerance** = strip a leading U+FEFF before `Parse`. A UTF-8 BOM decodes to a
  single U+FEFF, which `String.Trim()` does NOT strip on .NET Core; without stripping,
  the first header reads `<BOM>experiment` and never matches by name. Wrote the char as
  `char 0xFEFF` (no fragile literal-BOM byte in the source).
- **Invariant-culture parsing** of every cell: `Int32.TryParse` / `Double.TryParse` /
  `DateTimeOffset.TryParse` with `CultureInfo.InvariantCulture`. `description` (the raw
  analyzer dial angle in degrees) is elevated to the engine `Angle` via `Angle.degree`.
- **`captured_at` parse style**: `DateTimeStyles.AssumeUniversal` — an offset-bearing
  ISO-8601 string keeps its offset; an offset-less one is assumed UTC (deterministic,
  no local-timezone dependence). `RoundtripKind` is unsupported for `DateTimeOffset`,
  so it was avoided.
- **Typed-error mapping.** A missing required column / short row / unparseable cell →
  `MalformedRow reason`; a header with no capture rows (or empty text) → `EmptyFile
  reason`. The whole parse body is under `try/with`, so no exception crosses the
  boundary. `createFileBacked`'s `loadThrough` catches any `File.ReadAllText` IO
  exception and maps it to `MalformedRow` — WITHIN the DECLARED two-case channel, whose
  `MalformedRow` doc explicitly covers "a native/IO failure mapped onto the channel".
  This mirrors `ExperimentDataStore.loadThrough` / `createFileBacked` (ExperimentDataStore.fs:34, :51).
- **No `.contract-ids/XDUO-json` edit.** That registry tracks contract IDENTITY
  (`status: active`); the prior IMPLEMENT_CONTRACT (step 004, SVC_XDUO_0002) left it
  untouched (still `active`), so the declared→implemented lifecycle is arc-runner
  step-state, not a worker edit.

## Testing state

Per the IMPLEMENT_CONTRACT worker's **Invariant 6 (the worker acts; it runs no
checks)**, this worker did NOT execute the `build` / `unit-tests` /
`constructor-unit-tests` gates — the deterministic gate engine runs them after exit.
Code was reviewed by hand for compile-correctness and zero warnings (matches complete
→ no FS0025; `--warnaserror+:25` clean; all `open`s used).

New coverage in `BerremanTests/MuellerReconstructionTests.fs`, all driving the REAL
`MuellerDataStore` (not a mock):

- **committed multi-column fixture** (the acceptance) — a BOM-prefixed 10-column CSV
  parses through `parseMuellerCsv` into two `MuellerRawRow`s; asserts `experiment`,
  `captureIndex`, `capturedAt`, `avgTotal`, and `description` elevated to `Angle`
  (read back via `.degrees`);
- **malformed file → typed error** — a non-integer `capture_index` yields
  `Error (MalformedRow _)` with a non-empty reason — never a throw;
- **`createFileBacked` IO boundary** — the real proxy's `tryLoadFamily` (bound to an
  explicitly-typed local pinning the signature) maps a missing file to a typed
  `MuellerDataError` — never a throw.

The three facts only ADD to the `berreman_unit_tests` count (a `count_at_least` gate);
no other project's tests changed. `commit_ready: true`.

## Artifacts

None (no captured logs/traces this round — a pure code + test slice).
