# State of the world — spec 0042, slice 006 (IMPLEMENT_CONTRACT STORE_XDUO_0009 MuellerDataProxy)

## Where we are

Slice 006 is the sixth slice of arc 0042. It is the real construction behind the
Mueller measured-data CSV-**LOAD** seam that slice 005 *declared*
(`STORE_XDUO_0009`, `MuellerDataProxy`). Step 005 shipped the DECLARED surface —
the elevated `MuellerRawRow` row DTO, the typed `MuellerDataError` channel, the
`[<ReferenceEquality>] MuellerDataProxy` — plus an in-memory mock. This slice fills
the missing `path -> read CSV -> rows` EDGE with a real file-backed adapter in
`OpticalConstructor.Storage`, flipping the contract to `implemented`. Each loaded
row carries one capture's analyzer azimuth (`description`, the engine `Angle`) and
averaged intensity (`avgTotal`) — the exact (azimuth, intensity) pairs `kron4` and
the step-004 `MuellerSolverProxy` consume for the reconstruction.

## What's working

- Add `OpticalConstructor.Storage/MuellerDataStore.fs` with the PURE
  `parseMuellerCsv` — header-aware, BOM-tolerant, invariant-culture — reusing
  `FSharp.Data CsvFile.Parse` exactly as `SpectralImport.parseSpectrumCsv`.
- Read the `experiment`, `capture_index`, `captured_at`, `description`, `avg_total`
  columns BY NAME, elevating `description` to the engine `Angle` via `Angle.degree`.
- Map a missing column / short row / unparseable cell to `MalformedRow`, and a
  header with no capture rows to `EmptyFile` — no exception crosses into the Domain.
- Add the file-backed `createFileBacked ()` whose `tryLoadFamily` reads the file
  text under try/with and delegates to `parseMuellerCsv`, mirroring
  `ExperimentDataStore.loadThrough`/`createFileBacked`.
- Reference `OpticalConstructor.Storage` from `BerremanTests` and add three facts
  driving the REAL parser/store (committed multi-column fixture, malformed file,
  missing-file IO boundary).

## Tests

Gate roster for this slice (from `006.gates`): `build`, `unit-tests`
(`berreman_unit_tests`), `constructor-unit-tests` (`constructor_unit_tests`).

Per the IMPLEMENT_CONTRACT worker's **Invariant 6 (the worker acts; it runs no
checks)**, this worker did NOT execute the gates — the deterministic gate engine
runs them after exit. New coverage in `BerremanTests/MuellerReconstructionTests.fs`,
all driving the REAL `MuellerDataStore`:

- **committed multi-column fixture** (the acceptance) — a BOM-prefixed 10-column CSV
  parses through `parseMuellerCsv` into two `MuellerRawRow`s; asserts every field
  and `description` elevated to `Angle`;
- **malformed file → typed error** — a non-integer `capture_index` returns
  `Error (MalformedRow _)` with a non-empty reason — never a throw;
- **`createFileBacked` IO boundary** — the real proxy's `tryLoadFamily` maps a
  missing file to a typed `MuellerDataError` — never a throw.

The three facts only ADD to the `berreman_unit_tests` count (a `count_at_least`
gate); no other project's tests changed.

```yaml
gates:
  berreman_unit_tests: 0
  constructor_unit_tests: 0
```

(The gate engine records the real captured counts post-exit; the baseline for a
`count_at_least` gate is the prior green step's checkpoint, not a value the worker
authors — 0 is the safe floor. The three new facts only add to the count.)

## Architecture

- **The real adapter adds NO parsing to the Domain.** The pure parser and the disk
  read both live in `OpticalConstructor.Storage` (the storage/IO seam); the Domain
  keeps only the declared types. `createFileBacked` is a drop-in for the step-005
  mock — every consumer that holds a `MuellerDataProxy` is unchanged.
- **The parser mirrors the `SpectralImport` precedent.** `CsvFile.Parse` on the file
  TEXT, `csv.Headers` / `csv.Rows` / `r.Columns` — but column access is BY NAME (a
  trimmed header→index map), so the capture file's many unread columns and any
  re-ordering are tolerated. The `string` argument keeps filesystem IO out of the
  parser (it never throws past the boundary).
- **The IO edge mirrors `ExperimentDataStore`.** `loadThrough parseMuellerCsv` wraps
  `File.ReadAllText` in try/with and maps any .NET IO exception to a typed
  `MalformedRow` — the DECLARED channel's `MalformedRow` doc explicitly covers "a
  native/IO failure mapped onto the channel", so the seam stays total.
- **Primitives confined to the DTO seam.** `MuellerRawRow`'s bare
  `string`/`int`/`DateTimeOffset`/`float` are the raw parsed fields at the
  parse/storage boundary (CLAUDE.md permits primitives there); the one physical
  quantity, the azimuth `description`, is elevated to `Angle`.

## Deferred

- Wiring the loaded rows into the full reconstruction — assemble the design from many
  `(azimuth, intensity)` captures, drive the step-004 `MuellerSolverProxy`, read the
  `frobeniusDiff` residual, apply the dark-frame / gain corrections and the excluded
  contaminated point from the manual test note — remains a later slice.
- Composing `createFileBacked ()` into the app/composition root (an Elmish/Storage
  wire-up) is not in this slice's scope.

## Gotchas

- **System-prompt path drift.** The task file's "System prompt" line points at
  `C:\GitHub\AI-Strategy-Generator\implement_contract_worker.system-md`, which does
  not exist; the real file is
  `.../src/ai_strategy_generator/multistep/implement_contract_worker.system-md` (a
  thin `IMPLEMENT_CONTRACT` delta over `.../multistep/arc-runner.system-md`). Read
  those; do not block on the literal task-file path. (Same drift the 003/004/005
  workers recorded.)
- **BOM is not whitespace on .NET Core.** A UTF-8 BOM decodes to a single U+FEFF,
  and `String.Trim()` does NOT remove it (unlike .NET Framework). BOM-tolerance is
  an explicit `TrimStart(char 0xFEFF)` before `CsvFile.Parse`; otherwise the first
  header reads `<BOM>experiment` and column-by-name lookup fails. The literal BOM was
  written as `char 0xFEFF`, never as a raw BOM byte in the `.fs` source.
- **`DateTimeOffset` parse style (recorded choice).** `RoundtripKind` is unsupported
  for `DateTimeOffset` and throws; `AssumeUniversal` is used instead — it keeps a
  string's explicit offset and assumes UTC for an offset-less one, so parsing is
  deterministic regardless of the runner's timezone.
- **Malformed file → `MalformedRow` (recorded choice).** The acceptance requires *a*
  typed `MuellerDataError` for a malformed file; a bad cell is naturally a
  `MalformedRow` (vs the mock's `EmptyFile` for an unknown path). IO failures also
  map to `MalformedRow`, per that case's own declared doc.
- **No `.contract-ids/XDUO-json` edit.** That registry tracks contract IDENTITY
  (`status: active`), not the declared→implemented lifecycle; the prior
  IMPLEMENT_CONTRACT (step 004) left it untouched, so this slice does too — the
  lifecycle flip is arc-runner step-state.
- **New `BerremanTests` → `Storage` reference.** `OpticalConstructor.Tests` already
  references `Storage` in the solution, and `Storage`'s transitive package deps
  (FSharp.Data, Giraffe.ViewEngine.StrongName via Plotly.NET) already reach
  `BerremanTests` through `Analytics`, so the new edge should not introduce an
  `MSB3277` version conflict.
- **Line endings.** All edited/added `.fs` files are pure LF and carry no leading
  BOM (verified with `od -An -tx1`: no `0d` bytes, no `ef bb bf`).

## Changelog

- 2026-07-15 — slice 006 (IMPLEMENT_CONTRACT STORE_XDUO_0009) attempt 01: implement
  the real file-backed Mueller CSV-LOAD adapter behind the step-005 seam — add
  `OpticalConstructor.Storage/MuellerDataStore.fs` with the pure header-aware,
  BOM-tolerant, invariant-culture `parseMuellerCsv` (columns read by name,
  `description` elevated to `Angle`, malformed/empty mapped to typed
  `MuellerDataError`) and the file-backed `createFileBacked ()` (`File.ReadAllText`
  under try/with, IO exceptions mapped to `MalformedRow`), mirroring
  `ExperimentDataStore` / `SpectralImport`. Reference `OpticalConstructor.Storage`
  from `BerremanTests` and add three facts driving the real parser/store (committed
  multi-column fixture, malformed file, missing-file IO boundary). Contract flipped
  to `implemented`.
