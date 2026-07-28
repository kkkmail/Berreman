# 006 impl-plan — IMPLEMENT_CONTRACT STORE_XDUO_0009 MuellerDataProxy

## Goal

Implement the DECLARED (step 005) `MuellerDataProxy` seam to `implemented`:
the real file-backed Mueller measured-data CSV load. The contract types
(`MuellerRawRow`, `MuellerDataError`, `MuellerDataProxy`) already live in
`OpticalConstructor.Domain.MuellerReconstruction` (step 005). This slice adds
the pure parser + the file-backed adapter in `OpticalConstructor.Storage`, and
a `BerremanTests` fact that drives the real parser.

## Approach

1. **New file** `Berreman/OpticalConstructor/OpticalConstructor.Storage/MuellerDataStore.fs`
   (`module MuellerDataStore`), added to the Storage `.fsproj` compile list
   right after `ExperimentDataStore.fs`, before the `Storage.fs` anchor.
   - `parseMuellerCsv (csvText : string) : Result<MuellerRawRow list, MuellerDataError>`
     — PURE, header-aware, BOM-tolerant, invariant-culture. Reuses
     `FSharp.Data CsvFile.Parse` exactly as `SpectralImport.parseSpectrumCsv`
     (SpectralImport.fs:46): parse text, read `csv.Headers` / `csv.Rows`,
     `r.Columns`. Resolves the `experiment`, `capture_index`, `captured_at`,
     `description`, `avg_total` columns BY NAME (header→index map, trimmed),
     elevates `description` to `Angle.degree`, and never touches the filesystem.
     A malformed row / missing column → `MalformedRow reason`; a header-only or
     empty file → `EmptyFile reason`; never a throw (whole body under try/with).
   - `createFileBacked () : MuellerDataProxy` whose `tryLoadFamily` reads the
     file text at the IO boundary under try/with, mapping any IO exception to a
     typed `MuellerDataError` (`MalformedRow`, per the step-005 channel doc:
     "a native/IO failure mapped onto the channel"), then delegates to
     `parseMuellerCsv`. Mirrors `ExperimentDataStore.loadThrough` /
     `createFileBacked` (ExperimentDataStore.fs:34, :51).

2. **BerremanTests** — add the explicit `ProjectReference` to
   `OpticalConstructor.Storage` in `BerremanTests.fsproj`; add facts to
   `MuellerReconstructionTests.fs` that (a) parse a committed multi-column
   BOM-prefixed CSV fixture through the REAL `parseMuellerCsv` and assert
   `MuellerRawRow` values with `description` elevated to `Angle`; (b) assert a
   malformed file yields a typed `MuellerDataError` (never a throw); (c) assert
   `createFileBacked ()` produces a proxy whose `tryLoadFamily` maps a missing
   file to a typed error at the IO boundary (never a throw).

## Files to modify

- ADD `Berreman/OpticalConstructor/OpticalConstructor.Storage/MuellerDataStore.fs`
- EDIT `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj` (compile item)
- EDIT `Berreman/BerremanTests/BerremanTests.fsproj` (ProjectReference to Storage)
- EDIT `Berreman/BerremanTests/MuellerReconstructionTests.fs` (facts + fixture)

## Risks

- BOM tolerance: a UTF-8 BOM decodes to U+FEFF, which `String.Trim()` does NOT
  strip on .NET Core → strip the leading `﻿` explicitly before Parse.
- `DateTimeOffset.TryParse` styles: `RoundtripKind` is unsupported for
  `DateTimeOffset`; use `DateTimeStyles.AssumeUniversal` (deterministic for
  offset-less ISO strings) with `InvariantCulture`.
- The new BerremanTests → Storage reference must not introduce an assembly
  version conflict (MSB3277); Storage flows Domain transitively, already
  referenced.
