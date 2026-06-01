# 013 impl-log — Persistence II (Part I §I.5–I.8)

## Progress

- [x] RecentFiles.fs (recent-files MRU + Autosave)
- [x] History.fs (immutable EditHistory undo/redo)
- [x] Export.fs (CSV/Excel + chart-image export, ChartHandle/ChartImageFormat)
- [x] Report.fs (Report.generate + MaterialLibrary + DesignHistory/ProjectDiff)
- [x] HistoryTests.fs (AC-I6, AC-I7)
- [x] ExportImportTests.fs (AC-I8, AC-I10, AC-I11)
- [x] fsproj + .gitignore wiring
- [x] gates: build, unit-tests, constructor-unit-tests — all PASS

### Cycle 2 (code-judge route-back)

- [x] R-1 coverage gap: add a no-IO `RecentFiles.bump` MRU test in `HistoryTests.fs`
      (case-insensitive dedup of a re-pushed path, most-recent-first ordering,
      truncation to `maxRecent = 16`).
- [x] F1 (reuse): type `MaterialEntryDto.category` as `MaterialCategory`; delete the
      hand-rolled `categoryName`/`categoryOfName` map; let the shared
      `ProjectJson.options` (`.WithUnionUnwrapFieldlessTags()`) serialize the DU on both
      the import (typed deserialize) and export (boxed-DU runtime type) sides.
- [x] F2/F3 and the `diffRevisions` interpretation calls left as-is per the retry hint.
- [x] gates re-run: build PASS, unit-tests 84 PASS, constructor-unit-tests 148 PASS.

## Files modified

New:
- `OpticalConstructor.Storage/RecentFiles.fs`, `History.fs`, `Export.fs`, `Report.fs`
- `OpticalConstructor.Tests/HistoryTests.fs`, `ExportImportTests.fs`

Edited:
- `OpticalConstructor.Storage.fsproj` — register 4 modules; add Giraffe.ViewEngine.StrongName
- `OpticalConstructor.Tests.fsproj` — register 2 test files
- `.gitignore` — add `*.autosave`

## Decisions

- **ChartHandle** absent after slice 012; defined in Export.fs as a closure-carrying
  record (`title` + `saveImage : ChartImageFormat -> string -> Result<unit, exn>`) so
  the Storage layer takes no ScottPlot/Plotly/WebView2 dependency and never
  rasterizes (AC-I9). The UI (Part J) supplies the library's own writer.
- **Material export** is a metadata projection (id/name/category/description) matching
  the `materialEntry` `$def`; `OpticalPropertiesWithDisp` holds closures and cannot
  serialize. JSON import rebuilds the metadata boundary with a vacuum `properties`
  placeholder (the §D.9 model→tensor reconstruction is Part D, out of scope here).
- **Report.generate** carries no output-path parameter in the spec signature; writes
  to a per-user reports folder and defers WebView2 PDF pagination to the UI host.
- **Giraffe.ViewEngine.StrongName 2.0.0-alpha1** — the same package Plotly.NET /
  Charting.fs use transitively; added to Storage for the HTML report (not a second
  HTML builder). This goes beyond the slice's planned-edits bullet (which named only
  ClosedXML), but R-4 mandates Giraffe.ViewEngine for the report HTML.

## Testing state

All three gates pass on the local run:

- `build` — PASS (`dotnet build Berreman.slnx -c Release`). Build succeeded; ClosedXML
  and Giraffe.ViewEngine.StrongName resolve; no new C# project introduced.
- `unit-tests` (BerremanTests) — PASS, 84 passed / 5 skipped (baseline 84; untouched).
- `constructor-unit-tests` (OpticalConstructor.Tests) — PASS, 148 passed (140 + 8 new:
  3 HistoryTests for AC-I6/AC-I7 + 1 R-1 `bump` MRU test, 4 ExportImportTests for
  AC-I8/AC-I10/AC-I11).

Every slice requirement R-1..R-4 is addressed this round, plus the cycle-2 route-back
items (R-1 `bump` coverage + reuse finding F1). Nothing deferred to a "round 2".
`commit_ready: true`.

## Artifacts

No persistent run artifacts captured this round (numeric/storage slice; gate output is
the console summaries recorded above).

## Artifacts

(none)
