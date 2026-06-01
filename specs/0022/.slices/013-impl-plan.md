# 013 impl-plan — Persistence II (Part I §I.5–I.8)

## Approach

Layer four new modules onto the slice-003 storage core, every persistence path
reusing `ProjectJson.serializeProject` / the shared `ProjectJson.options` and the
`StorageError` `Result` channel. No second JSON stack, no FsPickler for canonical
data, no hand-rolled CSV reader (FSharp.Data via the existing `MaterialImport`).

- **RecentFiles.fs** — `RecentFiles` (`loadRecent`/`pushRecent`, bounded MRU list
  persisted to a single per-user app-data JSON file through `ProjectJson.options`)
  and `Autosave` (`autosavePath` = `<name>.ocproj.autosave`, `writeAutosave`
  reusing `ProjectFile.saveProject` so it writes the same canonical JSON to the
  `.autosave` sidewrite path and never touches the `.ocproj`).
- **History.fs** — immutable `EditHistory { past; present; future }` with
  `ofPresent`/`push`/`undo`/`redo`, pattern-matched no-ops on empty lists, no
  `.Value`/`.IsSome`.
- **Export.fs** — `ChartImageFormat = Png | Svg | Pdf` (net-new DU), `ChartHandle`
  (renderer-neutral record carrying the chart library's OWN save closure supplied
  by the UI — so Storage takes no ScottPlot/Plotly/WebView2 dependency and never
  rasterizes), `exportCsv`/`exportExcel` (ClosedXML) over the already-computed
  sweep tuples, and `exportChartImage` dispatching to the handle's closure.
- **Report.fs** — `MaterialLibrary` (import/export over the `materialEntry` `$def`,
  CSV via `MaterialImport`/FSharp.Data, export validated against the `$def`),
  `DesignHistory` (`appendRevision` numbered JSON snapshots under `history/`,
  pure `diffRevisions` → net-new `ProjectDiff`), and `Report.generate`
  (Giraffe.ViewEngine HTML assembly; PDF is the UI-hosted WebView2 print of that
  HTML).

## Files

- New: `RecentFiles.fs`, `History.fs`, `Export.fs`, `Report.fs` in
  `OpticalConstructor.Storage`; `HistoryTests.fs`, `ExportImportTests.fs` in
  `OpticalConstructor.Tests`.
- Edit `OpticalConstructor.Storage.fsproj` — register the four modules in compile
  order; add `Giraffe.ViewEngine.StrongName` (the package Plotly.NET/Charting.fs
  already use transitively) for the report. ClosedXML + FSharp.Data are already
  referenced. `sample-nk.csv` fixture + its content item already exist.
- Edit `OpticalConstructor.Tests.fsproj` — register the two test files.
- Edit `.gitignore` — add `*.autosave` (slice 003 owns the bullet; added `*.binz`).

## Risks

- ChartHandle did not exist after slice 012 (no `ChartHandle` symbol anywhere).
  Defined here as a closure-carrying record so Storage stays free of chart-lib /
  WebView2 deps and honours "no custom rasterizer in OpticalConstructor.Storage".
- `OpticalPropertiesWithDisp` holds closures (un-serializable); material export is
  a metadata projection (id/name/category/description) matching the `materialEntry`
  `$def`; JSON import rebuilds the metadata boundary (vacuum placeholder for the
  tensor — the §D.9 model→tensor reconstruction is Part D, out of scope here).
- `Report.generate` has no output-path parameter in the spec signature; it writes
  to a conventional per-user reports path and defers WebView2 PDF pagination to the
  UI host.
