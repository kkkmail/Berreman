# 013 state-of-the-world — Persistence II (Part I §I.5–I.8)

## Where we are

Slice 013 is the second persistence slice of Spec 0022's Part I. It layers the
[Standard] I/O features — recent files + autosave (§I.5), whole-project-snapshot
undo/redo (§I.6), chart-data/chart-image export (§I.7), and the report generator +
material-library import/export + design-history/diff (§I.8) — on top of the slice-003
storage core (`serializeProject`, the shared `JsonSerializerOptions`, `StorageError`,
schema validate-on-load). It consumes the slice-004 `MaterialEntry` boundary and
introduces the export/report `ChartHandle` boundary the Part J UI will supply.

## What's working

- Add recent-files MRU + autosave (`RecentFiles.fs`): bounded MRU through the shared
  JSON options; `writeAutosave` writes the canonical JSON to a distinct
  `<name>.ocproj.autosave` and never touches the `.ocproj`.
- Add immutable undo/redo (`History.fs`): `EditHistory` past/present/future with
  pattern-matched, no-op-on-empty `undo`/`redo` (no `.Value`/`.IsSome`).
- Add CSV/Excel + chart-image export (`Export.fs`): `exportCsv`/`exportExcel` serialize
  already-computed sweep tuples; `exportChartImage` dispatches to the chart library's
  own writer via a `ChartHandle` closure (no rasterizer in Storage).
- Add report + material-library + design-history (`Report.fs`): Giraffe.ViewEngine HTML
  report, schema-validated material import/export, numbered `history/` JSON snapshots,
  and a pure `diffRevisions → ProjectDiff` (no external VCS).
- Add `*.autosave` to `.gitignore`; all features return the `StorageError` `Result`.

## Tests

- `build` gate — PASS (`dotnet build Berreman.slnx -c Release`; all projects compile,
  ClosedXML + Giraffe.ViewEngine.StrongName resolve; no new C# project).
- `unit-tests` gate (BerremanTests) — PASS, 84 passed / 5 skipped (baseline 84; not
  touched by this slice).
- `constructor-unit-tests` gate — PASS, 148 passed (140 baseline + 8 new).
  - HistoryTests: AC-I7 undo→pre-edit, redo→edited, undo/redo on empty = no-op;
    AC-I6 `writeAutosave` leaves `.ocproj` byte-unchanged and writes a distinct
    `.ocproj.autosave`; R-1 `RecentFiles.bump` MRU invariant proven directly with no
    IO (case-insensitive dedup of a re-pushed path, most-recent-first ordering,
    truncation to `maxRecent = 16`).
  - ExportImportTests: AC-I8 CSV header + one comma-separated row per row;
    AC-I10 `importMaterials` of the `sample-nk.csv` refractiveindex.info CSV returns
    a `MaterialEntry list`, and `exportMaterials` output validates against the
    `materialEntry` `$def`; AC-I11 `appendRevision` twice yields two numbered
    snapshots and `diffRevisions` returns a `ProjectDiff` (Added on an extra system).
- AC-I9 (chart-image writer) — verified at the dispatch boundary (no rasterizer in
  `OpticalConstructor.Storage`); the WebView2/Plotly render is not asserted headlessly.

```yaml
gates:
  berreman_unit_tests:    84
  constructor_unit_tests: 148
```

## Architecture

- **One storage core, reused everywhere.** Recent-files prefs, autosave, material
  export, and design-history revisions all go through `serializeProject` / the shared
  `ProjectJson.options`; no second JSON stack, no FsPickler for canonical data.
- **`ChartHandle` as a closure boundary.** `ChartHandle` did not exist after slice 012.
  It is defined in `Export.fs` as a record carrying the chart library's own
  `saveImage` writer, so the Storage layer takes no ScottPlot/Plotly/WebView2
  dependency and contains no rasterizer/vectorizer (AC-I9). The UI (Part J) supplies
  the concrete writer.
- **Material library is metadata-only JSON.** `OpticalPropertiesWithDisp` holds
  closures and cannot serialize; export projects each entry to the `materialEntry`
  `$def` display fields (id/name/category/description) and validates each against a
  standalone schema built from the slice-003 schema's `$defs` (so internal `$ref`s
  resolve and no `$id` collision occurs).
- **`MaterialCategory` round-trips through the one JSON stack (F1, cycle 2).** The
  `MaterialEntryDto.category` field is the `MaterialCategory` DU itself; the shared
  `ProjectJson.options` (`.WithUnionUnwrapFieldlessTags()`) serializes it to/from
  `"Glass"`/`"Metal"`/… on both import (typed deserialize) and export (the boxed DU's
  runtime type drives the converter). The hand-maintained `categoryName`/
  `categoryOfName` map was deleted, so adding a category can no longer silently drift.
  The `description`-omit-when-`None` dict on the export side stays (it diverges from
  `.WithUnwrapOption()`'s null-writing, has no shared helper, and is recorded below).
- **`materialEntry` validation reuses the published schema** by re-pointing a
  no-`$id` wrapper at `#/$defs/materialEntry` with the original `$defs` embedded.

## Deferred

- The dispersion-model→tensor reconstruction (n,k/model → `OpticalPropertiesWithDisp`)
  is Part D §D.9; JSON material import rebuilds only the metadata boundary, using a
  vacuum `properties` placeholder.
- Autosave timer interval/trigger and undo-history depth bound are owned by the UI
  model (Part J §J.10).
- WebView2 PDF pagination of the report HTML is UI-hosted (Part J); `Report.generate`
  assembles and persists the print-source HTML.
- Retry-on-failure / rolling autosave generations, on-disk undo journals,
  command-pattern diff logs, bounded-history eviction — out of scope (§I.5/§I.6).

## Gotchas

- `open System.Text` makes `Json.Schema` resolve as `System.Text.Json.Schema`
  (partial-path error FS0893). Report.fs qualifies `System.Text.UTF8Encoding` and
  does not `open System.Text`, mirroring `SchemaValidation.fs`.
- `Report.generate`'s spec signature has NO output-path parameter; it writes to a
  conventional per-user reports folder and the UI owns final placement / file-offer.
- `ProjectDiff.materials` is always empty: the project aggregate (slice 003/007)
  carries `beamTree`/`systems`/`sources` but no top-level materials list yet.
- ClosedXML + FSharp.Data + the `sample-nk.csv` fixture/content-item were already
  present in the tree; this slice did not re-add them. It DID add
  `Giraffe.ViewEngine.StrongName` (the package Plotly.NET/Charting.fs use
  transitively) — beyond the slice's planned-edits bullet (ClosedXML only), but R-4
  mandates Giraffe.ViewEngine for the report HTML.
- A `description = None` is omitted from the exported material JSON (not written as
  `null`), because the `materialEntry` `$def` types `description` as `string`.

## Changelog

- 2026-06-01 — 013: recent-files/autosave, undo/redo, CSV/Excel/chart-image export,
  report + material-library import/export + design-history/diff, all over the
  slice-003 storage core. `*.autosave` added to `.gitignore`. Gates green
  (build; unit-tests 84; constructor-unit-tests 147).
- 2026-06-01 — 013 (cycle 2): add a no-IO `RecentFiles.bump` unit test proving R-1's
  MRU invariant (case-insensitive dedup, most-recent-first, truncate-to-16); collapse
  reuse finding F1 by typing `MaterialEntryDto.category` as `MaterialCategory` and
  letting the shared `ProjectJson.options` serialize it on both sides (deleting the
  hand-rolled `categoryName`/`categoryOfName` map). Gates green (build; unit-tests 84;
  constructor-unit-tests 148). F2/F3 and the `diffRevisions` interpretation left as-is.
