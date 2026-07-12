# 044 — IMPLEMENT — impl-log

## Progress

- [x] Read system prompt, project prompt, slice spec, gates.
- [x] Surveyed all 27 id-holder modules (12 Controls + `ChartWindowIds` + 7 Ui views + 6 TestWindows + TestLauncher) and every reference site.
- [x] Authored `OpticalConstructor.Controls/UiIds.fs` — ONE `[<RequireQualifiedAccess>] module UiIds` with 27 nested per-surface sub-modules.
- [x] Wired `UiIds.fs` FIRST in `OpticalConstructor.Controls.fsproj`.
- [x] Rewrote references (qualified + `LW`/`MW`/`Scene` abbreviations + `ChartWindowIds.` + bare self-refs in defining files + the 6 `open`-view test files) via a scripted, idempotent, negative-lookbehind transform.
- [x] Deleted all 27 per-control `UiIds`/`ChartWindowIds` blocks; relocated `LibraryWindowView`/`MaterialsWindowView` Domain-typed `entryNode`/`versionRow` to top-level view functions.
- [x] Added `open OpticalConstructor.Controls` to the 13 referencing files that lacked it.
- [x] Cross-checked completeness: all 340 original id members and all 334 id string values present in the consolidated module (+ 4 relocated helpers).
- [x] Build green; all test gates located their controls.

## Files modified

61 changed + 1 new (`UiIds.fs`) = 62 files; +1232 / −1947 lines. Highlights:

- **New:** `OpticalConstructor.Controls/UiIds.fs` — the single consolidated id module (27 sub-modules:
  Rotation, ElementPalette, RayPosition, Renderer, Ribbon, Library, LayerBands, Experiment, Materials,
  SampleLibrary, Category, FacetedTree, ChartWindow, Handoff, TableAndElementRotation, SampleEditor,
  MaterialEditor, CategoryEditor, LibraryWindow, MaterialsWindow, RendererTest, SnapToReflected,
  SnapToBeam, ElementMovement, ElementRotation, TableRotation, TestLauncher).
- **`OpticalConstructor.Controls.fsproj`** — `UiIds.fs` compiled first.
- **12 Controls files + `ChartWindow.fs`** — per-control `UiIds` / `ChartWindowIds` blocks removed; internal refs re-pointed to `UiIds.<Surface>.…`.
- **7 Ui view files** — `UiIds` blocks removed; refs re-pointed. `LibraryWindowView`/`MaterialsWindowView`
  keep `entryNode`/`versionRow` as plain top-level functions (Domain types stay in Ui).
- **6 TestWindows views + `TestLauncherWindow.fs`** — `UiIds` blocks removed; refs re-pointed.
- **~30 Ui.Tests + window files** — references re-pointed; `open OpticalConstructor.Controls` added where needed.

## Testing state

All gates run locally and green (advisory — the arc-runner gate engine is authoritative):

- **build** (`dotnet build Berreman.slnx -c Release`): 0 errors. 10 warnings, ALL pre-existing and none
  from this change (Wolfram NU1701, MathNet SYSLIB0051, ScottPlot FS0044 in `ChartWindow.ChartPlot`,
  Dispersion FS3873, SeriesData FS1125).
- **unit-tests** (BerremanTests): Passed 119, Skipped 5, Failed 0.
- **constructor-unit-tests** (OpticalConstructor.Tests): Passed 674, Failed 0.
- **ui-smoke** (Category=ui-smoke): Passed 173, Failed 0.
- **ui-tests** (Category!=ui-smoke): Passed 461, Failed 0.

Value/member preservation verified programmatically against git HEAD: 340 original members and 334
id string values all present. LF endings verified clean (no CRLF churn).

## Artifacts

Under `specs/0038/.artifacts/`:
- `044-consolidate-uiids.py` — the reference-rewrite / block-deletion transform script.
- `044-build-01.log`, `044-unit-tests-01.log`, `044-constructor-tests-01.log`,
  `044-ui-smoke-01.log`, `044-ui-tests-01.log` — captured gate outputs.

## Gotchas

- **Controls stays domain-free.** `OpticalConstructor.Controls` has no Domain project reference by
  design. The two window views' Domain-typed id helpers (`LibraryWindowView.versionRow`/`entryNode`,
  `MaterialsWindowView.versionRow`/`entryNode` — over `VersionNumber` / `MaterialId` / the view-local
  `entryNodeCode`) therefore stay as plain top-level functions beside their views; only their fixed
  `[<Literal>]` constants moved into `UiIds.LibraryWindow` / `UiIds.MaterialsWindow`. Reference form for
  those two members is `LW.entryNode` / `MW.versionRow` (was `LW.UiIds.entryNode` / `MW.UiIds.versionRow`).
- **`ChartWindowIds` folded in.** It held automation-id literals for the chart window, so to satisfy
  "exactly one module holds every automation-id literal" it became `UiIds.ChartWindow` (not left as a
  second id module). Values unchanged.
- **Sub-module naming is 1:1 per surface** (the spec's list — "Selector, Materials, … and the rest" —
  is illustrative, "e.g."). The spec-pinned `Handoff` name (per `SolverHandoffView`'s own comment) is
  honoured. Distinct names avoid collisions: `Library`≠`LibraryWindow`, `Materials`≠`MaterialsWindow`
  ≠`MaterialEditor`, `Category`≠`CategoryEditor`.
- **Pre-existing value duplication preserved, not "fixed".** `UiIds.Materials.categoryOption` and
  `UiIds.MaterialEditor.categoryOption` both yield `"MaterialCategoryOption_"+code` (two different
  surfaces already did so before this step). Faithful mechanical move keeps both; coupling them would
  be a semantic change beyond this slice's scope.
- Some test files (`MaterialEditorWindowTests`, `SampleEditorWindowTests`, `TestLauncherTests`,
  `CategoryEditorWindowTests`, `MainWorkbenchTests`, `OutOfBandBadgeTests`) referenced their view's ids
  **bare** (`UiIds.foo`) via an `open` of the view module — those needed the same
  `UiIds.<Surface>.foo` re-point, not just the qualified-ref rewrite.
