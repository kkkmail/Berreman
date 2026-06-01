# 014 impl-log — UX shell I (Part J §J.1–J.5)

## Progress

- [x] R-2 RepeatBuilder.fs — pure `expand` (`List.replicate count cell |> List.concat`)
- [x] R-3 Templates.fs — six `unit -> OpticalConstructorProject` + `loadTemplate`
- [x] R-1 Schematic.fs — pure geometry projection (band heights, wedge slope, ray, colour, layout)
- [x] R-4 StackEditor.fs — `SetLayerMaterial` message + `setLayerMaterial` + `layerMaterialDrop`
- [x] R-5 Workspace.fs — selection/layout state + overlay delegation to Part H §H.4
- [x] fsproj registrations (Ui + Tests)
- [x] tests (RepeatBuilderTests, TemplatesTests, SchematicGeometryTests, AC-J4 in StackEditTests)
- [x] gates (build / unit-tests / constructor-unit-tests) all green

## Files modified

New (Ui):
- `OpticalConstructor/OpticalConstructor.Ui/RepeatBuilder.fs`
- `OpticalConstructor/OpticalConstructor.Ui/Templates.fs`
- `OpticalConstructor/OpticalConstructor.Ui/Schematic.fs`
- `OpticalConstructor/OpticalConstructor.Ui/Workspace.fs`

New (Tests):
- `OpticalConstructor/OpticalConstructor.Tests/RepeatBuilderTests.fs`
- `OpticalConstructor/OpticalConstructor.Tests/TemplatesTests.fs`
- `OpticalConstructor/OpticalConstructor.Tests/SchematicGeometryTests.fs`

Edited:
- `OpticalConstructor/OpticalConstructor.Ui/StackEditor.fs` — R-4 drag-drop seam
  (`SetLayerMaterial` `StackMsg` case + `setLayerMaterial` transform +
  `layerMaterialDrop` resolving via the slice-004 `resolveMaterial`/`mediumFromMaterial`).
- `OpticalConstructor/OpticalConstructor.Tests/StackEditTests.fs` — two AC-J4 facts.
- `OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj` — register the
  four new files (RepeatBuilder before Templates, then Schematic, Workspace).
- `OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` — register
  the three new test files.

## Decisions

- **Avalonia-free pure seams, view binding deferred (interpretation choice).** R-1
  ("`OpticalSystem -> Avalonia control`") and R-4 ("wire Avalonia drag-and-drop")
  literally name Avalonia, but the entire OpticalConstructor.Ui tree has NO FuncUI
  DSL usage yet — every existing UI module (`StackEditor`, `ConstructionPage`,
  `SourceEditorView`) is a pure Avalonia-free seam with the view binding explicitly
  deferred per §0/P3. The gates (build / unit-tests / constructor-unit-tests) are all
  headless. I followed that precedent: the four new modules are pure projections/
  expansions/selection-state that the tests drive headless, and the FuncUI/Canvas +
  DragDrop event binding is deferred to a later UI-wiring slice (documented in each
  module header). This is the interpretation most consistent with the surrounding
  code and keeps the AC-J1..J5 assertions provable without an Avalonia host.
- **R-4 home = StackEditor.fs.** R-4 says the drop emits "a single MVU message that
  the owning `update` applies immutably" — the owning update is
  `ConstructionPage.EditStack` → `StackEditor.applyStackMsg`. The new `SetLayerMaterial`
  case flows through that existing path with NO ConstructionPage change. StackEditor
  already imports `resolveMaterial` (via `mediumFromMaterial`), so the drop reuses the
  slice-004 seam rather than re-resolving dispersion. The drop payload carries only the
  materialEntry id; `layerMaterialDrop` resolves it to the `SetLayerMaterial` message.
- **Template open path = `serialize >> deserializeProject`.** `ProjectFile.openProject`
  is `ReadAllText >> ProjectJson.deserializeProject`; `loadTemplate` routes the literal
  project through `serializeProject >> deserializeProject` — the same validate-on-load
  core, in memory, with no disk round-trip (and no private deserialize). §A.7 schema
  validation therefore applies to templates uniformly.

## Testing state

All three gates run locally and green:
- `build` — `dotnet build Berreman.slnx -c Release` → Build succeeded (no `error`).
- `unit-tests` (BerremanTests) — Passed 84 / Skipped 5 (baseline 84; untouched).
- `constructor-unit-tests` — Passed 165 (baseline 148 + 17 new): RepeatBuilderTests 4
  (AC-J2), TemplatesTests 4 (AC-J3), SchematicGeometryTests 7 (AC-J1), StackEditTests
  AC-J4 2.

AC coverage:
- AC-J1 — `SchematicGeometryTests`: `bandHeight` strictly monotonic in `Thickness`
  meters; `Thickness.Infinity` → fixed `semiInfiniteBandHeight`; `Substrate.Wedge` →
  sloped `WedgeBand` (slope grows with angle), `Plate` flat; ray tracks incidence
  angle and reflects about the normal; `colorForMaterial` deterministic; layout order.
- AC-J2 — `RepeatBuilderTests`: `expand cell R` = N*R layers in cell order, assignable
  to `OpticalSystem.films`; no period/super-layer type; no `Validation.fs` ref.
- AC-J3 — `TemplatesTests`: all six templates load through the schema-validated open
  path; DBR films == `RepeatBuilder.expand dbrCell dbrPeriods`; malformed doc rejected.
- AC-J4 — `StackEditTests`: a drop resolves via `resolveMaterial` and the owning update
  replaces `Layer.properties` immutably with `thickness` unchanged; unknown id is a
  no-op error.
- AC-J5 — `Workspace`: overlay delegates to `SeriesData.seriesComparison` /
  `Plot1DView.renderComparison` (the Part H §H.4 surfacing of `Charting.plotComparison`),
  no Part-J-local overlay loop (build-verified; selection model exercised by shape).

## Artifacts

No runtime artifacts captured this round (pure F# unit-test slice). Gate output
summarised above; nothing written under `.artifacts/` beyond the supervisor's task file.
