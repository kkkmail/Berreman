# 014 impl-plan — UX shell I (Part J §J.1–J.5)

## Approach

Follow the established OpticalConstructor precedent (`StackEditor.fs`,
`ConstructionPage.fs`, `SourceEditorView.fs`): every UI module is an
**Avalonia-free, pure seam** that the gates (build / unit-tests /
constructor-unit-tests, all headless) and tests can drive; the FuncUI/Avalonia
`view` binding is deferred to a later UI-wiring slice, exactly as the existing
modules' headers record. There is no FuncUI DSL usage anywhere in the project
yet, so introducing Avalonia control construction would add headless-test risk
for no acceptance benefit. Each new module reads canonical domain values and
delegates real work to a seam owned elsewhere.

## Files

New (Ui):
- `RepeatBuilder.fs` (R-2) — `expand : Layer list -> int -> Layer list` =
  `List.replicate count cell |> List.concat`. No `Validation.fs` reference
  (slice-015 forward ref). No new period/super-layer type.
- `Templates.fs` (R-3) — six `unit -> OpticalConstructorProject` factories (AR,
  bandpass, beam splitter, polarizer, waveplate, DBR). DBR films via
  `RepeatBuilder.expand`. `loadTemplate` routes through the slice-003
  validate-on-load core (`ProjectJson.serializeProject >> deserializeProject`,
  the in-memory form of `ProjectFile.openProject`) so schema validation applies.
- `Schematic.fs` (R-1) — pure geometry projection over `OpticalSystem`:
  `bandHeight` (monotonic in `Thickness` meters; `Infinity` → fixed band),
  `wedgeSlope`/substrate band kind, `rayGeometry` from `IncidenceAngle`,
  `colorForMaterial` (deterministic static map + stable fallback hash),
  layout list. Labels reuse `StackEditor.displayThickness` (Units seam).
- `Workspace.fs` (R-5) — multi-system selection/layout state over
  `OpticalConstructorProject.systems`; overlay delegates to Part H §H.4
  (`SeriesData.seriesComparison` / `Plot1DView.renderComparison`, which reuse
  `Charting.plotComparison`'s per-system layout). No local overlay.

Edited:
- `StackEditor.fs` (R-4) — add `SetLayerMaterial of int * OpticalProperties`
  message + `setLayerMaterial` immutable transform + `layerMaterialDrop` (resolves
  the dropped materialEntry id via the slice-004 `resolveMaterial` seam at the
  active wavelength). The drop flows through the existing owning `update`
  (`ConstructionPage.EditStack` → `applyStackMsg`), no ConstructionPage change.
- `OpticalConstructor.Ui.fsproj` — register RepeatBuilder, Templates, Schematic,
  Workspace (in that order, after the Charts block).
- `OpticalConstructor.Tests.fsproj` — register the new test files.

New tests:
- `RepeatBuilderTests.fs` (AC-J2) — N*R layers in cell order, assignable to films.
- `TemplatesTests.fs` (AC-J3) — each template validates through the schema open
  path; DBR films == `RepeatBuilder.expand`.
- `SchematicGeometryTests.fs` (AC-J1 portion only) — band-height monotonic;
  `Infinity` fixed; `Wedge` sloped; color determinism.
- AC-J4 facts added to existing `StackEditTests.fs` (natural StackEditor home).

## Risks

Cross-slice seam discipline (the medium risk): keep `RepeatBuilder.expand` pure
with no `Validation.fs` ref; route templates through the slice-003 open core;
resolve drops via the slice-004 `resolveMaterial`; overlay via the slice-012
`plotComparison`/§H.4 surface. Keep `SchematicGeometryTests.fs` confined to the
AC-J1 cases (slice 016 adds AC-J12).
