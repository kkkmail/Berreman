# 014 state-of-the-world — UX shell I (Part J §J.1–J.5)

## Where we are

Slice 014 is the first Part J UX-shell tranche of Spec 0022: the surfaces that are
mostly UI/plumbing over capabilities the engine already has. It adds the [Core]
to-scale schematic cross-section (J.1), the [Standard] repeat/period unit-cell
builder (J.2), the [Standard] code-defined stack templates (J.3), [Standard]
drag-and-drop of library materials onto layer rows (J.4), and the [Standard]
multi-system comparison workspace (J.5). Each reads canonical domain values and
delegates real work to a seam owned elsewhere (slice 003 `openProject`, slice 004
`resolveMaterial`, slice 012 `plotComparison` via Part H §H.4) rather than
reinventing it. Following the established OpticalConstructor.Ui precedent, the new
modules are pure Avalonia-free seams; the FuncUI/Canvas + DragDrop view binding is
deferred to a later UI-wiring slice.

## What's working

- Add `Schematic.fs` (J.1): a pure `OpticalSystem` geometry projection — band heights
  monotonic in `Thickness` meters, `Thickness.Infinity` as a fixed semi-infinite band,
  a sloped `Wedge` lower edge from `WedgeAngle`, incident-ray geometry from the
  incidence angle, deterministic per-material colour, and thickness labels reusing the
  `StackEditor` Units boundary seam.
- Add `RepeatBuilder.fs` (J.2): the pure `expand cell R = List.replicate R cell |>
  List.concat` yielding N*R ordinary `Layer`s, with no period/super-layer type and no
  `Validation.fs` forward reference.
- Add `Templates.fs` (J.3): six `unit -> OpticalConstructorProject` factories (AR,
  bandpass, beam splitter, polarizer, waveplate, DBR) with the DBR stack built via
  `RepeatBuilder.expand`; `loadTemplate` routes through the slice-003 validate-on-load
  core so §A.7 schema validation applies.
- Wire J.4 drag-drop in `StackEditor.fs`: a `SetLayerMaterial` MVU message + immutable
  `setLayerMaterial`, and `layerMaterialDrop` resolving the dropped materialEntry id via
  the slice-004 `resolveMaterial` seam; the drop replaces `Layer.properties` and leaves
  `thickness` unchanged through the existing owning `update`.
- Add `Workspace.fs` (J.5): multi-system visibility/active selection state over
  `OpticalConstructorProject.systems`, delegating the overlay to Part H §H.4
  (`SeriesData.seriesComparison` / `Plot1DView.renderComparison`, reusing
  `plotComparison`) with no Part-J-local overlay.

## Tests

- `build` gate — PASS (`dotnet build Berreman.slnx -c Release`; all projects compile at
  Release/x64, no new C# project, no FuncUI-clone reference).
- `unit-tests` gate (BerremanTests) — PASS, 84 passed / 5 skipped (baseline 84; not
  touched by this slice).
- `constructor-unit-tests` gate — PASS, 165 passed (148 baseline + 17 new):
  - RepeatBuilderTests (AC-J2): N*R layers in cell order, assignable to films, no
    period type, no `Validation.fs` reference.
  - TemplatesTests (AC-J3): all six templates load through the schema-validated open
    path; DBR films equal `RepeatBuilder.expand dbrCell dbrPeriods`; a corrupted
    template document is rejected on load.
  - SchematicGeometryTests (AC-J1 portion only): band-height monotonicity, fixed
    semi-infinite band, sloped wedge edge vs flat plate, ray-angle/reflection geometry,
    deterministic material colour, layout ordering.
  - StackEditTests (AC-J4): a drop resolves via `resolveMaterial` and replaces
    `Layer.properties` immutably with `thickness` unchanged; unknown id is a no-op error.
- AC-J5 is verified by delegation shape (overlay routes to the Part H §H.4
  `seriesComparison` / `renderComparison`, no local overlay) and the build gate.

```yaml
gates:
  berreman_unit_tests:    84
  constructor_unit_tests: 165
```

## Architecture

- **Pure seams, deferred view (P3 / precedent).** The whole OpticalConstructor.Ui tree
  has no FuncUI DSL yet; every UI module is an Avalonia-free pure core with the view
  binding deferred. Slice 014's four new modules follow that pattern, so all AC-J1..J5
  acceptances are provable headless and the gates need no Avalonia host. The FuncUI
  Canvas (J.1) and Avalonia DragDrop event wiring (J.4) are the only deferred pieces.
- **Every J surface delegates to its owning seam.** Templates load through the slice-003
  validate-on-load core (`serializeProject >> deserializeProject`, the in-memory form of
  `openProject`); the DBR template's films come from `RepeatBuilder.expand`; the J.4
  drop resolves through the slice-004 `resolveMaterial` (no re-resolved dispersion); the
  J.5 overlay routes to Part H §H.4 / `plotComparison` (no local multi-curve plot).
- **R-4 lives on the existing stack-editor MVU.** `SetLayerMaterial` is a new `StackMsg`
  case applied by `applyStackMsg`, so a drop flows through `ConstructionPage.EditStack`
  with no new page wiring; the drop payload carries only the materialEntry id.
- **Schematic colour is data, not Avalonia.** `colorForMaterial` is a deterministic pure
  `string -> SchematicColor` (a curated static map + a process-independent FNV hash
  fallback), so the same material is the same colour across redraws and across runs; the
  deferred view maps `SchematicColor` to `Avalonia.Media.Color`.

## Deferred

- The FuncUI/Avalonia `Canvas` view body (J.1) and the Avalonia DragDrop event handlers
  (J.4) — the actual control binding is a later UI-wiring slice, per the established
  module precedent; the pure geometry and the MVU message/update land here.
- The J.9 `Validation` module (slice 015) enforces and tests the repeat builder's
  `R < 1` rejection (AC-J9); `RepeatBuilder.expand` deliberately carries no reference to
  it (one-round-per-slice forward-reference constraint).
- The OpticalSystem→FixedInfo lift (which source/wavelength a system is plotted under)
  is owned by the source layer (Part E) and supplied to `Workspace.comparisonSeries` /
  `renderOverlay` by the caller; the workspace introduces no source store.
- J.6–J.9 (favorites/preferences/theme/dockable panels/validation, slice 015) and
  J.10–J.12 (job runner, help/gallery, `SystemView3D`, slice 016). The AC-J12 /
  `SystemView3D` portion of `SchematicGeometryTests.fs` is slice 016's, not added here.

## Gotchas

- `OpticalProperties.transparentGlass` / `uniaxialCrystal` are type augmentations in
  module `OpticalProperties.Standard` — `open OpticalProperties.Standard` is required
  (mirrors `MaterialLibrary.fs`), in addition to `open Berreman.MaterialProperties` for
  the `OpticalProperties` type and `fromRefractionIndex`.
- Unit-of-measure literals (`<nm>`, `<mm>`, `<meter>`) need `open Berreman.Constants`;
  omitting it fails with "the type 'nm' is not defined".
- `colorForMaterial`'s unknown-id fallback uses a hand-rolled FNV-1a fold, NOT
  `String.GetHashCode` — the latter's per-process random seed would change an unknown
  material's colour between runs, violating R-1 item 6's "same colour across redraws".
- `bandHeight` is monotonic but not yet display-normalised (a 1 mm film returns a huge
  raw height); the deferred view normalises the band column to the canvas. Only
  monotonicity is required by R-1 item 1 / AC-J1, and that is what is tested.
- R-4 edits `StackEditor.fs` (and adds AC-J4 facts to `StackEditTests.fs`) — both
  outside the slice's "Files in scope" four-new-file list, because R-4 is a wiring
  requirement into the EXISTING owning stack-editor MVU; this is the minimal faithful
  home and reuses the slice-004 resolver already imported there.

## Changelog

- 2026-06-01 — 014: first Part J UX tranche — to-scale schematic geometry
  (`Schematic.fs`, J.1), pure repeat builder (`RepeatBuilder.fs`, J.2), six code-defined
  templates (`Templates.fs`, J.3), library-material drag-drop seam on the stack editor
  (`StackEditor.fs`, J.4), and the multi-system comparison workspace (`Workspace.fs`,
  J.5). All new modules are pure Avalonia-free seams (view binding deferred per
  precedent); J.3 routes through the slice-003 open path, J.4 through the slice-004
  `resolveMaterial`, J.5 through the slice-012 `plotComparison`/Part H §H.4. Gates green
  (build; unit-tests 84; constructor-unit-tests 165).
