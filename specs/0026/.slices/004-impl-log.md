# 004 — Impl log: Optical table, drawer & top-down rendering geometry; standardized controls

## Progress

- [x] Read system prompt, project prompt, slice spec, dependency code (slices 001–003).
- [x] Wrote impl-plan + this skeleton.
- [x] `Table.fs` (new) — `OpticalTable` plate + `TableViewState`.
- [x] `Project.fs` (edit) — add `table` field.
- [x] schema (edit) — `opticalTable` `$def` + optional `table` root property.
- [x] Sweep 9 `OpticalConstructorProject` literals for the new mandatory `table` field.
- [x] `Controls.fs` (new) — flavours + destructive-gate CTAs; rewire `ConstructionView.fs:40`.
- [x] `ConstructorTable.fs` (new) — drawing-weight constants, layout, ray strokes, active indicator, CR-only default.
- [x] `Drawer.fs` (new) — the one standard cylinder drawer.
- [x] `ProjectJsonRoundtripTests.fs` (edit) — AC-C1 table round-trip.
- [x] `ConstructorViewTests.fs` (new) — AC-C4 / AC-C5 / AC-C6 / AC-J1 + table/view-state defaults.
- [x] Register new files in the three `.fsproj`.
- [x] Run gates; normalize EOL to LF.

## Files modified

New:
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Table.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/Controls.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorTable.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/Drawer.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ConstructorViewTests.fs`

Edited:
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Project.fs` (+`table` field)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` (+`Table.fs`)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/optical-constructor-project.schema.json` (+`opticalTable` `$def` + `table` property)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj` (+`Controls.fs`, `ConstructorTable.fs`, `Drawer.fs`)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructionView.fs` (`:40` CTA brushes → `Controls`)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` (+`ConstructorViewTests.fs`)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ProjectJsonRoundtripTests.fs` (AC-C1 + `table` in literals)
- 8 further `OpticalConstructorProject` literals: `Templates.fs`, `PlacementTests.fs`,
  `RoundTripTests.fs`, `HistoryTests.fs`, `ExportImportTests.fs`, `StackEditTests.fs`,
  `SystemView3DTests.fs` (+ both `ProjectJsonRoundtripTests` literals above).

## Testing state

See `Tests` section of the state-of-the-world. All gates run locally; logs in
`C:\GitHub\Berreman\specs\0026\.artifacts\`.

## Artifacts

Gate logs under `C:\GitHub\Berreman\specs\0026\.artifacts\004-*.log`.

## Decisions / notes

- `TableViewState` (R1/R2/R3-rel-to-screen, pan, zoom) lives in `Table.fs` per the
  slice scope ("the plate record + view state") but is NOT persisted in the project —
  only the physical `OpticalTable` plate persists (C.1.2). View state is ephemeral UI
  state owned by the interaction layer (slice 005).
- The drawer returns geometry in table-frame meters (`TablePoint`); the view transform
  (zoom/pan/rotation) is applied by `ConstructorTable.project` / slice 005, keeping the
  drawer independent of any Avalonia type (0.3).
- `toggle` flavour is a button-based toggle (the established codebase idiom:
  `Button.background (if active then accent else transparent)` in SourceView /
  MaterialsView / ResultsView / Shell), avoiding an unconfirmed CheckBox DSL.
- `ConstructionView.fs:40` is in this slice's "File:line references owned" list, so the
  two-line CTA rewire is in scope (behaviour-preserving: SeaGreen/IndianRed unchanged).
