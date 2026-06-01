# 015 impl-log — UX shell II (Part J §J.6–J.9)

## Progress

- [x] `UserEnvironment.fs` — `EnvironmentSettings`/`Preferences`, JSON persist + validate + defaults-fallback (R-1/R-2)
- [x] `Validation.fs` — pure `Result` validators + gain warning (R-4)
- [x] `optical-constructor-environment.schema.json` — env schema (R-1)
- [x] `AppShell.fs` — FuncUI-DSL dockable shell, theme, palettes (R-3)
- [x] fsproj edits (Ui compile order + schema content; Tests files)
- [x] `ValidationTests.fs` (AC-J9)
- [x] `EnvironmentRoundTripTests.fs` (AC-J6/J7/J8)
- [x] gates: build, unit-tests, constructor-unit-tests — all PASS

## Files modified

New:
- `OpticalConstructor/OpticalConstructor.Ui/UserEnvironment.fs` — `EnvironmentSettings`
  (favorites board, last folders, recent files, panel layout, theme, chart palette,
  toolbar, preferences) + `Preferences` (per-quantity units, wavelength range in
  canonical meters, sweep points, decimal precision, engine `SolverParameters`).
  JSON persist/validate/load reusing the slice-003 `ProjectJson.options`; `load`
  falls back to `defaults` on any failure (no migration).
- `OpticalConstructor/OpticalConstructor.Ui/Validation.fs` — pure
  `Result<'T, ValidationError list>` validators: `validateThickness` (> 0 finite;
  `Infinity` valid), `validateRepeatCount` (>= 1), `validateWavelengthRange`
  (finite positive endpoints, non-empty), `imaginaryIndexGainWarning` (advisory).
- `OpticalConstructor/OpticalConstructor.Ui/AppShell.fs` — FuncUI-DSL dockable
  shell (`shellView`), `themeVariant`/`toggleTheme`, `paletteColors` (surfaced to
  Part H), Avalonia-free layout reducers (`setPanelVisible`/`dockPanel`).
- `OpticalConstructor/OpticalConstructor.Ui/optical-constructor-environment.schema.json`
  — §A.7-family JSON Schema, distinct `$id`, validated with `JsonSchema.Net`.
- `OpticalConstructor/OpticalConstructor.Tests/ValidationTests.fs` (12 facts, AC-J9).
- `OpticalConstructor/OpticalConstructor.Tests/EnvironmentRoundTripTests.fs` (8 facts, AC-J6/J7/J8).

Edited:
- `OpticalConstructor.Ui.fsproj` — registered `UserEnvironment.fs`/`Validation.fs`/
  `AppShell.fs` (compile order) and the schema as a copied `Content` item. NO clone
  dependency added.
- `OpticalConstructor.Tests.fsproj` — registered the two new test files.

## Decisions

- **Reuse, not a second JSON stack.** `EnvironmentSettings` serialization reuses
  `ProjectJson.options` (the slice-003 shared `JsonSerializerOptions`, §I.1), so the
  favorites' `Layer`/`OpticalSystem` tensors round-trip through the same
  `ComplexMatrix`/FSharp converters as the canonical project. Env schema validation
  mirrors `SchemaValidation.fs` (lazy single-load, distinct `$id`) rather than
  generalizing the storage validator — the env schema lives in the Ui project per
  the slice's files-in-scope, and `JsonSchema.Net` flows transitively via Storage.
- **Favorites pins.** `FavoritePin = LayerPin | SystemPin | MaterialPin id | SourcePin id`
  — reused engine fragments stored by value (survive project deletion, §J.6 item 2),
  library materials/sources by id. Serialized as the FSharp.SystemTextJson
  `{Case,Fields}` shape; the env schema is permissive on the Fields payload, mirroring
  the project schema's permissive physics `$defs`.
- **AppShell against the PUBLIC FuncUI DSL.** Per R-3 / the Risk note / AC-J8 this is
  the first Ui module authored against the FuncUI DSL surface (public MIT NuGet) so
  the `build` gate compiles it; the audit-gated clone stays unreferenced and its
  linking mechanism unresolved. The view is intentionally minimal (DockPanel +
  Border + TextBlock) — the richer docking-facility binding is the clone's once
  cleared. The theme/palette/layout LOGIC stays Avalonia-free and reuses the
  `UserEnvironment` types (so the gates need no Avalonia host).
- **Severity naming.** `Validation.Severity = Blocking | Warning` (not `Error`) to
  avoid shadowing the `Result.Error` constructor used throughout the module.

## Testing state

- `build` — PASS (`dotnet build Berreman.slnx -c Release`, 0 errors; FuncUI DSL
  compiles against the public NuGet, no clone reference).
- `unit-tests` (BerremanTests) — PASS, 84 passed / 5 skipped (baseline 84; untouched).
- `constructor-unit-tests` — PASS, **185 passed** (165 baseline + 20 new). Schema
  content item confirmed present in both the Ui and Tests output bins, so
  validate-on-load resolves it at runtime.

## Artifacts

`C:\GitHub\Berreman\specs\0022\.artifacts` — no separate capture files needed; gate
output is reproduced in the SoW `Tests` section.

## Gotchas

- The env JSON carries an envelope `schemaVersion` (const "1.0") that the
  `EnvironmentSettings` record does not declare; System.Text.Json ignores the
  unmapped property on bind (same pattern `deserializeProject` uses). A mismatched
  version fails the schema `const` → `load` returns `defaults` (no migration).
- `JsonSchema.FromText` registers by `$id` globally; the env schema uses a DISTINCT
  `$id` from the project schema and is loaded once behind `lazy`, so it never
  collides with `SchemaValidation.fs`.
