# 015 impl-plan — UX shell II (Part J §J.6–J.9)

## Approach

Four net-new Ui modules + one JSON Schema + two test files, plus two fsproj
edits. All five domain seams already exist; this slice is plumbing + the first
real FuncUI-DSL view in the tree.

- **`UserEnvironment.fs`** (R-1/R-2, [Core]+[Standard]): `EnvironmentSettings` /
  `Preferences` records (favorites board, last folders, recent files, panel
  layout, theme, chart palette, toolbar, preferences). Persist as JSON validated
  against the new env schema, REUSING the slice-003 `ProjectJson.options`
  (shared `JsonSerializerOptions`, §I.1) — never `.binz`, never a second JSON
  stack. `load` is a total `path -> EnvironmentSettings` that falls back to
  `defaults` on any missing/parse/schema failure (AC-J6, no migration). Solver
  defaults are the engine's `Berreman.Solvers.SolverParameters` (`Solvers.fs:15`)
  — no parallel solver-config record (R-2/AC-J7). Favorites pins store `Layer` /
  `OpticalSystem` fragments BY VALUE plus `materialEntry`/`sourceSpec` ids
  (§A.7 shapes), serialized through `ProjectJson.options`' ComplexMatrix
  converter so a pinned favorite survives independently.
- **`Validation.fs`** (R-4, [Standard]): pure validators returning
  `Result<'T, ValidationError list>` — thickness `> 0` (rejects `Thickness d`,
  `d <= 0.0<meter>`, `Media.fs:12`; `Infinity` is valid), repeat count `>= 1`
  (the J.2 seam slice 014 defers to), non-empty wavelength range with finite
  positive endpoints, plus the named physical-sanity gain warning (imaginary
  index `< 0` ⇒ gain). No exceptions, no coercion; operates on already-parsed
  canonical values (no unit conversion).
- **`AppShell.fs`** (R-3, [Standard]): dockable-panel shell authored against the
  PUBLIC MIT `Avalonia.FuncUI` DSL surface (slice 001 NuGet) — a real `IView`
  shell so the `build` gate compiles it. The audit-gated clone stays
  unreferenced and its linking mechanism unresolved (constraint 5/§A.6/§A.9/
  AC-J8). Theme→`ThemeVariant`, chart palette→`Avalonia.Media.Color` surfaced to
  Part H (no charting engine here). Layout/theme reducers are Avalonia-free and
  reuse the `UserEnvironment` types (theme + serialized layout are fields of
  `EnvironmentSettings`, round-tripping through its JSON).
- **`optical-constructor-environment.schema.json`** (R-1): JSON Schema for
  `EnvironmentSettings`, §A.7 family, distinct `$id`, validated with
  `JsonSchema.Net` (flows transitively via Storage). Permissive on the deep
  physics shapes (favorite Layer/System fragments), mirroring the project
  schema's permissive `$defs`.

## Files

New: `UserEnvironment.fs`, `Validation.fs`, `AppShell.fs`,
`optical-constructor-environment.schema.json`, `ValidationTests.fs`,
`EnvironmentRoundTripTests.fs`. Edited: `OpticalConstructor.Ui.fsproj`
(register 3 fs in order: UserEnvironment → Validation → AppShell; copy schema),
`OpticalConstructor.Tests.fsproj` (register 2 test files).

## Risks

1. **JSON discipline** — env settings MUST be schema-validated JSON reusing the
   slice-003 options, never `.binz`, fall-back-to-defaults (not migrate) on
   failure. Proven by round-trip + invalid-file tests.
2. **FuncUI clone gate** — `AppShell` must compile against the PUBLIC FuncUI DSL
   while the clone at `C:\GitHub\Avalonia.FuncUI.Clone\` stays unreferenced and
   its linking mechanism unresolved. Mitigation: keep the view minimal (only
   core Avalonia controls), grep the Ui fsproj for any clone path before exit.
3. **Content flow** — the schema content item must reach the Tests bin so
   validate-on-load works under `constructor-unit-tests`; mirrors how the
   project schema reaches Tests/bin via transitive content.
