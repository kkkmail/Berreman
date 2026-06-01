# 015 state-of-the-world — UX shell II (Part J §J.6–J.9)

## Where we are

Slice 015 is the second Part J UX-shell tranche of Spec 0022: the persistent
UI-customization layer. It adds the [Core] persistent environment + favorites board
(J.6), the [Standard] preferences (J.7), the [Standard] theme & dockable panels
(J.8), and the [Standard] input validation & physical-sanity warnings (J.9). The
`EnvironmentSettings`/`Preferences` records persist as schema-validated JSON reusing
the slice-003 storage core, the `AppShell` is the first Ui module authored against
the public MIT FuncUI DSL (the audit-gated clone stays unreferenced), and the
`Validation` module is the J.9 seam slice 014's J.2 repeat builder already defers to.

## What's working

- Add `UserEnvironment.fs` (J.6/J.7): `EnvironmentSettings` (favorites board, last
  folders, recent files, panel layout, theme, chart palette, toolbar, preferences)
  and `Preferences` (per-quantity units, canonical-meter wavelength range, sweep
  points, decimal precision, the engine's own `SolverParameters`); persisted as JSON
  validated against the new schema, reusing the slice-003 `ProjectJson.options`,
  never `.binz`, with `load` falling back to built-in defaults on any failure.
- Add `Validation.fs` (J.9): pure `Result<'T, ValidationError list>` validators —
  thickness `> 0` for finite films (`Infinity` valid), repeat count `>= 1`, non-empty
  wavelength range with finite positive endpoints — plus the advisory imaginary-index
  gain warning; no exceptions, no coercion, no unit conversion.
- Add `optical-constructor-environment.schema.json` (J.6): §A.7-family JSON Schema
  for `EnvironmentSettings`, validated with `JsonSchema.Net`, copied to output and
  flowing to the Tests bin for validate-on-load.
- Add `AppShell.fs` (J.8): a dockable-panel shell authored against the public FuncUI
  DSL (so the build gate compiles it), with theme→`ThemeVariant`, chart palettes
  surfaced to Part H, and Avalonia-free layout reducers; the clone stays unreferenced
  and its linking mechanism unresolved.
- Wire the fsproj compile order (`UserEnvironment` → `Validation` → `AppShell`) and
  register the schema content item + two new test files; 20 new tests
  (AC-J6/J7/J8/J9) pass.

## Tests

- `build` gate — PASS (`dotnet build Berreman.slnx -c Release`; 0 errors; `AppShell`
  compiles against the public MIT `Avalonia.FuncUI` DSL, no FuncUI-clone reference,
  no new C# project).
- `unit-tests` gate (BerremanTests) — PASS, 84 passed / 5 skipped (baseline 84; not
  touched by this slice).
- `constructor-unit-tests` gate — PASS, 185 passed (165 baseline + 20 new):
  - `ValidationTests` (AC-J9, 12 facts): non-positive/negative/non-finite thickness,
    repeat count `< 1`, empty/non-positive/non-finite wavelength range each return
    `Result.Error` without throwing; `Infinity`, positive thickness, count `>= 1`, and
    a valid range return `Ok` unchanged; the gain warning fires only for `k < 0`.
  - `EnvironmentRoundTripTests` (AC-J6/J7/J8, 8 facts): `EnvironmentSettings`
    serializes to schema-validating JSON (never `.binz`), a full round-trip preserves
    every field (favorites board incl. by-value `Layer`/`OpticalSystem` fragments,
    recent files, last folder, theme, palette, panel layout, preferences), a
    deliberately invalid / malformed / missing file falls back to defaults without
    migration, save↔load round-trips through disk, a preference edit leaves stored
    `double<meter>` magnitudes identical, and solver defaults are the engine's
    `SolverParameters`.

```yaml
gates:
  berreman_unit_tests:    84
  constructor_unit_tests: 185
```

## Architecture

- **Reuse the slice-003 storage core, not a second JSON stack.** `EnvironmentSettings`
  serialization reuses `ProjectJson.options` (§I.1), so the favorites' reused engine
  `Layer`/`OpticalSystem` fragments — and their `Matrix<Complex>` tensors — round-trip
  through the same converters the canonical project uses, and Math.NET value-equality
  makes a whole-record round-trip compare equal. Env schema validation mirrors
  `SchemaValidation.fs` (lazy single-load, distinct `$id`) inside `UserEnvironment`
  because the env schema lives in the Ui project per the slice's files-in-scope;
  `JsonSchema.Net` flows transitively via Storage.
- **Validation failure falls back, never migrates (AC-J6).** `load` is a total
  `path -> EnvironmentSettings` returning `defaults` on a missing/malformed/invalid
  file. Schema versioning/migration is out of scope (§0 constraint 6); the envelope
  `schemaVersion` `const` turns a version drift into a validation failure → defaults.
- **AppShell is the first FuncUI-DSL view in the tree.** Every prior Ui module is a
  pure Avalonia-free seam with the view deferred. R-3 / the Risk note / AC-J8 require
  `AppShell` to compile against the public FuncUI DSL while keeping the audit-gated
  clone unreferenced and its linking mechanism unresolved — so `AppShell` carries a
  minimal real `IView` shell (DockPanel/Border/TextBlock) plus Avalonia-free layout
  reducers; the theme/palette/layout LOGIC needs no Avalonia host, so the gates stay
  headless.
- **Solver defaults are the engine's `SolverParameters` (AC-J7).** `Preferences.solver`
  is `Berreman.Solvers.SolverParameters` (`Solvers.fs:15`); Part J defines no parallel
  solver-config record. Default-unit and wavelength-range preferences are
  display/boundary labels (the range stored in canonical meters), never altering a
  stored magnitude.

## Deferred

- The richer docking-facility binding (drag-to-redock, floating windows) that §J.8
  routes "through the FuncUI clone" — deferred until the §A.6/§A.9 audit clears the
  clone and its linking mechanism is decided; this slice leaves that decision
  unresolved and lays panels out with public Avalonia primitives only.
- The MVU wiring of `AppShell`/`UserEnvironment`/`Validation` into the live app
  window (a model/update/host) follows the established view-binding-deferred
  precedent; this slice lands the records, the validators, the persistence, and the
  DSL-compiled shell seam.
- Non-causal-tabulated-data physical-sanity detection (Kramers–Kronig) beyond the
  named gain warning — out of the [Standard] minimum (§0 constraint 6); the gain
  warning is the representative 010 Part II §8 check delivered here.
- J.10–J.12 (the `JobRunner` whose active job appears in `AppShell`'s shared status
  area, the `Help`/glossary/gallery, and the `SystemView3D` viewport) are slice 016
  (LAST).

## Gotchas

- The env JSON carries an envelope `schemaVersion` (const "1.0") that the
  `EnvironmentSettings` record does not declare; System.Text.Json ignores the
  unmapped property on bind (the same pattern `deserializeProject` uses). A mismatched
  version fails the schema `const` and `load` returns `defaults`.
- `JsonSchema.FromText` registers a schema by its `$id` in a process-global registry;
  the env schema uses a DISTINCT `$id` from the project schema and is loaded once
  behind `lazy`, so it never collides with `SchemaValidation.fs`.
- `Validation.Severity` cases are `Blocking | Warning` (not `Error`) to avoid
  shadowing the `Result.Error` constructor inside the module.
- The schema is a build-copied `Content` item at the Ui project root; it reaches the
  Tests output bin transitively (verified present in both bins), which is what makes
  `constructor-unit-tests`' validate-on-load resolve it at runtime.

## Changelog

- 2026-06-01 — 015: second Part J UX tranche — persistent environment + favorites
  board (`UserEnvironment.fs`, J.6) and preferences (J.7) persisting as
  schema-validated JSON reusing the slice-003 storage core with defaults-fallback;
  the `optical-constructor-environment.schema.json` env schema (§A.7 family);
  boundary validators + gain warning (`Validation.fs`, J.9, the J.2 seam); and the
  `AppShell.fs` dockable shell authored against the public MIT FuncUI DSL (clone
  unreferenced, linking unresolved). Gates green (build; unit-tests 84;
  constructor-unit-tests 185).
