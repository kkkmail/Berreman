# Step 018 — state of the world

## Where we are

Step 018 lands spec 0038 Part G's pre-binding (operator Q7/Q28) and retires the last silent
analyzer fallback, on top of the step-016/017 Select machinery: every element ADDED to the table
except a sample now lands PRE-BOUND to its seeded Library entry — LightSource → `src-600`,
Detector → `det-intensity`, and the polarizer palette exposes THREE add buttons (LP → `pol-lp`,
CPL → `pol-cp-left`, CPR → `pol-cp-right`) over the UNCHANGED `CatalogueKind`s; a new sample
stays unbound (the inverse-flow hook Part L builds on). The rotate-R1 experiment's analyzer is
now READ from the bound polarizer entry through the typed `AnalyzerResolution` DU — a scene with
no bound polarizer reports the typed 'no analyzer present' status on the experiment surface
instead of silently assuming an ideal linear analyzer. Next in this part: step 019 gives the
sample editor its Choose material… verb over the step-016/017 Select machinery.

## What's working

- Pre-bind every palette add to its seeded default: the add path binds source→src-600,
  detector→det-intensity, LP→pol-lp, CPL→pol-cp-left, CPR→pol-cp-right; a new
  sample (and the not-bindable lens/mirror kinds) stays unbound.
- Grow the palette to THREE polarizer buttons (stable ids PaletteAdd_LP/CPL/CPR)
  over the UNCHANGED CatalogueKinds — only label + pre-bound entry differ
  (public `paletteButtons`; new targeted `AddElementBoundTo` arm, `AddElement`
  delegates with `defaultSeedEntry`).
- Retire the silent IdealLinear analyzer fallback: `runAnalyzerKind` returns the
  typed `AnalyzerResolution`; a polarizer-less rotate-R1 run reports the typed
  'no analyzer present' status chart (no fallback-synthesized series).
- 16 new tests (15 pure + 1 headless palette-click drive); build clean, suites
  571 / 119 / 149 (+1) / 413 (+15).

## Tests

- Gates are executed by the arc-runner's deterministic gate engine after this worker exits
  (IMPLEMENT Invariant 6 — the worker acts, it runs no checks). The roster for this step is
  `build`, `unit-tests`, `constructor-unit-tests`, `ui-smoke`, `ui-tests`.
- Diagnostic verification (not gate authority): `dotnet build Berreman.slnx -c Release`
  succeeded with 0 errors, **no MSB3277**, and zero warnings from the touched projects
  (Ui / Ui.Tests) — the only warnings are the step-001-catalogued pre-existing set in untouched
  files (NU1701 Wolfram ×2, SYSLIB0051 vendored MathNet ×2, FS3873 Dispersion, FS1125
  SeriesDataTests ×4, FS0044 Controls/ChartWindow). Suites: ui-smoke **149/149** (checkpoint
  148, +1: the palette's LP/CPL/CPR/Sa buttons clicked by stable id — unchanged kinds, seeded
  pre-binds, unbound sample), ui-tests **413/413** (checkpoint 398, +15 pure: the per-kind
  pre-bind theory incl. live-seam resolution, the unbound theory, the explicit CPR
  `AddElementBoundTo` path, the three-button palette mapping, and the analyzer-resolution set —
  varied-element-wins, circular-only-never-linear, `NoAnalyzerPresent`, the typed status chart,
  the pre-bound Malus curve), OpticalConstructor.Tests **571/571** (== checkpoint),
  BerremanTests **119 passed / 5 pre-existing skips** (== checkpoint). Logs in
  `specs/0038/.artifacts/018-diag-*.log`.
- Nothing deferred.

## Architecture

- **One add site, targeted message**: `AddElementBoundTo of CatalogueKind * string option` is the
  arm that creates a (possibly pre-bound) element; `AddElement` DELEGATES to it with the kind's
  `defaultSeedEntry` (the step-017 `BindValueIdTo` delegation precedent — `update` is `let rec`).
  All ~40 existing `AddElement` dispatch sites gained the pre-bound semantics with no churn.
- **The palette expansion is data at the button layer** (`PaletteButton` / public
  `paletteButtons`): the Model keeps `palette : CatalogueKind list`; `CircularPolarizer` expands
  to the CPL/CPR pair with explicit pre-binds and the kind UNCHANGED. `ElementPaletteControls`
  needed NO change — it is generic over id+label items, already keyed with AutomationIds.
- **`AnalyzerResolution` is a named two-case DU** (`ResolvedAnalyzer | NoAnalyzerPresent`), never
  an option: `NoAnalyzerPresent` IS the typed status, surfaced on the chart via the
  material-error precedent (`noAnalyzerTitle`/`noAnalyzerStatus` on an empty chart, which
  `experimentState.description` carries into the bay). Resolution order preserved: the varied
  element's bound polarizer, else the LAST bound scene polarizer, else the typed status.
- **Seed ids centralized once** (`SeedEntryIds` literals) — the add path, palette, and tests
  share them; pre-binds are committed without proxy validation at add time because `boundEntry`
  already degrades an unresolvable `valueId` to unbound and the Domain seeds are re-seeded at
  every start (spec §0.2); the pure theory pins that each seed id resolves for its kind.

## Deferred

- The sample editor's Choose material… verb (Materials window Select state over a
  `LayerPosition`) — step 019.
- Part J's bound/unbound/not-bindable visual states (dashed unbound rendering) — the pre-binding
  here is what keeps freshly added non-sample elements solid there.
- Threading the app-configured `QuickPickThreshold` / `SelectWindowsModal` into the product
  workbench Model (defaults today; step 47 owns the composition acceptance); the seeded initial
  src/det elements stay unbound (see Gotchas — recorded interpretation).
- Pre-existing warnings in untouched files (Part N).

## Gotchas

- **The task file's system-prompt path was stale again** — the IMPLEMENT worker prompt lives
  under `src/ai_strategy_generator/multistep/implement_worker.system-md` in the tool repo (the
  step-007..017 gotcha recurred). The slice's `ElementId.fs:535-540` /
  `TestWindows/TableAndElementRotationView.fs:1407-1427` references resolved by symbol to
  `ElementId.fs:602-611` (`seedEntries`) and the relocated `Ui/TableAndElementRotationView.fs`
  `runAnalyzerKind`.
- **Pre-binding applies to the ADD path, not the seeded initial scene** (recorded
  interpretation): `initMainWith`'s seeded src/det stay unbound — the step-016/017 acceptance
  tests prove their bind flows over the seeded UNBOUND detector, and the runners' defaults make
  the unbound seeds behave identically (600 nm / Intensity). A later step re-scoping this only
  needs `defaultSeedEntry` at the two seed sites.
- **A direct `AddElement CircularPolarizer` pre-binds the LEFT seed** (`pol-cp-left`, the seed
  order); only the palette's CPR button carries `pol-cp-right` (via `AddElementBoundTo`).
- **Every `AddElement LinearPolarizer` fixture is now a BOUND polarizer**: the input Stokes is
  polarized through `pol-lp`'s behaviour and the analyzer resolves from the bound entry. No
  existing assertion depended on the old unbound defaults (swept before implementing) — but a
  future test wanting an UNBOUND polarizer must strip `valueId` explicitly.
- **A VaryR1+Intensity draft in a polarizer-less scene yields the typed status chart** (empty
  series + `noAnalyzerTitle`) where it silently produced a Malus curve before;
  `openChartWindowHook` guards on non-empty series, so the pop-out refuses it. A test pinning a
  rotate-R1 series must have a bound polarizer (the pre-bound add gives one by default).
- **A `ConstantMueller` polarizer now falls through to `NoAnalyzerPresent`** (its `idealKindOf`
  is None — the documented step-014 skip) instead of the silent linear default; no seed or
  editor can produce one yet.
- Step 002–017 carried-over gotchas remain valid (baselines come from `.checkpoints-json`, not
  this SoW's YAML; the FuncUI Elmish host skips a structurally-equal model; a test that opens a
  Select-state window must CLOSE it; never pin a numeric assertion on a store thickness
  magnitude; the appsettings.json write-back into test output copies is expected).

## Changelog

- 2026-07-11 — Step 018 (IMPLEMENT, attempt 1): Part G pre-binding — the add path pre-binds
  every new non-sample element to its seeded entry (source→src-600, detector→det-intensity,
  LP/CPL/CPR→pol-lp/pol-cp-left/pol-cp-right over UNCHANGED kinds; new `AddElementBoundTo` arm,
  `AddElement` delegates via `defaultSeedEntry`; public `paletteButtons` grows the palette to
  three polarizer buttons with stable ids; samples stay unbound). The silent IdealLinear
  analyzer fallback is RETIRED: `runAnalyzerKind` returns the typed `AnalyzerResolution` and a
  polarizer-less rotate-R1 run reports the typed 'no analyzer present' status. 16 new tests.
  Build clean (no MSB3277); suites 571 / 119 / 149 / 413.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 571
  ui_smoke_tests: 149
  ui_tests: 413
```
