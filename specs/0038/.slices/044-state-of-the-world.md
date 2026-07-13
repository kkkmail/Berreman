# State of the world — 0038 step 044 (IMPLEMENT)

## Where we are

Step 044 is the automation-id consolidation slice near the end of arc 0038. Before it, every reusable
control, view and window carried its own nested `UiIds` module (plus `ChartWindowIds`), scattering the
same automation-id literals across ~27 modules in four projects. This round gathers them into ONE
`[<RequireQualifiedAccess>] module UiIds` (`OpticalConstructor.Controls/UiIds.fs`, compiled first) with a
nested per-surface sub-module, re-points every control site and test mechanically, and deletes the old
modules. Id string values are byte-for-byte unchanged.

## What's working

- Consolidate all 27 scattered `UiIds`/`ChartWindowIds` id modules into one `UiIds.fs` with 27
  per-surface sub-modules, compiled first in `OpticalConstructor.Controls`.
- Re-point every control, view, window and test reference to `UiIds.<Surface>.<member>`; delete every
  per-control id module so exactly one module holds every automation-id literal.
- Keep `OpticalConstructor.Controls` domain-free by leaving the two window views' Domain-typed
  `entryNode`/`versionRow` id helpers as plain functions beside their views.
- Preserve all 340 id members and 334 id string values (verified against git HEAD); every headless test
  still locates its controls.

## Tests

All gates run locally and passed (advisory; the arc-runner gate engine re-runs them authoritatively):

- build — 0 errors (10 pre-existing warnings, none from this change).
- unit-tests (BerremanTests) — 119 passed, 5 skipped, 0 failed.
- constructor-unit-tests (OpticalConstructor.Tests) — 674 passed, 0 failed.
- ui-smoke — 173 passed, 0 failed.
- ui-tests — 461 passed, 0 failed.

```yaml
gates:
  berreman_unit_tests:   119
  constructor_unit_tests: 674
  ui_smoke_tests:        173
  ui_tests:             461
```

## Architecture

- The single id home lives in `OpticalConstructor.Controls` — the lowest project that Ui, TestWindows,
  TestWindows.App and Ui.Tests all reference (directly or transitively) — compiled FIRST so every
  downstream file resolves `UiIds.<Surface>`.
- Fixed ids are `[<Literal>]` (usable in `InlineData` attributes); host-code-derived ids stay functions.
- Sub-modules map 1:1 to surfaces with distinct names, so no id-constant name collides across surfaces.
- `ChartWindowIds` folded into `UiIds.ChartWindow` so no second automation-id module survives.
- The Controls domain-free boundary is preserved: Domain-typed id helpers stay in the Ui views.

## Deferred

- Nothing for this slice. The pre-existing value duplication `Materials.categoryOption` /
  `MaterialEditor.categoryOption` (both `"MaterialCategoryOption_"+code`) is preserved verbatim rather
  than de-duplicated — collapsing two surfaces' ids would exceed a mechanical move and belongs to a
  later cleanup if ever desired.

## Gotchas

- `OpticalConstructor.Controls` intentionally has no Domain reference; do not "simplify" by moving
  `LibraryWindowView`/`MaterialsWindowView` `entryNode`/`versionRow` into `UiIds` — they need
  `VersionNumber`/`MaterialId`. They live beside their views and are addressed as `LW.entryNode` /
  `MW.versionRow` (no longer `…UiIds.…`).
- A handful of test files reference their view's ids **bare** (via `open <View>`); they were re-pointed
  to `UiIds.<Surface>.…` too, not only the `X.UiIds.…`-qualified sites.
- All fixed ids are now `[<Literal>]`; keep it that way so attribute-cited ids (e.g. TestLauncher's
  `InlineData`) keep compiling.

## Changelog

- 2026-07-11 — Consolidated 27 scattered `UiIds`/`ChartWindowIds` modules into one
  `OpticalConstructor.Controls/UiIds.fs` (27 per-surface sub-modules); re-pointed all references;
  values unchanged; build + all test gates green.
