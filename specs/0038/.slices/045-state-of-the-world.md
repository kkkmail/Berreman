# State of the world — 045 (IMPLEMENT: finalize the product launcher)

## Where we are

This is the WIRE step that finalizes the product launcher for spec 0038. Prior slices retired the
Elmish shell and made the launcher the startup window with Main only (steps 002/004/006), built the
app-scope `AppContext` (006), the `WindowLauncher` seam + host-layer `WindowRegistry` (007/008), the
single-instance Materials / Library windows (013/015), and the inverse constructor state
`initInverse` (037). Step 045 brings them together: the launcher now offers all four surfaces —
Main / Inverse / Materials / Library — over the ONE shared app scope, and the Materials / Library
buttons meet the constructor strip's buttons in one open-or-activate space.

## What's working

- Finalize the product launcher: FOUR buttons — Main / Inverse / Materials / Library, in that order,
  each with a stable Name and AutomationId.
- Add `InverseConstructorWindow` (the step-037 `initInverse` state); the Inverse button opens it.
- Open Materials / Library from the launcher through the SAME `EditorLaunchers.defaults` seam the
  constructor strip uses, so both callers share one single-instance window over the app scope.
- Refactor the Main / Inverse windows onto a shared `ConstructorScene.mount` helper; Main's title,
  size, and forward seed are unchanged.
- Prove it headless: four buttons present in order with their ids; each button opens (or activates)
  its surface; Materials from the launcher then the constructor strip activates ONE instance.

## Tests

- **build** — green (0 errors; 10 pre-existing/third-party warnings, none from the changed files).
- **ui-smoke** — green, 177 passed (173 → 177: +4 launcher behaviour tests).
- **ui-tests** — green, 461 passed (launcher structure tests replaced 1:1).
- **unit-tests** / **constructor-unit-tests** — untouched by this change (no Domain / Storage /
  Berreman-core edit); baselines carry forward.
- All gate runs local + advisory; the arc-runner's deterministic gate engine is authoritative and
  re-runs every gate after this session exits (Invariant 6).

## Architecture

- **`ConstructorScene.mount`** — one private helper mounts the dynamic workbench (`mainView`) on a
  `HostWindow` from a seed thunk. Main and Inverse are the SAME scene over the SAME app scope,
  differing only in seed (`initMainWith` vs `initInverse`) and title, so the shared helper removes
  the duplication a second window type would otherwise introduce.
- **`InverseConstructorWindow`** is a distinct type (not a mode flag on `MainConstructorWindow`), so
  the inverse open is observable by type in a headless test and the composition reads as prose.
- **`LauncherIds`** centralizes the four launcher-button ids in the App project (the launcher is
  App-owned; the Controls `UiIds` module — out of this slice's scope — keeps the in-window ids). The
  ids are distinct from the strip's `OpenMaterialsWindowButton` / `OpenLibraryWindowButton`.
- **Single-instance sharing** rides the existing module-level `WindowLauncher.WindowRegistry`: the
  launcher opens Materials / Library through `EditorLaunchers.defaults`, the exact seam the strip
  uses, keyed by `MaterialsWindowKey` / `LibraryWindowKey` — no launcher-specific window policy that
  could drift from the strip's.

## Deferred

- Threading the app-configured window modality / thresholds (`AppContext.settings`) onto the
  launcher-opened windows is the step-047 composition acceptance's concern, not this slice — the
  launcher opens Browse-mode windows through `EditorLaunchers.defaults` exactly as the strip does.
- The Solver-handoff window (step 039) is opened from inside the inverse constructor, not the
  launcher form; step 047 owns that wiring.

## Gotchas

- Every Materials / Library window opened in a test MUST be closed — the shared module-level
  `WindowRegistry` keys them by `MaterialsWindowKey` / `LibraryWindowKey`, and a leaked registration
  makes a later test's open ACTIVATE a stale window over the wrong stores (the
  `WireUiCompositionTests` discipline). The new behaviour tests close their windows each tick.
- The launcher and the constructor must share ONE `AppContext` for the single-instance proof to be
  meaningful (the registry key is context-independent, but the STORES are not).
- The constructor strip's Materials button is a `Border`, so a strip click uses the
  bounding-rectangle-centre mouse seam; the launcher's own buttons are real `Button`s driven by
  `RaiseEvent(Button.ClickEvent)`.

## Changelog

- 2026-07-11 — 045 IMPLEMENT: launcher finalized to Main / Inverse / Materials / Library over the
  shared app scope; added `InverseConstructorWindow`, `LauncherIds`, and the shared
  `ConstructorScene.mount`; Materials / Library open through the strip's `EditorLaunchers.defaults`
  seam (single-instance sharing). Build + ui-smoke (177) + ui-tests (461) green.

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 674
  ui_smoke_tests:         177
  ui_tests:               461
```
