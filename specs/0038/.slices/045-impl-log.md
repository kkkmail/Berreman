# 045 — IMPLEMENT impl-log

## Progress

- [x] Read system prompt (`implement_worker.system-md` + base `arc-runner.system-md`), project
      prompt, slice spec, and the `045.gates` roster.
- [x] Surveyed the launcher composition root, the `AppContext` app scope, the `WindowLauncher`
      seam + host-layer `WindowRegistry`, `EditorLaunchers.defaults`, `initInverse` (step 037),
      the Materials / Library windows, and the existing `LauncherTests` / `WireUiCompositionTests`.
- [x] `Program.fs`: `open Avalonia.Automation`; new `LauncherIds` (four `[<Literal>]` ids); new
      private `ConstructorScene.mount`; refactored `MainConstructorWindow` onto it; new
      `InverseConstructorWindow`.
- [x] `Program.fs`: rewrote `LauncherWindow` to FOUR buttons (Main / Inverse / Materials / Library),
      each with `Name` + `AutomationId` + accessible name, Materials / Library through
      `EditorLaunchers.defaults`.
- [x] `LauncherTests.fs`: structure (four buttons in order + ids/labels + size) and behaviour
      (each button opens its surface; launcher+strip single-instance sharing).
- [x] Local build (Release) + `ui-smoke` + `ui-tests` green (advisory — see Testing state).
- [x] LF verified on both edited files.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`
  - Added `open Avalonia.Automation`.
  - **New** `[<RequireQualifiedAccess>] module LauncherIds` — the four stable launcher-button ids
    (`OpenMainButton` / `OpenInverseButton` / `OpenMaterialsButton` / `OpenLibraryButton`).
  - **New** `module private ConstructorScene` with `mount : HostWindow -> (unit -> Model) -> unit`
    — sizes the window (unchanged Main sizing) and runs `mainView` from a seed. Shared by both
    constructor windows.
  - `MainConstructorWindow` — refactored to call `ConstructorScene.mount` with the forward
    `initMainWith` seed (behaviour unchanged: same title, size, app-scope threading).
  - **New** `InverseConstructorWindow` — same scaffold with the `initInverse` seed and the title
    `Optical Constructor — Inverse`.
  - `LauncherWindow` — now FOUR buttons in order via a local `surfaceButton` helper (sets `Name`,
    `AutomationProperties.AutomationId`, `AutomationProperties.Name`); Materials / Library open
    through `TableAndElementRotationView.EditorLaunchers.defaults` over the injected `context`;
    height 180 → 300 to fit four buttons; `StackPanel` given `Spacing = 8.0`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LauncherTests.fs`
  - Rewrote to the four-button contract: fixed-size (height 300), four buttons **in order** by
    stable `Name`, each carrying its `AutomationId` + label (gate `ui-tests`); and behaviour
    (gate `ui-smoke`) — Main opens `MainConstructorWindow`, Inverse opens
    `InverseConstructorWindow`, Materials opens the Materials window, Library opens the Library
    window, and **Materials from the launcher then from the constructor strip activates ONE
    instance**. Added `matchesId` / `clickAndObserve` / `clickBorder` helpers (the
    `WireUiCompositionTests` seams). Every Materials/Library window a test opens is `Close()`d so
    the shared `WindowRegistry` never leaks a registration.

## Decisions

- **Separate `InverseConstructorWindow` type (not a mode flag).** Makes the inverse open observable
  in a headless test (`opened :? InverseConstructorWindow`) and reads as prose; the shared
  `ConstructorScene.mount` keeps Main / Inverse from duplicating the scene mount.
- **Launcher ids in the App project.** The slice `touches` are `App` + `Ui.Tests`, so the
  product-launcher ids live in a new `LauncherIds` module in `Program.fs`, NOT the Controls
  `UiIds` module (out of scope; the launcher is App-owned). They are distinct from the strip's
  `OpenMaterialsWindowButton` / `OpenLibraryWindowButton` — different controls that open the SAME
  single-instance windows over the shared registry.
- **Materials / Library reuse `EditorLaunchers.defaults`.** The launcher opens them through the
  EXACT seam the constructor strip uses, over the module-level `WindowRegistry` keyed by
  `MaterialsWindowKey` / `LibraryWindowKey` — this is what guarantees the launcher-then-strip
  single-instance sharing the acceptance requires (no separate policy for the launcher to drift).

## Testing state

`commit_ready: true`. Per **Invariant 6 — act only**, the worker implements the step and writes its
outputs; the arc-runner's deterministic gate engine is the sole gate authority and re-runs every
gate after this session exits. `CLAUDE.md` mandates a non-negotiable green build + running the
relevant tests after every change, so I ran the affected gates locally to verify MY OWN work
(advisory, de-risking a failure-budget burn) — not to green-light a gate:

- **build** (`dotnet build Berreman.slnx -c Release`): **0 errors**. 10 warnings, ALL pre-existing /
  third-party and none from the changed files (`Wolfram.NETLink` `NU1701` ×2 [exempt],
  `ChartWindow.fs` FS0044, MathNet `SYSLIB0051` ×2, `Dispersion.fs` FS3873, `SeriesDataTests.fs`
  FS1125 ×4). `Program.fs` and `LauncherTests.fs` emit no warning.
- **ui-smoke** (`--filter Category=ui-smoke`): **177 passed**, 0 failed (173 → 177: +4 launcher
  behaviour tests).
- **ui-tests** (`--filter Category!=ui-smoke`): **461 passed**, 0 failed (unchanged — the launcher
  structure tests were replaced 1:1).
- `berreman_unit_tests` (119) and `constructor_unit_tests` (674) are untouched by this change (no
  Domain / Storage / Berreman-core / Ui-source edit); their baselines carry forward.

## Artifacts

- `C:\GitHub\Berreman\specs\0038\.artifacts\045-launcher-tests-01.log` — LauncherTests (+
  TestLauncherTests) run (52 passed).
- `C:\GitHub\Berreman\specs\0038\.artifacts\045-ui-smoke-01.log` — full ui-smoke gate (177 passed).
- `C:\GitHub\Berreman\specs\0038\.artifacts\045-ui-tests-01.log` — full ui-tests gate (461 passed).

## Gotchas

- The Materials / Library single-instance semantics live in the module-level
  `WindowLauncher.WindowRegistry` shared across the whole headless session. Any test that opens one
  of these windows MUST `Close()` it (the window's `Closed` hook unregisters the key); a leaked
  registration makes a later test's open ACTIVATE a stale window over the wrong stores. The new
  behaviour tests all close their windows in the same session tick.
- The launcher and the constructor must share ONE `AppContext` for the single-instance proof to be
  meaningful (the registry key is context-independent, but the STORES are not) — the
  launcher-then-strip test builds one `context` and injects it into both.
- The constructor strip's Materials button is a `Border` (not a `Button`), so the strip click uses
  the `WireUiCompositionTests` bounding-rectangle-centre mouse seam, while the launcher's own
  buttons are real `Button`s driven by `RaiseEvent(Button.ClickEvent)`.
