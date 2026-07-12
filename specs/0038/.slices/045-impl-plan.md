# 045 — IMPLEMENT: finalize the product launcher (Main / Inverse / Materials / Library)

## Goal

Turn the single-button product launcher (`LauncherWindow`, Main only) into the FOUR-button
launcher the spec ends on: **Main / Inverse / Materials / Library**, in that order, each with a
stable `Name` + `AutomationId`.

- **Main** — opens the constructor as today (`MainConstructorWindow`, forward `initMainWith`).
- **Inverse** — opens the constructor in the step-037 inverse state (`initInverse`).
- **Materials / Library** — open the step-013/015 single-instance windows through the SAME
  app-scoped launcher seam the constructor strip uses (`EditorLaunchers.defaults`), so a window
  opened from the launcher and the same window opened from the strip meet in ONE open-or-activate
  space (the shared host-layer `WindowRegistry` under `MaterialsWindowKey` / `LibraryWindowKey`).

## Files to modify

- `OpticalConstructor.App/Program.fs`
  - Add `open Avalonia.Automation`.
  - Add a public `LauncherIds` constants module (four `[<Literal>]` button ids).
  - Add a private `ConstructorScene.mount` helper (sizes the window + runs `mainView` from a seed) —
    shared by the two constructor windows so Main / Inverse do not duplicate the mount.
  - Refactor `MainConstructorWindow` to use `ConstructorScene.mount` (behaviour unchanged: same
    title, size, forward seed).
  - Add `InverseConstructorWindow` (inverse seed, distinct title `Optical Constructor — Inverse`).
  - Rewrite `LauncherWindow` with the four buttons over the injected `context`; Materials / Library
    reuse `TableAndElementRotationView.EditorLaunchers.defaults`.
- `OpticalConstructor.Ui.Tests/LauncherTests.fs`
  - Update the fixed-size assertion (taller form).
  - Structure (`ui-tests`): four buttons present in order; each carries its stable `Name`,
    `AutomationId`, and label.
  - Behaviour (`ui-smoke`): Main opens `MainConstructorWindow`, Inverse opens
    `InverseConstructorWindow`, Materials opens the Materials window, Library opens the Library
    window; **Materials from the launcher then from the constructor strip yields ONE window**.

## Risks / decisions

- **Distinct window type for inverse.** A separate `InverseConstructorWindow` type (vs a mode flag)
  makes the inverse open observable in a headless test (`opened :? InverseConstructorWindow`) and
  reads as prose; the shared `ConstructorScene.mount` keeps it DRY.
- **Launcher ids centralized in the App project.** The slice `touches` are `App` + `Ui.Tests`, so
  the product-launcher ids live in a new `LauncherIds` module in `Program.fs` (not the Controls
  `UiIds` module, which is out of scope). They are distinct from the strip's
  `OpenMaterialsWindowButton` / `OpenLibraryWindowButton` (different controls, same target windows).
- **Registry hygiene.** Every Materials/Library window a test opens is `Close()`d so the shared
  module-level `WindowRegistry` does not leak a registration into a later test (the
  `WireUiCompositionTests` discipline).
- Gates are run by the arc-runner after exit; this worker only implements + writes outputs.
