# Step 014 — state of the world

## Where we are

Step 014 of spec 0035 gives the Library (samples workbench) bay's **Make-multilayer** verb its own
launcher path. Before this slice `SmpAdd` and `SmpMakeMultilayer` shared one update arm that opened
a blank Sample editor; the editor's opening mode was a bare `Sample option` (`None` = blank Add,
`Some s` = Edit). This slice adds the third mode — a NEW sample pre-seeded with a foldable starter
multilayer period — by elevating that `Sample option` to a named 3-case DU `SampleEditorIntent`
(`NewBlankSample | NewSeededMultilayer | EditSample of Sample`), threaded through `init`, the
`SampleEditorWindow` ctor, and the `EditorLaunchers.openSampleEditor` seam. The seed itself is a
pure Domain value (`Library.starterMultilayerStructure`). It touches `OpticalConstructor.Domain`,
`OpticalConstructor.TestWindows`, and `OpticalConstructor.Ui.Tests`.

## What's working

- Give `SmpMakeMultilayer` a distinct launcher path — split from the blank `SmpAdd`.
- Open Make-multilayer onto a NEW sample seeded with one `Repeated` 2-layer period, ready for the
  `SampleStackEditor` K-stepper, while blank Add still opens empty and Edit updates in place.
- Elevate the editor's `Sample option` open-mode to a named `SampleEditorIntent` DU (no naked bool).
- Add the pure Domain seed `Library.starterMultilayerStructure` (glass 1.52 / vacuum λ/4 cell).
- Keep Save minting a fresh `SampleId` for both NEW intents through `SampleProxy.addSample`.
- Add a pure contract test and two headless ui-smoke acceptance tests.

## Tests

- Gates are executed by the arc-runner's deterministic gate engine after this worker exits
  (IMPLEMENT Invariant 6 — the worker acts, it runs no checks). This session ran no gate commands.
- Step 014 roster: `build`, `unit-tests`, `constructor-unit-tests`, `ui-smoke`, `ui-tests`. The
  change is exercised by `ui-tests` (one new pure `SampleEditorWindowTests` fact — the seeded init)
  and `ui-smoke` (two new headless facts — one drives the real `SampleEditorWindow` on the seeded
  intent and Saves; one drives the Main workbench's Make-multilayer button end-to-end and Saves).
  `build` covers the Domain/view/window/launcher signature changes.
- `unit-tests` (Berreman) and `constructor-unit-tests` (Domain) are behaviourally untouched — the
  Domain change is one new pure value referenced only by the view.
- Deferred: none — the slice's whole surface (distinct path + seeded init + Save + tests) landed.

## Architecture

- **The open-mode is a named DU, not a `Sample option` + bool.** `SampleEditorIntent` gives the
  three intents (blank / seeded / edit) their own names at one seam. Both NEW intents map to the
  existing `EditorTarget.NewSample` (Save mints a fresh id); they differ only in the seeded
  structure, so the Save path is unchanged — the DU is purely about what the editor opens onto.
- **The seed is pure Domain DATA.** `Library.starterMultilayerStructure` is a `SampleStructure`
  (one `Repeated` `PeriodGroup` of a 2-layer cell), reusing the existing private λ/4 thicknesses of
  the `multilayerQw` seed. It is NOT a seeded `Sample` — no id, no name — so the editor opens a
  brand-new sample over it and testability stays at the Domain/`init` level, window-free.
- **One seam, not two launchers.** The distinct path reuses the single `openSampleEditor` field by
  passing a different intent, rather than adding a parallel launcher field — the launcher stays one
  function, and the three-way match is exhaustive (compiler-guided for future intents).

## Deferred

- Nothing deferred for later slices from this step.

## Gotchas

- **`SampleEditorIntent` cases are qualified in `MainWorkbenchTests`.** That file opens
  `TableAndElementRotationView`, whose `UiIds`/`Msg`/`Model` would shadow `SampleEditorView`'s, so
  the intent cases and `SampleEditorView.UiIds.*` are written qualified there (unqualified in
  `SampleEditorWindowTests`, which opens `SampleEditorView`).
- **Seeded Save needs a non-blank name.** The seeded structure is non-empty, so `validateSample`
  only rejects a blank name — the headless Save tests set the name box first.
- **Starter count = 1.** "a foldable starter **period** … ready for the K-stepper" reads as one
  minimal period the group's inline K-stepper (`SetRepeatCount`) then builds up. Documented in the
  impl-log; a trivial change if a reviewer prefers a ≥2 starter.

## Changelog

- 2026-07-08 — Step 014 (IMPLEMENT): gave the Library bay's Make-multilayer verb a distinct
  launcher path opening a NEW sample seeded with a foldable 2-layer `Repeated` period; elevated the
  Sample editor's `Sample option` open-mode to a named `SampleEditorIntent` DU threaded through
  `init` / the window ctor / the launcher seam; added the pure Domain seed
  `Library.starterMultilayerStructure`; Save still mints a fresh `SampleId` via `addSample`; added a
  pure contract test and two headless ui-smoke acceptance tests.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 446
  ui_smoke_tests: 98
  ui_tests: 329
```
