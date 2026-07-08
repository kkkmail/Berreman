# 019 — impl-plan (WIRE_UI — composition acceptance for the category catalogue)

## Goal

Slice 019 is arc 0035's `WIRE_UI` step. It owns the **composition acceptance**
for the category catalogue that slice 009 host-wired: the launcher-composed real
`MainConstructorWindow` must wire the in-memory `CategoryProxy` at the root beside
the material / sample / library / experiments proxies, and the `ui-smoke` suite
must render the reordered full-surface Materials and Library bays and open **all
three** editor windows (Material / Sample / **Category**) without throwing.

## What is already in place (do not re-wire)

- `OpticalConstructor.App/Program.fs:138-145` already builds the five in-memory
  proxies — `SampleProxy` → `MaterialProxy` (over `samplesReferencing`) →
  `CategoryProxy` (over `materialsReferencingCategory`) beside `library` /
  `experiments` — and injects all five through `initMainWith`. Slice 009 landed
  this mechanically; step 019 confirms it final (comment-only touch, the 0033/026
  precedent).
- `initMainWith` / `initWith` / `DefaultStores` already thread `categories`
  (TableAndElementRotationView.fs).
- The Materials bay already carries the host-added **Categories…** verb
  (`WorkbenchIds.categoriesButton = "ManageCategoriesButton"`) that dispatches
  `MatOpenCategories` → `EditorLaunchers.defaults.openCategoryEditor model.categories`
  → `CategoryEditorWindow(categories).Show()`.
- `WireUiCompositionTests` already drives the real window for the reordered bay
  sweep and the Material + Sample editors, and proves the materials↔samples
  root coupling.

## The gap this round closes

`WireUiCompositionTests` covers only two of the three editor windows. It does not
yet exercise the **Category** editor from the composition root, so the category
proxy's root-wiring is not asserted end-to-end. This slice adds that proof.

## Files to modify

1. `OpticalConstructor.Ui.Tests/WireUiCompositionTests.fs` — add one
   `[<Trait("Category","ui-smoke")>]` fact: mount the real `MainConstructorWindow`,
   subscribe to the public `Window.WindowOpenedEvent`, click the Materials tab
   then the **Categories…** verb, and assert exactly the real `CategoryEditorWindow`
   (`CategoryEditorView.UiIds.window`) opens visible and without throwing — the
   launcher passed `model.categories`, the root-composed proxy, so this proves the
   `CategoryProxy` is wired at the root beside the material/sample proxies. Refresh
   the module doc-comment to name the five proxies / three editors.
2. `OpticalConstructor.App/Program.fs` — comment-only: update the composition
   comments to say five proxies inject through `initMainWith` and that step 019
   delivers the acceptance covering all three editors. No behavioural change.

## Risks

- Per WIRE_UI **Invariant 6** I ACT only — I do NOT run build/test/ui-smoke gates;
  the arc-runner gate engine runs them after I exit. Mitigation: the new fact
  mirrors the proven Material/Sample editor blocks in the same file verbatim (same
  `mountRoot` / `Window.WindowOpenedEvent` / `clickOn` seam), so compile + runtime
  risk is minimal.
- The Categories… verb is a clickable `Border` (host `workbenchButton`), the same
  shape `clickOn` already drives for `MaterialsControls.UiIds.addButton` and the
  confirm buttons — no new interaction pattern.

## Gate baseline (from step 018 / checkpoint 017)

berreman_unit_tests 119 · constructor_unit_tests 447 · ui_smoke_tests 105 →
**106** (one new fact) · ui_tests 329.
