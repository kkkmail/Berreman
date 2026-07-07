# Impl plan — spec 0033, slice 024 (IMPLEMENT: wire the Materials + Library workbench bays)

## What the slice asks

Two new Main-screen ribbon bays in `TableAndElementRotationView` (TestWindows):

1. **BayNames**: `materials = "Materials"` and `library = "Library"` (the samples
   workbench — the label freed by the step-014 Selector rename), added to
   `BayNames.all` and `mainBays`.
2. **Thread `MaterialProxy` + `SampleProxy` into the host model** through
   `initWith`/`initMainWith`; update `OpticalConstructor.App/Program.fs`
   composition mechanically (final WIRE_UI owns composition acceptance).
3. **Flatten** `listMaterials`/`searchMaterials` → `MaterialsControls.Row`s and
   `listSamples`/`searchSamples` → `SampleLibraryControls.Row`s, handlers → Msgs,
   exactly the `libraryState`/`flattenNode` discipline (re-query the proxy on
   every render, so a verb's write shows in the same render pass).
4. **Verbs**: search box + facet selectors drive proxy queries; View shows
   read-only metadata + the step-19 dual-axis n/k chart (materials) / the
   LayerBandsControls band view (samples, as the Details bay renders); Add/Edit
   open the step-022/023 editor windows; Remove is confirm-gated inline;
   `MaterialStillReferenced` surfaces as an inline message naming the
   referencing samples — never a cascade.
5. **Acceptance**: headless tests drive both bays end-to-end by UiIds — search
   filters rows; Add/Edit open the editor windows; removing a referenced
   material surfaces the message and leaves the store unchanged; removing an
   unreferenced entry updates the list in the same render pass.

## Approach

- **Compile-order move (TestWindows fsproj)**: `SampleEditorView/Window.fs` and
  `MaterialEditorView/Window.fs` move BEFORE `TableAndElementRotationView.fs`
  (they depend only on Domain/Controls/NkDispersionChart — verified by grep), so
  the workbench verbs can construct the real editor windows without forward refs.
- **`EditorLaunchers`** (`[<ReferenceEquality>]` function record, the
  functional-proxy convention): `openMaterialEditor` / `openSampleEditor`;
  `defaults` opens the real step-022/023 windows; held in the Model so headless
  tests substitute recording launchers by record update. Window opening is a
  side effect performed in `update` — the `openChartWindowHook` precedent.
- **Model** gains: `materials`, `samples` (the two write seams),
  `materialQuery : MaterialQuery` / `sampleQuery : SampleQuery` (elevated query
  DATA driving `searchMaterials`/`searchSamples`), `selectedMaterial` /
  `selectedSample`, `materialRemoveConfirm` / `sampleRemoveConfirm`
  (`RemoveConfirm<'id>` two-case DU carrying the id — a selection change between
  Remove and Confirm can never delete a different entry), `materialsError` /
  `samplesError` (typed store errors rendered as the inline message),
  `viewedMaterial` / `viewedSample` (View-panel toggles), `launchers`.
- **Msg**: `Mat…` × 10 and `Smp…` × 10 arms (search text, facet picks carry the
  DOMAIN facet — the code→facet mapping happens in the handlers at the control
  boundary; row select carries the elevated id; Add/Edit/View;
  Request/Confirm/Cancel remove). `SmpMakeMultilayer` opens the sample editor on
  a NEW sample (the stack editor's fold vocabulary IS the multilayer flow) —
  recorded as a decision.
- **Projections** (public, the `experimentState` precedent): `materialsState`,
  `samplesState` re-query the proxy per render; row editability from
  `complexity` (`None` ⇒ ViewOnly). Facet code maps public for tests.
- **Bay content**: shared control view + host-added inline confirm row, inline
  message row (`TextBlock` per bay: `MaterialsWorkbenchMessage` /
  `SamplesWorkbenchMessage`), and the View panel (`MaterialViewPanel` with the
  n/k chart; `SampleViewPanel` with `LayerBandsControls` over
  `sampleBandsState` — extracted from `detailsState` so both reuse it).
- **n/k chart reuse**: REAL-MOVE `MaterialEditorView.previewCanvas` into
  `NkDispersionChart.inlineCanvas (autoId) (chart)` (same file family,
  TestWindows); `MaterialEditorView` re-points; the Materials View panel calls it
  with its own automation id over `MaterialEditorView.previewRange`.
- **App/Program.fs**: build `SampleProxy.createInMemory()` +
  `MaterialProxy.createInMemory (samplesReferencing samples)` in
  `MainConstructorWindow`, pass through the new `initMainWith` signature. Needs
  `open OpticalConstructor.Domain.Library` (the `MaterialProxy.createInMemory`
  augmentation is an optional extension living in module `Library`).
- **Default stores for the test scenes**: a nested `module private
  DefaultStores` (scoped `open …Domain.Library`) so `init`/`initMain` keep
  working parameterless.

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/OpticalConstructor.TestWindows.fsproj` (compile order)
- `…TestWindows/NkDispersionChart.fs` (+ `inlineCanvas`)
- `…TestWindows/MaterialEditorView.fs` (re-point `previewCanvas`)
- `…TestWindows/TableAndElementRotationView.fs` (the bulk)
- `…OpticalConstructor.App/Program.fs` (mechanical composition)
- `…OpticalConstructor.Ui.Tests/LibraryControlsTests.fs`,
  `ExperimentControlsTests.fs` (the "no bay named Library" / exact-roster pins
  are consciously obsoleted by this step — Library returns as the samples
  workbench)
- `…OpticalConstructor.Ui.Tests/MainWorkbenchTests.fs` (NEW) + fsproj entry

## Tests (TDD: red capture first)

Pure (`ui-tests`): bay names/roster; materialsState flattening + editability;
search/facet filtering (materials + samples); select/request/confirm/cancel
remove; referenced-remove → `MaterialStillReferenced` in `materialsError`, store
unchanged; unreferenced remove → row gone from the SAME model's projection;
launcher recording for Add/Edit/MakeMultilayer; View toggles; `sampleBandsState`
bands (the ×N collapse).

Headless (`ui-smoke`, real Elmish loop via `HostWindow` + `mkSimple` over an
injected model — clicks re-render): ribbon tab → search filters rows in the
tree; Add/Edit open the REAL editor windows (recording launchers that construct
them); referenced-material remove surfaces the message naming a referencing
sample with the store + list unchanged; unreferenced material AND sample removes
drop the row in the same render pass; View panels render (chart canvas id /
band 0 inside `SampleViewPanel`). Rows are clicked after narrowing the search
(rows deeper than the list's 220 px scroll viewport can't be clicked headless —
the 022 lesson).

## Risks

- The step-014 pins (`DoesNotContain "Library"`) fail until updated — planned.
- Two `LayerBandsControls` instances (Details bay + samples View panel) share
  control names; the panel only renders when `viewedSample` is set, and its
  assertions are scoped to `SampleViewPanel` descendants.
- Guid-ordered rows make deep-row clicks fragile headless — mitigated by
  searching first.
- `MaterialProxy.createInMemory` is an optional type extension: callers must
  open `OpticalConstructor.Domain.Library` (scoped nested module in the view
  file; a plain open in Program.fs).

## Gates

build, unit-tests (119), constructor-unit-tests (407), ui-smoke (76 + new),
ui-tests (294 + new) — run locally as ADVISORY per Invariant 6; logs to
`specs/0033/.artifacts/024-<gate>.log`.
