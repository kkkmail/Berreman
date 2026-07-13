# Step 013 — impl plan (ADD_COMPONENT UICOMP_XDUO_0009 MaterialsWindow)

## Approach

Instantiate the step-012 `FacetedTreeControls` over the step-011 material
facet catalogue as the single-instance Materials WINDOW, remove the Materials
bay from the ribbon, and give the ribbon tab-strip row a right-aligned
`Materials…` button that opens the window through the step-008 WindowLauncher
under `MaterialsWindowKey`.

1. **`Ui/MaterialsWindowView.fs` (new, pure MVU)** — the window's model
   (`MaterialsWindowContext` = proxies + `TreeAutoBuildThreshold` + injected
   editor launchers; applied facet constraints in application order; committed
   text query; active representation; sticky build request; selected entry;
   window-local remove-confirm gate; last `MaterialError`), the update (verbs
   Add / Edit / Remove / Categories… + facet apply/remove/filter/representation/
   select/build), and the projection of the LIVE `MaterialProxy` corpus through
   the step-009 engine (`Facets.filter` / `countFor` / `breadcrumbCounts` /
   `buildTree`) over the step-011 `materialFacets` — with the category def's
   extractor swapped to resolve names through the LIVE `CategoryProxy` (so a
   rename re-labels the facet; the seeded `categoryName` is static). The view
   = `FacetedTreeControls.view` (offers apply constraints; the tree carries the
   engine facet/branch nodes plus a `Materials (N)` node whose entry leaves
   select the view-panel target) beside the view panel (metadata + the shared
   `EmbeddedChart` n/k chart) and the verbs row with the inline confirm gate
   and the typed `MaterialStillReferenced` block. Every projection re-queries
   the proxies, so any verb's write shows in the same render pass.
2. **`Ui/MaterialsWindow.fs` (new)** — the `HostWindow` composition root
   (`MaterialEditorWindow` precedent): real launchers open the Material /
   Category editors through `WindowLauncher.create` under the step-008 editor
   keys (`MaterialEditorKey id` / `CategoryEditorKey`), Browse-mode.
3. **`Ui/TableAndElementRotationView.fs`** — drop `materialsBay` from
   `mainBays`, `BayNames.materials` from the roster, the `Mat…` messages/arms,
   the bay-only model fields (`materialQuery` / `selectedMaterial` /
   `materialRemoveConfirm` / `materialsError` / `viewedMaterial`), the bay
   projections/handlers/rows and their code-map helpers, and the two
   editor-launcher fields only the bay used; `EditorLaunchers` gains
   `openMaterialsWindow` (defaults → `MaterialsWindow` via the launcher under
   `MaterialsWindowKey`); `mainControlBar` gains the right-aligned strip
   button dispatching the new `OpenMaterialsWindow` message.
4. **App** — no structural change needed: the factory threads through
   `EditorLaunchers.defaults` over the model's injected app-scope proxies
   (the step-008 precedent); the `MainConstructorWindow` doc comment is
   updated. Solution builds; step 47 owns composition acceptance.
5. **Tests** — new `MaterialsWindowTests.fs` (pure contract + headless
   acceptance: the strip button opens ONE window / second click activates,
   facet constraints narrow the corpus, verbs over the shared app-scope
   stores, a category rename re-labels the category facet, remove-confirm
   discipline, gating). Rework the bay-coupled tests instead of deleting:
   `MainWorkbenchTests` (Materials halves move to the window suite, samples
   halves stay), `MainSceneMsgTests` (Mat* disarm tests → window-model
   equivalents), `WireUiCompositionTests` (Materials assertions → the real
   window through the real strip button), `AppContextTests` (two-surface
   proof via the Library bay), `EmbeddedChartTests` (View-panel host site →
   the window's view panel).

## Files

- add `Berreman/OpticalConstructor/OpticalConstructor.Ui/MaterialsWindowView.fs`
- add `Berreman/OpticalConstructor/OpticalConstructor.Ui/MaterialsWindow.fs`
- edit `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`
- edit `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- edit `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs` (comment-level)
- add `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialsWindowTests.fs`
- edit `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/{OpticalConstructor.Ui.Tests.fsproj,
  MainWorkbenchTests.fs, MainSceneMsgTests.fs, WireUiCompositionTests.fs,
  AppContextTests.fs, EmbeddedChartTests.fs}`

## Risks

- **Baseline counts** (ui_tests 363 / ui_smoke 128): every deleted bay test is
  replaced 1:1+ in the window suite; net counts must not regress — tally
  before finishing.
- **App-global WindowRegistry**: the real strip button registers the window
  under `MaterialsWindowKey`; every test that opens it must close it.
- **Category rename**: `materialFacets`' category def reads the static seeded
  catalogue — the window swaps in a live-`CategoryProxy` extractor keyed by
  the SAME facet key (recorded interpretation).
- **FuncUI keyed alternates** and Enter double-dispatch — reuse the step-012
  control as-is; the window adds no text-change subscriptions.
