# Step 013 — WIRE_UI — impl-plan

## Anchor and scope

`WIRE_UI` / `depends_on: [3, 5, 6, 7, 10, 11, 12]` / `touches:
[OpticalConstructor.App, OpticalConstructor.Ui.Tests]`. This is the CLOSING
composition acceptance for arc 0040. It is a **structural composition** step: no
new behaviour, no new wiring — the prior steps built every changed surface. This
round finalises the composition and ships a **headless wiring assertion**
(`ui-smoke`) that drives the WHOLE changed surface over ONE app scope composed
exactly like `Startup.context` (`AppContext.create WorkbenchSettings.defaults`),
rendering one frame each without throwing.

## What the prior steps changed (the surfaces to drive)

- **Steps 002/003** — the faceted tree opens **collapsed**; selecting an entry
  leaf paints exactly that row with the chosen fill + a thicker (non-hue) border.
  Both windows (`MaterialsWindowView`, `LibraryWindowView`) project it.
- **Steps 005/006/007** — Anisotropy/Transparency facets total (005); the
  Material editor's **gyration/μ preview tabs restricted** to the applicable
  components (006); the description box **wraps** (`TextWrapping.Wrap` +
  `AcceptsReturn`) (007).
- **Steps 009/010/011/012** — sample `supportedEmission`; the Sample editor
  exposes **R/T emission checkboxes** for a `Plate` and pins **R fixed-on +
  disabled** for a `ThinFilm` (010); a film-less `Plate` sets its substrate
  through the **Materials Select** window via `SetSubstrateButton` (011); a
  placed sample seeds its emission (012).

## Approach

Add ONE new test file `WireUiCompositionTests.fs`'s sibling —
`WireUi0040CompositionTests.fs` — modelled on `WireUiFinalCompositionTests.fs`
(the spec 0038 WIRE_UI closing file). Every proof builds a FRESH
`AppContext.create WorkbenchSettings.defaults` (the exact `Startup.context`
composition; the stores are mutable so per-test isolation matters) and drives
REAL input on the REAL wired views, asserting the resulting semantic-tree
projection:

1. **Materials + Library windows** — open through the REAL App-root ribbon strip
   on `OpticalConstructor.App.MainConstructorWindow` (the `WireUiCompositionTests`
   precedent), assert the tree opens collapsed (chevron present, leaves hidden),
   expand the entries chevron (leaves render), select a leaf and assert it reads
   the chosen fill while a sibling stays idle (the step-002/003 surface). Close
   each opened window before the next — no `WindowRegistry` key leaks.
2. **Material editor** — build `MaterialEditorWindow(ctx.materials, NewMaterial …,
   categories = ctx.categories)`; assert the description box wraps, and the
   Gyration/μ tabs appear exactly when active/magnetic (the step-006/007 surface),
   rendering one frame without throwing.
3. **Sample editor (Plate)** — `EditSample` a film-less `Plate`; assert both R/T
   checkboxes render checked+enabled, a T click clears T and leaves R; then the
   substrate `Set…` verb opens the Materials window in Select over the SAME app
   scope, a pick sets the substrate summary (steps 010/011).
4. **Sample editor (ThinFilm)** — `NewBlankSample` (a film-less ThinFilm); assert
   R is fixed on + disabled and no T control renders (step 010).

## Files

- **New:** `OpticalConstructor.Ui.Tests/WireUi0040CompositionTests.fs` — the
  `ui-smoke` wiring assertions.
- **Edit:** `OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` —
  add the new `<Compile Include=…>` last (it consumes the App composition root +
  every Ui window, like `WireUiFinalCompositionTests`).
- **No change to `OpticalConstructor.App`** — the composition root already
  composes all four windows over `AppContext` / the launcher factory seam; the
  `touches` entry is the composition *confirmation* the wiring assertion makes.

## Risks

- Registry-key leaks across the app-global `WindowRegistry` — every opened window
  (Materials, Library, the editor-opened Materials Select) is closed before the
  next open. Mitigated by the `WireUiCompositionTests` close-discipline.
- Clickability: filter/expand first so target leaves render near the top and are
  effectively visible (the acceptance-003 precedent narrows before selecting).
- `ui-smoke` is a `count_at_least` gate — this round only ADDS facts, so the
  captured count rises above baseline; no existing test is touched.
