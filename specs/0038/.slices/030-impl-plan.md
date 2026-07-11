# Step 030 — impl-plan (IMPLEMENT: unbound-element render cue)

## Goal

Extend the shared renderer's `Drawable` input with a `bindingState : BindingState`
(`BoundElement | UnboundElement | NotBindableElement`) and render an **UnboundElement**
as a colourblind-safe PATTERN cue — a **dashed outline** + a **ghosted (reduced-opacity)
fill** — while **BoundElement** stays solid (as today) and **NotBindableElement** renders
normally. The readout / Details bay text is unchanged.

## Approach

1. **`OpticalConstructor.Ui/ElementRenderer.fs`**
   - Add `type BindingState = BoundElement | UnboundElement | NotBindableElement`.
   - Add `bindingState : BindingState` to the `Drawable` record.
   - Add `bindingStateOf : ElementPlacement -> BindingState` — the host derivation:
     `Lens | FlatMirror | CurvedMirror -> NotBindableElement`; every other kind is
     `BoundElement` when `valueId` is `Some`, else `UnboundElement`.
   - Add private cue constants (`unboundDash`, `ghostFill`) and an `isUnbound` predicate.
   - Refactor the shape renderer's cap polygons through a `capPolygon` helper that dashes
     the outline and scales the fill opacity when unbound.
   - Dash the wireframe renderer's box edges when unbound (no fill there to ghost — the
     dash alone is the pattern cue).

2. **Host construction sites** (must pass the new field so the solution compiles):
   - `OpticalConstructor.Ui/TableAndElementRotationView.fs` (`mainElementViews`) —
     `bindingState = ElementRenderer.bindingStateOf placement`.
   - `OpticalConstructor.TestWindows/RendererTestView.fs` (`mk`) — same. TestWindows is
     outside the slice's `touches` list but is in the solution, so it must compile; this
     is an unavoidable mechanical fix, recorded in the impl-log Gotchas.

3. **`OpticalConstructor.Ui.Tests`** — a new `ElementRendererBindingTests.fs`:
   - Pure: `bindingStateOf` maps each kind/valueId to the right case.
   - Headless (`ui-smoke`): an unbound Sample renders dashed + ghosted, a bound polarizer
     renders solid, a lens renders normally — one frame each without throwing.
   - Register the file in `OpticalConstructor.Ui.Tests.fsproj`.

## Risks

- **New required record field breaks every `Drawable` literal.** Only two literal sites
  exist (both found by grep); copy-updates (`{ e with … }`) keep the field. Handled.
- **`Shape.strokeDashArray` DSL availability.** Confirmed present in the vendored FuncUI
  clone (`DSL/Shapes/Shape.fs`, `double list` overload); the codebase already resolves
  sibling `Shape` members (`Line.stroke`, `Polygon.fill`) through the control modules.
- **Zero-warning / warnaserror:25.** `bindingStateOf` matches all `CatalogueKind` cases
  exhaustively; no new `open`s beyond what is used.
- **LF endings.** Verify no CRLF churn after editing.
