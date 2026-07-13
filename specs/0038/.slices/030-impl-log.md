# Step 030 — impl-log (IMPLEMENT: unbound-element render cue)

## Progress

- [x] `BindingState` DU + `bindingState` field on `Drawable` (ElementRenderer.fs)
- [x] `bindingStateOf` host-derivation helper
- [x] dashed + ghosted unbound cue in the shape renderer (cap polygons via `capPolygon`)
- [x] dashed unbound cue in the wireframe renderer (box edges)
- [x] host construction sites updated (TableAndElementRotationView, RendererTestView)
- [x] headless render tests + fsproj registration
- [x] LF-ending check (all touched/new files are pure LF — 0 CR bytes)

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ElementRenderer.fs`
  - `open Avalonia.Collections` (for `AvaloniaList`).
  - New `type BindingState = BoundElement | UnboundElement | NotBindableElement`.
  - `Drawable` gains `bindingState : BindingState`.
  - `bindingStateOf : ElementPlacement -> BindingState` — the host derivation.
  - Private `unboundDash` (dash pattern), `ghostFill` (0.3 fill-opacity factor),
    `isUnbound`, and a `capPolygon` helper that dashes the outline + ghosts the fill
    when unbound.
  - `cylinderViews` now draws its two caps through `capPolygon` (so every bindable
    kind — all cylinders — shows the cue); `wireframeRenderer` dashes the box edges
    when unbound.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
  - `mainElementViews` derives `bindingState = ElementRenderer.bindingStateOf placement`
    from the drawn placement (recomputed each render, always fresh).
- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/RendererTestView.fs`
  - `mk` sets `bindingState = ElementRenderer.bindingStateOf placement` (see Gotchas —
    outside the slice's `touches`, but a required compile-fix for the shared type).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ElementRendererBindingTests.fs`
  - NEW. Pure test of `bindingStateOf` (ui-tests) + two headless render proofs
    (ui-smoke): the shape renderer draws unbound dashed+ghosted / bound solid / lens
    normal; the wireframe renderer dashes an unbound box and leaves a bound one solid.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`
  - Registers the new test file after `RendererControlsTests.fs`.

## Testing state

Per the IMPLEMENT act-only invariant (Invariant 6), the worker ran NO gates this
round — the arc-runner's gate engine is the sole authority. Static verification only:

- New render code mirrors the surrounding shape/wireframe draw idioms; the dashed
  attribute uses the FuncUI `Shape.strokeDashArray` `AvaloniaList<double>` overload
  (confirmed present in the vendored FuncUI clone `DSL/Shapes/Shape.fs`).
- All matches are exhaustive (`bindingStateOf` covers every `CatalogueKind`; the test
  helpers have `_`/`[]` fall-throughs); no new unused opens beyond `Avalonia.Collections`
  (used by `AvaloniaList`).
- Only two `Drawable` record literals exist (grep-verified); both carry the new field.
  Copy-updates (`{ e with … }`) keep it.
- Every touched/new file is pure LF (verified with `tr -cd '\r' | wc -c` = 0; git index
  is `eol=lf`).

`commit_ready: true` — every requirement of the slice is addressed this round.

## Artifacts

None — pure code + headless tests; no captured logs/screenshots this round.

## Gotchas

- **TestWindows edited though outside `touches: [Ui, Ui.Tests]`.** Adding a required
  `bindingState` field to the SHARED `ElementRenderer.Drawable` breaks its only other
  record literal (`RendererTestView.mk` in `OpticalConstructor.TestWindows`, which is in
  the solution the `build` gate compiles). Updating it is an unavoidable mechanical
  compile-fix, not scope creep; the diagnostic scene's binding state is inert (its
  `valueId` never changes).
- **NotBindable set is exactly {Lens, FlatMirror, CurvedMirror}.** The slice enumerates
  only those three, so LightSource/Detector follow the `valueId` rule and render as
  UnboundElement when `valueId = None` (the seeded Main source/detector draw dashed).
  This is the literal reading of the slice; chosen deliberately over "sources/detectors
  are also not bindable", which the slice does not say.
- **Cue applied in BOTH renderers.** The slice's "dashed outline + ghosted fill" is
  fill-centric (shape renderer). The wireframe look has no fill, so its cue is the
  dashed box edges alone — still a colourblind-safe PATTERN difference. Extending it to
  wireframe keeps the cue renderer-independent (the app default is Wireframe), matching
  the "PATTERN difference, not hue" intent; the always-solid N1/N2 normals are left
  solid so orientation still reads cleanly.
- **`strokeDashArray` overload.** Used the `AvaloniaList<double>` overload (a direct
  `SetValue`) rather than the `double list` overload, whose setter reads
  `x.StrokeDashArray` — null by default on a fresh Shape, an NPE risk.
- **`bindingState` is derived, not stored state.** It is a pure function of the
  placement's `valueId`/kind; the host recomputes it at draw time (fresh every render in
  the Main scene). The Details bay / readout keep stating the binding in text — that path
  is untouched.
- **Stale system-prompt path in the task file.** It names
  `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md`; the file actually lives at
  `.../src/ai_strategy_generator/multistep/implement_worker.system-md`. Read there; no
  scope impact.
