# State of the world — step 030 (IMPLEMENT: unbound-element render cue)

## Where we are

Spec 0038's Optical Constructor UI arc. The shared optical-element renderer
(`OpticalConstructor.Ui/ElementRenderer.fs`, moved out of TestWindows in an
earlier step) draws every element for every screen through ONE `Drawable`
input. Earlier steps grew the palette to add PRE-BOUND polarizers and UNBOUND
samples (`valueId : string option` on `ElementPlacement`); the Library/Details
bays already state each element's binding in text. This step makes the binding
VISIBLE on the schematic: an unbound element now reads as dashed-and-ghosted,
a bound one stays solid, and a not-bindable lens/mirror renders normally — a
colourblind-safe PATTERN cue, not a hue change. No model/behaviour change; the
Details text is untouched.

## What's working

- Add `BindingState = BoundElement | UnboundElement | NotBindableElement` and a
  `bindingState` field on the renderer's `Drawable`, derived by the host with
  `bindingStateOf` (Lens/FlatMirror/CurvedMirror are NotBindable; every other
  kind is Bound when `valueId` is Some, Unbound when None).
- Draw an UnboundElement with a dashed outline + a ghosted (0.3×-opacity) fill in
  the shape renderer's cylinder caps; a BoundElement stays solid as before.
- Dash an unbound element's box edges in the wireframe renderer too (no fill to
  ghost there — the dashed pattern alone is the cue), keeping the always-solid
  N1/N2 normals solid.
- Derive `bindingState` at both host draw sites (Main scene + renderer test scene);
  no `Drawable` literal is left without the field.
- Add `ElementRendererBindingTests.fs`: a pure `bindingStateOf` proof plus headless
  render proofs that unbound draws dashed+ghosted, bound solid, and a lens normal —
  one frame each without throwing — in both renderer looks.

## Tests

Per the IMPLEMENT act-only invariant (Invariant 6), the worker ran NO gates this
round; the arc-runner's gate engine is the sole authority. Static verification
only: the new render code mirrors the surrounding shape/wireframe draw idioms;
`Shape.strokeDashArray`'s `AvaloniaList<double>` overload is confirmed present in
the vendored FuncUI clone; matches are exhaustive; the only two `Drawable` literals
both carry the new field; every touched/new file is pure LF.

Expected roster (engine-run): `build`, `unit-tests`, `constructor-unit-tests`,
`ui-smoke`, `ui-tests`. This round adds 3 `[<Fact>]` cases in one new Ui.Tests file:
+2 under `ui-smoke` (158 → 160) and +1 under `ui-tests` (425 → 426). The solver /
constructor gate counts are untouched.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 623
  ui_smoke_tests: 160
  ui_tests: 426
```

## Architecture

- `bindingState` is pure data on the `Drawable` input, derived by the host via
  `bindingStateOf : ElementPlacement -> BindingState`; the renderer is a pure
  projection of it. This keeps the cue logic testable without a window (the pure
  `bindingStateOf` fact) and keeps the draw functions the single source of the look.
- The visual difference is a PATTERN (dash) + opacity (ghost), never a hue, so it is
  colourblind-safe; the readout/Details text remains the always-available textual
  statement of the binding.
- The shape-renderer caps route through a new `capPolygon` helper (the one place that
  decides dashed/ghosted), so the two cap draws stay identical minus the cue and a
  future style tweak is one edit.

## Deferred

- Nothing from this slice is deferred. The cue is not applied to the spherical
  lens/curved-mirror cap surfaces (`capSurface`) because those kinds are
  NotBindableElement by definition and never carry the unbound state — intentional,
  not a gap.

## Gotchas

- Adding a required `bindingState` field to the SHARED `Drawable` forced a one-line
  compile-fix in `OpticalConstructor.TestWindows/RendererTestView.fs` (`mk`), which is
  outside the slice's `touches: [Ui, Ui.Tests]` but in the solution the `build` gate
  compiles. Unavoidable mechanical fix, not scope creep.
- The slice's NotBindable set is exactly {Lens, FlatMirror, CurvedMirror}; so
  LightSource/Detector follow the `valueId` rule and draw dashed while unbound. This is
  the literal reading of the slice, chosen deliberately.
- Used the `AvaloniaList<double>` overload of `Shape.strokeDashArray` (direct SetValue),
  not the `double list` overload whose setter reads a null-by-default `StrokeDashArray`.
- The task file's system-prompt path is stale; the file lives under
  `.../src/ai_strategy_generator/multistep/implement_worker.system-md`. No scope impact.

## Changelog

- 2026-07-11 — step 030: added `BindingState` + `bindingState` on the shared renderer
  `Drawable` and `bindingStateOf`; drew unbound elements dashed-and-ghosted (shape
  renderer caps) and dashed (wireframe box edges), leaving bound solid and lenses/mirrors
  normal; wired the field at both host draw sites (+ the TestWindows scene); added
  `ElementRendererBindingTests.fs` (1 pure + 2 headless facts) and registered it in the
  Ui.Tests fsproj. Details/readout text unchanged.
