# Step 010 — state of the world

## Where we are

Spec 0040 Part D, step 010 (IMPLEMENT) — the D.2 rule "expose a sample's supported
emission in the editor". Step 9 gave `Sample` a geometry-constrained
`supportedEmission : Emission` field and pinned the invariant (a `ThinFilm` is
`EmitReflectedOnly` and unclearable; a `Plate` defaults to `EmitBoth` and is
constrainable). This step surfaces that field in `SampleEditorView`: a `Plate`
editor gets R and T checkboxes that constrain emission to R-only or T-only through
the existing `Emission.withReflected` / `withTransmitted` setters (never neither),
while a `ThinFilm` editor shows R fixed on + disabled and no T control. Scope is
`OpticalConstructor.Ui` + `OpticalConstructor.Ui.Tests` (plus the shared `UiIds`
constants module); `depends_on: [9]`.

## What's working

- Add `supportedEmission` to the editor `Model`, seed it in `init` from the edited
  sample (or the geometry default for a new one), and normalise it through
  `constrainEmission` so the model starts consistent.
- Carry the edited emission into `toSample` and the dirty snapshot
  (`SampleEditSnapshot` / `currentEdit`) so an R/T change marks the editor dirty.
- Render R and T checkboxes (`emissionRow`) driven by `Emission.withReflected` /
  `withTransmitted`: a `Plate` shows both; a `ThinFilm` shows R fixed on + disabled
  and no T box. Re-constrain emission on a geometry flip (`SetSubstrate`).
- Give the checkboxes stable `UiIds.SampleEditor.emitReflectedCheck` /
  `emitTransmittedCheck` AutomationIds; wire them loop-free (one-way `isChecked` +
  user-only `Button.onClick`).
- Add 4 pure `update` facts and 2 headless `ui-smoke` proofs for the Plate toggle /
  never-neither / ThinFilm-fixed behaviour, plus 2 id-contract assertions.

## Tests

Per the IMPLEMENT worker Invariant 6, gate execution belongs to the arc-runner's
deterministic gate engine after this session exits; this worker runs no gates. The
step's roster and how the change lands against each:

- `build` — VERIFIED green this round (`dotnet build Berreman.slnx -c Release` →
  `Build succeeded. 0 Error(s)`; the only warnings are the exempt `NU1701` Wolfram
  advisory). Attempt 01 had failed here on two `FS0039` errors: the CheckBox attributes
  were qualified with base-class module names that are not in scope
  (`ToggleButton.isChecked`, `InputElement.isEnabled` — `Avalonia.Controls.Primitives` /
  `Avalonia.Input` are not opened). Fixed by qualifying all four through the in-scope
  `CheckBox` class (`CheckBox.isChecked` / `.isEnabled` / `.content` / `.onClick`), which
  inherits them — the `Border.isEnabled` precedent. No behaviour/test change; only the
  four attribute qualifiers.
- `unit-tests` (BerremanTests) — solver core, untouched; count unaffected.
- `constructor-unit-tests` (OpticalConstructor.Tests) — Domain/Storage untouched; count
  unaffected.
- `ui-smoke` — 2 net-additive headless facts (`[<Trait Category=ui-smoke>]`): a Plate
  editor renders both checkboxes, a T click clears T leaving R and dirties the editor; a
  ThinFilm editor shows R fixed on + disabled and no T control. Count rises by 2.
- `ui-tests` — 4 net-additive pure `update` facts + 2 id-contract asserts on an existing
  test. Count rises.

Acceptance mapping (D.2): "a Plate sample editor MUST let the user constrain emission to
R-only or T-only via the checkboxes (never neither, marking the editor dirty)" →
`a Plate editor toggles T off leaving R on and marks the editor dirty`,
`clearing both emission groups is impossible on a Plate`, and the headless
`acceptance (010): a Plate editor renders both emission checkboxes; a T click clears T,
leaves R, and dirties the editor`. "a ThinFilm editor MUST show R fixed on" →
`a ThinFilm editor pins R on under any emission message and a geometry flip re-imposes it`
and the headless `acceptance (010): a ThinFilm editor shows the R emission checkbox fixed
on and disabled, with no T control`.

```yaml
gates:
  berreman_unit_tests: 0
  constructor_unit_tests: 0
  ui_smoke_tests: 0
  ui_tests: 0
```

## Architecture

- **Reuse the step-9 enforcement point.** Every emission write in the editor routes
  through the shipped `constrainEmission m.substrate (…)` over the `Emission` smart
  setters — no new emission logic, no parallel type. The "both off" state stays
  unrepresentable and a `ThinFilm` stays `EmitReflectedOnly` even if a stray message
  reaches its update arm.
- **Model is the single consistent source.** `init` normalises the seed emission,
  `SetSubstrate` re-constrains on a geometry flip, and the R/T arms re-constrain on each
  toggle — so `m.supportedEmission` is always geometry-valid and `toSample` can carry it
  verbatim while the dirty snapshot tracks it faithfully.
- **Loop-free checkbox wiring.** `ToggleButton.isChecked` is one-way from the model; the
  dispatch rides `Button.onClick` (user activation only), dispatching `not <render-time
  value>`. No `onIsCheckedChanged` feedback path — the ui-smoke render-loop hazard is
  avoided by construction.
- **No serialized-contract or versioning change.** `Sample` is an in-memory value and
  `SampleStore.payloadOf` derives nothing from `supportedEmission`; carrying the edited
  value needs no store/schema touch (unchanged from step 9).

## Deferred

- Consuming a sample's `supportedEmission` downstream (the beam-tree / engine mapping that
  actually suppresses a group when a sample emits reflected-only or transmitted-only). This
  slice only exposes the editor control; the constraint stays a Domain property.
- A richer emission affordance (e.g. a radio pair or a tri-state summary) if UX later wants
  it; the two checkboxes over the `Emission` setters are the minimal, invariant-honest surface.

## Gotchas

- **FuncUI attribute qualifiers must name an in-scope control class (attempt-01 build
  break).** FuncUI 2.0-preview1 augments each Avalonia control *class* with static members
  for its own + inherited attributes, so `X.attr` resolves only when class `X` is opened.
  `Avalonia.Controls` is opened (→ `CheckBox` / `Border` / `Button` / `ContentControl`);
  `Avalonia.Controls.Primitives` (`ToggleButton`) and `Avalonia.Input` (`InputElement`)
  are not. Qualify CheckBox attributes through the in-scope `CheckBox` class, which
  inherits them — never through `ToggleButton` / `InputElement`. Do not open the extra
  namespaces just to name a base module (a `Vector3` collision hazard).
- **`SetSubstrate` re-constrains emission — a deliberate, unnamed-in-spec decision.**
  Without it a Plate set to T-only then switched to ThinFilm would leave the R box
  unchecked (violating "R fixed on") and desync the dirty snapshot from what `toSample`
  saves. A pure test pins the flip.
- **`Placement.Emission` is qualified, not opened.** Opening `Placement` would collide with
  the already-open `System.Numerics.Vector3`; the static setters use
  `Placement.Emission.withReflected` (the existing `Placement.CatalogueKind` precedent).
  `Library` is opened, so `defaultSupportedEmission` / `constrainEmission` stay unqualified.
- **`CheckBox` declares only `create`.** All its attrs come from generic base-module DSL
  functions (`ToggleButton` / `ContentControl` / `InputElement` / `Button`); this is normal
  FuncUI and the `ContentControl.content` precedent is live at `EmbeddedChart.fs:59`.
- **Headless checkbox click uses a new `clickControl` helper.** The existing `clickOn` finds
  only a `Border`; a `CheckBox` is toggled by a full pointer down+up at its centre, so the
  emission tests use `clickControl` (any `Control`) and read `IsChecked` / `IsEnabled`
  directly.

## Changelog

- 2026-07-13 — Step 010 (attempt 02): fix the `build` gate — the CheckBox attributes were
  qualified through out-of-scope base-class modules (`ToggleButton.isChecked`,
  `InputElement.isEnabled`); re-qualified all four through the in-scope `CheckBox` class
  (`CheckBox.isChecked` / `.isEnabled` / `.content` / `.onClick`). Build verified green
  (0 errors, only the exempt Wolfram `NU1701`). No behaviour or test change.
- 2026-07-13 — Step 010: expose the sample's geometry-constrained `supportedEmission` in
  `SampleEditorView` — Model field + dirty snapshot + `init` seed/normalise + `toSample`
  carry + `SetSubstrate` re-constraint + R/T `Emission` update arms; render loop-free R/T
  `CheckBox`es (Plate: both; ThinFilm: R fixed on + disabled, no T) with stable
  `UiIds.SampleEditor` ids; add 4 pure + 2 headless emission tests.
