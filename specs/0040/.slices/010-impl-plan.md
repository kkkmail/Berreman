# Step 010 — impl-plan

## Goal

Expose the sample's geometry-constrained `supportedEmission` (the step-9 Domain
field) in the Sample editor (spec 0040 Part D.2). A `Plate` sample editor MUST let
the user constrain emission to R-only or T-only through R/T checkboxes (never
neither, marking the editor dirty); a `ThinFilm` editor MUST show R fixed on and
disabled with no T control (the step-9 invariant). Reuse the existing three-state
`Emission` DU and its `withReflected` / `withTransmitted` smart setters
(`Placement.fs:126-138`) — no parallel type, no new invariant logic.

## Approach — `OpticalConstructor.Ui/SampleEditorView.fs`

1. **Model** — add `supportedEmission : Placement.Emission` beside `substrate`.
2. **Dirtiness snapshot** — extend `SampleEditSnapshot` to
   `string * string * SubstrateKind * Placement.Emission * SampleStructure` and add
   the field to `currentEdit` so an emission change dirties the editor.
3. **init** — seed the field: `EditSample s` → `s.supportedEmission`, both NEW
   intents → `defaultSupportedEmission ThinFilm`. Route through
   `constrainEmission substrate …` (defensive: an inconsistent input sample is
   normalised at load) and use the same value for `initialEdit`.
4. **toSample** — carry `m.supportedEmission` (preserve the user's choice) instead
   of re-deriving the geometry default.
5. **update** — three arms:
   - `SetSubstrate kind` re-constrains: `constrainEmission kind m.supportedEmission`
     so switching to `ThinFilm` re-pins R-only (keeps the R-fixed-on invariant true
     after a geometry flip; keeps the model consistent with what `toSample` saves).
   - `SetReflectedEmission on` → `constrainEmission m.substrate (Emission.withReflected on m.supportedEmission)`.
   - `SetTransmittedEmission on` → the `withTransmitted` mirror.
   The smart setters make "both off" unrepresentable; `constrainEmission` re-pins a
   ThinFilm to `EmitReflectedOnly` even if a message reaches it.
6. **Msg** — `SetReflectedEmission of bool`, `SetTransmittedEmission of bool`.
7. **view** — a new `emissionRow` in the top facet panel (after `substrateRow`):
   an "Emission:" label, an R `CheckBox`, and — only for a `Plate` — a T `CheckBox`.
   For a `ThinFilm` the R box is checked + disabled and no T box renders. Each box
   is a real FuncUI `CheckBox` bound one-way to the model
   (`ToggleButton.isChecked`) and dispatching on `Button.onClick` with `not <current>`
   — `Click` fires only on USER activation, never on the programmatic re-render set,
   so there is no FuncUI render loop (the ui-smoke hazard). Stable AutomationIds.

## Approach — `OpticalConstructor.Controls/UiIds.fs`

Add `SampleEditor.emitReflectedCheck` / `emitTransmittedCheck` `[<Literal>]` ids.

## Tests — `OpticalConstructor.Ui.Tests/SampleEditorWindowTests.fs`

Pure `update` facts + headless `ui-smoke` proofs:
- Plate: toggling T off leaves R on (`EmitReflectedOnly`) and marks the editor dirty.
- Plate: clearing both is impossible (unchecking R forces T on, and vice versa).
- ThinFilm: R is pinned on under any emission message, and a Plate→ThinFilm
  geometry flip re-pins R-only.
- Headless Plate: both R/T checkboxes render, T starts checked, a click unchecks T
  leaving R checked, and the edit gates Cancel behind the discard confirm (dirty).
- Headless ThinFilm: the R checkbox is present, checked, and disabled; no T control.

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/SampleEditorView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SampleEditorWindowTests.fs`

## Risks

- **FuncUI render loop.** A two-way `isChecked`/`onIsCheckedChanged` binding re-fires
  on the render's programmatic set and can spin ui-smoke (the known FuncUI hazard).
  Mitigation: one-way `isChecked` from the model + `Button.onClick` (user-only) that
  dispatches `not <render-time value>`. No feedback path.
- **Geometry flip.** Without re-constraining on `SetSubstrate`, a Plate set to T-only
  then switched to ThinFilm would leave the R box unchecked — violating "R fixed on".
  `SetSubstrate` re-constrains, keeping the model the single consistent source.
- **`Placement.Emission` qualification.** `Placement` is not opened in the view
  (`System.Numerics.Vector3` would collide); the static setters are qualified
  `Placement.Emission.withReflected`, matching the existing `Placement.CatalogueKind`
  precedent. Instance members (`.emitsReflected`) need no qualifier.
