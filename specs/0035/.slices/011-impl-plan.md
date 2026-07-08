# Impl-plan — slice 011 (IMPLEMENT — surface the step-10 Constant/Dispersive sub-branch on the active & magnetic rungs)

## Goal

Step 010 lifted the func-valued gyration ρ / Polder μ into the pure
`MaterialComplexityEditor` edit model: a per-rung `ComponentDispersion` sub-branch
(`ConstantComponents | DispersiveComponents`), the independent dispersive facets
`gyrationDispersion : GyrationClass<DispersionFormula>` /
`polderDispersion : PolderValue<DispersionFormula>`, the six messages
(`SetActivityDispersion`, `SetGyrationComponentDispersion`, `SetMagneticDispersion`,
`SetMuDiagonal/Parallel/GyrationDispersion`), and the `toComplexity` /
`ofComplexity` derivation. All of that is Domain and is unit-proven. **Nothing is
wired to the window yet** — the step-10 SoW explicitly deferred the UI to this slice.

Slice 011 is the UI projection: surface the sub-branch in
`OpticalConstructor.TestWindows/MaterialEditorView.fs` and prove it with headless
tests in `OpticalConstructor.Ui.Tests`. `touches: [OpticalConstructor.TestWindows,
OpticalConstructor.Ui.Tests]`.

## Approach

`MaterialEditorView.fs`:

1. **New UiIds** (intent-named, prefixed so they cannot collide):
   - `activityDispersiveToggle = "ActivityDispersiveToggle"`,
     `magneticDispersiveToggle = "MagneticDispersiveToggle"` — the two sub-toggles.
   - `gyrationComponentFormulaEditor code` / `polderComponentFormulaEditor code` —
     the per-component dispersion-formula editor CONTAINER (the id the tests probe).
   - `gyrationComponentFormulaBox code key` / `polderComponentFormulaBox code key` —
     one coefficient entry inside a component's editor (the wrapped-model param key).

2. **`formulaAsModel` / `formulaOfModel`** — wrap a scalar component's
   `DispersionFormula` as the raw `SumOfTerms (RealNK (formula, emptyK))` model so the
   EXISTING `modelParameters` coefficient surface edits it (spec §C.0 — a component IS a
   `DispersionFormula`, the raw `SumOfTerms` payload); the empty k formula contributes
   no boxes, so only the n-formula terms are exposed. Reading back takes the n slot.

3. **`componentFormulaView editorId boxId formula dispatchFormula`** — the reusable
   coefficient surface for one component: `paramBoxWithUnit` boxes from
   `modelParameters (formulaAsModel formula)` inside a WrapPanel carrying `editorId`;
   each edit rebuilds the formula and dispatches `dispatchFormula`.

4. **`gyrationPanel`** (the activity rung): add the activity Dispersive sub-toggle;
   render the component section by `m.editor.activityDispersion` —
   `ConstantComponents` keeps the existing per-component `coeffNumberBox`es,
   `DispersiveComponents` renders one `componentFormulaView` per symmetry-allowed
   component of `gyrationDispersion` (class-synced with `gyration`, so the component
   set matches the class picker).

5. **`muPanel`** (the magnetic rung): add the magnetic Dispersive sub-toggle; the
   `ConstantComponents` branch is the existing kind-options + component boxes + (for
   gyromagnetic) the axis picker; the `DispersiveComponents` branch renders the three
   Polder component formula editors (the dispersive Polder is ALWAYS the full tensor —
   the engine's `MuWithDispValue` has no scalar case) plus the axis picker.

`OpticalConstructor.Ui.Tests/MaterialEditorWindowTests.fs`:

6. Two headless window-driving tests (tagged `ui-smoke`, matching the sibling
   acceptance tests in this file):
   - activity: choose Uniaxial → enable activity → enable the activity Dispersive
     sub-toggle exposes a formula editor per symmetry-allowed component (g11, g33) and
     removes the constant component boxes; unchecking restores the constant boxes.
   - magnetic: enable magnetic → enable the magnetic Dispersive sub-toggle exposes the
     three Polder component formula editors and removes the constant Polder box;
     unchecking restores it.
   - extend the UiId-literals contract test with the four new id families.

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/MaterialEditorView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialEditorWindowTests.fs`

## Risks

- **Render-loop hang** (memory: auto-committing FuncUI number boxes hang ui-smoke).
  Mitigated by reusing `coeffNumberBox` (LostFocus commit), exactly as the existing
  constant gyration components do — never the auto-committing `numberBox`.
- **Branch-switch control removal.** Presence assertions rely on FuncUI removing /
  re-labelling the old subtree's controls on a toggle — the same mechanic the existing
  eps constant↔dispersive and anisotropy-change tests already depend on.
- Scope: no Domain change (all messages/facets exist from step 10); no other view
  touched.
