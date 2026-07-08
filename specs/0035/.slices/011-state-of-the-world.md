# State of the world — slice 011 (IMPLEMENT — active & magnetic Dispersive sub-branch, UI)

## Where we are

Slice 011 is the UI projection of the step-10 Part-C domain work. Step 010 lifted the
func-valued gyration ρ and Polder μ into the pure `MaterialComplexityEditor` edit model
(the `ComponentDispersion` sub-branch, the `gyrationDispersion` /
`polderDispersion` facets, the six `Set…Dispersion` messages, and the `toComplexity` /
`ofComplexity` derivation) and DEFERRED the window wiring to this slice. Slice 011 wires
that sub-branch into `OpticalConstructor.TestWindows/MaterialEditorView.fs` and proves it
with headless tests in `OpticalConstructor.Ui.Tests` — matching the slice `touches`
`[OpticalConstructor.TestWindows, OpticalConstructor.Ui.Tests]`. No Domain, Storage, or
other view was touched; every message and facet the wiring uses already existed from step 10.

## What's working

- Add a per-rung Constant/Dispersive sub-toggle to the optically-active and magnetic rungs
  (`ActivityDispersiveToggle` / `MagneticDispersiveToggle`).
- Enabling the activity Dispersive sub-toggle replaces each symmetry-allowed gyration
  component box with a dispersion-formula coefficient editor (the raw `SumOfTerms`
  `modelParameters` surface) over `gyrationDispersion`.
- Enabling the magnetic Dispersive sub-toggle replaces the Polder component boxes with the
  full-tensor Polder component formula editors over `polderDispersion`, keeping the
  magnetization-axis picker.
- Unchecking either sub-toggle restores the constant component boxes losslessly.
- Add intent-named UiIds for the two sub-toggles and the per-component formula editors
  (container + coefficient-box id families), extending the id contract test.

## Tests

Per the IMPLEMENT worker role (Invariant 6 — the worker acts and runs no checks), the
`build` / `unit-tests` / `constructor-unit-tests` / `ui-smoke` / `ui-tests` gates are
executed by the arc-runner's deterministic gate engine AFTER this worker exits; they were
NOT run here as authoritative gates. The counts below were observed locally as due
diligence (advisory only).

- NEW in `MaterialEditorWindowTests` (`ui-smoke`), 2 headless acceptance tests: the activity
  Dispersive sub-toggle swaps each gyration component (uniaxial g11 + g33) for a formula
  editor and back; the magnetic Dispersive sub-toggle swaps the Polder components for the
  three formula editors and back. `ui_smoke_tests` 93 → 95.
- The `slice-mandated stable ids` contract test (`ui-tests`) was extended in place with the
  six new id assertions — no new test, so `ui_tests` holds at 327 (all green).
- `berreman_unit_tests` 119 and `constructor_unit_tests` 444 held — no file in those
  projects changed; the whole-solution build is green.
- Build: 0 errors, no new warning from our code.

## Architecture

- **A component IS a `DispersionFormula`, edited through the raw `SumOfTerms` surface.** The
  per-component editor wraps the scalar formula as `SumOfTerms (RealNK (formula, emptyK))`
  and reuses the existing `modelParameters` coefficient boxes (the same surface the eps
  segments edit) — NOT a model-kind picker. A general `DispersionModel -> DispersionFormula`
  is impossible for the transcendental cases (step-10 §C.0), so the raw term surface is the
  honest, total representation. The empty k slot means only the real formula's boxes appear.
- **Container-id automation contract.** Each per-component editor is a WrapPanel carrying a
  stable `…FormulaEditor_<code>` AutomationId (the `gyrationClassPicker` WrapPanel-id
  precedent), so automation probes by meaning (the component's editor) rather than reaching
  into the internal `SumOfTerms` param keys; the inner coefficient boxes still carry
  `…FormulaBox_<code>_<key>` ids for direct editing.
- **Constant vs dispersive are mutually-exclusive subtrees.** Each rung renders exactly one
  branch by its `ComponentDispersion`; switching removes the other branch's controls (the
  eps constant↔dispersive precedent), which is what makes the lossless-restore observable in
  the visual tree.
- **Dispersive Polder is always the full tensor.** The magnetic Dispersive branch shows all
  three component editors + the axis picker regardless of `muKind` (which gates only the
  constant branch), matching the engine's `MuWithDispValue` (no scalar dispersive case) and
  `toComplexity`'s `DispersiveComponents` arm.

## Deferred

- Nothing from THIS slice's own scope is deferred. Later Materials-workbench slices may
  surface the same dispersive-component editors outside the standalone editor window; that
  is out of this slice's `touches`.

## Gotchas

- **LostFocus commit is load-bearing.** The component coefficient boxes commit on focus loss
  (`coeffNumberBox` via `paramBoxWithUnit`), never per keystroke — a `TextChanged` commit
  rebuilds the whole facet on every FuncUI render echo and spins the known ui-smoke render
  loop. Any future edit to these boxes must preserve LostFocus commit.
- **Headless `setText` will NOT commit a coefficient box** (it sets `.Text` without raising
  LostFocus), so the acceptance tests assert on EXPOSURE/RESTORE by UiId presence, not on a
  coefficient value flowing through — the derivation of edited component formulas is already
  pinned by step-10's `MaterialComplexityTests`.
- **F# precedence:** `xs |> List.map f @ ys` parses as `xs |> (List.map f @ ys)` (`@` binds
  tighter than `|>`). The dispersive Polder body binds the mapped list first, then appends
  the axis row — a caught-and-fixed build error this round.
- **MSB3277 (WindowsBase) is pre-existing** and not owned by this slice (no project
  reference changed).

## Changelog

- 2026-07-08 — Part C UI: surfaced the step-10 Constant/Dispersive sub-branch on the
  optically-active and magnetic rungs of the Material editor. Added the two sub-toggles
  (`ActivityDispersiveToggle` / `MagneticDispersiveToggle`) and the per-component
  dispersion-formula editor id families; a `componentFormulaView` reusing the raw
  `SumOfTerms` `modelParameters` coefficient surface; a `gyrationComponentsSection` for the
  activity rung; and a Constant/Dispersive split in `muPanel` (the dispersive branch always
  the full Polder tensor + axis). Unchecking each sub-toggle restores the constant boxes.
  Two `ui-smoke` acceptance tests + six new id assertions.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 444
  ui_smoke_tests: 95
  ui_tests: 327
```
