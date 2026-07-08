# Impl-log — slice 011 (IMPLEMENT — active & magnetic Dispersive sub-branch in the Material editor view)

## Progress

- [x] Read protocol, project prompt, slice spec, step-10 SoW, Domain edit model, view, tests.
- [x] Add the four new UiId families + two sub-toggle literals.
- [x] Add `formulaAsModel` / `formulaOfModel` / `componentFormulaView`.
- [x] Wire the activity Dispersive sub-toggle + dispersive gyration component editors into `gyrationPanel` (via a new `gyrationComponentsSection`).
- [x] Wire the magnetic Dispersive sub-toggle + dispersive Polder component editors into `muPanel`.
- [x] Add the two headless acceptance tests + extend the UiId contract test.
- [x] Build green; ran ui-smoke and ui-tests locally for due diligence.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/MaterialEditorView.fs`
  - `UiIds`: `activityDispersiveToggle`, `magneticDispersiveToggle` literals;
    `gyrationComponentFormulaEditor` / `gyrationComponentFormulaBox` /
    `polderComponentFormulaEditor` / `polderComponentFormulaBox` id factories.
  - `formulaAsModel` / `formulaOfModel` — wrap a component `DispersionFormula` as the raw
    `SumOfTerms (RealNK (formula, emptyK))` so the existing `modelParameters` surface edits it,
    and read the edited n-formula back out.
  - `componentFormulaView` — the reusable per-component coefficient surface (a WrapPanel
    carrying the editor-container id, `paramBoxWithUnit` boxes committing on focus loss).
  - `gyrationComponentsSection` (new) — Constant per-component boxes vs one formula editor
    per symmetry-allowed component of `gyrationDispersion`.
  - `gyrationPanel` — the activity Dispersive sub-toggle + the section (was the inline constant
    WrapPanel).
  - `muPanel` — the magnetic Dispersive sub-toggle; a `constantBody` (the prior kind-options +
    component boxes + gyromagnetic axis) and a `dispersiveBody` (the three full-tensor Polder
    formula editors + the axis picker); the axis picker was lifted into a shared `axisRow`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialEditorWindowTests.fs`
  - Extended the `slice-mandated stable ids` contract test with the six new id assertions.
  - Two `ui-smoke` headless acceptance tests driving the window by the new UiIds.

## Testing state

Per Invariant 6 the arc-runner's gate engine runs build / unit-tests /
constructor-unit-tests / ui-smoke / ui-tests AFTER this worker exits; the counts below
were observed locally for due diligence (advisory only).

- Build: `dotnet build Berreman.slnx -c Release` → 0 errors. No new warning from our code
  (the pre-existing MSB3277/NU19xx/FS0044/FS3873/SYSLIB0051 advisories are unowned by this slice).
- `ui-smoke` (`Category=ui-smoke`): 95 passed / 0 failed (baseline 93 → +2 new acceptance tests).
- `ui-tests` (`Category!=ui-smoke`): 327 passed / 0 failed (baseline 327 — the contract test was
  extended in place, not added, so the count holds).
- `berreman_unit_tests` (119) and `constructor_unit_tests` (444) held — no file in those
  projects was touched; the whole-solution build is green.
- No CRLF churn (`git diff --numstat` == `--ignore-cr-at-eol`; `file` reports LF).

`commit_ready: true` — every requirement of the slice is addressed this round.

## Artifacts

None beyond this round's local build/test console output (not persisted — volatile
tool output). No captured logs/screenshots/dumps were produced.

## Gotchas

- **Reused `SumOfTerms`, no model picker for a component.** A component's dispersive facet
  IS a `DispersionFormula` (step-10 §C.0 — a general `DispersionModel -> DispersionFormula`
  is impossible for the transcendental cases), so the per-component editor shows the raw
  `SumOfTerms` coefficient surface only (mult / λ0 / power / cₖ boxes), NOT the segment
  model-kind picker. This is the honest, total representation and matches the spec's
  "existing per-axis modelParameters / SumOfTerms box surface".
- **Empty k slot.** `formulaAsModel` pairs the real component formula with an empty k
  formula (`terms = []`), so `modelParameters` yields ONLY the n-prefixed boxes — a
  component is one real scalar, not an n/k pair.
- **LostFocus commit (render-loop guard).** The component coefficient boxes go through
  `paramBoxWithUnit` → `coeffNumberBox`, which commits on focus loss, NOT per keystroke —
  the same guard the gyration constant boxes and segment coefficients already use. A
  `TextChanged` commit here would rebuild the whole facet on every programmatic re-render
  echo and spin the FuncUI render loop (the known ui-smoke hang).
- **Container-id probe, not param-key.** Each per-component editor WrapPanel carries a
  stable `…FormulaEditor_<code>` AutomationId (the `gyrationClassPicker` WrapPanel-id
  precedent), so the headless proof checks presence by container id and never couples to
  the internal `SumOfTerms` param-key scheme.
- **Dispersive Polder is always the full tensor.** The engine's `MuWithDispValue` carries
  no scalar dispersive case, so the magnetic Dispersive branch always shows all three
  component editors (muDiagonal / muParallel / muGyration) + the axis picker, regardless of
  `muKind` (which only gates the constant branch) — matching `toComplexity`'s
  `DispersiveComponents -> MuWithDispValue state.polderDispersion` arm.
- **F# precedence trap fixed.** `xs |> List.map f @ ys` parses as `xs |> (List.map f @ ys)`
  (`@` binds tighter than `|>`); the dispersive Polder body binds the mapped list first,
  then appends the axis row.
- **MSB3277 (WindowsBase) is pre-existing** and not owned by this slice (no project
  reference changed).
