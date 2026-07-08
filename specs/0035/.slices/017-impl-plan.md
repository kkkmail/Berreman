# 017 — IMPLEMENT — impl-plan

## Goal

Spec 0035 step 017 (cosmetic): in
`OpticalConstructor.TestWindows/MaterialEditorView.fs`, replace the sticky
single "Dispersive" toggle in the eps branch (`togglesRow`) with **two
mutually-exclusive options** — `Constant` / `Dispersive` — that route through
the *same* `SetDispersion` message (`NonDispersive` / `DispersiveSegments`). The
derived model is unchanged (`SetDispersion` just assigns the choice, so the
state machine is identical). Keep the `Absorbing` sub-option **only** in the
Constant branch (already the case — it is `if dispersive then [] else [...]`).

## Approach

1. **UiIds** — add a new `[<Literal>] constantToggle = "ConstantToggle"` for the
   new Constant option. Keep `absorbingToggle` / `dispersiveToggle` verbatim.
2. **`togglesRow`** — replace the single sticky `dispersiveToggle` clickBox with
   two clickBoxes:
   - `constantToggle` "Constant", chosen when `not dispersive`, dispatches
     `SetDispersion NonDispersive`.
   - `dispersiveToggle` "Dispersive", chosen when `dispersive`, dispatches
     `SetDispersion DispersiveSegments`.
   Update the section label (drop "off = constant n, k", now a two-option pick)
   and the doc comment above `togglesRow`. The Absorbing / activity / magnetic
   options are unchanged.
3. **Tests** (`OpticalConstructor.Ui.Tests/MaterialEditorWindowTests.fs`):
   - Id contract test: add `Assert.Equal("ConstantToggle", UiIds.constantToggle)`.
   - Mount test: add `constantToggle` to the present-ids list.
   - Lossless-uncheck test: the dispersive round-trip now returns to Constant by
     clicking `constantToggle` (a second `dispersiveToggle` click is a no-op in
     the two-option model).
   - Add a focused acceptance test proving the two options are mutually
     exclusive and select `NonDispersive` / `DispersiveSegments`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/MaterialEditorView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialEditorWindowTests.fs`

## Risks

- Low. Purely cosmetic UI restructuring; the `SetDispersion` message and the
  derived model are untouched, so no domain/solver behaviour changes.
- The lossless-uncheck test previously double-clicked `dispersiveToggle` to
  restore; under two mutually-exclusive options that is a no-op, so the restore
  must click `constantToggle`. This is the one behavioural test-shape change.
