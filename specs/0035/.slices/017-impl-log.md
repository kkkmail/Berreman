# 017 — IMPLEMENT — impl-log

## Progress

- [x] Add `constantToggle` UiId literal (`"ConstantToggle"`), documented as the
      pair of the kept `dispersiveToggle`.
- [x] Rework `togglesRow` to two mutually-exclusive Constant / Dispersive
      options routed through the same `SetDispersion` message.
- [x] Update the id-contract test with `Assert.Equal("ConstantToggle", …)`.
- [x] Update the mount test present-ids list to include `constantToggle`.
- [x] Update the lossless-uncheck test to restore via `constantToggle`.
- [x] Add the two-option mutually-exclusive acceptance ui-smoke test.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/MaterialEditorView.fs`
  - `UiIds`: new `[<Literal>] constantToggle = "ConstantToggle"` (kept
    `absorbingToggle` / `dispersiveToggle` verbatim).
  - `togglesRow`: replaced the single sticky `dispersiveToggle` clickBox with
    two clickBoxes — `constantToggle` "Constant" (chosen when `not dispersive`,
    dispatches `SetDispersion NonDispersive`) and `dispersiveToggle` "Dispersive"
    (chosen when `dispersive`, dispatches `SetDispersion DispersiveSegments`).
    Section label changed from "Dispersion model (off = constant n, k):" to
    "Dispersion model:" (the "off =" note no longer fits a two-option pick). The
    `Absorbing` sub-option is unchanged: still shown only in the Constant branch
    (`if dispersive then [] else [absorbing]`). Doc comment updated.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialEditorWindowTests.fs`
  - Id-contract test: added `Assert.Equal("ConstantToggle", UiIds.constantToggle)`.
  - Mount test: added `UiIds.constantToggle` to the present-ids list.
  - Lossless-uncheck test: the dispersive round-trip now restores by clicking
    `constantToggle` (a second `dispersiveToggle` click is a no-op under the
    two mutually-exclusive options).
  - Added acceptance ui-smoke test
    ``acceptance: the eps branch offers Constant and Dispersive as two
    mutually-exclusive options`` — both options present, default is Constant,
    Dispersive selects `DispersiveSegments` (segment editor appears, summary
    reads "dispersive"), Constant selects `NonDispersive` (index field, no
    segment editor, summary no longer "dispersive").

## Testing state

Per **Invariant 6** of the IMPLEMENT worker prompt (the worker acts and runs no
checks; the arc-runner's deterministic gate engine runs the step's gates —
`build`, `unit-tests`, `constructor-unit-tests`, `ui-smoke`, `ui-tests` — after
the worker exits), the gates were **not** run in this session. The change was
verified by careful manual review instead:

- `SetDispersion` (`MaterialComplexityEditor.fs:617`) simply assigns the choice,
  so two options routing `NonDispersive` / `DispersiveSegments` produce the exact
  same derived model as the former sticky toggle — the round-trip is lossless
  (nothing but `state.dispersion` changes).
- `toComplexity` (`MaterialComplexityEditor.fs:757`) returns `Ok` for both
  branches, so the new acceptance test's summary substrings ("dispersive" for the
  dispersive branch, absent for the constant branch) hold.
- LF line endings verified (`git diff --numstat` identical with and without
  `--ignore-cr-at-eol`; zero CR in the diff).

Baseline counts (this round): `ui_smoke_tests` gains exactly one method
(104 → 105); `ui_tests` unchanged (an assertion added to an existing method, no
new method); `berreman_unit_tests` / `constructor_unit_tests` untouched.

## Artifacts

None — cosmetic UI restructuring, no captured logs / traces produced.

## Gotchas

- **The lossless round-trip test changed shape.** Under the old sticky toggle,
  the test double-clicked `dispersiveToggle` to restore Constant. Under two
  mutually-exclusive options a second `dispersiveToggle` click is a no-op
  (`SetDispersion DispersiveSegments` while already dispersive), so the restore
  must click `constantToggle`. This is the one behavioural test-shape change; the
  derived model is otherwise identical.
- **New `constantToggle` id, `dispersiveToggle` kept.** The slice said to update
  the `absorbingToggle` / `dispersiveToggle` acceptance tests "to the two-option
  model" and gave no name for the Constant option's id. I minted
  `constantToggle = "ConstantToggle"` following the module's naming discipline
  (intent-named, PascalCase literal) and kept `dispersiveToggle` for the
  Dispersive option so the existing id contract and the tests that enter the
  dispersive rung by clicking `dispersiveToggle` stay valid. Recorded per the
  "don't ask the user / pick the consistent interpretation" rule.
