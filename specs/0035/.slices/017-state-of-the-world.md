# 017 — state of the world

## Where we are

Slice 017 of arc 0035 is a cosmetic restyle of the Material editor's eps branch
in `OpticalConstructor.TestWindows/MaterialEditorView.fs`. It sits after the
per-rung Constant/Dispersive component work of slice 011 and reuses the same
`clickBox` / `SetDispersion` machinery already in place; nothing downstream
(domain, solver, storage) changes.

## What's working

- Present the primary eps branch as two mutually-exclusive options — Constant /
  Dispersive — replacing the sticky single Dispersive toggle.
- Route both options through the same `SetDispersion` message
  (`NonDispersive` / `DispersiveSegments`), so the derived model is unchanged.
- Keep the Absorbing sub-option in the Constant branch only (removed, not greyed,
  under Dispersive).
- Add the `ConstantToggle` automation id; keep `AbsorbingToggle` /
  `DispersiveToggle` verbatim.
- Update the toggle acceptance tests to the two-option model and add a mutual-
  exclusivity ui-smoke test.

## Tests

Per Invariant 6 the worker runs no gates; the arc-runner's gate engine runs
`build`, `unit-tests`, `constructor-unit-tests`, `ui-smoke`, and `ui-tests`
after this session exits. Changes were verified by manual review of the pure
`SetDispersion` / `toComplexity` semantics and by confirming LF line endings.

- `ui-smoke`: one new acceptance test method added (the two-option mutual
  exclusivity proof) → expected 104 → 105.
- `ui-tests`: an assertion added to the existing id-contract test; no new method
  → unchanged.
- `unit-tests` / `constructor-unit-tests`: untouched.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 447
  ui_smoke_tests: 105
  ui_tests: 329
```

## Architecture

- The Constant/Dispersive pair are two mutually-exclusive `clickBox` options
  (each dispatching a fixed `SetDispersion` value), not a sticky toggle. This
  matches the module's existing option-group idiom (anisotropy, handedness,
  gyration class) — a group of `clickBox`es highlighted by the current state —
  and keeps the derived model a pure function of `state.dispersion`.
- The choice-to-message mapping stays a single `DispersionChoice` DU
  (`NonDispersive` / `DispersiveSegments`); no new message or model field was
  introduced, so the change is confined to the view projection.

## Deferred

Nothing. The slice is fully addressed this round.

## Gotchas

- The lossless round-trip test now restores Constant by clicking
  `constantToggle`; a second `dispersiveToggle` click is a no-op under two
  mutually-exclusive options.
- `constantToggle = "ConstantToggle"` was minted for the Constant option (the
  slice named no id for it); `dispersiveToggle` is retained for the Dispersive
  option so the existing id contract and dispersive-rung tests stay valid.

## Changelog

- 2026-07-08 (017): eps branch restyled to two mutually-exclusive Constant /
  Dispersive options over the same `SetDispersion` message; added
  `ConstantToggle` id; updated the absorbing/dispersive toggle acceptance tests
  and added a mutual-exclusivity ui-smoke test. Derived model unchanged.
