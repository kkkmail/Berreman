# Step 0012 — IMPLEMENT — state of the world

## Where we are

This is step 12 of spec 0050, in section S1.2 ("A material's physics is its complexity"), under
obligation `S12-CONTROLS-EDIT`. The domain-free materials control (`MaterialsControls`) used to
remove its Edit verb for a view-only row. Its stated purpose was the `complexity = None`
built-ins. This step deletes that gating with nothing in its place (.spec-md §3.2). No product
view mounts `materialsControlsView`; only `MaterialsControlsTests` does. The Materials window
keeps its own gate, `MaterialsWindowModel.editableSelection`, which reads
`MaterialComplexity.isEditable`, and this step does not touch it.

## What's working

- Deleted `MaterialsControls.MaterialEditability` and `MaterialsControlsRow.editability`
- The materials verb row always lists Add / Edit / Remove / View, and Edit is offered on a selection exactly as Remove and View are
- `verbButton` is no longer keyed (`View.withKey` dropped), because the row's membership is now fixed
- Migrated the view-only fact to `every selected row offers the Edit verb and a click dispatches editMaterial`

## Tests

These runs are local observations, made to confirm the change. The arc-runner's gate engine runs
and records the gates.

- `build`: 0 errors. The only warnings were the two vendored MathNet SYSLIB0051 ones that were
  already on the start tree (`.artifacts/0012-build.log`).
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 5 project(s)`. Controls changed, and Ui, App
  and the two TestWindows heads were analysed with it. The test file is outside the analyzers'
  scope (`.artifacts/0012-elevation.log`).
- `ui-tests` (MaterialsControlsTests filter): 3/3 succeeded (`.artifacts/0012-ui-tests.log`).
- `ui-smoke` (whole suite): 208/208 succeeded, the same count as step 11. The TRX records
  `every selected row offers the Edit verb and a click dispatches editMaterial` as Passed, and
  the old view-only fact is gone (`.artifacts/0012-ui-smoke.log`,
  `.artifacts/0012-ui-smoke-results/ui-smoke.trx`).
- All five MaterialsControlsTests facts (3 + 2 smoke) pass
  (`.artifacts/0012-materials-controls-tests.log`).
- The four s02 invariants (`elev-list-empty`, `no-vstest`, `no-kit-kit-json` and
  `no-eps-axis-evaluated`) each find nothing.

```yaml
gates:
  ui_smoke_tests:         208
  ui_tests:                 3
```

`ui_tests` is the count under this step's MaterialsControlsTests filter, so it is not comparable
with step 11's FsxAcceptanceTests filter.

## Architecture

- **The domain-free control no longer gates editability.** A host that mounts it and needs a
  gate applies it itself, the way the Materials window does with `editableSelection`.
  `MaterialsControlsRow` is back to an id and a label.
- **`verbButton` is unkeyed, and `clickBox` stays keyed.** The verb row's children are now one
  fixed list. The rows and facet options still change membership on every host filter, so their
  `withKey` still holds.

## Deferred

- Nothing in this step's scope is deferred.

## Gotchas

- **The Materials window's gate is a different seam.** `MaterialsWindowModel.editableSelection`
  (`OpticalConstructor.Ui/MaterialsWindowView.fs`) is unaffected; do not take this step as
  removing Edit gating from the app.
- **The migrated fact asserts the whole recorder.** It checks that `calls` is exactly
  `[ "edit" ]` after one click, so a click that also fired another handler would fail it.

## Changelog

- 2026-11-05 — step 0012:
  - The materials control's view-only Edit gating is deleted (`MaterialEditability`,
    `MaterialsControlsRow.editability`, `verbButton`'s key).
  - The view-only fact is migrated to the always-offered Edit verb.
