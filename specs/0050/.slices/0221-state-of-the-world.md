# Step 0221 — state of the world (IMPLEMENT: the item mapping of a copy, S72-COPY-MAPPING)

## Where we are

Spec 0050, section S6 (send and receive kits; copy experiments between kits), lands the shared item contracts before
S7, so that copying an experiment includes its item mapping (001 §H, §S6; .spec-md §3.15). Steps 213–220 built the
item, its kit file and IO, its recording and the app-scope item-model store. This step adds two things:

- **The mapping as pure data.** It covers suffixed labels, default choices, mapping rows and the items to add.
- **Explicit physical identity on a captured experiment.** Each element of a setup may name the `ItemId` assigned at
  it, kept apart from its library binding. It is persisted, validated and preserved when the edited experiment is
  recommitted.

The next step (0222) overlays the kit's item-model snapshots on the session's item-model store.

## What's working

- Add `ItemLabel.nextFreeAmong`, `ItemCopyChoice`, `Item.defaultCopyChoice` and `ItemCopyRow` with `copyRowsFor` and
  `itemsToAdd`: the copy's item mapping as pure data (spec 0050 S7.2).
- Give `ElementDescriptor` an explicit `itemIdOpt`, and add `Experiment.itemsUsedBy`, which resolves only assigned
  ids and refuses a missing one with `UnknownKitItemErr`.
- Make the experiment schema require each descriptor's `itemIdOpt` (`null` or a canonical GUID). A body without it is
  refused, with no fallback reader.
- Keep an edited experiment's assigned pieces on recommit while each element's binding is unchanged. A new or rebound
  element carries none.
- Add the 8 declared facts: constructor-unit-tests 1312 → 1319, `InverseConstructorTests` 13 → 14.

## Tests

These are development runs over the final source, recorded in `0221-impl-log.md`. The gate engine gives the verdict
(Invariant 6).

- **build:** exit 0. The final rebuild had 0 warnings and 0 errors. The first full build showed only the two
  existing vendored MathNet `SYSLIB0051` warnings.
- **constructor-unit-tests** (all discovered): 1319 passed, 0 failed, 0 skipped.
  - That is 1312 + 7: `KitItemTests` 4, `ExperimentProxyTests` 2, `KitContentFileJsonTests` 1.
  - Every declared identity is `Passed` in the TRX.
- **ui-tests** (selection `InverseConstructorTests`, `SolverHandoffWindowTests`, `ExperimentControlsTests`):
  - 51 passed, 0 failed, 0 skipped (29 + 14 + 8);
  - `recommitting a copied experiment preserves its assigned physical ids` is `Passed`.
- **ui-smoke:** 237 passed, 0 failed, 0 skipped, so the count is unchanged.
  - 206 s against the 600 s `attempt_timeout_seconds` (manual 015), measured while elevation ran concurrently.
  - No deadline was exceeded.
- **elevation** (from the arc origin `f395597`, ELEV004 excluded):
  - `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 204 s against its 600 s estimate;
  - suppressed findings went from 1309 to 1311 (measured, not gated).
- **The fourteen `invariant-s22-*` gates:** each holds; their greps find nothing.
- **The facts bite.** Each mutant was restored byte for byte afterwards:
  - dropping the recapture rule fails the recommit fact;
  - ignoring the binding check fails it on the rebound sample;
  - dropping the schema's `required` fails the JSON fact.
- Not in this step's roster: `unit-tests`. No solver code changed.

```yaml
gates:
  constructor_unit_tests: 1319
  ui_smoke_tests: 237
```

## Architecture

- **Identity is explicit, never inferred** (spec 0050 S7.2; 001 §I).
  - `ElementDescriptor.itemIdOpt` is the only link from a captured element to a physical piece.
  - `itemsUsedBy` reads nothing but those ids. Two pieces of one model, one sample version and one preset stay two
    pieces.
  - A dangling id is `UnknownKitItemErr` naming the id, the element and the experiment; it is never matched to a
    held item of the same preset.
- **The mapping is pure and unstored.**
  - `ItemCopyChoice` and `ItemCopyRow` carry no wire tag.
  - `copyRowsFor` keeps the user's choices by source id while the picked experiments change.
  - `itemsToAdd` checks every row before it answers. A held piece is mapped, never added, and a mapping must name a
    held id. Added labels take the `copyTargetFor` suffix form over file stems, because `<label>.item-json` shares the
    kit root.
- **One parser, one canonical form.** The option rides the shared serializer (`null` or the GUID). The schema makes
  the field mandatory and validates its GUID, so a pre-S7.2 body is a `SchemaValidationErr`. There is no up-converter
  and no inferred binding (no legacy, no migration).
- **Recapture rule.** `experimentSetup` looks up the experiment the draft edits. It keeps an element's piece only for
  the same `elementId` and an equal recaptured `ElementBinding`, so rebinding an element releases its piece.

## Deferred

Nothing in this step's scope is deferred. By the spec's plan:

- the overlaid item-model store is step 222;
- the copy flow that writes these rows into a target kit, and the Items panel, are later S6/S7 steps.

## Gotchas

- **`KitLibraryTests.fs` is outside the edit list, but the descriptor's change_shape forced a one-line
  `itemIdOpt = None`** in its `recordingExperiment` record literal. No assertion or identity changed. Step 0210 is
  the precedent.
- **A missing `itemIdOpt` refused without the schema rule would be the wrong error.** FSharp.SystemTextJson already
  refuses a missing option field, but as `MalformedJsonErr`. The schema makes the refusal the typed
  `SchemaValidationErr` the slice requires.
- **The schema's descriptor rule applies only to objects.** A non-object descriptor still binds and fails as
  `MalformedJsonErr`, which the existing facts expect.
- **The scene is not a kit file.** The UI fact reopens the folder with the first session's elements, because
  `AddElement` mints element ids freshly.
- **No `ItemModelProxy` resolution happens here.** Model matching compares `ItemModelInstance` values, which carry
  the exact version (step 216). Fixtures take every version from `BuiltinCatalogue.itemModel` (step 210).

## Changelog

- 2026-11-09 — step 0221: added the copy's item mapping (`nextFreeAmong`, `ItemCopyChoice`, `defaultCopyChoice`,
  `ItemCopyRow.copyRowsFor` / `itemsToAdd`); added `ElementDescriptor.itemIdOpt`, `Experiment.itemsUsedBy` and the
  schema's required, GUID-validated field; kept assigned pieces on recommit; 8 new facts (constructor 1312 → 1319,
  `InverseConstructorTests` 13 → 14), ui-smoke unchanged at 237.
