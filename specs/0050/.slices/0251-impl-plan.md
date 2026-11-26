# Step 0251 — impl plan (IMPLEMENT: one entry from an item's model to its bound element)

## Approach

Add one member, `ItemModelChoice.boundWith materials values`, in a second `type ItemModelChoice with` block placed
after `BoundOpticalElement` in `ItemEvaluation.fs` (the first block, holding `emissionWith`, precedes the bound
element, so the new member cannot live there). It routes each of step 216's choices to the constructors steps
247–249 already built, adding no physics:

- `PolarizerPresetChoice (_, p)` → `Ok (BoundPolarizerPreset p.behavior)`;
- `StackSampleChoice (_, _, sample)` → `BoundOpticalElement.ofStackSample materials values sample`;
- `LibraryModelChoice v` matched on `v.entry.model`, with no wildcard:
  - `OpticalElementModel form` → `BoundOpticalElement.ofOpticalForm values form`;
  - `EmptySlotModel` → `Ok BoundEmptySlot`;
  - the three detectors → `NotAnOpticalElementErr` naming the model (a detector reads);
  - the three binding forms chosen without their binding → `NotAnOpticalElementErr` naming the model and the
    binding it lacks (the slice's "typed missing-binding error", see the impl-log's decisions);
- `SourcePresetChoice (model, _)` → `NotAnOpticalElementErr` naming the model (a source emits through
  `emissionWith`).

`ItemEvaluationError` gains `NotAnOpticalElementErr of reason : string`, rendered
`the model is not an optical element: <reason>`; the type's doc comment names the new condition.

## Tests (`ItemEvaluationTests`)

- New `every item model binds as its kind, and a source or a detector is no optical element`.
- New `an item's own parameter values reach its Mueller matrix` (CPL1 through `ItemDraft.addTo`, `withParameterState`
  of `c2.retardance` to π / 4, `ParameterValues.ofParameters`).
- Widened `every evaluation error renders its condition and its payload` (distinct renders 7 → 8).

Every model comes from step 210's shipped entries (`BuiltinCatalogue.itemModel`), every binding from a Built-in
preset or sample; materials from `BuiltinCatalogue.materialStore (fun _ -> [])`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemEvaluation.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemEvaluationTests.fs`

## Risks

- `BoundStackSample` holds `[<ReferenceEquality>]` resolved materials, so the stack-sample binding is compared through
  the matrices it evaluates to (exactly) and through its typed refusal, not by `Result` equality of the bound value.
- ELEV: the new member speaks only our types; its local reason helper is a closure inside the member, as
  `emissionWith`'s and `ItemDraft.addTo`'s are.
