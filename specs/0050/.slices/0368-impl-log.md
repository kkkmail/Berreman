# Step 0368 — impl-log

## Progress

- [x] Read the protocol, the project prompt, AGENTS.md, the slice and its `.gates` snapshot. There is no operator note
  and no operator ruling for this step.
- [x] Domain `ItemEvaluation.fs`:
  - opens `Placement` (for `CatalogueKind`) and `Setups` (for `SetupSlot`);
  - after `ItemModelChoice.boundWith`, adds `[<ReferenceEquality>] SlotRunParam` with the slice's five fields, and
    `SlotRunParam.resolveSlotItem`;
  - adds `SlotRunError` (`SlotItemErr` | `SlotEvaluationErr`), whose `describe` returns the inner `describe`;
  - adds `type SetupSlot with`: `runElementWith`, `runStepOf` and the static `runEmissionOf`;
  - `sequenced` is now generic in its error (`Result<'Item, 'Error> list`).
- [x] UI `TableAndElementRotationView.fs`:
  - `member internal model.slotRunParam` builds the one param over `model.library`, `model.materials`,
    `model.kitItems`, `model.tryItemModelChoice` and `ParameterValues.ofParameters item.parameters`;
  - `model.runEmission` is `SetupSlot.runEmissionOf model.elements model.slotRunParam`, with its error's `describe`
    as the `ControlCaption`;
  - `model.resolveSlotItem` is deleted. `slotAssigned` (`AssignSlot` / `SwapSlot`) calls
    `model.slotRunParam.resolveSlotItem`;
  - `chartForRun`:
    - the pre-resolved `slotItems` map and the three inline binding arms of `builderFor` are deleted;
    - each path step binds through `e.runElementWith runParam`, and `sweptOrFixed` makes each matrix its step through
      `e.runStepOf`;
    - the evaluation runs over `SlotRunError`;
    - the failure match keeps both texts: `SlotEvaluationErr (SampleMaterialErr err)` gives "Sample materials failed
      to resolve" with `err.runFailureCaption`, and every other case gives "The run cannot be evaluated" with its
      `describe`.
- [x] Tests (`ItemEvaluationTests`): the three facts the step names, over `SetupTests`-style slots, `ItemDraft.addTo`
  items, `BuiltinCatalogue.itemModel` models (step 210), `BuiltinCatalogue.library ()` and
  `BuiltinCatalogue.materialStore (fun _ -> [])`.
- [x] Development runs of build, constructor-unit-tests, ui-smoke, the selected ui-tests, elevation and the 20 s37
  invariants.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemEvaluation.fs` (+104 / −3)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs` (+38 / −91)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemEvaluationTests.fs` (+197)

Every file is LF: `git diff --numstat` matches `--ignore-cr-at-eol`. No other file changed. The supervisor's
`.manifest.state.json` was already dirty.

## Decisions

- **`resolveSlotItem` moved to `SlotRunParam`, not copied.**
  - The slice spells out the run's item resolution: the item of that id, else `UnknownSlotItemErr` naming the id; then
    `choiceOf`, else `UnknownSlotItemErr` naming the label. That is exactly the UI's
    `model.resolveSlotItem`, which its own doc called "one resolution for `AssignSlot`, `SwapSlot` and the run".
  - Writing it privately in Domain and keeping the UI copy for `AssignSlot` would leave two bodies, which AGENTS.md
    "Reuse, never copy" forbids. So the body moved onto the new `SlotRunParam` as `resolveSlotItem`; the UI member is
    deleted and its one other caller (`slotAssigned`) reads `model.slotRunParam.resolveSlotItem`.
  - This adds one public member to the new contract type. No signature outside this step's three files changes.
  - `SetupGrids.slotsOver` keeps its own lookup: it interleaves the `SlotItemTwiceErr` check and lies outside this
    step's edits.
- **`sequenced` is generic in its error.** The table's run now sequences `Result<_, SlotRunError>` along a path. The
  manual 016 ruling says the ordered Result collector in `ItemEvaluation.fs` is the one to reuse, so its error type is
  generalised instead of writing a second fold. Every existing caller still infers `ItemEvaluationError`, and the
  order and first-error behaviour are unchanged.
- **One builder path for every bound element.**
  - The old polarizer-preset arm built its step at `IncidenceAngle.normal`. Now every bound element goes through
    `sweptOrFixed`, at the drawn (or swept) incidence.
  - `BoundPolarizerPreset.muellerAt` reads no incidence: it is `behaviorMueller behavior rotation` on the transmitted
    branch and `UnmodelledBranchErr` on the reflected one. So every number is unchanged.
- **Lens, mirror and detector slots are `Ok None`, as the slice rules.** The old run resolved and bound an item in
  any non-source slot on the path. That changes no chart:
  - `slotKinds` lets no item model fill a lens or a mirror slot;
  - a detector terminates its beam, so it is never on its own path, or on any path it would block.
- **Error order.** The old run resolved every captured-path item before binding any slot. Now resolution and binding
  go per path step, as the slice prescribes (`runElementWith` per step). The caption differs only when one path has
  both an unresolvable item and an earlier binding error. Each text is still the same `describe`.
- **Test fixtures.**
  - The slots repeat `SetupTests`' private `emptySlot` shape, as the slice asks ("slots built as `SetupTests`'
    fixtures build them").
  - `choiceOf` is a stub over choices built from the shipped `BuiltinCatalogue.itemModel` versions (step 210, step 216
    payloads), as `SetupGridTests` does.
  - The 30° azimuth is written as `ParameterValue (π / 6)` and read back through `ParameterValue.angle`. No
    `Angle` → `ParameterValue` re-wrap is used.
  - Test extras:
    - a bare polarizer slot is `Ok None`;
    - the item wins over the slot's preset binding;
    - the real LP's m22 is √0.01, which shows the given values reach the matrix;
    - curved-mirror and lens slots are `Ok None` too;
    - a missing source item answers `SlotItemErr` from `runEmissionOf`;
    - both `describe` cases are checked;
    - a dark source slot before the lit one is skipped;
    - a slot bound to the HeNe preset with no item still emits the default.

## Testing state

These are development runs of each gate's own command from `0368.gates`, over the final tree. Logs are in
`specs/0050/.artifacts/`.

- **build:** exit 0, `0 Error(s)`, 22–54 s (deadline 600 s).
  - The only warnings are the exempt NU1902.
  - The first build failed with FS0010 on a multi-line or-pattern in the failure match. It was fixed by splitting
    that match into two arms.
  - Log: `0368-build-local.log`.
- **constructor-unit-tests:** `Passed!`, 1614 of 1614, 71 s (deadline 180 s).
  - That is 1611 (0367) + 3.
  - The TRX shows all three new facts `Passed`.
  - Log: `0368-constructor-unit-tests-local.log`.
- **ui-tests** (the six selected classes): `Passed!`, 99 of 99, 107 s (deadline 360 s).
  - No Ui.Tests file changed.
  - 0367's 101 included `OutOfBandBadgeTests`, which this step's selection does not.
  - Log: `0368-ui-tests-local.log`.
- **ui-smoke:** `Passed!`, 258 of 258 (baseline 258), 228 s (allowance 600 s). Log: `0368-ui-smoke-local.log`.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 224s` (deadline 600 s).
  - It ran with `ASG_ARC_BASE_COMMIT` set to the `.arc-base-json` origin `f395597`. Because the slnx changed in the
    arc, it covered all 13 projects.
  - 1519 ELEV004 findings suppressed (not gated; 1520 at 0367), 0 known findings.
  - Log: `0368-elevation-local.log`.
- **invariant-s37-\* (20):** each decoded `--find-nothing` grep finds nothing.

No run went past its deadline.

TDD note: the three facts name `SlotRunParam`, `SlotRunError`, `runElementWith`, `runStepOf` and `runEmissionOf`,
none of which existed before this step. Before the move they are red by compile error (the symbol is undefined). The
Domain members were written first and the facts then, in the same round.

## Artifacts

- `specs/0050/.artifacts/0368-build-local.log`
- `specs/0050/.artifacts/0368-constructor-unit-tests-local.log`
- `specs/0050/.artifacts/0368-ui-tests-local.log`
- `specs/0050/.artifacts/0368-ui-smoke-local.log`
- `specs/0050/.artifacts/0368-elevation-local.log`

## Gotchas

- An or-pattern split over two lines inside `Error ( … )` fails to parse (FS0010 / FS0583). Write two match arms
  that share one body instead.
- The Bash heredoc failed on the test text (`unexpected EOF while looking for matching '`). The facts were appended
  with the Edit tool.
- `CatalogueKind.Sample` is written qualified in the test fixture. `Sample` is also the library's sample record type,
  which the test file uses in type positions.
