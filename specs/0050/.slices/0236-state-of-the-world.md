# Step 0236 — state of the world

## Where we are

Spec 0050, section S7.2 ("The Items panel, the flow strip and item mapping on copy"), step 0236 (IMPLEMENT),
obligation `S72-ITEM-EDITS`.

- Section S7.1 built the item: `Item`, `ItemDraft.addTo`, `withParameterState` and `publishRevision` (steps
  213–216, 234, 235). The S7.2 copy mapping (`ItemCopyRow`) already uses `UnknownKitItemErr`.
- This step adds the pure domain edits the Items panel sends besides AddItem and SetParameterState: notes, photo
  and label. It also adds the provider that mints a new item's id.
- Step 239 wires `ItemIdProvider.system` and the panel's `AddItem`.

## What's working

- Added `Item.withNotes` and `Item.withPhoto`, pure in-place edits that keep the parameters, revision and history.
  `withPhoto None` drops the reference and never a file.
- Added `Item.relabelledAmong`. It refuses a label whose `<label>.item-json` another item holds, ignoring case, with
  `DuplicateItemLabelErr` naming both labels. The item's own entry never refuses it.
- `ItemDraft.addTo` now calls `relabelledAmong` instead of keeping its own copy of the label rule. Its behaviour and
  its refusal text are unchanged.
- Added `ItemIdProvider { mintItemId }`, the injected provider that mints a new item's identity.
- Added two `KitItemTests` facts (constructor-unit-tests 1358 → 1360). `UnknownKitItemErr` is reused unchanged.

## Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster: build, elevation,
constructor-unit-tests over everything discovered, and the fourteen s24 invariants.

The worker's development runs over the final tree are in the impl-log's `Testing state`, with logs under
`specs/0050/.artifacts/0236-*`:

- **build:** exit 0, 0 warnings, 0 errors, 51 s.
- **constructor-unit-tests:** `Passed!`, 1360 succeeded, 0 failed, 0 skipped, in 65 s. That is checkpoint 235's
  1358 plus the 2 declared facts.
- **KitItemTests:** 23, up from 21. The TRX holds both new ids and `every item error renders its condition and its
  payload`, each `Passed`.
- **elevation** from the arc origin `f395597`: exit 0, `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, in 195 s. The run widened to all 12 projects, because the solution changed since the origin. Known findings: 0
- **The fourteen s24 invariant greps:** none finds anything.

No run came near a deadline. No test is removed or renamed, and no assertion is removed or weakened.

```yaml
gates:
  constructor_unit_tests: 1360
```

## Architecture

- **Pure members on the record they edit.** `withNotes`, `withPhoto` and `relabelledAmong` are intrinsic members of
  `Item`, beside `withParameterState`. That keeps them off the module-level surface (`ELEV013`). They write nothing:
  whatever records the item (the Items panel through `KitLibraryContext.recordItem`) persists the answer. Like
  `withParameterState`, they edit the working revision, and only `publishRevision` moves it.
- **One label rule, one body.** `relabelledAmong` holds the file-name clash rule that `ItemDraft.addTo` (step 216)
  carried privately, and `addTo` now calls it on its draft's item.
  - The own-entry exemption is safe there, because `addTo` refuses a held id before the label check.
  - So there is one rule and one payload for adding and for relabelling.
- **The id is an injected effect.** `ItemIdProvider` is a `[<ReferenceEquality>]` record of one function, in the
  same shape as `ClockProvider`. Tests can supply fixed GUIDs, and the composition root (step 239) supplies
  `Guid.NewGuid`.

## Deferred

Nothing in this step's scope is deferred. `ItemIdProvider.system` is assigned to step 239 by that step's own slice;
it is not left over from this step.

## Gotchas

- **The `change_expectation` on `every item error renders its condition and its payload` was already met.** Step
  213 added the `UnknownKitItemErr` row and the distinct count of 7 together with the case. The fact therefore
  already covers it, and is left unchanged.
- **The refusal text names the new label first:** `'lp1' beside 'LP1': both are stored as LP1.item-json, ignoring
  case`. That is exactly `addTo`'s former text, so an AddItem refusal and a relabel refusal read the same.
- **Relabelling to the item's own label in another case succeeds** (`CPL1` → `cpl1`), because its own entry in
  `items` is ignored by id. The rename of the file on disk is the recorder's concern, not this member's.
- **`relabelledAmong` does not re-validate the label.** An `ItemLabel` is born through `ItemLabel.tryCreate`, which
  refuses a blank one.

## Changelog

- 2026-11-09 — Step 0236:
  - added `Item.withNotes`, `Item.withPhoto`, `Item.relabelledAmong` and `ItemIdProvider`;
  - `ItemDraft.addTo` now reuses the label rule;
  - added two `KitItemTests` facts (constructor-unit-tests 1358 → 1360).
