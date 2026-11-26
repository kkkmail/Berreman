# Step 0263 — state of the world

## Where we are

Spec 0050 section S8.2 (setups as slots), step 0263 (IMPLEMENT). Steps 0256–0262 made `Setups.Setup` the kit's setup
file and gave the constructor `CreateSetup`, `ActivateSetup`, `SaveSetup` and the commit's setup recording. Step 0258
gave a slot `withItem` / `withoutItem`. This step puts a kit item into a table slot as a semantic message, `AssignSlot`,
over one resolution of an item's model through the window's stores, `tryItemModelChoice`, which the Items panel's
`itemModelCaption` now reads too.

## What's working

- Added `model.tryItemModelChoice`: resolves an item's stored model version first, checks its form against the
  instance, and only then resolves its preset or sample version.
- Rebuilt `itemModelCaption` over that one resolution, with the Items panel's texts unchanged.
- Added `AssignSlot of Library.ElementId * ItemId option`: binds a fitting kit item into a slot or empties it.
- Refused, writing nothing, an unknown slot, an item the kit lacks, an item whose model or sample the stores lack, and
  an item that does not fit the slot, each told as `Not assigned — …`.
- Added three `SetupModelTests` facts: assignment and emptying (with a read-only reopen that resolves only from the
  kit's snapshots), the refusals, and the dial reading recorded and drawn through the item's current zero.

## Tests

No gate verdict is claimed (Invariant 6); the supervisor runs the frozen roster. Development runs over the final tree
(attempt 2, after the preset-arm fix) used each gate's own command, with logs in `specs/0050/.artifacts/0263-dev-*.log`.

- **build:** exit 0, 0 warnings, 0 errors, 15 s incremental (deadline 600 s). Attempt 1's first build, before a
  test-file fix, recompiled the vendored MathNet and showed its two pre-existing `SYSLIB0051` warnings in
  `MathNetNumerics/Numerics/Exceptions.cs` (vendored code, untouched here); every later build showed none.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, 216 s (deadline 600 s).
- **constructor-unit-tests:** `Passed!`, 1426 succeeded, 0 failed, 0 skipped, 71 s (deadline 180 s). The count rises
  1423 → 1426 (+3), as the acceptance states. The three new facts are `Passed` in the TRX.
- **ui-smoke:** `Passed!`, 245 succeeded, 0 failed, 0 skipped, 226 s (deadline 600 s). Count unchanged.
- **ui-tests (selected `ItemsPanelTests`):** `Passed!`, 5 succeeded, 0 failed, 0 skipped, 8 s (deadline 360 s). The
  class held 5 non-smoke facts before and after (count unchanged); its caption facts pass unchanged.
- **The fifteen s26 invariants** each find nothing (`0263-dev-invariants.log`).
- **Whole non-smoke UI suite** (beyond the gate, because `itemModelCaption` changed internally): 678 of 678 passed,
  215 s.
- No run exceeded its deadline.

```yaml
gates:
  constructor_unit_tests: 1426
  ui_smoke_tests: 245
  ui_tests: 5
```

## Architecture

- **One resolution of an item's model.** `tryItemModelChoice` is the only place the window resolves an item to a
  step-216 `ItemModelChoice`. `itemModelCaption` and `AssignSlot` both read it, and the private `ItemModelLookup` is
  gone.
- **Model first.** The stored `ItemModelVersionId` is resolved through `itemModels` and its form's `bindingKindOpt`
  must match the instance (none for a standalone model, a source or polarizer preset binding for a preset, a
  stack-sample binding for a sample). The match is on `bindingKindOpt` alone, and only inside the matching binding's
  case is the preset (`library.tryGetEntry`, a `SourceItem` or `PolarizerItem` matching the binding) or the sample
  version (`samples.resolveVersion`) read; every other binding and entry case is listed explicitly as `None`. An
  unresolved model is `None` even when the preset or sample resolves.
- **AssignSlot writes nothing.** It is not behind `guardKitWrite`, so a read-only kit takes it. The fit and the binding
  are `SetupSlot.withItem`'s (step 258). The setup records the slot at the next CreateSetup, SaveSetup or commit.

## Deferred

Nothing in this step's scope.

## Gotchas

- **Never put the dependency read in a tuple with the binding kind.** `match bindingKindOpt, tryPreset preset with`
  evaluates `tryPreset` before any case is tried, so the library is read before the model's form is validated, which
  S8.2 forbids. Match the binding first and read the preset or sample inside its case.
- **The caption texts were kept, not switched to `ItemModelChoice.caption`.** The brief says `itemModelCaption`
  "becomes the resolved choice's `caption` (step 241)". But `ItemModelChoice.caption` is the add form's offer caption,
  which carries the model GUID. Using it would change every Items-panel row and break the selected `ItemsPanelTests`
  caption facts, which the same brief says pass unchanged ("its texts are unchanged"). So the caption is built from
  the resolved choice in the existing words.
- **`ItemModels.ParameterName.dialZero` does not compile qualified** (FS0039): the union case `ParameterName` shadows
  the type. Open `OpticalConstructor.Domain.ItemModels` and write `ParameterName.dialZero`, as `ItemEvaluationTests`
  does.
- **The snapshot-only reopen opens the kit read-only while the fact still holds it read-write.** A read-only open takes
  no lock, and `AssignSlot` writes nothing, so the folder's bytes stay unchanged. The scene is composed as
  `MainConstructorWindow` composes it: empty in-memory stores `overlaidWith` the reopened kit's overlay, and the item
  models `overlaidWith` it too.
- **`kitFileBytes` is local to `SetupModelTests`:** `KitWindowModelTests.folderBytes` is private to a file outside
  this step's authorized edits.
- **Dial angles are radians in the item's parameters.** `dialZero` 10° is `ParameterValue (Angle.degree 10.0<deg>).value`.
  With `DialNegative`, R1 30° records the reading 10° − 30° = −20°, and a zero of 20° then draws −(−20° − 20°) = 40°.

## Changelog

- 2026-11-10: Step 0263 added `tryItemModelChoice` (model first, then preset or sample), rebuilt `itemModelCaption`
  over it with its texts unchanged, and added `AssignSlot`, which writes nothing. Constructor 1426 (+3), smoke 245,
  selected UI 5 (unchanged).
- 2026-11-10 (attempt 2): `tryItemModelChoice`'s preset arm now matches the model's binding kind before it reads the
  library, as the sample arm already did, with explicit case lists; nothing else changed. Counts unchanged.
