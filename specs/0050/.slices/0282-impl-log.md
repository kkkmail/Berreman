# Step 0282 — impl log

## Progress

- [x] Model field `setupGridOpt` and `initWith`
- [x] `model.setupGrid`
- [x] Six grid messages and their arms
- [x] Three `SetupModelTests` facts
- [x] Development runs (build, elevation, constructor-unit-tests, ui-smoke, ui-tests selected, s28 invariants)
- [x] Mutation check (two rounds), restored byte for byte and rebuilt

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
  - `TableAndElementRotationModel.setupGridOpt : SetupGrids.SetupGridDraft option` after `slotChipOpenOpt`; `initWith`
    sets it to `None` (the only full-record construction of the model).
  - `member model.setupGrid : SetupGrids.SetupGrid option` after `slotChipItems`: the open draft's
    `gridOver model.elements model.kitItems model.tryItemModelChoice`.
  - Six `TableAndElementRotationMsg` cases after `ToggleSlotChip`: `OpenSetupGrid`, `CloseSetupGrid`,
    `ToggleSetupGridItem of Library.ElementId * ItemId`, `AddSetupGridAngle of Library.ElementId * BarDegrees`,
    `RemoveSetupGridAngle of Library.ElementId * Angle`, `ToggleSetupGridCombination of SetupGrids.GridCombination`.
  - `let private withGridDraftEdited`, before the update: the open draft mapped, or the model itself while closed.
  - Six arms after the `ToggleSlotChip` arm. None goes through `guardKitWrite`; `GenerateSetupGrid` is unchanged.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SetupModelTests.fs`
  - Module doc extended with the grid panel's messages.
  - Private helpers `plateCombination`, `openGrid`, `platesTried`, and the three facts the slice names.

## Decisions

- `AddSetupGridAngle` converts through `BarDegrees.wrappedAngle`, the table's angle convention, so −45° is R1 315°.
- `OpenSetupGrid` keeps a draft already open (`Option.defaultValue SetupGridDraft.empty`).
- The four edits share one private helper, so "with none open each leaves the model as it is" is one rule. The helper
  returns the model itself, which the second fact checks with `Assert.Same`.
- The facts extend the slice's listed assertions with: opening again keeps the draft, edits with the panel closed
  leave the model as it is, and the edits leave elements, setups, experiments and `kitStatus` unchanged.

## Testing state

These are development runs on the final tree. The gate engine runs the gates themselves.

| Run | Outcome | Time / deadline |
|---|---|---|
| build | exit 0, 0 warnings, 0 errors (final incremental); first full build only vendored MathNet's `SYSLIB0051` | 16 s (73 s full) / 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0 | 202 s / 600 s |
| constructor-unit-tests | 1472 / 1472 (1469 + 3; the three new facts `Passed` in the TRX) | 68 s / 180 s |
| ui-smoke | 247 / 247, unchanged | 201 s / 600 s |
| ui-tests (`InverseConstructorTests`, `SetupSaveLoadTests`) | 18 / 18, unchanged | 26 s / 360 s |
| invariant-s28-\* (16) | each finds nothing, tracked and untracked | — |

Mutation check:

- Round 1 used four mutants: no angle wrap, `OpenSetupGrid` always empty, `CloseSetupGrid` keeping the draft, and
  `setupGrid` dropping unticks. It turned all three new facts red, with 26 other `SetupModelTests` green.
- Round 2 used the open and close mutants only. It turned the second fact red, at "opening again keeps the draft".
- The close mutant was masked behind the open one. The fact asserts `closed.setupGridOpt = None` directly.
- After restoring, `cmp` showed the file identical to the pre-mutation copy. Every copy of the Ui assembly was rebuilt
  after it.

## Artifacts

- `specs/0050/.artifacts/0282-dev-build.log`, `0282-dev-build-final.log`
- `specs/0050/.artifacts/0282-dev-elevation.log`
- `specs/0050/.artifacts/0282-dev-constructor-unit-tests.log`, `0282-dev-ui-smoke.log`, `0282-dev-ui-tests.log`
- `specs/0050/.artifacts/0282-invariants.log`
- `specs/0050/.artifacts/0282-mutation/` (the pre-mutation copy, both mutant builds and test logs)

## Gotchas

- The slice writes `RotationControls.BarDegrees`. `BarDegrees` is declared at namespace level in
  `OpticalConstructor.Controls` (`RotationControls.fs:45`), not inside the `RotationControls` module, so that qualified
  name does not resolve. The case uses `BarDegrees`, which the file already opens. It is the same type.
- The slice points at `TableAndElementRotationView.fs:815` for the angle convention. That line has moved: `wrappedAngle`
  is on the `BarDegrees` extension near line 1190.
