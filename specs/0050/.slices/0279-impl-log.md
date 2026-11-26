# Step 0279 — impl log

## Progress

- Read the task, system prompts, project prompt, slice and gates snapshot; no operator note.
- Read steps 277/278's state of the world and the relevant code (`tryItemModelChoice`, `SwapSlot`, `fittingItems`).
- Wrote the impl plan.
- `TableAndElementRotationView.fs`: added `slotChipOpenOpt` (after `kitItems`, `None` in `initWith`),
  `model.slotChipItems` (right after `tryItemModelChoice` on the same extension), `ToggleSlotChip` (after
  `RecordExperiment`, with its arm after `RecordExperiment`'s), and wrapped `SwapSlot`'s arm so every outcome sets
  `slotChipOpenOpt = None`.
- `SetupModelTests.fs`: added `chipTable` / `slotOfKind` helpers and the two facts; widened the doc of the existing
  private `TemplateTable` record, which the chip facts reuse, and the module doc.
- Ran every gate in the snapshot locally on the final tree: all pass (see Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SetupModelTests.fs`

## Decisions

- `SwapSlot`: the old arm body is bound to `answered` unchanged (re-indented) and the arm answers
  `{ answered with slotChipOpenOpt = None }`, so the switch (through `ActivateSetup`), the create, the refusal, the
  read-only guard's refusal and a failed write all close the list from one line.
- `ToggleSlotChip` answers the same `model` instance for an id the table lacks; otherwise only `slotChipOpenOpt`
  changes. It is not behind `guardKitWrite`, so a read-only kit takes it.
- The toggle fact also walks all three swap outcomes (created, refused, switched to a held setup) with the list open,
  beyond the slice's single created swap: the slice requires "every outcome", and the extra assertions sit inside the
  authorized fact.

## Testing state

Development runs with the snapshot's own commands, on the final tree. None exceeded its effective deadline.

| Gate | Result | Time (deadline) |
|---|---|---|
| build | pass, 0 errors; only the two vendored MathNet `SYSLIB0051` warnings | 77 s (600 s) |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0 | 196 s (600 s) |
| constructor-unit-tests | pass, 1466 of 1466 (1464 before, +2); both new facts `Passed` in the TRX | 69 s (180 s) |
| ui-smoke | pass, 245 of 245 | 205 s (600 s) |
| ui-tests (`SetupSaveLoadTests`) | pass, 4 of 4 | 16 s (360 s) |
| invariant-s28-* (16) | every grep finds nothing | — |

## Artifacts

- `specs/0050/.artifacts/0279-build.log`
- `specs/0050/.artifacts/0279-constructor-unit-tests.log`
- `specs/0050/.artifacts/0279-invariants.log`
- `specs/0050/.artifacts/0279-elevation.log`
- `specs/0050/.artifacts/0279-ui-smoke.log`
- `specs/0050/.artifacts/0279-ui-tests.log`

## Gotchas

- The model holds function-valued fields, so it has no structural equality; the "changes nothing" checks compare the
  fields a toggle could touch (`slotChipOpenOpt`, `elements`, `selection`, `kitStatus`, `kitItems`, `kitSetups`,
  `activeSetupOpt`) plus the kit folder's bytes.
