# Step 0281 — impl log

## Progress

- Read the task, both system prompts, the project prompt, the slice and the gates snapshot. There is no operator note.
- Read `SetupGrids.fs` (step 274), `SetupGridTests.fs`, `Setups.fs` (`SetupSlot.inLightOrder`, `RoledSlot`) and the
  default R1 in `Placement.fs` (`Angle.zero`).
- Wrote the impl plan.
- `SetupGrids.fs`, after `SetupGrid`:
  - `GridSlotDraft`;
  - a private module function `withSlotDraft`, which the three slot edits share;
  - `SetupGridDraft` with `empty`, `slotDraft`, `withItemToggled`, `withGridAngle`, `withoutGridAngle`,
    `withCombinationToggled` and `gridOver`.
- `SetupGridTests.fs`: two private fold helpers (`withItems`, `withAngles`) and the slice's three facts, appended after
  the step-274 facts.
- The first build compiled cleanly. All 7 `SetupGridTests` facts passed on their first run, and the suite reached 1469.
- Mutation check. I made `gridOver` order its axes by the draft's first-edited order instead of light order, and dropped
  the "still listed" filter on the draft's unticks. Then I rebuilt and ran the class.
  - `a grid draft tries each ticked item … in light order` went red, and so did
    `a draft's unticked combinations stay unticked … while they are listed`.
  - The other 5 stayed green.
  - The source was restored from a byte copy, and the backup is deleted. The log is in `0281-mutation-tests.log`.
- Rebuilt the solution and re-ran constructor-unit-tests and the sixteen invariant greps on the final tree.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/SetupGrids.fs` (+101 lines)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SetupGridTests.fs` (+92 lines)

Both files are in the slice's authorized edits. No project file changed, since `SetupGridTests.fs` is already compiled.

## Decisions

- **The shared slot-draft edit is a private module function, not a member.** The slice says "No cache, sort or other
  member is added", and the three slot edits must not each copy the place-or-append-then-drop logic. So
  `withSlotDraft (drafts : GridSlotDraft list) (edited : GridSlotDraft)` sits between the two types as a `let private`.
  It takes two of our types, so ELEV013 does not apply.
- **The draft's unticks go through `SetupGrid.untick`.** `gridOver` filters them to the combinations the grid lists,
  then folds them through step 274's `untick`, which already lists a combination once. So "that it lists and does not
  already hold, in the draft's order" reuses that rule rather than restating it.
- **The draft keeps an untick after its combination stops being listed.** The slice's `withItemToggled` touches only
  the slot's items, and `gridOver` only applies the unticks the grid lists. So toggling `QZ` back in after removing it
  un-ticks its combination again. No test pins this: it follows from the slice's wording, and the slice asks for no fact
  on it.
- **The first fact turns `lpB` to 30° in a local copy of the table.** The fixture's R1 is `Angle.zero`, the same as the
  0° the samples are tried at. A `gridOver` that wrongly used 0° would still have passed against "CPL2 at lpB's R1". The
  turned copy keeps the same slot ids, kinds and light order, so it is the same fixture and only the R1 is visible.
- The third fact also checks that unticking `Ghost`'s combination, which the grid already leaves out as impossible,
  does not list it twice. This is the slice's "does not already hold".

## Testing state

These are development runs on the final tree, using the snapshot's own commands. The gate engine runs the gates.

- **build:** exit 0, with 0 errors and 0 warnings, in 53 s (deadline 600 s).
  - The first build after the edit took 73 s. It also recompiled vendored MathNet and showed its two pre-existing
    `SYSLIB0051` warnings, as recorded at step 279. No warning came from our code.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 197s`, with known findings 0 (deadline 600 s).
  - It ran over this step's source, which the restore left byte-identical.
  - The ELEV004 tally (measured, not gated) went from 1373 at step 279 to 1376. Step 0280's run also reports 1376, so
    this step added none.
- **constructor-unit-tests:** `Test run summary: Passed!`, 1469 of 1469, in 68 s (deadline 180 s). That is the 1466
  baseline plus 3, and the TRX shows each of the three new facts `Passed`.
- **invariant-s28-\* (all sixteen):** each `git grep` finds nothing, also with `--untracked`.

No run went over its deadline.

## Artifacts

- `specs/0050/.artifacts/0281-build.log`
- `specs/0050/.artifacts/0281-elevation.log`
- `specs/0050/.artifacts/0281-constructor-unit-tests.log`
- `specs/0050/.artifacts/0281-invariants.log`
- `specs/0050/.artifacts/0281-mutation-build.log`, `specs/0050/.artifacts/0281-mutation-tests.log`
