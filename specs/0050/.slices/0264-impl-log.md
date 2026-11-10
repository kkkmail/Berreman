# Step 0264 — implementation log

## Progress

- Read the task, the ASG system prompts, the project prompt, the slice and step 0263's state of the world.
- Wrote the impl-plan.
- `TableAndElementRotationView.fs`: added `SwapSlot of Library.ElementId * ItemId` after `AssignSlot`; moved
  `AssignSlot`'s slot / item / choice resolution into the private `slotAssigned` (and the slot replacement into
  `withSlotFilled`), which both arms call; added the `SwapSlot` arm after `AssignSlot`'s.
- `SetupModelTests.fs`: the `QZ2` draft, the `platesStocked` / `benchHolding` / `twoBenches` / `heldSetup` /
  `withReadOnlyReopen` fixtures and the three facts the step names.
- First build: exit 0, 0 errors. Its only warnings were the vendored MathNet's two existing `SYSLIB0051`, shown because
  that project recompiled; the vendored code is untouched.
- First `SetupModelTests` run: the three new facts failed in the fixture. The Main scene seeds only the source and the
  detector (`mainSeedElements`; the src / sample / det table is the inverse scene's). `platesStocked` now adds the
  sample through the existing `withSample`, which gives the brief's "a source, a sample and the seeded detector". Rebuilt with 0 warnings; 14 of 14 passed (11
  before, 3 new).
- Ran each gate's own command over the final tree (below).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SetupModelTests.fs`

## Decisions

- **One resolution for both arms.** "Refuses as `AssignSlot` does" is met by calling the same code, not by copying
  it ("Reuse, never copy"). `AssignSlot`'s inline resolution moved unchanged into the private `slotAssigned model
  slotId itemIdOpt`, and its slot replacement into the private `withSlotFilled`. `AssignSlot`'s texts and behaviour are
  unchanged, and its three facts still pass. Both helpers are private and take only our types. No message, field or
  option was added beyond `SwapSlot`.
- **`SwitchToHeldSetup s` is `ActivateSetup s.name`'s update.** The arm calls `updateTableAndElementRotation
  (ActivateSetup s.name) model` (the update is already `let rec`). So the table is the held setup's slots drawn
  through the items, selection and drag are reset as a load resets them, `kitStatus` is `Loaded setup '<name>'.` and
  nothing is written. It is outside `guardKitWrite`, so a read-only kit takes it.
- **`CreateSwappedSetup s`** is behind `guardKitWrite` (step 185). On `Ok` it is `model.withSetupRecorded s` (name
  order, active) with `elements = swapped` and `kitStatus = Created setup '<name>' by the swap.`. On `Error e` only
  `kitStatus` changes, to `Not saved to the kit — <e.describe>`. Selection is kept because the swapped slots keep
  their ids. The setup swapped from is never passed to `recordSetup`.
- **The fresh name.** With a setup active it is `active.name.nextFreeAmong` the held names (`Bench A` → `Bench A (2)`);
  otherwise it is `SetupName.firstFree` of them (`Setup 1`).
- **New content under the read-only kit.** That fixture's kit holds only QZ and QZ2, and `Bench A` / `Bench B` hold one
  each. So the "swap to new content" adds one linear polarizer to the table (a table edit, which a read-only kit
  takes). `swapTarget` compares kinds and contents slot by slot, so no held setup matches, and the swap reaches the
  read-only refusal. The file snapshot is taken before both swaps and the added slot, so it also shows that adding the
  polarizer wrote nothing.
- **"No setup yet" runs in a second fresh kit** inside the same fact. A model from before `Bench A` was created would
  hold stale `kitSetups` against the folder.
- **The switch fact also checks the `Not swapped` refusal** (an unknown slot and an item the kit lacks). It changes
  nothing and writes nothing. This adds assertions to an authorized fact, not a new identity.

## Testing state

Development runs of each gate's own command over the final tree. No verdict is claimed (Invariant 6); the supervisor
runs the frozen roster.

- build: exit 0, 0 warnings, 0 errors, 22 s incremental (deadline 600 s).
- elevation: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, 214 s (deadline 600 s).
- constructor-unit-tests: `Passed!`, 1429 succeeded (1426 → 1429, +3), 0 failed, 0 skipped, 78 s while elevation ran
  alongside (deadline 180 s). All three new identities are `Passed` in `TestResults/constructor-unit-tests.trx`, under
  the census's exact names.
- ui-smoke: `Passed!`, 245 succeeded (unchanged), 0 failed, 0 skipped, 210 s (deadline 600 s).
- ui-tests over the selected `SetupSaveLoadTests` (the gate's filters, `ui-smoke` and `kit-acceptance` excluded):
  `Passed!`, 4 succeeded, 0 failed, 0 skipped, 18 s (deadline 360 s). No UI test file was touched, so the class's
  count is unchanged.
- The fifteen s26 invariant greps each find nothing.
- `git diff --numstat` equals `--ignore-cr-at-eol` for both edited files: no CRLF churn.
- No run exceeded its deadline.

Every requirement of the slice is addressed; nothing is deferred.

## Artifacts

- `specs/0050/.artifacts/0264-dev-build.log`: the final development build.
- `specs/0050/.artifacts/0264-dev-setupmodeltests.log`: `SetupModelTests` alone, 14 of 14.
- `specs/0050/.artifacts/0264-dev-elevation.log`: the elevation run.
- `specs/0050/.artifacts/0264-dev-constructor-unit-tests.log`: the whole constructor suite.
- `specs/0050/.artifacts/0264-dev-ui-smoke.log`: the ui-smoke suite.
- `specs/0050/.artifacts/0264-dev-ui-tests.log`: the selected `SetupSaveLoadTests`.
- `specs/0050/.artifacts/0264-dev-invariants.log`: the fifteen invariant greps.
