# Step 0328 — impl plan

## Approach

Section S9.3 closes with an end-to-end proof through the kit window over a real folder kit, plus two AGENTS.md edits.
No product code changes; `OpticalConstructor.App/Program.fs` is untouched.

1. `OpticalConstructor.Ui.Tests/KitWindowTests.fs` gains one `ui-smoke` fact:
   `acceptance: the Data step splits a lab file across the kit's experiments, excludes a point and corrects a setup,
   all kept in the kit`.
   - The kit is a fresh folder kit under the class's test directory (`TestAppScope.freshKitFolder owner`), opened
     read-write through `openKitAt` and loaded into the Inverse constructor (`kitWindowModel … InverseConstructor
     KitWindowShell.inert`, which is `initInverseWithKit` + `loadKit`).
   - Items LP1, QZ, QZ2, Laser and LPd are added through the Items panel's messages, reusing step 284's `withOfferAdded`
     with `linearPolarizerOffer` and `plateOffer`, plus one new offer predicate for the HeNe source preset.
   - The table: a linear polarizer, a sample turned to R1 45° and a second linear polarizer before the seeded detector,
     each filled by `AssignSlot`. Then `ExpChooseElement` on the first polarizer and `ExpCommit` give the setup and
     experiment `E1`, both named `LP1-(QZ#45)-LPd`.
   - Mount: `kitWindowView` in a `HostWindow` through `Program.mkProgram`. Its init command captures the dispatch
     (`Cmd.ofEffect`, as `ConstructorScene.mount` does), and its update is `updateTableAndElementRotation`, keeping the
     latest model.
   - The lab file is written outside the kit by generalising step 312's `labFileOutsideKits` to take the file's bytes.
     Step 312's fact passes its `phoneRoiBytes`, so this is a reuse, not a copy.
   - The drive, step by step:
     - press Data;
     - dispatch `SplitByColumn`;
     - press the QZ2 offer, then Apply split;
     - press E1's matrix entry;
     - dispatch `SetExclusionReason "saturated"`, then press Exclude on point 2;
     - dispatch `SetCorrectionNote "QZ was at 0"`, then press the re-point to `LP1-(QZ2#0)-LPd`.
   - Each step asserts on the window (ids, labels) and on the kit folder: files, `readExperiments` read-back, and the
     `data/` file's bytes.
2. `AGENTS.md`:
   - the Main ribbon gotcha's Data files clause now says Attach data file… lives in the Data step;
   - a new gotcha "The Data step never edits a lab file" goes after "Lab files are read through import profiles".

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`
- `AGENTS.md`

## Risks

- **The headless window width.** The offer toggle's caption is long, and a press at a centre outside the window would
  hit nothing. Mitigation: a window wide enough, with each press asserting the control is shown first.
- **The `LP1-(QZ#45)-LPd-2` group** reads through the template with the note `2`. It is offered over the held setup,
  stays unticked and writes nothing. The fact asserts only what the slice prescribes, plus the told status counts.
- **Dispatch timing.** An Elmish dispatch from the test is followed by `Dispatcher.UIThread.RunJobs()` before any
  assertion, the deterministic wait the class already uses.
