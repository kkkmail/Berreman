# Step 0276 — impl log

## Progress

- [x] Read the protocol, the project prompt, the slice and the code it names: `SetupGrids.fs`, `Experiments.fs`, the
  `ExpCommit` / `CreateSetup` / `SwapSlot` arms, `guardKitWrite`, `withSetupRecorded` and `SetupModelTests`.
- [x] Added `TableAndElementRotationMsg.GenerateSetupGrid of SetupGrids.SetupGrid` and its update arm.
- [x] Added the naming-template gotcha to `AGENTS.md`.
- [x] Added the four `SetupModelTests` facts and split step 272's fixture.
- [x] Ran build, constructor-unit-tests, ui-smoke, the selected ui-tests, elevation and the fifteen s27 invariants
  locally on the final tree.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs` (+63 lines):
  - the message case `GenerateSetupGrid of SetupGrids.SetupGrid`, after `SwapSlot`, with a one-line doc citing spec
    0050 S8.3;
  - its arm in `updateTableAndElementRotation`, after the `SwapSlot` arm. A two-line comment cites S8.3.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SetupModelTests.fs` (+190 / −4):
  - The private `templateCommitted` is split into `templateTable`, which answers a private `TemplateTable` record (the
    table, the first polarizer's slot id and the sample's slot id), and `templateCommitted`, which commits that table
    as before and answers the same pair. The two facts already using it are unchanged.
  - New private helpers: `plateChoice`, `plateGrid` (made by `SetupGrid.create`, with (QZ2, 0°) unticked) and
    `sampleIn`.
  - The four new facts sit at the end of the module.
- `AGENTS.md` (+6 lines): the **Names come from the kit's naming template** bullet, directly after **The table is the
  active setup**.

## Decisions

- **Arm shape.** It sits behind `guardKitWrite` (the step-185 guard). Inside, it calls
  `grid.generate model.elements model.kitItems model.tryItemModelChoice model.kit.kitFile.namingTemplate model.kitSetups`.
  - `Error e` gives `Not generated — <e.describe>` and writes nothing.
  - For `Ok setups`, everything is computed before any write. `fresh` is the distinct setups not already in
    `kitSetups` (compared by equality). `committed` applies
    `chooseMeasurement capture |> commitOverSetups (references in order)` when `model.draftCapture` is `Some capture`,
    and is the collection unchanged otherwise. `appended` is the experiments of `committed` whose ids the model's
    collection lacked.
  - One local fold, `writtenInOrder`, does the writing. It is generic over the written thing and carries
    `Result<model, model * KitLibraryError>`. It records the fresh setups through `KitLibraryContext.recordSetup`, then
    the appended experiments through `recordExperiment`, which is how `ExpCommit` records. One fold serves both lists,
    so the stop-at-first-failure logic exists once.
- **The setups keep name order, and the active setup is not touched.** Each written setup joins the model through the
  existing `withSetupRecorded`, so `kitSetups` stays in ordinal name order. `activeSetupOpt` is then restored, because
  the slice says the active setup is unchanged, and `withSetupRecorded` would have made the new setup active.
- **The collection holds what was written.** Each written experiment extends the model's collection as
  `{ committed with experiments = held @ written }`. After a full run this equals `committed`. After a failure part-way
  it holds exactly the experiments on disk, under `committed`'s `nextId` and draft. If nothing could be committed, or no
  experiment was written before a failure, the collection is left exactly as it was. That covers the measurement too,
  so a draft the grid cannot commit keeps all its fields.
- **The draft after a run.** Following `commitOverSetups` (step 275), it is the draft editing nothing. Its element,
  variable and range stay as chosen, and the first fact asserts that for element and variable. Its measurement becomes
  the capture the detectors allow, as it does after an `ExpCommit`.
- **Counts.** `n` is the distinct setups the grid answers. `m` is `fresh.Length`. `k` is `appended.Length`.
- **AGENTS.md wording.** The slice says a second or later slot of a role, and the note, "are left out with the text
  before them". `NamingTemplates.writeName` leaves an optional placeholder out only when the reading lacks its value,
  so the bullet says "an empty second or later slot of a role, or an empty note". The bullet is six lines.
- **Test 4 (read-only).** Before reopening, the read-write kit records the table as `Bench`. The read-only model then
  draws `Bench` and chooses the polarizer. The fact first shows that `grid.generate` answers two setups over that
  model, then that the message gives the read-only status and changes no file, setup, experiment, element or active
  setup. This shows that only the guard stops the run.
- **Test 2's impossible grid.** It is a record (`unticked = []`) whose sample options are QZ2 at 0° and Laser at 0°.
  QZ2's setup would be new, so "no kit file changes" also shows that the possible half is not written.

## Testing state

Every gate ran locally on the final tree, using each gate's own command from `0276.gates`. Captures are under
`specs/0050/.artifacts/0276-local/`.

- build: `dotnet build Berreman.slnx -c Release -nologo -v:m` exited 0 with `0 Error(s)` in 73 s (deadline 600 s).
  The only warnings are the vendored MathNetNumerics `SYSLIB0051` ones that were already there. None came from our
  code.
- constructor-unit-tests, whole suite: `Test run summary: Passed!`, total 1457, succeeded 1457, failed 0, skipped 0,
  in 71 s (deadline 180 s). The baseline was 1453 at step 275, so the count rose by 4. The TRX shows all four new
  identities `Passed`:
  - `GenerateSetupGrid records a setup per ticked combination, named by the kit's template, and an experiment of the
    draft over each`
  - `a grid over setups the kit holds writes no setup again, and a grid it cannot make writes nothing`
  - `a grid over a draft that cannot be committed records its setups and no experiment`
  - `a kit opened read-only generates nothing`
- ui-smoke: `Passed!`, 245 of 245, in 221 s (deadline 600 s). It ran at the same time as the elevation gate, which
  explains the time; step 0263 measured 27 s.
- ui-tests, selected (`InverseConstructorTests`, `SetupSaveLoadTests`, with the gate's `ui-smoke` and `kit-acceptance`
  exclusions): `Passed!`, 18 of 18, in 27 s (deadline 360 s).
- elevation, from the arc origin `f395597c071910aaa215518626563f6c784bfb62` (`.arc-base-json`):
  `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 206s`, with 0 known findings (deadline 600 s).
- invariant-s27-*: all fifteen print `INVARIANT HOLDS`. They were run through the ASG evaluator commands in
  `0276.gates`.
- LF check: `git ls-files --eol` shows `i/lf w/lf` for every edited file, and none of them contains a CR byte.

Red: all four facts dispatch `GenerateSetupGrid`, so they cannot compile without the production case, which counts as
red under the base protocol. I wrote the case before the facts, so I never captured the red build on its own.

## Artifacts

- `specs/0050/.artifacts/0276-local/build.log`
- `specs/0050/.artifacts/0276-local/setup-model-tests.log` (the class alone: 20 of 20)
- `specs/0050/.artifacts/0276-local/constructor-unit-tests.log` and `constructor-unit-tests.trx`
- `specs/0050/.artifacts/0276-local/ui-smoke.log`
- `specs/0050/.artifacts/0276-local/ui-tests.log`
- `specs/0050/.artifacts/0276-local/elevation.log`
- `specs/0050/.artifacts/0276-local/invariants.log` and `invariant-commands.tsv`

## Gotchas

- `withSetupRecorded` also makes the recorded setup active. A grid must not change the active setup, so the arm puts
  `activeSetupOpt` back after each setup. Calling `withSetupRecorded` on its own would leave the last generated setup
  active.
- `generate` answers a held setup (or one made earlier in the same run) whenever a combination's slots equal its own.
  Two ticked combinations with equal slots therefore answer the same setup twice. The arm writes it once and counts it
  once in `n`, but `commitOverSetups` still commits once per ticked combination, so `k` can be larger than `n`.
- The experiments' capture is the table's (`model.draftCapture`), as `ExpCommit` reads it, not each generated setup's.
  The grid only re-picks items and angles, so the detectors, and with them the capture, are the table's in every setup.
- No fact makes a kit write fail part-way. The four facts the census names do not include one, and adding one would
  need a new test identity. The behaviour is in the design: the fold carries the model as written so far, and the
  experiments fold never runs after a failed setup write.
