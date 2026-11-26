# Step 0278 — impl log

## Progress

- [x] Read the protocol, the project prompt, the slice and the gate snapshot. Also read `ExpCommit` and
  `GenerateSetupGrid`, `Experiments.commit`, `commitOverSetups` and `canCommit`, `SetupModelTests`, and the read-only
  fact of `KitWindowModelTests`.
- [x] Split `ExpCommit`'s two halves into shared code; `GenerateSetupGrid` calls the experiment half.
- [x] Added `RecordExperiment`: the message, its one-line doc and its arm.
- [x] Added the four `SetupModelTests` facts and extended the read-only fact.
- [x] Ran build, constructor-unit-tests (whole), ui-smoke (whole), ui-tests (the six selected classes), elevation (all
  12 projects) and the sixteen s28 invariants locally, all on the final tree.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs` (+123 / −51):
  - `TableAndElementRotationMsg.RecordExperiment`, placed after `GenerateSetupGrid`. Its doc reads "A new experiment
    of the draft over the active setup (spec 0050 S8.4; 001 §P `RecordExperiment`)."
  - `member model.tableSetup : Result<Setups.Setup, KitLibraryError>`, placed after `tableWrittenOver`. It is
    `ExpCommit`'s former local `tableSetup`, moved here unchanged: the active setup written over (`revisedWith` over
    `pinFor`), or `Setup.create` under `setupNameAmong … (SetupName.firstFree taken)`.
  - Three private functions placed before the update:
    - `writtenInOrder record held writes model` is step 276's local fold, lifted out of `GenerateSetupGrid`. Its
      `state` argument became the starting `model`, because both callers started from `Ok model`. Its body is
      unchanged.
    - `withTableRecorded setup model` calls `recordSetup` unless the setup is the unchanged active one, then
      `withSetupRecorded`, which sets `kitSetups` and `activeSetupOpt`.
    - `withExperimentsRecorded committed experiments model` records each experiment through `recordExperiment`, in
      order, via `writtenInOrder`. After each write the collection is `committed` with that experiment placed by id
      (replaced if held, appended if not).
  - `appendedOver held committed` is a fourth private function: the experiments a commit appended. It is step 276's
    `heldIds` / `appended` lines, which `GenerateSetupGrid` and `RecordExperiment` now share.
  - `ExpCommit` calls `model.tableSetup`, then `withTableRecorded setup model` and then
    `withExperimentsRecorded committed [ experiment ]`. The `Not saved to the kit — …` failure answers the original
    `model`, as before.
  - `GenerateSetupGrid` calls `writtenInOrder library.recordSetup setupHeld fresh model`, then
    `Result.bind (withExperimentsRecorded committed appended)`. Its texts are unchanged.
  - The `RecordExperiment` arm sits behind `guardKitWrite` (see the Decisions section).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SetupModelTests.fs` (+146 / −2):
  - an "S8.4" section with one private helper, `narrowed`, which sets `ExpSetRangeMax 180`;
  - the four facts named in the slice;
  - the module doc now names `RecordExperiment` and S8.4.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs` (+2): the read-only fact also sends
  `ready |> RecordExperiment`, under the same assertions.

## Decisions

- **The first half is a pure member plus a recording function.**
  - `ExpCommit` decides whether to write anything before it writes. It answers the model untouched when the commit
    changes nothing or edits nothing, and that decision needs the setup's reference.
  - A single function that both computed and recorded the setup would have forced `ExpCommit` to write the setup
    first. Its outcome would then change for a draft whose commit appends nothing, for example an element with no
    variable to vary.
  - So the computation is `model.tableSetup`, a fact about the model and therefore a member (the ELEV013 shape), and
    the recording is `withTableRecorded`. No line of either half exists twice.
- **The experiment half places by id.** `ExpCommit` records the experiment its draft edits, and that experiment may
  replace a held one. The grid and `RecordExperiment` record appended experiments.
  - Replace-or-append yields exactly `committed` once every write succeeds, because `commit` edits in place and
    appends at the end.
  - It equals step 276's append for new ids, so the grid's partial-failure collection is unchanged.
- **`ExpCommit`'s failure outcome is unchanged.** When the setup is written but the experiment write fails, the
  answer is still the original model with `Not saved to the kit — …`, as before. The `snd` of the experiment half's
  error is its error.
- **`RecordExperiment`'s order follows the slice:**
  1. `guardKitWrite` refuses a read-only kit.
  2. `not canCommit` gives `Not recorded — choose the element and the variable to vary in the Experiments bay first.`
  3. `draftCapture = None` gives `Not recorded — no detector captures this table.`
  4. Otherwise `tableSetup` and `withTableRecorded` run. Their error keeps the model, with `Not saved to the kit — …`.
  5. Then `chooseMeasurement capture` and `commitOverSetups [ setup.reference ]` run. `commitOverSetups` appends a new
     experiment whatever the draft edits, and leaves the draft editing nothing. The appended experiment
     (`appendedOver`) is recorded through `withExperimentsRecorded`. Its error keeps the collection, with the setup
     recorded, and the same text.
  6. Success sets `Recorded <ids> over <setup.reference.describe>.`
- **The status renders the appended experiments' ids joined by ", ".** A one-reference commit over a committable
  draft appends exactly one, so the text is `Recorded E2 over 'Bench' revision 1.`. Joining keeps the arm total and
  needs no unreachable branch.
- **The id is rendered through `experiment.id.fileStem.value`.** That is the experiment's file identity `E<n>`, which
  step 261's messages already use.
  - The elevation run counts it as one more suppressed ELEV004 in the Ui project: 511 before, 512 now.
  - ELEV004 is excluded in the gate command (R7) and measured only.
  - The slice's edits do not cover `ConstructorUiIdOfDomain.fs`, so no render member was added to `ExperimentId`.
- **Test 4 asserts the full refusal text and the `Not recorded` prefix.** It sends `ExpNew` over a table with a
  detector, so the refusal comes from the draft and not from the capture.
- **No test identity was added or renamed beyond the census.** The four facts are created as named, and the read-only
  fact keeps every assertion.

## Testing state

Production code was written first. The four new facts reference `RecordExperiment`, so before this step they did not
compile, which counts as red under the protocol's "new code" rule. A `RecordExperiment` that behaved like `ExpCommit`
would fail facts 1 and 2: the second send would edit `E1` rather than append `E2`.

| Run | Command | Result | Time (deadline) |
|---|---|---|---|
| build | `dotnet build Berreman.slnx -c Release -nologo -v:m` | pass, 0 errors; the only warnings are the 2 pre-existing vendored MathNet `SYSLIB0051` | 72 s (600 s) |
| constructor-unit-tests | the gate's command, whole suite | `Test run summary: Passed!`, 1464 of 1464 | 73 s (180 s) |
| ui-tests | the gate's command, the six selected classes | `Passed!`, 89 of 89 | 111 s (360 s) |
| ui-smoke | the gate's command, whole | `Passed!`, 245 of 245 | 203 s (600 s) |
| elevation | `Elevation/Run-ElevationGate.ps1 -ExcludeRules ELEV004`, no base, so all 12 projects | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0 | 188 s (600 s) |
| invariant-s28-* | all sixteen greps | each finds nothing | — |

- **constructor-unit-tests:** the baseline was 1460 (step 277), so the count rose by 4. In the TRX the four new facts
  and `a kit opened read-only refuses every write message and writes nothing` show `Passed`. So does every
  `ExpCommit` / commit fact and every `GenerateSetupGrid` / grid fact of `SetupModelTests`.
- **ui-tests:** no UI test was touched, so the count for the selected classes is unchanged. They are
  `ExperimentControlsTests`, `FsxAcceptanceTests`, `InverseConstructorTests`, `ScrollExtentTests`,
  `SetupSaveLoadTests` and `SolverHandoffWindowTests`.
- **ui-smoke:** this run was sequential, while elevation ran alongside constructor-unit-tests and ui-tests.
- **Invariants:** see `0278-invariants.log`.

No run exceeded its effective deadline. These are local development runs; the arc-runner's gate engine is the
authority.

## Artifacts

- `specs/0050/.artifacts/0278-build.log`
- `specs/0050/.artifacts/0278-constructor-unit-tests.log`
- `specs/0050/.artifacts/0278-ui-tests.log`
- `specs/0050/.artifacts/0278-ui-smoke.log`
- `specs/0050/.artifacts/0278-elevation.log`
- `specs/0050/.artifacts/0278-invariants.log`
