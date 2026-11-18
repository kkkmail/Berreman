# Step 0490 — impl log

## Progress

- Read the task, both system prompts, the project prompt, rulings R0001–R0005 and the slice. The rulings bind earlier
  steps' behaviour (series dispatch, the circular analyzer, element identities, the study fit's runtime and allocation);
  none of it is touched here, and every one is preserved.
- Wrote the impl plan.
- **Tests first, in the census's files.**
  - `KitWindowModelTests`: the two changed facts list Plan between Unknowns and Fit with its caption. They also check
    that `stepsOf ForwardConstructor` lacks Plan, that `ShowFlowStep PlanFlowStep` keeps the open bay, that leaving the
    Inverse constructor from Plan lands on Experiments, and that `ShowFlowStep PlanFlowStep` over `initMain ()` changes
    nothing.
  - `KitWindowTests`: the flow strip's ids pin `FlowPlanStep`. The flow strip fact finds the Inverse Plan box between
    Unknowns and Fit, `planPanel` alone on `PlanFlowStep`, and no Plan box in Main. The read-only fact finds Run plan
    in a read-only window over `planKitIn`.
  - `TestAppScope.planKitOver` / `planKitIn` (see Gotchas).
  - New `PlanViewTests`, last in the compile list: the ids fact, the report fact and the ui-smoke Run/Cancel fact.
  - `ScrollExtentTests`: `the kit window's Plan step leaves everything reachable`.
- **Production.**
  - `ConstructorFlowStep.PlanFlowStep` with its caption and id. It is added to `all`, filtered out of
    `stepsOf ForwardConstructor`, and added to the Inverse-only `ShowFlowStep` arm.
  - The fourteen ids in `UiIds.fs`, and `planMaterialChoice` in `ConstructorUiIdOfDomain.fs`.
  - New `PlanView.fs`, compiled after `StudyUnknownsView.fs`.
  - `KitWindowView`'s `plan` surface.
- **Green.** The build was green the first time. `PlanViewTests` passed 3 of 3, `KitWindowModelTests` 41 of 41, and the
  selected ui-tests 40 of 40.
- **Red, shown by mutation.** The tests were written in the same pass as the production code, so red is shown by
  mutation instead of a pre-production compile. One build carried four independent mutations; each was caught by its
  intended fact, then reverted byte for byte (`cmp`):
  - Main's strip keeps Plan: caught by `the Data step is the Inverse constructor's alone…` (Forward's steps
    `[…; PlanFlowStep]`), `the flow strip UiIds…` and `the flow strip shows…` (a Plan box in Main).
  - The findings list drops the structural findings: caught by `the Plan step shows…` (the off-axis finding not
    found).
  - Cancel and the progress bar leave the run's row: caught by `pressing Run plan…` (`PlanProgressBar must be in the
    window`).
  - Run plan is withheld from a read-only kit: caught by `a read-only window withholds every write control`
    (`PlanRunButton must be in the window`).
- **Extended the report fact** so the bounds grid is rendered at least once. The brief's rank-0 fixture never shows
  it. The extension is the same report at its last ranked step's standing (rank 2), and it checks:
  - the condition line;
  - `Weakest: …`;
  - the grid's header;
  - `3E-05` and `-1E-05` as the nominal values;
  - each σ and σ / |value|.
  It adds no test identity. `PlanViewTests` then passed 3 of 3.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`: `PlanFlowStep`.
- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`: the Plan step's fourteen ids.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`: `planMaterialChoice`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/PlanView.fs`: new.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`: one `<Compile>` line.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitWindowView.fs`: the `plan` surface.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/TestAppScope.fs`: `planKitOver`, `planKitIn`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/PlanViewTests.fs`: new.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`: one `<Compile>` line.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`: three changed facts.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ScrollExtentTests.fs`: one new fact.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`: two changed facts.

- **Elevation, first run: FAILED, with 6 × `ELEV013P` in `PlanView.fs`.** Each was a private helper taking one
  parameter of our own type (`planLine` and `planHeading` of a `ControlCaption`, `findingCaptions` and `reportRows` of
  a `PlanReport`, `rankCaption` and `confoundedCaptions` of a `PlannerInformation`). I had taken
  `MaterialEditorView.gainWarningRow` as precedent, but the analyzer flags this shape in `PlanView.fs`. Why the
  precedent passes (it may be among the run's suppressed findings) was not investigated.
  - **Fix:** each helper became a private extension member of its type (`planLine`, `planHeading`,
    `planFindingCaptions`, `planReportRows`, `planRankCaption`, `planConfoundedCaptions`), as `PlanJob.fs` does for
    `PlanCandidateCount.toJobStepCount`. No behaviour changed.
  - The script that applied the fix is `.artifacts/0490-elev013-fix.py`.
  - The rebuild and the second elevation run were clean.
- **Final local runs** over the final tree, each with its gate's own command; see Testing state.

## Testing state

The supervisor's gate engine is the authority; these are my local runs over the final tree.

| Gate | Outcome | Measured | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors; warnings only the exempt `NU1902` | 92 s (first, MathNet recompiled) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0 | 243 s | 600 s |
| constructor-unit-tests (whole, as selected) | `Passed!`, 1905 of 1905 (count unchanged: two changed expectations) | 137 s | 180 s |
| ui-smoke (whole, as selected) | `Passed!`, 269 of 269 = 268 + `pressing Run plan…` | 230 s | 600 s |
| ui-tests (`KitWindowTests`, `PlanViewTests`, `ScrollExtentTests`) | `Passed!`, 40 of 40, three of them the new census facts | 62 s | 360 s |
| 21 × `invariant-s46-*` | each decoded `git grep` finds nothing, also with `--untracked` | — | 300 s |

- Elevation ran standalone, with no comparison base, so it covered all 13 projects. That is wider than the arc-origin
  run.
- The TRX files name the four new facts exactly as the census ids.
- The touched files are LF, with 0 CR bytes, and `git diff --numstat` equals `--ignore-cr-at-eol`.
- No run exceeded its effective deadline.

`commit_ready: true`.

## Artifacts

- `0490-build-1.log`, `0490-build-2.log`, `0490-build-3.log`: the solution builds; `-3` is over the final tree.
- `0490-planview-tests-1.log`, `0490-planview-tests-2.log`, `0490-kitwindowmodel-tests-1.log`,
  `0490-ui-tests-1.log`: the development runs.
- `0490-mutation-build.log`, `0490-mutation-ui-tests.log`, `0490-mutation-model-tests.log`: the mutation round.
- `0490-elev013-fix.py`: the `ELEV013P` repair script.
- `0490-elevation.log`, `0490-constructor-unit-tests.log`, `0490-ui-smoke.log`, `0490-ui-tests.log`: the final gate
  runs.

## Gotchas

- **The fixture cannot reuse `PlannerInputsTests`.** `OpticalConstructor.Ui.Tests` does not reference
  `OpticalConstructor.Tests` (it links only `BuiltinCatalogue.fs` and `SceneTestFixture.fs`).
  - So `planKitOver` builds its own probe quartz and QZ-Z: the brief's numbers, under GUIDs of its own (`49000000-…`).
  - It saves them through the context's material and sample stores, as the brief says.
  - g11 and g33 are freed from the opened copy's own values (`componentValue`), within ±50 %, as
    `PlanRunTests.freedWithin` does.
- **`actionButton` keeps its first click handler per key.** So the run row is keyed by study, material and criterion.
  Otherwise Run plan would send a stale request after the criterion or material changed. This is the same pattern
  as `StudyFitView`'s run row.
- **The `σ / value` column is σ / |value|.** The header keeps the brief's text, but a relative uncertainty is
  positive, and g33 is negative. A zero or missing value shows `—`.
- **Two choices the brief leaves open:**
  - **The rank line at full rank without a finite condition** (or with no targets) reads
    `Rank r of n from what the kit holds.`. The brief names only the condition form and the confounded form.
  - **Before the first candidate**, the progress text reads `Starting…`, as `kitArchiveProgressView` does.
- **An empty instrument list stays empty.** The fixture's kit holds no polarizer, so `planInstrumentList` holds no
  line, as the brief's fact requires. The `Instrument states:` heading stays.
- **Run plan is offered in a read-only kit.** A plan writes no kit file; the model's `StudyPlan` arm has no read-only
  guard, and the read-only fact pins this. Add to kit is step 0491's.
- **The bounds grid would go untested by the brief's assertions**, because the fixture's baseline is rank 0. The
  report fact therefore also renders the same report at its last ranked step's standing (rank 2) and checks the grid
  cell by cell. This adds no test identity.
- **`SYSLIB0051` ×2 in the first build come from the vendored `MathNetNumerics/Numerics/Exceptions.cs`.** They
  appear whenever MathNet recompiles (0438 and 0439 logged them too). They are not this step's, and the later builds
  show only `NU1902`.
- **`ELEV013P` is enforced on private helpers.** A free private function whose one parameter is one of our types
  fails the gate. Put it on the type as a private extension member.
