# Step 0491 — impl log

## Progress

- Read the task, both system prompts, the project prompt, rulings R0001–R0005 (all confined to earlier steps; their
  preserved behaviours are untouched here), the slice and step 0490's state of the world.
- `UiIds.fs`: `planCandidateList` at the end of the Plan step's group.
- `ConstructorUiIdOfDomain.fs`: the private `PlanCandidate.planCandidateUiKey` (one helper writing the key) and the
  static members `planCandidateRow` / `addPlanCandidateButton` beside `planMaterialChoice`.
- `PlanView.fs`: `RankedPlanStep.planCaptionAt` (private extension member) and `planCandidateRows`, appended after the
  report's instrument lines; coverage is read in the view over `model.kitSetups` and the collection's experiments.
- Tests: the two new `PlanViewTests` facts, the extended ids fact, the Plan reachability fact and the read-only fact.
- First filtered run: one failure, a null `TextBlock.Text` in the read-only predicate; `textsOf` now skips a block
  holding no text (it also serves step 0490's fact, unchanged in meaning).
- Mutation check (one build, two mutations, reverted byte for byte):
  - Add to kit offered regardless of coverage and writes → `KitWindowTests.a read-only window withholds every write
    control` and `pressing Add to kit records…` fail;
  - the criterion's measure dropped from the row → `the Plan step ranks the candidates…` fails on the row text.

- First elevation run: `FAILED - 1 new finding`, `ELEV010P PlanView.fs:170` — the private extension member
  `planCaptionAt (position : int) (criterion : PlanCriterion)` mixed a bare `int` with our type. Members are exempt from
  the primitive-signature rules only while the signature names no type of ours. Fixed by moving the number out: the
  member is now `RankedPlanStep.planStepCaption (criterion : PlanCriterion)` and the list's local `rowOf` writes
  `<k>. ` before it. The row text is unchanged.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/PlanView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/PlanViewTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ScrollExtentTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`

## Testing state

Final runs over the final tree, each with the gate's own command:

| Gate | Outcome | Measured | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors; only the exempt `NU1902` | 18 s (incremental; 85 s on the first build after the edits) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0 | 255 s | 600 s |
| constructor-unit-tests | `Passed!`, 1905 of 1905 (unchanged) | 137 s | 180 s |
| ui-smoke | `Passed!`, 270 of 270 = 269 + `pressing Add to kit records…` | 232 s | 600 s |
| ui-tests (selected `KitWindowTests`, `PlanViewTests`, `ScrollExtentTests`) | `Passed!`, 41 of 41 = 40 + `the Plan step ranks…` | 68 s | 360 s |
| 21 × `invariant-s46-*` | each grep finds nothing, also with `--untracked` | — | 300 s |

No run exceeded its effective deadline. The edited files carry no CR byte.

## Artifacts

- `specs/0050/.artifacts/0491-build.log`, `0491-elevation.log`, `0491-constructor-unit-tests.log`, `0491-ui-smoke.log`,
  `0491-ui-tests.log` — the final gate-command runs.
- `0491-build-uitests.log`, `0491-ui-tests-planview.log`, `0491-ui-smoke-planview.log` — the iteration runs.
- `0491-mutation/run.log` — the mutation check's red run.

## Gotchas

- The determinant row's log det of the fixture is negative (`-10.322`): it is a log pseudo-determinant in the scaled
  coordinates less rank · ln noise, so the sign carries no meaning of its own.
- The read-only predicate met a `TextBlock` whose `Text` is null; `textsOf` skips such blocks.
- `ELEV010P` fires on a member too: an `int` parameter beside one of our types is a breach (see Progress).
- Choices the brief leaves open, taken from the surrounding code:
  - the row is a horizontal `StackPanel` whose caption does not wrap (AGENTS.md: no wrapping text in a horizontal
    StackPanel), keyed by its id so a row never takes another candidate's handler;
  - the measure and the `Already in the kit` lines follow the report's own criterion and `inKit`, not the surface's
    current choice: the view shows what the report holds;
  - `In the kit as <name>.` shows whatever the kit's access, since it writes nothing; Add to kit needs writes;
  - the key's line in nm repeats `PlanCandidate.caption`'s one-decimal formatting: the Domain file is outside this
    step's edits, so no shared member could be extracted.
