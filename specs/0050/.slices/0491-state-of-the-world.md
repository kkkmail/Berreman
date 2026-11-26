# Step 0491 — state of the world

## Where we are

Spec 0050, section S14.2: the Measurement Planner.

- Steps 0486–0490 built the plan run, its job, its window model, Add to kit and the Plan view with the report.
- This step adds the report's ranked candidates to the Plan view, each with Add to kit.
- Step 0492 composes the real host.

## What's working

- The Plan view lists the plan's ranked candidates in order, with rank, criterion measure and each target's σ
- Each candidate the kit lacks offers Add to kit while the kit takes writes; one it holds reads `In the kit as <name>.`
- A candidate's place in the kit is read live, so one added since the plan ran shows as added with no new run
- Added `planCandidateList`, `planCandidateRow` and `addPlanCandidateButton`
- Added two `PlanViewTests` facts; extended the ids, Plan reachability and read-only window facts

## Tests

These are local runs over the final tree, each with the gate's own command. The supervisor's gate engine is the
authority.

| Gate | Outcome | Measured | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors; only the exempt `NU1902` | 18 s (incremental) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0 | 255 s | 600 s |
| constructor-unit-tests | `Passed!`, 1905 of 1905 (no test of this suite changed) | 137 s | 180 s |
| ui-smoke | `Passed!`, 270 of 270 = 269 + `pressing Add to kit records a planned experiment of the candidate in the kit` | 232 s | 600 s |
| ui-tests (selected `KitWindowTests`, `PlanViewTests`, `ScrollExtentTests`) | `Passed!`, 41 of 41 = 40 + `the Plan step ranks the candidates and offers Add to kit for each the kit does not hold` | 68 s | 360 s |
| 21 × `invariant-s46-*` | each grep finds nothing, also with `--untracked` | — | 300 s |

- **Counts.** ui-smoke and ui-tests each rise by 1, as the acceptance asks.
- **The three changed facts pass:**
  - `the plan view UiIds are the stable intent-named ids` now pins `PlanCandidateList` and a test-local candidate's
    `PlanCandidate_…_r_532.0` / `AddPlanCandidate_…_r_532.0`;
  - `the kit window's Plan step leaves everything reachable` asserts the two ranked candidates' rows are in the window
    it checks;
  - `a read-only window withholds every write control` runs the fixture's plan and finds the rows but no Add to kit.
- **Red, shown by mutation.** One build carried two mutations, and each was caught by its intended fact:
  - Add to kit offered regardless of coverage and writes → the read-only fact and the ui-smoke fact failed;
  - the criterion's measure dropped from the row → `the Plan step ranks…` failed on the row text.

  Both were reverted byte for byte, and the final runs above are over the restored tree.
- **The first elevation run failed** with 1 × `ELEV010P`, and was fixed before the final runs; see Gotchas.
- No run exceeded its effective deadline.

```yaml
gates:
  constructor_unit_tests: 1905
  ui_smoke_tests: 270
  ui_tests: 41
```

## Architecture

- **The view computes nothing but each candidate's place in the kit.** Rank, measure, σ, `inKit` and order all come
  from the committed `PlanReport`. Only `PlanCandidate.coverageIn model.kitSetups model.experimentCollection.experiments`
  is read at render time (.spec-md §10.4). After Add to kit, the row turns to `In the kit as <name>.` on the next
  render, without a new plan run.
- **The list layout.** `planCandidateList` holds, in order:
  - one `Already in the kit: <caption>.` line per report `inKit` candidate;
  - then one row per ranked step;
  - or, with no ranked step, the `No candidate is open: …` line.

  Each row is a horizontal `StackPanel` carrying `planCandidateRow`, keyed by it. It holds a non-wrapping caption, then
  Add to kit or the in-kit text.
- **The candidate's id key** is one private extension member, `PlanCandidate.planCandidateUiKey`, in
  `ConstructorUiIdOfDomain.fs`. It writes `<plate item GUID>_<t|r>_<line nm, F1, invariant>`, and both ids read it.
- **Add to kit** sends step 489's `AddPlannedExperiment candidate`. This step adds no message, model field or domain
  member.

## Deferred

Nothing. Every requirement of the brief and its five census facts landed this step. The composed host is step 0492's
own brief.

## Gotchas

- **`ELEV010P` fires on members too.** `member private this.planCaptionAt (position : int) (criterion : PlanCriterion)`
  failed `elevation`, though members are said to speak primitives. The exemption holds only while the signature names
  none of our types. The fix moved the number into the list's local closure (`planStepCaption` takes the criterion
  alone); the row text is unchanged.
- **The fixture's log det is negative** (`-10.322` for the first row). It is a log pseudo-determinant less rank · ln
  noise, in scaled coordinates, so the sign means nothing on its own.
- **`TextBlock.Text` can be null** in the headless tree. `PlanViewTests.textsOf` skips such blocks.
- **Choices the brief leaves open:**
  - The measure and the `Already in the kit` lines follow the report's own criterion and `inKit`, not the surface's
    current choice.
  - `In the kit as <name>.` shows whatever the kit's access, since it writes nothing.
  - The key's line in nm repeats `PlanCandidate.caption`'s one-decimal formatting. `MeasurementPlanner.fs` is outside
    this step's edits, so no shared member could be extracted.

## Changelog

- 2026-11-18 — Step 0491: the Plan view's ranked candidates. Each row reads `<k>. <caption> — rank <r>`, the criterion's
  measure and σ, then Add to kit or `In the kit as <name>.`. Held candidates are listed first, and the no-open-candidate
  line shows when nothing is ranked. Added `planCandidateList`, `planCandidateRow` and `addPlanCandidateButton`, two
  `PlanViewTests` facts, and the extended ids, reachability and read-only facts.
