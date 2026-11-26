# Step 0492 — state of the world

## Where we are

Spec 0050, section S14.2: the Measurement Planner.

- Steps 0486–0491 built the plan run, its job host, the window model, Add to kit and the Plan view with its report and
  ranked candidates.
- All of that ran through `PlanJobHost.inert` in the app until now.
- This step composes the real host into the constructor windows and proves the Plan step end to end through a shown
  Inverse window over a real kit folder.

## What's working

- The constructor windows' shell runs plans on `PlanJobHost.onUiThread` over the window's dispatch
- A plan pressed in an Inverse window runs on a worker, caches its candidates under `.oc/planner/` and posts its end
  back to the window
- AGENTS.md's Inverse-flow gotcha lists Plan before Fit and names the Planner, its cache and Add to kit
- Added the ui-smoke acceptance fact: the Inverse constructor plans a kit's next experiments off the UI thread and
  caches them under .oc/planner

## Tests

These are local runs over the final tree, each with the gate's own command, run one after another. The supervisor's gate
engine is the authority.

| Gate | Outcome | Measured | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors; only the exempt `NU1902` | 4 s (incremental; 85 s on the first build) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0 | 228 s | 600 s |
| constructor-unit-tests | `Passed!`, 1905 of 1905 (no test of this suite changed) | 131 s | 180 s |
| ui-smoke | `Passed!`, 271 of 271 = 270 + `acceptance: the Inverse constructor plans a kit's next experiments off the UI thread and caches them under .oc/planner` | 242 s | 600 s |
| ui-tests (selected `KitWindowTests`) | `Passed!`, 16 of 16, unchanged: the new fact carries `ui-smoke` | 32 s | 360 s |
| 21 × `invariant-s46-*` | each grep finds nothing, also with `--untracked` | — | 300 s |

- **Red first.** The new fact was built and run alone before the wiring. It failed with its own message, "the real plan
  host must post the plan's end to the window within thirty seconds" (36.5 s). Every press landed, and the inert host
  never ended the plan. After the wiring it passed alone in 13.8 s, and in 24.7 s inside the whole ui-smoke run.
- **Counts.** ui-smoke rises by 1. constructor-unit-tests and the selected ui-tests are unchanged, as the acceptance asks.
- No run exceeded its effective deadline.

```yaml
gates:
  constructor_unit_tests: 1905
  ui_smoke_tests: 271
  ui_tests: 16
```

## Architecture

- **One added seam.** `ConstructorScene.mount` gains `planRuns = PlanJobHost.onUiThread`. It sends
  `StudyPlan (PlanProgressed p)` and `StudyPlan (PlanEnded r)` through `hostDispatch`, the dispatch the init command
  captures. It sits beside `studyFits`, in the same shape. No other seam, message or model field changes.
- **The fact composes the window as step 419's does.**
  - It builds the fixture with `TestAppScope.planKitOver` over `TestAppScope.freshAppContext owner` (`KitWindowTests`).
  - It closes that model's kit and reopens its `kitFolderOpt` through `openKitAt` (`openedAt`) over the same context, so
    the library already holds every snapshot.
  - It shows `InverseConstructorWindow(ctx, opened)` and drives it by id at the controls' centres.
- **What the fact asserts:**
  - the rank text is the fixture's report, `Rank 0 of 2 from what the kit holds: 2 combination(s) confounded.`;
  - no plan error shows;
  - the kit folder holds exactly two `.oc/planner/<sha256>.candidate-json` files;
  - `planCandidateList` holds QZ-Z's transmitted and reflected rows, captioned `1. QZ-Z (z-cut plate), …` and
    `2. QZ-Z (z-cut plate), …`.

## Deferred

Nothing. All three parts of the brief landed this step: the wiring, the AGENTS.md gotcha and the acceptance fact.

## Gotchas

- **Press order.** An Inverse window opens on the Setups step, which has no study bar. The fact therefore presses
  `flowPlanStep`, then `studyChoice QZ` in the Plan step's own study bar, then `planRunButton`. These are the three
  controls the brief names, in the only order the window allows.
- **AGENTS.md line width.** Six lines (the brief's bound) for the bullet's words plus the new sentence need lines of up
  to 157 columns. The file's other lines stay within 130. Only the brief's two changes were made. The bullet was
  rewrapped, keeping the flow list on one line.
- **The wait.** It polls `Dispatcher.UIThread.RunJobs` until the rank text or the plan's error text shows, bounded at
  30 s from the Run plan press. There is no sleep. A refusal fails at once with its text.
- **Candidate row ids.** They are compared without their wavelength suffix (`PlanCandidate_<QZ-Z id>_<t|r>_`). That
  keeps the fact independent of the HeNe preset's line formatting, which `PlanViewTests` already pins.
- **Vendored warnings.** A full rebuild of the vendored MathNet `Numerics` prints `SYSLIB0051` from its own
  `Exceptions.cs`. That source predates this step, the warning appears in 707 earlier artifacts logs, and nothing here
  touches it.

## Changelog

- 2026-11-18 — Step 0492: the constructor windows compose `PlanJobHost.onUiThread`. AGENTS.md's Inverse-flow gotcha
  lists Plan before Fit and names the Planner's cache. `KitWindowTests` gains the ui-smoke acceptance fact proving a plan
  runs off the UI thread in a real Inverse window, caches its two candidates under `.oc/planner/` and shows its report
  and ranked rows.
