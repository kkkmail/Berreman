# Step 0492 — impl log

## Progress

- Read the task, both system prompts, the project prompt, rulings R0001–R0005 (all confined to earlier steps; the
  behaviours they preserve are not touched here) and the slice.
- Read the seams: `PlanJobHost.onUiThread` (step 488), `KitWindowShell.planRuns` (inert by default), the `StudyPlan`
  update, `PlanView` (Run plan, rank text, candidate rows), `TestAppScope.planKitOver` (step 490) and step 419's
  Inverse-window fit fact.
- Red first: appended the new `ui-smoke` fact to `KitWindowTests` and built. Run alone over the unwired app it failed
  with its own message, "the real plan host must post the plan's end to the window within thirty seconds" (36.5 s,
  `0492-red-new-fact.log`): every press landed, and the inert host never ended the plan.
- `Program.fs`, `ConstructorScene.mount`: the kit-bar shell gains `planRuns = PlanJobHost.onUiThread` over
  `hostDispatch`, sending `StudyPlan (PlanProgressed p)` and `StudyPlan (PlanEnded r)`, beside `studyFits`;
  `open OpticalConstructor.Ui.PlanJob`. The `MainConstructorWindow` / `InverseConstructorWindow` doc line now reads "The
  Inverse window's study runs its fits and its Plan step off the UI thread (spec 0050 S11.5, S14.2)."
- `AGENTS.md`, gotcha "The Inverse flow ends at the study": the flow list reads `Unknowns → Plan → Fit → Results →
  Publish after Data` and the slice's sentence on the Planner follows the fit sentence; rewrapped to six lines.
- Green: the new fact passes alone in 13.8 s, fixture included (`0492-green-new-fact.log`).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`
- `AGENTS.md`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`

## Testing state

Final runs over the final tree, each with the gate's own command, one after another:

| Gate | Outcome | Measured | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors; only the exempt `NU1902` | 4 s (incremental; 85 s on the first build) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0 | 228 s | 600 s |
| constructor-unit-tests | `Passed!`, 1905 of 1905 (unchanged) | 131 s | 180 s |
| ui-smoke | `Passed!`, 271 of 271 = 270 + the new acceptance fact (24.7 s inside the run) | 242 s | 600 s |
| ui-tests (selected `KitWindowTests`) | `Passed!`, 16 of 16; the new fact carries `ui-smoke`, so this count is unchanged | 32 s | 360 s |
| 21 × `invariant-s46-*` | each decoded grep finds nothing, also with `--untracked` | — | 300 s |

No run exceeded its effective deadline. The three edited files carry no CR byte, and `git diff --numstat` equals
`--ignore-cr-at-eol`.

## Artifacts

- `specs/0050/.artifacts/0492-build-red.log`, `0492-red-new-fact.log` — the red build and run of the new fact.
- `0492-build-green.log`, `0492-green-new-fact.log` — the build and run after the wiring.
- `0492-build.log`, `0492-elevation.log`, `0492-constructor-unit-tests.log`, `0492-ui-smoke.log`, `0492-ui-tests.log` —
  the final gate-command runs.

## Gotchas

- Press order: an Inverse window opens on the Setups step, which has no study bar, so `studyChoice QZ` cannot be pressed
  first. The fact presses `flowPlanStep`, then `studyChoice QZ` in the Plan step's own study bar, then `planRunButton`:
  only the three controls the slice names, in the one order the window allows.
- AGENTS.md: the bullet's existing words plus the slice's sentence come to about 880 characters, so six lines need
  lines of up to 157 columns (the file's other lines stay within 130). Only the slice's two changes were made; the
  bullet was rewrapped to keep the six-line bound, with the flow list kept on one line.
- The fact's wait polls `Dispatcher.UIThread.RunJobs` until the rank text or the plan's error text shows, bounded at
  30 s; a refusal fails at once with its text instead of waiting out the bound.
- A full build recompiles the vendored MathNet `Numerics` project, which prints `SYSLIB0051` from its own
  `Exceptions.cs`; that is pre-existing vendored source (seen in 707 earlier artifacts logs) and not touched here.
