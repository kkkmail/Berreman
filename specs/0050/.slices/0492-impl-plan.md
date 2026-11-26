# Step 0492 — impl plan (WIRE_UI)

## Approach

1. `OpticalConstructor.App/Program.fs`, `ConstructorScene.mount`: the kit-bar shell gains
   `planRuns = PlanJobHost.onUiThread (…StudyPlan (PlanProgressed p)) (…StudyPlan (PlanEnded r))` over `hostDispatch`,
   the dispatch the init command captures — the same shape as `studyFits` beside it. `open OpticalConstructor.Ui.PlanJob`
   is added. The `MainConstructorWindow` and `InverseConstructorWindow` doc lines about fits off the UI thread also name
   the Plan step. No other seam changes.
2. `AGENTS.md`, gotcha "The Inverse flow ends at the study": the flow list becomes
   `Unknowns → Plan → Fit → Results → Publish after Data` and the slice's one sentence on the Planner is added; the bullet
   is rewrapped to stay within six lines. Nothing else in the file changes.
3. `KitWindowTests`: a new `ui-smoke` fact, `acceptance: the Inverse constructor plans a kit's next experiments off the
   UI thread and caches them under .oc/planner`. It builds `TestAppScope.planKitOver` over
   `TestAppScope.freshAppContext (FolderName "KitWindowTests")`, closes the model's kit, reopens its `kitFolderOpt`
   through `KitOpening.openKitAt` over that context, shows an `InverseConstructorWindow` as step 419's fact does, presses
   `flowPlanStep`, `studyChoice QZ` and `planRunButton` at their centres, and pumps `Dispatcher.UIThread.RunJobs` until
   `planRankText` shows text (30 s bound, no sleep). It then asserts exactly two `.oc/planner/*.candidate-json` files and
   `planCandidateList` holding QZ-Z's two `planCandidateRow`s (one transmitted, one reflected), with no plan error.

## Order of presses

The study bar (`studyChoice`) is not rendered in the Setups step an Inverse window opens on; it is in every study step,
the Plan step included. So the fact presses `flowPlanStep` first, then `studyChoice QZ` in the Plan step's study bar,
then `planRunButton` — only the three controls the slice names.

## Risks

- Red before the wiring: with `PlanJobHost.inert` the press starts nothing, `planRankText` never shows and the fact
  fails on its wait — the TDD red for this step.
- Plan duration: the 0491 measurements put a whole plan fact at 8–12 s with the fixture; 30 s is the slice's bound.
- AGENTS.md's six-line bound forces longer wrapped lines than the file's usual ~120 columns; only rewrapping, no
  wording beyond the slice's.
