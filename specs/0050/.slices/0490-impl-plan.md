# Step 0490 — impl plan

The Plan view: its own surface and the flow strip's Plan step between Unknowns and Fit, in the Inverse constructor
alone. The model (`PlanJobState`, `PlanSurfaceState`, `planMaterials`, `planMaterialOpt`, `planReportOpt`, `StudyPlan`,
`SetPlanMaterial`, `SetPlanCriterion`, the shell's `planRuns`) is steps 0487–0489's; this step adds only the step and the
view over it.

## Production

- `TableAndElementRotationView.fs`: `ConstructorFlowStep.PlanFlowStep` between `UnknownsFlowStep` and `FitFlowStep`,
  caption `Plan`, id `flowPlanStep`; `all` gains it; `stepsOf ForwardConstructor` drops it with the study's steps; the
  `ShowFlowStep` arm for the Inverse-only steps gains it. `SwitchConstructorMode ForwardConstructor` already leaves any
  step `stepsOf ForwardConstructor` lacks.
- `UiIds.fs`: the fourteen fixed ids under `// --- The study's Plan step (PlanView). ---`, after the Unknowns group.
- `ConstructorUiIdOfDomain.fs`: `planMaterialChoice (version : MaterialVersionId)` = `PlanMaterial_<GUID>_v<n>`.
- New `PlanView.fs` (after `StudyUnknownsView.fs`, before `StudyFitView.fs`): `planView model dispatch`, the
  scroll-both-ways panel with the study bar, then for the chosen study the material row, the criterion row, the run row
  (Run plan · Cancel · progress bar · progress text, keyed by study/material/criterion so `actionButton`'s retained
  handler never sends a stale request), the refusal, and the committed report's findings, rank line, confounded
  combinations, bounds grid and instrument lines.
- `KitWindowView.fs`: `PlanFlowStep` shows `PlanView.planView` keyed `plan`.

## Tests

- `TestAppScope.planKitOver` / `planKitIn`: the probe quartz saved through the context's material store, QZ-Z through
  its sample store, Laser / QZ-Z / Polarimeter recorded through the kit's library, the kit reopened into an Inverse
  model, the Polarimeter in `det`, study QZ created, the material opened, g11 and g33 freed within ±50 %.
- `PlanViewTests` (new, last in the compile list): the ids fact; the report fact over a real plan through
  `PlanJobHost.posting`, its posted messages folded into the model; the ui-smoke Run/Cancel fact with a recording host.
- `KitWindowTests`: the three changed facts. `KitWindowModelTests`: the two changed facts. `ScrollExtentTests`: the Plan
  step's reachability over a completed and a running plan.

## Risks

- A real plan runs the Berreman forward over 2 × 16 configurations; it is run once per fixture use, off the UI thread.
- `actionButton` retains its first handler per key: the Run row must be keyed by its request.
- Read-only: a plan writes no kit file, so Run plan stays offered (the brief), while Add to kit is step 0491's.
