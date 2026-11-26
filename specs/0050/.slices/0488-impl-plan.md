# Step 0488 — impl plan (the window plans a study's material)

## Approach

Host step 0487's plan job in the constructor window exactly as step 0412 hosts the study fit: the host in the shell, the
state in the model, one message wrapping `PlanMsg`.

1. `KitWindowShell` gains `planRuns : PlanJobHost` after `studyFits`; `KitWindowShell.inert` holds `PlanJobHost.inert`.
2. Before `TableAndElementRotationModel`: `PlanSurfaceState = { materialOpt : MaterialVersionId option; criterion :
   PlanCriterion }`. The model gains `planJob : PlanJobState` and `planSurface : PlanSurfaceState` after `studyFit`;
   `initWith` sets `PlanJobState.idle` and `{ materialOpt = None; criterion = DeterminantPlanCriterion }`.
3. On the model extension holding `studyForwardContext`: `planMaterials` (the chosen study's opened materials whose
   physics is a crystal, in study order), `planMaterialOpt` (the surface's choice when `planMaterials` holds it, else the
   first) and `planReportOpt` (the committed report when its study and material are the chosen ones).
4. `TableAndElementRotationMsg` gains `StudyPlan of PlanMsg`, `SetPlanMaterial of MaterialVersionId` and
   `SetPlanCriterion of PlanCriterion`. `StudyPlan msg` is `msg.updateWith (PlanEnvironment.create
   model.kitShell.planRuns model.kit model.studyForwardContext) model.planJob`, with no read-only guard.
   `SetPlanMaterial` changes the surface only when `planMaterials` holds the version.
5. Tests: new `PlanWindowModelTests.fs` (`module PlanWindowModelTests`), last in the compile list, with the three
   census facts. They run over `initInverse` loaded from an in-memory kit. The kit holds `PlanRunTests`' items (Laser,
   Polarimeter, QZ-Z), the probe quartz version, and two studies. `QZ` is `PlanRunTests.planStudy` renamed: g11 and g33
   unknown. A second study opens the tabulated glass (step 460's `TabulatedStudyFixtures`) before the crystal. The
   catalogue store receives the crystal, the glass and the QZ-Z sample first, so the kit can snapshot them and the
   window resolves QZ-Z.

## Files

- `OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `OpticalConstructor.Tests/PlanWindowModelTests.fs` (new)
- `OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (one Compile line, last)

## Risks

- **Record-field inference.** Opening `PlanJob`, `PlannerInformation` and `PlannerRuns` in the view can shadow record
  labels: `PlanJobState` has the same labels as `StudyFitJobState`, and `PlanRequest` / `PlanReport` share `study`.
  Annotate wherever a literal is ambiguous; the build will tell.
- **The suite deadline.** The constructor suite stood at 139 s of its 180 s deadline at step 0487. The completion fact
  must not add a real solve. It seeds the window kit's `.oc/planner/` cache with literal rows, under the keys the
  window's own forward context gives. The plan then reads its rows from the cache, through the posting host, in
  milliseconds.
- **ELEV rules.** Every new member and field carries only our types. No `.IsSome`: filters pattern-match.
