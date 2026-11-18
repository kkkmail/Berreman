# Step 0488 — impl log (the window plans a study's material)

## Progress

- [x] `KitWindowShell.planRuns` (inert: `PlanJobHost.inert`), `PlanSurfaceState`, `planJob` / `planSurface`, the three
  members and the three messages
- [x] `PlanWindowModelTests.fs` (three census facts) and its Compile line, last
- [x] Build, elevation, constructor-unit-tests, ui-smoke, ui-tests (selected), the 21 invariants

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`:
  - **Opens.** It opens `PlannerInformation`, `PlannerRuns` and `PlanJob`.
  - **Shell.** `KitWindowShell` gains `planRuns : PlanJobHost` after `studyFits`. `inert` holds `PlanJobHost.inert`, and
    its doc cites S14.2.
  - **Surface state.** `PlanSurfaceState { materialOpt; criterion }` is declared before the model, after
    `StudySurfaceState`.
  - **Model.** It gains `planJob : PlanJobState` and `planSurface : PlanSurfaceState` after `studyFit`. `initWith` sets
    `PlanJobState.idle` and `{ materialOpt = None; criterion = DeterminantPlanCriterion }`.
  - **Messages.** `StudyPlan of PlanMsg`, `SetPlanMaterial of MaterialVersionId` and `SetPlanCriterion of PlanCriterion`,
    each with its update arm.
  - **Members.** `planMaterials`, `planMaterialOpt` and `planReportOpt` sit on the extension holding
    `studyForwardContext`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlanWindowModelTests.fs` (new, `module PlanWindowModelTests` in
  `namespace OpticalConstructor.Tests`): the three facts.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: `PlanWindowModelTests.fs`
  appended last, with no comment.

Nothing outside `effects.edits` changed.

## Decisions

- **`StudyPlan` takes no read-only guard.** The arm is `message.updateWith (PlanEnvironment.create
  model.kitShell.planRuns model.kit model.studyForwardContext) model.planJob`, and the model takes the answered state.
  A one-line comment says why it differs from `StudyFit`'s guarded `RunFit`. No queue, second job or option.
- **`planMaterials`** is the chosen study's `materials` through `List.choose` over `physics.crystalOpt`, in study order.
  It is `[]` with no held study. It maps no option to a `bool` (ELEV012).
- **`planMaterialOpt`** is `planSurface.materialOpt` filtered to `planMaterials`' versions, else the first of them.
- **`planReportOpt`** is `planJob.job.committedResults` when its `study` is `selectedStudyOpt`'s name and its `material`
  is `planMaterialOpt`. The criterion does not take part, as the brief states.
- **`SetPlanMaterial`** changes the surface only when `planMaterials` holds the version. Otherwise it answers the model
  itself, so a test compares by equality. `SetPlanCriterion` always sets the surface's criterion.
- **The fixture's kit is in-memory, as the brief says.**
  - `openKitAt` opens only a disk folder, and `KitOpening.openedOver` is private (and outside this step's touches). So
    the fixture assembles `OpenedKit` from the in-memory kit's public reads: `readExperiments`, `readItems`,
    `readStudies` and `readSetups`, `resolveSnapshots ()` and `openOverlay []`, with `InMemoryKitFolder.kitFolder`. It
    has no fit runs, because the kit holds none.
  - That `OpenedKit` goes through `loadKit` over `initInverse`, with `freshStores`, `liveLibrary`, the shared import
    context and the store's item models, exactly as `StudyWindowModelTests.withWindow` loads its disk kit.
- **The catalogue holds what the kit snapshots.**
  - The probe quartz (`crystalEntry`), the tabulated glass (`uncertainTableEntry`, step 460's fixture) and QZ-Z's
    sample are put through `BuiltinCatalogue.putSealedEntries` before the kit is written.
  - With them there, `recordItem` / `recordStudy` snapshot them into the kit ("its uniaxial version"), and the window's
    `tryItemModelChoice` resolves QZ-Z.
  - QZ-Z's `Sample` is read from `qzZDraft.model` by matching, so `PlannerInputsTests` is not edited.
- **The studies.**
  - QZ is `PlanRunTests.planStudy` renamed, g11 and g33 unknown (reuse, not a copy).
  - "Glass and QZ" opens the glass first, then the crystal. Fact 2 uses it to show the filter and the default; fact 3
    uses it as the other study that opens the same crystal yet shows no report.
  - "Glass" opens the glass alone, so a study can plan nothing.
- **Fact 1 checks what the recorded job plans, without solving.** Each recorded job runs once under an observer whose
  token is already cancelled. It reads the window's QZ study and answers `PlanCancelledErr "0 of 2 candidates were
  evaluated"` before its first candidate. The read-write and read-only kits both get this check, and both check that
  the kit's paths are unchanged.
- **Fact 3 seeds the window kit's cache instead of solving.**
  - At step 0487 the constructor suite stood at 139 s of its 180 s deadline, and a real plan through the host under the
    parallel suite missed the ten-second wait. 0487's Gotchas: "A further real-plan fact should reuse a cached kit".
  - So the fact computes both candidates' keys from the window's own `studyForwardContext`, and
    `model.kit.cachedPlannerRows` stores literal rows under them (transmitted `[1, 0]`, reflected `[0, 2]`, as
    `PlanRunTests`' literal rows).
  - `RunPlan` then runs through the posting host and reads its rows from that cache. It ends with a committed report
    whose `ranked` holds QZ-Z's two candidates.
  - The cache's files are unchanged afterwards. The real solve and the cache's write path stay `PlanJobTests`' and
    `PlannerCacheTests`' subjects.
- **Red was shown by mutation.** The facts were written against the new symbols (red by compile). Two mutations then
  failed the intended facts, and the source was restored:
  - guarding `StudyPlan` with `guardKitWrite` failed fact 1 at its read-only `isRunning`;
  - dropping the study-name test from `planReportOpt` failed fact 3 at "another study shows no report".

## Testing state

Each run uses the gate's own command over the final tree; the supervisor's gate engine is the authority.

| Gate | Outcome | Measured | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors; only the exempt `NU1902`s (and the vendored MathNet's pre-existing `SYSLIB0051`s on the first, full recompile) | 86 s first, 25–42 s incremental | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0 (arc origin `f395597c` from `.arc-base-json`) | 242 s | 600 s |
| constructor-unit-tests (`all_discovered`) | `Passed!`, 1902 of 1902 = 1899 + the three census facts | 136 s | 180 s |
| ui-smoke (trait `ui-smoke`) | `Passed!`, 268 of 268 | 224 s | 600 s |
| ui-tests (selected `KitWindowTests`, `StudyViewTests`) | `Passed!`, 24 of 24 | 34 s | 360 s |
| 21 × `invariant-s46-*` | each grep finds nothing, also with `--untracked` | — | 300 s |

- **The new class alone:** 3 of 3, 2.5 s. In the suite's TRX the facts took 4.5 s, 0.6 s and 0.7 s, and their names
  match the three census ids.
- **The final tree.** After those runs, one missing space (`itemModels =`) and one comment's citation were corrected,
  with no semantic change. The build, the new class and elevation were then re-run over the final tree:
  - build: exit 0, 0 errors, only `NU1902`s, 18 s;
  - new class: `Passed!`, 3 of 3;
  - elevation: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 246 s.
- No run exceeded its effective deadline.

## Artifacts

- `specs/0050/.artifacts/0488-build.log` (production only), `0488-build-02.log` (missing opens, red),
  `0488-build-03.log`, `0488-build-04.log` (gate run), `0488-build-05.log` (final tree)
- `specs/0050/.artifacts/0488-plan-window-tests.log`, `0488-plan-window-tests-mutated.log` (the two mutations, red),
  `0488-plan-window-tests-02.log` (final tree)
- `specs/0050/.artifacts/0488-constructor-unit-tests.log`, `0488-ui-smoke.log`, `0488-ui-tests.log`
- `specs/0050/.artifacts/0488-elevation.log`, `0488-elevation-02.log` (final tree)

## Gotchas

- **Loading an in-memory kit into the window.** `KitOpening.openedOver` is private and `openKitAt` opens only a disk
  folder. A model test over an in-memory kit assembles `OpenedKit` from the kit's public reads, then calls `loadKit`.
- **A kit item over a test-local sample or material** records only when the catalogue store holds the entry. Put the
  entries through `BuiltinCatalogue.putSealedEntries` first; the window's `tryItemModelChoice` then resolves the sample
  too.
- **A completed plan in a model fact:** seed the kit's `.oc/planner/` cache under the window's own keys, and the plan
  through the posting host ends in milliseconds.
- **Field labels.** `PlanRequest`, `PlanReport` and `PlanSurfaceState` share `criterion` (the first two also share
  `study` and `material`). Annotate the type of a record literal where more than one is in scope.
