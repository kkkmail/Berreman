# Where we are

Slice 0427 implements S121-AT1-STUDY for spec 0050 S12.1. The staged AT-1
acceptance implementation is committed as 7f4efee8; the R0001 series-dispatch
and R0002 analyzer-traversal repairs are committed as 3fc40c54. This recovery
reviewed that code against the slice, immutable obligations and binding
rulings, and refreshed only the three canonical worker handoff files.
The previous authority block is resolved. No remaining implementation gap
was identified. The supervisor must complete normal review and its unchanged
26-gate cycle before publishing measured checkpoint 426.

# What's working

- Built the 53-experiment staged study through window messages with all six reference gain rules.
- Ran calibration, publication and object reconstruction through the real fit host with deterministic timestamps.
- Added the authorized saved-kit acceptance fact covering provenance, revisions, instruments, matrices and product residuals.
- Preserved both earlier acceptance facts and their pre-publication assertions.
- Preserved linear dispatch for independently reconstructed series and nonlinear dispatch for unresolved, repeated or linked products.
- Shared the analyzer compound traversal between the study forward model and table experiment runs.

These describe delivered source. Runtime evidence below belongs to the
operator's earlier verification, not this worker turn.

# Tests

No build, test, analyzer or invariant command was executed by this worker.
Invariant 6 of implement_worker.system-md assigns gate execution and verdicts
to the supervisor after worker exit. commit_ready=false reflects pending
supervisor verification; the requested implementation is complete. No worker
development duration or deadline overrun exists to report.

The existing operator evidence at
[gate-results.json](../.artifacts/0427-operator-repair/gate-results.json)
records this separate run over the repaired tree:

| Gate | Operator's recorded outcome | Duration | Effective deadline |
| --- | --- | --- | --- |
| build | Passed | 3.8 s | 600 s |
| elevation | Passed | 221.6 s | 600 s |
| ui-smoke, whole suite | 264/264 | 235.0 s | 600 s |
| ui-tests, selected classes | 18/18 | 28.5 s | 360 s |
| kit-acceptance, selected class | 3/3 | 34.9 s | 3600 s |
| All 21 invariant-s41-* gates | Passed | 0.2 s each | 300 s each |
| Supplemental constructor-unit-tests, whole suite | 1761/1761 | 157.6 s | 180 s |

The record reports zero failures/skips and no deadline overrun. The saved
elevation log covers all 13 product projects from immutable origin
f395597c071910aaa215518626563f6c784bfb62, with zero new findings and ELEV004
excluded. The saved build log reports only exempt NU1902 package advisories.

The supervisor's fresh roster remains exactly the 26 gates in 0427.gates:
build, elevation, whole ui-smoke, ui-tests selected to StudyViewTests and
DataStepTests, kit-acceptance selected to KitAcceptanceTests, and all 21 s41
invariants. R0001/R0002's supplemental pre-commit constructor verification
is recorded above; it does not rewrite that roster. Normal review and green
supervisor measurement must precede checkpoint 426. Preserve executability
profile 1 and do not use --accept-current-slice or launch a second supervisor.

Exactly one acceptance identity was added by the committed implementation:

- OpticalConstructor.Ui.Tests.KitAcceptanceTests.AT-1's staged study calibrates the instruments on the AIR runs, publishes them and reconstructs QZ, LR and QZ then LR as the reference does

It reads the saved study and two runs through a second KitProxy; verifies
53 experiment names in kit order, StagedFit, six offered session gains and
shared dark runs; checks both LinearStudyEngine runs and CalibrationHeldFixed;
checks the five calibration entries, sigmas, provenance and their stage-two
snapshots; and verifies revision 2 for LP2/CPL1/CPL2 with every other item at
revision 1. Reference assertions compare five instrument angles, 48 row-major
matrix elements in [QZ], [LR], [QZ; LR], and the one product-check RMSE ratio.
The existing two facts keep their assertions.

The repair extends only the three existing StudyEngineTests facts named in
R0001. Their previous assertions, including nonlinear refusals and rank
deficiency, remain. No identity was added, renamed or removed by the repair
or this recovery.

The YAML below is inherited unchanged from the prior handoff and 0426. It
does not promote the operator's measurements to new baselines. The supervisor
owns measured counts and checkpoint publication.

```yaml
gates:
  constructor_unit_tests: 1761
  ui_smoke_tests: 264
  ui_tests: 9
  kit_acceptance_tests: 0
```

# Architecture

AT-1 uses CreateStudy, SetFitMode and SetSessionGainModel, followed by
RunFit CalibrationStage, PublishCalibration and RunFit ObjectStage.
StudyFitJobHost.posting queues progress and completion; a ManualResetEventSlim
bounds the wait to sixty seconds, and the window update replays the recorded
messages. The existing ClockProvider yields the required 12:01 and 12:02 UTC
fit-run names. The calibration run's stored text is checked unchanged after
publication and the object fit. No direct solver call or synthetic completion
replaces the production flow.

R0001 amends the dispatch restriction described by the earlier post-mortem.
StagedStudy.engineWith allows a path bearing several unknown matrices only
when each item also has lone runs in the same fit, no item repeats on the
path, and no study link joins constituents. The existing linear engine solves
constituent coefficients from lone runs and the combined matrix separately
as a diagnostic. Unresolved series, double passes and linked products remain
nonlinear; deficient rank still reaches the solver's typed refusal.

R0002 makes an analyzer compound the film seen from behind:
F * (A1 * ... * An) * F, with F = diag(1, 1, -1, -1), followed by the item's
rotation. BoundOpticalElement reuses its extracted component evaluator.
SetupSlot.runElementAs applies the role, and both StudyForward and chartForRun
use it with SetupSlot.inLightOrder. Other slot roles keep their binding law.
This agrees with stage one's circular analyzer without changing stored items,
models, serialization or a published contract.

# Deferred

No requested implementation is deferred. The supervisor's normal review and
fresh frozen-roster verification remain required before acceptance.

# Gotchas

- The older blocked diagnosis in the implementation log and post-mortem is
  historical. R0001/R0002 explicitly authorize the committed repairs.
- The earlier two kit facts stop at their setup/data checkpoints so publication
  does not invalidate their original revision-one and unknown-value assertions.
- CPL1's fitted fast-axis offset is compared with the reference angle minus
  45 degrees. All tolerances come from MuellerReference.dataMatrixTol.
- R0001 and R0002 preserve their rules through steps 0428-0494; additional repair
  authority is confined to 0427. They do not amend gates, deadlines, counts,
  census, formats, oracle or budgets.
- Implementation and repair are already committed. Do not amend, reset or
  re-stage them. This worker changed only the plan, log and state-of-world.
- The initial dirty .manifest.state.json and .supervisor-lock.diag belong to
  the supervisor and were preserved. Counters, checkpoints, descriptors,
  published contracts and arc origin were not edited.
- No Git mutation, version bump, external-repository write, raw-data/notebook
  edit or new runtime artifact occurred. Existing operator evidence remains
  under specs/0050/.artifacts/0427-operator-repair/.

# Changelog

- 2026-11-15: Implemented AT-1's staged study and saved-reference acceptance coverage in attempt 01.
- 2026-11-15 (attempt 02): Repaired the nested fit-host record's strict indentation.
- 2026-11-15 (attempt 03): Diagnosed the ObjectStage dispatch failure and recorded the missing repair authority.
- 2026-11-15 (operator recovery): Reviewed committed implementation and R0001/R0002 repairs, refreshed the three handoffs, and returned normal verification/checkpoint publication to the supervisor.
