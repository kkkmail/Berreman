# Slice 0427 implementation log

## Committed recovery, 2026-11-15

Before source review: read the task, worker/project protocols, AGENTS.md,
0427, R0001/R0002, the post-mortem and its review, and the applicable arc-origin
and deadline rulings. HEAD is the operator's repair commit `3fc40c54`, following
implementation commit `7f4efee8`. The earlier authority block below is historical:
R0001 and R0002 grant the repairs, which are already committed. Inspect their
source and saved evidence before replacing the stale state-of-world handoff.

Current planned edits are the three canonical handoff files only. No gate
execution, Git mutation, checkpoint publication or supervisor-state edit is
part of this IMPLEMENT worker turn. The initial dirty manifest state and
supervisor-lock diagnostic belong to the running supervisor and are preserved.

After source review: compared the committed code with the immutable
S121-AT1-STUDY and S113-ENGINE-CHOICE obligations, as amended by R0001/R0002.
No remaining implementation defect was identified and no source edit is needed.

- `KitAcceptanceTests.fs` creates the 53-experiment QZ-LR study through the
  window's messages with the six prescribed gain settings. It uses the posting
  host, queues progress/completion, waits at most sixty seconds on completion,
  and replays the messages through the real update. The shared clock produces
  the two required fit-run names; publication occurs between stages and the
  first run's stored text remains unchanged.
- The declared fact reopens the kit through a second KitProxy, checks the
  study, ordered runs, engines, fixed calibration, all item revisions and the
  five values with their sigmas/provenance. Reference helpers compare five
  instrument angles, 48 row-major matrix elements and the sole product-check
  ratio using MuellerReference.dataMatrixTol. The earlier two facts retain
  their original assertions at their pre-publication checkpoints.
- R0001: `engineWith` requires lone-run coverage of each unknown-bearing item
  in a series, rejects repeated items and links joining its constituents, and
  retains the other linear-shape guards. Its three authorized existing facts
  now cover resolved/unresolved series, linked products, double passes and
  automatic ObjectStage/JointStage dispatch with anchored/interpolated gains.
  The rank-deficiency refusal assertions remain present.
- R0002: `runElementAs` applies analyzer roles to compounds using
  F * (A1 * ... * An) * F, followed by the item's rotation. `compoundPartsAt`
  reuses the component evaluation extracted from the previous forward law.
  StudyForward and chartForRun both bind through this shared member; the
  remaining slot roles keep the existing binding behavior. No stored shape
  changed and the repair added no test identity.

Before final handoff: replace the obsolete blocked state-of-world with the
committed recovery status, retain the earlier diagnosis below as history,
and distinguish the operator's evidence from supervisor verification still
to run. Current worker changes are limited to `0427-impl-plan.md`, this log
and `0427-state-of-the-world.md`.

### Recovery testing state

No build, test, analyzer or invariant command was executed by this worker,
per implement_worker.system-md Invariant 6. `commit_ready: false` means the
fresh supervisor cycle is pending; implementation is complete, and the old
scope blocker was resolved by the two rulings and commit `3fc40c54`.

Read the operator's existing `.artifacts/0427-operator-repair/gate-results.json`
and build/elevation logs. That separate run records all 26 frozen gates plus
the supplemental constructor gate as passed: build 3.8 s, elevation 221.6 s,
ui-smoke 264/264 in 235.0 s, selected ui-tests 18/18 in 28.5 s, kit-acceptance
3/3 in 34.9 s, each of the 21 invariants in 0.2 s, and constructor-unit-tests
1761/1761 in 157.6 s. Zero failures/skips and no effective deadline overrun
are recorded. Elevation used origin f395597c071910aaa215518626563f6c784bfb62,
all 13 projects, ELEV004 excluded and zero new findings. The saved build log
reports only the exempt NU1902 advisories. These are historical operator
measurements, not results produced or accepted by this worker.

Normal review and the unchanged 26-gate roster must run after worker exit;
only the supervisor can publish measured checkpoint 426 on green. Preserve
profile 1 and the recovery exception forbidding --accept-current-slice.
No nested supervisor, Git mutation, metadata change or new runtime artifact
was produced. Existing evidence remains under the supplied artifacts folder.

After handoff: all three canonical outputs now describe the committed recovery
and pending supervisor cycle. Read-only final diff inspection shows only those
three worker edits alongside the two pre-existing supervisor-owned files;
no product or test source is dirty. All three handoffs contain LF only, and
diff counts are identical with and without ignoring CR at end of line.

## Earlier attempts (historical)

## Progress

- Attempt 03, before diagnosis: read the supplied kit-acceptance failure and
  confirm the authorized effects permit only KitAcceptanceTests.fs. Trace
  stage-two selection without weakening the preserved acceptance assertions.
- Attempt 03, after diagnosis: QZ and LR retain sixteen unknown coefficients
  each; their Science runs include QZ then LR. StudyEngine.engineWith requires
  at most one unknown-bearing matrix on EVERY fitted path, so these series
  runs select NonlinearStudyEngine. Its anchored-gain refusal is exactly the
  supervisor's measured error. No test-file-only repair preserves the required
  semantic-message flow, unknowns, gain rules and three matrix groups.
- Attempt 03, before handoff: record the scope conflict, source evidence and
  missing operator authority in the three canonical outputs. Preserve the
  existing test implementation and all supervisor-owned metadata.
- Attempt 03, after handoff: updated the plan, this log and the required
  state-of-world with the source-level diagnosis, unchanged inherited counts,
  supervisor measurements and explicit blocked status. No source edit or
  runtime check was performed in this attempt.
- Attempt 02: read the retry task and worker/project instructions. The supervisor
  build failed in 80.58 seconds (600-second allowance), with FS0058/FS3524/FS0010/
  FS0604 at the nested `kitShell` update in `runAcceptanceFit`.
- Before repair: move the outer record's `kitShell` field to its own line so
  its nested record is indented beyond the field's offside context. Preserve
  the scoped host, message flow, all assertions and the authorized fact identity.
- After repair: the nested update now matches the existing posting-host setup
  in StudyWindowModelTests. Reviewed the added callback flow, fit-run fields,
  calibrated-parameter fields, angle accessors and matrix accessor against their
  declarations. No further source change was identified; compilation and runtime
  results await the supervisor. Updated all three handoffs for this retry.
- Read AGENTS.md, the task, both worker prompts, the project prompt, slice 0427,
  its gate snapshot, naming conventions and applicable standing rulings.
- Inspected the existing AT-1 setup/experiment builder, shared reference values
  and StudyWindowModelTests' asynchronous fit-host pattern.
- Before implementation: extend the builder with staged-study messages and a
  controlled clock, preserving both earlier facts at their original checkpoints.
- Implemented the six gain settings as fixture data; derived the existing AIR
  calibration-name list from those settings without changing its contents/order.
- Implemented the staged continuation through CreateStudy, SetFitMode,
  SetSessionGainModel, RunFit and PublishCalibration. The existing clock advances
  to the two required timestamps. Each fit records concurrent callbacks, waits
  on a completion event for at most sixty seconds, and replays messages in order.
  The scoped host is removed before its completion event is disposed.
- Preserved the setup and data facts' assertions and original checkpoints.
- Before reference coverage: inspect fit-run entries/sigmas, the publication
  contract, the angle accessor and row-major matrix accessor to compare the saved
  artifacts through their existing APIs.
- Added the one authorized staged-study fact and the two reusable private
  reference helpers. The fact reads through a second KitProxy and asserts study
  order/mode/gains, session offers/shared dark runs, both fit-run stamps/stages,
  five item-owned entries and sigmas, publication provenance/revisions and the
  exact calibration held by stage two. It compares five angles, 48 matrix
  elements and the product-check RMSE ratio at MuellerReference.dataMatrixTol.
- Source review traced CreateKit's clock through loadKit into the fit environment,
  confirmed stage-two parameter snapshots retain CalibratedParameter, and kept
  the stage-one stored text unchanged across publication and stage two.
- Before handoff: record the complete implementation, inherited baseline entries
  and supervisor-owned verification without claiming a measured result.
- Wrote the state-of-world handoff with every required section and the prior
  handoff's unchanged baseline YAML. Source diff statistics matched with and
  without ignoring carriage returns; no line-ending churn was introduced.
  Final scope is the authorized test file and three handoffs, plus the
  preserved supervisor-owned manifest-state edit.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs`:
  gain fixture data, staged builder, callback/clock harness, reference helpers,
  one new fact and checkpoint call-site adjustments.
- The three required slice-local implementation plan, log and state-of-world files.

## Testing state

Attempt 03 is BLOCKED by the production dispatch outside the declared effects,
not by pending checks alone. No source was changed or test/gate run this attempt.
The supplied retry reports review green and build gates passed. The existing
supervisor log records kit-acceptance failing after selective retries in 86.3 s;
the supplied final retry summary is 1 failed, 0 passed, 16.711 s, against its
3,600-second deadline. Neither duration exceeds the allowance. The failure is
at the ObjectStage fit, before its saved-reference assertions can execute.
These are inherited supervisor measurements, not worker results.

The following records retain the earlier attempts' history:

No checks or gates run. Invariant 6 of implement_worker.system-md reserves
execution and measured verdicts to the supervisor. The authorized test effect
is exactly one new KitAcceptanceTests fact; existing assertions remain.

The attempt-02 task supplies the supervisor's previous `build` failure: exit
code 1, five syntax/indentation diagnostics in the nested record update,
elapsed 80.58 seconds against a 600-second deadline. That run did not exceed
its deadline. This retry repairs that source; no passing build or test result
is claimed. The remaining gates were not reached by that short-circuiting run.

The exact added identity is
`OpticalConstructor.Ui.Tests.KitAcceptanceTests.AT-1's staged study calibrates the instruments on the AIR runs, publishes them and reconstructs QZ, LR and QZ then LR as the reference does`.
The class's declared count grows from two facts to three, not a worker-measured
pass count. Build, elevation, ui-smoke, selected StudyViewTests/DataStepTests,
selected KitAcceptanceTests and all 21 s41 invariants await the supervisor.
No worker development duration or deadline overrun exists to report.

## Attempt 03 authority conflict

The immutable oracle obligation S121-AT1-STUDY (step 0427) requires the real
window RunFit to produce LinearStudyEngine stage two with [QZ], [LR] and
[QZ; LR], the six reference gains and both append-only runs. The slice's only
authorized source edit is KitAcceptanceTests.fs.

Evidence in the current production source:

- KitAcceptanceTests.fs:304-305 makes every QZ/LR coefficient unknown;
  buildAcceptanceStudy:623-648 sends the specified messages and publishes
  calibration before ObjectStage. Existing checks confirm revision two for
  the three instruments before the reported failure.
- StudyEngine.fs:338-356, StagedStudy.engineWith.blackBoxShape, counts unknown
  bearers per path and requires bearing.Length <= 1, including series paths.
  QZ and LR on the same path violate that condition even when each also has
  its own independent observations.
- StudyEngine.fs:1844-1848 dispatches this shape to fitNonlinear.
  Its scaledProblemWith at 425-438 returns SessionGainNeedsLinearEngineErr for
  the required LP-LP anchors, reproducing the supplied diagnostic in source.
- StudyEngineTests.fs:1887-1909 covers the three matrix groups by calling
  reconstructLinear directly. It never asserts automatic engine selection or
  runs that fixture through StudyFitParam.runFit. The tests at 769-795 protect
  nonlinear classification for unresolved series, coupled matrices and a
  repeated unknown matrix; those assertions must remain.
- Step 0386 / S113-ENGINE-CHOICE requires genuinely coupled unknown products
  to remain nonlinear. Step 0390 distinguishes independently reconstructed
  combined matrices as diagnostics. Step 0427 / S121-AT1-STUDY requires this
  diagnostic path to work through automatic window dispatch. The current
  classifier has no such distinction.

Missing authority: permit edits to
Berreman/OpticalConstructor/OpticalConstructor.Optimization/StudyEngine.fs and
extensions of the existing facts in
Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs.
The production repair must route independently observed constituent matrices
and their separate series diagnostic through the linear reconstruction, while
preserving nonlinear handling of unresolved/coupled products. Extend existing
classification, series-reconstruction and runFit coverage under operator-approved
test effects and verification; no new test identity is needed for the diagnosis.
Owners are OpticalConstructor.Optimization and OpticalConstructor.Tests,
introduced by steps 0386, 0390 and 0391. This worker does not amend the frozen
effects, test census, acceptance oracle or gate roster.

Changing gains to free, fixing one object's coefficients, disabling series
runs, injecting a synthetic fit result or calling reconstructLinear from the
acceptance fixture would violate S121-AT1-STUDY. None was done.
commit_ready remains false until the authority conflict and measured failure
are resolved; the slice's numerical acceptance is not claimed complete.

## Architecture

Reuse the real window update, StudyFitJobHost.posting, KitProxy and
MuellerReference. No new production seam or durable format is introduced.
The finish continuation accepts the existing clock's setter, leaving mutation
at the fixture IO boundary. Numeric assertions delegate to xUnit's tolerance
overload with named quantities and full-precision expected/actual diagnostics;
they introduce no independent epsilon or reference-file parser.

## Gotchas

- Publishing changes three item revisions and their parameter states. Keep the
  step-424 experiment fact on its experiment-only checkpoint so its original
  revision-one and unknown-parameter assertions continue to apply.
- The mutable clock is already created by the kit builder; the new continuation
  must advance that same provider, not replace the production fit machinery.
- The pre-existing `.manifest.state.json` edit belongs to the supervisor.
- No scope deviation: the existing setup/data assertions require pre-publication
  state, so their continuations stop at those checkpoints while
  `buildOpticsMuellerKit` now always completes both study stages.
- No product, raw-data, notebook, project, package, gate, census, oracle,
  checkpoint, version or external-repository changes were made.

## Artifacts

No runtime artifacts produced. Persistent runtime output, if any, belongs only
under `specs/0050/.artifacts`.
