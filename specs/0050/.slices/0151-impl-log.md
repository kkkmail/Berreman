# Implementation log

## Progress

- Read AGENTS.md, both worker prompts, the project prompt, slice 0151, its gate
  snapshot, naming conventions, manual 006 and the S43-NO-SCENE-SEAM oracle.
- Read the affected source, tests and project comments. AppContext is the only
  product consumer of SceneProxy. OpenedKit.scenes is a separate scene-name
  list and stays. ExperimentDataProxy.fs already contains no obsolete reference.
- Initial tracked modification: specs/0050/.manifest.state.json, owned by the
  supervisor and left untouched. No Git mutation or version bump is authorized.
- Before implementation: delete the obsolete proxy and its root composition;
  move all four retained methods verbatim, including both blank-name rows;
  delete only the two authorized proxy facts; preserve the UI fact's identity
  and its remaining assertions. Trim only the specified stale documentation.
- The first combined patch was rejected before applying because one project
  comment differed from the captured context. Read the exact comment and
  reapplied the patch; this required no scope or implementation change.
- After implementation: removed SceneProxy, its in-memory factory, the
  AppContext field and its composition. Preserved all scene data and file
  members. Moved the four test methods unchanged, retaining both theory rows;
  deleted SceneProxyTests.fs and its project entry. The destination already
  opens OpticalConstructor.Domain.Scene, which supplies SnapMode and its cases.
- Removed only the scene-store match and its comments from the UI fact;
  the experiment-collection assertion, settings assertion, test identity and
  all smoke facts remain. Shortened the specified docs and project comments.
- Before handoff: review the product/test diff and line-ending footprint, then
  write the required state-of-world with inherited counts and pending gates.
- After source review: the diff is limited to the eight planned product/test
  files. The migrated methods preserve their original names, bodies and theory
  rows. Existing kit-file assertions and UI smoke facts remain unchanged.
  Product numstats match with and without ignoring CR at line ends; removed
  the now-redundant final blank line in Scene.fs.
- Completed the supplied state-of-world with every required heading and
  checkpoint 149's inherited counts. No implementation requirement remains;
  execution and acceptance are pending with the supervisor.

## Files modified

- Created the supplied 0151-impl-plan.md and this log before product edits.
- Domain/Scene.fs and Domain/OpticalConstructor.Domain.fsproj.
- Ui/AppContext.fs.
- Tests/SceneProxyTests.fs (deleted), Tests/SceneRoundTripTests.fs and
  Tests/OpticalConstructor.Tests.fsproj.
- Ui.Tests/WireUiFinalCompositionTests.fs and
  Ui.Tests/OpticalConstructor.Ui.Tests.fsproj.
- Created the supplied 0151-state-of-the-world.md and completed both other
  worker documents at their authoritative paths.

## Testing state

No checks or gates run: IMPLEMENT Invariant 6 assigns execution to the
supervisor after worker exit. The gate roster is build, elevation,
constructor-unit-tests, ui-smoke, selected ui-tests and eleven s17 invariants.
Checkpoint 149 records 1157 constructor cases and 215 smoke cases. The declared
retirement removes two facts; the four migrated methods retain five cases.
Expected constructor total is 1155, not a new measurement or baseline.
The structured handoff uses empty gates_run and gate_results and commit_ready
false because this worker cannot certify gate acceptance. No implementation
blocker remains. Source/diff review is not executable verification.

## Artifacts

- Supplied task: specs/0050/.artifacts/0151-attempt-01-task.txt.
- No execution artifacts produced. Persistent run artifacts belong only under
  the supplied specs/0050/.artifacts directory.

## Gotchas

- Manual 006 requires preservation of both blank-name theory rows.
- The UI fact keeps its historical scene-seam name because the declared effect
  changes its expectation, not its identity.
- The role-specific no-checks instruction supersedes legacy local-gate guidance.
- Historical scene-proxy comments in ExperimentCollectionStore.fs and
  TableAndElementRotationView.fs are outside 0151's authorized effects;
  0152's broader retirement owns that scope. No neighbouring implementation
  was changed, and no 0151 requirement depends on those comments.
