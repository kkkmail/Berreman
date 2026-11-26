# Step 0412 state of the world

## Where we are

Spec 0050 S11.5 now connects the constructor window's study-fit messages to the existing background fit host and state machine. The window guards fit starts for read-only kits and immediately exposes successfully saved runs in its history and selection. The requested implementation and three declared regression facts are complete; supervisor verification is pending.

## What's working

- Added StudyFitJobHost.inert and the shell's default studyFits host.
- Added idle studyFit state and the forward context over the window's existing kit, stores and model-version resolver.
- Routed fit start, progress, cancellation and completion through StudyFitMsg.updateWith, guarding starts with the existing read-only rule.
- Appended and selected completed runs only after successful persistence, retaining history and the typed error on storage refusal.
- Added the three authorized facts for lifecycle/read-only behavior, persistence/history and background empty-study refusal.

## Tests

No build, tests, analyzers or invariant commands were run by the worker. The task's implement_worker.system-md Invariant 6 assigns executable checks and gate verdicts to the supervisor after worker exit. Source review is not a passing gate result or verified commit readiness. No worker gate duration or deadline overrun is claimed.

Pending gates are build (600 s), elevation (600 s), the complete constructor-unit-tests suite (180 s), the complete ui-smoke suite (600 s), ui-tests selecting OpticalConstructor.Ui.Tests.KitWindowTests (360 s), and all twenty invariant-s40 gates (300 s each). The gate roster, selections, acceptance oracle, test census, count floors and deadlines are unchanged.

Exactly three facts were added in StudyWindowModelTests, preserving all existing identities and assertions:

- RunFit starts one fit through the shell's host, Cancel cancels it, and a read-only kit starts none.
- a fit's end writes its fit run into the kit and shows it in the history at once.
- a fit that cannot run tells why and writes nothing.

Coverage includes the inert host, idle initialization, forward-context dependencies and stored model resolution, single-start behavior, progress forwarding, cancellation without writes, read-only refusal, real fit-run persistence, immediate history/selection, duplicate-name refusal preserving prior state and bytes, and the real background empty-study error FitStageErr (NoStageRunErr _). Background callbacks are recorded and applied in order after a ManualResetEventSlim wait bounded to ten seconds.

Historical baselines below come from supervisor checkpoint 410 (1,745 constructor cases and 258 smoke cases) and the 0410 handoff's same selected KitWindowTests class (16 cases). The three additions imply an expected constructor total of 1,748; this is not a measured result. No checkpoint or baseline file was edited.

```yaml
gates:
  constructor_unit_tests: 1745
  ui_smoke_tests: 258
  ui_tests: 16
```

## Architecture

KitWindowShell carries StudyFitJobHost, while TableAndElementRotationModel carries StudyFitJobState. Existing shells inherit the inert default. studyForwardContext projects studyContext, library, materials, experimentData, tryItemModelChoice and SolverParameters.defaultValue without another resolver or optical-model implementation.

The StudyFit message arm creates StudyFitEnvironment from the shell host, current kit proxy, forward context and kit clock, then delegates to StudyFitMsg.updateWith. Only RunFit passes guardKitWrite. A successful completion updates history and fitRunOpt only when the returned job is Completed, committedResults names the same run and errorOpt is None. Storage refusal stays in the returned fit state. The existing job state machine owns persistence, cancellation and the single-job rule.

No serialized contract, stored format, schema, migration, additional setting, retry or runtime queue changed. Tests reuse withWindow, shipped catalogue versions and FitRunFileFixtures; the existing shared-database material-editor fixture remains intact.

## Deferred

No slice implementation is deferred. Executable verification belongs to the supervisor under the worker protocol. Composing the actual window dispatcher host remains step 0419's stated scope, as required by this slice's inert default.

## Gotchas

- specs/0050/.manifest.state.json was already modified on entry and was preserved.
- Only the three authorized product/test files and this slice's three worker documents were edited.
- The duplicate completion test starts from a previously committed run with the same name, ensuring retained committedResults cannot hide a failed write or append a duplicate history entry.
- The background test queues callbacks before signaling completion and processes them on the fact's thread; it does not dispatch model updates concurrently or use sleeps.
- Product/test diff numstats matched with and without CR ignoring. Worker-authored text uses LF.
- No version bump, notebook edit, external repository mutation, analyzer suppression, gate/census/checkpoint edit or git mutation was performed. No runtime artifacts were produced.
- commit_ready is false because executable gate evidence is pending, not because requested implementation was omitted.

## Changelog

- 2026-11-14: Connected study fitting to the window shell/model, guarded fit starts, published successfully saved runs immediately and added the three authorized regression facts; handed executable verification to the supervisor.
