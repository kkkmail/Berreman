# Implementation log — 0152

## Progress

- Attempt 02, before repair: read the retry task, existing implementation handoff, worker protocols and applicable operator rulings. The supervisor's build reports FS0041 at InverseConstructorTests.fs lines 377, 390 and 444: xUnit cannot choose between scalar and sequence assertion overloads. Repair only those existing calls with explicit generic types, preserving all operands, assertions and identities.
- Read repository instructions, the worker protocols, slice 0152, naming conventions, and operator rulings 006 and 010.
- Before implementation: traced the collection messages, scene initializers and KitOpening contract. Manual/010 overrides the infallible initializer signatures and authorizes their downstream caller changes.

- Implemented kit ownership, file-backed measured data, loadKit attachment inspection, commit/attach writes and deletion with typed status. Removed collection persistence, messages and controls; retained the solver hand-off snapshot.
- Applied manual/010: initWith consumes a kit, scratch entry points return Result, and App/diagnostic factories propagate errors to their launcher status before showing a window.
- Added shared scene-success fixtures and converted initializer/window callers. Reworked the authorized inverse test identities around folder kits, preserving validation assertions and adding the two specified persistence facts.
- Before final caller pass: inspect the remaining fixture compositions and keep all published test identities and the gate roster unchanged.
- Finished the construction callers through the shared SceneTestFixture and Result-returning window factories. Diagnostic and product launchers show initialization errors through KitStatus; the diagnostic launcher scrolls to keep an error and all scene buttons reachable.
- Completed the folder-kit fixtures and authorized tests: reopen preserves experiments and attachments; unchanged, changed, missing and unreadable attachments retain their validation assertions; commit/edit/attach/remove use the experiment files; refused commits, edits, attachments and removals preserve live state; an already-missing file permits removal.
- Removed the five collection-proxy methods and the three authorized UI retirements, renamed the three specified kit-opening methods, and added exactly the two requested persistence facts. Existing callers retain their test identities and assertions.
- Read checkpoint 150 for inherited counts: 1,155 constructor cases and 215 smoke cases. The corrected constructor decrease is six cases; the UI decrease is one. No checkpoint, test census, oracle, gate, analyzer configuration or version file was changed.
- Attempt 02, after repair: supplied `Assert.NotEqual<string>`, `Assert.NotEqual<Experiments.Experiment list>` and `Assert.Equal<Map<Experiments.ExperimentId, ExperimentFileStatus>>` at the three reported calls. Their operands and surrounding behavior are unchanged. The retry changes only InverseConstructorTests.fs and the three prescribed worker handoff files; the prior kit implementation and all test identities/assertions are retained.

## Files modified

- Domain: retained CollectionName and ExperimentCollectionSnapshot; removed their obsolete persistence seam and updated its existing project comment.
- Ui/Controls: kit-backed constructor model, initializers, persistence messages, loadKit, KitStatus, AppContext and automation identifiers.
- App/TestWindows: fallible window factories, caller error reporting, and diagnostic scrolling, under manual/010's explicit downstream authority.
- Tests: retired ExperimentCollectionProxyTests and its project entry; added SceneTestFixture once and file-linked it into Ui.Tests; updated constructor/UI initialization callers and the specified inverse, hand-off, app-scope and composition fixtures.
- Worker plan and log created at the task's exact paths; state-of-world written alongside them.

## Testing state

This worker performed no build, test, analyzer, census or invariant execution: IMPLEMENT invariant 6 assigns these to the arc-runner after worker exit. The attempt-02 task reports the supervisor's previous build failure with three FS0041 overload-resolution errors in InverseConstructorTests.fs. This retry repairs those calls; no subsequent build result is available or claimed. The same supplied build output also reports two SYSLIB0051 warnings in vendored MathNetNumerics/Exceptions.cs, which this retry does not modify or claim to clear.

The unchanged 0152.gates roster awaits supervisor execution: build, elevation, constructor-unit-tests, ui-smoke, selected ui-tests and eleven s17 invariants. Source inspection and editing only; no measured post-change count is claimed. Empty gate results and commit_ready false mean pending supervisor verification, not an implementation blocker.

## Artifacts

- Task: specs/0050/.artifacts/0152-attempt-01-task.txt.
- Retry task and supplied build diagnostics: specs/0050/.artifacts/0152-attempt-02-task.txt.

## Gotchas

- Manual/006: five retired constructor methods represent six executed cases; UI decrease remains one.
- Manual/010: scratchKit returns Result; construction failures must reach App and diagnostic composition, never be discarded or hidden behind an infallible product facade.
- Existing supervisor change to .manifest.state.json is outside this worker's edits.
- The private concrete window constructors now accept successful scene models; tryCreate is the public fallible composition surface. Tests unwrap only through the shared success fixture, which fails with the typed error's description.
- recordExperiment wraps kit errors in KitLibraryKitErr, so the read-only assertion preserves both the user-facing prefix and the underlying KitReadOnlyErr text.
- Reopen tests release the previous kit lock before openKitAt, then close the reopened context. Test folders and measured files live below test output, never Documents or data/.
- Edited text was written with LF line endings. No Git mutation or ASG/STL write was performed.
- xUnit's scalar and sequence generic assertion overloads require explicit type arguments here: strings, F# lists and maps can otherwise satisfy both candidates. Keep the existing assertions and disambiguate the calls.
