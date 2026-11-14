# Implementation log — 0414

## Progress

- Attempt 03, before repair: read the task, worker protocols, slice and existing source/handoffs. The supervisor reports the same smoke fact failing at the new immediate ParameterEditPanel assertion (StudyViewTests.fs:229): 259 succeeded / 1 failed / 260 total, MTP duration 234.015 seconds against 600 seconds. Attempt 02's UpdateLayout preparation was insufficient. Trace the control geometry and event delivery before editing; preserve all existing assertions, identities and supervisor metadata. No worker gate execution.
- Attempt 03 source diagnosis, before edit: the row sends EditStudyParameter with the stored Phone id/name, and its draft is included by the existing editor projection. The Unknowns scroller leaves AllowAutoHide at Avalonia's true default. In the package-pinned Fluent template that makes the content presenter span the scrollbar row/column; BringDescendantIntoView aligns a lower offscreen control with the viewport bottom. Thus completing layout still permits the horizontal scrollbar to intercept the row-centre press. Reserve scrollbar space in StudyUnknownsView and assert the rendered hit target before each study smoke press, preserving the shared real-pointer helper. This is a source-based diagnosis requiring supervisor runtime verification.
- Attempt 03 repair completed: StudyUnknownsView sets AllowAutoHide=false on its own ScrollViewer, keeping both bars Auto but reserving their layout space when present. StudyViewTests.pressStudyBox advances the headless render after layout and asserts that the centre resolves to the identified control or its descendant, then delegates the click to SlotBarTests.pressBox. All existing editor, persistence, start-value, bounds, file and grid assertions remain. No windows were enlarged and no semantic model command replaced the pointer input.

- Attempt 02, before repair: read the retry task and current implementation. The supervisor reports ui-smoke 259 succeeded / 1 failed / 260 total in 230.025 seconds (600-second deadline), with ParameterEditUnknownChoice absent after the Phone.darkOffset row press. The existing implementation, tests and supervisor-owned metadata are preserved. Trace the existing row press through scrolling, EditStudyParameter and the shared editor before changing the authorized files; no gate execution in this worker.

- Read the task, worker protocols, project prompt, slice, repository guidance, naming rules and applicable standing rulings. The only pre-existing dirty file is supervisor-owned `specs/0050/.manifest.state.json`; leave it alone.
- Before implementation: inspect existing flow routing, study state/commands, shared parameter editor, grid cells and UI fixture conventions.
- Implemented the fixed/derived automation IDs, Inverse-only navigation, caption resolution and kit-window routing. Added StudyUnknownsView using the existing study commands, gridCells and parameterEditorView. Create-study actions are keyed by their current draft; study offers and grids are keyed by study identity.
- Before fixture/test work: traced canonical recordItem/recordSetup/recordExperiment APIs and existing headless mounting/input helpers. Material references are a Set and are enumerated in stable order for offers.
- Added the reopened disk-kit fixture with shipped models, Science scans and real copied data attachments, plus one shared material-opening fixture helper. Added exactly the authorized four StudyViewTests and one ScrollExtentTests fact; extended the five authorized existing facts. UI smoke coverage types into controls and reads canonical kit files after Create/Set; the structure test also exercises scope persistence, and the create test exercises Add experiment.
- Source review caught and corrected the material-reference Set/list conversion and the fixture's Propagation import. Fixture data uses KitProxy.copyInFile so the attached files actually exist under data/ before the kit is reopened.
- Final source review: kept the shared editor inside a wrapper keyed by both study and parameter owner, so equal parameter names on CPL1/CPL2 cannot retain another owner's Set handler. Name-to-caption conversions live on their source types. Preserved all existing assertions and the supervisor-owned dirty metadata.
- Attempt 02 diagnosis: EditStudyParameter resolves Phone.darkOffset from kitItems and the view includes its draft. The pointer helper instead has a layout timing gap: BringIntoView invalidates scrolling layout, but it reads the centre after RunJobs alone; HeadlessWindowExtensions.MouseDown then flushes render work before delivering that saved position. Source inspection of the pinned Avalonia commit confirms layout is queued on render. This is a source-based diagnosis of the supplied failure, not a worker reproduction.
- Attempt 02 repair completed: added a private pressStudyBox fixture helper in StudyViewTests that completes initial layout, brings the identified control into view, drains dispatcher work and completes scroll layout before delegating the real press to SlotBarTests.pressBox. Both existing smoke facts use it. The parameter-edit fact additionally asserts the shared editor is present and its Known choice is shown immediately after the row press. All previous persistence, bounds, start-value, file and grid assertions remain unchanged; no test identity, trait, window size or product behavior changed.
- Attempt 02 handoff: update these exact output files with the diagnosis and supplied supervisor evidence. Verification remains the supervisor's responsibility; no build, test, analyzer or invariant command was executed by this worker.

## Files modified

- Controls/UiIds.fs; Ui/ConstructorUiIdOfDomain.fs, TableAndElementRotationView.fs, StudyUnknownsView.fs, KitWindowView.fs and OpticalConstructor.Ui.fsproj.
- Ui.Tests/TestAppScope.fs, StudyViewTests.fs, KitWindowTests.fs, ScrollExtentTests.fs and OpticalConstructor.Ui.Tests.fsproj; Tests/KitWindowModelTests.fs.
- Planning and handoff files at the task's exact paths.

## Testing state

No gates or checks run. IMPLEMENT Invariant 6 assigns execution to the supervisor after worker exit. The authorized additions are two ui-smoke and three ui-tests identities; existing tests retain their identities.

Attempt 02 received the supervisor's earlier results: build and review passed; the supervisor log also records elevation passing in 235.6 seconds and constructor-unit-tests passing in 158.6 seconds. ui-smoke failed with exit 2: 259 succeeded, 1 failed, 260 total, MTP duration 230.025 seconds against a 600-second deadline. Its failure is the missing ParameterEditUnknownChoice after the row press. Those results predate this repair; they do not validate it. Selected ui-tests and the remaining invariants were not reached. No supplied measurement exceeds its gate deadline.

All requested implementation and the fixture repair are present. The full resolved roster awaits supervisor execution over the repaired tree. No new result or baseline is claimed. Historical checkpoint 412 supplies constructor 1,752 and smoke 258; the prior handoff supplies the historical ui_tests floor of 16 for KitWindowTests, not a measurement of this slice's expanded selection.

Attempt 03 received the supervisor's result over attempt 02: review/build passed, then ui-smoke failed with exit 2 at the immediate ParameterEditPanel assertion. MTP reports 259 succeeded, 1 failed, 260 total in 234.015 seconds (600-second deadline); the failing case took 41.932 seconds. These supplied results predate the scrollbar repair. The worker has not run or green-lit any gate. The repaired tree is ready for supervisor verification, with commit_ready=false until that verification; no supplied duration exceeds the gate allowance.

## Artifacts

Persistent artifacts, if needed, belong only in `specs/0050/.artifacts`.
No runtime artifacts were generated because this worker did not execute gates.

Diagnostic sources: the supplied `0414-attempt-02-task.txt`, the existing supervisor `.manifest-arc-runner.log`, and the package-pinned Avalonia commit `2d79ba55fae1b1e71c69f93a668080c9029e6a31`: [HeadlessWindowExtensions.RunJobsOnImpl](https://github.com/AvaloniaUI/Avalonia/blob/2d79ba55fae1b1e71c69f93a668080c9029e6a31/src/Headless/Avalonia.Headless/HeadlessWindowExtensions.cs#L153) and [LayoutManager.QueueLayoutPass](https://github.com/AvaloniaUI/Avalonia/blob/2d79ba55fae1b1e71c69f93a668080c9029e6a31/src/Avalonia.Base/Layout/LayoutManager.cs#L319).

Attempt 03 sources: `0414-attempt-03-task.txt`, the local Avalonia 12.1.4 nuspec confirming that same commit, [Fluent ScrollViewer template](https://github.com/AvaloniaUI/Avalonia/blob/2d79ba55fae1b1e71c69f93a668080c9029e6a31/src/Avalonia.Themes.Fluent/Controls/ScrollViewer.xaml#L67), [AllowAutoHide default](https://github.com/AvaloniaUI/Avalonia/blob/2d79ba55fae1b1e71c69f93a668080c9029e6a31/src/Avalonia.Controls/ScrollViewer.cs#L106), and [BringDescendantIntoView](https://github.com/AvaloniaUI/Avalonia/blob/2d79ba55fae1b1e71c69f93a668080c9029e6a31/src/Avalonia.Controls/Presenters/ScrollContentPresenter.cs#L213). No runtime evidence of the new repair is claimed.

## Gotchas

The worker-specific act-only protocol supersedes the shared protocol's legacy self-check path. Gate results and measured baselines remain supervisor-owned.

No scope deviation, durable-format change, version bump, git mutation or external-repository mutation was made. The material fixture helper is shared by the structure and scroll facts; all UI study fixtures use the one studyKitIn constructor. Tracked source numstats matched with and without CR ignoring during diff review; final authored files are normalized to LF.

Attempt 02 changes only StudyViewTests.fs and the three assigned worker documents. The shared SlotBarTests helper is outside the declared effects, so the study fixture prepares layout and calls it without copying or editing its pointer implementation. There are no sleeps, arbitrary coordinates, direct model-command replacements or enlarged test windows. Both supervisor-owned dirty metadata files, including `.expected-red-observed-json`, remain untouched.

Attempt 03 changes only StudyUnknownsView.fs, StudyViewTests.fs and the three assigned worker documents. It supersedes attempt 02's assumption that layout timing alone caused the missing editor. The scrollbar diagnosis is inferred from source and the repeated measured failure, not confirmed by a worker reproduction. The new hit assertion distinguishes occlusion from a missing editor on the supervisor's next run. Acceptance, test identities/traits, gates, count floors, versions and supervisor metadata remain unchanged.
