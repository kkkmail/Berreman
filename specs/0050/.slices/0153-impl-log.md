# Implementation log — 0153

## Progress

- Read AGENTS.md, the task, both worker prompts, the project prompt, slice/gate contract, naming conventions and manual/010.
- Before implementation: traced scene snapshots, kit recording/opening and the workbench update/view seams. Existing storage owns canonical setup files and library snapshots.
- Before tests: use exactly the four declared identities. Construct opened-kit models through a reused Main initializer taking the supplied context, as manual/010 requires.
- After tests authored: added SceneSaveLoadTests and its project entry immediately after InverseConstructorTests. The four identities cover physical files/snapshots, restoration/reopen, ordinal unique names, blank/missing/read-only refusals and headless controls in both modes, including Enter, blur and button messages. No tests executed under the act-only protocol.
- Before product wiring: add the requested fields/messages/IDs, reuse supplied-kit Main initialization, and project the scene row before status/saved scenes/inverse attachments. Workbench buttons will expose their caption as the accessibility name.
- After product wiring: implemented SceneSetName/SceneSave/SceneLoad, initialized the two scene fields, populated savedScenes from OpenedKit, and added the scene row, status, saved-scene WrapPanel and three fixed/one derived IDs. Saves call recordScene before accepting a distinct ordinal-sorted name; loads call readScene and restoreScene. Refusals change only kitStatus.
- Reused the existing Main seed construction in initMainWithKit. initMainWith still constructs a scratch kit through Result and delegates; opened-kit tests call the same initializer with their supplied context. No seed records or serializers were copied.
- Source/diff inspection: the new test is directly after InverseConstructorTests, the four declared identities remain exact, snapshot tests assert the actual linear-polarizer preset, and the product diff's numstat matches --ignore-cr-at-eol. No CRLF churn was introduced. No build, test, analyzer or invariant command was run.
- Completed the worker state-of-the-world and handoff. All requested implementation work is present; gate execution remains with the supervisor.
- Attempt 02, before repair: read the retry task and governing sources, then traced FS0039 at TableAndElementRotationView.fs:1837. Domain/Scene.fs already defines SceneName.tryCreate; the module-qualified expression resolves through the same-named DU case. Existing SceneRoundTripTests open Domain.Scene and call SceneName.tryCreate directly; TableAndElementRotationView already documents this resolution issue for WorkbenchSettings. Apply that existing pattern without changing validation or tests.
- Attempt 02, after repair: opened OpticalConstructor.Domain.Scene and changed the save arm to SceneName.tryCreate. No validation logic, message, field, ID, initializer, persistence behavior or test assertion changed. Updated the plan, this log and state-of-the-world for the retry; no gates executed.
- Attempt 03, before repair: read the retry task, worker/project prompts, slice/gate contract, manual/010, naming conventions and existing implementation. The supplied build reports FS0039 at SceneSaveLoadTests.fs:49 for Seq.ofType and :82 for RemoveSelected. The product defines RemoveSelectedElement, already used by MainSceneMsgTests and ElementMenuTests. Replace only the invalid sequence helper and message reference; preserve all test identities and assertions.
- Attempt 03, after repair: findSceneControl now uses Seq.choose with a Control type pattern before the unchanged automation-ID lookup. The persistence fact dispatches RemoveSelectedElement before the unchanged removal/restoration assertions. No test identity, assertion, fixture, product implementation or gate contract changed. Updated the plan, this log and state-of-the-world; no gates executed.
- Attempt 04, before repair: read the retry task, worker/project prompts, slice/gate contract, manual/010 and code-judge cycle 1. The smoke fact raises plain RoutedEventArgs for LostFocus, but the production onLostFocus handler expects FocusChangedEventArgs. Replace only the synthetic event arguments; retain the blur-commit assertion, all subsequent button assertions and all four declared identities. The judge reports a source-established defect, not an executed test failure; normal verification remains assigned to the supervisor.
- Attempt 04, after repair: SceneSaveLoadTests.fs:214 now raises FocusChangedEventArgs(InputElement.LostFocusEvent). This is the retry's only source change; the blur-commit assertion, every remaining assertion, four test identities and product implementation remain intact. Updated the plan, this log and state-of-the-world. Gate roster and acceptance authority are unchanged; no gates executed.

Attempt 05, before repair: read the task, worker/project prompts, slice/gate contract, manual/010, naming conventions and code-judge cycle 2. The nonfocusable workbench Border dispatches SceneSave at pointer press, while the TextBox commits only on Enter/blur. Resolve the current name from the clicked control's window and commit it first, preserving the existing semantic surface. Extend the existing smoke fact before the product change with direct focused typing/clicking through a real Elmish loop, including a second save after a previous name was committed. No new test identity or gate authority is required; verification stays with the supervisor.

Attempt 05, after regression extension and before product repair: moved the existing click sequence and its assertions into clickSceneControl, retaining every original smoke assertion. Added a real Elmish host in both modes, with two focused KeyTextInput edits followed directly by Save clicks. The added assertions observe the exact SceneSetName/SceneSave sequence, the input model when saving, persisted scene contents/names, unchanged previous scenes and the re-rendered saved-scene control. The second edit follows the first committed save in the same host. No tests executed under the act-only protocol.

Attempt 05, after product repair: workbenchButton passes its pointer event through to its action. The scene Save action resolves the existing sceneNameField in the event's owning window, calls the same commitName used by Enter/blur, then dispatches SceneSave. The callback reads the live control each time, so the ID-keyed subscription retains no draft or model snapshot. Other workbench actions ignore the event and retain their existing semantic dispatch. No message, model field, ID, persistence contract or test identity changed.

Attempt 05, source review and handoff: confirmed the new fixture uses the existing MainWorkbenchTests Elmish host pattern, the added polarizer leaves the Experiments bay selected, and all five workbenchButton call sites use the event callback. All earlier assertions remain, with only their shared click sequence moved to the helper. Product diff line counts agree with --ignore-cr-at-eol; the supervisor's pre-existing manifest change is untouched. Updated the required plan/log/state-of-world. No build, test, analyzer, invariant or census execution was performed.

## Files modified

- Worker plan and this log created before product edits.
- Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SceneSaveLoadTests.fs
- Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj
- Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs
- Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs
- Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs
- specs/0050/.slices/0153-state-of-the-world.md

## Testing state

No checks or gates run. IMPLEMENT Invariant 6 assigns execution and measured results to the arc-runner after worker exit. Build, elevation, constructor-unit-tests, ui-smoke, selected ui-tests and eleven s17 invariants remain for the supervisor.

Attempts 02 and 03 recorded historical supervisor build diagnostics and repaired the validator resolution and two test references. Attempt 04 addresses code-judge cycle 1's source-established LostFocus argument mismatch; the judge explicitly supplies no executed test result and lists the gates as pending. The synthetic event now uses FocusChangedEventArgs. No post-repair build or test result is claimed, and the historical build diagnostics are not current verification results. No MathNet source repair is claimed.

Attempt 05 addresses code-judge cycle 2's source-established stale-name Save defect. The existing smoke identity now includes two direct edit/click saves in each mode, through the real reducer and kit. Exact message order, the reducer's save input, successful status, stored names/content, unchanged previous scenes and new saved-scene controls are asserted. Enter/blur coverage and every prior assertion remain. These are authored regression assertions, not executed results; normal supervisor verification remains required with the unchanged roster and acceptance authority.

Checkpoint 151 records constructor_unit_tests = 1149 and ui_smoke_tests = 215. These are inherited measurements, not new baselines. This slice adds exactly three ui-tests facts and one ui-smoke fact; the expected smoke count is 216, subject to supervisor measurement. The checkpoint has no whole-suite ui_tests value, so none is invented. commit_ready remains false pending supervisor verification, with no unresolved implementation blocker.

## Artifacts

No runtime artifacts generated. Any persistent artifacts belong only in specs/0050/.artifacts.

## Gotchas

- Manual/010 overrides the slice's scratch-then-load initializer recipe. The supplied context must be reused; construction errors continue through existing Result-returning scratch entry points.
- Physical-file/reopen fixtures use TestAppScope.freshKitFolder and openKitAt to retain the folder path; the missing-scene and smoke fixtures use TestAppScope.openedKitIn. All use the existing populated app scope, shared kitSuccess helper and output-local locations. The first context closes before the same folder is reopened; every context/window closes in finally.
- The pre-existing specs/0050/.manifest.state.json modification belongs to the supervisor and is left untouched.
- No gate/census/oracle, checkpoint, analyzer configuration, version ring, ASG/STL source or notebook was changed. No Git mutation command was run.
- For this single-case DU, Scene.SceneName.tryCreate resolves through the case rather than the type. Open Domain.Scene and use SceneName.tryCreate, as the existing scene tests do.
- The slice prose calls removal RemoveSelected; the actual semantic message is RemoveSelectedElement. Follow the production union and existing tests without renaming the message or changing the test census.
- LostFocus must receive FocusChangedEventArgs for the typed FuncUI subscription; plain RoutedEventArgs triggers a cast failure before the existing blur-commit assertion.
- workbenchButton retains an ID-keyed callback. Scene Save therefore resolves its field by the existing automation ID in the click's window at activation, rather than capturing a draft or model value that can become stale after a render.
