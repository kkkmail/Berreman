# Where we are

Slice 0153 implements the scene save/load workbench surface in spec 0050 S4.3, over the kit-backed model from 0152 and the existing scene/snapshot storage. Implementation and the four authorized test identities are written. Attempt 05 repairs Save activation so it commits the current name field before dispatching SceneSave, and extends the existing smoke fact with direct edit/click saves through a real Elmish host. Prior repairs and all original assertions remain intact. The arc-runner owns normal verification after this worker exits.

# What's working

- Added semantic scene naming, save and load commands with kit-status feedback.
- Reused kit scene recording and restoration, keeping saved names distinct and ordinal-sorted.
- Added the accessible scene row and named saved-scene buttons in both constructor modes.
- Reused Main initialization with an already-opened kit context under manual/010.
- Added three persistence/refusal facts and one headless scene-row fact.
- Committed the live scene-name text before Save and extended the existing smoke fact with successive direct edit/click saves.

# Tests

No build, test, analyzer, invariant or census execution was performed, per IMPLEMENT Invariant 6. No pass or measured post-change count is claimed. The supervisor must run build, elevation, constructor-unit-tests, ui-smoke, selected ui-tests and all eleven s17 invariants from 0153.gates.

Attempts 02 and 03 repaired historical supervisor-reported compilation errors. Attempt 04 addresses code-judge cycle 1's source-established runtime defect: plain RoutedEventArgs cannot satisfy the typed onLostFocus handler. The event now uses FocusChangedEventArgs(InputElement.LostFocusEvent), preserving the blur-commit assertion and subsequent Save/Load button checks. The judge supplied no executed test failure and listed gates as pending. No post-repair result is available; historical build diagnostics are not current verification results. Prior repairs remain intact; no MathNet source repair is claimed.

The four exact SceneSaveLoadTests identities cover the sanitized setup filename, preset snapshot under library, restoration and reopen, blank-name refusal with no writes, missing-scene refusal preserving the table, and headless controls in both modes. Existing identities and assertions remain unchanged. Additional assertions within those identities cover repeat-save de-duplication, ordinal ordering, read-only refusal, name commits on Enter/blur and Save/Load button dispatch.

Attempt 05 addresses code-judge cycle 2's source-established stale-name Save defect. The same smoke fact now also opens a real Elmish host in each mode, focuses and types into the name field, then clicks Save without synthesizing Enter or LostFocus. A second edit/save follows the first committed name in the same host. It asserts SceneSetName before SceneSave, the current name in the reducer's save input, successful status, stored names/content, preservation of all previous scenes and rendered saved-scene buttons. The original click implementation and assertions are reused through a private helper. No new identity was added and no prior assertion was removed; these extensions await supervisor execution.

Checkpoint 151 supplies the inherited counts below. The expected smoke increase is one (215 to 216), and the selected non-smoke roster gains three facts, subject to supervisor measurement. The checkpoint supplies no whole-suite ui_tests baseline; none is authored here.

```yaml
gates:
  constructor_unit_tests: 1149
  ui_smoke_tests: 215
```

# Architecture

SceneName.tryCreate validates the draft caption at save. KitLibraryContext.recordScene retains sole ownership of snapshot-before-setup persistence; KitProxy.readScene and restoreScene retain sole ownership of reading and restoring the table. The model accepts a saved name only after successful recording; failure leaves state intact except for kitStatus. loadKit initializes the saved names from OpenedKit.scenes.

The view projects the same model in both modes. IDs remain on ConstructorUiId, with the SceneName-derived ID and caption in the Ui extension module. The shared workbenchButton supplies its caption as AutomationProperties.Name. No schema, serializer, migration or fallback was added.

Scene Save receives the workbench pointer event, locates the live name field by its existing automation ID in that event's window and invokes the same commitName used by Enter/blur before dispatching SceneSave. The shared button passes its event to the action; the other actions ignore it. The ID-keyed subscription captures neither a draft name nor a model snapshot, so successive renders do not make the name stale. The reducer and storage remain unchanged by this repair.

Manual/010 overrides the scratch-then-load test recipe: initMainWithKit now owns the existing Main seed construction, reused by the Result-returning scratch initializer and the new tests. Opened-kit models receive the supplied context directly. Fixtures retain explicit output-local paths where file assertions/reopen require them and otherwise use TestAppScope.openedKitIn.

# Deferred

No implementation requirement is deferred. Verification is assigned to the supervisor. Empty gate results and commit_ready false indicate pending verification, not an unresolved implementation question.

# Gotchas

- Scene names preserve their original text in storage/UI; file stems sanitize forbidden characters, so Bench: A is stored as Bench_ A.setup-json.
- Open Domain.Scene before calling SceneName.tryCreate: the module-qualified expression resolves through the same-named DU case, producing FS0039.
- The removal message is RemoveSelectedElement, despite the slice prose's RemoveSelected shorthand. Visual descendants are selected with Seq.choose and a Control type pattern; Seq.ofType is unavailable.
- Synthetic LostFocus events must carry FocusChangedEventArgs to match the typed FuncUI handler; plain RoutedEventArgs fails before the blur-commit assertion.
- The Save verb is a nonfocusable Border. Its handler must commit the live field explicitly; focus loss cannot be assumed, and an ID-keyed callback must not retain a stale draft.
- Snapshot errors may retain snapshots already written, per the existing recordScene contract; this slice does not add transaction or rollback behavior.
- Tests close the first kit before reopening its folder and close all kit/window owners in finally blocks.
- Product diff counts match with and without --ignore-cr-at-eol; source and worker output edits use LF.
- The pre-existing supervisor-owned .manifest.state.json change is untouched. No gates, census/oracle, checkpoints, analyzer settings, version rings, ASG/STL files or notebooks were modified. No Git mutation was performed.

# Changelog

- 2026-11-08: Added kit-backed scene save/load controls, reused supplied-kit Main initialization and authored the four declared acceptance tests; handed verification to the arc-runner.
- 2026-11-08 (attempt 02): Repaired the supervisor-reported SceneName.tryCreate resolution error; preserved all implementation behavior, four test identities, assertions and inherited counts, with verification assigned to the supervisor.
- 2026-11-08 (attempt 03): Repaired the two supervisor-reported test compilation references; preserved all four identities, assertions and product behavior, with verification assigned to the supervisor.
- 2026-11-08 (attempt 04): Repaired the smoke fact's lost-focus event arguments; preserved the blur-commit and all remaining assertions, all four identities, gate roster and acceptance authority, with normal verification assigned to the supervisor.
- 2026-11-08 (attempt 05): Committed the current scene-name field before Save and extended the existing smoke identity with successive focused edit/click saves in both modes; preserved all prior assertions, semantic contracts, four identities and gate authority. Normal supervisor verification remains pending.
