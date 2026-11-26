# Where we are

The operator-approved recovery started from the committed partial attempt
`d461ad3fca4cbfd5d98cae362029d7671b370e60`. Root-qualified `open global.Elmish`
resolved FS0893. The first executable focused run then exposed a separate
rendering failure: the saved-scenes panel declared both the control Name and
AutomationProperties.Name, which FuncUI 2.0.0 matches by the same short name.
On rerender it tried to reassign an already styled control's Name. Elmish caught
the exception and retained the previous model, so the subsequent save received
the old blank caption. A view key alone did not fix the collision.

The panel now initializes its write-once Name through WrapPanel.init, retains
its automation ID and accessible name, and carries its stable view key in the
variable-membership section. The existing smoke fact captures Elmish errors
and checks both names after each save. Every original assertion and all four
declared identities remain. The regression passes two successive focused
edit/click saves in each mode, with actual reducer input, persistence and
previous-scene preservation; Enter/blur and refusal coverage remain intact.

The repair is ready for the operator's commit. No commit or relaunch was
performed. After that commit and an explicit relaunch request, use
`--accept-current-slice --executability-profile 1`, as approved in the secondary
review. The 0150-only normal-resume exception does not apply. Acceptance uses
the existing expected-red backfill; numeric floors walk back to checkpoint 151.
Do not fabricate checkpoint 152 or a selected-UI baseline. Step 0154 remeasures
the two whole suites, but its different UI selection does not replace this
completed 0153 verification.

# What's working

- Added semantic scene naming, save and load commands with kit-status feedback.
- Reused kit scene recording and restoration, keeping saved names distinct and ordinal-sorted.
- Added the accessible scene row and named saved-scene buttons in both constructor modes.
- Reused Main initialization with an already-opened kit context under manual/010.
- Added three persistence/refusal facts and one headless scene-row fact.
- Committed the live scene-name text before Save and extended the existing smoke fact with successive direct edit/click saves.

# Tests

All sixteen unchanged 0153 gates passed: build, elevation, constructor-unit-tests,
ui-smoke, the full 46-class selected ui-tests roster and eleven s17 invariants.
Measured 1,149 constructor cases, 216 smoke cases and 617 selected UI cases
(581 method identities), with zero failures/skips and exact census matches.
The focused SceneSaveLoadTests run passed all four facts. The repaired Release
build and full-roster build gate both reported zero warnings and zero errors.
Supplemental elevation against accepted 0152 (`fc0d289`) passed over five
product projects, including the committed Controls/Ui changes and dependents.

Read-only validation passed for all 494 steps and the existing profile-1 receipt.
All 988 compiled slice/gate artifacts remain byte-identical. Published spec,
oracle, effects, supervisor state/counters, expected-red records, checkpoints,
version files/lock, ASG/STL source and Git HEAD/index are unchanged. No checkpoint
152 was created. The selected UI count is an observation, not a whole-suite floor.
Inherited YAML floors remain 1,149 constructor / 215 smoke from checkpoint 151.

Evidence is under [0153-operator-repair](../.artifacts/0153-operator-repair/):
`verification-summary.json`, `gate-results.json`, `test-inventory.json`, the
three full-gate TRX reports, `focused-results/scene-focused.trx`, build/analyzer
logs, `contract-validation.json` and `final-scope-check.json`. The
[rendering diagnosis](../.artifacts/0153-operator-repair/rendering-diagnosis.md)
links the exact installed FuncUI source and preserved failing runs. The final
scope audit confirms the verified product bytes and all original assertions.

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

No implementation or verification work remains for this repair. Waiting for the operator to commit and then explicitly request relaunch. Historical worker handoffs remain historical; supervisor acceptance has not been published.

# Gotchas

- Root-qualify Elmish when Avalonia.FuncUI is open; its child module otherwise captures the bare import.
- The saved-scenes panel initializes Name once, outside FuncUI's property diff; do not combine Name and AutomationProperties.Name as ordinary attributes.
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

- 2026-11-08 (approved operator recovery): Repaired the Elmish import and measured saved-scenes Name collision; preserved all prior assertions and strengthened the existing smoke fact. All sixteen gates, exact census, supplemental elevation and unchanged plan/receipt validation passed. Awaiting operator commit; no relaunch or acceptance performed.
