# Where we are

Slice 0414 implements the study's Unknowns step for spec 0050 S11.5. The Inverse constructor now places Unknowns after Data; Main keeps Items, Setups and Experiments. Attempt 03 addresses the repeated missing-editor smoke failure by reserving scrollbar space on the Unknowns surface and checking the rendered hit target before pointer input. All requested implementation and authorized test changes are present; verification of the repaired tree is pending.

## What's working

- Added study selection, creation, experiment additions and referenced-material opening to the Unknowns surface.
- Rendered ordered item and material parameters with state, value, bounds, sigma, scope, links and the shared editor.
- Added stable automation IDs and guarded Inverse-only flow navigation with read-only controls.
- Added the shared reopened study-kit fixture, two smoke facts, two structure facts and one scroll-reachability fact; extended existing flow and read-only coverage.
- Reserved scrollbar space on the Unknowns surface so bars cannot cover rows brought to the viewport edge.
- Added rendered hit-target checks before study smoke pointer presses; retained immediate editor and persistence assertions.

## Tests

No build, tests, elevation or invariant gates were run by this worker. The IMPLEMENT protocol's Invariant 6 assigns execution to the supervisor after worker exit. Commit readiness remains false pending verification of the repaired tree.

Historical supervisor evidence for attempt 01: build and review passed; elevation passed in 235.6 seconds (600-second deadline), and constructor-unit-tests passed in 158.6 seconds (180-second deadline). ui-smoke failed with exit 2, 259 succeeded / 1 failed / 260 total in 230.025 seconds reported by MTP (600-second deadline). The failure was ParameterEditUnknownChoice absent after pressing Phone.darkOffset. These are supplied supervisor results, not worker runs or results of the repair. No supplied duration exceeded its allowance; selected ui-tests and the twenty invariants were not reached.

The attempt 03 task supplies the supervisor's result over attempt 02: review/build passed; ui-smoke again failed with exit 2, 259 succeeded / 1 failed / 260 total, MTP duration 234.015 seconds against the 600-second deadline. The failure moved to the added immediate ParameterEditPanel assertion at StudyViewTests.fs:229, after the same row press; the case took 41.932 seconds. Those results do not validate the new repair. No supplied gate duration exceeds its deadline, and no worker-measured counts or passes are claimed.

The resolved roster is build, elevation, whole constructor-unit-tests, whole ui-smoke, selected ui-tests (KitWindowTests, ScrollExtentTests, StudyViewTests), and all twenty s40 invariants. The authorized census additions are two ui-smoke identities and three ui-tests identities; constructor identities are unchanged. The smoke facts type through actual controls and reread persisted study/item files. Structure coverage also exercises scope persistence and study experiment additions.

Historical floors below are inherited, not measured this round: checkpoint 412 supplies 1,752 constructor and 258 smoke cases; the 0413 handoff supplies 16 ui_tests for its KitWindowTests selection. This slice expands the selected classes, so 16 is not a claimed total for the new selection. No checkpoint, census, deadline, oracle or gate descriptor was changed.

```yaml
gates:
  constructor_unit_tests: 1752
  ui_smoke_tests: 258
  ui_tests: 16
```

## Architecture

StudyUnknownsView projects the existing model and sends the existing study and parameter messages. StudyContext.itemsUsedBy owns item selection/order; opened materials retain study order; Parameter.gridCells and ItemsPanelView.parameterEditorView own parameter formatting and editing. No new domain or storage behavior is introduced. Material offers resolve stored item-model versions through model.tryItemModelChoice.

Study actions and editor wrappers carry keys that preserve their current study and parameter owner. Scope/link callbacks refresh on each patch. Writable controls follow model.kitWrites; the underlying command guards remain authoritative. The fixture records canonical item/setup/experiment files, copies real data through KitProxy, closes and reopens the kit, and creates QZ-LR through StudyCommand.

Attempt 03 changes StudyUnknownsView and the study test interaction fixture. The view sets AllowAutoHide=false on its own ScrollViewer; horizontal and vertical visibility remain Auto. This reserves layout space for the bars instead of allowing them to cover the viewport's edge. The test helper completes scrolling and rendering, asserts that the computed centre hits the identified control or its descendant, and reuses SlotBarTests.pressBox for the actual pointer input. Standard window size, test identities and stored-result assertions are preserved; domain and storage behavior are unchanged.

The interception diagnosis follows the repeated supervisor failure and the pinned Avalonia 12.1.4 source: the [Fluent template](https://github.com/AvaloniaUI/Avalonia/blob/2d79ba55fae1b1e71c69f93a668080c9029e6a31/src/Avalonia.Themes.Fluent/Controls/ScrollViewer.xaml#L67) spans the scrollbar row and column when auto-hide is enabled, while [BringDescendantIntoView](https://github.com/AvaloniaUI/Avalonia/blob/2d79ba55fae1b1e71c69f93a668080c9029e6a31/src/Avalonia.Controls/Presenters/ScrollContentPresenter.cs#L213) can align the target with that viewport edge. It is a source-based inference, not a reproduced runtime diagnosis; supervisor verification must confirm the repair.

## Deferred

No requested implementation is deferred. Supervisor verification is pending by the worker protocol.

## Gotchas

- The shared parameter editor's own key uses the parameter name; the new wrapper also keys by study and owner so equal names on different items cannot retain a stale Set handler.
- Referenced material versions are a Set; offers enumerate it before deduplication across study items.
- Runtime fixtures belong under test output. This worker generated no runtime artifacts.
- The pre-existing change to specs/0050/.manifest.state.json was preserved. Product/test edits stay within the twelve authorized paths.
- Attempt 02 also preserves the supervisor-owned `.expected-red-observed-json` change. Its only source edit is StudyViewTests.fs; the shared pointer helper remains outside this slice's effects.
- Attempt 02's layout-only repair was insufficient. Attempt 03 reserves scrollbar space in the product and checks the hit target; it changes only StudyUnknownsView.fs, StudyViewTests.fs and the three assigned worker documents.
- A control can be effectively visible and laid out yet have its centre covered by a scrollbar. The smoke helper now reports the hit control's type and centre if that occurs, without fixed sleeps or bypassing the UI handler.
- All authored text uses LF. No notebooks, version locks, analyzer configuration, external repositories or git history were changed.

## Changelog

- 2026-11-14: Implemented the Inverse Unknowns step, stable IDs, shared disk-backed study fixture and authorized UI/model coverage; handed verification to the supervisor.
- 2026-11-14, attempt 02: Repaired study smoke scroll-layout preparation, retained all acceptance assertions and added immediate editor checks; handed the repaired tree to supervisor verification.
- 2026-11-14, attempt 03: Reserved scrollbar space on Unknowns and added rendered hit-target assertions after the repeated missing-editor failure; retained the full acceptance surface and left runtime verification to the supervisor.
