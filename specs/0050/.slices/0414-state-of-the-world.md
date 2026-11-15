# Where we are

Slice 0414 implements the Inverse constructor's Unknowns step for spec 0050 S11.5. The implementation is committed as `a8678177`, followed by the render-until-hit repair in `c92d8b87`. This normal-resume worker reviewed the committed source and updated the three assigned handoff documents; no further product or test change was indicated. Formal review and the unchanged 25-gate verification remain pending with the active supervisor.

## What's working

- Added Inverse-only Unknowns navigation after Data, preserving the open bay and Main's existing flow.
- Added study selection, creation, experiment additions, material opening and the ordered parameter grid with the shared editor.
- Preserved stable automation IDs, stored model resolution, read-only guards and the authorized model/UI coverage.
- Preserved reserved scrollbar space and the bounded render-until-hit wait before every study smoke pointer press.
- Reviewed the committed repair without changing source, test identities, assertions or supervisor metadata.

These describe the delivered source; they are not fresh runtime-pass claims.

## Tests

This IMPLEMENT worker ran no build, tests, elevation or invariant command. The role protocol's Invariant 6 assigns those phases to the supervisor after worker exit. `commit_ready=false` reflects the absence of fresh verification, not unfinished implementation.

The active supervisor has already launched with `--executability-profile 1` and `.manual/028-step-0414-operator-reply.md`, completed its boot checks and dispatched this worker at index 413. Its log records the 25-gate snapshot and immutable arc origin `f395597c071910aaa215518626563f6c784bfb62`. Do not launch a second supervisor or use `--accept-current-slice`.

Required supervisor verification is unchanged:

- Build and elevation from the immutable arc origin: 600-second deadlines each.
- Whole constructor-unit-tests: 180 seconds; constructor identities unchanged.
- Whole ui-smoke: 600 seconds; two authorized additions imply 260 expected cases from the historical 258, not a measured result.
- Selected ui-tests over KitWindowTests, ScrollExtentTests and StudyViewTests: 360 seconds; three authorized additions.
- All twenty `invariant-s40-*` gates in `0414.gates`: 300 seconds each.

Require the exact authorized census, zero failures/skips and no owned warnings. Only a successful full supervisor cycle may publish measured checkpoint 413. No gate allowance, acceptance condition, baseline or checkpoint was changed by this worker.

Historical evidence is distinct from verification of the committed repair. The post-mortem records three pre-repair smoke failures, each 259 passed / 1 failed / 260 total and zero skips. MTP durations were 230.025, 234.015 and 232.052 seconds against 600 seconds; the last stopped because the centre hit ScrollContentPresenter. The last preceding build, elevation and constructor runs took 4.0, 221.4 and 160.5 seconds against 600, 600 and 180 seconds. No supplied historical duration exceeded its deadline; selected ui-tests and the twenty invariants were not reached.

Operator reply 028 reports reproducing the overlay-scrollbar obstruction and stale hit-test frame independently, and 1–2 ticks to converge after the bounded-wait repair. That is operator evidence, not a worker-run test result or a full-roster pass.

The YAML retains inherited floors only: checkpoint 412 supplies constructor 1,752 and smoke 258; the prior handoff supplies ui_tests 16 for its narrower KitWindowTests selection. Sixteen is not the expected total of this slice's expanded selection.

```yaml
gates:
  constructor_unit_tests: 1752
  ui_smoke_tests: 258
  ui_tests: 16
```

## Architecture

StudyUnknownsView projects the existing model and sends the existing study and parameter messages. StudyContext.itemsUsedBy owns item order; opened materials retain study order; Parameter.gridCells and ItemsPanelView.parameterEditorView own formatting and editing. Material offers use stored item-model versions through model.tryItemModelChoice. The fixture writes canonical kit content, closes and reopens the kit, and creates its study through StudyCommand.

The Unknowns ScrollViewer keeps AllowAutoHide=false so visible scrollbars reserve space. StudyViewTests.pressStudyBox brings the identified control into view, then advances render ticks and dispatcher jobs until the centre hits that control or a descendant, bounded at 50 ticks. Failure includes the hit type, centre and target bounds. Actual input still comes from SlotBarTests.pressBox; no fixed sleep, resized window or semantic-command substitute was introduced. Immediate editor checks and persisted Phone.darkOffset start/bounds assertions remain.

No domain, storage, durable-format or architectural change was made during this resume. The committed product and fixture already contain the required repairs.

## Deferred

No requested implementation is deferred. The supervisor must complete its normal formal review and all 25 gates after this worker exits; the worker cannot publish its checkpoint.

## Gotchas

- Keep both repairs together: reserving scrollbar space does not settle stale hit data, and rendering does not remove a scrollbar obstruction.
- Steps 0415–0418, 0426 and 0460 must retain the bounded wait for every study press and AllowAutoHide=false when editing the study source/tests.
- The post-mortem's original uncommitted-tree and proposed-repair text is historical. Operator reply 028 establishes the current committed state and normal-resume route.
- The editor wrapper includes study and owner identity so equal parameter names cannot retain another owner's Set handler.
- Entry changes to `.executability-json`, `.manifest.state.json` and `.supervisor-lock.diag` belong to the active supervisor and were left untouched. Published contracts, descriptors, census effects, consumed counters and checkpoints were preserved.
- Only the three assigned worker Markdown files were edited. No Git mutation, version bump, external-repository write or product/test modification was performed.

## Changelog

- 2026-11-14: Implemented the Inverse Unknowns surface, IDs, shared disk-backed fixture and authorized UI/model coverage.
- 2026-11-14, attempts 02–03: Added layout preparation, then reserved scrollbar space and a single-frame hit assertion; supervisor smoke verification still failed.
- 2026-11-14, operator repair: Committed the bounded render-until-hit wait as c92d8b87 after reproducing the two stacked causes.
- 2026-11-14, normal resume: Reviewed the committed implementation and repair, preserved source and test contracts, and refreshed the worker handoff for the active profile-1 supervisor's full verification.
