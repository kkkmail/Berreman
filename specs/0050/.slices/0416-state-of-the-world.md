# Where we are

Slice 0416 completes spec 0050 S11.5's retirement of the obsolete solver handoff after the study Fit step landed in 0415. The source changes and exact authorized test migrations are implemented. Supervisor review and the frozen verification roster remain pending; this IMPLEMENT worker executed no gates.

## What's working

- Removed the obsolete handoff view, window, window key, automation IDs and collection snapshot types.
- Moved three view facts into StudyViewTests over the live Fit step and its existing pre-fit validation.
- Retired eight redundant validation facts while preserving the existing domain and ellipsometric coverage.
- Updated chart reachability, root window composition and single-instance launcher coverage for the remaining windows.
- Removed stale project entries and shortened affected documentation to the current contracts.

These bullets describe delivered source, not runtime pass results.

## Tests

No build, test, analyzer or invariant command ran. IMPLEMENT Invariant 6 assigns execution and gate verdicts to the supervisor after worker exit. `commit_ready=false` records pending verification; no implementation is deferred and no passing build is claimed. No development-run duration or deadline overrun exists to report.

The unchanged `0416.gates` roster requires:

- Build and arc-origin elevation, 600 seconds each.
- All constructor-unit-tests, 180 seconds; checkpoint 414's 1,753 cases remain the expected count.
- All ui-smoke, 600 seconds; checkpoint 414's 261 cases remain the expected count.
- Selected ui-tests over ScrollExtentTests, StudyViewTests and WindowLauncherTests, 360 seconds.
- All twenty `invariant-s40-*` gates, 300 seconds each.

The three migrated facts check QZ-LR's E1/E2/E3 order and typed results, the mounted Fit panel and pre-fit list, and the displayed ready/missing-data/inconsistent-data messages. E1 reads three in-range points; E2 stays unattached; E3 includes x = 120 outside its 0–90 scan. The attachment-status proxy returns AttachedDataUnchanged. Exactly two facts retain the smoke trait and one remains non-smoke. Eight other validation facts retire as authorized, reducing the full non-smoke UI census by eight; the two renamed facts and launcher expectation change preserve their counts.

The YAML carries historical floors only: constructor and smoke are copied from checkpoint 414, and ui_tests retains the prior handoff's historical 16, not a measured count for this selected roster. The supervisor owns census and checkpoint publication.

```yaml
gates:
  constructor_unit_tests: 1753
  ui_smoke_tests: 261
  ui_tests: 16
```

## Architecture

The study Fit view remains the sole UI projection of the existing Domain pre-fit checks. No new validation logic, storage format or compatibility layer was introduced. The obsolete snapshot existed solely for the retired window and is deleted with it; studies continue reading the kit's experiments.

The migrated tests reuse `TestAppScope.studyKitIn`, `fitModelWith`, the canonical intensity parser and existing semantic UI helpers. Their local proxy selects mapped points by each experiment's actual attachment path. The shared Fit preparation binds the source before reloading, ensuring these tests reach data validation instead of failing slot specification.

## Deferred

No requested implementation is deferred. Supervisor gate execution remains mandatory before acceptance under the worker protocol.

## Gotchas

- The brief's ellipsometric test title is historical. The existing `InverseConstructorTests.the profile the wizard chose selects the ellipsometric read` still asserts the detector, import profile and validated ellipsometric result; it was preserved unchanged.
- `PrefitChecks.fs`, the data-load project entry and constructor round-trip documentation already lacked the retired identifiers, so no unnecessary edits were made there.
- Source inspection found no remaining retired identifiers under Berreman. The edited source is LF, with matching ordinary and ignore-CR-at-EOL numstats. These are source-review observations, not gate verdicts.
- Preserve the entry change to `.manifest.state.json`; it belongs to the supervisor. No Git mutation, version bump, gate/census/checkpoint edit, external-repository change or notebook edit occurred.
- No runtime artifacts were produced. The plan, log and this handoff are at the task's exact assigned paths.

## Changelog

- 2026-11-14: Removed the obsolete solver handoff and snapshot, migrated three view facts to the study Fit step, retired eight redundant validation facts and updated remaining window coverage; handed verification to the supervisor.
