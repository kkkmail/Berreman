# Where we are

Slice 0417 implements spec 0050 S11.5's Results step after the study Fit surface and obsolete-handoff retirement. Product code and all four authorized test additions are present. Attempt 03 repaired the two UI fixtures' references to a helper compiled only into the constructor unit-test project, reusing the existing UI history constructor. Reverification against the frozen roster remains pending; this IMPLEMENT worker executed no gates.

# What's working

- Added Results after Fit in the Inverse flow and kept the Main flow unchanged.
- Added six results tabs with stable automation IDs and window-only selection state.
- Displayed the selected study's chosen or latest stored fit run, including all six diagnostics and empty states.
- Reused owner captions, product ratios, realizability and the shared study controls.
- Added model, rendering, semantic click and wide correlation-table reachability coverage.
- Repaired the UI fixtures' unavailable helper dependency while retaining all Results assertions and test identities.

These bullets describe delivered source, not measured runtime results.

# Tests

This worker ran no build, test, analyzer or invariant command. The task's IMPLEMENT Invariant 6 assigns gate execution and verdicts to the supervisor after worker exit. `commit_ready=false` records that verification remains pending.

The latest supervisor build failed with 19 errors, starting with unavailable `FitRunFileFixtures` references in the UI test project and cascading to ambiguous record types. Its output reported 25.47 seconds elapsed against the 600-second gate allowance. Attempt 03 removed both invalid dependencies and gave the new payload records concrete types. No post-repair build or test outcome is claimed. No worker development-run duration or deadline overrun exists to report. Attempt 02's preceding indentation repairs remain in place.

The unchanged `0417.gates` roster requires build and arc-origin elevation (600 seconds each), all constructor-unit-tests (180 seconds), all ui-smoke (600 seconds), selected non-smoke ui-tests over KitWindowTests, ScrollExtentTests and StudyViewTests (360 seconds), and all twenty s40 invariants (300 seconds each).

The exact authorized additions are:

- Constructor: `a results tab and a fit run choose what the Results step shows, the study's latest run by default`.
- Non-smoke UI: `the Results step shows the chosen fit run's six tabs and each tab's table`.
- Smoke: `pressing each results tab shows its table`.
- Non-smoke UI: `the kit window's Results step leaves everything reachable`.

The model fact covers latest, explicit, stale, empty and unselected run resolution, tab order/captions and unchanged kit bytes. Rendering coverage writes a test-local run through `writeFitRun`, reusing the UI history constructor with a rich payload, a 2-by-2 correlation matrix and seven profile points. It checks invariant formatting under fr-FR, undetermined and absent sigma, rank-deficient correlation, element/fringe coordinates, both realizability verdicts, stored residual/product summaries, failed profile points and all empty states. The smoke fact presses every stable tab ID and returns to Values; the reachability fact exercises a stored 16-by-16 matrix at both existing small-screen sizes. All assertion bodies from attempt 02 are unchanged by this repair.

The YAML retains historical floors only: constructor and smoke come from checkpoint 415, and ui_tests retains the prior handoff's historical 16, not a measured count for this selected roster. Expected additions are +1 constructor, +1 smoke and +2 non-smoke UI. The supervisor owns census measurement and checkpoint publication.

```yaml
gates:
  constructor_unit_tests: 1753
  ui_smoke_tests: 261
  ui_tests: 16
```

# Architecture

Results is a pure view over `FitRun.entries` and `FitRun.results`. `selectedFitRunOpt` resolves only the selected study's history, with latest-run fallback. `ShowResultsTab` updates window state and performs no write. No fit is run by the view. The existing `ObjectRealizability.realizability` and `SeriesProductCheck.rmseRatio` members remain the sole definitions of those readouts.

`ownerCaption` shares item/material naming with the existing study parameter captions; `fitEntryCaption` adds the parameter name. The new view reuses the study bar and action/verb controls. A single scroll viewer surrounds the complete Results panel, with both axes enabled and reserved scrollbar space. The chosen tab's table is keyed by its stable tab ID.

No serialized shape or schema changes. No migration or compatibility path is introduced. Production and test edits remain within the ten authorized source paths.

Attempt 03 changes only the two UI fixture constructions. Common fit-run metadata comes from the earlier-compiled `StudyFitViewFixture.fitViewRunOf`; each fixture declares the result data it displays. The UI project gains no dependency on another executable test project or its fact-containing source files.

# Deferred

No requested implementation is deferred. Supervisor verification remains mandatory before acceptance under the worker protocol.

# Gotchas

- `ItemId.labelAmong` already returns ControlCaption; its result is reused directly.
- Results may show an older selected run while the Fit step continues checking against its latest run; the existing Fit behavior is preserved.
- The table's correlation entries use F3; the other numerical readouts use invariant G6. Rank-deficient solves show the specified explanation rather than a grid.
- Preserve the entry change to `.manifest.state.json`; it belongs to the supervisor. No Git mutation, version bump, gate/census/checkpoint edit, external-repository change or notebook edit occurred.
- Ordinary and ignore-CR-at-EOL numstats match for both files repaired in attempt 03. This is a source-review observation, not a gate verdict.
- `FitRunFileFixtures` is available to constructor unit tests only. UI fixtures must reuse their own earlier-compiled `StudyFitViewFixture`; an `open OpticalConstructor.Tests` does not import the other assembly's files.
- No runtime artifacts were produced. The implementation plan, log and this handoff use the task's exact assigned paths.
- A multiline nested record value must be indented beneath its field; these repairs place the outer `with` on a separate line so the field starts at the established indentation. Preserve the F# strict-indentation setting.

# Changelog

- 2026-11-14: Added the study Results step and six stored-result tabs, shared owner/entry captions and the four authorized model/UI tests; handed verification to the supervisor.
- 2026-11-14, attempt 02: Repaired the supervisor-reported nested record-update indentation in all three Results fixtures; preserved acceptance, test identities and assertions, and returned the tree for supervisor verification.
- 2026-11-14, attempt 03: Replaced unavailable constructor-only fixture references in both Results UI fixtures with the existing UI history constructor and explicit local result payloads; preserved all assertions and handed the unchanged gate roster back to the supervisor.
