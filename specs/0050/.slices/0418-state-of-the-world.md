# Where we are

Slice 0418 implements spec 0050 S11.5's Publish step after Results. All declared production changes, existing-test extensions and three new test identities are present in source. This IMPLEMENT worker ran no gates; supervisor verification is pending.

# What's working

- Added Publish after Results in the Inverse flow with stable control and promotion IDs.
- Previewed the selected run's pending calibrated revisions through calibratedItemsWith.
- Connected calibration and material/black-box promotion buttons to the existing window messages.
- Withheld write controls for read-only kits and retained the revision/provenance explanations.
- Added preview, promotion, persisted calibration, mode-switch and small-screen reachability coverage.

These bullets describe delivered source, not measured runtime results.

# Tests

No build, test, analyzer or invariant command was executed. The task's IMPLEMENT Invariant 6 assigns all gate execution and verdicts to the supervisor after worker exit. `commit_ready=false` means verification remains pending, not that implementation has been intentionally deferred.

The unchanged `0418.gates` roster requires build and arc-origin elevation (600 seconds each), all constructor-unit-tests (180 seconds), all ui-smoke (600 seconds), selected non-smoke ui-tests over KitWindowTests, ScrollExtentTests and StudyViewTests (360 seconds), and all twenty s40 invariants (300 seconds each). No worker development-run duration or deadline overrun exists to report.

The exact authorized additions are:

- Non-smoke: `StudyViewTests.the Publish step previews the chosen run's calibration and lists what it can promote`.
- Smoke: `StudyViewTests.pressing Publish calibration writes the calibrated items' next revisions into the kit`.
- Non-smoke: `ScrollExtentTests.the kit window's Publish step leaves everything reachable`.

Existing ID, flow ordering, mode switching and read-only assertions were extended without removing their prior assertions. The preview fact also exercises invariant rendering under fr-FR, missing-sigma refusal, no-run rendering, both real promotion buttons, recorded provenance, preserved original material version and selecting an older run. The smoke fact reads CPL1.item-json through the kit proxy, asserts revision 2 and the calibration, preserves its previous parameter history and verifies other items and the fit-run file remain unchanged. The reachability fixture calibrates three items and offers both a material and a black-box promotion at the existing 800×600 and 1000×540 sizes.

The YAML below inherits constructor/smoke counts from supervisor checkpoint 416. There is no ui_tests entry in the checkpoint registry; 16 is the preceding handoff's historical floor, not a measured count for this selection. Expected test effects are +1 smoke, +2 non-smoke and no constructor count change. The supervisor owns measurement and checkpoint publication.

```yaml
gates:
  constructor_unit_tests: 1754
  ui_smoke_tests: 262
  ui_tests: 16
```

# Architecture

The Publish view projects selectedStudyOpt and selectedFitRunOpt. It uses calibratedItemsWith for both the displayed pending revisions and Publish availability, then renders the parameters whose states change in those revisions. Promotion subjects come from promotionSubjectsWith and model.tryItemModelChoice; no alternative publishing, promotion or model-resolution implementation was added. Material display names use the study's opened-material snapshot.

PublishCalibration and PromoteToLibrary remain the sole write paths. The study bar displays their existing kitStatus outcomes. The view explains that publication keeps previous values in item revision history and promotion preserves the version the study opened. Its complete content scrolls in both directions; the run-specific subtree is keyed so action handlers cannot retain an earlier selected run.

The shared UI fixture lives in the already earlier-compiled ScrollExtentTests.fs, within the authorized edits. It reuses StudyFitViewFixture, TestAppScope and shipped BuiltinCatalogue item models, records append-only fit runs through writeFitRun and selects them through the real window update. No schema, storage contract, numerical result or migration changes.

# Deferred

No requested implementation is deferred. Supervisor execution of the frozen gate roster remains required before acceptance.

# Gotchas

- The existing actionButton retains its handler by stable ID; preserve the run key around the publication controls.
- readLabFile returns LabFileSource; byte comparisons use its content field.
- The UI test executable cannot refer to constructor-only test fixtures. Shared support remains in the UI project without adding a project reference or test identity.
- Ordinary and ignore-CR-at-EOL numstats agree for the edited tracked source. All ten production/test files were written with LF endings.
- Preserve the pre-existing specs/0050/.manifest.state.json change; it belongs to the supervisor. No Git mutation, version bump, gate/census/checkpoint rewrite, analyzer exception, external-repository edit or notebook edit occurred.
- No runtime artifacts were produced. Worker documents use only the task's exact assigned paths.

# Changelog

- 2026-11-14: Added the Inverse Publish step, calibration preview/publication and material/black-box promotion controls; extended the authorized tests and handed verification to the supervisor.
