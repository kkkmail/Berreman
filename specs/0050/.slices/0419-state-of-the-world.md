# Where we are

Slice 0419 completes S115-WIRE's constructor composition for the study flow. The
shared constructor mount now uses the existing real fit host, and the one
authorized smoke test and documentation changes are present in source. The
WIRE_UI worker ran no gates; supervisor verification is pending.

# What's working

- Connected both constructor windows to StudyFitJobHost.onUiThread through their captured dispatch.
- Added the real-window Empty-study smoke test with a bounded dispatcher wait and persisted-file assertions.
- Documented background study fitting on both windows and the complete Inverse study flow in AGENTS.md.
- Preserved every existing consumer assertion and the other window shell seams.

These bullets describe delivered source, not measured runtime results.

# Tests

No build, test, analyzer or invariant command was executed. WIRE_UI Invariant 6
assigns gate execution and verdicts to the supervisor after worker exit.
`commit_ready=false` means verification remains pending; no requested
implementation is deferred.

The unchanged `0419.gates` roster requires build and arc-origin elevation
(600 seconds each), all constructor-unit-tests (180 seconds), all ui-smoke
(600 seconds), selected non-smoke ui-tests (360 seconds) and all twenty s40
invariants (300 seconds each). The selected UI classes are AppContextTests,
KitWindowTests, LauncherTests and TableAndElementRotationTests. There is no
worker development-run duration or deadline overrun to report.

The sole new identity, carrying Category=ui-smoke, is
`OpticalConstructor.Ui.Tests.KitWindowTests.acceptance: the Inverse constructor creates a study in its kit and runs its fit off the UI thread, telling why it ended`.
It shows the real InverseConstructorWindow over a fresh app context and opened
kit, presses Unknowns, names and creates Empty, presses Fit and Run joint, and
pumps dispatcher jobs until a nonempty error appears within ten seconds, without
sleeping. Assertions cover the required no-stage-run phrase, Empty's diagnostic
name, visible error, Empty.study-json both before and after fitting, and no
fitrun-json file. The window closes in finally, releasing its kit.

The YAML inherits constructor and smoke counts from supervisor checkpoint 417.
There is no ui_tests entry in the checkpoint registry; 16 is the previous
handoff's historical floor, not a measurement for this selection. Expected test
effects are +1 smoke, no constructor change and no non-smoke change. The
supervisor owns measured counts and checkpoint publication.

```yaml
gates:
  constructor_unit_tests: 1754
  ui_smoke_tests: 263
  ui_tests: 16
```

# Architecture

ConstructorScene.mount overrides the inert shell's studyFits with
StudyFitJobHost.onUiThread. Its two callbacks send StudyFitProgressed and
StudyFitEnded through StudyFit and the existing hostDispatch captured by the
Elmish init command. Both constructor windows share this composition; the
existing JobRunner worker, UI dispatcher posting, typed fit errors and
completion-time persistence remain the sole implementations.

The test reuses TestAppScope, controlById, pressBox and kitFilesIn. No alternate
fit host, storage seam, initializer, schema, durable shape or migration was
introduced. The other authorized consumer files require no signature adaptation.

# Deferred

No requested implementation is deferred. Supervisor execution of the frozen
gate roster remains required before acceptance.

# Gotchas

- The study-fit error control appears only after completion. Re-resolve it while
  pumping dispatcher jobs; do not retain a control from a previous render or sleep.
- FitStageErr adds a prefix and NoStageRunErr carries a diagnostic payload; the
  test checks the specified explanation and study name through the existing renderers.
- AGENTS.md has only the requested six-line addition. The three source edits
  have matching ordinary and ignore-CR-at-EOL numstats, with no line-ending churn.
- Preserve the pre-existing supervisor change to specs/0050/.manifest.state.json.
  No Git mutation, version bump, gate/census/checkpoint rewrite, analyzer exception,
  external-repository edit or notebook edit occurred.
- No runtime artifacts were produced. Worker documents use the task's exact paths.

# Changelog

- 2026-11-14: Wired constructor study fits to the real UI-dispatched background host, added the Empty-study acceptance fact and documented the Inverse study flow; handed verification to the supervisor.
