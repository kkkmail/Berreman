# Where we are

Slice 0424 implements S121-AT1-DATA, the experiment/data continuation of AT-1
after the items and setups from 0421 and manual split offers from 0422/0423.
All requested source is present in the sole authorized test file. Supervisor
compilation and gate execution remain pending.

# What's working

- Extended AT-1's message-driven builder with all seven final lab files and their 53 experiment groups.
- Applied nine Calibration roles, six Dark roles, two Diagnostic roles and the six gain sessions.
- Recorded the two LR setup corrections and the reference frame exclusion through semantic messages.
- Added the declared persisted-data fact, retaining every original AT-1 assertion and guarding lab bytes.

These describe delivered source, not measured runtime results.

# Tests

No build, test, analyzer or invariant command was executed by this worker.
The IMPLEMENT worker protocol's Invariant 6 assigns gate execution and verdicts
to the supervisor after worker exit. There are no worker development durations
or deadline overruns to report. commit_ready=false reflects pending verification,
not missing implementation.

The frozen roster requires build and arc-origin elevation (600 seconds each),
all ui-smoke (600 seconds), ui-tests selected over StudyViewTests and DataStepTests
(360 seconds), kit-acceptance selected over KitAcceptanceTests (3600 seconds),
and the twenty-one s41 invariants (300 seconds each).

The sole new identity is:

- OpticalConstructor.Ui.Tests.KitAcceptanceTests.AT-1 attaches every lab group of the final data files through the Phone ROI CSV profile, with its role, session, correction and exclusion

It reads the built folder through a second KitProxy and asserts 53 experiments
in file/group order with sequential identities, their complete scan, setup
revision, role, session, notes, correction audit, attachment path/hash/profile,
keyed selection and exclusion. The window's experimentData proxy must return
every source capture, including the excluded frame; keptBy must remove exactly
the reference's frame. It also checks all descriptions lie on the 19-point raw
dial sweep and validate as intensity data. Item, setup, library-snapshot and lab
byte assertions are reused; before/after inventories and SHA-256 maps guard both
the copied and committed data trees.

The acceptance class is expected to rise from one fact to two. Existing selected
UI and smoke identities are unchanged. The YAML preserves the prior handoff's
floors (0423), including its unmeasured acceptance floor; smoke 264 is also in
supervisor checkpoint 422. These are inherited values, not measurements of this
round. The supervisor owns measured counts and checkpoint publication.

```yaml
gates:
  ui_smoke_tests: 264
  ui_tests: 9
  kit_acceptance_tests: 0
```

# Architecture

The setup constructor now accepts a continuation inside its existing failure
cleanup. buildOpticsMuellerKit continues with the data messages; the original
fact uses the setup-only checkpoint, preserving even its empty-experiments
assertion. No construction or cleanup body is copied.

Read-only expectations use the existing Phone ROI profile/parser and naming
template. Writes use ActivateSetup, ExpChooseElement/Variable/Measurement,
SplitByColumn, ToggleSplitOffer, OfferSplitGroupOver, MatchByTemplate,
SetExperimentRole/Session, CorrectSetup, ReviewExperimentData and ExcludePoint.
No direct experiment or attachment writer is called by the builder. All readable
split offers must reuse an existing grid setup; the eight unreadable keys use
the specified family AIR setups. The restart boundary is the earliest capture
in BullshitCheck, read from the lab data. The exclusion's mapped point number
comes from its one-based place within its key's captures in file order.

There is no product behavior, serializer, durable shape or migration change.

# Deferred

No requested implementation is deferred. The supervisor must execute the frozen
gate roster before acceptance.

# Gotchas

- Preserve the original setup-only fact and its assertions; its builder call
  now selects the shared setup-only checkpoint.
- Only the two specified day-1 +90 runs are corrected. The original lab's
  images_final_export_task.md confirms the final +45 runs were remeasured
  correctly. The correction's setup name is -90; the existing setup stores its
  equivalent normalized 270-degree angle.
- Capture index 17 is not assumed to be mapped point number 17. The reference's
  label, description and capture index locate the frame within its group.
- Test kits stay under the test output and are closed/deleted by the existing
  scoped cleanup, including a failure in the data continuation.
- The pre-existing specs/0050/.manifest.state.json edit belongs to the supervisor.
  No Git mutation, version bump, external-repo write, notebook or lab-file edit,
  gate/oracle/census edit, analyzer exemption or checkpoint publication occurred.
- No runtime artifacts were produced. The supplied .artifacts folder remains
  the only permitted location for any persistent run output.

# Changelog

- 2026-11-15: Added AT-1's 53-group data construction and persisted acceptance fact; handed all verification to the supervisor.
