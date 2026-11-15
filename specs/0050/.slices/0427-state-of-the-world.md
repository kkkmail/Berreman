# Where we are

Slice 0427, S121-AT1-STUDY, is blocked on a production dispatch defect outside
its authorized effects. The earlier attempts implemented the staged AT-1
builder and its acceptance fact in KitAcceptanceTests.fs. The supervisor
reports review green and successful build gates, but ObjectStage fails through
the real window RunFit path. Attempt 03 diagnosed that failure and preserved
all source, assertions and supervisor metadata. Only the three required
handoff documents were changed this attempt; commit_ready is false.

# What's working

- Preserved the message-driven staged builder, six gain rules and reference assertions.
- Traced the measured stage-two refusal to engineWith's one-unknown-matrix restriction.
- Identified why direct reconstructLinear coverage misses the automatic-dispatch defect.
- Documented the production owners and authority needed without weakening acceptance.

The existing supervisor failure occurs after calibration and the publication
revision checks. The stage-two matrix and product-check assertions have not
been reached successfully; the slice is not complete.

# Tests

No build, test, analyzer or invariant command was run by this worker. The
IMPLEMENT worker's Invariant 6 reserves gate execution to the supervisor.
The attempt-03 task reports review green and build gates passed, then
kit-acceptance exhausted its selective retries with the sole residual failure:

`OpticalConstructor.Ui.Tests.KitAcceptanceTests.AT-1's staged study calibrates the instruments on the AIR runs, publishes them and reconstructs QZ, LR and QZ then LR as the reference does`

The reported ObjectStage error is SessionGainNeedsLinearEngineErr for LP-LP's
required anchors LP-(AIR#0)-LP-2 and LP-(AIR#0)-LP-2R. The final retry had one
failure and zero passes in 16.711 s (test body 15.089 s). The supervisor's
manifest log records 86.3 s including retries. Both are below the unchanged
3,600-second kit-acceptance deadline. No timeout or worker runtime overrun is
involved. No passing result is claimed for this repair attempt.

Evidence: `.artifacts/0427-attempt-03-task.txt`, the supervisor's
`.manifest-arc-runner.log` entries at 2026-11-15T14:42:03Z, and the existing
OpticalConstructor.Ui.Tests/TestResults/kit-acceptance.trx. The task already
preserves the final retry diagnostic under the canonical artifact folder.

The following YAML is inherited unchanged from the previous handoff and
0426-state-of-the-world.md. It is not a new measured baseline; the supervisor
owns counts and checkpoints. Test identities, assertions, gate membership,
deadlines and acceptance remain unchanged.

```yaml
gates:
  constructor_unit_tests: 1761
  ui_smoke_tests: 264
  ui_tests: 9
  kit_acceptance_tests: 0
```

# Architecture

AT-1 uses 53 experiments, unknown QZ/LR matrices and the prescribed six session
gain rules. The ObjectStage includes individual QZ/LR observations and series
observations containing both unknown matrices. StagedStudy.engineWith in
StudyEngine.fs:338-356 requires at most one unknown-bearing matrix on every
fitted path. A series path therefore selects NonlinearStudyEngine.
StudyFitParam.runFit at line 1848 dispatches accordingly, and scaledProblemWith
at lines 425-438 refuses anchored/interpolated session gains. This explains the
exact error supplied by the supervisor without changing the fixture.

The existing series test at StudyEngineTests.fs:1887 calls reconstructLinear
directly, which separately solves [QZ], [LR] and [QZ; LR]. It bypasses
engineWith and runFit. Successful direct reconstruction therefore does not
establish that the window can run AT-1.

S113-ENGINE-CHOICE (step 0386) protects nonlinear classification when products
of unknown matrices are coupled in the fitted objective. Step 0390 separately
supports a reconstructed combined matrix as a diagnostic. S121-AT1-STUDY
(step 0427) requires these individual and diagnostic solves through automatic
dispatch with LinearStudyEngine. The classifier currently rejects both shapes.

# Deferred

No work is designated for a later slice. This slice is blocked and requires
an operator authority decision before its measured failure can be repaired.
Its effects authorize only KitAcceptanceTests.fs; the necessary production
and regression owners lie outside that file:

- OpticalConstructor.Optimization/StudyEngine.fs: distinguish the independent
  constituent solves and series diagnostic from a genuinely nonlinear product
  objective, preserving unresolved/coupled-series refusal (step 0386).
- OpticalConstructor.Tests/StudyEngineTests.fs: extend existing classification,
  series-reconstruction and runFit facts (steps 0386/0390/0391) to cover the
  automatic path with individual observations, combined diagnostics and the
  required gain models, retaining all existing assertions.

Authority must cover those production/test edits and their verification while
preserving S121-AT1-STUDY. This worker has not changed effects, the oracle,
test census or gate roster. A new test identity is not proposed.

# Gotchas

- A test-only engine override, synthetic completion, direct reconstructLinear
  call, free gain, fixed constituent or removed series run would evade the
  required semantic-message acceptance path. None was introduced.
- Merely removing the one-matrix guard would incorrectly classify unresolved
  or coupled products as linear. Existing negative classification assertions
  must survive any authorized repair.
- The previous two acceptance facts retain their pre-publication checkpoints
  and all assertions. The new fact and its reference tolerances are unchanged.
- The dirty KitAcceptanceTests.fs is inherited work from attempts 01/02.
  The existing manifest-state and expected-red edits belong to the supervisor
  and were preserved. No product, version, raw data, notebook or external
  repository was changed.
- No new runtime artifact was produced. Handoff text uses LF line endings.

# Changelog

- 2026-11-15: Implemented AT-1's staged study and saved-reference acceptance coverage in attempt 01.
- 2026-11-15 (attempt 02): Repaired the nested fit-host record's strict indentation.
- 2026-11-15 (attempt 03): Diagnosed the measured ObjectStage dispatch failure and recorded the out-of-scope production/test authority required; preserved acceptance and left commit_ready false.
