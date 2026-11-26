# Where we are

Slice 0425 implements S121-STUDY-GAIN-MSG for spec 0050 S12.1. Studies now
expose session gain and fit-mode edits as semantic messages, and sessions
provide their allowed gain choices for the study view. All requested source
and both declared test facts are present; supervisor verification is pending.

# What's working

- Added SetSessionGainModel and SetFitMode through the existing study lookup and update members.
- Added free, anchored and interpolated gain offers from each session's Calibration runs in session order.
- Added persisted-message coverage with unchanged kit files after every refusal.
- Added gain-offer coverage for empty inputs, role filtering, session isolation and first/last ordering.

These describe delivered source, not measured runtime results.

# Tests

No build, test, analyzer or invariant command was executed. Invariant 6 of the
IMPLEMENT worker protocol assigns gate execution and verdicts to the supervisor
after worker exit. No worker development durations or deadline overruns exist
to report. commit_ready=false reflects pending verification.

The frozen roster requires build and arc-origin elevation (600 seconds each),
all constructor-unit-tests (180 seconds), kit-acceptance selected over
OpticalConstructor.Ui.Tests.KitAcceptanceTests (3600 seconds), and the twenty-one
s41 invariants (300 seconds each).

Exactly two test identities were added:

- OpticalConstructor.Tests.StudyMessageTests.setting a session's gain model and the study's fit mode are study messages, and a refused one records nothing
- OpticalConstructor.Tests.StudySessionTests.a session offers a free gain, a gain anchored to its AIR runs, and one interpolated between its first and last

The message fact uses the existing folder-kit fixture and recordStudy path. It
reads back the anchored model and StagedFit, checks the complete stored study,
and checks study contents, file paths and bytes after missing-study refusals
from both messages, a missing session and an anchor held in another session.
The offers fact covers zero, one, two and three Calibration runs; unrelated
Calibration runs; Science, Dark and Diagnostic roles; reversed session order;
and missing kit experiments. Existing facts and assertions remain intact.

The expected constructor count rises by two, from 1759 to 1761. Kit-acceptance
identities are unchanged. The YAML retains constructor 1759 from supervisor
checkpoint 422 and kit-acceptance 0 from the preceding 0424 handoff. These are
inherited floors, not measurements of this round; the supervisor owns measured
counts and checkpoint publication.

```yaml
gates:
  constructor_unit_tests: 1759
  kit_acceptance_tests: 0
```

# Architecture

StudyMsg.applyTo reuses studyNamed for UnknownStudyErr, withSessionGainModel
for the existing session and anchor refusals, and withFitMode for the mode
update. The existing StudyCommand already guards read-only kits and records
successful replies, so its behavior and shape remain unchanged.

StudySession.gainModelOffers uses session experiment order and CalibrationRun
membership. It always offers FreeSessionGain, adds AnchoredSessionGain when
at least one anchor exists, and adds InterpolatedSessionGain from the first
anchor to the last when at least two exist. No additional domain type,
setting, durable storage shape, serializer or migration was introduced.

# Deferred

No requested implementation is deferred. The supervisor must execute the
frozen gate roster before acceptance.

# Gotchas

- Kit order does not determine interpolation endpoints; session order does.
- A session without Calibration runs offers only free gain, even if another
  session has Calibration runs. Existing gain-validation behavior is preserved.
- List.last receives a nonempty tail because the empty and singleton anchor
  cases are handled first.
- All new docs cite S12.1. The source diff has no line-ending churn.
- The pre-existing specs/0050/.manifest.state.json edit belongs to the
  supervisor and was not changed by this worker.
- No Git mutation, version bump, external-repository write, lab/notebook edit,
  gate/oracle/census edit, analyzer exemption or checkpoint publication occurred.
- No runtime artifacts were produced. The supplied .artifacts folder remains
  the only permitted destination for persistent run output.

# Changelog

- 2026-11-15: Added session-gain and fit-mode messages, ordered gain offers and the two declared facts; handed verification to the supervisor.
