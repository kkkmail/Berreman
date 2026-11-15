# Implementation plan

Implement S121-STUDY-GAIN-MSG within Studies.fs and the two authorized test
files. Reuse Study.withSessionGainModel, Study.withFitMode, StudyMsg.applyTo's
study lookup, and the tests' existing folder-kit recording and byte snapshots.
The existing StudyCommand already guards writes and records successful replies.

Add exactly the two declared facts first. Cover persisted anchored gain and
staged mode, both messages' missing-study refusals, missing sessions and anchors
outside the session, with unchanged kit files after every refusal. Cover free,
anchored and interpolated offers in session order, excluding other roles and
other sessions even when the kit's experiment order differs.

Add the two message cases and their delegation, then StudySession.gainModelOffers
using only the session's Calibration runs, in its order. Preserve current
validation and storage shapes; add no window case or additional setting.

The IMPLEMENT worker protocol's Invariant 6 assigns all compilation, tests,
analyzers and invariant gates to the supervisor. Write the required handoff
without claiming gate results or measured counts. Preserve inherited baselines,
supervisor metadata and LF line endings. Main risks are accidentally taking kit
order for interpolation and allowing refused commands to reach recordStudy.
