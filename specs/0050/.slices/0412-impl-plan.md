# Step 0412 implementation plan

Connect the constructor window to the existing StudyFitJob state machine. Add the inert host, shell field, model state, forward context and StudyFit message. Guard RunFit through guardKitWrite; let the existing environment own execution and persistence. Append and select a completed run only after the returned state confirms successful commitment without an error.

Extend StudyWindowModelTests with exactly the three declared facts. Reuse its real-kit/catalogue fixture and FitRunFileFixtures. A recording host covers one start, duplicate suppression, cancellation and read-only refusal. Completion covers storage, immediate history/selection and duplicate-name refusal. A posting host runs the empty study, recording messages until ManualResetEventSlim signals completion within ten seconds.

Only StudyFitJob.fs, TableAndElementRotationView.fs and StudyWindowModelTests.fs are product/test edits. Preserve stored model-version resolution, all existing tests, LF endings and the pre-existing supervisor state modification. No durable format changes or migration.

The IMPLEMENT worker protocol's Invariant 6 assigns all executable gates to the supervisor. This round writes implementation and handoff documents without executing gates or claiming results. Carry existing measured baselines from supervisor evidence; the three additions are an expectation, not a measured count.
