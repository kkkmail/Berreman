# Implementation plan

Implement only step 0418: append Publish to the inverse flow, add its stable IDs, and mount a scrollable StudyPublishView after Results. Reuse the study bar, selectedFitRunOpt, calibratedItemsWith, promotionSubjectsWith and the existing PublishCalibration / PromoteToLibrary messages. Preserve read-only controls, item revision history and library provenance.

Extend the authorized existing flow and read-only assertions. Add exactly the two StudyViewTests identities and one ScrollExtentTests identity declared by the slice, using shipped item-model versions and stored fit runs. Keep fixture support within the authorized source paths and reuse the existing UI test fixture infrastructure.

Risks: retained action handlers need keys tied to the selected run; preview parameters must be the parameters this publication changes; small windows must reach both calibration and promotion actions; UI tests cannot use fixtures compiled only in the constructor test executable.

The IMPLEMENT worker protocol reserves all builds, tests and gates for the supervisor. Perform source inspection and edits only, then write the assigned implementation log and state-of-the-world with verification explicitly pending. Do not mutate Git, gate metadata, baselines, versions or external repositories.
