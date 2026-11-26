# Implementation plan — 0414

## Normal resume after the operator's commits

Preserve the Unknowns implementation in `a8678177` and the render-until-hit repair in `c92d8b87`. Review their source against the slice and `.manual/028-step-0414-operator-reply.md`, including the existing test identities, real pointer input, reserved scrollbar space and read-only behavior. Change product or test code only if that review identifies an unmet obligation.

The active supervisor has already launched with `--executability-profile 1` and the operator reply, and dispatched this IMPLEMENT worker at index 413. Return the required handoff to that process for normal review and the unchanged 25 gates; do not launch a second supervisor. IMPLEMENT Invariant 6 assigns gate execution and measured checkpoint publication to the supervisor. Preserve the immutable arc origin, historical floors and all supervisor metadata; do not claim a measured pass or publish checkpoint 413 from this worker.

Update only the three assigned worker documents unless the source review finds a required repair. Preserve the earlier attempt history below and distinguish its superseded single-tick diagnosis from the operator's reproduced stale-frame defect and committed bounded wait.

## Original implementation and repair plan

Implement the Inverse constructor's Unknowns flow step using the existing study commands, stored item-model resolution, parameter grid formatting and shared parameter editor. Add intent-based automation IDs and route the surface through the kit window. Keep Main's strip limited to Items, Setups and Experiments and preserve the existing bay when entering Unknowns.

Add the shared disk-backed study fixture in TestAppScope, the four authorized StudyViewTests, and the authorized scroll-reachability fact. Extend the existing flow/model/read-only tests without removing their assertions. Changes stay within the twelve source/project files authorized by the slice.

Risks are stale FuncUI event closures, read-only writes, parameter ordering and material-version identity. Reuse keyed action patterns and the existing study context rather than adding domain or storage behavior. Preserve shipped model versions and canonical storage APIs.

Per the IMPLEMENT worker protocol's Invariant 6, the supervisor owns build, test, elevation and invariant execution. This worker will record unexecuted verification explicitly and will not manufacture counts or gate results.

Attempt 02 repairs the supervisor's measured smoke failure: pressing Phone.darkOffset did not expose ParameterEditUnknownChoice. Trace the row's pointer delivery, scrolling and EditStudyParameter/editor projection; repair within the declared source/test paths and retain every existing assertion and test identity. Record the supplied failure separately from the supervisor's pending rerun. No worker gate execution is authorized.

Attempt 03 follows the repeated supervisor failure: the added immediate ParameterEditPanel assertion fails after pressing Phone.darkOffset, so completing layout alone did not repair the interaction. Inspect the existing row geometry, hit target, scrolling and message path, then repair the actual cause within the declared effects. Preserve all acceptance assertions and test identities; keep supervisor verification pending under IMPLEMENT Invariant 6.

Source inspection identifies the Unknowns scroller's default overlay scrollbars as an interception path for a row aligned to the viewport edge by BringIntoView. Reserve the scrollbar space in StudyUnknownsView and add a rendered hit-target assertion to pressStudyBox before it calls the shared pointer helper. Limit attempt 03 source edits to those two files. Runtime confirmation remains with the supervisor.
