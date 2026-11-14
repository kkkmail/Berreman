# Implementation plan — 0414

Implement the Inverse constructor's Unknowns flow step using the existing study commands, stored item-model resolution, parameter grid formatting and shared parameter editor. Add intent-based automation IDs and route the surface through the kit window. Keep Main's strip limited to Items, Setups and Experiments and preserve the existing bay when entering Unknowns.

Add the shared disk-backed study fixture in TestAppScope, the four authorized StudyViewTests, and the authorized scroll-reachability fact. Extend the existing flow/model/read-only tests without removing their assertions. Changes stay within the twelve source/project files authorized by the slice.

Risks are stale FuncUI event closures, read-only writes, parameter ordering and material-version identity. Reuse keyed action patterns and the existing study context rather than adding domain or storage behavior. Preserve shipped model versions and canonical storage APIs.

Per the IMPLEMENT worker protocol's Invariant 6, the supervisor owns build, test, elevation and invariant execution. This worker will record unexecuted verification explicitly and will not manufacture counts or gate results.

Attempt 02 repairs the supervisor's measured smoke failure: pressing Phone.darkOffset did not expose ParameterEditUnknownChoice. Trace the row's pointer delivery, scrolling and EditStudyParameter/editor projection; repair within the declared source/test paths and retain every existing assertion and test identity. Record the supplied failure separately from the supervisor's pending rerun. No worker gate execution is authorized.

Attempt 03 follows the repeated supervisor failure: the added immediate ParameterEditPanel assertion fails after pressing Phone.darkOffset, so completing layout alone did not repair the interaction. Inspect the existing row geometry, hit target, scrolling and message path, then repair the actual cause within the declared effects. Preserve all acceptance assertions and test identities; keep supervisor verification pending under IMPLEMENT Invariant 6.

Source inspection identifies the Unknowns scroller's default overlay scrollbars as an interception path for a row aligned to the viewport edge by BringIntoView. Reserve the scrollbar space in StudyUnknownsView and add a rendered hit-target assertion to pressStudyBox before it calls the shared pointer helper. Limit attempt 03 source edits to those two files. Runtime confirmation remains with the supervisor.
