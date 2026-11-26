# Implementation plan

## Attempt 03 repair

Replace the UI fixtures' unavailable `FitRunFileFixtures` dependency with the UI suite's existing `StudyFitViewFixture.fitViewRunOf` constructor. The constructor-only fixture lives alongside facts in `KitContentFileJsonTests.fs` and is neither referenced nor file-linked by the UI project. Construct the rich Results payload locally in StudyViewTests.fs, retaining every asserted value, and construct the 16-by-16 correlation problem directly in ScrollExtentTests.fs. Reuse the existing empty-history constructor in both places, keep all test identities and assertions, and add concrete record types to avoid ambiguous field inference. No project-reference, census or production change is required. Gate execution remains supervisor-owned under IMPLEMENT Invariant 6.

## Attempt 02 repair

Repair the three nested record updates identified by the supervisor's build diagnostics in StudyWindowModelTests.fs, StudyViewTests.fs and ScrollExtentTests.fs. Put each outer record's `with` and nested field on separate lines with conforming F# indentation. Preserve the existing test identities, assertions and fixtures. Review the surrounding authorized Results implementation for related source defects, then update the assigned log and state-of-the-world with the repair and the supervisor-owned verification status. Do not run gates under IMPLEMENT Invariant 6.

Implement spec 0050 S11.5's Results step as a projection of the selected study's stored fit runs. Extend the existing flow and study surface with Results and its six tab choices, expose run selection and owner/entry captions on the model, centralize all requested automation IDs, and mount the new view immediately after StudyFitView in compilation order.

Reuse the stored FitRunResults shapes, the existing study bar and action buttons, item-label resolution, fit-run file fixtures and semantic headless UI helpers. Display every requested result and empty state without invoking a fit or changing storage. Keep numeric formatting invariant and the whole surface scrollable in both directions.

Extend only the authorized model, ID, flow, Results rendering, smoke and reachability test identities. Add one constructor fact, one smoke fact and two non-smoke UI facts, retaining all existing assertions. Source changes stay within the ten authorized files. Preserve supervisor metadata, published contracts, test census, version locks and reference data.

Risks are latest-run fallback, resolving all owner kinds, handler identity during tab changes, and reaching a wide 16-by-16 correlation grid in small windows. Record implementation and verification state at the task's exact handoff paths; the worker protocol assigns acceptance gate execution to the supervisor.
