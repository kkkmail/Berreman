# Step 0407 implementation plan

Implement S11.5's pure library-promotion values in `StudyPublishing.fs`: preserve
item identity and provenance, reconstruct opened material crystals from the fit
snapshot, evaluate fitted black-box matrices through the existing bound-form
pipeline, reject scoped/unfitted values, and enumerate promotion subjects.

Extend `StudyPublishingTests.fs` with exactly the five authorized facts and widen
the existing error-rendering fact. Reuse shipped model versions and existing
crystal axes and tolerance helpers. Preserve every existing assertion.

The main risks are using today's item/material state instead of `run.parameters`,
silently accepting per-session/per-run fitted values, and changing matrix
normalization or orientation. Cover those within the authorized test identities.
Only the two authorized F# files and the three designated worker documents change.

Per the IMPLEMENT worker's Invariant 6, the supervisor owns all gate execution.
Do not run checks or report gate verdicts; hand off the implementation with the
inherited measured constructor baseline and pending verification clearly stated.

## Attempt 02 repair

The supervisor build reported FS0039 for `ElementValueId` at the new item
conversion. Its existing declaration belongs to `OpticalConstructor.Domain.Placement`,
which this file does not open. Add that import, as `ElementId.fs` already does,
without changing the conversion, promotion behavior, or authorized test identities.
Update the designated handoff documents with the supplied failure and repair;
gate execution remains with the supervisor under Invariant 6.
