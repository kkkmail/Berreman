# Step 0409 implementation plan

Add the last `StudyMsg` case, `SetOpenedMaterialParameter`, and dispatch it
through the existing study lookup and `Study.withMaterialParameterState`.
Keep validation and errors in the existing pure update; recording stays with
the caller through `recordStudy`. Document the added contract with spec 0050
S11.5. No storage shape or migration changes are needed.

Add exactly the authorized fact to `StudyMessageTests.fs`, reusing its
in-memory kit, absorbing material, message sender and file-byte snapshot
helper. Exercise persisted n1 freedom at 1.70 within [1.60, 1.80], preservation
of the other known components, and refusal without any file changes for an
unopened version and reversed bounds. Cover missing-study and missing-parameter
error forwarding within the same fact.

Only `Studies.fs`, `StudyMessageTests.fs` and the three designated worker
documents are changed. Preserve the inherited supervisor manifest change and
the frozen test census, gates, counts and oracle. The prior measured constructor
count is 1738 at checkpoint 407; one added fact implies an expected 1739.

The IMPLEMENT worker protocol's Invariant 6 assigns all executable verification
to the supervisor. Review the source changes and LF endings, then write the
handoff with gates pending and no claimed test result.
