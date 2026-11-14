# Step 0408 implementation plan

Implement the two `KitLibraryContext` promotion members in `StudyPublishing.fs`.
Reject read-only access before any IO, reuse step 0407's pure promoted values,
select the held entry's next version (or the first version), build and seal its
content with fit provenance, write through `putEntryVersion`, and snapshot the
explicit version through `snapshotReferences`. Add the three prescribed typed
publish errors without changing the opened study or its pinned material version.

Extend `StudyPublishingTests.fs` with exactly the three authorized facts and
widen the existing error-rendering fact. Use a fresh disposable library database
per fact and an initialized in-memory kit with an injected stepping clock. Reuse
the existing crystal, material study, fit runs and shipped black-box model.
Cover version preservation, provenance, snapshots and pins, repeated black-box
promotion, and read-only refusal before reads or writes.

Only those two source files and the three designated worker documents change.
Preserve the inherited supervisor manifest modification. No versions, gate
metadata, census or baselines change. The IMPLEMENT worker protocol assigns
build, tests, analyzers and invariant gates to the supervisor; record pending
verification accurately without executing gates or claiming a passing result.

Attempt 02 repairs the supervisor's measured build diagnostics in the existing
test file: replace the nonexistent `MaterialError.describe` call with the typed
save-result assertion already used in `StudyMessageTests`, and rename the local
`component` binding to `muellerComponent`. Preserve every test identity,
assertion and promotion contract. Update the handoff with the supplied failed
build evidence and leave fresh gate execution to the supervisor.
