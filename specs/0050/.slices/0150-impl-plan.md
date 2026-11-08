# Implementation plan

## Attempt 03 repair

Resolve the supervisor's two FS0041 errors at KitOpeningTests.fs:276-277 by
specifying CatalogueImportLogEntry list and HeldEntry list on Assert.Equal.
Keep both whole-list equality assertions and all six authorized test identities.
Preserve production code, compile entries, supervisor state and inherited counts.
Update the supplied log and state-of-world; executable verification remains
with the supervisor under IMPLEMENT Invariant 6.

The retry also reports FS3369 warnings in KitContentFileJsonTests.fs, which is
outside step 0150's four authorized edit paths. Record the exact scope conflict
without editing that file, suppressing warnings or claiming a warning-free build.

## Attempt 02 repair

The supervisor's build reported FS0010 and FS0583 at KitOpeningTests.fs:232.
Replace the malformed multiline nested OR pattern with one complete Error
pattern per MaterialError case, sharing the existing failure assertion. Keep
all six test identities and assertions, production behavior and compile entries.
Review the authorized source against its existing dependencies, then update the
implementation log and state-of-world. Gate execution remains the supervisor's
responsibility under IMPLEMENT Invariant 6.

## Original implementation

Implement S43-OPENING in the four authorized source/project files. Reuse the
existing kit open/initialise, content readers, snapshot decisions, overlay and
measured-data adapters. Add KitOpening.fs immediately before
TableAndElementRotationView.fs and append KitOpeningTests.fs to the test compile
list. Keep the six declared fact identities exactly as specified.

Write the six facts first using isolated folders under the test output directory
and fresh Built-in stores. Cover initialization, persisted content, foreign-lock
propagation, read-only non-mutation, cleanup after failed reads, all snapshot
decision rows (including reuse), overlay resolution and independent scratch kits.
Then implement the documented opening composition, system clock, folder picker
and scratch constructor without adding a cache, retry or new storage format.

The operator's manual 010 overrides scratchKit's infallible signature:
scratchKit returns Result<KitContext, KitError> through Storage's existing
createInMemory extension. Preserve typed failures and release an opened context
on any subsequent read/resolution/overlay failure; log a close failure without
replacing the original error. All snapshot rows remain pending confirmation.

IMPLEMENT Invariant 6 assigns all executable verification to the supervisor.
This worker will run no build, test, analyzer or invariant gate and will claim
no passing result. Preserve supervisor state, test census, checkpoints, version
rings and unrelated files; finish the required log and state-of-world handoff.
