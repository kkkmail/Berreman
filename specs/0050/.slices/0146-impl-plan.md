# Implementation plan

Implement S43-EXPERIMENT-FILE within the seven source/project paths authorized
by slice 0146. Add the elevated ExperimentFile envelope and ExperimentId file
stem in Domain; add the conditional versioned experiment schema and its cached
validator in Storage; expose canonical text and typed parsing through the
existing nodeOf, canonicalFileText and parseVersionedText stack.

Add exactly the four declared KitContentFileJsonTests identities, using local
E1 and E3 fixtures to cover naming, deterministic round-trips, typed unsupported
versions and rejection of invalid/malformed input. Append its compile entry and
place KitContentFiles immediately after KitFolderStore; copy the new schema
with PreserveNewest.

Preserve serializer ownership of descriptor/binding/range/wavelength payloads.
Unknown positive schema versions must reach the typed version check even when
the version-1 body is absent. Keep shared schema registration and existing
canonical formatting unchanged. No format migration, version bump, git
mutation, supervisor-record edit or work outside this slice is planned.

The IMPLEMENT role's Invariant 6 reserves all checks/gates for the supervisor.
This worker will write implementation and handoff records without running or
claiming gate results. The inherited baseline will be copied from the existing
supervisor checkpoint, never inferred from added source tests.

Attempt 02 repairs the supervisor-reported FS0597 errors in the two attachment
field-removal fixtures. Parenthesize the chained JSON indexer receiver before
calling AsObject, preserving all four test identities and every assertion.
Update the implementation log and state of the world with this repair; gate
execution remains with the supervisor under IMPLEMENT Invariant 6.
