# Implementation plan

Implement the six spec 0050 S4.3 KitProxy content members in KitContentFiles.fs
over the existing per-kind verbs and canonical file adapters. Preserve listing
and mutation failures and the individually requested missing-scene error. Map
per-file read and serialization/schema failures to filename-bearing
KitFileRejectedErr values, reject experiment identity/stem mismatches, and
return experiments by id and scene names in ordinal order. Do not add a cache,
retry, serializer, proxy field or unrelated member.

Repoint SceneRoundTripTests.fs and ExperimentCollectionRoundTripTests.fs onto
fresh kit folders beneath AppContext.BaseDirectory, initialized with test-local
KitFile values. Preserve their fixtures and applicable assertions, implement
exactly the authorized test renames/deletions/additions, and cover replacement,
missing files, whole-read rejection and read-only byte preservation. Extend
those authorized facts for ordinal ordering, stem collisions and case-insensitive
experiment identity matching where useful.

Only those three product/test files and the supplied plan, log and state-of-world
paths change. Preserve the supervisor-owned manifest-state modification. Gate
execution belongs to the supervisor under IMPLEMENT Invariant 6; no executable
checks or gate results will be produced by this worker. Read the inherited
checkpoint count without publishing a new baseline. Main risks are error-channel
mapping, filename-versus-payload identity, and preserving the existing behavioral
coverage while changing persistence seams.
