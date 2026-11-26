# Step 0064 implementation plan

Implement S21-RELATIONAL-KEYS in LibraryDatabaseStore.fs with two
StorageBoundary members on LibraryEntryEnvelope. Decode by the declared entry
kind, reuse LibraryFacets over empty category/material lists, retain distinct
discrete key/value pairs, and map decoder failures to LibraryRowInvalidErr.
Sample material rows reuse SampleStructure.referencedMaterialVersions and the
envelope's identity/version. No schema, index, serializer or durable format
changes are needed.

Add the four named LibraryDatabaseTests facts. Pin the specified built-in
facets and material references independently of the projection, verify row
identity/version and uniqueness, cover decoder refusal, and persist every
built-in through one in-memory database write. Read back in fresh contexts and
delete the multilayer version to exercise the existing database cascades.

Only the two authorized source files and the supplied worker output paths are
edited. Preserve the supervisor's existing manifest-state change. Risks are
accidentally persisting numeric or material-derived sample facets, storing
category display names, losing material versions, and comparing EF entities by
reference instead of their column values. The supervisor retains acceptance
gate ownership; record verification evidence without inventing baselines.
