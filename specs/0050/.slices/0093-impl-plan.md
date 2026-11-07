# Implementation plan

Implement S31-DECISION-TABLE within the five authorized source/project paths. Reuse the existing library envelopes, elevated identities, version/lifecycle/provenance types, sample reference projection and Storage sealing member. Add the eight specified ImportDecision facts before the implementation and the ninth reference fact in LibraryEntryFilesTests; register the new module immediately after CatalogueImportInMemoryTests.

Add LibraryEntryContent.referencedEntryIds and the specified decision data and members before CatalogueImportProxy. Preserve precedence (kind, version/hash, then name), move latest only for a newer protected built-in arriving from its own catalogue, and retain every row in split.awaiting. Keep captions separate from sealed content. No store, caller or durable format changes are needed.

Risks are version-history lookup versus latest-only lookup, incorrectly auto-applying reuse or independent rows, and accidentally changing a collision's name/hash. The nine facts cover those decisions and reference dependencies. The S3.1 brief's older auto-apply wording is superseded by .spec-md §3.7 and this slice's explicit all-rows confirmation requirement.

The workspace's higher-priority testing requirement calls for local verification despite the worker document's act-only restriction. Run the resolved Release build, scoped elevation, constructor suite and eleven invariants; capture persistent output only in the supplied .artifacts folder. Supervisor acceptance remains separate; do not mutate checkpoints or Git state.
