# Implementation plan — step 0137

Implement S42-REFERENCES as pure domain data. Add LibraryVersionRef and
EntryReference immediately before LibraryEntryContent, then derive content
references from material categories and the existing sample material-version
set. Project referencedEntryIds from those references and expose an envelope's
own version reference. Reuse the existing id conversions in Lifecycle.VersionRef
and Experiments.ElementBinding, and retain setup order while removing duplicate
references in Experiment.libraryReferences.

Add exactly the two authorized facts in LibraryEntryFilesTests.fs and
ExperimentProxyTests.fs before production edits. Cover the shipped Silicon,
quarter-wave sample and source, exact/latest reference rendering, envelope
identity, and ordered experiment bindings with duplicates and an unbound element.
Keep the existing entry-id reference fact unchanged. Only the five source files
listed in slice 0137 and the three supplied worker-output paths will be edited.

Risks are preserving material-version set order, distinguishing versions of one
entry, ignoring non-GUID preset ids through the existing conversion, and avoiding
new reverse conversions or public members beyond the slice. No stored shape or
schema changes are needed. IMPLEMENT invariant 6 assigns all executable
verification to the supervisor; the worker will not run or claim gate results.
