# Implementation plan — 0113

Implement S33-ATTRIBUTION in the three authorized source files. In Domain's
`LibraryEntryFiles.fs`, add the elevated attribution union and display line,
the single case-insensitive CC-BY prefix rule, recursive provenance lookup,
and the library entry's own identity followed by its sample material identities.
These are derived presentation values; no stored format changes.

Extend the shared `BuiltinCatalogue.fs` fixture using its existing catalogue
decoders and content builders. Add licensed/user material, user category,
single-layer sample, and ordered seal-and-put helpers. Add exactly the three
declared facts in `LibraryEntryFilesTests.fs`, covering attribution rendering,
fork ancestry, all non-licensed provenance cases, and identity ordering.

Preserve existing catalogue sources, version pins, supervisor state, and all
eleven section invariants. Pay particular attention to canonical/non-GUID ids,
set ordering, lifted fixture values, and the shared UI-suite compilation of
BuiltinCatalogue. Record local verification without modifying supervisor
checkpoints. The session's higher-priority instruction to run appropriate
tests and required checks overrides the task-file act-only wording; formal
gate acceptance remains the arc-runner's responsibility.
