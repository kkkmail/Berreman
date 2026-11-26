# Step 0077 implementation plan

Add the single prescribed LibraryProxyTests fact first. Use the existing
BuiltinCatalogue presets and Library.createInMemory to cover successful adds,
exact kind filtering, identity lookup, a missing identity and duplicate refusal
without changing the stored detector.

In ElementId.fs, add PresetRefusedErr and LibraryError.entryAlreadyHeld, preserving
the duplicate diagnostic text. Route the in-memory store through that member and
document the distinction between refusal and backing-store failure. Extend
CatalogueImport.fs's reason match so refusals still become ImportRejectedErr.
Also extend the exhaustive match in Tests/BuiltinCatalogue.fs: source inspection
found this additional consumer, and the slice authorizes complete-match repairs.
No persisted shape changes or migrations are involved.

Keep the existing supervisor-owned .manifest.state.json modification untouched.
Write the implementation log and state of the world at the supplied paths.
Per the IMPLEMENT worker's Invariant 6, do not run builds, tests, analyzers or
invariant gates; the supervisor executes the compiled roster after this worker
exits. Report no measured result or newly authored test baseline.
