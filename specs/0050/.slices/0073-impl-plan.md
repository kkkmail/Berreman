# Step 0073 implementation plan

Extract the 21 samples-store facts into abstract SampleProxyContract in the
existing SampleProxyTests.fs. Expose freshProxy and move proxyWithPinned onto the
contract so every store fact uses the adapter supplied by its subclass. Move the
nine shared fixtures unchanged into a private module above both classes.

Keep SampleProxyTests as the concrete in-memory class using
BuiltinCatalogue.sampleStore, with the nine non-store facts declared directly
on it. Preserve every fact name and assertion, and keep the file's compile
position. No product, project, schema, package or store implementation changes
are needed.

The risks are altered assertions during the move and inherited xUnit discovery
changing concrete test identities or counts. Compare source bodies and use local
development verification of the affected test project and constructor suite,
as required by the development environment. Acceptance gates and measured
checkpoints remain supervisor-owned; do not assert gate results or author a new
baseline.
