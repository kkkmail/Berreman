# Step 0075 implementation plan

Extract the two user-source store facts from SourceLibraryTests into the abstract
SourceProxyContract, whose freshSources factory supplies an empty user store.
Keep SourceLibraryTests as the concrete in-memory subclass and retain the seven
catalogue, identity and live-library facts there with their existing names and
assertions. Follow the material/sample contract pattern already in this suite.

Move the fixture helpers into a private module above both classes. Change
userSource to accept ElementValueId directly and use newSourceId for the source
saved in the insert/replace/remove fact. Its lookups and removals use that
source's id. Keep literal ids elevated at their other fixture call sites.

Only SourceLibraryTests.fs changes among source files; its compile position and
all store implementations stay intact. Preserve all nine concrete test identities
and the existing suite count. Write the required log and state-of-the-world at
the supplied paths. Do not touch the supervisor's existing manifest-state edit.

The principal risk is xUnit discovery of inherited facts. Local development
verification will build the affected test project and run the constructor suite
to inspect the concrete identities. The development environment requires this
verification; formal gate execution, results and commit readiness remain with
the supervisor. Capture any local output only under the supplied artifacts folder.
