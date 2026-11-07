# Implementation plan

Implement only slice 0125 and obligation `S41-KIT-PROXY`: declare the three-field `KitProxy`, provide a shared `InMemoryKitFolder`, and author the five prescribed facts in an abstract `KitProxyContract` inherited by `KitProxyTests`. Reuse `KitFile`, `KitError`, `KitRelativePath`, `KitFileText` and the existing kit filename. Keep the metadata option, text map and folder set private to the in-memory folder; each proxy captures that same state without caching.

Add `OpticalConstructor.Domain/KitFolders.fs` immediately after `Kit.fs` in its project. Add `OpticalConstructor.Tests/KitProxyTests.fs` at the end of the test compile list. The tests use a deterministic stepping clock and an adapter that opens a new proxy over the same folder each time. They cover missing-kit read/write refusal, exact initial contents, preservation of existing paths, refusal to initialise twice and replacement visible through another proxy.

Keep production signatures elevated, conversions on the source type, documentation concise and spec-cited, and text files LF. Do not introduce disk IO, locks, additional proxy fields, dependencies or test identities. The contract registry already records `STORE_BigBoy_0004` as declared at step 125; preserve that entry and leave supervisor-owned state untouched.

The ADD_CONTRACT worker's invariant 6 prohibits running checks. The supervisor will execute the resolved build, elevation, constructor-unit-tests and eleven s15 invariant gates after handoff. Write the implementation log and state-of-the-world at the supplied paths, with inherited counts clearly distinguished from unmeasured results.
