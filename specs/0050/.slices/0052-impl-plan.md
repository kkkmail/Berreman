# Implementation plan — 0052

Re-point the nine constructor test files named by slice 0052 to the existing
`BuiltinCatalogue` fixture. Replace seed lists, individual samples and sources,
material libraries, and self-seeding stores with the specified fixture members.
Keep every fact name, assertion, count, and numeric expectation. Select entries
by their well-known ids wherever the old code selected seed-list positions.

Files: `SampleProxyTests.fs`, `PropagationTests.fs`, `ExperimentProxyTests.fs`,
`ExperimentCollectionRoundTripTests.fs`, `ExperimentRunTests.fs`,
`PlacedSampleEmissionTests.fs`, `LibraryProxyTests.fs`, `SourceLibraryTests.fs`,
and `PolarizerBehaviorTests.fs`, all under `OpticalConstructor.Tests`.
Update seed/store documentation in those files without adding narrative.

The main risk is ordering: the derived Sources tree groups fixed lines before
tunable sources, so compare its recursively collected ids order-independently.
Preserve ordered comparisons where the fixture and product use the same order.
No product code, fixture members, facts, packages, or version changes are needed.

The IMPLEMENT worker protocol's Invariant 6 assigns all checks to the supervisor.
Do not run build, tests, analyzers, acceptance greps, or invariant gates locally;
record their pending status in the implementation log and state-of-the-world.
