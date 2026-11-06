# Step 0069 implementation plan

## Authorized recovery resume — 2026-11-06

The operator committed the implementation in `308fead`, the recovery records in
`a3e6c47`, and the ASG repair in `76e953f3`, then authorized this normal profile-1
worker resume. Preserve those changes. Review the product diff against accepted
0068 (`38c6d3c5d5c86cef040f069108b06062756ba96b`), the immutable slice obligation,
and the existing recovery evidence. Do not repeat the implementation.

Refresh this plan, the implementation log and the state-of-the-world handoff.
Preserve all lifecycle and origin registries, published requirements, compiled
artifacts, expected-red records and checkpoint evidence. The worker-role
protocol assigns normal critic review, gate execution and measured checkpoint
68 to the supervisor after this worker exits. The recovery ruling requires that
cycle before advancement; it does not authorize a worker-authored checkpoint.
Checkpoint 67 remains 811 constructor cases, and the selected 35 UI cases must
never replace the whole-suite UI baseline.

## Original implementation plan

Implement STORE_XDUO_0001's SQLite material adapter in MaterialDatabaseStore.fs,
compiled immediately after LibraryDatabaseStore.fs. Reuse the entry builders,
decoders, database staging operations, version payload, versioning decision and
material refusal members. Each operation uses one database.run; successful writes
finish with one SaveChanges. Preserve latest-material identity, lifecycle,
provenance, pinning and sample-reference-before-pin removal refusal ordering.

Add MaterialStoreUnavailableErr and cover it in all exhaustive consumers,
including the BuiltinCatalogue test fixture. Add MaterialProxySqliteTests in the
existing test file, inherit all 22 contract facts, and add the two specified
provenance/disposed-database facts. Track and dispose every fixture database.

The freshProxy fixture must match the existing reference-free in-memory factory:
remove sample entries from its fresh catalogue database. composedProxies retains
the samples and uses live database reads/removals. This preserves the existing
contract assertions, including the pin-only removal refusal for glass152.

Record the inherited contract's implementation in this arc's lifecycle registry
without changing its identity or earlier arc history. Write the required handoff
documents. Acceptance-gate execution remains with the supervisor; local
development verification required by the coding instructions is recorded
separately. No production composition, schema, migration, version bump or analyzer
configuration changes are planned.
