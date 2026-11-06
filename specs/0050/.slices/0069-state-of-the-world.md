# Step 0069 state of the world

## Where we are

Slice 0069 implements STORE_XDUO_0001's SQLite MaterialProxy in spec 0050 S2.2.
The adapter, typed storage failures, consumer matches and SQLite contract suite
are complete. Acceptance execution and measured checkpoints remain with the
arc-runner. Product composition remains unchanged as required by this slice.

## What's working

- Implemented SQLite material reads, search, versioned saves and lifecycle edits.
- Preserved provenance, one-way pins and sample-reference-before-pin removal refusals.
- Mapped database and decoder failures to MaterialStoreUnavailableErr.
- Added all 24 SQLite material cases while preserving existing contract assertions.
- Recorded the inherited MaterialProxy contract's implementation at step 69.

## Tests

The affected OpticalConstructor.Tests project and its Domain, Storage and Ui
dependencies built locally in Release x64 with zero warnings and zero errors.
All 49 selected material cases passed (25 existing in-memory cases and 24 new
SQLite cases), with no failures or skips. The new cases include all 22 inherited
contract facts plus provenance preservation and typed failure from every proxy
field over a disposed database. Logs and the TRX are under
specs/0050/.artifacts/0069-local-*.

Local development verification follows the higher-priority coding instructions
and is separate from acceptance. The supervisor must execute build, elevation,
constructor-unit-tests, ui-smoke, the selected MaterialsWindowTests and the ten
s09 invariants from 0069.gates. No acceptance result or commit readiness is claimed.

The following are inherited supervisor baselines from checkpoints 67 and 64,
respectively, not new measurements. The full constructor suite is expected to
gain 24 cases; this worker has not changed the checkpoints or invented a ui-tests
baseline.

```yaml
gates:
  constructor_unit_tests: 811
  ui_smoke_tests: 208
```

## Architecture

MaterialProxy.createInDatabase is a StorageBoundary type extension in
MaterialDatabaseStore.fs, compiled immediately after LibraryDatabaseStore.fs.
Its functions close over one LibraryDatabase, each operation performs one run,
and successful writes end with one SaveChanges. Entry codecs, relational staging,
versionPayload, decideVersioning, byQuery and MaterialError refusal members are
reused. No cache, extra connection, retry or alternative serializer was added.

Material identity follows the latest row's kind. Listings sort material ids and
honor latest lifecycle; direct resolution remains available for inactive
versions. New saves use UserProvenance, edits retain the stored provenance, and
physics edits of pinned versions mint an active, unpinned successor. Removal
queries active/latest sample-material relations at any material version,
decodes referencing sample names, then checks all material versions for pins.

The only domain shape addition is MaterialStoreUnavailableErr. All complete
matches were updated, including the BuiltinCatalogue fixture. No durable shape
or schema changed, so there is no migration or compatibility path. The inherited
contract registry entry records implementStep 69 with no local declaration.

## Deferred

No implementation work is deferred. Acceptance execution is assigned to the
supervisor by the worker protocol.

## Gotchas

- freshProxy removes samples from its own fresh catalogue database to preserve
  the existing reference-free factory semantics. composedProxies keeps them and
  exposes live reads/removals. Otherwise the pin-only glass152 contract fact
  would receive the correctly higher-priority sample-reference refusal.
- Every database opened by the SQLite test class is disposed by its IDisposable
  implementation, including additional factories used by the equality fact.
- The first local build encountered two pre-existing SYSLIB0051 warnings in
  vendored MathNetNumerics/Numerics/Exceptions.cs. The final build had zero
  warnings. No vendored source was changed.
- The supervisor's pre-existing .manifest.state.json edit was left untouched.
- No gate configuration, analyzer baseline, version ring or manual note changed.
- Source files and handoff documents use LF; all run artifacts are in the
  authoritative per-arc .artifacts folder.

## Changelog

- 2026-11-06: Implemented the SQLite material proxy and typed failure handling;
  added its 24 contract cases; the affected project built and 49 material cases
  passed locally.
