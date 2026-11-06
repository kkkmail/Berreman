# Step 0069 state of the world

## Where we are

Slice 0069 implements STORE_XDUO_0001's SQLite MaterialProxy in spec 0050 S2.2.
The adapter, typed storage failures, consumer matches and SQLite contract suite
are complete and were committed by the operator in `308fead`. Operator recovery
verification passed all 15 unchanged gates; normal supervisor review, acceptance
and checkpoint 68 remain outstanding. Follow `.manual/007-inherited-registry-recovery.md`.
Product composition remains unchanged as required by this slice.

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

2026-11-06 operator recovery: all 15 unchanged gates passed. Constructor:
835 cases; UI smoke: 208; selected MaterialsWindowTests: 35. Zero failures or
skips, with exact census checks. This verifies the required 24-case increase
from checkpoint 67's 811. Evidence is in `.artifacts/0069-operator-repair/`.
Supplemental elevation against accepted 0068 (`38c6d3c`) checked all seven
affected/dependent projects and reported zero new findings.

These are fresh recovery measurements, not supervisor acceptance or a new
checkpoint. Checkpoint 67 remains 811 and checkpoint 68 is absent. The selected
35 UI cases must not become a whole-suite baseline. The supervisor still runs
its normal review, gate and measured checkpoint cycle before advancing.

```yaml
gates:
  constructor_unit_tests: 835
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

## Recovery handoff

The ASG compiler now prepares planned inherited lifecycle rows before publishing
the receipt; the receipt validates and all 988 compiled artifacts are unchanged.
The existing implemented MaterialProxy row and every earlier registry entry
are preserved; the remaining inherited rows for 0071/0074/0095 are initialized.
The ASG escalation event/watcher mismatch is repaired as well.

Wait for the operator to commit the repair and explicitly authorize relaunch.
Then resume 0069 normally with an operator note and `--executability-profile 1`,
without `--accept-current-slice`, as `.manual/007` requires even though the
implementation is already committed. Preserve it and review it against
accepted 0068 (`38c6d3c`). Do not restore or repeat the implementation, alter
published contracts, waive gates or manufacture checkpoint 68.

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
