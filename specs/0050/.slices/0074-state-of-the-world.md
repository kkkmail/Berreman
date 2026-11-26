# Step 0074 state of the world

## Where we are

Implemented STORE_XDUO_0002 for S2.3: SampleProxy now has a SQLite adapter in
Storage using the library database's existing entry/version operations. The
in-memory adapter and its shared contract remain intact. Application composition
is unchanged, as required by this slice. Supervisor acceptance is pending.

## What's working

- Implemented SQLite sample listing, search, lookup and versioned saves.
- Preserved provenance, per-version pins and lifecycle-aware visibility.
- Reused material-reference staging and cascading removal with pin refusals.
- Mapped storage and decoder failures to SampleStoreUnavailableErr.
- Added all 23 SQLite contract cases and preserved the existing sample tests.
- Marked the inherited SampleProxy contract implemented at step 74.

## Tests

The affected test project and its Domain, Storage and Ui dependencies built in
Release x64. The final build had zero warnings and errors. The initial build
reported the same two SYSLIB0051 warnings in vendored MathNetNumerics/Exceptions.cs
recorded by preceding slices; no warnings originated in our code.

The final local constructor run passed all 868 tests, zero failed or skipped,
matching the prior 845-case checkpoint plus the declared 23 cases. TRX review
confirmed exactly the 23 new identities and no abstract-contract discovery.
The existing 30 in-memory test bodies and fixtures are unchanged. An initial
run passed 867 cases and rejected an added fixture that combined ThickPlate
with a thin-film structure; correcting its substrate kind resolved that failure
without altering production validation or removing assertions.

Local logs and TRX reports are under specs/0050/.artifacts/0074-local-*, with
identity/source review in 0074-source-discovery-review.log. No acceptance-gate
results are asserted. The supervisor must execute the resolved build, elevation,
constructor-unit-tests, ui-smoke, selected ui-tests and eleven s10 invariants.
Commit readiness remains unasserted until that execution.

These counts are inherited supervisor checkpoints (constructor step 72,
ui-smoke step 70), not newly authored baselines. There is no recorded ui_tests
checkpoint to invent; the selected UI suite remains supervisor-owned.

```yaml
gates:
  constructor_unit_tests: 845
  ui_smoke_tests: 208
```

## Architecture

SampleProxy.createInDatabase captures one LibraryDatabase. Each valid store
operation uses one database.run boundary, and each write commits once after
staging. Validation, SampleQuery.selectFrom, versionPayload, decideVersioning,
typed refusal members and the entry codec remain shared. The existing
stageVersion projection owns material-version keys; stageRemoval owns cascades.
No cache, retry, connection, serializer, schema or migration path was added.

New entries use UserProvenance; edits preserve stored provenance. Latest-row
kind determines sample identity, so another entry kind's GUID is not a sample.
Lifecycle updates affect only the latest version, version resolution ignores
lifecycle, and removal inspects every stored version's pin. Database/decoder
failures become SampleStoreUnavailableErr with their diagnostic description.

The current arc's existing STORE_XDUO_0002 registry row changed only lifecycle
and implementStep; declaration metadata and consumers remain intact.

## Deferred

No implementation requirement is deferred. Acceptance execution is the
supervisor's assigned next stage.

## Gotchas

- BuiltinCatalogue.fs also exhaustively matches SampleError. Its extra match
  arm is a necessary fixture repair beyond the two consumers named by the slice.
- SampleProxySqliteTests owns and disposes every database opened by its factory,
  including those opened by inherited equality and pinning facts.
- A ThickPlate fixture needs a plate substrate; thin-film structure changes
  must use a compatible substrate kind.
- The supervisor-owned .manifest.state.json edit is preserved. No git mutation,
  version bump, gate/oracle change, analyzer change or checkpoint update occurred.
- Worker-owned files use LF and persistent output stays in the supplied
  per-arc artifacts directory.

## Changelog

- 2026-11-06: Implemented the SQLite SampleProxy and typed failure path, added
  23 passing SQLite contract cases, and recorded its inherited lifecycle update.
