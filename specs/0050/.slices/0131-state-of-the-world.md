# Where we are

Slice 0131 implements S41-KIT-CONTEXT in spec 0050 S4.1, building on the existing metadata, locking and change-stamp proxy operations. KitContext now provides the requested open/save/close lifecycle and selects a read-only proxy when requested. Code and the prescribed tests are authored; acceptance execution belongs to the arc-runner.

# What's working

- Added KitReadOnlyErr with the requested diagnostic rendering.
- Added an explicit read-only KitProxy that refuses every write and leaves locks untouched.
- Added KitContext opening in metadata, optional lock, then stamp order, with lock cleanup on stamp failure.
- Added immutable metadata saves stamped by the injected clock and closing through the selected proxy.
- Added five shared contract facts covering both backends, for ten concrete test identities, and widened the existing error-rendering fact.

# Tests

No build, test, analyzer or invariant gates were run, per IMPLEMENT invariant 6. The supervisor's compiled 0131.gates roster remains build, elevation, all-discovered constructor-unit-tests and the eleven s15 invariants. No passing result is claimed.

The authorized facts cover successful read-write open/close, the loaded metadata/access/clock/proxy, baseline recording, foreign-lock refusal and read-only fallback, no filesystem changes or cache creation from read-only operations, all five refused proxy write verbs and saveKitFile, read/stamp passthrough, and a failed stamp scan. They also cover preserving an already-owned lock through a read-only open/close, read/lock failures short-circuiting later operations, releasing a lock after stamp failure while retaining the original error if cleanup reports an error, deterministic modified time two seconds after creation, unchanged created time and original context, write-error propagation, and unchanged metadata bytes when writing/deleting another kind of kit file. Both access modes reject a non-kit folder without writes. Existing assertions and test identities remain intact.

The YAML repeats the supervisor-owned constructor_unit_tests count of 1062 from checkpoint 129, the latest preceding recorded count in .checkpoints-json; checkpoint 130 is absent. It is inherited information, not a newly authored or worker-measured baseline. The authorized new test identities imply a +10 census change, subject to supervisor measurement.

```yaml
gates:
  constructor_unit_tests: 1062
```

# Architecture

All production changes remain in Domain/Kit.fs and Domain/KitFolders.fs. KitContext contains only the requested proxy, kitFile, access and clock fields, and only openKit, close and saveKitFile members. Existing Storage code supplies filesystem operations and serialization unchanged; existing in-memory proxies supply the same behavior for tests.

readOnlyView constructs every KitProxy field explicitly, making future additions require an access-policy decision at compile time. Observational fields retain the underlying closures, including the in-memory stamp record. Every write returns KitReadOnlyErr naming its proxy field; releaseLock returns Ok without delegating. A read-only save is refused by writeKitFile after the requested clock stamp is computed.

openKit reads metadata before consulting the lock clock or acquiring a lock. Only read-write access takes and, on stamp failure, releases the lock. Cleanup cannot replace the original stamp error. saveKitFile returns a new context only after a successful proxy write. Other kit-file operations remain on context.proxy and do not touch metadata. No durable format, serializer, schema version or migration changes were introduced.

# Deferred

No requested implementation is deferred. Supervisor gate execution is pending as required by the worker protocol.

# Gotchas

- The slice's placement instruction names KitProxy.createInMemory in Domain/KitFolders.fs, but that factory is now a Storage extension using shared serializers. The new Domain declarations follow the existing InMemoryKitFolder implementation; no factory or serializer was moved or copied.
- Opening a read-only view over a proxy that already owns a lock must not release that lock when the view closes.
- On a stamp failure, the original error is returned even when the release attempt also reports an error.
- A future writing KitProxy field must be refused in readOnlyView as well.
- specs/0050/.manifest.state.json was already modified by the supervisor and remains untouched by this worker.
- Source diff counts match with and without ignoring CR-at-EOL; no source line-ending churn was introduced.

# Changelog

- 2026-11-07: Implemented the kit context lifecycle and read-only proxy, authored the ten authorized contract identities and updated error-rendering coverage; prepared handoff for supervisor gates.
