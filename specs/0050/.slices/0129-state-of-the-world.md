# Where we are

Slice 0129 implements S41-KIT-LOCK in spec 0050 S4.1. Both KitProxy implementations now acquire and release per-proxy locks, identify a foreign holder through KitLockedErr, and preserve the cache folder on release. The eleven authorized regression identities and widened error-rendering expectation are authored. Attempt 02 repairs the supervisor-reported ELEV010P by returning an optional live holder from the private liveness member, preserving the existing lock behavior and tests. Implementation awaits the arc-runner's gate execution; this IMPLEMENT worker ran no gates and claims no passing result or commit readiness.

# What's working

- Added elevated kit lock ownership, acquisition time and invariant-culture holder diagnostics.
- Added per-proxy acquisition/release to both backends with exact foreign-holder errors and lazy cache creation.
- Reused canonical JSON for lock text and recovered malformed or dead-local-process locks.
- Protected stale takeover and release with temporary coordination and a non-overwriting holder-file move.
- Added the eleven prescribed test identities, including contention, outside replacement and IO-failure assertions.
- Replaced the private liveness boolean with an optional KitLockHolder consumed directly by the contention error path.

# Tests

No build, tests, analyzers or invariant checks were executed, per IMPLEMENT invariant 6. The supervisor owns the complete pending roster in 0129.gates: build, elevation, all-discovered constructor-unit-tests and the eleven s15 invariants. There are no locally measured gate outcomes.

Attempt 02 is a repair of the ELEV010P diagnostic supplied in 0129-attempt-02-task.txt. Only the private helper and its sole caller changed in production during this retry. Existing tests and their assertions are preserved; the repair has not been verified by a new analyzer or test run here.

The expected census increase is eleven: one KitTests fact, three shared KitProxyContract facts discovered under both concrete subclasses, and four folder-only facts. The existing error-rendering test gains the new error and changes its expected distinct count from seven to eight. Existing test identities and other assertions remain intact.

Authored coverage includes the exact lab/BENCH-1/4242 timestamp rendering fixture under a different culture; holder identity and canonical stored JSON; sequential in-memory owner ids; contention, repeated acquisition and non-owner release; identical process/timestamp values across distinct proxies; lazy .oc creation and retention; externally replaced locks; local dead-process takeover with case-insensitive machine matching; foreign live locks; malformed, null and unbinding JSON; three simultaneous openers against missing/stale/malformed locks; temporary-file cleanup; and inaccessible-lock acquisition/release returning KitIoErr while preserving bytes and ownership. These are source assertions, not reported execution results.

The YAML below repeats the supervisor-owned constructor_unit_tests baseline visible at checkpoint 127 in .checkpoints-json. Checkpoint 128 was absent when read. This is inherited information, not a new baseline measured or authored by the worker.

```yaml
gates:
  constructor_unit_tests: 1047
```

# Architecture

Kit.fs defines MachineName and UserName, KitLockOwner using STL ProcessId, and KitLockHolder using the existing ClockTime. Its describe member formats the timestamp and process id with the invariant culture. KitLockedErr carries the holder. KitRelativePath centralizes the cache and lock paths used by both backends.

KitProxy gains acquireLock : ClockTime -> Result<KitLockHolder, KitError> and releaseLock : unit -> Result<unit, KitError>. Each proxy remembers only the holder it successfully acquired. Equality of machine/user/process alone never makes another proxy the owner. Repeated acquisition returns the unchanged owned holder, and release compares the current stored holder before deleting. Non-owners do nothing.

InMemoryKitFolder assigns successive STL ProcessId values starting at one, with machine/user in-memory. A folder monitor serializes owner allocation and lock transitions. Lock text occupies the existing shared text map, so paths and readDataFile observe the real stored lock and outside text replacement is respected. Storage supplies lock serialization/parsing through the existing injection seam used for metadata; Domain acquires no Storage dependency. Every valid in-memory lock is live.

Storage binds cache text through CanonicalJson.options and writes nodeOf/canonicalFileText, with no schema or migration for .oc. Malformed or unbinding text is stale; other IO failures remain KitIoErr. Only a matching machine (ordinal, case insensitive) whose process lookup raises ArgumentException is stale among bound holders. Other-machine holders remain live.

The private tryFindLiveKitLockOn member returns KitLockHolder option, preserving the stored holder when it is live. tryExistingLock first recognizes this proxy's held lock, then maps a foreign live holder into KitLockedErr. A stale holder yields None, allowing acquisition to continue. This removes the mixed primitive/domain signature without adding a type, exemption or stored field; process lookup exceptions other than ArgumentException still reach the outer KitIoErr boundary.

Filesystem acquisition writes .oc/lock-<GUID>.tmp and moves it to .oc/lock with overwrite false. The failure path deletes the temporary file and reads the winner. A short exclusive, delete-on-close .oc/lock-guard.tmp coordinates recheck/delete/move and compare/delete across cooperating openers, closing the stale-delete race in the literal uncoordinated sequence. This internal refinement was recorded in the log before implementation. The guard leaves no durable artifact, and known foreign live locks/repeated acquisition return without creating it. Readers share delete access so they do not obstruct mutation; a per-proxy monitor protects that proxy's state. All streams are disposed within the operation.

There is no durable project/kit schema change, migration, UI wiring or read-only-context implementation in this slice. The new shape is the requested lock cache and in-process proxy contract.

# Deferred

No implementation requirement was deferred. Gate execution belongs to the supervisor under the worker protocol. Read-only opening and window integration are outside this slice's authorized scope.

# Gotchas

- Gate success cannot be inferred from authored assertions or source review; all gate outcomes remain unmeasured here.
- Only the five authorized source files and the supplied worker documents were edited across attempts. Attempt 02 changes KitFolderStore.fs and the three worker documents only. The supervisor's pre-existing .manifest.state.json and .expected-red-observed-json changes remain untouched.
- No gates, oracle, census, checkpoint, analyzer configuration, version ring, notebooks, STL or ASG files were changed; no git mutation commands were run.
- The temporary coordination file is removed on handle disposal, including exceptions/process exit. It must not be confused with the persisted holder at .oc/lock.
- The in-memory composition member now receives lock serialization and parsing in addition to metadata serialization; createInMemory remains in Storage.KitFolderStore.
- Filesystem fixtures remain under the test output directory. Source diff numstats agree with and without ignoring CR at line ends.

# Changelog

- 2026-11-07: Implemented per-proxy kit locks and holder diagnostics, canonical cache text, stale-lock recovery and contention coordination; authored the eleven prescribed tests and handed gate execution to the supervisor.
- 2026-11-07 (attempt 02): Replaced the private bool liveness result with an optional KitLockHolder to address ELEV010P; preserved all lock behavior and test assertions and returned verification to the supervisor.
