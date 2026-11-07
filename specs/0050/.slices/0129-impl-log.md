# Slice 0129 implementation log

## Progress

- Attempt 02, before repair: read the retry task, worker/project protocols, slice and gate snapshot, existing worker outputs, S41-KIT-LOCK oracle and affected source/tests. The measured ELEV010P identifies the private isKitLockLiveOn member's MachineName -> bool signature. Its sole caller needs the live holder for KitLockedErr; return KitLockHolder option instead and map that holder into the error. Keep the established lock semantics and every test assertion. Only KitFolderStore.fs requires a production edit, within the declared effects.
- Attempt 02, after repair: replaced isKitLockLiveOn with tryFindLiveKitLockOn : MachineName -> KitLockHolder option and updated tryExistingLock to map Some holder to KitLockedErr. Foreign-machine and running-local-process holders return Some this; only ArgumentException from local process lookup returns None. The owner-first branch and outer KitIoErr exception boundary remain in place. No test, acceptance, public contract or stored format changed in this retry. No scope conflict was encountered.
- Attempt 02 handoff: inspected the changed member and its caller and updated the prescribed state-of-the-world. Source diff numstats match with and without --ignore-cr-at-eol, with no CRLF churn. Git status retains the original five source/test modifications and supervisor-owned state changes; this retry added no files outside the prescribed worker outputs. No gates or tests executed.
- Read AGENTS.md, the task, both cross-repo worker prompts, the project prompt, naming conventions, slice 0129 and its gate snapshot. Inspected S41-KIT-LOCK in the immutable oracle, existing kit code/tests and STL ProcessId.
- Before test changes: will add only the eleven authorized discovered identities and widen the existing KitError rendering fact. Existing assertions will remain.
- After test changes: added the holder-rendering fact, three shared KitProxyContract facts (six discovered identities) and four folder-only facts. Widened only the authorized error-rendering expectation from seven cases to eight. Added assertions inside those identities for equal timestamps across proxies, external replacement before release, canonical stored text and malformed/null lock input. No tests were executed.
- Before implementation: add the typed holder contract and proxy operations. Supply both lock serialization and parsing from Storage to the in-memory folder so outside text replacement and readDataFile share the same stored contents. Track which holder each proxy actually acquired, independently of process identity.
- After initial implementation: both proxies now expose acquisition/release, use per-proxy ownership, share canonical lock serialization and retain .oc after release. Storage distinguishes malformed cache text from IO errors and checks local process liveness; the in-memory folder assigns monotonically increasing STL ProcessId owners.
- Before concurrency refinement: source reasoning exposed a stale-delete race in the literal read-stale/delete/move sequence: two openers can observe stale text, then one can delete the other's newly moved live lock. Add an exclusively opened, delete-on-close cache coordination file around the recheck/delete/move and compare/delete sequences. This is a non-trivial internal refinement to satisfy the oracle's no-live-lock-overwrite requirement across processes/machines; it adds no durable format or permanent path. Foreign live locks and repeated acquisition still return from the initial read without creating anything. The prescribed unique holder .tmp and non-overwriting move remain unchanged.
- After concurrency refinement: Storage uses .oc/lock-guard.tmp with FileShare.None and DeleteOnClose for the short mutation sequence, rechecking the holder after obtaining the guard. Only sharing/lock violations retry; other failures flow through KitIoErr. Lock readers share delete access so they cannot block an opener's move/release. A per-proxy monitor serializes its own calls; the in-memory folder monitor serializes counter allocation and lock operations. No live lock handle is retained after an operation.
- Extended the existing authorized contention and stale/unreadable facts with three simultaneous openers, equal acquisition instants, exactly one winner, exact loser diagnostics and no remaining temporary paths. Added inaccessible-lock acquisition/release assertions inside the unreadable-lock fact; IO failures preserve bytes/ownership and are not treated as malformed text. No further test identity was created.
- Completed source/diff review against S41-KIT-LOCK and the authorized effects. Kept the new Domain match free of repeated-type tuples, kept new record braces on their own lines, and preserved all pre-existing test assertions except the explicitly authorized seven-to-eight error count. Source numstats agree with and without ignoring CR at line ends; no source CRLF churn was introduced.
- Wrote the required state-of-the-world handoff. All slice implementation work is present; verification remains the supervisor's responsibility, with no implementation blocker recorded.

## Files modified

- Attempt 02: KitFolderStore.fs plus the prescribed impl-plan, impl-log and state-of-the-world; all other existing source/test changes belong to the first attempt and are preserved.
- Worker plan and this log created at the task's prescribed paths.
- KitTests.fs and KitProxyTests.fs: authorized regression assertions authored before production implementation.
- Kit.fs: MachineName, UserName, KitLockOwner, KitLockHolder, invariant holder diagnostics, KitLockedErr, and shared cache/lock relative paths.
- KitFolders.fs: acquireLock/releaseLock contracts, serialized in-memory lock contents, sequential in-memory owners and per-proxy ownership tracking.
- KitFolderStore.fs: canonical cache binding, local process liveness, lazy filesystem acquisition, non-overwriting move, temporary coordination/cleanup and conditional release.
- Required 0129-state-of-the-world.md created beside the slice.

## Testing state

Attempt 02 uses the ELEV010P diagnostic supplied in 0129-attempt-02-task.txt as its repair input. The offending primitive result has been removed from the private signature; this is a source change, not a measured analyzer result. No gates or tests were run by this worker. The supervisor must execute the existing roster after exit; gates_run and gate_results remain empty and commit_ready remains false until gate success is established externally. Existing regression coverage and test identities are unchanged.

No checks or gates run. IMPLEMENT invariant 6 assigns execution to the supervisor. Pending roster: build, elevation, all-discovered constructor-unit-tests and eleven s15 invariants. Expected test increase is eleven, not a measured count.

The authored census is one new KitTests fact, three KitProxyContract facts inherited by each of KitProxyTests and KitProxyFolderTests, and four KitProxyFolderTests-only facts. The existing KitTests error-rendering fact gains KitLockedErr. No fact is removed or renamed. The source race assertions are part of those same identities and have not been executed.

The inherited constructor_unit_tests baseline visible in .checkpoints-json is 1047 at checkpoint 127; checkpoint 128 was absent when read. The state-of-the-world repeats that supervisor-owned number as inherited information, without manufacturing a measured baseline. The structured summary will report empty gates_run/gate_results and commit_ready false because this worker cannot claim gate success.

## Artifacts

- No captured execution artifacts; persistent artifacts, if needed, belong only under specs/0050/.artifacts.

## Gotchas

- Attempt 02: ELEV010P applies to a private extension member when its signature combines MachineName with bool; the member primitive exemption does not excuse that mixed signature. Returning the existing holder as an option resolves the shape without a new wrapper or suppression.
- Attempt 02 also found specs/0050/.expected-red-observed-json already modified by the supervisor; it remains untouched, like .manifest.state.json.
- specs/0050/.manifest.state.json was already modified by the supervisor; leave it untouched.
- Prior slice 0128 moved createInMemory to Storage and injected the metadata serializer into Domain. Extend this seam rather than duplicate JSON or introduce a Domain-to-Storage reference.
- No gates, oracle, census, checkpoint, version, analyzer configuration, STL/ASG or notebook edits are authorized.
- The guard is an ephemeral IO implementation detail, not a second durable lock contract. It exists only while changing .oc/lock; final paths still contain only the prescribed lock, and release preserves .oc. This records the refinement when made rather than editing the published slice.
- Source inspection is not proof that a compiler, analyzer or test gate passes. No such result is reported here.
