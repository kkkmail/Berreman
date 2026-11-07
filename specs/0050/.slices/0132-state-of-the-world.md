# Where we are

Slice 0132 implements S42-ATTACH-SHAPES, the first S4.2 slice of spec 0050. The attachment implementation and four prescribed facts are preserved from attempt 01. Attempt 02 investigated the supervisor's constructor-unit-tests failure and is blocked by an authority conflict: the filesystem lock-guard repair belongs in Storage/KitFolderStore.fs, outside this slice's two authorized source files. The slice is not commit-ready.

# What's working

- Preserved validated data-folder selection and pure copy naming with the first free numbered suffix, ignoring case.
- Preserved lexical kit containment, lab-file sources, attachment hashes and unchanged/changed/missing status.
- Preserved all four prescribed attachment facts and every existing test assertion.
- Traced the measured contention failure to the existing storage guard and recorded the exact repair authority needed.

# Tests

This IMPLEMENT worker ran no build, test, analyzer or invariant gates, as required by invariant 6. The supervisor owns execution of 0132.gates, including build, elevation, all-discovered constructor-unit-tests and the eleven s16 invariants. No new gate results are claimed.

The supplied attempt-02 diagnostic and existing supervisor-produced constructor-unit-tests.trx record 1076 total tests, 1075 passed and one failed. All four new attachment facts are marked Passed in that report. The failing identity is `OpticalConstructor.Tests.KitProxyFolderTests.a lock left on this machine by a process that has ended is taken over`. Its contention helper received KitIoErr for access denied to `.oc/lock-guard.tmp`, where the existing assertion requires KitLockedErr naming the winner. This is an unresolved measured failure, not an accepted or waived result.

The YAML preserves the inherited supervisor-owned constructor_unit_tests baseline of 1072 from checkpoint 130 recorded in attempt 01. The failed run's 1075 passes are not a new baseline; no checkpoint or census was edited.

```yaml
gates:
  constructor_unit_tests: 1072
```

# Architecture

The attachment additions remain pure and confined to Domain/Kit.fs. KitDataFolder owns collision naming and uses KitRelativePath.tryCreate for candidates; KitFolder.tryKitPathOf normalizes separators and validates the remainder without IO. LabFileSource reuses FileByteContent, and DataAttachment stores a relative path and ContentHash. No schema, serialization or UI changes were made.

The unrelated failing path is `KitProxy.acquireLock -> FileName.openKitLockGuard` in Storage/KitFolderStore.fs. The guard opens an exclusive delete-on-close file and retries only IOException low HRESULT 32/33. Access denied escapes that loop and is converted to KitIoErr by tryKitFolderIo. Release uses the same guard. The timing and DeleteOnClose flag suggest a transient deletion race, but the diagnostic does not expose the native code and no reproduction was run; that mechanism remains an inference.

This path belongs to step 0129's S41-KIT-LOCK obligation. Its production repair must distinguish transient contention from permanent access denial, preserve exclusive acquisition, stale-lock recovery, loser diagnostics and cleanup, and retain typed IO errors. There is no call from this locking path to the new attachment members.

# Deferred

No requested implementation is voluntarily deferred. Completion is blocked by conflicting obligations: step 0132 requires the entire constructor suite to pass, but authorizes edits only to Domain/Kit.fs and Tests/KitTests.fs. The necessary repair is in Storage/KitFolderStore.fs at openKitLockGuard, with acquisition/release callers if needed.

The supervisor/operator must authorize that storage edit under the existing S41-KIT-LOCK contract before repair can proceed. Preserve the existing KitProxyFolderTests regression identity and assertions; no new test identity, changed acceptance, narrowed gate selection or baseline change is requested. This conflict is reported as required by 0132-attempt-02-task.txt; commit_ready is false.

# Gotchas

- IMPLEMENT invariant 6 assigns all gate execution to the supervisor; source inspection and reading an existing TRX are not new gate runs.
- Blanket retry of access denied could hang on a real permission failure; the diagnosis does not justify that shortcut.
- The all-discovered suite includes previous slices' tests but does not authorize changes to their files.
- Data-folder validation is ordinal and case-sensitive; collision detection and kit containment are ordinal and case-insensitive.
- Attempt 02 changed only the three prescribed worker documents. The inherited source edits and supervisor-owned .manifest.state.json and .expected-red-observed-json remain untouched.

# Changelog

- 2026-11-07: Implemented pure attachment shapes, collision naming and kit containment; authored four authorized facts and prepared the supervisor handoff.
- 2026-11-07 (attempt 02): Diagnosed the supplied lock-guard access-denied failure, preserved the attachment implementation/tests, and recorded the out-of-scope storage repair needed before acceptance can be completed.
