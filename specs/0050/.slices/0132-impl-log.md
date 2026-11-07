# Implementation log — 0132

## Operator-authorized recovery — 2026-11-07

This section supersedes the unresolved authority conflict and testing status
in the historical worker entries below. The operator approved the four-step
secondary-review plan; `.manual/009-step-0132-lock-guard-recovery.md` records
the expanded scope and the required normal-resume procedure. Entry HEAD was
`90b0649`, containing the failed attempt and its reports.

- Reproduced native errors 32 and 5 using the original exclusive delete-on-close
  guard. Replaced its unbounded spin with a shared IO proxy allowing at most
  101 opens and 100 ten-millisecond waits. Other errors return immediately;
  exhausted retries retain the path, exception type and HRESULT in `KitIoErr`.
- Preserved the exclusive guard, ownership recheck, non-overwriting publication
  and temporary-file cleanup. Both acquisition and release use the same retry
  policy; failed release retains ownership for another release attempt.
- Strengthened existing lock facts with deterministic retry-limit checks,
  permanent guard failures and release recovery, and 25 synchronized contention
  rounds for each filesystem initial state. All old assertion lines and fact
  identities remain. `KitTests.fs` is unchanged.
- Renamed reserved `process` locals in Domain and Storage to clear measured
  FS0046 warnings. Domain's attachment behavior is otherwise unchanged.
- The operator repair ran 58 focused kit tests, three independent-process
  contention scenarios, and all 14 unchanged slice gates successfully. The
  final build has zero warnings/errors; elevation passed seven projects; all
  1,076 constructor cases passed with exact census and zero skips; all eleven
  invariants passed. These are new operator-repair measurements, separate from
  the earlier IMPLEMENT worker's restricted execution role.
- Validated all 494 steps and refreshed the profile-1 receipt for the project
  prompt. All 988 compiled artifacts, published contracts, oracle, supervisor
  state, checkpoints, expected-red records and accepted baselines are unchanged.

Evidence: `.artifacts/0132-operator-repair/final-review.md`. The repair is ready
for the operator to commit; the arc remains escalated at 0132. No commit or
restart was performed. After the commit and explicit relaunch instruction,
use the recovery `operator-reply.md` with `--executability-profile 1` and normal
resume. Do not use `--accept-current-slice`: the supervisor must measure the
new 1,076-case floor and publish checkpoint 131 itself. ASG is unchanged.

## Attempt 02 progress

- Read the attempt-02 task, worker protocols, project prompt, slice and compiled gates; inspected the inherited implementation and the reported failing test without running a gate.
- Before retry disposition: trace the measured access-denied result to the filesystem lock guard, identify its owning contract and minimum repair location, and preserve the existing attachment implementation and test census. Record an authority conflict if that location is outside the two authorized source files.
- Traced the existing failing fact at KitProxyTests.fs:959 through contendForKitLock at line 121: three proxies start together, exactly one must acquire the lock, and every loser must return KitLockedErr naming the winner. The measured failure occurs at the existing assertion on line 144, after the sequential stale-lock takeover assertions have completed.
- Located the guard in OpticalConstructor.Storage/KitFolderStore.fs:98. Its FileStream uses OpenOrCreate, FileShare.None and DeleteOnClose; the retry catches only IOException with low HRESULT 32 or 33. An access-denied failure is not handled there and reaches tryKitFolderIo at line 59, which maps it to the observed KitIoErr. Acquisition enters this guard at line 273; release shares the guard at line 303.
- The observed contention and delete-on-close guard suggest a transient Windows guard-file deletion race. The supplied diagnostic does not include the native error code, so that mechanism remains an inference, not a reproduced finding. The concrete finding is that access denied at the guard escapes the contention protocol.
- Read the existing supervisor-produced constructor-unit-tests.trx without executing tests: it records all four new attachment facts as Passed, 1076 total, 1075 passed and the one supplied KitProxyFolderTests failure. No new result or passing checkpoint was authored.
- Confirmed the locking path does not call any new attachment symbol. The guard and failing fact belong to step 0129, obligation S41-KIT-LOCK. Step 0132's effects permit only Domain/Kit.fs and Tests/KitTests.fs; neither contains the filesystem guard. The retry therefore requires an authority decision, as explicitly directed by the task.
- Before handoff: retain all production/test changes from attempt 01, update these three prescribed worker documents with the exact conflict, keep the inherited baseline, and return commit_ready false. No source repair, gate/census amendment, test weakening or worker gate execution is performed.
- Completed the retry disposition and state-of-world update. Only the three prescribed worker documents were edited in attempt 02; the source changes and supervisor-owned state files remain as found.

## Authority conflict — attempt 02

Step 0132 acceptance requires the all-discovered constructor-unit-tests suite to pass. Its explicit edits list allows only Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs and Berreman/OpticalConstructor/OpticalConstructor.Tests/KitTests.fs, while the measured failure originates in Berreman/OpticalConstructor/OpticalConstructor.Storage/KitFolderStore.fs, FileName.openKitLockGuard. The retry instruction explicitly requires reporting a conflict when the necessary edit is outside scope.

The missing authority is permission to repair that storage member and, if needed, its acquisition/release callers under S41-KIT-LOCK (owner: OpticalConstructor.Storage.KitFolderStore; original step: 0129). The repair must distinguish transient guard contention from genuine access denial, retain one winner and KitLockedErr for losers, preserve stale-lock recovery and temporary-file cleanup, and continue returning KitIoErr for real IO failures. Broadly retrying every access-denied error would risk hanging on a permanent permission failure and is not an established repair.

The existing regression identity is OpticalConstructor.Tests.KitProxyFolderTests.a lock left on this machine by a process that has ended is taken over, in Tests/KitProxyTests.fs. It and its contention assertions must remain intact. No new or renamed test identity, acceptance change, reduced selection or baseline change is requested. No authorized attachment edit can repair this filesystem boundary while preserving the declared scope. The supervisor/operator must authorize the storage repair before this slice can be commit-ready.

## Progress

- Read AGENTS.md, both worker prompts, the project prompt, slice 0132, its compiled gate snapshot, naming conventions, S42-ATTACH-SHAPES oracle, the relevant manual 001 sections and spec shared shapes, and the preceding worker records.
- Confirmed the scope is Domain/Kit.fs and Tests/KitTests.fs, with exactly four new fact identities. Existing KitRelativePath.tryCreate owns validation and ContentHash.ofBytes owns hashing; FileByteContent comes from Softellect.Sys.Primitives.
- Before test changes: author the four prescribed facts with all required examples and edge coverage inside those identities. Tests are not executed under IMPLEMENT invariant 6.
- Authored the four exact KitTests fact names, preserving existing assertions. Coverage includes all slice examples, ordinal data-folder casing, gaps in copy suffixes, occupied paths in other folders, slash/backslash normalization on both folder and file, invalid path remainders, and reuse of LabFileSource bytes for attachment hashing.
- Before production changes: append the prescribed pure shapes and members to Kit.fs. Keep extension splitting lexical (last dot except a leading dot), validate every candidate through KitRelativePath.tryCreate, and compare attachment status solely by contentHash.
- Added KitDataFolder/root/tryCreate/copyTargetFor, KitFolder.tryKitPathOf, LabFileSource, DataAttachmentStatus, DataAttachment/statusAgainst and KitDataFile.attachment. Numbered suffixes use invariant formatting; the recursive search stops on the first invalid or unoccupied candidate. Containment normalizes both texts and passes only the remainder to the existing validator.
- Reviewed the source diff: only the two authorized source files changed, with 95 and 139 added lines respectively and no removed lines. Numstat matches when ignoring CR-at-EOL, so no source line-ending churn was introduced. No existing test was removed or renamed.
- Before handoff: write the state-of-world with all required headings and the inherited supervisor checkpoint count. The latest preceding recorded constructor_unit_tests count is 1072 at checkpoint 130; checkpoint 131 is absent. The expected census increase for this slice is four, subject to supervisor measurement.
- Completed the prescribed state-of-world and implementation records. No requested implementation is deferred, no implementation blocker is known, and no gate execution or git mutation was performed.

## Files modified

- Created the prescribed implementation plan and this log.
- Modified OpticalConstructor.Tests/KitTests.fs with the four authorized facts.
- Modified OpticalConstructor.Domain/Kit.fs with the requested pure attachment shapes and members.
- Created the prescribed 0132-state-of-the-world.md beside the slice.
- Attempt 02 updated only 0132-impl-plan.md, 0132-impl-log.md and 0132-state-of-the-world.md. The two source files above remain exactly as inherited from attempt 01.

## Testing state

No build, test, analyzer or invariant gates run by this worker, per IMPLEMENT invariant 6. The attempt-02 task supplies a constructor-unit-tests failure; the existing supervisor-produced TRX records 1076 total, 1075 passed and 1 failed, with the four attachment facts passed. These are inherited diagnostics, not worker gate results. No pass is asserted for unexamined gates. The suite failure remains unresolved because the required storage repair is outside step 0132's effects. commit_ready is false for that authority conflict; acceptance is not waived.

## Artifacts

The authoritative artifact directory is specs/0050/.artifacts. No run artifacts created. The supplied diagnostic is 0132-attempt-02-task.txt there; the existing supervisor TRX at Berreman/OpticalConstructor/OpticalConstructor.Tests/TestResults/constructor-unit-tests.trx was read only.

## Gotchas

- specs/0050/.manifest.state.json was already modified before this worker began and remains untouched.
- The data-folder prefix is ordinal and case-sensitive; occupied paths and absolute kit containment use OrdinalIgnoreCase.
- The role-specific IMPLEMENT invariant 6 supersedes the shared prompt's legacy self-check instructions.
- On attempt-02 entry, the two source files, .manifest.state.json and .expected-red-observed-json were already modified and the three worker records were untracked. All supervisor-owned files remain untouched.
- A failed all-discovered gate does not widen the effects list. Storage and KitProxyTests.fs are outside step 0132 even though their existing tests are included in its suite.
