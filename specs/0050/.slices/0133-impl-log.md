# Implementation log — 0133

## Progress

- Read AGENTS.md, task, both worker protocols, project prompt, slice, resolved
  gates, naming conventions, S42-COPY-IN oracle and manual 001's kit contract.
  Read manual 009 and the inherited atomic-writer repair from step 0126.
- Entry worktree has a supervisor-owned change to .manifest.state.json; leave
  it untouched. Checkpoint 131 records 1,076 constructor cases.
- Before tests: extend the shared fixture and author the ten prescribed test
  identities, retaining existing assertions and the lock coverage. Test execution
  belongs to the supervisor under IMPLEMENT invariant 6.
- Authored the four shared facts and two filesystem-only facts with the exact
  prescribed names. Added fixture paths and outside deletion. Additional
  assertions within these identities cover a folder occupying a suffix (with
  different case), an absolute source name, reuse of a freed suffix, deletion
  visibility, and read-only refusal before missing folders can be created.
- Before production edits: add the two proxy fields, share byte decoding/hash
  construction at FileByteContent, keep in-memory text storage, and route disk
  copying through one ExistingFilePolicy-based atomic writer. Leave all guard
  acquisition/release code unchanged.
- Added both KitProxy verbs and read-only forwarding/refusal. InMemoryKitFolder
  now identifies its absolute kit folder and supports outside deletion with
  MissingKitFileErr and reserved-metadata InvalidKitPathErr. Copy-in selects a
  free name under the folder's existing synchronization, stores decoded text in
  the existing map and reads the answer through the same in-memory reader.
- Moved the existing FileByteContent UTF-8 decoder from Storage to Domain and
  added its shared KitDataFile construction member. Filesystem read and copy
  now share decoding and hashing; no duplicate parser or second storage map was
  introduced.
- Factored atomic writing into one private FileName member over STL
  ExistingFilePolicy. Existing writes retain overwrite behavior; the new writer
  uses File.Move with overwrite disabled and the existing failure cleanup.
  Filesystem copy-in creates parents, lists files and folders, selects a name,
  writes it through that member, and maps IO failures to KitIoErr with context.
- Source review kept the one-argument in-memory reader as a private member and
  moved filesystem path construction inside the exception boundary. The diff
  preserves all existing test lines and the complete shared guard implementation.
  Only the four authorized source files changed; their numstats match with and
  without ignore-cr-at-eol. No source line-ending churn was introduced.
- Before handoff: write the mandatory state-of-world, recording inherited
  checkpoint 131's count without publishing a new baseline or claiming results.
- Completed all slice implementation and report outputs. No implementation
  work is deferred; return the changes for normal supervisor verification.

## Attempt 02 progress

- Read the retry task, worker protocols, resolved gates, code judge's cycle-1
  finding, naming conventions and manual 009's guard requirements. The previous
  implementation and reports are present; supervisor state remains untouched.
- Source inspection confirms the defect: RefuseExistingFile still writes with
  File.WriteAllBytes, which truncates an occupied staging file, and failure
  cleanup unconditionally deletes that path. The final move alone cannot
  protect an existing lab file named `<destination>.tmp`.
- Before regression extension: retain the authorized new-file writer fact and
  all its assertions; add direct-writer and copyInFile scenarios over an
  occupied staging path with an absent destination. No new identities or gate
  changes are authorized or needed. Execution remains supervisor-owned.
- Extended the existing refusal fact with an occupied data/run.csv.tmp and an
  absent data/run.csv. It asserts FileIoExn from the writer, KitIoErr naming the
  destination from copyInFile, unchanged staging bytes, absent destination and
  unchanged path lists after each call. Every original assertion remains.
- Before production repair: keep File.WriteAllBytes and unconditional failure
  cleanup for OverwriteExistingFile. For RefuseExistingFile, use CreateNew and
  permit cleanup only after that call returns a stream; dispose the stream
  before publication. Keep the existing File.Move policies and error mapping.
- Updated the shared writer: RefuseExistingFile now opens its staging path
  with FileMode.CreateNew and FileShare.None, marks cleanup ownership only
  after acquisition, and disposes the stream at the end of that match arm.
  Acquisition failure reaches FileIoExn without deleting the occupied path;
  later failures still clean up the staging file created by this operation.
  The overwrite branch retains File.WriteAllBytes and its previous cleanup.
- The final File.Move calls, copyInFile error mapping and no-retry behavior
  remain intact. No changes were made to Domain, KitFolderStore or any lock
  regression code in this retry. Next, review the source diff and update the
  mandatory handoff report; no gate execution is performed by this worker.
- Source review uses Path.Combine for the new fixture's absolute names so the
  KitIoErr assertion matches the platform-separated destination constructed by
  copyInFile. Normal and ignore-cr-at-eol source numstats match; the test diff
  only adds lines. The existing supervisor state has a CRLF warning and was
  not edited. No build, test, analyzer or invariant command was run.
- Before final handoff: update the state-of-world's architecture and coverage
  notes for exclusive staging and ownership-aware cleanup. Preserve the
  inherited checkpoint count and all supervisor-owned verification artifacts.
- Updated the plan, this log and the prescribed state-of-world with the repair,
  coverage and unchanged verification ownership. Attempt 02 is complete and
  returned for the supervisor's existing gates; no requested work is deferred.

## Files modified

- Created the prescribed implementation plan and this log.
- OpticalConstructor.Tests/KitProxyTests.fs: shared fixture and ten new cases.
- OpticalConstructor.Domain/KitFolders.fs: proxy verbs, shared byte decoding and
  file construction, in-memory copy/location/deletion and read-only behavior.
- OpticalConstructor.Storage/CatalogueFolderStore.fs: shared policy-based writer
  and the new-file entry point.
- OpticalConstructor.Storage/KitFolderStore.fs: filesystem copy/location and
  reuse of the shared data-file decoder/hash construction.
- Created the prescribed 0133-state-of-the-world.md.

## Testing state

No worker checks or gates run. Required gates are build, elevation,
constructor-unit-tests and the eleven s16 invariants in 0133.gates. Their results
remain pending supervisor execution; no passing result is asserted.

Attempt 02 extends the existing filesystem refusal fact with the occupied
staging regression at both the writer and KitProxy boundaries. No test identity,
census entry, gate selection or baseline changes; the total increase remains
the ten cases declared by step 0133.

The source adds exactly four inherited facts (eight concrete cases) and two
filesystem-only facts. Expected constructor count is 1,086 from checkpoint
131's measured 1,076; this is arithmetic, not a new test measurement.
`gates_run` and `gate_results` are empty. Under the role-specific act-only
protocol, `commit_ready: true` returns the completed implementation for the
supervisor's review and gates; it does not assert gate success.

## Artifacts

No execution artifacts created. The authoritative artifact folder is
C:/GitHub/Berreman/specs/0050/.artifacts.

## Gotchas

- Signature deviation: writeNewBytesAtomically will accept FileByteContent,
  matching writeBytesAtomically and step 0126's measured ELEV010 repair.
  AGENTS.md gives the analyzers precedence over the literal byte[] signature;
  no overload, new carrier or exemption is introduced.
- The in-memory double drops a leading BOM before storing text, so its returned
  hash describes the stored text's UTF-8 bytes. Disk preserves and hashes every
  supplied byte. This distinction is explicit in step 0133.
- The shared guard's bounded retry and ownership contract from manual 009 stays
  unchanged. No versions, census, gates, oracle or supervisor state are edited.
- Attempt 02 fixes the initial writer's unsafe staging reuse. Refuse-existing
  writes must use CreateNew, not an existence check followed by a truncating
  write; cleanup is enabled only after successful acquisition. Overwrite writes
  intentionally keep their previous behavior. This is the judge-requested
  refinement of the slice's unconditional temporary-file cleanup wording.
