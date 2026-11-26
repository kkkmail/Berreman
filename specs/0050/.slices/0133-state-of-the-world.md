# Where we are

Step 0133 implements S42-COPY-IN in section S4.2, following step 0132's path
and collision shapes. Both kit implementations now support copying a lab file
into data and locating an absolute name inside the kit. Implementation and the
ten prescribed test cases are complete. Attempt 02 repairs the code judge's
occupied-staging finding; supervisor review and gate execution remain pending
under the IMPLEMENT act-only protocol.

# What's working

- Added copy-in and lexical file location to both KitProxy implementations.
- Reused collision selection and created missing data-folder parents.
- Shared atomic writing across overwrite and refuse-existing policies.
- Acquired new-file staging exclusively and preserved occupied staging files.
- Preserved disk bytes and hashes, and refused copy-in through read-only views.
- Added ten contract cases and in-memory outside-file deletion support.

# Tests

No worker build, test, analyzer or invariant gate was run, as required by
IMPLEMENT invariant 6. The supervisor runs build, elevation,
constructor-unit-tests and the eleven s16 invariants from 0133.gates after
handoff. No new gate result or measured test count is claimed.

The four new KitProxyContract facts are inherited by KitProxyTests and
KitProxyFolderTests; two additional facts belong only to KitProxyFolderTests.
They cover unchanged originals, suffix selection including occupied folders,
new parents, absolute-name lookup with and without a file, outside deletion,
read-only refusal, BOM byte preservation and failed new-file publication cleanup.
Attempt 02 extends the existing new-file refusal fact to assert FileIoExn for an
occupied staging path and KitIoErr through copyInFile, preserving its bytes,
leaving the destination absent and adding no paths. Existing assertions, test
identities and lock-regression code remain unchanged.

The count below is inherited from supervisor checkpoint 131, not authored as a
new baseline. The expected increase is ten cases to 1,086, subject to supervisor
measurement and exact census verification.

```yaml
gates:
  constructor_unit_tests: 1076
```

# Architecture

KitProxy gains only tryKitPathOf and copyInFile. Both implementations use the
existing KitFolder.tryKitPathOf and KitDataFolder.copyTargetFor rules. The
in-memory folder represents C:/in-memory-kit, retains its one map of texts and
adds parent folders when storing a copy. removeLabFile represents an outside
writer and refuses the reserved metadata path.

The existing FileByteContent decoder moved from Storage into Domain; its
KitDataFile construction member shares decoding and hashing between reads and
copies. Disk copies retain all original bytes. The in-memory double drops the
leading BOM before storing text and returns the hash of that stored text's
UTF-8 bytes, as the slice specifies.

FileName.writeBytesAtomically and writeNewBytesAtomically call one private
writer over STL ExistingFilePolicy. Publication replaces only for
OverwriteExistingFile; RefuseExistingFile uses a non-overwriting move. New-file
staging is acquired with FileMode.CreateNew and FileShare.None; failure cleanup
is enabled only after that acquisition succeeds. The stream closes before
publication. An occupied staging path returns FileIoExn without modifying or
deleting it; copyInFile maps that error to KitIoErr naming the destination.
OverwriteExistingFile retains File.WriteAllBytes and its original cleanup.
The new writer accepts FileByteContent, preserving step 0126's measured ELEV010
repair instead of restoring the literal raw byte-array signature in this slice.

No durable format, version, dependency or migration changed. The shared lock
guard and its manual 009 recovery contract are unchanged.

# Deferred

No requested implementation is deferred. Verification and acceptance belong
to the supervisor and have not been performed by this worker.

# Gotchas

- .manifest.state.json was already modified at entry and remains supervisor-owned.
  No gate, oracle, census, checkpoint, version or supervisor-state file was edited.
- New-file publication deliberately has no retry or overwrite option. A file
  appearing after name selection causes a typed failure at the non-overwriting
  move. An occupied sibling `.tmp` is likewise a typed failure: it may itself
  be a lab file and must not be reused or deleted by a refuse-existing write.
- The raw-byte signature deviation follows the existing measured analyzer
  repair recorded in 0126-impl-log.md; no new carrier or compatibility overload
  was introduced.
- Source diff review showed matching normal/ignore-cr-at-eol numstats and no
  removed test lines. This is source inspection, not an executed gate result.

# Changelog

- 2026-11-07: Implemented kit copy-in/location, shared non-overwriting atomic
  publication, in-memory deletion and ten prescribed contract cases; returned
  the completed slice for supervisor verification without running gates.
- 2026-11-07 (attempt 02): Repaired staging acquisition and cleanup ownership;
  extended the existing refusal fact at the writer and copy-in boundaries,
  preserving test identities, overwrite behavior and the shared lock guard.
