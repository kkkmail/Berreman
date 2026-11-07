# Where we are

Step 0134 implements S42-ATTACH in section S4.2 after step 0133 supplied
copy-in and kit-path location. The two Domain members and all six prescribed
concrete test cases are implemented. Verification and acceptance remain with
the arc-runner under the IMPLEMENT act-only protocol.

# What's working

- Added in-place attachment for files already inside the kit.
- Added outside-file attachment through existing collision-safe copy-in.
- Added live unchanged, changed and missing attachment status checks.
- Preserved read-only behavior and non-missing typed errors.
- Added three shared contract facts covering both kit implementations.

# Tests

No worker build, test, analyzer or invariant gate was run, as required by
IMPLEMENT invariant 6. The supervisor runs build, elevation,
constructor-unit-tests and the eleven s16 invariants from 0134.gates after
handoff. No passing gate or new measured test count is claimed.

The three prescribed facts on KitProxyContract are inherited by KitProxyTests
and KitProxyFolderTests, adding six concrete cases. They cover stored-path and
hash selection, different picked bytes for an in-kit source, unchanged paths
and stored texts, outside-copy collision suffixes, unchanged original copies,
external replacement/deletion and all three statuses through readOnlyView.
Additional assertions within these same identities cover missing in-kit
sources, read-only copy refusal and unchanged propagation of KitIoErr.

The count below is inherited from supervisor checkpoint 132. The expected
increase is six cases to 1,092, subject to supervisor measurement and census
verification; no new baseline was authored.

```yaml
gates:
  constructor_unit_tests: 1086
```

# Architecture

Both methods extend KitProxy directly after readOnlyView. attachLabFile uses
tryKitPathOf to choose readDataFile or copyInFile and projects the resulting
KitDataFile.attachment. It never uses supplied content for an in-kit source.
An attachment retains only the kit-relative path and hash of stored content.

attachmentStatus calls readDataFile on every request and delegates comparison
to DataAttachment.statusAgainst. Only MissingKitFileErr is mapped to absence;
every other error passes through unchanged. Existing read-only proxy behavior
applies naturally to both members. There is no new cache, retry, storage
implementation, format, version, dependency or migration.

# Deferred

No requested implementation is deferred. Verification belongs to the
supervisor and has not been performed by this worker.

# Gotchas

- The supervisor-owned .manifest.state.json was already modified at entry and
  remains untouched. No gates, oracle, census, checkpoint or version were edited.
- Missing in-kit sources fail attachment instead of copying the picked bytes;
  missing files become AttachedDataMissing only during status checks.
- Only the two authorized source files changed. Source diff inspection shows
  additions only and matching normal/ignore-cr-at-eol numstats, with no LF churn.
- No shared storage-guard or fixture implementation changed.

# Changelog

- 2026-11-07: Implemented kit attachment and live status members, added six
  contract cases, and returned the completed slice for supervisor verification.
