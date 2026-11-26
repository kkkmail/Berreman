# Where we are

Slice 0130 implements S41-CHANGE-CHECK in spec 0050 S4.1. KitProxy now records and compares kit-file stamps independently for each opener, with implementations for the shared in-memory folder and the filesystem folder. The requested production behavior and four prescribed concrete test identities are authored. The IMPLEMENT worker has not run gates or claimed commit readiness; the arc-runner owns acceptance execution.

# What's working

- Added recordStamps and changedKitFiles to both KitProxy implementations.
- Kept each opener's stamp record private and entirely in memory.
- Detected added, changed and removed root kit files in ordinal filename order.
- Acknowledged only successful own writes/deletes while retaining unrelated external changes.
- Reused SHA-256 of stored bytes for filesystem stamps and content equality for in-memory stamps.
- Added the two shared contract facts covering both backends and the required four-identity census increase.

# Tests

No build, tests, analyzers or invariant gates were executed, per the task's IMPLEMENT invariant 6. The pending supervisor roster is the compiled 0130.gates: build, elevation, all-discovered constructor-unit-tests and the eleven s15 invariants. No passing gate result is asserted.

The authored facts cover the exact A/B writer sequence; independent records; repeated queries retaining changes; own initialization, metadata replacement, kind-file writes and deletes; a failed deletion preserving its pending removal; additions, modifications and removals for all five kinds; recording again clearing the report; a fresh unrecorded opener reporting all existing files; no folder writes while recording or opening; ignored lab, nested library, cache and temporary files; same-content metadata/text rewrites; raw BOM changes; case-insensitive extensions; and ordinal ordering with uppercase, lowercase and Unicode filenames. Existing test assertions and identities remain intact.

The YAML repeats the supervisor-owned constructor_unit_tests baseline of 1058 from checkpoint 128, the latest preceding recorded value read in .checkpoints-json. Checkpoint 129 was absent. This is inherited information, not a worker-measured or newly authored baseline. The four authorized identities imply an expected next count of 1062, subject to supervisor measurement.

```yaml
gates:
  constructor_unit_tests: 1058
```

# Architecture

KitProxy gains the two specified Result-returning fields with one-line contract documentation. Its two construction sites initialize empty closure-local maps. There is no stamp file, schema change, migration or change to the lock protocol.

KitRelativePath.tryKitFileName selects only root metadata and files matching KitFileKind.all extensions. Existing kind-file listing reuses this selector. Shared generic map operations update one filename or compare snapshots; identity is case insensitive to match the existing Windows/in-memory file contract, while reports retain actual filename spelling and explicitly use ordinal sorting.

InMemoryKitFolder records metadata as KitFile and other files as their stored KitFileText, including BOMs. Its proxy closures share folder contents but never their recorded maps. Filesystem reads use ContentHash.ofBytes over File.ReadAllBytes, through the existing tryKitFolderIo error boundary. recordStamps replaces its map only after the complete scan succeeds. The atomic writer's successful result records the bytes it was given, so an outside replacement between the write and a later scan remains visible. Successful deletion removes only that entry; failed operations preserve the record.

# Deferred

No requested implementation is deferred. Supervisor gate execution remains pending by protocol. The reload offer belongs to S5.3 and is explicitly outside this slice.

# Gotchas

- changedKitFiles is observational: querying never acknowledges changes.
- Before recordStamps, only successful own writes/deletes have affected the initially empty record.
- Unchanged bytes do not count as filesystem changes, regardless of file timestamps; BOM changes do count.
- Root selection excludes every nested file, even if it has a kit extension, and all temporary files.
- A failed filesystem scan returns KitIoErr without installing a partial baseline.
- specs/0050/.manifest.state.json was already modified by the supervisor and remains untouched by the worker.
- Source diff counts match with and without ignoring CR-at-EOL; no source line-ending churn was introduced.

# Changelog

- 2026-11-07: Implemented per-proxy external kit-file change detection and authored the four authorized contract identities; prepared handoff for supervisor gates.
