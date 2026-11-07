# Where we are

Slice 0127 implements S41-KIND-FILES in spec 0050 S4.1. Both KitProxy factories now provide the four generic per-kind file operations, and all seven prescribed test identities are authored. Implementation is complete; build, analyzer, test and invariant execution belongs to the arc-runner after this IMPLEMENT worker exits. No gate outcome or commit readiness is claimed.

# What's working

- Added list, read, write and delete operations for every KitFileKind to both KitProxy implementations.
- Shared ordinal root-only stem selection and leading-BOM removal between the memory and folder backends.
- Reused the existing atomic byte writer for UTF-8 replacements and typed IO failures.
- Preserved external filename casing in the memory map while resolving kind files case-insensitively.
- Added six inherited contract cases and the exclusive-handle failure case, retaining every existing assertion.

# Tests

No checks or gates run, as required by the IMPLEMENT worker's invariant 6. The pending supervisor roster is build, elevation, all-discovered constructor-unit-tests and the eleven s15 invariants in 0127.gates. The three new KitProxyContract facts run under both KitProxyTests and KitProxyFolderTests; the latter gains one own fact. The expected increase is seven, not a measured count.

Coverage within those identities includes every kind coexisting, exact lab stems, ordinal mixed-case and Unicode ordering, uppercase extensions, nested and temporary-file exclusion, reads after proxy/outside replacements, leading BOM removal, unchanged missing-file operations, no-BOM UTF-8 bytes, complete shorter replacements, and preservation/cleanup after a locked-target failure followed by successful retry.

The YAML repeats the latest inherited constructor count found in .checkpoints-json, checkpoint 125. Checkpoint 126 was absent when read. It is existing supervisor data, not a new baseline or a result from this round.

```yaml
gates:
  constructor_unit_tests: 1036
```

# Architecture

KitProxy remains a functional record with four new elevated fields. InMemoryKitFolder keeps one text map; kind-file operations and its outside writer resolve existing paths case-insensitively so replacing a file with a differently cased extension cannot introduce a second map key. A KitFileKind member selects matching root paths by OrdinalIgnoreCase extension and sorts the resulting stems ordinally; both implementations call it.

Storage enumerates top-level files and performs every operation inside tryKitFolderIo. Reads share one private FileName.readKitText UTF-8 decoder and KitFileText.withoutByteOrderMark; metadata continues through its existing schema parser. Metadata and kind-file writes share writeText, which elevates encoded bytes to FileByteContent and delegates to the existing FileName.writeBytesAtomically. No atomic-write body or parser was copied. Missing kind files return MissingKitFileErr naming the file; IO failures remain KitIoErr.

There is no durable schema change, migration, fallback reader or per-kind format interpretation. FileStem.ofText and KitFileKind.fileNameOf remain the naming authorities. The text remains the caller's.

# Deferred

No implementation requirement was deferred. Verification execution is assigned to the supervisor by the worker protocol.

# Gotchas

- Gate results are unmeasured; authored coverage and source inspection do not establish a passing build or suite.
- The folder failure fact uses FileShare.None under Windows and fresh folders beneath the test output directory.
- Existing shared and folder facts are unchanged. No new test class, project entry or test identity beyond the seven authorized cases was added.
- The source diffs have identical numstats with and without ignoring CR at line ends; no CRLF churn was introduced.
- The pre-existing .manifest.state.json modification is supervisor-owned and untouched. Gate snapshots, census, oracle, checkpoints, analyzer configuration, version rings, notebooks, STL and ASG are unchanged.

# Changelog

- 2026-11-07: Implemented generic kit-kind file operations in both backends, reused atomic UTF-8 IO, added the seven authorized cases and handed verification to the supervisor.
