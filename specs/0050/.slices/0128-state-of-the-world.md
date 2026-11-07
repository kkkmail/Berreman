# Where we are

Slice 0128 implements S41-DATA-FILES in spec 0050 S4.1. Attempt 02 repairs the judge's metadata counterexample: the in-memory proxy now stores .kit-json as canonical text on initialization and replacement, making it available to readDataFile with its stored-byte SHA-256. The four authorized test identities are preserved. Implementation is complete; the arc-runner owns execution of build, analyzer, test and invariant gates after this IMPLEMENT worker exits. No passing gate outcome or commit readiness is claimed.

# What's working

- Added readDataFile to both kit proxies using the existing KitDataFile contract.
- Reused ContentHash.ofBytes to hash original UTF-8 bytes, including a stored leading BOM.
- Stored in-memory metadata text through the existing Storage serializer so any-file reads include .kit-json.
- Shared folder text decoding, preserved BOM bytes in hashes and returned typed missing-file and IO errors.
- Preserved the four authorized test identities and extended metadata coverage through existing and fresh proxies.

# Tests

No checks or gates run, per the IMPLEMENT worker's invariant 6. The supervisor's pending roster is build, elevation, all-discovered constructor-unit-tests and the eleven s15 invariants in 0128.gates. One new KitProxyContract fact runs under KitProxyTests and KitProxyFolderTests; two additional facts belong to KitProxyFolderTests. The expected increase is four, not a measured result.

The new facts cover the exact data/raw/run 1.csv fixture and an independent SHA-256 calculation; root and non-data files; Unicode/CRLF and empty text; leading and embedded BOMs; fresh proxies and outside replacement; missing files under existing and absent parents; unchanged paths and bytes; BOM-inclusive hashing; and KitIoErr while a FileShare.None handle is held, followed by successful reading after release. Attempt 02 extends the existing shared fact to compare .kit-json against KitFile.fileText and independent SHA-256 after initialization and replacement, through two proxies opened before initialization and fresh proxies. It also confirms typed metadata and unchanged paths. Existing assertions are preserved; no further identity is created or renamed.

The following YAML repeats supervisor-owned data already present in .checkpoints-json: constructor_unit_tests is 1043 at checkpoint 126. Checkpoint 127 was absent when read. This is inherited information, not a baseline authored or measured by this worker.

```yaml
gates:
  constructor_unit_tests: 1043
```

# Architecture

KitProxy gains the elevated field readDataFile : KitRelativePath -> Result<KitDataFile, KitError>. InMemoryKitFolder resolves its existing text map through the existing case-insensitive lookup, encodes the original text to FileByteContent, and uses ContentHash.ofBytes. Returned text uses the existing withoutByteOrderMark member. The operation mutates no folder state.

Attempt 02 moves the KitProxy.createInMemory extension from Domain.KitFolders to Storage.KitFolderStore while retaining its folder-only signature. The composition now supplies serializeKitMetadata, which calls the unchanged KitFile.fileText contract and shared typed error conversion, to InMemoryKitFolder.createKitProxy. That Domain member accepts KitFile -> Result<KitFileText, KitError>; no Storage type or reference enters Domain. Successful initialization and replacement store both the canonical text in texts and the typed KitFile in kitFileOpt through one write operation. Serialization errors change neither. The paths member now derives metadata presence from the actual file map instead of synthesizing a separate entry. Both backends reuse one serializer; no serialization occurs during readDataFile.

Storage joins the relative path beneath the captured KitFolder using the platform separator and reads bytes once. A private FileByteContent.kitFileText member contains the existing decoder moved from FileName.readKitText; metadata, kind-file and data-file reads share it. ContentHash.ofBytes receives the unchanged byte carrier. Missing files or parent directories are caught at the storage boundary as MissingKitFileErr with the relative path. Other exceptions flow through the existing tryKitFolderIo wrapper to KitIoErr. Direct reading avoids an existence pre-check that could misclassify an access failure or race with removal.

There is no durable shape or format change, schema migration, copy-in operation or attachment record. In-process changes are the KitProxy record's readDataFile field, the serializer parameter/public accessibility of the Domain composition member, and the relocated createInMemory extension. The current caller already opens Storage.KitFolderStore, so no caller or project-reference edits are needed.

# Deferred

No implementation requirement was deferred. Verification execution belongs to the supervisor under the worker protocol. Copy-in and attachment remain outside this slice's authorized scope.

# Gotchas

- Source inspection and authored assertions do not establish a passing build or suite; all gate outcomes remain unmeasured here.
- Filesystem fixtures stay under the test output directory. The locked-file assertion uses a normalized full path to match Windows IO diagnostics.
- The source diffs have matching numstats with and without ignoring CR at line ends; no CRLF churn was introduced.
- Only the three authorized source files and the supplied worker documents were edited. The pre-existing supervisor-owned .manifest.state.json modification is untouched.
- No changes were made to gates, census, oracle, checkpoints, analyzer configuration, version rings, notebooks, STL or ASG; no git mutation commands were run.
- KitProxy.createInMemory is now supplied by OpticalConstructor.Storage.KitFolderStore; callers need that module open. InMemoryKitFolder and the proxy contract remain in Domain.

# Changelog

- 2026-11-07: Implemented read-only kit lab-file access, reused stored-byte hashing and text decoding, authored the four prescribed tests and handed verification to the supervisor.
- 2026-11-07 (attempt 02): Made .kit-json readable from in-memory folders through the existing serializer and extended the authorized shared fact for initialization and replacement across proxies.
