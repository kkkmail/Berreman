# Implementation log

## Progress (attempt 01)

- Read AGENTS.md, both worker prompts, the project prompt, naming conventions, slice 0128, its resolved gates, S41-DATA-FILES in the immutable acceptance oracle, the cited manual rules and the existing kit types, proxy factories and shared tests.
- The working tree initially has only the supervisor-owned modification to specs/0050/.manifest.state.json; leave it untouched.
- Before test authoring: add exactly the shared lab-file fact and two folder-only facts named by the authorized effects. Extend their coverage without changing existing assertions or creating further test identities. No checks or gates run under the IMPLEMENT act-only instruction.
- After test authoring: added the shared fact (two inherited identities) plus the two prescribed folder facts. Coverage includes the exact CSV fixture, root and non-data paths, Unicode/CRLF and empty files, leading versus embedded BOMs, fresh proxies and outside replacement, missing files with existing or absent parents, unchanged paths/bytes, the BOM-inclusive independent SHA-256 and exclusive-handle failure followed by successful read. Existing facts and assertions remain intact.
- Before implementation: add the map-backed read and the filesystem read, hashing FileByteContent through ContentHash.ofBytes. Move the existing filesystem decoder to a private FileByteContent member for reuse. Catch missing-file/directory exceptions at the storage boundary and let tryKitFolderIo translate every other IO failure, without an existence pre-check that can hide access failures.
- After implementation: both factories supply readDataFile and return the requested KitRelativePath, BOM-free KitFileText and ContentHash.ofBytes. The memory implementation hashes the map's original UTF-8 text; Storage reads the original bytes once and shares its decoder with metadata/kind reads. Storage translates missing files/parents to MissingKitFileErr naming the relative path and uses the existing KitIoErr wrapper for other exceptions. The new operation never writes.
- Before handoff: inspect the authorized source diff and line endings, read the inherited supervisor checkpoint and write the required state-of-the-world. Do not run a build, test, analyzer or invariant gate.
- Source review: the two new filesystem fixtures construct paths with forward slashes; normalize their full filenames through Path.GetFullPath so the exclusive-handle diagnostic assertion matches the Windows exception's native-separator path. This is a fixture correction within the authorized facts, not a test identity change.
- After handoff review: normalized both filesystem fixture filenames, reviewed the three authorized source diffs and found matching numstats with and without ignoring CR at line ends. Existing tests have no removed assertions. Wrote the required state-of-the-world with the inherited checkpoint count. No implementation blocker or unresolved scope question remains; all measured verification remains with the supervisor.

## Attempt 02 progress

- Read the retry task, worker and project instructions, judge cycle 1, authorized effects, S41-DATA-FILES and the current implementation. The three source files and worker documents already contain attempt 01; the supervisor's .manifest.state.json edit remains untouched.
- Confirmed the source-derived counterexample: initialization and replacement set kitFileOpt, but readDataFile only searches texts, and outside placement refuses .kit-json. Existing tests have no metadata readDataFile coverage.
- Before regression extension: add initialization and replacement reads to the authorized shared lab-file fact, retaining its current assertions and identities. Compare against KitFile.fileText and independent SHA-256, using existing and fresh proxies. No tests or gates will be executed by this act-only worker.
- After regression extension: the shared fact now reads .kit-json after initialization and after another existing proxy replaces the metadata. Its common read assertions compare path, canonical text, independent stored-byte hash and unchanged paths through two previously opened proxies and a fresh proxy. Typed metadata reads also confirm the shared state. All prior lab-file cases and assertions remain in that same fact; no test identity was added or renamed.
- Before production repair: move only the createInMemory extension into Storage, retaining its signature, and inject one typed serialization function into the Domain folder's proxy composition member. Reuse KitFile.fileText and the existing error mapper for both backends. Store successful metadata writes in texts and kitFileOpt together; derive paths from texts instead of synthesizing metadata presence. This small wiring adjustment is necessary because the canonical serializer belongs to Storage and cannot be referenced from Domain.
- After production repair: Storage's serializeKitMetadata calls the unchanged KitFile.fileText serializer and shared KitFileRejectedErr conversion. Both filesystem writes and the relocated createInMemory factory use it. The Domain double stores that text in its map on successful initialization/replacement; readDataFile needs no metadata special case, and paths now reports the map's actual files. Serialization failure returns before changing the metadata state.
- Source review confirms the only current createInMemory caller already opens OpticalConstructor.Storage.KitFolderStore, so moving the extension needs no caller or project-reference edits. Domain's createKitProxy now publicly accepts the elevated serialization function; createInMemory retains its existing folder-only signature. These are in-process composition changes only, with no new serializer or durable format.
- Before handoff: update the state-of-the-world to include the metadata repair and factory relocation, retain the inherited supervisor checkpoint value, and inspect the final source/document diff without executing any gate.
- After handoff review: updated all three supplied worker documents and reviewed the final diff. Source numstats match with and without ignoring CR at line ends, and the test-file diff removes no pre-existing assertion. The working tree still contains only the authorized source files, worker documents and the untouched supervisor state edit. No gate, test, build or analyzer was executed; handoff remains pending supervisor verification.

## Files modified

- Created 0128-impl-plan.md and this log at the supplied paths.
- Extended OpticalConstructor.Tests/KitProxyTests.fs with the four authorized test identities.
- Updated OpticalConstructor.Domain/KitFolders.fs with the documented readDataFile field and map-backed operation.
- Updated OpticalConstructor.Storage/KitFolderStore.fs with relative-path byte reading, typed missing-file errors and the shared decoder.
- Created 0128-state-of-the-world.md at the supplied path.
- Attempt 02 updates those same three source files and three worker documents: serialize metadata into the shared map, compose the double in Storage, and extend the existing shared fact. No additional source file is changed.

## Testing state

No checks or gates run. The pending supervisor roster is build, elevation, all-discovered constructor-unit-tests and the eleven s15 invariants in 0128.gates. The prescribed test-count increase is four; it is not a measured result.

Attempt 02 adds no test identity. Its metadata regression is inside the same inherited shared fact and covers initialization, replacement by another existing proxy, canonical text, independent byte hashes, typed metadata and unchanged paths. No passing or failing execution is claimed.

The latest inherited constructor count present in .checkpoints-json is 1043 at checkpoint 126; checkpoint 127 is absent. The state-of-the-world repeats that supervisor data and does not create or modify a baseline.

The structured handoff uses gates_run: [], gate_results: {} and commit_ready: false pending supervisor verification. All implementation obligations are addressed; no build, test, analyzer or invariant outcome is self-reported.

## Artifacts

- Worker documents use only the supplied specs/0050/.slices/0128-* paths.
- No persistent run artifacts generated.

## Gotchas

- The per-anchor act-only instruction supersedes the base protocol's legacy local-gate instructions. Gate outcomes and commit readiness remain unclaimed pending supervisor execution.
- No git mutation commands, published spec/census/oracle changes, analyzer waivers, notebook edits, version bumps or writes to STL/ASG are authorized.
- Missing-file and missing-parent exceptions are handled specifically at the storage IO boundary. This preserves the required MissingKitFileErr while avoiding an existence pre-check that could mask other IO failures; other errors still use the established KitIoErr wrapper.
- No durable format change or migration is introduced. Copy-in and attachment are excluded by the slice, and no implementation requirement is deferred.
- The createInMemory extension now lives in OpticalConstructor.Storage.KitFolderStore alongside create. The existing caller already opens this module; future callers must open it too. InMemoryKitFolder remains in Domain and accepts the serializer as an elevated Result-returning function, so the dependency direction is unchanged.
