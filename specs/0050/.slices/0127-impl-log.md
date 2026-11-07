# Implementation log

## Progress

- Read AGENTS.md, both worker prompts, the project prompt, slice 0127, its resolved gates, naming conventions, the immutable S41-KIND-FILES obligation and the existing kit proxy, folder store, atomic writer and shared tests.
- The working tree initially contains only the supervisor-owned modification to specs/0050/.manifest.state.json. Leave it untouched.
- Before test authoring: add exactly three shared facts and one folder-only fact with the prescribed names. Cover the required behaviors and boundary cases within those identities without changing the existing facts. No checks or gates run under the act-only worker protocol.
- After test authoring: added the three prescribed inherited facts (six concrete identities) and one folder-only fact. They cover every kind with the exact lab names, all kinds coexisting, outside uppercase extensions, ordinal mixed-case/Unicode stems, nested and temporary-file exclusion, BOM reads, missing-file diagnostics and unchanged paths, whole replacement, and exclusive-handle failure followed by successful retry. Existing tests and assertions are unchanged.
- Before implementation: add the four fields and map-backed operations, sharing root-stem selection and BOM removal with Storage. Resolve map paths case-insensitively for the new operations so a filename listed with an uppercase extension can be read, replaced and deleted under the Windows filename rule. Storage will call the existing caught IO wrapper and atomic writer; metadata will reuse the same text writer and BOM handling.
- After implementation: KitProxy has the four documented fields, and both factories implement them generically from KitFileKind.extension/fileNameOf. The in-memory proxy uses the existing shared text map. A shared stemsAtKitRoot member filters root paths and sorts ordinally; withoutByteOrderMark removes one leading mark. Storage catches all path/filesystem operations, shares one readKitText decoder and writeText encoder for metadata and kind files, and delegates atomic writes to writeBytesAtomically with FileByteContent. Missing reads/deletes name the file; failed replacements use the existing cleanup path and map to KitIoErr.
- Before handoff: inspected the source diffs and fact names against the authorized effects. All existing facts remain unchanged. Reused the metadata decoder as a private FileName member for both reads and bound per-kind test texts once, avoiding copied codec bodies and fixture strings. Complete source/line-ending review and write the state-of-the-world without running gates.
- After handoff review: the only source changes are in the three authorized files, existing tests have no deleted lines, and all three source diffs have matching numstats with and without ignoring CR at line ends. Wrote 0127-state-of-the-world.md with the required headings and inherited checkpoint count. All implementation obligations are addressed; no unresolved scope question or implementation blocker remains. Gate execution and measured results remain with the supervisor.

## Files modified

- Created the implementation plan and this log at the supplied 0127 paths.
- Extended OpticalConstructor.Tests/KitProxyTests.fs with the seven authorized test identities.
- Updated OpticalConstructor.Domain/KitFolders.fs with four proxy fields, shared listing/BOM members and the map-backed operations.
- Updated OpticalConstructor.Storage/KitFolderStore.fs with the real folder operations and shared read/write text helpers.
- Created 0127-state-of-the-world.md at the supplied path.

## Testing state

No checks or gates run. The IMPLEMENT role assigns execution and measured results to the supervisor. The resolved roster is build, elevation, all-discovered constructor-unit-tests and eleven s15 invariants. The authorized test-count increase is seven; it is not a measured result.

The latest inherited constructor count currently present in .checkpoints-json is 1036 at checkpoint 125; checkpoint 126 is absent. This is supervisor data, not a new baseline authored by this worker.

The final source has all seven authorized identities: three facts inherited by each of the two concrete classes and one folder-only fact. No build, test, analyzer or invariant result is self-reported. The structured handoff uses gates_run: [], gate_results: {} and commit_ready: false pending supervisor verification, with no implementation blocker.

## Artifacts

- Worker documents use only the supplied specs/0050/.slices/0127-* paths.
- No persistent run artifacts generated.

## Gotchas

- The per-anchor act-only instruction supersedes the base protocol's legacy local-gate instructions. Gate outcomes and commit readiness remain unclaimed pending supervisor execution.
- No implementation requirement is deferred. No git mutation commands are authorized.
- Case-insensitive resolution in the double is shared by kind-file operations and placeLabFile so uppercase external extensions remain usable after listing, matching the Windows folder backend. The map still owns the original path; neither proxy replacement nor outside-writer replacement creates a second key. The shared all-kinds fact covers both replacement routes before deleting the file.
- No durable stored shape changes: kind file contents remain the caller's text. No schema parser, compatibility path or migration is introduced.
