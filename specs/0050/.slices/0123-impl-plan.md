# Implementation plan

Implement only slice 0123 and immutable obligation `S41-KIT-PATHS`. Extend `Kit.fs` after `KitAccess` with the relative-path, file-kind, shared file-text and lab-file types, and add the two specified diagnostic errors. Path parsing converts separators and rejects only the specified invalid forms; valid text receives no further normalization. Reuse `FileStem.withExtension` for all five file kinds.

Move the existing SHA-256 body from `CanonicalJson.contentHashOf` to `ContentHash.ofBytes` in `LibraryEntryFiles.fs`. Keep the canonical rendering and UTF-8 encoding unchanged, so the frozen canonical-content hash remains the existing acceptance oracle.

Add exactly the three authorized facts in `KitTests.fs` and `LibraryEntryFilesTests.fs`, and widen the existing kit-error fact without removing its assertions. Cover the required examples and separator/path edge cases within the authorized path fact. No project, schema, version, census or gate changes are needed.

The IMPLEMENT role prohibits worker-run checks. Build, elevation, the complete constructor suite and eleven s15 invariants belong to the supervisor after this session. Record implementation progress and pending verification in the canonical log and state report. Preserve the supervisor's existing `.manifest.state.json` modification.

## Attempt 02 repair

Repair the supervisor's measured ELEV010 at `ContentHash.ofBytes` by reusing `Softellect.Sys.Primitives.FileByteContent`, the existing elevated file-byte type. Change the member's input from `byte[]` to `FileByteContent` and unwrap only at `SHA256.HashData`. Elevate canonical UTF-8 bytes and the two hash fixtures at their origin, keeping the same hash body, expected literals and test identities. Only `LibraryEntryFiles.fs`, `CanonicalJson.fs` and `LibraryEntryFilesTests.fs` need further source edits.

This is a deliberate deviation from the how-to's primitive signature, required by AGENTS.md's analyzer-precedence ruling and the measured ELEV010. The immutable `S41-KIT-PATHS` acceptance behavior remains intact; no new contract type, dependency, suppression or test identity is needed. Preserve all prior kit changes and supervisor-owned state. The supervisor owns validation under IMPLEMENT invariant 6.
