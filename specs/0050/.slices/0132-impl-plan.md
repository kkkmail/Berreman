# Implementation plan — 0132

Implement S42-ATTACH-SHAPES in the two authorized source files. Reuse KitRelativePath validation, FileName, FileByteContent and ContentHash; add only the prescribed types and members after KitDataFile in Domain/Kit.fs. Copy target selection will be pure, compare occupied paths with OrdinalIgnoreCase, and insert the first free suffix before the last non-leading dot. Kit containment will normalize separators and validate the relative remainder without reading disk. Attachment status will compare the recorded hash with the current file's hash.

First author the four exact facts in OpticalConstructor.Tests/KitTests.fs, preserving all existing tests. Cover every example specified in the slice, plus gaps in suffix numbering and ordinal folder validation within those same identities. Then implement the shapes with concrete member signatures and short S4.2 contract documentation. No storage format, project entry, version or UI change is required.

Risks are filename splitting, case-sensitive data-folder validation versus case-insensitive collisions/containment, and accidental acceptance of traversal or sibling prefixes. The existing path validator remains the only relative-path parser. IMPLEMENT invariant 6 assigns every build, test, analyzer and invariant check to the supervisor after worker exit; this worker will not execute or claim those checks. The final state file will repeat the latest available supervisor checkpoint count as inherited information, not a measured result.

## Attempt 02 — measured lock failure

Preserve the attachment implementation and all four authorized facts. Trace the supervisor-reported failure in KitProxyFolderTests through contendForKitLock to the filesystem guard, and compare the required repair with step 0132's explicit effects. Do not rerun gates, weaken assertions, change test identities or alter the gate selection. If the repair requires KitFolderStore.fs, document the exact authority conflict and affected step-0129 contract in the prescribed log and state file; that storage file is outside this slice's authorized edits.
