# Implementation plan — 0132

## Normal recovery resume — 2026-11-07

The operator committed the approved recovery as `66ac13c`; this dispatch is the
normal profile-1 resume requested by manual 009. Preserve the existing S42
attachment implementation, its four facts and the S41 shared guard repair.
Read the current source and existing tests against both obligations and the
operator's evidence. Do not run build, tests, analyzers, invariant commands or
other gates: IMPLEMENT invariant 6 reserves execution for the supervisor.

Update only this plan, `0132-impl-log.md` and
`0132-state-of-the-world.md` with the recovery review and handoff. Distinguish
inherited operator measurements from this worker's source review; retain the
accepted checkpoint's 1,072-case baseline pending supervisor measurement. Do
not modify supervisor state, checkpoints, counters, expected-red records,
published contracts, gate snapshots or the test census, and do not invoke
`--accept-current-slice`. Normal supervisor acceptance owns checkpoint 131 and
the expected measured floor of 1,076.

Completed: reviewed both obligations and their existing coverage, found no
repair needed, and updated the reports for supervisor verification. Production
code, tests and supervisor-owned records remain unchanged by this worker.

## Original implementation plan (historical)

Implement S42-ATTACH-SHAPES in the two authorized source files. Reuse KitRelativePath validation, FileName, FileByteContent and ContentHash; add only the prescribed types and members after KitDataFile in Domain/Kit.fs. Copy target selection will be pure, compare occupied paths with OrdinalIgnoreCase, and insert the first free suffix before the last non-leading dot. Kit containment will normalize separators and validate the relative remainder without reading disk. Attachment status will compare the recorded hash with the current file's hash.

First author the four exact facts in OpticalConstructor.Tests/KitTests.fs, preserving all existing tests. Cover every example specified in the slice, plus gaps in suffix numbering and ordinal folder validation within those same identities. Then implement the shapes with concrete member signatures and short S4.2 contract documentation. No storage format, project entry, version or UI change is required.

Risks are filename splitting, case-sensitive data-folder validation versus case-insensitive collisions/containment, and accidental acceptance of traversal or sibling prefixes. The existing path validator remains the only relative-path parser. IMPLEMENT invariant 6 assigns every build, test, analyzer and invariant check to the supervisor after worker exit; this worker will not execute or claim those checks. The final state file will repeat the latest available supervisor checkpoint count as inherited information, not a measured result.

## Attempt 02 — measured lock failure

Preserve the attachment implementation and all four authorized facts. Trace the supervisor-reported failure in KitProxyFolderTests through contendForKitLock to the filesystem guard, and compare the required repair with step 0132's explicit effects. Do not rerun gates, weaken assertions, change test identities or alter the gate selection. If the repair requires KitFolderStore.fs, document the exact authority conflict and affected step-0129 contract in the prescribed log and state file; that storage file is outside this slice's authorized edits.
