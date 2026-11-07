# Slice 0129 implementation plan

## Attempt 02 repair

Repair the supervisor-measured ELEV010P in KitFolderStore.fs without changing the accepted lock behavior or test census. Replace the private bool-returning isKitLockLiveOn member with tryFindLiveKitLockOn : MachineName -> KitLockHolder option, returning the existing holder for a foreign machine or a running local process and None for a missing local process. Update its sole caller to map the returned holder directly into KitLockedErr. Preserve the existing IO exception boundary, owner-first check, acquisition/release paths and all test assertions.

Only KitFolderStore.fs and the three prescribed worker documents need retry edits. No new type, boundary exemption, analyzer configuration or acceptance change is needed. Gate execution remains exclusively the supervisor's responsibility under IMPLEMENT invariant 6; this worker will not claim fresh gate results.

## Original implementation plan

Implement S41-KIT-LOCK in the five authorized source files. Add elevated machine/user identities and holder diagnostics in Kit.fs, then extend KitProxy and the shared in-memory folder with per-proxy lock ownership. Reuse STL ProcessId, ClockTime, the existing canonical JSON stack, typed storage errors and the folder IO boundary. Serialize lock text through the same injected-storage approach already used for in-memory metadata, so readDataFile can observe the stored lock text too.

Write the eleven authorized regression identities first (three inherited contract facts count twice), widen the existing error-rendering fact, and preserve all existing assertions. Cover contention, repeated acquisition, non-owner release, lazy cache creation, owner identity, local dead-process takeover, remote live locks and malformed lock takeover. Keep filesystem fixtures under the test output folder.

Implement filesystem acquisition with a unique temporary file and File.Move without overwrite; clean temporary files, retain .oc on release, and compare stored holders before deleting. No schema or migration is added for disposable cache text. Review ownership, stale-lock handling and failed-IO paths against the immutable S41-KIT-LOCK oracle. Record any substantive implementation choice in the log.

The IMPLEMENT worker protocol assigns all gate/check execution to the supervisor. Do not build, test or run analyzers/invariants locally, claim a passing gate, edit the test census/oracle/checkpoints, bump a version, or mutate git. Write the required log and state-of-the-world at the supplied paths; report gates as unrun and commit readiness as unclaimed.
