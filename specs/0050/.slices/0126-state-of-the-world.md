# Where we are

Slice 0126 implements `STORE_BigBoy_0004` (`KitProxy`) for spec 0050 S4.1 and addresses `S41-KIT-FOLDER`. The real folder implementation, shared atomic byte writer and seven prescribed folder-test identities are written. The contract registry records implementation at step 126 and preserves declaration at step 125. Verification belongs to the arc-runner after this IMPLEMENT_CONTRACT worker exits; no gate outcome is claimed.

Attempt 02 repairs the supervisor's sole measured ELEV010 finding by replacing the shared writer's raw `byte[]` parameter with the existing `FileByteContent`. Both UTF-8 callers elevate their output at its binding. The repair stays within the authorized Storage files and preserves the tests and file behavior.

# What's working

- Added a folder-backed KitProxy that performs no IO at construction and writes kit metadata last on initialisation.
- Shared one atomic byte writer between catalogue text and kit metadata writes, with temporary-file cleanup on failure.
- Reused FileByteContent at the atomic writer and both callers to address the reported ELEV010 signature violation.
- Mapped missing kits, existing kits, rejected metadata and filesystem failures to the required typed errors.
- Added the real-folder contract adapter, five inherited facts and two filesystem facts, including BOM reads and unchanged rejected bytes.
- Updated the proxy documentation and contract lifecycle without changing stored formats or existing contract assertions.

# Tests

The attempt-02 task reports that the prior review/build passed and elevation failed on this one signature. Those are supervisor-supplied diagnostics, not worker results. The repair has not been verified by a new gate run.

No checks or gates run: IMPLEMENT_CONTRACT invariant 6 reserves execution and results for the supervisor. Pending supervisor verification is the resolved build, elevation, all-discovered constructor-unit-tests and eleven s15 invariant roster. The seven added identities are the five unchanged inherited `KitProxyContract` facts under `KitProxyFolderTests` and its two explicitly authorized own facts. The expected test-count increase is seven; it has not been measured.

This YAML repeats the inherited count from checkpoint 124. Checkpoint 125 is absent in the checkpoint file read by this worker. It is neither a newly authored baseline nor a result from this round.

```yaml
gates:
  constructor_unit_tests: 1029
```

# Architecture

The Domain proxy remains a three-field functional record. Storage's factory captures only the supplied `KitFolder`; each operation catches its filesystem and path calls at the boundary. Initialisation preserves existing contents, creates missing content directories and then writes `.kit-json`. Reads use the existing schema parser after UTF-8 decoding and removal of one leading BOM. Replacements and initialisation both use `FileName.writeBytesAtomically`, which owns the moved catalogue write/move/cleanup body. `LibraryFileText.writeAtomically` calls that same writer with no-BOM UTF-8 bytes.

`KitProxyFolderTests` reuses the existing abstract contract unchanged, supplies fresh GUID folders under `AppContext.BaseDirectory`, and adds only the two authorized own facts. No project-file changes or additional test identities are introduced. No durable schema is changed, so no migration or compatibility path is needed.

The atomic writer now accepts STL's existing `FileByteContent` and unwraps it only at `File.WriteAllBytes`. This deliberately changes the slice's literal raw-array signature because AGENTS.md gives the measured analyzers precedence; ELEV010 explicitly does not exempt the mixed signature at an IO boundary. The member name, single writer body and both acceptance paths remain intact. No new type or compatibility overload is needed.

# Deferred

No implementation requirement was deferred. Build, test, analyzer and invariant execution is assigned to the supervisor by the worker protocol.

# Gotchas

- Do not interpret the contract's implemented lifecycle or authored tests as a measured gate pass. Commit readiness remains unclaimed pending supervisor verification.
- Every real test folder is under the test output; no committed kit, user data directory or temporary OS directory is used.
- The catalogue's existing facts and all five shared kit facts retain their assertions.
- `.manifest.state.json` was already modified on entry and remains supervisor-owned; its CRLF warning is unrelated to this worker's LF edits.
- Gate snapshots, the acceptance oracle, test census, checkpoints, analyzer settings and version rings remain untouched.

# Changelog

- 2026-11-07: Implemented the folder KitProxy, shared atomic byte writing, seven authorized folder-test identities and the contract lifecycle update; handed verification to the supervisor.
- 2026-11-07 (attempt 02): Reused FileByteContent in the atomic writer and both UTF-8 callers to repair the reported ELEV010 finding; preserved all tests and left verification to the supervisor.
