# Where we are

Step 0168 is the `IMPLEMENT_CONTRACT` step for `STORE_BigBoy_0005` (`KitHistoryProxy`) in spec 0050 S5.1. Earlier
steps built the pieces it uses: the pure history (164), the declared seam with its in-memory double and contract
tests (165), and the history's canonical JSON and schema (166–167). This step adds the real store over the per-user
preferences folder and runs the shared contract over real folders. Wiring the store into the composition root is
step 175. Verification belongs to the arc-runner after this act-only worker exits.

# What's working

- Added `KitHistoryProxy.create` over the preferences folder, writing `kit-history.json` atomically as UTF-8 with no BOM.
- A missing, unreadable or refused history file now reads as the empty history after one logged warning, and is left untouched.
- Render, write and folder failures now return `KitHistoryWriteErr` naming the file or folder, never an exception.
- Added `KitHistoryProxyFolderTests`: the four contract facts over real folders plus four filesystem facts.
- Marked `STORE_BigBoy_0005` implemented at step 168 in the contract registry.

# Tests

No gates were run: under invariant 6 this role does not run checks. The arc-runner still has to run build,
elevation, constructor-unit-tests (all discovered) and the thirteen s19 section invariants.

This step adds exactly eight constructor-unit-test identities under
`OpticalConstructor.Tests.KitHistoryProxyFolderTests`:
- four inherited `KitHistoryProxyContract` facts;
- four own facts, named as the slice gives them.

The count is expected to rise by 8 over the measured count after step 167. That has not been measured here. The
YAML below repeats the latest recorded checkpoint (166). It is not a result of this round.

```yaml
gates:
  constructor_unit_tests: 1152
```

# Architecture

- The Domain record is unchanged. Storage adds its real `create` as an extension beside the Domain's
  `createInMemory`, so the record has exactly one name per factory (ELEV019).
- Building the store only composes the file path, and every .NET IO exception is caught inside the closures.
- The read path is: `File.Exists` → `File.ReadAllBytes` → `FileByteContent.kitFileText` (the shared UTF-8 decoder
  that drops a BOM) → `KitFileText.toKitHistoryText` (a private bridge) → `parseKitHistory`.
- The write path is: render → create the folder → `FileName.writeBytesAtomically`, step 126's single atomic writer.
- No stored format changes, so there is nothing to migrate.

# Deferred

Nothing in this slice is deferred. The composition root starts using `KitHistoryProxy.create` at step 175, as the
spec's step list orders it.

# Gotchas

- A missing history file logs a warning on every read, first run included, because the brief requires one warning
  for every case where the history is empty. Do not "fix" this by dropping the warning.
- The store renders before it creates the preferences folder, so a render failure touches nothing. The brief's
  sentence lists the folder first; this order still creates the folder before the atomic write.
- Test folders are made under `AppContext.BaseDirectory/KitHistoryProxyFolderTests/<GUID>` and, like
  `KitProxyFolderTests`, are not deleted afterwards. Nothing goes under `%TEMP%`, `%APPDATA%` or Documents.
- `.manifest.state.json` was already modified when this worker started. It belongs to the supervisor and was not
  touched.

# Changelog

- 2026-11-08: Implemented the folder-backed `KitHistoryProxy.create`, added the eight `KitHistoryProxyFolderTests`
  identities, updated the proxy doc and the contract lifecycle, and left verification to the arc-runner.
