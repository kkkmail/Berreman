# Step 0197 — state of the world (ADD_CONTRACT FILE_BigBoy_0001 KitArchiveProxy)

# Where we are

This is S6 of spec 0050, "Send and receive kits". Step 196 added the send rule: `KitTreeEntry`,
`KitFile.sentEntries` and `KitName.zipFileName`. This step declares the zip / unzip seam, `KitArchiveProxy`
(contract `FILE_BigBoy_0001`, now `declared`), with its in-memory double and the contract facts. Step 198 adds
`sendKit` / `receiveKit` and the zip pickers over this seam. Step 199 implements the real Storage `create` over
System.IO.Compression against the same `KitArchiveProxyContract`.

# What's working

- Declared the `KitArchiveProxy` seam in Domain: `listKitTree`, `writeKitZip`, `listZipEntries` and `extractKitZip`, plus `KitArchiveError` and its five rendered cases
- Added zip entry names: `KitTreeEntry.tryOfZipEntryName` refuses any name that escapes its folder, and `zipEntryName` writes one back
- Added per-file progress and cancellation through `KitArchiveObserver` (`unobserved`) and `KitArchiveOutcome`, following `JobRunner.runLoop`'s rule
- Added the in-memory double: `InMemoryArchiveDisk` with `KitArchiveProxy.createInMemory`; cancelled or failed jobs leave nothing behind
- Added `KitArchiveProxyContract` with six facts run by `KitArchiveProxyTests`, plus two `KitArchiveTests` facts (constructor-unit-tests 1220 → 1228)

# Tests

Development runs; the gate engine owns the verdict (Invariant 6).

- `build`: exits 0 with 0 warnings and 0 errors.
- `constructor-unit-tests`: 1228 passed, 0 failed, 0 skipped. The eight declared identities pass in the TRX.
- `elevation` (from the arc base `f395597…`): `ELEV-GATE: OK`, 0 new findings in 12 projects. A first run found
  ELEV010 on a `string` parameter, fixed by `ZipEntryName`.
- The fourteen `invariant-s22-*` greps find nothing.

```yaml
gates:
  constructor_unit_tests: 1228
  ui_smoke_tests: 236
```

`ui_smoke_tests` is carried from checkpoint 194. This step's roster does not run it.

# Architecture

- **One extra type: `ZipEntryName`.** The slice gave `tryOfZipEntryName` a `string` parameter, but ELEV010 rejects a
  bare primitive beside `KitArchiveError`, and no boundary mark exempts it. The analyzers win over the slice text
  (0048 `.manual/002` R6). So a zip entry name is a single-case union, born in `KitTreeEntry.zipEntryName`.
- **The double's state.** `InMemoryArchiveDisk` keeps one private map from a case-folded, forward-slashed name to what
  it holds: a file's bytes, a folder, or a zip's entries with each file's bytes.
  - Its public surface is exactly `create`, `placeFile`, `placeFolder`, `tryDiskFile` and `paths`.
  - Four `internal` members (look up, place, remove, list under a folder) are the only way in.
  - `createInMemory` composes the four proxy fields over them. That matches the boundary of `InMemoryKitFolder`.
- `listKitTree` locates names through `KitFolder.tryKitPathOf`, the kit's one location rule.

# Deferred

Nothing. The real Storage implementation is step 199's `IMPLEMENT_CONTRACT FILE_BigBoy_0001`, and
`sendKit` / `receiveKit` are step 198's. Both are separate steps of the plan, not work left over from this one.

# Gotchas

- `tryOfZipEntryName` takes `ZipEntryName`. Storage wraps `ZipArchiveEntry.FullName` at the interop line, and reads
  `.value` for `CreateEntry`.
- The contract facts place `out` before writing a zip. A real disk cannot create a file in a missing folder, so step
  199's folder-backed subclass can inherit the facts unchanged.
- Both job records have `zip` and `entries`, so annotate job values (`: KitZipJob`, `: KitUnzipJob`) where the type is
  not known from context.
- The double keeps a zip as entries, not bytes. `tryDiskFile` answers `None` for a zip, so zipping a kit that holds a
  zip is `KitArchiveIoErr` in the double.

# Changelog

- 2026-11-09 — Step 0197: declared `KitArchiveProxy` with its errors, entry names, observer, outcome, jobs,
  in-memory disk and `createInMemory`, and added `KitArchiveProxyContract` / `KitArchiveProxyTests` and two
  `KitArchiveTests` facts. Entry names are elevated as `ZipEntryName` (ELEV010).
