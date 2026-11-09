# Step 0198 — state of the world

## Where we are

Spec 0050, section S6 (send and receive kits). Steps 196–197 built the `KitArchiveProxy` seam, its in-memory double
and its contract tests. This step adds the two Domain operations over that seam: Send kit… (`sendKit`) and Open kit
from zip… (`receiveKit`). It also adds the `KitZipPicker` record that step 200's Avalonia dialogs will fill. Step 199
adds the real System.IO.Compression proxy.

## What's working

- Add `KitArchiveProxy.sendKit`: zips the kit file's sent entries, never `.oc` or what `sendExclude` leaves out, and
  refuses a zip inside the kit it holds (`KitZipInsideKitErr`).
- Add `KitArchiveProxy.receiveKit`: refuses a zip without a root `.kit-json` (`KitZipHoldsNoKitErr`) and never
  extracts a zip's app cache.
- Declare `KitZipPicker` (`pickZipToWrite`, `pickZipToOpen`) with `KitZipPicker.none`.
- Add five `KitArchiveProxyContract` facts covering send, the inside-kit refusal, a byte-for-byte receive, the no-kit
  refusal, and that the app cache is not received.

## Tests

These are development runs, recorded in `0198-impl-log.md`; the gate engine gives the verdict.

- **build:** 0 warnings, 0 errors.
- **constructor-unit-tests:** 1233 passed (1228 + 5).
- **elevation** (from the arc origin): OK, 0 new findings.
- **The fourteen s22 invariants:** each finds nothing.

```yaml
gates:
  constructor_unit_tests: 1233
```

## Architecture

- **Send and receive are members on the proxy record (an extension), not new proxy fields.**
  - The seam stays IO-only.
  - What travels is the kit's rule (`KitFile.sentEntries`, `KitRelativePath.pathRole`), composed above the four IO
    fields.
  - Every proxy, the in-memory double and step 199's real one alike, therefore gets the same rule.
- **The inside-kit refusal is decided before any IO**, by `KitFolder.tryKitPathOf`, so nothing is listed or written.
- **The no-kit check runs on the zip's listed entries before extraction**, so a refused zip leaves no target folder.
- **`KitZipPicker` follows `KitFolderPicker`** (a `[<ReferenceEquality>]` record of async functions answering `None`
  on cancel). `none` is the picker of a scene that picks nothing.

## Deferred

Nothing in this step's scope. Later steps, as the spec already plans them:

- step 199: the real System.IO.Compression proxy;
- step 200: the job host and the Avalonia save/open dialogs that fill `KitZipPicker`.

## Gotchas

- **ELEV012:** any filter over `KitTreeEntry` or `KitPathRole` (two-case unions) must be `List.choose` /
  `List.tryPick`, never a `bool` lambda.
- **`KitZipJob` and `KitUnzipJob` share field names**, so a record literal of either needs a type annotation.
- **`KitId` lives in `OpticalConstructor.Domain.LibraryEntryFiles`, not in `Kit`.** A test that builds a `KitFile`
  must open both.

## Changelog

- 2026-11-09 — step 0198: `sendKit`, `receiveKit` and `KitZipPicker` in `KitArchive.fs`, plus five contract facts in
  `KitArchiveProxyTests.fs`.
