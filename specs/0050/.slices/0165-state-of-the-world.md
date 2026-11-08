# Where we are

Step 0165 of spec 0050 is in section S5.1, the kit history over the per-user preferences. It is the
`ADD_CONTRACT STORE_BigBoy_0005` step. It sits on step 164's pure `KitHistory` value (stars, eviction, display
order) and declares the seam that reads and writes that history and probes kit folders on disk. It also ships the
seam's in-memory double and a contract test class. The contract stays `declared`: the real per-user preferences
implementation is the later `IMPLEMENT_CONTRACT STORE_BigBoy_0005` step.

# What's working

- Declare `KitHistoryProxy` (`readHistory`, `writeHistory`, `probeFolder`) in `OpticalConstructor.Domain/KitHistory.fs`
- Add `KitFolderPresence` (`KitFolderPresent` | `KitFolderMissing`) and `KitHistoryError` (`KitHistoryWriteErr`, with `describe`)
- Add `KitHistoryProxy.createInMemory`: it starts empty, keeps the last history written and sees only the given folders
- Add the `KitHistoryProxyContract` facts, run under `KitHistoryProxyTests` over the in-memory double
- Add `a kit history error renders its condition and its reason` to `KitHistoryTests`

# Tests

The roster is build, elevation, constructor-unit-tests (all discovered) and the thirteen s19 invariants. Under
Invariant 6 the worker ran none of them; the gate engine runs them after exit. Five new facts were authored, and no
existing fact was changed or removed. Expected constructor-unit-tests: 1,149 (+5 over the 1,144 at checkpoint 163).
That number is an expectation, not a measurement.

```yaml
gates:
  constructor_unit_tests: 1149
```

# Architecture

- The seam follows the functional-proxy convention of `SourceProxy` (`ElementId.fs`): a `[<ReferenceEquality>]` record of
  functions, built once at the composition root, with `createInMemory` as an intrinsic augmentation in the same module.
- `readHistory` cannot fail: an unreadable or missing history reads as `KitHistory.empty`. Only `writeHistory` returns a
  `Result`, and its one error case carries a reason that names the file or folder and the cause.
- `KitFolderPresence` is runtime-only (probed, never stored), so it has no wire tag.
- The double's probe compares folders structurally ("equal to"). The history's own case-insensitive folder match stays
  private to `KitHistory`.
- Reading the history, changing it through step 164's members and writing it back is the caller's sequence. The proxy
  has no verb beyond read, write and probe.

# Deferred

Nothing. The real store behind `KitHistoryProxy` belongs to the later `IMPLEMENT_CONTRACT STORE_BigBoy_0005` step by the
spec's own design, not by deferral.

# Gotchas

- `ClockTime` is declared in `Kit`, not `WorkbenchSettings`. `KitId` is declared in `LibraryEntryFiles`, not `Kit`: a test
  that builds a `KitHistoryEntry` must open `LibraryEntryFiles` (attempt 01 broke the build on exactly that).
- The contract facts live on the abstract base. A database or preferences-backed head added later inherits
  `KitHistoryProxyContract` and overrides `freshHistory`, `presentFolder` and `missingFolder`. Its `presentFolder` must
  be a folder that really exists.
- The fixture instants carry sub-second ticks on purpose: a persisting store must round-trip `lastOpened` at full precision.
- `.contracts-json` already records `STORE_BigBoy_0005` as declared at step 165; this step did not edit it.

# Changelog

- 2026-11-08 — Step 0165: declared `KitHistoryProxy`, `KitFolderPresence` and `KitHistoryError`, plus the in-memory
  double, `KitHistoryProxyContract` / `KitHistoryProxyTests` (4 facts) and the error-render fact in `KitHistoryTests`.
- 2026-11-08 — Step 0165, attempt 02: fixed the measured build failure (`FS0039 KitId`) by opening
  `LibraryEntryFiles` in `KitHistoryProxyTests.fs`. No fact or Domain line changed.
