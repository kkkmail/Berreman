# Step 0165 — impl log (ADD_CONTRACT STORE_BigBoy_0005 KitHistoryProxy)

## Progress

- [x] Declare `KitFolderPresence`, `KitHistoryError`, `KitHistoryProxy` and `KitHistoryProxy.createInMemory` in `KitHistory.fs`.
- [x] Add `KitHistoryProxyTests.fs` (contract + in-memory head) and register it last in the test project.
- [x] Add `a kit history error renders its condition and its reason` to `KitHistoryTests`.
- [x] Attempt 02: repair the measured build failure (`KitHistoryProxyTests.fs(18,21): error FS0039: The value or
  constructor 'KitId' is not defined`) by opening `OpticalConstructor.Domain.LibraryEntryFiles`.

Before: read the slice, the gate snapshot, `KitHistory.fs` / `KitHistoryTests.fs` (step 164), the
`SourceProxy` declaration and `SourceProxy.createInMemory` (`ElementId.fs`), the step-39 `describe` forms
(`Kit.fs`, `CatalogueFiles.fs`, `LibraryBackup.fs`) and the contract-test precedent
`LibraryBackupProxyTests.fs`. After: all four authorized files edited, nothing else.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitHistory.fs` — after `KitHistory`, in the same module:
  - `KitFolderPresence` (`KitFolderPresent` | `KitFolderMissing`), with no tag;
  - `KitHistoryError` (`KitHistoryWriteErr of reason : string`), whose `describe` renders
    `the kit history could not be written: <reason>`;
  - `[<ReferenceEquality>] KitHistoryProxy { readHistory; writeHistory; probeFolder }`, one doc line per field;
  - `type KitHistoryProxy with static member createInMemory (presentFolders : KitFolder list)`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitHistoryProxyTests.fs` (new):
  - `[<AbstractClass>] KitHistoryProxyContract` with abstract `freshHistory`, `presentFolder` and
    `missingFolder` (one doc line each), and the four store facts;
  - `KitHistoryProxyTests` inherits it over `KitHistoryProxy.createInMemory [ this.presentFolder ]`, with
    `C:/in-memory/kits/present` and `C:/in-memory/kits/missing`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitHistoryTests.fs` — adds
  `a kit history error renders its condition and its reason`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` — appends
  `<Compile Include="KitHistoryProxyTests.fs" />` after `KitHistoryTests.fs`, the last compile item, with no comment.

## Decisions

- **The probe uses structural `KitFolder` equality** (`List.contains`), because the brief says "a folder equal to one of
  `presentFolders`". The history's case-insensitive folder match (`changeEntry`) is private to `KitHistory`, and the brief
  does not extend it to the probe.
- **The in-memory store is a closure-private `ref`** starting at `KitHistory.empty`, as `SourceProxy.createInMemory` keeps
  its store. No cache, retry, verb or other field was added.
- **Test fixture:** two entries with fixed GUIDs, `C:/kits/a` starred and `C:/kits/b` unstarred, named `Kit a` and
  `Kit b`. Their instants are `DateTime(2026, 11, 3, 12, 0, 0, Utc).AddTicks n` with sub-second ticks
  (987 654 321 and 123 456 789), so a persisting implementation must keep full precision. The entries are listed newest
  open first (`a` before `b`). The one-entry history is `[ kitB ]`.
- **The error fact checks the exact step-39 text and also that the text contains the reason**, which covers both
  "condition" and "reason" in its name.
- **Contract registry:** `.contracts-json` already records `STORE_BigBoy_0005` as `declared` at step 165. It is not
  among the authorized edits and was not touched; the contract stays `declared`.

## Testing state

As Invariant 6 requires (ADD_CONTRACT, act only), this worker ran no gates: no `dotnet build`, no `dotnet test`, no
analyzer and no invariant. After exit the gate engine runs build, elevation, all discovered constructor-unit-tests and the
thirteen s19 invariants. The only commands run were read-only: reads, `grep`, `git diff --numstat` (with and without
`--ignore-cr-at-eol`, identical, so there is no CRLF churn) and `file` (the new file is ASCII with LF endings).

Compile and ELEV review was done by reading:

- Every name used in the tests resolves through the opens:
  - `FolderName` comes from `Softellect.Sys.Primitives`;
  - `KitId` comes from `LibraryEntryFiles` (`LibraryEntryFiles.fs:135`). Attempt 01 said it came from `Kit`, which was
    wrong, and that is what broke the build;
  - `KitFolder`, `KitName` and `ClockTime` come from `Kit`;
  - `KitStar`, `KitHistoryEntry` and `KitHistory` come from `KitHistory`.
- The new type names (`KitFolderPresence`, `KitHistoryError`, `KitHistoryProxy`) and case names (`KitFolderPresent`,
  `KitFolderMissing`, `KitHistoryWriteErr`) were grepped over `Berreman/` and appear nowhere else (ELEV014 / 016).
- `createInMemory` is a static member, so ELEV013 does not apply.
- The error case ends in `Err` (ELEV020).
- No module-level public name was added (ELEV015).

Expected: constructor-unit-tests +5, all new and none changed or removed. They are 4 facts inherited under
`KitHistoryProxyTests` plus 1 in `KitHistoryTests`, taking the count from the latest visible checkpoint (163: 1,144)
to 1,149. That figure is an expectation, not a measurement.

No implementation blocker remains, and nothing in the acceptance deviates from the brief.

## Attempt 02 — build repair

The supervisor measured the build as red with exactly one error: `KitHistoryProxyTests.fs(18,21): error FS0039: The
value or constructor 'KitId' is not defined`. `KitId` is declared in `OpticalConstructor.Domain.LibraryEntryFiles`
(`LibraryEntryFiles.fs:135`), not in `Kit`, and the new test file did not open that module.

- **Repair:** add `open OpticalConstructor.Domain.LibraryEntryFiles` before `open OpticalConstructor.Domain.Kit` in
  `KitHistoryProxyTests.fs`. This is the same open order as `KitHistory.fs` and `KitHistoryTests.fs`, so no name in
  `LibraryEntryFiles` can shadow one from `Kit` or `KitHistory`. Nothing else changed: no fact, assertion, id or
  Domain line.
- **Why nothing else should fail to compile:** F# keeps type-checking after an unresolved name, and the measured build
  reported no other error in the file. So the remaining lines (`KitName`, `ClockTime`, the abstract members, the
  overrides, the `Assert.Equal` calls) already resolved.
- **Elevation, reviewed by reading** (the gate never ran, because build short-circuits it):
  - `Result<unit, _>` on a proxy field has ELEV-clean precedent: `CatalogueFiles.fs:78` and `CatalogueImport.fs:304–306`;
  - the closure-private `ref` has precedent too: `CatalogueFiles.fs:84` and `CatalogueImport.fs:828`;
  - none of the six new type and case names appears anywhere else in `Berreman/` or in STL (`C:\GitHub\Softellect`).
- **Line endings:** `file` reports the edited file as ASCII with LF, and `git diff --numstat` is the same with and
  without `--ignore-cr-at-eol`.
- **Gates:** the worker ran none (Invariant 6). The gate engine re-runs build, elevation, constructor-unit-tests and
  the s19 invariants after exit.

## Artifacts

None. Nothing was run that produces a capture.

## Gotchas

- `ClockTime` lives in `Kit` (`Kit.fs`), not `WorkbenchSettings`; the new test file does not open `WorkbenchSettings`.
- The four contract facts are declared on the abstract `KitHistoryProxyContract`. xUnit v3 reports them under the
  derived `KitHistoryProxyTests`, which gives the test ids the brief names, as `LibraryBackupProxyTests` does.
- `KEEP_NAME`: none declared. No operator note was in flight.
