# Step 0444 — impl log

## Progress

- [x] `CatalogueImportContext.materialSources` (Domain), with `create` setting `[]` (Storage).
- [x] `MaterialSourcePulls.fs`, holding:
  - the error, change and result types;
  - `pullRecord`, `refreshPulls` and `updateSourceSnapshot`.
- [x] `MaterialSourcePullTests` (8 facts): all pass when the class is run alone.
- [x] build: green.
- [x] constructor-unit-tests: 1780 of 1780.
- [x] the 21 s43 invariants: no findings.
- [x] elevation: OK, 0 new findings in 13 projects.
- [x] kit-acceptance: passed, 14 succeeded and 1 skipped (Explicit).
- [x] state-of-the-world written.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/CatalogueImport.fs`:
  - `open OpticalConstructor.Domain.MaterialSources`;
  - `CatalogueImportContext.materialSources : MaterialSourceProxy list`, with its one-line doc.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/CatalogueImportStore.fs`: `create` sets
  `materialSources = []`. Its signature is unchanged.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialSourcePulls.fs` (new), in the brief's
  order:
  - `MaterialSourcePullError`, `SourcePullChange`, `SourcePullResult`, `SourceRefreshInfo` and
    `SourceRefresh`;
  - four private helpers: `putPulledVersion`, `activePulls`, `refreshPull` and `refreshEach`;
  - the `CatalogueImportContext` extension: `pullRecord`, `refreshPulls` and `updateSourceSnapshot`.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`: one
  `<Compile>` line between `CatalogueImport.fs` and `KitLibrary.fs`, with no comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialSourcePullTests.fs` (new):
  `module MaterialSourcePullTests`, 8 facts.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: one
  `<Compile>` line appended at the end of the list, with no comment.

## Decisions

- **One refresh rule, written once.** `pullRecord` (for a held record) and `refreshPulls` share two
  private helpers:
  - `activePulls` takes each entry's newest pulled row, with its held entry, in
    `PulledRecord.inRecordOrder`, and leaves out an `InactiveEntry`;
  - `refreshEach` and `refreshPull` handle each row as absent, unchanged or minted.

  The write is `putPulledVersion`: `sealEntry`, then `proxy.putEntryVersion` with `MoveLatestVersion`.
  It is the import's existing write; no library verb is added.
- **The newest row per entry.** Rows are grouped by entry, and each group keeps its highest version
  (`List.maxBy`). The kept rows are then put in `PulledRecord.inRecordOrder`: record text, then GUID
  text, then version.
- **`pullRecord` keeps only the rows of its record before choosing each entry's newest row.** This is
  the brief's literal order ("keeps the rows of `record` … each entry they name is refreshed").
- **A record held only by retired entries is refused, and nothing is written.** The refusal is
  `PullImportErr (ImportRejectedErr …)`, whose reason names the record and every retired entry's GUID.
  The rule: if any entry the rows name is active, it is refreshed, and the retired ones are left out.
- **A pulled row whose entry the library does not hold is refused.** `tryHeldEntry` answering `None`
  gives `PullImportErr (ImportRejectedErr …)`, naming the record and the entry. Neither store can
  produce such a row, because `pulledRecordsOf` derives from held envelopes. This is the brief's own
  refusal for a missing latest envelope, applied one read earlier. Without it the result would have no
  name.
- **The names in the answers:**
  - an unchanged or absent result carries the held entry's name;
  - a minted result carries the name the refreshed content keeps, which is the latest version's;
  - an added result carries the fetched material's name, which is its hit's name.
- **`Lifecycle` is not opened.** The brief names exactly four opens, so the match qualifies
  `Lifecycle.ActiveEntry` and `Lifecycle.InactiveEntry`.
- **ELEV012 forbids mapping a two-case union to `bool`,** so the retired check matches inside the fold
  rather than through a predicate.

## Operator rulings

R0001–R0005 (steps 0427, 0428, 0436) confine their extra authority to their own steps. This step touches
none of the code they govern: the series dispatch, the analyzer law, the element-identity provider, the
fit runtime and the solver allocation. Each is preserved unchanged.

## Testing state

Development runs only; the supervisor's gate engine is the authority.

- **build:** exit 0, 25 s (incremental) against 600 s. Warnings: 12 × NU1902, which is exempt.
  Log: `.artifacts/0444-dev-build.log`.
- **MaterialSourcePullTests**, run alone: 8 of 8 passed.
- **constructor-unit-tests** (whole suite, `all_discovered`): `Passed!`, 1780 of 1780, 113 s against
  180 s. That is 1772 + 8. Log: `.artifacts/0444-dev-constructor-unit-tests.log`.
- **The 21 s43 invariants:**
  - each decoded `git grep` finds nothing;
  - a direct scan of the two untracked new files with the same patterns finds nothing.
- **elevation** (ELEV004 excluded, all 13 projects, no comparison base):
  `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 263 s against 600 s. Domain has 0 findings.
  Log: `.artifacts/0444-dev-elevation.log`.
- **kit-acceptance** (`KitAcceptanceTests`): `Passed!`, 1410 s against 3600 s.
  - 14 succeeded and 1 skipped: AT-2's ensemble, which is Explicit, and the gate's skip policy is
    `allow`.
  - Both kit file-by-file comparisons passed.
  - Log: `.artifacts/0444-dev-kit-acceptance.log`.
- No development run exceeded its gate's effective deadline.
- After the runs, `git status` shows only this step's files, plus the supervisor's
  `.manifest.state.json`.
- **commit_ready:** yes.

## Artifacts

- `.artifacts/0444-dev-build.log`
- `.artifacts/0444-dev-constructor-unit-tests.log`
- `.artifacts/0444-dev-elevation.log`
- `.artifacts/0444-dev-kit-acceptance.log`

## Gotchas

- **`CatalogueProxy` is declared in `CatalogueFiles`,** not in the Storage module that extends it. The
  test opens both.
- **The cancelled fact publishes two records.** The double reads the cancellation before each record.
  With one record, a cancel raised in the first progress report would come too late, and the snapshot
  would install (as at step 443).
- **In the in-memory store, `putEntryVersion` with `MoveLatestVersion` makes an entry active again.**
  The retired check therefore runs before any write.
