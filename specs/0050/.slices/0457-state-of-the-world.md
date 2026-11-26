# Where we are

Step 0457 is the adapter of S13.2's Henke compound calculator (spec 0050, obligation S132-HENKE-ADAPTER).

- Steps 454 and 455 gave PTB OCDB its reader and adapter.
- Step 456 added the pure Domain physics in `HenkeCompounds`.
- This step makes the calculator a third `MaterialSourceProxy`. Its snapshot is CXRO's 92 per-element `.nff`
  scattering-factor files, fetched once through the one HTTP seam. A record is a compound computed from them.
- Later S13.2 steps wire it to the app.

# What's working

- Added `HenkeScatteringText` and its `factors` reader for CXRO `.nff` files. It reads the `E(eV)` header and leaves
  out rows whose f1 is −9999. Bad text is `MalformedSourceFileErr` naming the line; a file with no rows is `NoDataErr`.
- Added `MaterialSourceProxy.createHenkeCompounds`, a snapshot in `<sources>/cxro-henke` under `LicenseRef-CXRO`:
  - its update fetches H to U file by file into `<folder>.tmp`, through the unchanged `SourceArchiveDownloader`;
  - it checks each file, reports 1 to 92 of 92, and installs through the layout's `installFrom`;
  - a cancelled or failed update keeps the held snapshot.
- The search answers the compound its query writes. The fetch computes the table through `indexTableWith`, under
  CXRO's citation, with a hash over the record and its elements' files.
- The archive update and the file-by-file update share one leftover-clearing member.
- Added the `si.nff` and `o.nff` fixtures, three fixture helpers and six facts; constructor-unit-tests went from 1814 to
  1820.

# Tests

These are development checks. The gate engine runs the roster after this session and is the authority. Every figure
is from the final source.

| Check | Result | Time / deadline |
|---|---|---|
| build | exit 0, 0 errors; only the exempt NU1902 warnings, no FS or MSB warning | 48 s / 600 s |
| constructor-unit-tests, whole suite as the gate selects it (`all_discovered`) | `Passed!`, 1820 of 1820 | 107 s / 180 s |
| elevation, all 13 projects | `ELEV-GATE: OK - 0 new finding(s)`, known findings 0 | 232 s / 600 s |
| kit-acceptance, filtered to `OpticalConstructor.Ui.Tests.KitAcceptanceTests` | `Passed!`, 14 succeeded, 1 skipped | 1434 s / 3600 s |
| the 21 s44 invariants | each decoded `git grep` finds nothing, as written and with `--untracked` | — |
| LF | numstat equals the CR-ignoring numstat; new fixtures hold no CR or BOM | — |

- **constructor-unit-tests:** 1820 is 1814 plus this step's six facts.
  - Filtered first, `MaterialImportTests` and `MaterialSourceStoreTests` passed 40 of 40.
  - `--list-tests` shows the six census identities exactly.
- **kit-acceptance:** the skip is AT-2's five-seed ensemble, which is xUnit v3 Explicit; the gate's skip policy is
  `allow`.
  - Both "kit equals the committed kit file by file" facts passed, so AT-1 and AT-2 are unchanged.
  - `data/` is untouched.
- No development run exceeded its gate's effective deadline.

```yaml
gates:
  constructor_unit_tests: 1820
  kit_acceptance_tests: 14
```

# Architecture

- **The layout runs every update.** `SourceSnapshotLayout.updateFileByFile` is the file-by-file counterpart of
  `updateFromArchive`.
  - It takes a list of private `SourceSnapshotDownload` records, each a URL, a file name and a check.
  - It stages in `<folder>.tmp`, reads the cancellation before each file and after each download, reports after each
    file, and installs through the shared `installFrom`.
  - Both updates clear an interrupted run's leftovers through one private member, `clearLeftovers`.
- **The adapter supplies only data.** `createHenkeCompounds` writes its constants once each:
  - the source name;
  - the `cxro-henke` folder;
  - `LicenseRef-CXRO`;
  - the base URL.

  It builds the 92 downloads from `ChemicalElement.all`, and building it does no IO.
- **The adapter adds no physics and no parser.** Search and fetch read the query and the record through step 456's
  `CompoundSpec.tryOfQuery` / `tryOfRecord`. The table comes from `indexTableWith`, and the entry from
  `TabulatedNkTable.isotropicEntry`. Element files are read through the kit's one decode and
  `HenkeScatteringText.factors`, and errors are collected by the shared ordered collector `ItemEvaluation.sequenced`.
- **No new HTTP, retry, cache or setting.** Every download goes through step 447's `SourceArchiveDownloader`, unchanged.

# Deferred

Nothing.

# Gotchas

- **The fetch fact runs an update before it fetches.** The brief expects the source version `2026-11-04`, while
  `writeHenkeSnapshot` stamps `2026-11-01`. `henkeFile` serves the fixtures' own `si.nff` and `o.nff`, so the expected
  table and hash are still those of the two fixture files.
- **The density is written twice with one rule.** `CompoundSpec`'s G6 `densityText` is private to Domain, and
  `HenkeCompounds.fs` is outside this step's edits. The description and citation therefore render the density with the
  same rule (G6, invariant culture) in Storage.
- **A table `AtomicScatteringFactors.tryCreate` refuses names no line.** It is `MalformedSourceFileErr` carrying the
  refusal's `describe`, which names the index and the energies. A header or row refusal names its 1-based line.
- **Shell heredocs expand `\t` to a TAB,** and F# refuses TABs (FS1161). Edit F# text that holds escapes with the Edit
  tool.
- **ELEV004 is measured, not gated.** Storage's ELEV004 count rose from 210 to 218, all new `.value` reads in error
  texts and paths.

# Changelog

- 2026-11-17, step 0457:
  - added CXRO's scattering-factor reader `HenkeScatteringText.factors`;
  - added the Henke compound adapter `MaterialSourceProxy.createHenkeCompounds`, with its file-by-file snapshot update,
    compound search and computed fetch;
  - added the `si.nff` and `o.nff` fixtures and six facts; constructor-unit-tests went from 1814 to 1820.
