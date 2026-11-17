# Step 0457 — impl log

## Progress

1. Read the protocol and its IMPLEMENT delta, the project prompt and AGENTS.md, rulings R0001–R0005 (`.manual/030`,
   `031`, `033`, `035`, `036`, each "Binding ruling"), and the slice.

   None of the rulings names this step's files. Each is preserved by leaving its files untouched (StudyEngine,
   ItemEvaluation, StudyForward, TableAndElementRotationView, FittedProblems, BerremanMatrix, Fields, the two server-GC
   project files).
2. Wrote the impl-plan and this log's skeleton.
3. **Reader.** Added `HenkeScatteringText` and its `factors` member to `MaterialImport.fs`, and opened `HenkeCompounds`.
4. **Adapter.** In `MaterialSourceStore.fs`:
   - added `KitFileText.toHenkeScatteringText` and the private `SourceSnapshotDownload` record;
   - moved the archive update's local leftover clearing into one `SourceSnapshotLayout.clearLeftovers` member, which
     both updates share;
   - added `SourceSnapshotLayout.updateFileByFile` and `MaterialSourceProxy.createHenkeCompounds`.
5. **Domain docs.** Two doc edits in `MaterialSources.fs`.
6. **Fixtures.** Added `fixtures/si.nff` and `fixtures/o.nff`, with two `Content` items after `ptb-Co.txt`'s.
   `MaterialSourceFixture` gained `henkeFile`, `writeHenkeSnapshot` and `servingDownloader`.
7. **Tests.**
   - `MaterialImportTests` gained one fact.
   - `MaterialSourceStoreTests` gained five facts. Its table-matching helper became `tabulatedTableOf`, which the
     existing `tabulatedUncertaintyOf` now calls.
8. **Build.**
   - The first build failed with FS1161. A shell edit had written literal TABs for `\t` in two lines of
     `MaterialImport.fs`; both were restored to `\t`.
   - The second build was green.
9. **Filtered run of the two touched classes:** `Passed!`, 40 of 40. `--list-tests` shows the census's six identities
   exactly.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/MaterialImport.fs`: `HenkeScatteringText` and `factors`.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/MaterialSourceStore.fs`:
  - `toHenkeScatteringText`, `SourceSnapshotDownload`, `clearLeftovers` and `updateFileByFile`;
  - `createHenkeCompounds`;
  - the module doc now names CXRO's element files beside GitHub archives.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialSources.fs`: the proxy doc and the `searchRecords`
  field doc.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/fixtures/si.nff`, `fixtures/o.nff` (new).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: two `Content` items, no
  comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialSourceFixture.fs`: three helpers and their opens.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialImportTests.fs`: one fact and its `RefusedHenkeText`
  record.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialSourceStoreTests.fs`: five facts, their helpers and
  `tabulatedTableOf`.

## Decisions

Each is the reading most consistent with the brief and the surrounding code.

- **The file-by-file update is a member of `SourceSnapshotLayout`** (`updateFileByFile`), over a list of private
  `SourceSnapshotDownload` records: each holds a URL, a file name and a check.
  - The layout's doc calls it "the one body every adapter's version, install and update run".
  - Its staging and cleanup members (`unpackedFolder`, `retiredFolder`, `deleteFolder`, `cleanQuietly`) are private to
    it.
  - The Henke-specific parts (the base URL, `<symbol>.nff`, the `factors` check that answers `SourceArchiveInvalidErr`
    naming the element) come in as data from `createHenkeCompounds`.
  - Staging is `<folder>.tmp`, and the install is the layout's `installFrom` with the download count, 92.
- **One leftover rule.** The archive update's local `clearLeftovers` became the private member
  `SourceSnapshotLayout.clearLeftovers`, which both updates call. It removes `<folder>.zip.tmp`, `<folder>.tmp` and
  `<folder>.old`. A Henke run therefore also removes a stray `.zip.tmp`, which does no harm. The archive update's
  behaviour is unchanged.
- **The record field is `fileUrl`, not `url`,** so record construction never has to choose between it and
  `SourceArchiveSpec.url`.
- **The density's text.**
  - `CompoundSpec`'s G6 `densityText` is private to Domain, and `HenkeCompounds.fs` is outside this step's edits.
  - The description and citation therefore render the density as G6 in the invariant culture, the same rule, in a
    local of `createHenkeCompounds`. A comment cites the reason.
  - Parsing `recordId` back instead would be a second reader of the record format.
- **`HenkeScatteringText.factors` refusals:**
  - A header that is not CXRO's names line 1 (or wherever the first non-empty line is). A row that is not three
    numbers names its 1-based line.
  - A table `AtomicScatteringFactors.tryCreate` refuses is `MalformedSourceFileErr` carrying the refusal's `describe`.
    That text names the index and the energies, not a line: the refusal carries no line, and recomputing one would copy
    `tryCreate`'s rule.
  - A text with no non-empty line has no CXRO header, so it is `MalformedSourceFileErr`. A header alone is `NoDataErr`.
  - Rows are read through the shared ordered collector `ItemEvaluation.sequenced`, as the PTB reader does.
- **Search** applies no `query.filter`: the source has no catalog, so its one hit is the compound the query writes, as
  the brief says. A formula element without its `.nff` in the snapshot is `SourceRecordMissingErr` naming `'<Symbol>'`.
- **Fetch:**
  - A record `tryOfRecord` refuses is `SourceRecordMissingErr` naming the record.
  - A missing element file is `SourceRecordMissingErr`.
  - A file `factors` refuses, or a table `indexTableWith` refuses, is `SourceRecordUnreadableErr`.
  - An IO exception is `SourceSnapshotIoErr` naming the snapshot folder.
  - The hash covers the argument record's UTF-8 bytes, then each element file's bytes in formula order.
- **The fetch fact updates first.** The brief expects the source version `2026-11-04`, while `writeHenkeSnapshot`
  stamps `2026-11-01` as the brief specifies. So the fact writes that snapshot, then runs an update with
  `servingDownloader` of `henkeFile` and the 2026-11-04 clock, then fetches. `henkeFile` serves the fixtures' own
  `si.nff` and `o.nff` bytes, so the fetched table and hash are those of "the two fixture files", as the brief asks.
- **Extra assertions inside the census's facts.** No identity is added or renamed.
  - The update fact checks each installed file's bytes.
  - The cancel/fail fact also checks that a file `factors` refuses stops the update at Fe with
    `SourceArchiveInvalidErr` naming `'Fe'` and keeps the held snapshot.
  - The fetch fact checks the category and the description. It also checks that a record without `@` is
    `SourceRecordMissingErr`.
- **One table-matching helper in `MaterialSourceStoreTests`.** The new `tabulatedTableOf` holds the pattern, and the
  existing `tabulatedUncertaintyOf` now reads `(tabulatedTableOf material).uncertaintyOpt`.
- **`henkeFile` for other elements** is the header and two rows, at 100 eV (f1 1, f2 0.5) and 200 eV (f1 2, f2 0.25).

## Testing state

These are development checks only. The gate engine runs the roster after this session and is the authority. Every
figure below is from the final source; nothing was edited after these runs.

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`):
  - exit 0 in 48 s, against a 600 s deadline, with 0 errors;
  - the only warnings are the exempt NU1902; there is no FS or MSB warning;
  - log: `.artifacts/0457-dev-build.log`.
- **The touched classes, filtered** (`MaterialImportTests`, `MaterialSourceStoreTests`):
  - `Passed!`, 40 of 40;
  - `--list-tests` shows the census's six new identities exactly;
  - log: `.artifacts/0457-dev-touched-classes.log`.
- **constructor-unit-tests, the whole suite as the gate selects it** (`all_discovered`, the gate's own command):
  - `Passed!`, 1820 of 1820, in 107 s against a 180 s deadline;
  - that is 1814 plus the six new facts, the "count rising by 6" the acceptance asks for;
  - log: `.artifacts/0457-dev-constructor-unit-tests.log`.
- **elevation, all 13 projects** (standalone, so no comparison base):
  - `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 232s`, against a 600 s deadline, with known findings 0;
  - log: `.artifacts/0457-dev-elevation.log`.
- **kit-acceptance, filtered to `OpticalConstructor.Ui.Tests.KitAcceptanceTests`**, with the gate's command:
  - `Passed!`, 14 succeeded and 1 skipped, in 1434 s against a 3600 s deadline;
  - the skip is AT-2's five-seed ensemble, which is xUnit v3 Explicit; the gate's skip policy is `allow`;
  - both "kit equals the committed kit file by file" facts passed;
  - AT-2's grid-generator fact took 788 s and the read-only reopen fact 571 s;
  - `data/` is untouched (`git status data/` is empty);
  - log: `.artifacts/0457-dev-kit-acceptance.log`.
- **The 21 s44 invariants:** each decoded `git grep` finds nothing (exit 1), run both as written and with
  `--untracked`. Log: `.artifacts/0457-dev-invariants.log`.
- **LF:**
  - the numstat equals the CR-ignoring numstat;
  - the two new fixtures hold no CR byte and no BOM;
  - no edited `.fs` file holds a TAB.
- No development run exceeded its gate's effective deadline.

## Artifacts

All of these are under `specs/0050/.artifacts/`:

- `0457-dev-build.log`
- `0457-dev-touched-classes.log`
- `0457-dev-constructor-unit-tests.log`
- `0457-dev-elevation.log`
- `0457-dev-kit-acceptance.log`
- `0457-dev-invariants.log`

## Gotchas

- **A shell heredoc turned `\t` into literal TABs** in two lines of `MaterialImport.fs`: a doc comment and the
  `Split` character literal. F# refuses TABs (FS1161), so the first build failed. Both lines were restored to `\t`
  through Python's `chr(92)`, since `sed` also expands `\t`. Write F# text that holds escapes with the Edit tool.
- **ELEV004 is measured, not gated** (R7). Storage's suppressed count rose from 210 to 218. These are the new `.value`
  reads in error texts and paths; the gate excludes ELEV004 by command, as AGENTS.md states.
- **`SourceSnapshotDownload` has a function field,** so it is `[<ReferenceEquality>]`, as `SourceArchiveSpec` is.
