# Step 0443 — impl log (ADD_CONTRACT SVC_BigBoy_0002 MaterialSourceProxy)

## Progress

- Read the task, the base and contract-family worker prompts, the project prompt, AGENTS.md, the
  slice and rulings R0001–R0005. All five bind other steps' repairs (0427, 0428, 0436); this step
  touches none of the files they name, so it preserves them by leaving them alone.
- Read the precedents:
  - `KitArchive.fs`: step 197's observer, outcome, in-memory disk and `createInMemory`;
  - `LibraryEntryFiles.fs`: step 24's source ids and `SourcePullInfo`, with step 441's `recordHash`;
  - `BuiltinCatalogue.fs`: test access to the catalogue;
  - `CatalogueImportProxyTests.fs`: the "every error renders" fact.
- Checked every new type and case name for collisions in Berreman and STL: none.
- Contract registry: `.contracts-json` already holds `SVC_BigBoy_0002` as `declared` at step 443, so
  there was nothing to edit.
- Wrote `MaterialSources.fs` in the how-to's order (1)–(7) and its `<Compile>` line directly before
  `CatalogueImport.fs`, with no comment.
- Wrote `MaterialSourceProxyTests.fs` with the census's seven identities and appended it to the end of
  the test compile list, with no comment.
- Development runs: build, elevation, constructor-unit-tests (whole suite, as the gate selects it) and
  the 21 invariant greps. Results are under Testing state.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialSources.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` (one line)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialSourceProxyTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (one line)

These are exactly the step's four authorized edits. `specs/0050/.manifest.state.json` was already
modified when the round started; it is the supervisor's, and this round did not touch it.

## Testing state

These are development runs, evidence only. Each used its gate's own command and selection.

- **build:** exit 0, 85 s against 600 s. The only warnings are the exempt NuGet advisories and the
  vendored MathNet's existing SYSLIB0051 (`Exceptions.cs`), as at step 0442.
- **elevation** (ELEV004 excluded, all 13 projects, no comparison base):
  `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 234 s against 600 s. Run concurrently with the
  constructor suite.
- **constructor-unit-tests** (`all_discovered`, the whole suite as the gate selects it): `Passed!`,
  1772 of 1772, 112 s against 180 s.
  - The count rises by 7 from checkpoint 441's 1765, as the acceptance requires.
  - TRX: all seven `MaterialSourceProxyTests` identities passed.
- **The twenty-one s43 invariants:** each decoded `git grep` finds nothing. The two new files are not
  yet tracked, so `git grep` cannot see them; a direct scan of both for the same patterns also finds
  nothing.
- **Line endings:** all six written files have no CR bytes, and `git diff --numstat` equals
  `--ignore-cr-at-eol`.

## Artifacts

- `specs/0050/.artifacts/0443-dev-build.log`
- `specs/0050/.artifacts/0443-dev-elevation.log`
- `specs/0050/.artifacts/0443-dev-constructor-unit-tests.log` and `0443-dev-constructor-unit-tests.trx`
- `specs/0050/.artifacts/0443-dev-invariants-b64.txt`

## Gotchas

- **`open System` left out.** The how-to says the file opens `MaterialLibrary`, `LibraryEntryFiles` and
  `System.Threading`. The two BCL names `filter` needs (`StringSplitOptions`, `StringComparison`) are
  therefore written in full rather than adding a fourth `open`.
- **Splitting on white space.** `String.Split(Array.empty<char>, RemoveEmptyEntries)` splits on every
  white-space character, so a blank or empty query has no words, and `Array.forall` over no words
  keeps every hit.
- **Cancellation is read before every record, the first included.** The cancelled-update fact cancels
  its token inside the first progress report, so the second record's check is the one that stops the
  update. Nothing is installed until every record is counted, so the held snapshot is untouched.
- **The double keeps what it published after an update.** A second update re-installs the same
  snapshot, as a real source still serving its current version would. The how-to does not say to
  clear it.
- **The `SourceSnapshotUpdated` field is named `snapshot`.** The how-to writes the case
  `of SourceSnapshotInfo`; the field name follows `KitArchiveFinished of files : …` and changes no
  construction or match site.
