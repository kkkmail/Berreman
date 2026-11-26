# Step 0330 — impl log

## Progress

- [x] `.gitattributes` and AGENTS.md: the byte-for-byte rule for lab files.
  - `.gitattributes` gains, after its `*.nb` rule, a one-line comment citing spec 0050 S9.4 and
    `data/optics-mueller/data/raw/** -text !eol`.
  - AGENTS.md's "Line endings: LF" gotcha gains one sentence: the lab files are the exception, kept byte for byte by
    their `-text !eol` rule, never converted.
  - `git check-attr -a`:
    - the raw CSVs report `text: unset` and no `eol`;
    - the shape JSONs keep `text: set`, `eol: lf`. They are LF-only with no BOM, so this is a no-op on their bytes.
- [x] Extracted `data.zip` into the kit with Python's `zipfile`, writing each entry's exact bytes.
  - The 15 CSVs went to `data/optics-mueller/` + their archive path.
  - The 3 `data/shapes/*.json` went to `data/optics-mueller/data/reference/shapes/`.
  - The archive's entries were exactly the slice's 18 files.
  - Every written file's SHA-256 equals the slice's value: "ALL MATCH", log in `.artifacts/0330-data-move.log`.
  - `git hash-object` with and without filters agrees for all 18, so `git add` stores them unchanged.
  - Then deleted `data.zip`, `Berreman/Data/MuellerMatrix/` and `Berreman/Data/`.
- [x] `BerremanTests.fsproj`:
  - dropped the `data.zip` `Content` `ItemGroup` with its comment;
  - dropped the comment above the Storage `ProjectReference`, which named `createFileBacked`. The reference stays.
  - added `<Compile Include="CommittedKits.fs" />` directly before `MuellerReconstructionTests.fs`, with no comment.
- [x] Kit README rewritten where the data moved:
  - the opening paragraph;
  - the paragraph after the provenance table;
  - the line-ending sentence;
  - the measured-data table row;
  - the inventory paragraphs.

  Every other line keeps its text.
- [x] `CommittedKits.fs`: `type CommittedKit = | OpticsMuellerKit` with `member this.tryFindFolder : KitFolder option`.
  - It walks up from `AppContext.BaseDirectory` to the first folder holding `Berreman/Berreman.slnx`.
  - It answers `KitFolder (FolderName <root>/data/optics-mueller)` when that folder exists, else `None`.
  - It writes nothing and reads no environment variable.
- [x] `MuellerReconstructionTests.fs`:
  - **Opens.** The two `open OpticalConstructor.Storage…` lines lost their trailing comments. Added opens for
    `LibraryEntryFiles` (`ContentHash`), `KitFolders` (`KitProxy`, `readOnlyView`), `ImportProfiles` (the profile
    choice, `CapturePoints`), `KitFolderStore` (`KitProxy.create`) and `LabFileImport` (`mapText`).
  - **Class doc.** It names the Phone ROI CSV parser. Its spec 0044 §8 bullet says the measured data is the kit's lab
    files, found from the repository root and read read-only through the Phone ROI CSV import profile (S9.4).
  - **`loadDataSet`.** It is now `KitRelativePath -> MuellerRawRow list`. It reads through `OpticsMuellerKit.tryFindFolder`,
    `(KitProxy.create folder).readOnlyView.readDataFile` and `(BuiltInProfileChoice PhoneRoiCsvProfile).mapText`. Any
    refusal fails the test with what it says.
  - **The seven `*File` values.** Bound ahead of the members through a local `kitPath` helper over
    `KitRelativePath.tryCreate`; an `Error` fails with its `describe`.
  - **Stage-1 and §7.2 facts.** They bind `load = loadDataSet` and load by file. The §7.2 comment that named
    `createArchiveBacked ()` now names the kit's lab files.
  - **Renamed fact.** `createArchiveBacked produces…` is now
    `the optics-mueller kit's final lab files load through the Phone ROI CSV profile with their documented row counts`.
  - **New fact.** `the optics-mueller kit holds every file of the former data archive byte for byte`.
- [x] Development runs of build, elevation, unit-tests (selected) and the s32 invariants. All green; see Testing state.

## Files modified

- `.gitattributes`
- `AGENTS.md`
- `Berreman/Data/MuellerMatrix/data.zip` (deleted, with `Berreman/Data/MuellerMatrix/` and `Berreman/Data/`)
- `data/optics-mueller/data/raw/*.csv` (8 new) and `data/optics-mueller/data/raw/final/*.csv` (7 new)
- `data/optics-mueller/data/reference/shapes/*.json` (3 new)
- `data/optics-mueller/data/reference/README.md`
- `Berreman/BerremanTests/CommittedKits.fs` (new)
- `Berreman/BerremanTests/BerremanTests.fsproj`
- `Berreman/BerremanTests/MuellerReconstructionTests.fs`

## Testing state

These are development runs of each gate's own command from `0330.gates`, over the final tree.

| Gate | Result | Time |
|---|---|---|
| build | exit 0, 0 errors | 77 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; ELEV004 suppressed count 1453, unchanged | 205 s |
| unit-tests (`BerremanTests.MuellerReconstructionTests`) | Passed!, 30 of 30 succeeded, 0 failed, 0 skipped | 3 s |
| invariant-s32-\* (16) | each `INVARIANT HOLDS`, exit 0 | under 1 s each |

- **Build warnings.** Only the two known `SYSLIB0051` warnings from the vendored MathNet `Exceptions.cs`.
- **The count.** 29 at step 0329, 30 now: the renamed fact plus the one new fact. The TRX lists the renamed and new
  names, and the old name is absent.
- **Second run.** The tests were re-run after deleting `bin/.../Data/` from the build output: 30 of 30 passed. No
  archive copy is reachable, so nothing reads `data.zip`.
- **Numbers unchanged.** The Stage-1 and §7.2 facts pass at their unchanged tolerances.
- **Writes nothing.** No `data/optics-mueller/.oc/` was created by the runs, so the read-only view wrote nothing.
- **LF.** Every edited text file has zero CR bytes, and `git diff --numstat` equals `--ignore-cr-at-eol`.

## Artifacts

- `.artifacts/0330-data-move.log`: per-entry extraction, SHA-256 checks and the filtered/unfiltered blob-hash comparison.
- `.artifacts/0330-dev-build.log`, `.artifacts/0330-dev-elevation.log`, `.artifacts/0330-dev-unit-tests.log`,
  `.artifacts/0330-dev-invariants.log`
- `.artifacts/0330-dev-trx/unit-tests.trx`

## Gotchas

- **`kitPath` is a local helper over `KitRelativePath.tryCreate`.** The slice requires each path to be "born by
  `KitRelativePath.tryCreate` … an `Error` failing the test". The helper is that call plus the failure. The new
  byte-for-byte fact reuses it, so each literal is elevated where it is written.
- **The data move used Python and `rm`, not `git mv`.** The worker protocol forbids git mutation, so git sees a
  deleted zip and untracked kit files until the supervisor stages them. The `.gitattributes` rule is in the working
  tree, so that staging stores the CRLF/BOM bytes unchanged. `git hash-object` with filters equals `--no-filters`
  for all 18 files.
- **`MuellerDataStore.fs` still names `Berreman/Data/MuellerMatrix/README.md` and the archive path.** Step 331
  deletes that seam; the file is outside this step's effects.
- The README's "Every CSV carries the same 16-column header … prefixed with a UTF-8 BOM" was already inexact:
  `cpl_lr45_standard_ellipse_subset.csv` has no BOM. The slice says every other line keeps its text, so it was left.
