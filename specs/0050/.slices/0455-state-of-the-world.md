# Where we are

Step 0455 is S13.2's PTB OCDB adapter (spec 0050, obligation S132-PTB-ADAPTER):

- step 447 gave refractiveindex.info its adapter over an offline snapshot;
- step 454 gave `MaterialImport` the PTB record reader, `PtbOcdbText`;
- this step adds the second real `MaterialSourceProxy`, `createPtbOcdb`, over an offline snapshot of PTB's GitHub
  repository (`PTB-SR/ocdb`, `master`).

Its update runs the snapshot body that step 447 wrote for refractiveindex.info. That body moved out of
`createRefractiveIndexInfo` into private shapes that both adapters, and later ones, call.

# What's working

- Moved the snapshot body out of `createRefractiveIndexInfo` into the private `SourceSnapshotLayout`
  (`snapshotVersion`, `heldVersion`, `installFrom`, `updateFromArchive`), with `SourceArchiveSpec` and
  `SourceArchiveEntryChoice`. refractiveindex.info's eight facts pass unchanged.
- Added `MaterialSourceProxy.createPtbOcdb`. It keeps `ocdb/db/COPYING` and the `.txt` records of `ocdb/db/data/`,
  lists one hit per record (`<stem> (PTB OCDB)`), and fetches through `PtbOcdbText`, with uncertainties, under the
  file's hash, the stamp's version, `CC-BY-4.0` and PTB's citation with the record's headline.
- Named `createPtbOcdb` in the `MaterialSourceProxy` doc.
- Added the PTB fixture (`ptbRecord`, `changedPtbRecord`, `writePtbSnapshot`, `writePtbArchive`). Its snapshot and zip
  writers are shared with refractiveindex.info's.
- Added four `MaterialSourceStoreTests` facts. constructor-unit-tests went from 1804 to 1808.

# Tests

These are development checks. The gate engine runs the roster after this session and is the authority. Every figure
is from the final build.

- **build:** exit 0 in 48 s, against a 600 s deadline, with 0 errors. The warnings are only the exempt NU1902; there
  is no FS or MSB warning.
- **constructor-unit-tests, the whole suite as the gate selects it** (`all_discovered`): `Passed!`, 1808 of 1808, in
  111 s against a 180 s deadline.
  - That is checkpoint 454's 1804 plus the four new facts, the "count rising by 4" the acceptance asks for.
  - Filtered first, `MaterialSourceStoreTests` passed 12 of 12:
    - the unchanged `an update downloads the archive and replaces the snapshot with its catalog and data, file by
      file` and S13.1's seven other facts;
    - `a PTB OCDB source without a snapshot answers no version and refuses search and fetch`;
    - `a PTB OCDB snapshot lists its data records, and a fetched record is the reader's material with CC BY 4.0 and
      its citation`;
    - `a PTB OCDB update keeps the data records and the licence file of the repository archive`;
    - `a PTB OCDB archive without its licence file is refused and the held snapshot kept`.
- **elevation, all 13 projects:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 225s`, against a 600 s
  deadline, with known findings 0.
- **The 21 s44 invariants:** each decoded `git grep` finds nothing, run both as written and with `--untracked`.
- **LF:** the numstat equals the CR-ignoring numstat, and no touched file holds a CR byte.
- No development run exceeded its gate's effective deadline.

```yaml
gates:
  constructor_unit_tests: 1808
```

# Architecture

- **One snapshot body, many adapters.** The shared body is the private record `SourceSnapshotLayout = { source; folder }`
  in `MaterialSourceStore`:
  - `snapshotVersion` reads the stamp, and another schema version is `UnsupportedSnapshotSchemaErr`;
  - `heldVersion` is `SourceSnapshotMissingErr` without a stamp;
  - `installFrom staged clock files` stamps the staged folder, moves the held one to `.old`, moves the staged one in,
    then deletes `.old`. A failed move-in moves `.old` back;
  - `updateFromArchive spec downloader clock observer` is step 447's sequence: clear leftovers, download, list the
    zip, check the one top folder, extract through `KitArchiveProxy.create ()` with cancellation per file, then
    install.

  An adapter supplies only a `SourceArchiveSpec = { url; requiredFile; keep }`, whose paths are relative to the
  archive's top folder, and its own search and fetch.
- **The keep rule selects files, not folders.** The extractor creates each file's parent folders. The choice is the
  two-case `SourceArchiveEntryChoice`, matched in a `List.choose`, never turned into a `bool` (ELEV012).
- **PTB has no index file.** Its search lists the `.txt` files of `ocdb/db/data/` in ordinal order. Its fetch refuses
  anything the search does not list, so `../Co.txt` cannot reach outside the folder.
- **Nothing is added** in the way of a retry, cache, mirror, second URL, setting, package, schema or stored format
  (.spec-md §8). PTB's snapshot uses the same `.snapshot-json` stamp at schema version 1.

# Deferred

Nothing.

# Gotchas

- **`EntryName` is in `OpticalConstructor.Domain.MaterialLibrary`.** A Storage file that renames a fetched material
  must open it.
- **In this Git Bash, `grep -c $'\r'` counts every line.** Count CR bytes with `tr -cd '\r' | wc -c`, or use
  `git ls-files --eol`.
- **A PTB stamp without `ocdb/db/data/` searches to `SourceSnapshotIoErr`,** not to an empty list. An update always
  installs that folder beside the required licence file, unless the archive holds no record.

# Changelog

- 2026-11-17, step 0455: moved the snapshot body into the private `SourceSnapshotLayout` and `SourceArchiveSpec`;
  added the PTB OCDB adapter `MaterialSourceProxy.createPtbOcdb` over `ocdb/db/data/*.txt`, licensed by
  `ocdb/db/COPYING` (CC BY 4.0); added the PTB fixture and four facts; constructor-unit-tests went from 1804 to 1808.
