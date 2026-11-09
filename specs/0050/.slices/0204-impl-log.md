# Step 0204 — impl-log (IMPLEMENT: a kit's lab file read whole, as bytes)

## Progress

- [x] Red first.
  - The three facts and the on-disk `absoluteName` adapter change were written before the production code.
  - `dotnet build` of `OpticalConstructor.Tests` failed with `FS0039` "The type 'KitProxy' does not define a field,
    constructor, or member named 'readLabFile'" (×3, lines 1067, 1073, 2087), plus the two `FS0072` inference errors
    those cause. No other error.
- [x] `KitProxy.readLabFile : KitRelativePath -> Result<LabFileSource, KitError>` (KitFolders.fs), after
  `readDataFile`, with a one-line doc.
- [x] In memory (`InMemoryKitFolder`):
  - new private member `readInMemoryLabFile`: the stored text's UTF-8 bytes, named
    `FileName "<kitFolder>/<path>"` (`C:/in-memory-kit/...`), or `MissingKitFileErr` naming the path;
  - `readInMemoryKitDataFile` is now `readInMemoryLabFile` + `FileByteContent.kitDataFile`, so the double has one
    read body and its hash is, by construction, `ContentHash.ofBytes` of the bytes `readLabFile` answers;
  - `createKitProxy` binds `readLabFile = this.readInMemoryLabFile`.
- [x] `readOnlyView` passes `readLabFile` through (it writes nothing).
- [x] On disk (`KitProxy.create`, KitFolderStore.fs):
  - new local `readLabSource`: the path's segments joined under the kit folder with the platform separator, the
    bytes read whole; `FileNotFoundException` / `DirectoryNotFoundException` → `MissingKitFileErr` naming the path,
    any other exception → `KitIoErr` through `tryKitFolderIo` (step 126's mapping, unchanged);
  - `readDataFile` is now `readLabSource` + `kitDataFile` (its old body moved into `readLabSource`, so the read
    exists once); `readLabFile = readLabSource`.
  - No cache, retry or other field. The shared lock guard (0050 `.manual/009`) is untouched.
- [x] Tests (KitProxyTests.fs):
  - `KitProxyContract`: `a lab file is read whole by its kit-relative path, with its absolute name in the kit`, so it
    runs under `KitProxyTests` and `KitProxyFolderTests`. Over an initialised folder with `data/raw/run 1.csv`
    placed as `phi,I\n0,0.5\n`, for both the proxy and its `readOnlyView`: `file` = `absoluteName
    "data/raw/run 1.csv"`, `content` = the UTF-8 bytes of that text, `ContentHash.ofBytes content` = `readDataFile`'s
    hash; `data/raw/none.csv` answers `MissingKitFileErr` naming it; `paths` unchanged.
  - `KitProxyFolderTests`: `a lab file's bytes are read as stored, a byte-order mark included`. The test writes
    EF BB BF + UTF-8 `a,b\n` at `data/bom.csv`; the proxy and its `readOnlyView` answer exactly those bytes and the
    file's absolute name; `paths` and the file's bytes are unchanged.
  - `kitFolderAdapter.absoluteName` (on disk) now replaces `/` with the platform separator before `Path.Combine`
    (see Decisions).
- [x] Development runs (below).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitFolders.fs`: the `readLabFile` field;
  `readInMemoryLabFile`; `readInMemoryKitDataFile` over it; the field in `createKitProxy` and `readOnlyView`.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitFolderStore.fs`: `readLabSource`; `readDataFile` over
  it; the `readLabFile` field.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitProxyTests.fs`: the two new facts (three test
  identities); the on-disk `absoluteName` separator normalisation.

## Decisions

- **The on-disk `absoluteName` adapter normalises `/` to the platform separator.**
  - The slice requires `file` = `absoluteName "data/raw/run 1.csv"`, and names the on-disk file by "the path's
    segments joined under the kit folder with the platform's separator".
  - Step 133's adapter was `Path.Combine(folder, path)`, which keeps the `/` of a kit-relative path on Windows
    (`...\<guid>\data/raw/run 1.csv`), so it could never equal the platform name (`...\<guid>\data\raw\run 1.csv`).
  - `absoluteName` is documented as "a kit-relative path's absolute name"; its platform form is still `Path.Combine`
    of the folder and the path. Every other use of it either goes through `tryKitPathOf` (which normalises `\` to
    `/`), opens the file (either separator works), or already passes a `Path.Combine("data", "run.csv")` name with no
    `/`. No existing assertion changed, and every existing fact still passes.
- **One read body per implementation.** `readDataFile` is `readLabFile` plus the decode and hash
  (`FileByteContent.kitDataFile`) in both proxies, so the text, hash and bytes cannot drift apart and the hash of
  `readLabFile`'s content equals `readDataFile`'s by construction.
- **The in-memory absolute name uses the requested path's spelling**, as `readDataFile` answers the requested path,
  not the case-insensitively held one.
- **No other `KitProxy` record needed the field.** Every other record over `KitProxy` (KitProxyTests,
  KitLibraryTests) is a copy-and-update expression.

## Testing state

Development runs only; the gate engine gives the verdict (Invariant 6).

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`): exit 0, 0 warnings, 0 errors.
  Log: `.artifacts/0204-dev-build.log`.
- **constructor-unit-tests** (the gate's command, whole suite): `Test run summary: Passed!`, total 1264,
  succeeded 1264, failed 0, skipped 0 (1261 → 1264, +3). In the TRX:
  - `KitProxyTests.a lab file is read whole by its kit-relative path, with its absolute name in the kit` — Passed;
  - `KitProxyFolderTests.a lab file is read whole by its kit-relative path, with its absolute name in the kit` —
    Passed;
  - `KitProxyFolderTests.a lab file's bytes are read as stored, a byte-order mark included` — Passed.
  Log: `.artifacts/0204-dev-constructor-unit-tests.log`.
- **elevation**: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, exit 0, 191 s, ELEV004 excluded, known findings 0. Run with `ASG_ARC_BASE_COMMIT=f395597c…` (`.arc-base-json`); the arc's `Berreman.slnx` change widens the scope to all 12 projects. Log: `.artifacts/0204-dev-elevation.log`.
- **The fourteen `invariant-s22-*` greps**: each finds nothing (0 lines each).
- **Line endings**: the three files stay `i/lf w/lf`; `git diff --numstat` equals `--ignore-cr-at-eol`.

## Artifacts

- `specs/0050/.artifacts/0204-dev-build.log`
- `specs/0050/.artifacts/0204-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0204-dev-elevation.log`

## Gotchas

- `specs/0050/.manifest.state.json` was already modified at the start of the round (supervisor state); it was not
  touched.
- The on-disk `absoluteName` is the PLATFORM name now. A future fact that compares an `absoluteName` string with a
  `/`-joined on-disk name must normalise the same way, or go through `tryKitPathOf`.
