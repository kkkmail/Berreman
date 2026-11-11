# Step 0307 — state of the world

## Where we are

Spec 0050 section S9.2 covers import profiles and the column-mapping wizard (001 §K).

- Steps 298 to 306 added the kit's import profiles, their one read of a lab file, attach-with-profile, and the copy of
  profiles between kits.
- This step adds what the wizard (step 308) needs before it can show anything:
  - what a picked file's bytes and first lines suggest for its layout (the byte-order mark, the delimiter and the
    header);
  - a preview of its columns and first rows through the same CSV read the mappings use.
- It also makes `FileByteContent.kitFileText`, now in `Kit.fs`, the kit's one decode.

## What's working

- Move `FileByteContent.kitFileText` into `Kit.fs`, beside the new `FileByteContent.byteOrderMark`, as the kit's one
  decode. The kit lock read now calls it instead of its own `StreamReader` decode.
- Add `LabFileSniff` and `LabFileSource.sniffedLayout`. It suggests the mark from the bytes, and the delimiter and
  header from the first ten non-empty lines.
- Add `PreviewCell`, `PreviewRow`, `LabFilePreview` and `KitFileText.previewWith`. They show the header names (or
  `Column <n>`) and the first twenty rows through step 302's CSV read.
- Two new constructor facts (1520 → 1522), one in `ImportProfileTests` and one in `LabFileImportTests`.

## Tests

These were development runs of each gate's own command over the final tree. The arc-runner's gate engine runs the
gates themselves. No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0; `0 Warning(s)`, `0 Error(s)`; no `error` in stdout | 43 s (incremental; first build 78 s, whose 2 warnings are the vendored MathNet `SYSLIB0051`) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded | 204 s (final tree; the first run, before the last doc and test edits, also OK in 195 s) | 600 s |
| constructor-unit-tests | Passed!, 1522 of 1522, 0 failed, 0 skipped | 70 s | 180 s |
| ui-tests (FsxAcceptanceTests) | Passed!, 14 of 14, 0 failed, 0 skipped | 72 s | 360 s |
| invariant-s30-\* (16) | each exit 0, `INVARIANT HOLDS` (`section_invariants --find-nothing`) | under 1 s each | 300 s |

- **constructor-unit-tests: 1522**, the 1520 baseline plus 2. In the TRX, both new identities are `Passed` under their
  exact names:
  - `ImportProfileTests.sniffing suggests the delimiter, the header and the byte-order mark of a lab file`;
  - `LabFileImportTests.the preview shows a lab file's columns and first rows as the mapping reads them`.
- **The `KitProxyTests` byte-order-mark facts** (steps 128, 133 and 204) pass unchanged within the 1522, as does every
  other kit and history fact.
- **ui-tests: 14** over the selected class `OpticalConstructor.Ui.Tests.FsxAcceptanceTests`. That is the same count
  as steps 303 to 305; no Ui.Tests file changed, and the constructor/export parity with the Analytics examples holds.
- **Elevation** analyzed all 12 product projects (a standalone run with no comparison base), the changed Domain and
  Storage and all their dependents included.
- **Red by construction.** Every symbol the two facts call (`LabFileSniff`, `sniffedLayout`, `byteOrderMark`,
  `LabFilePreview`, `previewWith`, and `kitFileText` from module `Kit`) is new at this step.
- **Mutation check:** six mutations, each failing its own fact.
  - Sniff: a one-cell split counts, text over a number is not a header, every line decides rather than ten, and the
    mark is never present.
  - Preview: thirty rows instead of twenty, and untrimmed row cells.
  - The sources were restored, their SHA-256 verified, and the tree rebuilt before the final runs
    (`.artifacts/0307-mutation-checks.log`).

```yaml
gates:
  constructor_unit_tests: 1522
  ui_tests: 14
```

## Architecture

- **One decode, one mark rule.**
  - `FileByteContent.kitFileText` moved from a `KitFolders.fs` extension to `Kit.fs`, after `LabFileSource`, so
    `ImportProfiles.fs` can reach it. Its body is unchanged, so every text the kit stores answer is the same.
  - The text-level `KitFileText.withoutByteOrderMark` moved with it, as an intrinsic member of `KitFileText`. It stays
    the single rule that drops the mark: `kitFileText` calls it, and so do the in-memory `readKindFile` and
    `parseLockHolder`, which hold text rather than bytes.
  - `byteOrderMark` compares the leading bytes with `Encoding.UTF8.GetPreamble()`, EF BB BF.
- **The kit's reads all decode through `kitFileText`.**
  - `KitProxy.create` reads kit files (`readKitText`), lab and data files (`kitDataFile`), the copy-in, and now the
    lock.
  - `KitProxy.createInMemory`'s copy-in.
  - The lock read keeps its `FileShare.Read ||| FileShare.Delete` stream (step 129). It copies the stream's bytes, then
    decodes. The guard (`KitLockGuardProxy`, 0050 .manual/009) is untouched.
- **The sniff is pure.**
  - `sniffedLayout` is a member over the picked bytes. It touches no disk and never fails: an unsplittable file
    suggests a comma, and a file with no line suggests a header.
  - Lines are split on `\n`, one trailing `\r` is trimmed, whitespace-only lines are skipped as the CSV read skips
    blank rows, and the first ten are kept.
  - The delimiter is the first of `DataDelimiter.all` whose `Split` gives them all one count of at least two.
  - The header rule is the slice's, using `Double.TryParse` with `NumberStyles.Float` and the invariant culture.
- **The preview reads through `labFileRows`**, step 302's private CSV read, so it refuses exactly what a mapping
  refuses.
  - An empty text is its `EmptyDataFileErr`.
  - Without a header, `Column <n>` runs to the widest of the twenty rows shown.
  - No cache, encoding guess or other member was added.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **"Five column names" vs step 303's fixture.** The slice says step 303's Phone ROI text shows "the five column names",
  or `Column 1` to `Column 5` without a header. That text (`phoneRoiText` in `LabFileImportTests`) has ten columns;
  the five-column text is the sniff fact's abbreviation.
  - The fact previews the named fixture and asserts its own header: ten names, and `Column 1` to `Column 10`.
  - The header-plus-two-rows and three-row shapes are exactly as the slice says.
- **Row cells are shown trimmed.** The slice trims the header cells. Rows are trimmed too, because `mapText` reads every
  cell trimmed and the preview "shows the cells a mapping reads".
  - This also avoids re-wrapping a `DataFileCell`'s `.value` as a `PreviewCell` (ELEV001) without adding the
    conversion member the slice rules out.
- **Step 303's fixture holds a U+FEFF inside a `KitFileText`.** The preview fact encodes it to bytes and decodes it
  through `kitFileText`, as the wizard reads a picked file. .NET's `Trim` keeps U+FEFF, so otherwise the first header
  cell would carry the mark.
- **xUnit's `Assert.StartsWith(string, string)` is culture-aware.** Under ICU it ignores a leading U+FEFF. The sniff
  fact passes `StringComparison.Ordinal`, or it would accept a decode that kept the mark.
- **The lock read no longer detects UTF-16/32 marks.** `StreamReader` used to. The app writes the lock as UTF-8
  without a mark, and the one-decode rule leaves no second decoder.
- **`LabFileSniff`'s fields (`bom`, `delimiter`, `header`) are a subset of `ImportProfile`'s.** A record literal of
  exactly those three would infer `LabFileSniff`, so annotate literals: the sniff and its fact both do.
- **The sniff fact gained two assertions beyond the slice's four texts.** One covers text over a number (`phi,633`),
  the other the ten-line window with CRLF and a blank line. Both are inside the same fact identity, so the count
  rises by exactly 2.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11 — Step 0307: sniffing and the preview of a picked lab file (S9.2).
  - `FileByteContent.kitFileText` (moved into `Kit.fs`) is the kit's one decode, the lock read included.
    `FileByteContent.byteOrderMark` is new.
  - `LabFileSource.sniffedLayout` suggests the mark, delimiter and header.
  - `KitFileText.previewWith` shows the header names or `Column <n>` and the first twenty rows, through step 302's
    read.
  - constructor-unit-tests rises by two, to 1522.
