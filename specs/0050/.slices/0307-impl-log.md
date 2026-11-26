# Step 0307 — implementation log

## Progress

- [x] Kit.fs: `FileByteContent.kitFileText` (moved from KitFolders.fs) and `FileByteContent.byteOrderMark`
- [x] KitFolders.fs / KitFolderStore.fs: old decode bodies deleted, lock read through `kitFileText`
- [x] ImportProfiles.fs: `LabFileSniff`, `PreviewCell`, `PreviewRow`, `LabFilePreview`, `LabFileSource.sniffedLayout`
- [x] LabFileImport.fs: `KitFileText.previewWith`
- [x] Tests: the sniff fact and the preview fact
- [x] Development runs: build, elevation, constructor-unit-tests, ui-tests (FsxAcceptanceTests), the s30 invariants
- [x] Mutation checks (six, each caught by its own fact; sources restored and SHA-256 verified)

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`
  - `open System.Text`.
  - `KitFileText.withoutByteOrderMark` is now an intrinsic member here. It moved from a `KitFolders.fs` extension and
    its body is unchanged.
  - After `LabFileSource`: `type FileByteContent with` holds `kitFileText` (moved, body unchanged: UTF-8 decode, then
    a leading U+FEFF dropped) and the new `byteOrderMark` (the bytes start with `Encoding.UTF8.GetPreamble()`,
    EF BB BF).
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitFolders.fs`
  - The `KitFileText.withoutByteOrderMark` extension and the `FileByteContent.kitFileText` member are deleted.
  - `kitDataFile`, the in-memory copy-in and the in-memory `readKindFile` now resolve the moved members.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitFolderStore.fs`
  - `tryReadKitLock` loses its own `StreamReader(stream, Encoding.UTF8, true)` decode. It copies the same stream's
    bytes, with the same `FileShare.Read ||| FileShare.Delete`, and calls `kitFileText`.
  - The guard (`KitLockGuardProxy`, 0050 .manual/009) is untouched.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ImportProfiles.fs`
  - After `ColumnRoles.tryOfChoices`: `LabFileSniff`, `PreviewCell` (`.value`), `PreviewRow` (`.cells`) and
    `LabFilePreview`.
  - `type LabFileSource with member this.sniffedLayout`.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/LabFileImport.fs`
  - `KitFileText.previewWith delimiter header` sits in the existing `type KitFileText with` block and goes through the
    private `labFileRows` (step 302).
  - The module doc now names the preview.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ImportProfileTests.fs`
  - `open System.Text` and `open Softellect.Sys.Primitives`.
  - The new fact `sniffing suggests the delimiter, the header and the byte-order mark of a lab file`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LabFileImportTests.fs`
  - The same two opens.
  - The new fact `the preview shows a lab file's columns and first rows as the mapping reads them`.

## Decisions

- **One decode.**
  - The decode body moved to `Kit.fs` unchanged, so every text the kit stores answer is byte-for-byte the same.
  - `withoutByteOrderMark` moved with it. It is the single rule that drops the mark; `kitFileText` calls it, and so do
    the in-memory `readKindFile` and `parseLockHolder`, which hold text rather than bytes.
  - The lock read was the one remaining decode of kit bytes with its own body (`StreamReader` BOM detection). It now
    calls `kitFileText`.
- **Sniff rule, exactly as the slice says.**
  - Lines: split on `\n`, one trailing `\r` trimmed, whitespace-only lines skipped (the CSV read skips blank rows the
    same way), and the first ten kept.
  - Delimiter: the first of `DataDelimiter.all` whose `Split` gives all those lines one count of at least two, else
    comma.
  - Header: `HeaderOnFirstRow` when the first line has no cell that `Double.TryParse(Float, Invariant)` reads, or
    when one of its cells does not read where the second line's cell in that position does. Otherwise `NoHeaderRow`.
  - With no line, there is no number to read, so the sniff is a header.
- **Preview cells are trimmed.** The slice trims the header cells. Row cells are trimmed too, because `mapText` reads
  every cell trimmed (`trimmedOf`) and the preview "shows the cells a mapping reads".
  - This also builds each `PreviewCell` from new text, not by re-wrapping a `DataFileCell`'s `.value`. That re-wrap
    would be an ELEV001 call-site re-wrap, and the slice forbids adding a conversion member.
- **Preview columns without a header.** `Column <n>` runs for n = 1 to the widest of the shown (first twenty) rows.

## Testing state

Development runs of each gate's own command over the final tree. These are not gate verdicts: the gate engine runs
the gates.

- **build:** exit 0, `0 Warning(s)`, `0 Error(s)`, 43 s incremental. The first full build took 78 s; its only two
  warnings are the vendored MathNet `SYSLIB0051`.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 204 s. ELEV004 is excluded, there are no known
  findings, and all 12 projects were analyzed.
- **constructor-unit-tests:** Passed!, 1522 of 1522 (baseline 1520, +2), 0 failed, 0 skipped, 70 s.
  - Both new identities are `Passed` in the TRX.
  - The `KitProxyTests` byte-order-mark facts pass unchanged.
- **ui-tests** (`--filter-method OpticalConstructor.Ui.Tests.FsxAcceptanceTests`): Passed!, 14 of 14, 72 s. Steps 303
  to 305 had the same count.
- **invariant-s30-\*:** all 16 hold.
- **Deadlines:** none exceeded (600 / 600 / 180 / 360 / 300 s).
- **Mutation checks:**
  - Sniff: A (a one-cell split counts), B (no text-over-number), C (no ten-line window) and D (the mark is never
    present) each failed the sniff fact.
  - Preview: E (thirty rows) and F (untrimmed row cells) each failed the preview fact.
- **Red:** every symbol the facts call is new at this step, so the test project could not compile against HEAD.

## Artifacts

- `specs/0050/.artifacts/0307-build-dev-1.log`, `0307-build-dev-2.log`
- `specs/0050/.artifacts/0307-constructor-unit-tests-dev-1.log`, `0307-constructor-unit-tests-dev-2.log`
- `specs/0050/.artifacts/0307-elevation-dev-1.log`, `0307-elevation-dev-2.log`
- `specs/0050/.artifacts/0307-ui-tests-dev-1.log`
- `specs/0050/.artifacts/0307-mutate.py`, `0307-mutation-checks.log`, `0307-mutation-baseline.sha256`

## Gotchas

- **"Five column names" vs step 303's fixture.** The slice says step 303's Phone ROI text, previewed with a header,
  "shows the five column names", and without one shows `Column 1` to `Column 5`. That text (`phoneRoiText` in
  `LabFileImportTests`) has ten columns. The five-column text is the sniff fact's abbreviation.
  - The fact previews the named fixture and asserts its own header: ten names, and `Column 1` to `Column 10`.
  - The header-row and three-row (header among them) shape is exactly as the slice says.
- **The fixture keeps a U+FEFF inside a `KitFileText`.** `phoneRoiText` simulates the old parser path. The preview
  fact encodes it to bytes and decodes through `kitFileText`, as the wizard reads a picked file. Without that, the
  first header cell would carry the mark, because .NET's `Trim` does not remove U+FEFF.
- **xUnit's `Assert.StartsWith(string, string)` is culture-aware.** Under ICU it ignores a leading U+FEFF, so the sniff
  fact passes `StringComparison.Ordinal`, or it would accept a text that kept the mark.
- **The lock read no longer detects UTF-16/32 marks.** `StreamReader` used to. The app writes the lock as UTF-8
  without a mark, and the single-decode rule leaves no second decoder.
- **The sniff fact gained two assertions beyond the slice's four texts.** One covers text over a number (`phi,633`),
  the other the ten-line window with CRLF and a blank line. They stay inside the same fact identity.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.
