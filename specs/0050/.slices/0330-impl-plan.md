# Step 0330 — impl plan

Move the measured data out of `Berreman/Data/MuellerMatrix/data.zip` into the optics-mueller kit byte for byte, and
read it there through the Phone ROI CSV import profile.

## Approach

1. **Byte-for-byte rule.** Add `data/optics-mueller/data/raw/** -text !eol` to `.gitattributes` after its last rule,
   with a one-line comment citing spec 0050 S9.4. Add one sentence to the AGENTS.md "Line endings: LF" gotcha.
2. **The move.** Extract each archive entry with Python's `zipfile`, writing its exact bytes:
   - the 15 CSVs to `data/optics-mueller/` + their archive path;
   - the 3 `data/shapes/*.json` to `data/optics-mueller/data/reference/shapes/`.

   Check every written file's SHA-256 against the slice's values before deleting `data.zip`. Then remove the empty
   `Berreman/Data/MuellerMatrix/` and `Berreman/Data/`.
3. **Project file.** In `BerremanTests.fsproj`:
   - drop the `data.zip` `Content` `ItemGroup` with its comment;
   - drop the stale comment above the Storage `ProjectReference`;
   - add `<Compile Include="CommittedKits.fs" />` directly before `MuellerReconstructionTests.fs`.
4. **README.** Rewrite the kit README's data paragraphs per the slice (opening, post-provenance, line endings,
   measured-data table, inventory).
5. **Locator.** New `CommittedKits.fs`: `CommittedKit = | OpticsMuellerKit` with `member this.tryFindFolder :
   KitFolder option`. It walks up from `AppContext.BaseDirectory` to the folder holding `Berreman/Berreman.slnx`, then
   answers `<root>/data/optics-mueller` when that folder exists.
6. **Tests.**
   - `loadDataSet : KitRelativePath -> MuellerRawRow list` reads through `KitProxy.create`, `readOnlyView`,
     `readDataFile` and the Phone ROI CSV profile's `mapText`.
   - The seven `*File` paths are bound through `KitRelativePath.tryCreate`.
   - The two data-backed facts load by file.
   - The archive-proxy fact is renamed and loads through `loadDataSet`.
   - New fact: the 18 files' content hashes.
   - Comments are updated per the slice.

## Files

`.gitattributes`, `AGENTS.md`, `Berreman/Data/MuellerMatrix/data.zip` (deleted), the 18 new kit files,
`data/optics-mueller/data/reference/README.md`, `Berreman/BerremanTests/CommittedKits.fs` (new),
`Berreman/BerremanTests/BerremanTests.fsproj`, `Berreman/BerremanTests/MuellerReconstructionTests.fs`.

## Risks

- **Line endings.** The CSVs are CRLF. The `-text !eol` rule must be in place so that `git add` stores them
  unchanged. Verify with `git check-attr` and `git hash-object` vs the raw-bytes blob hash.
- **The BOM.** `kitFileText` drops it, and `parseMuellerCsv` tolerates it too, so the rows should match the
  archive read.
- **Member access on a union case.** `OpticsMuellerKit.tryFindFolder` must resolve as a member access on the case.
  The build will confirm it.
