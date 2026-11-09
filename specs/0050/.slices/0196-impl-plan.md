# Step 0196 — impl plan

## Approach

The send rule is pure Domain data (spec 0050 S6; 001 §A, §E, §H). Four pieces:

1. **`Kit.fs`.**
   - `KitPathRole` (`KitAppCachePath` | `KitContentPath`) is declared directly before `KitRelativePath`.
   - `KitRelativePath` gains `appCacheFolder`, `pathRole`, `parentOpt` and `leafName`.
   - The existing `KitRelativePath.kitCachePath` (`.oc`) is the very member the slice names `appCacheFolder`. It is
     renamed, not duplicated, so `.oc` stays written once ("reuse, never copy"). It has no reader outside the three
     authorized product files.
   - `kitLockPath` stays, because tests outside this step's effects read it. It is already built from the cache
     folder, and now builds from `appCacheFolder`.
2. **`KitFolders.fs` / `KitFolderStore.fs`.**
   - The in-memory `.oc` folder entry and the store's cache folder name `.oc` through `appCacheFolder`.
   - The private `parentFolders` walker is rewritten over `parentOpt`, so the last-`/` split is written once.
   - Every answer stays unchanged. The lock guard (0050 .manual/009) is not touched.
3. **New `KitArchive.fs`**, compiled after `KitHistory.fs`:
   - `GlobPathMatch`;
   - `SendExcludeGlob.matchOf`: a recursive matcher over glob and path segments, and inside a segment over
     characters, ignoring case by `OrdinalIgnoreCase`. It has no regex and no bool;
   - `KitSendChoice` and `KitTreeEntry` (with `.path`);
   - `KitFile.sendChoiceOf` and `KitFile.sentEntries`;
   - `KitName.zipFileName`.
4. **Tests.**
   - New `KitArchiveTests.fs` with the four declared facts, appended to the test project's compile list.
   - `KitTests` gains one fact after the kit-relative path fact.

## Interpretation to record

The slice says `**` "matches zero or more whole path segments", and it also requires `data/raw/big/**` not to
match `data/raw/big`. Both hold if a FINAL `**` needs at least one segment, while a leading or inner `**` takes zero
or more. This is the gitignore reading, and it keeps `**` alone matching every real path.

## Risks

- ELEV: new names checked for collisions in Berreman and STL (none found).
- No bool in any signature (ELEV012). Every primitive is spoken only inside members of our types.
- The lock-guard contract of .manual/009 must survive. Only the `cacheFolder` name expression changes.
