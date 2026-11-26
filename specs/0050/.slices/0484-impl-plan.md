# Step 0484 — impl plan

The Planner's cache folder `.oc/planner/` and two `KitProxy` verbs over it (spec 0050 S14.2).

## Approach

1. `Kit.fs`: `KitRelativePath.plannerCacheFolder` = `appCacheFolder` + `/planner` (the text `.oc` written once),
   and one shared name check, `KitRelativePath.plannerCacheFileOf : FileName -> Result<KitRelativePath, KitError>`,
   which refuses a blank name, `.`, `..` or a name holding `/` or `\` with `InvalidKitPathErr` and otherwise
   answers `<plannerCacheFolder>/<name>` through `tryCreate`. Both stores call it, so neither verb can reach
   outside the folder and the rule is written once (reuse, never copy).
2. `KitFolders.fs`: `KitProxy` gains `readPlannerCacheFile : FileName -> Result<KitFileText option, KitError>`
   and `writePlannerCacheFile : FileName -> KitFileText -> Result<unit, KitError>`. `readOnlyView` refuses the
   write (`KitReadOnlyErr "writePlannerCacheFile"`) and passes the read through. The in-memory folder keeps the
   text in its one `texts` map under the kit-relative path and adds `.oc` and `.oc/planner` to its folders; the
   placing body shared with `placeLabFile` moves into one private member.
3. `KitFolderStore.fs`: `KitProxy.create` reads with the BOM dropped (`readKitText`), a missing file or folder
   answering `None`, and writes through `writeBytesAtomically` as UTF-8 without a BOM after creating
   `.oc/planner`. The planner write does not stamp: stamps cover root kit files only, and a stamp for a cache
   file would later be reported as a removed kit file. The KitIoErr mapping of the atomic write is shared with
   `writeText`; the kit-relative-to-absolute name is one local.
4. Tests: both facts go into the abstract `KitProxyContract`, which `KitProxyTests` (in-memory) and
   `KitProxyFolderTests` (folder under the test output) inherit — exactly how every earlier fact of both
   classes was added. Count rises by 4.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitFolders.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitFolderStore.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitProxyTests.fs`

## Risks

- `KitProxyFolderTests.fs` named in the effects does not exist; the class `KitProxyFolderTests` lives in
  `KitProxyTests.fs` and inherits its facts from `KitProxyContract`. A second file declaring the same type would
  not compile, so the facts go in the contract (class identity, not file, is the test's identity).
- The shared lock guard (`.manual/009`) is not touched; the lock code stays byte-identical.
- Elevation: new member signatures are members (exempt from the primitive rules); no free function is added.
