# Step 0484 — impl log

## Progress

- Read the arc-runner system prompt, the IMPLEMENT delta, the project prompt, rulings R0001–R0005 and `.manual/009`
  (the shared lock guard, named for step 0484). R0001–R0005 do not reach the kit proxy and are preserved untouched.
  `.manual/009`'s guard (exclusive delete-on-close guard, bounded retry, ownership rechecks, non-overwriting
  publication) is unchanged: only `lockFile`'s definition now reads the shared `fileAt`, which yields the same name.
- `Kit.fs`: `KitRelativePath.plannerCacheFolder` (`appCacheFolder` + `/planner`, so `.oc` stays written once) and
  `KitRelativePath.plannerCacheFileOf : FileName -> Result<KitRelativePath, KitError>`, the one name check both
  stores call (blank, `.`, `..`, `/` or `\` answer `InvalidKitPathErr`; otherwise `tryCreate` of
  `<plannerCacheFolder>/<name>`, which also refuses a colon).
- `KitFolders.fs`: `KitProxy` gains `readPlannerCacheFile` and `writePlannerCacheFile`, each with a one-line doc.
  `readOnlyView` passes the read and refuses the write with `KitReadOnlyErr "writePlannerCacheFile"`. The in-memory
  folder keeps the text in its one `texts` map under the kit-relative path; the placing body that `placeLabFile`
  held moved into the private `placeText`, which adds `.oc` and `.oc/planner` to `folders`, so `paths` lists them.
  The in-memory read drops a leading BOM as `readKindFile` does.
- `KitFolderStore.fs`: `KitProxy.create` reads through `readKitText` (the kit's one decode, BOM dropped), a missing
  file or folder answering `Ok None` (caught, not pre-checked, so a concurrent delete is not a race); it writes after
  `Directory.CreateDirectory` of `.oc/planner` through `writeBytesAtomically` as UTF-8 without a BOM. The UTF-8 encode,
  atomic write and `KitIoErr` mapping that `writeText` held moved into `writeTextBytes`, which both call; the
  kit-relative-to-absolute name, written four times before, is the local `fileAt`. Other IO exceptions are `KitIoErr`
  through `tryKitFolderIo`.
- `KitProxyTests.fs`: the two facts in the abstract `KitProxyContract`, which `KitProxyTests` (in-memory) and
  `KitProxyFolderTests` (folder under the test output) inherit, giving the four census identities.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs` (+11)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitFolders.fs` (+24 −3)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitFolderStore.fs` (+30 −5)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitProxyTests.fs` (+78)

## Decisions

- **The planner write is never stamped.** `writeText` stamps by leaf name for `changedKitFiles`, whose current scan
  covers root kit files only; a stamp for `a.candidate-json` would later be reported as a removed kit file. The fact
  asserts `changedKitFiles` stays `Ok []` after two cache writes.
- **Blank names are refused, not only the empty one.** "No character" is read as `String.IsNullOrWhiteSpace`, which
  matches `KitRelativePath.tryCreate`'s own blank rule; Windows would otherwise trim a space-only name onto the folder.
- **The name check lives on `KitRelativePath`** (a static member in `Kit.fs`, beside `tryCreate`), so the Domain
  double and the Storage folder share one rule (reuse, never copy) and no free function is added (ELEV013).
- **The in-memory verbs take `folderSync`**, as `copyInFile` and `writeSnapshot` do for the same map.
- **No delete, listing or eviction verb**, as the step says.

## Testing state

- Build: `dotnet build Berreman.slnx -c Release -nologo -v:m` from `Berreman/`, the gate's command. Exit 0, 85 s,
  0 errors. Its only warnings are the exempt NU190x advisories and the four pre-existing `SYSLIB0051` warnings of the
  vendored C# `MathNetNumerics/Numerics/Exceptions.cs` that every MathNet recompile carries (0483 recorded the same).
  Log: `.artifacts/0484-build-01.log`.
- Filtered, the touched classes `KitProxyTests` and `KitProxyFolderTests`: 86 of 86 passed, 13 s.
  Log: `.artifacts/0484-kitproxy-tests.log`.
- The new facts alone (`--filter-method "*planner cache*"`): 4 of 4, their TRX names exactly the four census ids,
  two under each class. Log: `.artifacts/0484-planner-cache-tests.log`; TRX: `.artifacts/0484-trx/`.
- `constructor-unit-tests`, the gate's command over the whole suite (its selection is `all_discovered`): Passed!,
  succeeded 1884, failed 0, skipped 0, 117 s against a 180 s deadline. Checkpoint 483 was 1880, so the count rises by
  exactly 4. Log: `.artifacts/0484-constructor-unit-tests.log`.
- The 21 `invariant-s46-*` must-find-nothing greps, decoded from `.slices/0484.gates`: every one finds nothing.
  Commands: `.artifacts/0484-invariant-commands.txt`.
- Elevation: `Elevation/Run-ElevationGate.ps1 -ExcludeRules ELEV004`, the gate's command, from the repo root; with no
  base it analyses all 13 projects. Exit 0, `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 233s`, known
  findings 0, against a 600 s deadline. Domain 0 findings (194 s), Storage 0, Ui 0. Log: `.artifacts/0484-elevation.log`.
- No run exceeded its effective gate deadline.
- Line endings: all four sources `i/lf w/lf`; `git diff --numstat` equals `--ignore-cr-at-eol`.

## Artifacts

- `.artifacts/0484-build-01.log`
- `.artifacts/0484-kitproxy-tests.log`
- `.artifacts/0484-planner-cache-tests.log`, `.artifacts/0484-trx/0484-planner-cache.trx`
- `.artifacts/0484-constructor-unit-tests.log`
- `.artifacts/0484-invariant-commands.txt`
- `.artifacts/0484-elevation.log`

## Gotchas

- **`KitProxyFolderTests.fs` does not exist.** The step's effects name it as the path of the two
  `KitProxyFolderTests` identities, but the class `OpticalConstructor.Tests.KitProxyFolderTests` is declared in
  `KitProxyTests.fs` (line ~1737) and gets its shared facts from the abstract `KitProxyContract`, exactly as every
  earlier `KitProxyFolderTests` fact (steps 0125–0131, all recorded against `KitProxyTests.fs`). A second file
  declaring the same type would not compile, and moving the class is outside this step. So both facts are written
  once in `KitProxyContract`; the identities are the census's exactly (class, not file, is a test's identity), and no
  identity was added or renamed beyond the four.
- The suite's whole run here was concurrent with the elevation run, as 0483's was.
