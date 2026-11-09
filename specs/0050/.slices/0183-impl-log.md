# Step 0183 — IMPLEMENT_CONTRACT SVC_BigBoy_0001 ExplorerProxy — impl-log

## Progress

- [x] Read the protocol (the base and the `IMPLEMENT_CONTRACT` delta), the project prompt, the slice and its gate
  snapshot. Also read:
  - the step 182 seam (`KitFolders.fs`, `ExplorerProxyTests.fs`);
  - `KitOpening.fs`;
  - STL's `FolderName` (`Sys/Primitives.fs`);
  - the ELEV001 analyzer source (`C:\GitHub\Softellect\Analyzers\NoCallSiteRewrap.fs`).
- [x] `KitOpening.fs`: `ExplorerProxy.system`.
- [x] `ExplorerProxyTests.fs`: the missing-folder fact.
- [x] `.contracts-json`: `SVC_BigBoy_0001` is `implemented` at step 183, and its declaration at step 182 is kept.
- [x] State of the world.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitOpening.fs`
  - New opens: `System.Diagnostics` and `System.IO`. Neither shadows a name the file already uses: no opened
    Berreman, STL or Avalonia namespace defines `Path`, `Directory`, `Process` or `ProcessStartInfo`.
  - `type ExplorerProxy with static member system : ExplorerProxy` sits directly after `KitFolderPicker`'s
    extension. Its type doc and member doc cite spec 0050 S5.3 and `.manual/001 §G`.
  - `showFolder folder`, all inside one `try … with`:
    1. `let fullPath = Path.GetFullPath folder.value`.
    2. If `Directory.Exists fullPath` is false, it answers `Error (MissingFolderErr fullPath)`. Nothing is
       started and nothing is created.
    3. If it is true, it builds `ProcessStartInfo("explorer.exe", UseShellExecute = false)`, adds the path with
       `startInfo.ArgumentList.Add fullPath`, then `use explorer = Process.Start(startInfo)`. The process is
       disposed when the arm returns `Ok ()`, and nothing waits for Explorer.
    4. `with e -> Error (ExplorerLaunchErr e.Message)`. Any exception from the resolution or the start becomes a
       typed error, so nothing above the seam handles an exception.
  - There is no retry, no fallback browser and no folder creation.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExplorerProxyTests.fs`
  - New opens: `System`, `System.IO` and `OpticalConstructor.Ui.KitOpening`. The Tests project already references
    Ui, and `KitOpeningTests` opens the same module.
  - New fact `the system explorer refuses a folder that does not exist and starts nothing`:
    - The folder is
      `FolderName (Path.Combine(AppContext.BaseDirectory, "ExplorerProxyTests", Guid.NewGuid().ToString("N")))`.
      It is elevated where it is made, and the fact never creates it.
    - The fact asserts that the folder is absent before the call.
    - `ExplorerProxy.system.showFolder` must answer `Error (MissingFolderErr reason)` with `reason` containing the
      folder's text. Any other error fails the fact with its `describe`, and `Ok ()` fails it outright.
    - The fact asserts that the folder is still absent afterwards.
  - The two existing facts from step 182 are unchanged.
- `specs/0050/.contracts-json`: `SVC_BigBoy_0001` changes from `lifecycle` `declared` with `implementStep` null to
  `implemented` with `implementStep` 183. `declaringStep` stays 182; `consumers`, `temporaries` and
  `openTemporary` are unchanged.

## Decisions

- **The reason is the resolved full path.** "answers `MissingFolderErr` naming it" leaves two readings: the input
  text or the resolved path. The resolved path is what `Directory.Exists` checked, so it is what the reason names.
  The fact's folder is already absolute: `AppContext.BaseDirectory` combined with `Path.Combine` gives a
  normalized Windows path. `GetFullPath` therefore returns it unchanged, and `Assert.Contains(missing.value,
  reason)` holds.
- **`ExplorerLaunchErr` carries `e.Message` alone**, exactly as the brief says ("carrying its message"). It adds no
  folder prefix, so the text matches the other exception-wrapping errors in Ui (`Localization.fs`,
  `UserEnvironment.fs`).
- **`match Directory.Exists … with | false | true`**, not `if`, follows `KitHistoryStore`'s `probeFolder`.
- **The process is disposed with a named `use explorer`, not `use _`.** The named form disposes on every F# version
  without ambiguity, and FS1182 (unused value) is not enabled in this repo.
- **The doc comment cites the ruling as `.manual/001 §G`**, the "Locations" section of
  `specs/0050/.manual/001-library-files-database-multi-setup-inverse.md`. The slice wrote it as "001 §G".
- **ELEV.** These checks were done by reading the analyzer sources and the diff. No analyzer was run.
  - The member names only `ExplorerProxy`, so it has no primitive signature (ELEV003 and ELEV010).
  - ELEV001 reports only `SingleCaseDu other.value`. `MissingFolderErr fullPath` builds a case of a multi-case
    `*Error` union from a string local, and a string field on an `*Error` union is sanctioned.
  - `system` is the only extension member named `system` on the `ExplorerProxy` receiver (ELEV019). The only other
    extension member on that receiver is `createInMemory`.
  - `.value` appears only at the `Path.GetFullPath` interop line. That is ELEV004, which the gate excludes.

## Testing state

No checks or gates were run. The `IMPLEMENT_CONTRACT` role is act-only (Invariant 6), so these all belong to the
arc-runner's gate engine after this worker exits:

- build and elevation;
- constructor-unit-tests (all discovered);
- ui-smoke (all discovered);
- ui-tests (`KitStartWindowTests`);
- the fourteen s21 invariants.

Because no build ran, every API was checked against its source or an existing use instead:

| API | Checked against |
|---|---|
| `ProcessStartInfo(string)` with a named property setter, and `ArgumentList` | .NET BCL; `KitFolderStore.fs` already uses `System.Diagnostics.Process` |
| `Process.Start(ProcessStartInfo)` | .NET BCL |
| `Path.GetFullPath`, `Directory.Exists` | .NET BCL |
| `ExplorerProxy`, `MissingFolderErr`, `ExplorerLaunchErr`, `describe` | step 182's `KitFolders.fs` |
| `Assert.Fail($"…")` | `BerremanTests/BiaxialInverseTests.fs:454` |
| `Assert.Fail "…"` | `CatalogueFolderTests.fs` |
| `FolderName (Path.Combine(AppContext.BaseDirectory, …, Guid.NewGuid().ToString("N")))` | `CatalogueImportInMemoryTests.fs:161` |

New identity: exactly one, `OpticalConstructor.Tests.ExplorerProxyTests.the system explorer refuses a folder that
does not exist and starts nothing`, as the slice's effects declare. None is removed or renamed. The
constructor-unit-tests count is expected to go from 1190 (checkpoint 181) to 1191. That has not been measured.

Hygiene:

- The edited files are LF with no BOM. `git diff --numstat` matches `--ignore-cr-at-eol`, and `git diff --check`
  is clean.
- I grepped the added lines for every s21 invariant word: none appears.
- `specs/0050/.manifest.state.json` was already modified before this round. It is the supervisor's file and was
  left untouched.

## Artifacts

None. No logs or captures were produced, because no checks were run.
