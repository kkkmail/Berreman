# Step 0183 — IMPLEMENT_CONTRACT SVC_BigBoy_0001 ExplorerProxy — impl-plan

## Approach

Step 182 declared `ExplorerProxy = { showFolder : FolderName -> Result<unit, ExplorerError> }` and its in-memory
double in `OpticalConstructor.Domain/KitFolders.fs`. This step adds the real construction at the IO edge, in
`OpticalConstructor.Ui/KitOpening.fs`, next to `KitFolderPicker.ofWindow`:

```fsharp
type ExplorerProxy with
    static member system : ExplorerProxy
```

`showFolder folder` does the following:

1. Resolves `Path.GetFullPath folder.value`.
2. If `Directory.Exists` answers false, it answers `Error (MissingFolderErr fullPath)` and starts nothing.
3. Otherwise it builds `ProcessStartInfo("explorer.exe", UseShellExecute = false)`, adds the full path through
   `ArgumentList`, starts the process, disposes it at once (`use`) and answers `Ok ()`.
4. One `try … with` wraps the whole body, so any exception from the resolution or the start becomes
   `ExplorerLaunchErr e.Message`.

It adds no wait, retry, fallback browser or folder creation (001 §G: a location that does not exist yet is told,
never made).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitOpening.fs`:
  - add `open System.Diagnostics` and `open System.IO`;
  - add the `ExplorerProxy` extension after `KitFolderPicker`'s extension.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExplorerProxyTests.fs`:
  - add `open System`, `open System.IO` and `open OpticalConstructor.Ui.KitOpening`;
  - add one fact, `the system explorer refuses a folder that does not exist and starts nothing`.
- `specs/0050/.contracts-json`: `SVC_BigBoy_0001` becomes `implemented` with `implementStep` 183, keeping
  `declaringStep` 182.

## Risks

- **Name clashes.**
  - `System.IO` and `System.Diagnostics` opened in `KitOpening.fs` could shadow a name the file already uses. I
    checked the file: it uses `Path`, `Directory` and `Process` nowhere else. `Avalonia.Platform.Storage` has no
    `Path`, `Directory` or `Process` type.
  - `system` is a new static member on the `ExplorerProxy` receiver only. ELEV019 compares extension members per
    receiver, so it does not clash with `ClockProvider.system`.
- **ELEV001.** It flags only `SingleCaseDu other.value`. `MissingFolderErr fullPath` builds a case of a multi-case
  error union from a string local, so ELEV001 does not apply, and a string field on an `*Error` union is sanctioned.
- **Machine side effects.** The test never asks the real proxy to show an existing folder, so no Explorer window
  opens on the test machine.
