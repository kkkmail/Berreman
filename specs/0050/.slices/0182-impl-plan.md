# Step 0182 — impl plan (ADD_CONTRACT SVC_BigBoy_0001 ExplorerProxy)

## Approach

Declare the "show a folder in Explorer" seam in `OpticalConstructor.Domain/KitFolders.fs`, right after
`KitFolderPicker` (step 145), as the brief places it:

1. `ExplorerError` — `MissingFolderErr of reason : string` | `ExplorerLaunchErr of reason : string`, each case
   documented, with `member this.describe : string` in step 39's `KitError.describe` form
   (`the folder does not exist: <reason>` / `the folder could not be shown: <reason>`).
2. `[<ReferenceEquality>] ExplorerProxy = { showFolder : FolderName -> Result<unit, ExplorerError> }`, one-line docs on
   the type and the field, the `KitFolderPicker` functional-proxy convention.
3. `type ExplorerProxy with static member createInMemory (record : FolderName -> unit) : ExplorerProxy` — the double:
   hands each folder to `record`, answers `Ok ()`, starts nothing.

The real proxy is step 183's; nothing else (existence check, retry, other field) is added.

## Tests

New `OpticalConstructor.Tests/ExplorerProxyTests.fs` (`module ExplorerProxyTests` in `namespace OpticalConstructor.Tests`),
appended at the end of the test project's compile list with no comment:

- `the in-memory explorer hands every folder it is asked to show to its recorder`
- `every explorer error renders its condition and its payload`

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitFolders.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExplorerProxyTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Risks

- ELEV uniqueness: `ExplorerError`, `ExplorerProxy`, `MissingFolderErr`, `ExplorerLaunchErr`, `showFolder` — grep found
  no namesake among ours. `createInMemory` is a static member (not a module-level name), as on a dozen other proxies.
- ELEV003/010: `describe : string` is a member on its own type (exempt); `createInMemory` takes `FolderName -> unit`
  (elevated parameter, `unit` return).
- The test fixtures must elevate at origin: folders are bound as `FolderName` literals, reasons as the error cases'
  own string payload.
