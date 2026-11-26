# Step 0182 — impl log (ADD_CONTRACT SVC_BigBoy_0001 ExplorerProxy)

## Progress

- [x] Read the protocol (`arc-runner.system-md`, `add_contract_worker.system-md`), the project prompt, the slice, its
      `.gates` snapshot, the step-183 / 186 / 193 consumers of the seam in `.spec-jsonl`, and `.manual/013` (binds from
      0160 on; nothing in it touches this step). The project prompt's Operator note is empty.
- [x] `ExplorerProxyTests.fs` written first, against symbols that did not exist yet (red by missing symbol).
- [x] `KitFolders.fs`: `ExplorerError`, `ExplorerProxy`, `ExplorerProxy.createInMemory`, after `KitFolderPicker`.
- [x] `OpticalConstructor.Tests.fsproj`: `ExplorerProxyTests.fs` appended at the end of the compile list, no comment.
- [x] Every edited or created file verified LF (byte count of `\r` is 0 in each).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitFolders.fs` — 31 lines added directly after
  `KitFolderPicker` (step 145), before `type KitContext with`:
  - `ExplorerError` = `MissingFolderErr of reason : string` | `ExplorerLaunchErr of reason : string`, each case with a
    one-line doc; `member this.describe : string` renders `the folder does not exist: <reason>` and
    `the folder could not be shown: <reason>`, in step 39's `KitError.describe` form.
  - `[<ReferenceEquality>] ExplorerProxy = { showFolder : FolderName -> Result<unit, ExplorerError> }` — one-line doc
    on the type and on the field (opens the folder and answers at once, never waiting for the browser).
  - `type ExplorerProxy with static member createInMemory (record : FolderName -> unit) : ExplorerProxy` — hands each
    folder to `record` and answers `Ok ()`, starting nothing.
  - Every doc is one line and cites spec 0050 S5.3.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExplorerProxyTests.fs` (new) — `module ExplorerProxyTests` in
  `namespace OpticalConstructor.Tests`, two facts:
  - `the in-memory explorer hands every folder it is asked to show to its recorder`: the recorder is a test-local
    `ResizeArray<FolderName>`'s `Add`; `showFolder (FolderName "C:/kits/a")` then `showFolder (FolderName "D:/data")`
    each answer `Ok ()`, and the list holds `[ C:/kits/a; D:/data ]` in that order.
  - `every explorer error renders its condition and its payload`: `MissingFolderErr` over `C:/kits/gone` and
    `ExplorerLaunchErr` over `explorer.exe was not found` (two distinct test-local reasons); each `describe` contains
    its reason and its condition (`does not exist` / `could not be shown`), and the two texts differ.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` — one `<Compile>` line after
  `KitStartTests.fs`.

## Decisions

- **The type doc names the real proxy by its step-183 name.** `.spec-jsonl` step 183 declares the real explorer as
  `type ExplorerProxy with static member system` in `OpticalConstructor.Ui/KitOpening.fs`, so the doc says "Ui's
  `ExplorerProxy.system`, or `createInMemory` the double", as `KitHistoryProxy`'s doc names its two constructions.
- **Every doc cites S5.3**, the case and field docs included, because the brief says "each doc one to three lines
  citing spec 0050 S5.3"; `KitProxy`'s field docs set the same precedent.
- **`describe` carries no doc**, as `KitError.describe` and `KitHistoryError.describe` carry none.
- **`createInMemory` is declared in a `type ExplorerProxy with` block**, exactly as the brief writes it (an intrinsic
  extension: same module and file, so it compiles as a member of the record).
- **Nothing else is added:** no existence check, no folder creation, no retry and no second field, as the brief limits.
- **Contract registry.** `specs/0050/.contracts-json` already records `SVC_BigBoy_0001` at `lifecycle: declared`,
  `declaringStep: 182`, `implementStep: null` (pre-registered by the step compiler). The file is not in this step's
  authorized edits, and its entry already says what this step makes true, so it is left untouched; the contract stays
  `declared` for step 183's `IMPLEMENT_CONTRACT`.
- **Names are unique.** `ExplorerError`, `ExplorerProxy`, `MissingFolderErr`, `ExplorerLaunchErr` and `showFolder` have
  no namesake in Berreman or in STL (`C:\GitHub\Softellect`, grepped read-only). `createInMemory` is a static member, not
  a public module-level name, as on `LibraryBackupProxy`, `KitHistoryProxy` and a dozen other proxies.
- **ELEV shape.** Both error cases end in `Err` and carry `reason : string` (the sanctioned string on an `*Error`
  union, ELEV020). `describe : string` is a member on its own type. `showFolder` takes the elevated `FolderName` and its
  `Ok` case is `unit`. `createInMemory`'s parameter is `FolderName -> unit`. No `.value` is read anywhere in the change.

## Testing state

This worker ran no build, test, analyzer or invariant gate: under Invariant 6 of the worker base and the
`ADD_CONTRACT` delta, gate execution belongs to the arc-runner's gate engine, which runs, per `0182.gates`:

- build;
- elevation;
- constructor-unit-tests, all discovered;
- the fourteen s21 invariants.

Checked without running a gate:

- The three edited/created source files and both slice markdown files hold no `\r` byte (counted with Python over the
  raw bytes); `git diff --numstat` equals `git diff --numstat --ignore-cr-at-eol`.
- The test idioms are the suite's own: module-level `[<Fact>] let`s (`BeamTreeTests`), `Assert.Equal<T list>`
  (`AppShellLayoutTests`, `BeamLayoutTests`), `Assert.Equal<Result<…>>` (`LibraryBackupProxyTests`),
  `Assert.Contains(string, string)` (`CatalogueImportInMemoryTests`).
- No line added matches any s21 invariant's pattern (`ELEV0` in the known-findings list, VSTest packages,
  `kit.kit-json`, `EpsAxisEvaluated`, `complexity = …`, preset id strings, seeding names, test-db names,
  `VersionsInUse`, `SceneProxy` / `ExperimentCollectionProxy`, `ocproj`, `recent.json`).

Test identities, matching the step's census:

- created: `OpticalConstructor.Tests.ExplorerProxyTests.the in-memory explorer hands every folder it is asked to show
  to its recorder` and `OpticalConstructor.Tests.ExplorerProxyTests.every explorer error renders its condition and its
  payload`;
- none removed or renamed.

So constructor-unit-tests should rise by 2, from the latest checkpoint's 1188 to 1190.

## Gotchas

- In this Git Bash, `grep -c $'\r' <file>` reported every line of an LF file as a match (it read 509 for
  `KitFolders.fs`). Count `\r` over the raw bytes (Python `read().count(b'\r')` or `od -c`) instead; the files are LF.
- `ExplorerProxy.createInMemory` does not check that the folder exists: the existence check (`MissingFolderErr`) is the
  real proxy's, step 183's. A test that wants a `MissingFolderErr` from the double has to build its own proxy record.

## Artifacts

None. This step wrote no logs or captures.
