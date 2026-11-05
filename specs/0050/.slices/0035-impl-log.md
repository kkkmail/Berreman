# Step 0035 — CLOSE_RED — impl log

## Progress

- [x] Read the protocol, the CLOSE_RED role, the project prompt, AGENTS.md, the slice and its `.gates` snapshot, steps 33, 34 and
  36, `.spec-md` §3.5 and §3.6, 001 §B, and the code the script calls.
- [x] Wrote `export-builtin.fsx` and added the `Content` item to `OpticalConstructor.Storage.fsproj`.
- [x] Built the solution, ran the export once, and built again so the new files reach the outputs.
- [x] Inspected the generated files against the slice's statements (see Testing state).

## Files modified

| File | Change |
|---|---|
| `Berreman/OpticalConstructor/Catalogue/export-builtin.fsx` | new: the throw-away export, which step 36 deletes |
| `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj` | the slice's `Content` item (3 lines), no comment |
| `Berreman/OpticalConstructor/Catalogue/Builtin/` | new, generated: `.catalogue-json` and 61 entry files |

No product code and no test changed. `BuiltInCatalogueParityTests.fs` is untouched.

## Decisions

- **The script `#r`s 12 assemblies:** `Softellect.Sys`, the two MathNet assemblies, `FSharp.SystemTextJson`, `Json.More`,
  `JsonPointer.Net`, `JsonSchema.Net`, the Berreman core, `OpticalProperties`, `Analytics`, Domain and Storage. The slice says
  "among them", so `OpticalProperties` and `Analytics` are added because Domain references them. The script ran with this list.
- **The script's seed list mirrors step 34's on purpose.** Step 34 computes its expected catalogue independently of the export, so
  this is not a second implementation to share. Step 36 deletes the script.
- **Errors print with `%A`.** The slice says an `Error` "prints its case". Of the script's three error types only `StorageError`
  has a `describe`, and that text leaves the case out. The analyzers do not read `.fsx` files, and step 34's `okOf` renders the
  same way.
- **The solution was built twice.** MSBuild evaluates the `**` glob when a build starts, so the first build, which the script
  needs, could copy nothing. The second one puts the files in the outputs.

## Testing state

**Not run: `dotnet test` and the ELEV analyzers.** The CLOSE_RED role says the worker never runs its gates and never reports their
result, and the gate engine runs them after this round. So **the five `BuiltInCatalogueParityTests` facts have not been observed
green in this round.** That they go green is what the gate engine measures. `gates_run` and `gate_results` are empty, and
`commit_ready: true` means every requirement of the slice is addressed, not that a gate passed.

**Run because the slice orders them:**

- `dotnet build Berreman.slnx -c Release -nologo -v:m`, before the export: exit 0, `0 Error(s)`, `2 Warning(s)`. Both warnings are
  the vendored `MathNetNumerics/Numerics/Exceptions.cs` `SYSLIB0051` ones earlier steps recorded.
- `dotnet fsi Berreman/OpticalConstructor/Catalogue/export-builtin.fsx`, once, from the repository root: exit 0, printing
  `export-builtin wrote 61 entries`.
- The same build after the export: exit 0, `0 Error(s)`, `0 Warning(s)`.

**Inspected, by reading the generated files:**

- **The file list:** 62 files on disk and 62 in the slice's effects list, compared exactly with none missing and none extra. No
  sub-folders. By kind: 12 materials, 5 categories, 11 samples, 28 sources, 2 detectors, 3 polarizers, plus `.catalogue-json`.
- **The bytes:** every file has no CR, no BOM and exactly one final LF. No name changes under NFC normalization.
- **`git check-ignore --no-index`** over the 62 files and the script: exit 1, reporting none.
- **The copies:** the App, TestWindows, TestWindows.App, Ui, Storage, Tests and Ui.Tests outputs each hold 62 files, byte-identical
  to the source, `.catalogue-json` included.
- **The fsproj diff:** `3 0` by `git diff --numstat`, with and without `--ignore-cr-at-eol`, so no CRLF churn.
- **The words:** the generated files hold no `EpsAxisEvaluated`, `kit.kit-json` or `complexity = None|Some`. This reads the files;
  it does not run the invariant gates, which `git grep` tracked files only.

## Artifacts

In `specs/0050/.artifacts/`:

- `0035-build-before-export.log`
- `0035-export-run-1.log`
- `0035-build-after-export.log`

## Gotchas

- **Local runs against "you run no checks".** Step 0034 flagged this as unsettled: step 0023 ran the suite, step 0033 ran nothing,
  and step 0034 ran its class. This round follows the role's text for tests and analyzers, and runs only what the slice orders. The
  file inspection above reads what the round produced and runs no test.
- **The first build copies nothing.** Anyone repeating the procedure needs the second build before the outputs hold the catalogue.
- **The export ran under `dotnet fsi`'s FSharp.Core, not the pinned 10.1.402.** The run reported no problem. Whether its bytes equal
  what the test process computes is fact (e)'s to measure: it compares every content hash.
- **After step 36 nothing regenerates these files.** A change to a seed value or to an entry's JSON shape before S1.7 changes a
  hash and would fail the parity facts, with no tool left to re-export. This is reasoned from the code, not exercised.
