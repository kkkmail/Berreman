# Step 0035 — CLOSE_RED — impl plan

Closes `RED_0006`: the five `BuiltInCatalogueParityTests` facts read `Catalogue/Builtin/` from the test output, and no such
folder exists yet. The fix is data plus one project item, with no change to product code.

## Approach

1. **New `Berreman/OpticalConstructor/Catalogue/export-builtin.fsx`,** the throw-away tool step 36 deletes. It `#I`s the Release
   x64 output of `OpticalConstructor.Tests` and `#r`s the assemblies by name, with no `#r "nuget:"`. It seals the seeds step 34
   names at `VersionNumber.first` under `BuiltInProvenance CatalogueIds.builtInCatalogue`, through step 25's builders and step 30's
   `sealed`. It builds the catalogue with `CatalogueData.tryCreate` and writes it with `CatalogueProxy.create (...).writeCatalogue`,
   so no second writer exists. Any `Error` prints its case and exits with 1.
2. **`OpticalConstructor.Storage.fsproj`** gains the slice's `Content` item with its `Link` and `PreserveNewest`, beside the
   `schema` items, with no new comment.
3. **Build the solution, run the script once from the repository root, and build again** so the copy reaches the outputs.

## Files

- `Berreman/OpticalConstructor/Catalogue/export-builtin.fsx` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`
- `Berreman/OpticalConstructor/Catalogue/Builtin/` (generated: `.catalogue-json` and one file per seed)

## What is inspected, and what is not run

The role says the worker runs no checks and never runs a gate. The slice orders the build and the export, so both run as steps of
the work. The generated files are inspected against the slice's own statements: the exact file list, LF-only with one final LF,
`git check-ignore`, and the copy in the test output. `dotnet test` and the ELEV analyzers are the gate engine's and are not run.

## Risks

- `dotnet fsi` carries its own FSharp.Core, older than the 10.1.402 the assemblies were built against.
- The Analytics dependency of Domain may need more `#r` lines than the slice names (it says "among them").
- `.catalogue-json` has an empty `%(Filename)`, and the names hold non-ASCII characters, so the `Link` metadata and the copy need
  looking at in the output.
- A generated name that differs from the effects list, down to the code point (`µ`, `×`, `₂`, `–`), would breach the slice's
  "exactly".
