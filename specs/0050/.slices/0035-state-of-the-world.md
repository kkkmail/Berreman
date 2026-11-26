# Step 0035 — CLOSE_RED — state of the world

## Where we are

Spec 0050, section S1.5: "The built-in catalogue generated once from the code seeds". Step 34 declared `RED_0006`: five facts say
the catalogue in the test output equals the code seeds, and they were red because no catalogue existed. This step exports the seeds
through the product writer, commits the result, and has the build copy it into every output that references Storage. Step 36 deletes
the export script. No product code reads the catalogue until S1.6, and the seeds stay until S1.7.

## What's working

- Added the throw-away `export-builtin.fsx`, which seals every seed and writes through `CatalogueProxy.create`, so no second writer exists
- Exported the seeds once into `Catalogue/Builtin/`: `.catalogue-json` and 61 entry files, exactly the slice's effects list
- Added the `Content` item to `OpticalConstructor.Storage.fsproj`, so the App, TestWindows heads and both test suites carry the files
- Confirmed by inspection that every file is LF-only, none is git-ignored, and the copies in the outputs are byte-identical

## Tests

The gate engine is the gate authority. This round ran no test and no analyzer, as the CLOSE_RED role requires, so **the five
`BuiltInCatalogueParityTests` facts have not been observed green here.**

- **Ordered by the slice, and run:** the solution build before the export (exit 0, 0 errors, the 2 vendored MathNet `SYSLIB0051`
  warnings), the export once (exit 0, 61 entries), and the solution build after it (exit 0, 0 errors, 0 warnings).
- **Inspected:** the file list, the bytes, `git check-ignore` and the copies, recorded in `0035-impl-log.md` (Testing state).
- **Not run:** `constructor-unit-tests`, `elevation` and the six s05 invariant commands.

The count below is **carried, not measured**: the last recorded `constructor_unit_tests` checkpoint, from step 32.

```yaml
gates:
  constructor_unit_tests: 786
```

## Architecture

- **The committed files are the product writer's output.** The script holds no JSON or file logic of its own: it calls step 25's
  builders, step 30's `sealed`, `CatalogueData.tryCreate` and `CatalogueProxy.writeCatalogue`.
- **The catalogue rides Storage's content,** as `schema/` does (`.spec-md` §3.5), and reaches consumers through the project
  reference. No project compiles or links the script.

## Deferred

Nothing. Every requirement of the slice is addressed. Deleting the script is step 36's own brief, and reading the catalogue in the
product is S1.6's.

## Gotchas

- **The first build copies nothing.** MSBuild evaluates the `**` glob when a build starts, so the copy lands on the build after the
  export.
- **After step 36 nothing regenerates these files.** A change to a seed value or to an entry's JSON shape before S1.7 changes a
  hash and would fail the parity facts. This is reasoned from the code, not exercised.
- **The export ran under `dotnet fsi`'s FSharp.Core,** not the pinned 10.1.402. Parity fact (e) compares every content hash with
  what the test process computes, so it measures any difference.
- **Whether a worker may run local checks is still unsettled.** Steps 0023, 0033 and 0034 each did something different. This round
  ran only what the slice orders; an operator ruling in `.manual/` would settle it.

## Changelog

- 2026-11-05: Step 0035 exported the code seeds into `Catalogue/Builtin/` (62 files), added the Storage `Content` item, and added the
  throw-away script. Tests and analyzers are left to the gate engine.
