# Step 0032 — ADD_CONTRACT STORE_BigBoy_0001 CatalogueProxy — impl plan

## Approach

Declare the catalogue-folder seam in Domain, with its in-memory double and the facts that pin the
surface. The folder implementation belongs to step 33 (`IMPLEMENT_CONTRACT`), so the contract stays
`declared`.

New `OpticalConstructor.Domain/CatalogueFiles.fs` (`module CatalogueFiles`), compiled directly after
`LibraryEntryFiles.fs`:

1. `CatalogueIds.builtInCatalogue`, shaped like `MaterialLibrary.MaterialIds`.
2. `CatalogueError`: five `of reason : string` cases, then `InvalidCatalogueEntriesErr of LibraryEntryError`.
3. `CatalogueData`, with `tryCreate` and `validate`. Both go through `CatalogueManifest.tryCreate`, so the
   sort rule and the two duplicate refusals stay step 26's alone.
4. `[<ReferenceEquality>] CatalogueProxy`, with exactly `readCatalogue` and `writeCatalogue`.
5. `CatalogueProxy.createInMemory`: at most one `CatalogueData` in a closure-local `ref`, the shape of
   `SourceProxy.createInMemory`.

New `OpticalConstructor.Tests/CatalogueProxyTests.fs` (`module CatalogueProxyTests`), after
`EntryFileJsonTests.fs`, with the four facts (a)–(d) under the slice's exact names.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/CatalogueFiles.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` (one line)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/CatalogueProxyTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (one line)

## Contract registry

`STORE_BigBoy_0001` is already in `.contracts-json` (`declared`, `declaringStep` 32) and in
`.contract-ids/BigBoy-json`. Neither is an authorized edit, and neither needs one.

## Risks

- **No local compile.** `add_contract_worker.system-md` says "you act; you run no checks", and it says that
  of this role specifically. This round follows it: no build, no test run and no ELEV run. The code is
  therefore checked by reading it against its precedents and the analyzer sources, and the gate engine gives
  the first measured result.
- **Record labels.** `contentHash` belongs to both `CatalogueManifestRow` and `LibraryEntryEnvelope`, and
  `id`, `name` and `version` to several records. Every record expression carries a type annotation.
- **`validate` has two separate conditions.** The rebuilt manifest is sorted, so comparing it alone cannot
  see reversed envelopes. The envelopes' rows must also equal the manifest's entries in order.

## Attempt 2

The code judge routed attempt 1 back: the test fixtures read code seeds that step 58 deletes, and step 58 may
not edit the test file. This attempt changes only those fixtures, to test-local records over the ids step 58
keeps, and corrects the Gotchas that said later work moves them. The production file and the facts stay as they
are.
