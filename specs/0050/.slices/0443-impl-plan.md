# Step 0443 — impl plan (ADD_CONTRACT SVC_BigBoy_0002 MaterialSourceProxy)

## Approach

Declare the online-source seam in Domain exactly as the slice's how-to lists it, in one new file
`OpticalConstructor.Domain/MaterialSources.fs` (`module MaterialSources`, namespace
`OpticalConstructor.Domain`), compiled directly before `CatalogueImport.fs`. In order:
`MaterialSourceError` (seven `*Err` cases, each `of reason : string`, with `describe`),
`MaterialSourceHit`, `MaterialSourceQuery` (with the one search rule `filter`),
`SourceMaterialRecord`, `SourceSnapshotFileCount`, `SourceSnapshotProgress`,
`SourceSnapshotObserver` (`[<ReferenceEquality>]`, `unobserved`), `SourceSnapshotInfo`,
`SourceSnapshotOutcome`, the `[<ReferenceEquality>]` record `MaterialSourceProxy` with exactly five
fields, then the double: `InMemorySourceRecord`, the sealed class `InMemoryMaterialSource` (private
mutable state behind a lock, as `InMemoryArchiveDisk`), and the extension
`MaterialSourceProxy.createInMemory`.

Shapes are copied from the step 197 precedent (`KitArchive.fs`: `KitArchiveObserver`,
`KitArchiveOutcome`, `InMemoryArchiveDisk`, `KitArchiveProxy.createInMemory`) so the cancellation rule
(read before each file, report after each) is the same one.

Tests: new `OpticalConstructor.Tests/MaterialSourceProxyTests.fs` (`module MaterialSourceProxyTests`),
appended to the end of the test compile list; seven facts with the census's exact identities.

The contract registry (`specs/0050/.contracts-json`) already records `SVC_BigBoy_0002` as `declared`
at step 443, so nothing is written there.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialSources.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` (one `<Compile>`)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialSourceProxyTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (one `<Compile>`)

## Risks

- ELEV: the class's private state must not expose primitives; `InMemoryMaterialSource` members speak
  our types only. `MaterialSourceQuery.filter` is an intrinsic member (exempt from ELEV013).
  `createInMemory` is an extension on `MaterialSourceProxy` (ELEV019: unique on that receiver).
- Name uniqueness (ELEV014/016): every new type and case name was grepped in Berreman and STL; none
  exists.
- The `cancelled on the first progress` test: the observer's report callback cancels its own token
  source, so the second record's pre-check sees it; the double must therefore read the token before
  every record, the first included.
