namespace OpticalConstructor.Tests

open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Facets
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Storage.CanonicalJson
open OpticalConstructor.Storage.CatalogueFolderStore

/// The shipped files remain independently verifiable after code-seed retirement (spec 0050 S1.7;
/// .manual/005 authorizes this step-0057 replacement before accepting step 0056).
module BuiltInCatalogueIntegrityTests =

    [<Fact>]
    let ``the shipped Built-in catalogue is whole: every file validates, every hash matches its manifest row, ids are unique and every built-in kind is present`` () : unit =
        match (CatalogueProxy.createBuiltIn ()).readCatalogue () with
        | Error error -> Assert.Fail(error.describe)
        | Ok catalogue ->
            let manifest = catalogue.manifest
            let builtInName : CatalogueName = CatalogueName "Built-in"

            Assert.Equal<CatalogueId>(CatalogueIds.builtInCatalogue, manifest.id)
            Assert.Equal<CatalogueName>(builtInName, manifest.name)
            Assert.Equal<VersionNumber>(VersionNumber.first, manifest.version)

            for row in manifest.entries do
                let envelope =
                    catalogue.envelopes
                    |> List.filter (fun envelope -> envelope.content.id = row.id && envelope.content.version = row.version)
                    |> Assert.Single<LibraryEntryEnvelope>

                Assert.Equal<ContentHash>(row.contentHash, envelope.contentHash)

                match contentHashOf envelope.content with
                | Error error -> Assert.Fail(error.describe)
                | Ok computed -> Assert.Equal<ContentHash>(row.contentHash, computed)

            catalogue.envelopes
            |> List.map (fun envelope -> envelope.content.id)
            |> Assert.Distinct<LibraryEntryId>

            for kind in LibraryEntryKind.all do
                let expectedCount : ItemCount =
                    match kind with
                    | MaterialEntryKind -> ItemCount 12
                    | CategoryEntryKind -> ItemCount 5
                    | SampleEntryKind -> ItemCount 11
                    | SourceEntryKind -> ItemCount 28
                    | PolarizerEntryKind -> ItemCount 3
                    | DetectorEntryKind -> ItemCount 2

                let actualCount : ItemCount =
                    manifest.entries
                    |> List.filter (fun row -> row.kind = kind)
                    |> List.length
                    |> ItemCount

                Assert.Equal<ItemCount>(expectedCount, actualCount)
