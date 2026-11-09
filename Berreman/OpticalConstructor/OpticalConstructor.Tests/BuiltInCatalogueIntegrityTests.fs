namespace OpticalConstructor.Tests

open Xunit
open Berreman.Constants
open Berreman.Geometry
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Facets
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.EntryFileJson
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

                // The hash covers the content as its file holds it, `ParameterKind` tags included (spec 0050 S7.1).
                match envelope.content.``sealed`` with
                | Error error -> Assert.Fail(error.describe)
                | Ok resealed -> Assert.Equal<ContentHash>(row.contentHash, resealed.contentHash)

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
                    | ItemModelEntryKind -> ItemCount 11

                let actualCount : ItemCount =
                    manifest.entries
                    |> List.filter (fun row -> row.kind = kind)
                    |> List.length
                    |> ItemCount

                Assert.Equal<ItemCount>(expectedCount, actualCount)

    [<Fact>]
    let ``the shipped item models include every required form under their well-known ids`` () : unit =
        let shipped = BuiltinCatalogue.itemModels ()

        // The bench budget is read from the catalogue, not transcribed: step 339 owns its parity with the bench.
        let noiseAxes : ItemParameterAxis list =
            (BuiltinCatalogue.itemModel ItemModelIds.intensityDetectorModelId).entry.model
            |> function
                | IntensityDetectorModel axes -> axes
                | _ -> failwith "The shipped intensity detector model must be an IntensityDetectorModel."

        match noiseAxes with
        | [ rotation; element; throughput ] ->
            Assert.Equal<ItemParameterAxis list>(ItemParameterAxis.detectorNoiseAxes rotation.defaultValue element.defaultValue throughput.defaultValue, noiseAxes)
            for axis in noiseAxes do
                Assert.True(axis.defaultValue.value > 0.0, $"The shipped noise axis '{axis.name.value}' must carry a positive budget.")
        | _ -> Assert.Fail($"The shipped detector models must carry three noise axes, not {noiseAxes.Length}.")

        let expected : (ItemModelId * EntryName * ItemModelForm) list =
            [
                ItemModelIds.realLinearPolarizerModelId, EntryName "Real linear polarizer", OpticalElementModel (RealPolarizerModel ExtinctionRatioLeakage)
                ItemModelIds.retarderPlateModelId, EntryName "Retarder plate", OpticalElementModel (RetarderPlateModel ConstantRetardance)
                ItemModelIds.circularPolarizerModelId,
                EntryName "Circular polarizer (linear polarizer and retarder)",
                OpticalElementModel (
                    CompoundModel
                        [
                            { form = RealPolarizerModel ExtinctionRatioLeakage; offset = Angle.zero }
                            { form = RetarderPlateModel ConstantRetardance; offset = Angle.degree 45.0<deg> }
                        ])
                ItemModelIds.blackBoxMuellerModelId, EntryName "Black-box Mueller matrix", OpticalElementModel (BlackBoxMuellerModel SixteenElementMueller)
                ItemModelIds.emptySlotModelId, EntryName "Empty slot (air)", EmptySlotModel
                ItemModelIds.intensityDetectorModelId, EntryName "Intensity detector (gain, dark offset and noise)", IntensityDetectorModel noiseAxes
                ItemModelIds.muellerPolarimeterModelId, EntryName "Mueller polarimeter", MuellerPolarimeterModel noiseAxes
                ItemModelIds.ellipsometerModelId, EntryName "Ellipsometer (noise budget)", EllipsometerModel noiseAxes
                ItemModelIds.sourcePresetItemModelId, EntryName "Light source (preset binding)", SourcePresetItemModel
                ItemModelIds.idealPolarizerItemModelId, EntryName "Ideal polarizer (preset binding)", IdealPolarizerItemModel
                ItemModelIds.stackSampleItemModelId, EntryName "Sample by stack", StackSampleItemModel
            ]

        let byId (id, _, _) = id
        Assert.Equal<(ItemModelId * EntryName * ItemModelForm) list>(
            expected |> List.sortBy byId,
            shipped |> List.map (fun held -> held.entry.id, held.entry.name, held.entry.model) |> List.sortBy byId)

        for held in shipped do
            Assert.Equal<VersionNumber>(VersionNumber.first, held.version)
            Assert.Equal<EntryProtection>(ProtectedBuiltIn, held.entry.protection)

        let envelopes =
            (BuiltinCatalogue.catalogueData ()).envelopes
            |> List.filter (fun envelope -> envelope.content.schema.kind = ItemModelEntryKind)

        Assert.Equal(expected.Length, envelopes.Length)
        for envelope in envelopes do
            Assert.Equal<EntryProvenance>(BuiltInProvenance CatalogueIds.builtInCatalogue, envelope.content.provenance)

        // The in-memory import pushes every item model through `ItemModelProxy.saveItemModel` (spec 0050 S7.1).
        let stores = BuiltinCatalogue.freshStores ()
        let byEntryId (held : ItemModelVersion) : ItemModelId = held.entry.id
        match stores.itemModels.listItemModels () with
        | Error error -> Assert.Fail(error.describe)
        | Ok imported -> Assert.Equal<ItemModelVersion list>(shipped |> List.sortBy byEntryId, imported |> List.sortBy byEntryId)

        let circularAtFirst : ItemModelVersionId =
            {
                itemModelId = ItemModelIds.circularPolarizerModelId
                version = VersionNumber.first
            }
        match stores.itemModels.resolveItemModelVersion circularAtFirst with
        | Error error -> Assert.Fail(error.describe)
        | Ok None -> Assert.Fail("The imported circular polarizer model must resolve at version 1.")
        | Ok (Some held) -> Assert.Equal<ItemModelVersion>(BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId, held)

        // A detector's noise axes store each ParameterKind as its int tag; an unknown tag is refused at the file seam.
        let intensity = envelopes |> List.find (fun envelope -> envelope.content.id = ItemModelIds.intensityDetectorModelId.toLibraryEntryId)
        match intensity.fileText with
        | Error error -> Assert.Fail(error.describe)
        | Ok file ->
            Assert.Contains($"\"kind\": {AngleParameter.value},", file.value)
            Assert.Contains($"\"kind\": {FractionParameter.value},", file.value)
            Assert.DoesNotContain(nameof AngleParameter, file.value)
            Assert.Equal<Result<LibraryEntryEnvelope, StorageError>>(Ok intensity, file.parseEntry ItemModelEntryKind)

            let angleTag = $"\"kind\": {AngleParameter.value},"
            let at = file.value.IndexOf angleTag
            let unknownTag = LibraryFileText (file.value.Remove(at, angleTag.Length).Insert(at, "\"kind\": 8,"))
            match unknownTag.parseEntry ItemModelEntryKind with
            | Error (MalformedJsonErr reason) -> Assert.Contains(nameof ParameterKind, reason)
            | Error other -> Assert.Fail($"An unknown ParameterKind tag must be MalformedJsonErr, not: {other.describe}")
            | Ok _ -> Assert.Fail("An unknown ParameterKind tag must be refused.")
