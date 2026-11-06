namespace OpticalConstructor.Tests

open Xunit
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library

module BuiltinCatalogueFixtureTests =

    [<Fact>]
    let ``the fixture's fresh stores hold the whole Built-in catalogue and are isolated per call`` () : unit =
        let first = BuiltinCatalogue.freshStores ()
        let second = BuiltinCatalogue.freshStores ()

        Assert.Equal<Result<int, CategoryError>>(
            Ok (BuiltinCatalogue.categories () |> List.length),
            first.categories.listCategories () |> Result.map List.length)
        Assert.Equal<Result<int, MaterialError>>(
            Ok (BuiltinCatalogue.materials () |> List.length),
            first.materials.listMaterials IncludeInactive |> Result.map List.length)
        Assert.Equal<Result<int, SampleError>>(
            Ok (BuiltinCatalogue.samples () |> List.length),
            first.samples.listSamples IncludeInactive |> Result.map List.length)

        let storedPresets =
            [ LightSource; Detector; LinearPolarizer; CircularPolarizer ]
            |> List.collect (fun kind ->
                match first.libraryEntries.entriesForKind kind with
                | Ok entries -> entries
                | Error _ -> failwith "The fixture's preset store must list every preset kind.")
            |> List.distinctBy (fun entry -> entry.entryId)

        Assert.Equal(BuiltinCatalogue.presets () |> List.length, storedPresets.Length)

        let sampleId = SampleIds.glassFilm600Id
        let expected = BuiltinCatalogue.sample sampleId
        Assert.Equal<Result<unit, SampleError>>(Ok (), first.samples.removeSample sampleId)
        Assert.Equal<Result<Sample option, SampleError>>(Ok None, first.samples.tryGetSample sampleId)
        Assert.Equal<Result<Sample option, SampleError>>(Ok (Some expected), second.samples.tryGetSample sampleId)
        Assert.Equal<Sample>(expected, BuiltinCatalogue.sample sampleId)

    [<Fact>]
    let ``the fixture resolves the built-ins by their well-known ids`` () : unit =
        let siliconName = EntryName "Silicon"
        let multilayerName = EntryName "Quarter-wave glass/vacuum multilayer (41 layers)"
        Assert.Equal<EntryName>(siliconName, (BuiltinCatalogue.material MaterialIds.silicon).name)
        Assert.Equal<CategoryVisibility>(HiddenOnCreate, (BuiltinCatalogue.category CategoryIds.vacuumCategory).visibility)
        Assert.Equal<EntryName>(multilayerName, (BuiltinCatalogue.sample SampleIds.multilayerQwId).name)

        match BuiltinCatalogue.preset PresetIds.detectorEllipsometer with
        | DetectorItem detector -> Assert.Equal<DetectorKind>(Ellipsometer, detector.kind)
        | _ -> Assert.Fail("The ellipsometer preset id must resolve to a detector.")

        for entry in BuiltinCatalogue.materials () do
            Assert.Equal<MaterialEntry>(entry, BuiltinCatalogue.material entry.id)

        for entry in BuiltinCatalogue.categories () do
            Assert.Equal<MaterialCategory>(entry, BuiltinCatalogue.category entry.id)

        for entry in BuiltinCatalogue.samples () do
            Assert.Equal<Sample>(entry, BuiltinCatalogue.sample entry.id)

        for entry in BuiltinCatalogue.presets () do
            Assert.Equal<LibraryEntry>(entry, BuiltinCatalogue.preset entry.entryId)
