namespace OpticalConstructor.Tests

open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Storage.EntryFileJson
open OpticalConstructor.Storage.CatalogueFolderStore

/// The catalogue the build copies into the test output equals the code seeds (spec 0050 001 §B "Shipped catalogue";
/// .spec-md §3.5, §3.6). S1.7 deletes this class together with the seeds.
module BuiltInCatalogueParityTests =

    let private okOf (result : Result<'Value, 'Problem>) : 'Value =
        match result with
        | Ok value -> value
        | Error problem -> failwith $"expected Ok, got %A{problem}"

    let private builtIn : EntryProvenance = BuiltInProvenance CatalogueIds.builtInCatalogue

    let private builtInName : CatalogueName = CatalogueName "Built-in"

    let private seedDetectors : DetectorPreset list =
        seedEntries
        |> List.choose (fun (entry : LibraryEntry) ->
            match entry with
            | DetectorItem detector -> Some detector
            | _ -> None)

    let private seedPolarizers : PolarizerPreset list =
        seedEntries
        |> List.choose (fun (entry : LibraryEntry) ->
            match entry with
            | PolarizerItem polarizer -> Some polarizer
            | _ -> None)

    let private readBuiltIn () : CatalogueData =
        (CatalogueProxy.createBuiltIn ()).readCatalogue ()
        |> okOf

    /// What the seeds seal to, computed here and not taken from anything the export (step 35) wrote. A function, so a
    /// failure sinks its fact and not the assembly's start-up.
    let private expectedCatalogue () : CatalogueData =
        (builtInEntries |> List.map (LibraryEntryContent.ofMaterialEntry VersionNumber.first builtIn))
        @ (standardCategories |> List.map (LibraryEntryContent.ofMaterialCategory VersionNumber.first builtIn >> okOf))
        @ (SeedSamples.allSeedSamples |> List.map (LibraryEntryContent.ofSample VersionNumber.first builtIn))
        @ (SeedSources.allSeedSources |> List.map (LibraryEntryContent.ofSourcePreset VersionNumber.first builtIn >> okOf))
        @ (seedDetectors |> List.map (LibraryEntryContent.ofDetectorPreset VersionNumber.first builtIn >> okOf))
        @ (seedPolarizers |> List.map (LibraryEntryContent.ofPolarizerPreset VersionNumber.first builtIn >> okOf))
        |> List.map (fun (content : LibraryEntryContent) -> content.``sealed`` |> okOf)
        |> CatalogueData.tryCreate CatalogueIds.builtInCatalogue builtInName VersionNumber.first
        |> okOf

    /// The catalogue's `kind` entries, decoded, are exactly `seeds`: the same ids, each with an equal value. Both sides
    /// sort by id, because a catalogue lists its entries by file name and the seeds keep their display order.
    let private assertDecodesToSeeds (kind : LibraryEntryKind) (decode : LibraryEntryContent -> Result<'Entry, LibraryEntryError>) (idOf : 'Entry -> 'Id) (seeds : 'Entry list) (catalogue : CatalogueData) : unit =
        let seedsById = seeds |> List.sortBy idOf

        let decodedById =
            catalogue.envelopes
            |> List.map (fun (envelope : LibraryEntryEnvelope) -> envelope.content)
            |> List.filter (fun (content : LibraryEntryContent) -> content.schema.kind = kind)
            |> List.map (decode >> okOf)
            |> List.sortBy idOf

        Assert.Equal<'Id list>(seedsById |> List.map idOf, decodedById |> List.map idOf)
        Assert.Equal<'Entry list>(seedsById, decodedById)

    [<Fact>]
    let ``the built-in catalogue is the Built-in catalogue at version 1`` () =
        let catalogue = readBuiltIn ()
        let manifest = catalogue.manifest

        Assert.Equal<CatalogueId>(CatalogueIds.builtInCatalogue, manifest.id)
        Assert.Equal<CatalogueName>(builtInName, manifest.name)
        Assert.Equal<VersionNumber>(VersionNumber.first, manifest.version)

        for envelope in catalogue.envelopes do
            Assert.Equal<VersionNumber>(VersionNumber.first, envelope.content.version)
            Assert.Equal<EntryProvenance>(builtIn, envelope.content.provenance)

        let seedCount =
            builtInEntries.Length
            + standardCategories.Length
            + SeedSamples.allSeedSamples.Length
            + SeedSources.allSeedSources.Length
            + seedDetectors.Length
            + seedPolarizers.Length

        Assert.Equal<int>(seedCount, manifest.entries.Length)

        let rowFiles = manifest.entries |> List.map (fun (row : CatalogueManifestRow) -> row.fileName)

        for named in [ FileName "Silicon.material-json"; FileName "Langasite (La3Ga5SiO14).material-json"; FileName "Transparent glass (n = 1.52).material-json" ] do
            Assert.Contains<FileName>(named, rowFiles)

    [<Fact>]
    let ``every built-in material and category in the catalogue equals its seed`` () =
        let catalogue = readBuiltIn ()

        assertDecodesToSeeds MaterialEntryKind (fun content -> content.toMaterialEntry) (fun (entry : MaterialEntry) -> entry.id) builtInEntries catalogue
        assertDecodesToSeeds CategoryEntryKind (fun content -> content.toMaterialCategory) (fun (category : MaterialCategory) -> category.id) standardCategories catalogue

    [<Fact>]
    let ``every built-in sample in the catalogue equals its seed`` () =
        let catalogue = readBuiltIn ()

        assertDecodesToSeeds SampleEntryKind (fun content -> content.toSample) (fun (sample : Sample) -> sample.id) SeedSamples.allSeedSamples catalogue

    [<Fact>]
    let ``every built-in source, detector and polarizer in the catalogue equals its seed`` () =
        let catalogue = readBuiltIn ()

        assertDecodesToSeeds SourceEntryKind (fun content -> content.toSourcePreset) (fun (source : SourcePreset) -> source.id) SeedSources.allSeedSources catalogue
        assertDecodesToSeeds DetectorEntryKind (fun content -> content.toDetectorPreset) (fun (detector : DetectorPreset) -> detector.id) seedDetectors catalogue
        assertDecodesToSeeds PolarizerEntryKind (fun content -> content.toPolarizerPreset) (fun (polarizer : PolarizerPreset) -> polarizer.id) seedPolarizers catalogue

    [<Fact>]
    let ``the built-in catalogue is exactly what the seeds seal to, and its manifest hash is stable`` () =
        let catalogue = readBuiltIn ()
        let expected = expectedCatalogue ()

        Assert.Equal<CatalogueData>(expected, catalogue)

        let hash = catalogue.manifest.manifestHash |> okOf

        Assert.Equal<ContentHash>(expected.manifest.manifestHash |> okOf, hash)
        Assert.Equal<ContentHash>(hash, (readBuiltIn ()).manifest.manifestHash |> okOf)
