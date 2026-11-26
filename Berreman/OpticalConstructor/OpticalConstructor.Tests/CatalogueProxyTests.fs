namespace OpticalConstructor.Tests

open Xunit
open Berreman.Constants
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Dispersion
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles

/// The catalogue seam's data, errors and in-memory double (spec 0050 001 §A; .spec-md §4, §5). The hashes
/// are test-local literals, because Domain does not hash. The entries are test-local too: step 58 deletes
/// the code seeds and may not edit this file.
module CatalogueProxyTests =

    [<Fact>]
    let ``every catalogue error renders its condition and its reason`` () : unit =
        let duplicateVersion : LibraryEntryError = DuplicateEntryVersionErr "entries detail"
        let invalidEntries : CatalogueError = InvalidCatalogueEntriesErr duplicateVersion
        let cases : (CatalogueError * string) list =
            [
                CatalogueIoErr "IO detail", "catalogue IO failed: IO detail"
                CatalogueFileRejectedErr "rejected file detail", "a catalogue file was rejected: rejected file detail"
                MissingEntryFileErr "missing file detail", "an entry file is missing: missing file detail"
                UnlistedEntryFileErr "unlisted file detail", "an entry file is not listed: unlisted file detail"
                ManifestMismatchErr "manifest detail", "the catalogue manifest does not match its entries: manifest detail"
                invalidEntries, "the catalogue entries are invalid: an entry version is duplicated: entries detail"
            ]

        for error, expected in cases do
            Assert.Equal<string>(expected, error.describe)

        let descriptions = cases |> List.map (fun (error, _) -> error.describe)
        Assert.Equal(6, descriptions |> List.distinct |> List.length)
        Assert.Contains(duplicateVersion.describe, invalidEntries.describe)

    let private okOf (result : Result<'Value, 'Problem>) : 'Value =
        match result with
        | Ok value -> value
        | Error problem -> failwith $"expected Ok, got %A{problem}"

    let private builtIn : EntryProvenance = BuiltInProvenance CatalogueIds.builtInCatalogue

    let private catalogueName : CatalogueName = CatalogueName "Built-in"

    let private langasite : MaterialEntry =
        {
            id = MaterialIds.langasite
            name = EntryName "Langasite"
            category = CategoryIds.crystal
            description = Some (EntryDescription "Langasite, a test material")
            physics = ComplexityPhysics { eps = EpsWithoutDispValue (UniaxialTransparent { ordinary = RefractionIndex 1.91; extraordinary = RefractionIndex 2.01 }); magnetic = None; active = None }
        }

    let private quarterWaveCell : Sample =
        {
            id = SampleIds.multilayerQwId
            name = EntryName "Quarter-wave glass and vacuum cell"
            structure = starterMultilayerStructure
            substrate = ThinFilm
            description = EntryDescription "One glass and vacuum period, a test sample"
            supportedEmission = ThinFilm.defaultSupportedEmission
        }

    let private tiSapphire800 : SourcePreset =
        {
            id = PresetIds.tiSapphire800
            name = EntryName "Ti:sapphire laser 800 nm"
            waveLength = WaveLength.nm 800.0<nm>
            tuning = FixedLine
            protection = ProtectedBuiltIn
        }

    let private materialEnvelope : LibraryEntryEnvelope =
        {
            content = LibraryEntryContent.ofMaterialEntry VersionNumber.first builtIn langasite
            contentHash = ContentHash "1a2b3c4d5e6f708192a3b4c5d6e7f8091a2b3c4d5e6f708192a3b4c5d6e7f809"
        }

    let private sampleEnvelope : LibraryEntryEnvelope =
        {
            content = LibraryEntryContent.ofSample VersionNumber.first builtIn quarterWaveCell
            contentHash = ContentHash "b1c2d3e4f5061728394a5b6c7d8e9fa0b1c2d3e4f5061728394a5b6c7d8e9fa0"
        }

    let private sourceEnvelope : LibraryEntryEnvelope =
        {
            content = LibraryEntryContent.ofSourcePreset VersionNumber.first builtIn tiSapphire800 |> okOf
            contentHash = ContentHash "7f6e5d4c3b2a1908f7e6d5c4b3a291807f6e5d4c3b2a1908f7e6d5c4b3a29180"
        }

    /// Given source first, so its envelope order can only come from the manifest's rows.
    let private threeKinds : CatalogueData =
        CatalogueData.tryCreate CatalogueIds.builtInCatalogue catalogueName VersionNumber.first [ sourceEnvelope; materialEnvelope; sampleEnvelope ]
        |> okOf

    let private assertHoldsNothing (proxy : CatalogueProxy) : unit =
        match proxy.readCatalogue () with
        | Error (CatalogueIoErr _) -> ()
        | other -> Assert.Fail($"a store nothing was written to: expected CatalogueIoErr, got %A{other}")

    [<Fact>]
    let ``the in-memory catalogue reads back what it was written`` () =
        let proxy = CatalogueProxy.createInMemory ()

        proxy.writeCatalogue threeKinds |> okOf

        let readBack : CatalogueData = proxy.readCatalogue () |> okOf
        Assert.Equal<CatalogueData>(threeKinds, readBack)
        Assert.Equal<LibraryEntryEnvelope list>([ materialEnvelope; sampleEnvelope; sourceEnvelope ], readBack.envelopes)
        Assert.Equal<CatalogueManifestRow list>(readBack.manifest.entries, readBack.envelopes |> List.map (fun e -> e.manifestRow))

    [<Fact>]
    let ``the in-memory catalogue holds nothing until it is written`` () =
        assertHoldsNothing (CatalogueProxy.createInMemory ())

    [<Fact>]
    let ``the in-memory catalogue reads its manifest alone`` () : unit =
        let proxy = CatalogueProxy.createInMemory ()

        match proxy.readManifest () with
        | Error (CatalogueIoErr _) -> ()
        | other -> Assert.Fail($"expected CatalogueIoErr before a write, got %A{other}")

        proxy.writeCatalogue threeKinds |> okOf

        Assert.Equal<CatalogueManifest>(threeKinds.manifest, proxy.readManifest () |> okOf)

    [<Fact>]
    let ``a catalogue whose manifest its entries do not make is refused and not written`` () =
        let proxy = CatalogueProxy.createInMemory ()

        let rowsWithForeignHash : CatalogueManifestRow list =
            threeKinds.manifest.entries
            |> List.map (fun (row : CatalogueManifestRow) ->
                match row.kind with
                | SourceEntryKind -> { row with contentHash = ContentHash "c0d1e2f3a4b5c6d7e8f90a1b2c3d4e5fc0d1e2f3a4b5c6d7e8f90a1b2c3d4e5f" }
                | _ -> row)

        let foreignRowHash : CatalogueData = { threeKinds with manifest = { threeKinds.manifest with entries = rowsWithForeignHash } }
        let envelopesReversed : CatalogueData = { threeKinds with envelopes = List.rev threeKinds.envelopes }

        for refused in [ foreignRowHash; envelopesReversed ] do
            match proxy.writeCatalogue refused with
            | Error (ManifestMismatchErr reason) -> Assert.Contains("'Built-in'", reason)
            | other -> Assert.Fail($"expected ManifestMismatchErr, got %A{other}")

        assertHoldsNothing proxy

    [<Fact>]
    let ``entries that cannot form a manifest are InvalidCatalogueEntriesErr`` () =
        let sameVersionAgain : LibraryEntryEnvelope = { materialEnvelope with content = { materialEnvelope.content with name = EntryName "Langasite again" } }

        match CatalogueData.tryCreate CatalogueIds.builtInCatalogue catalogueName VersionNumber.first [ materialEnvelope; sameVersionAgain ] with
        | Error (InvalidCatalogueEntriesErr (DuplicateEntryVersionErr _)) -> ()
        | other -> Assert.Fail($"one id at one version twice: expected InvalidCatalogueEntriesErr holding DuplicateEntryVersionErr, got %A{other}")
