namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Geometry
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles

/// The kinds, provenance, ids, name rule and content of the library-entry files, with the builders
/// and decoders that join the content to the domain values (spec 0050 001 §A, §B; .spec-md §3.11, §5).
module LibraryEntryFilesTests =

    [<Fact>]
    let ``every library entry error renders its condition and its reason`` () : unit =
        let cases : (LibraryEntryError * string) list =
            [
                EntryKindMismatchErr "kind detail", "the entry kind does not match: kind detail"
                EntryIdNotGuidErr "id detail", "the entry id is not a canonical GUID: id detail"
                ProvenanceMismatchErr "provenance detail", "the entry provenance does not match: provenance detail"
                MissingEntryFieldErr "field detail", "an entry field is missing: field detail"
                DuplicateEntryFileNameErr "file name detail", "an entry file name is duplicated: file name detail"
                DuplicateEntryVersionErr "version detail", "an entry version is duplicated: version detail"
            ]

        for error, expected in cases do
            Assert.Equal<string>(expected, error.describe)

        let descriptions = cases |> List.map (fun (error, _) -> error.describe)
        Assert.Equal(6, descriptions |> List.distinct |> List.length)

    let private kindsInTagOrder : LibraryEntryKind list =
        [ MaterialEntryKind; SampleEntryKind; SourceEntryKind; PolarizerEntryKind; DetectorEntryKind; CategoryEntryKind ]

    [<Fact>]
    let ``every library entry kind round-trips its wire tag and renders its 001 extension`` () =
        Assert.Equal<int list>([ 1; 2; 3; 4; 5; 6 ], kindsInTagOrder |> List.map (fun k -> k.value))
        Assert.Equal<LibraryEntryKind option list>(kindsInTagOrder |> List.map Some, kindsInTagOrder |> List.map (fun k -> LibraryEntryKind.tryOfWire k.value))
        Assert.Equal<LibraryEntryKind option>(None, LibraryEntryKind.tryOfWire 0)
        Assert.Equal<LibraryEntryKind option>(None, LibraryEntryKind.tryOfWire 7)

        let extensions =
            [
                FileExtension ".material-json"
                FileExtension ".sample-json"
                FileExtension ".source-json"
                FileExtension ".polarizer-json"
                FileExtension ".detector-json"
                FileExtension ".category-json"
            ]

        Assert.Equal<FileExtension list>(extensions, kindsInTagOrder |> List.map (fun k -> k.extension))
        Assert.Equal<FileSchemaVersion list>(kindsInTagOrder |> List.map (fun _ -> FileSchemaVersion 1), kindsInTagOrder |> List.map (fun k -> k.currentSchemaVersion))
        Assert.Equal<LibraryEntryKind list>(kindsInTagOrder, LibraryEntryKind.all)

    [<Fact>]
    let ``a file stem keeps the name's characters and replaces only the ones Windows forbids`` () =
        Assert.Equal<FileStem>(FileStem "Ti_sapphire laser, tunable 700–1000 nm", FileStem.ofText "Ti:sapphire laser, tunable 700–1000 nm")
        Assert.Equal<FileStem>(FileStem "a_b_c_d_e_f_g_h_i_j", FileStem.ofText """a/b\c*d?e"f<g>h|i:j""")
        Assert.Equal<FileStem>(FileStem "Langasite (La3Ga5SiO14)", FileStem.ofText "Langasite (La3Ga5SiO14)")
        Assert.Equal<FileStem>(FileStem "CO₂ laser 10.6 µm", FileStem.ofText "CO₂ laser 10.6 µm")

        let siliconFile = (EntryName "Silicon").fileStem.withExtension MaterialEntryKind.extension
        Assert.Equal<FileName>(FileName "Silicon.material-json", siliconFile)

    [<Fact>]
    let ``a built-in provenance is protected and every other provenance is user-managed`` () =
        let pulled : SourcePullInfo =
            {
                source = MaterialSourceName "refractiveindex.info"
                record = MaterialSourceRecordId "main/Si/Aspnes"
                sourceVersion = MaterialSourceVersion "2024-08-14"
                licence = EntryLicence "CC0-1.0"
                citation = EntryCitation "D. E. Aspnes and A. A. Studna, Phys. Rev. B 27, 985 (1983)"
            }

        let provenances =
            [
                BuiltInProvenance (CatalogueId (Guid.Parse "3f0c2d1e-7a4b-4c5d-9e8f-0a1b2c3d4e5f"))
                UserProvenance
                KitImportProvenance (KitId (Guid.Parse "b2e4f6a8-1c3d-4e5f-8a9b-c0d1e2f3a4b5"))
                SourcePullProvenance pulled
            ]

        Assert.Equal<EntryProtection list>([ ProtectedBuiltIn; UserManaged; UserManaged; UserManaged ], provenances |> List.map (fun p -> p.protection))
        Assert.Equal<CategoryOrigin list>([ BuiltInCategory; UserCategory; UserCategory; UserCategory ], provenances |> List.map (fun p -> p.categoryOrigin))

    [<Fact>]
    let ``a library entry id keeps the GUID of the kind's own id`` () =
        Assert.Equal<Guid>(MaterialIds.silicon.value, MaterialIds.silicon.toLibraryEntryId.value)
        Assert.Equal<Guid>(SampleIds.multilayerQwId.value, SampleIds.multilayerQwId.toLibraryEntryId.value)
        Assert.Equal<Guid>(CategoryIds.glass.value, CategoryIds.glass.toLibraryEntryId.value)

        let ellipsometerGuid = Guid.Parse PresetIds.detectorEllipsometer.value
        Assert.Equal<Guid option>(Some ellipsometerGuid, PresetIds.detectorEllipsometer.tryToLibraryEntryId |> Option.map (fun id -> id.value))

        let notAGuid = ElementValueId "not-a-guid"
        Assert.Equal<LibraryEntryId option>(None, notAGuid.tryToLibraryEntryId)

    let private builtInCatalogue : EntryProvenance = BuiltInProvenance (CatalogueId (Guid.Parse "6d2f4b8a-0c1e-4f3a-9b5d-7e8f0a1b2c3d"))

    let private okOf (result : Result<'Value, LibraryEntryError>) : 'Value =
        match result with
        | Ok value -> value
        | Error error -> failwith $"expected Ok, got %A{error}"

    let private langasite : MaterialEntry = BuiltinCatalogue.material MaterialIds.langasite

    let private intensityDetector : DetectorPreset =
        BuiltinCatalogue.presets ()
        |> List.pick (fun entry ->
            match entry with
            | DetectorItem d when d.id = PresetIds.detectorIntensity -> Some d
            | _ -> None)

    let private glassCategory : MaterialCategory = BuiltinCatalogue.category CategoryIds.glass

    let private tunableTiSapphire : SourcePreset = BuiltinCatalogue.source PresetIds.tiSapphireTunable

    let private tiSapphire800 : SourcePreset = BuiltinCatalogue.source PresetIds.tiSapphire800

    /// A user's compound polarizer: two linear components 45° apart, in light order.
    let private userCompoundPolarizer : PolarizerPreset =
        let atDegrees (d : float) : Angle = Angle.degree (d * 1.0<Berreman.Constants.deg>)
        let lp0 = Propagation.analyzerMueller IdealLinear (atDegrees 0.0)

        {
            id = ElementValueId "5c0e9a7b-3d2f-4e61-8a4b-9f1c2d3e4a5b"
            name = EntryName "My LP + LP at 45°"
            behavior = ConstantMueller [ { matrix = lp0; offset = atDegrees 0.0 }; { matrix = lp0; offset = atDegrees 45.0 } ]
            category = CustomMueller
            protection = UserManaged
        }

    let private schemaOf (kind : LibraryEntryKind) : EntrySchema = { kind = kind; version = FileSchemaVersion 1 }

    [<Fact>]
    let ``every kind's builder and decoder are inverse`` () =
        let materialContent = LibraryEntryContent.ofMaterialEntry VersionNumber.first builtInCatalogue langasite
        Assert.Equal<EntrySchema>(schemaOf MaterialEntryKind, materialContent.schema)
        Assert.Equal<CategoryId option>(Some CategoryIds.crystal, materialContent.categoryOpt)
        Assert.Equal<Result<MaterialEntry, LibraryEntryError>>(Ok langasite, materialContent.toMaterialEntry)

        let sampleContent = LibraryEntryContent.ofSample VersionNumber.first builtInCatalogue (BuiltinCatalogue.sample SampleIds.multilayerQwId)
        Assert.Equal<EntrySchema>(schemaOf SampleEntryKind, sampleContent.schema)
        Assert.Equal<Result<Sample, LibraryEntryError>>(Ok (BuiltinCatalogue.sample SampleIds.multilayerQwId), sampleContent.toSample)

        let sourceContent = LibraryEntryContent.ofSourcePreset VersionNumber.first builtInCatalogue tunableTiSapphire |> okOf
        Assert.Equal<EntrySchema>(schemaOf SourceEntryKind, sourceContent.schema)
        Assert.Equal<Result<SourcePreset, LibraryEntryError>>(Ok tunableTiSapphire, sourceContent.toSourcePreset)

        let detectorContent = LibraryEntryContent.ofDetectorPreset VersionNumber.first builtInCatalogue intensityDetector |> okOf
        Assert.Equal<EntrySchema>(schemaOf DetectorEntryKind, detectorContent.schema)
        Assert.Equal<Result<DetectorPreset, LibraryEntryError>>(Ok intensityDetector, detectorContent.toDetectorPreset)

        let polarizerContent = LibraryEntryContent.ofPolarizerPreset VersionNumber.first.next UserProvenance userCompoundPolarizer |> okOf
        Assert.Equal<EntrySchema>(schemaOf PolarizerEntryKind, polarizerContent.schema)
        Assert.Equal<VersionNumber>(VersionNumber.first.next, polarizerContent.version)
        Assert.Equal<Result<PolarizerPreset, LibraryEntryError>>(Ok userCompoundPolarizer, polarizerContent.toPolarizerPreset)

        let categoryContent = LibraryEntryContent.ofMaterialCategory VersionNumber.first builtInCatalogue glassCategory |> okOf
        Assert.Equal<EntrySchema>(schemaOf CategoryEntryKind, categoryContent.schema)
        Assert.Equal<EntryName>(EntryName "Glass", categoryContent.name)
        Assert.Equal<Result<MaterialCategory, LibraryEntryError>>(Ok glassCategory, categoryContent.toMaterialCategory)

        let notMaterials = [ sampleContent; sourceContent; detectorContent; polarizerContent; categoryContent ]
        Assert.Equal<CategoryId option list>(notMaterials |> List.map (fun _ -> None), notMaterials |> List.map (fun c -> c.categoryOpt))

    [<Fact>]
    let ``a decoder refuses another kind's content`` () =
        let materialContent = LibraryEntryContent.ofMaterialEntry VersionNumber.first builtInCatalogue langasite

        match materialContent.toSample with
        | Error (EntryKindMismatchErr _) -> ()
        | other -> Assert.Fail($"expected EntryKindMismatchErr, got %A{other}")

    [<Fact>]
    let ``a preset or category filed under a provenance that disagrees is refused`` () =
        match LibraryEntryContent.ofSourcePreset VersionNumber.first UserProvenance tunableTiSapphire with
        | Error (ProvenanceMismatchErr _) -> ()
        | other -> Assert.Fail($"a protected built-in source under a user provenance: expected ProvenanceMismatchErr, got %A{other}")

        let userCategory : MaterialCategory =
            {
                id = CategoryId (Guid.Parse "1e7c5a3b-9d2f-4b8e-a6c0-3f5d7b9e1a2c")
                name = CategoryName "Oxides"
                visibility = SelectableOnCreate
                origin = UserCategory
            }

        match LibraryEntryContent.ofMaterialCategory VersionNumber.first builtInCatalogue userCategory with
        | Error (ProvenanceMismatchErr _) -> ()
        | other -> Assert.Fail($"a user category under a built-in provenance: expected ProvenanceMismatchErr, got %A{other}")

        let notAGuidPolarizer : PolarizerPreset = { userCompoundPolarizer with id = ElementValueId "my-own-polarizer" }

        match LibraryEntryContent.ofPolarizerPreset VersionNumber.first UserProvenance notAGuidPolarizer with
        | Error (EntryIdNotGuidErr _) -> ()
        | other -> Assert.Fail($"a preset id that is not a GUID: expected EntryIdNotGuidErr, got %A{other}")

        let upperCaseGuidPolarizer : PolarizerPreset = { userCompoundPolarizer with id = ElementValueId "5C0E9A7B-3D2F-4E61-8A4B-9F1C2D3E4A5B" }

        match LibraryEntryContent.ofPolarizerPreset VersionNumber.first UserProvenance upperCaseGuidPolarizer with
        | Error (EntryIdNotGuidErr _) -> ()
        | other -> Assert.Fail($"a preset id in upper-case GUID text: expected EntryIdNotGuidErr, got %A{other}")

    [<Fact>]
    let ``a material without a category or a sample without a description does not decode`` () =
        let materialContent = LibraryEntryContent.ofMaterialEntry VersionNumber.first builtInCatalogue langasite

        match { materialContent with categoryOpt = None }.toMaterialEntry with
        | Error (MissingEntryFieldErr _) -> ()
        | other -> Assert.Fail($"a material without a category: expected MissingEntryFieldErr, got %A{other}")

        let sampleContent = LibraryEntryContent.ofSample VersionNumber.first builtInCatalogue (BuiltinCatalogue.sample SampleIds.multilayerQwId)

        match { sampleContent with descriptionOpt = None }.toSample with
        | Error (MissingEntryFieldErr _) -> ()
        | other -> Assert.Fail($"a sample without a description: expected MissingEntryFieldErr, got %A{other}")

    [<Fact>]
    let ``an entry's file name is its name made safe plus its kind's extension`` () =
        let sourceContent = LibraryEntryContent.ofSourcePreset VersionNumber.first builtInCatalogue tiSapphire800 |> okOf
        Assert.Equal<FileName>(FileName "Ti_sapphire laser 800 nm.source-json", sourceContent.fileName)

        let materialContent = LibraryEntryContent.ofMaterialEntry VersionNumber.first builtInCatalogue langasite
        Assert.Equal<FileName>(FileName "Langasite (La3Ga5SiO14).material-json", materialContent.fileName)

    let private materialHash : ContentHash = ContentHash "0f1e2d3c4b5a69788796a5b4c3d2e1f00f1e2d3c4b5a69788796a5b4c3d2e1f0"

    let private sampleHash : ContentHash = ContentHash "a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c6d7e8f90"

    let private sourceHash : ContentHash = ContentHash "5e6d7c8b9a0f1e2d3c4b5a6978695a4b3c2d1e0f5e6d7c8b9a0f1e2d3c4b5a69"

    let private testCatalogueId : CatalogueId = CatalogueId (Guid.Parse "8c3e5a7b-1d9f-4e2a-b6c8-0d2f4a6b8e1c")

    let private testCatalogueName : CatalogueName = CatalogueName "Test catalogue"

    let private manifestOf (envelopes : LibraryEntryEnvelope list) : Result<CatalogueManifest, LibraryEntryError> =
        CatalogueManifest.tryCreate testCatalogueId testCatalogueName VersionNumber.first envelopes

    let private materialEnvelope : LibraryEntryEnvelope =
        { content = LibraryEntryContent.ofMaterialEntry VersionNumber.first builtInCatalogue langasite; contentHash = materialHash }

    let private sampleEnvelope : LibraryEntryEnvelope =
        {
            content = LibraryEntryContent.ofSample VersionNumber.first builtInCatalogue (BuiltinCatalogue.sample SampleIds.multilayerQwId)
            contentHash = sampleHash
        }

    let private sourceEnvelope : LibraryEntryEnvelope =
        { content = LibraryEntryContent.ofSourcePreset VersionNumber.first builtInCatalogue tiSapphire800 |> okOf; contentHash = sourceHash }

    /// The envelope under another entry's identity: its own GUID and name, at the same version.
    let private renamed (id : LibraryEntryId) (name : EntryName) (envelope : LibraryEntryEnvelope) : LibraryEntryEnvelope =
        { envelope with content = { envelope.content with id = id; name = name } }

    [<Fact>]
    let ``a manifest lists one row per envelope, sorted by kind and file name`` () =
        let manifest = manifestOf [ sourceEnvelope; materialEnvelope; sampleEnvelope ] |> okOf

        Assert.Equal<FileSchemaVersion>(FileSchemaVersion 1, manifest.schemaVersion)
        Assert.Equal<FileSchemaVersion>(FileSchemaVersion 1, CatalogueManifest.currentSchemaVersion)
        Assert.Equal<CatalogueId>(testCatalogueId, manifest.id)
        Assert.Equal<CatalogueName>(testCatalogueName, manifest.name)
        Assert.Equal<VersionNumber>(VersionNumber.first, manifest.version)
        Assert.Equal<CatalogueManifestRow list>([ materialEnvelope.manifestRow; sampleEnvelope.manifestRow; sourceEnvelope.manifestRow ], manifest.entries)
        Assert.Equal<LibraryEntryKind list>([ MaterialEntryKind; SampleEntryKind; SourceEntryKind ], manifest.entries |> List.map (fun r -> r.kind))

        let sourceRow : CatalogueManifestRow =
            {
                kind = SourceEntryKind
                id = sourceEnvelope.content.id
                version = VersionNumber.first
                fileName = FileName "Ti_sapphire laser 800 nm.source-json"
                contentHash = sourceHash
            }

        Assert.Equal<CatalogueManifestRow>(sourceRow, sourceEnvelope.manifestRow)

        let zinc = renamed (LibraryEntryId (Guid.Parse "2b4d6f8a-0c1e-4a3b-9d5f-7a9c1e3b5d7f")) (EntryName "Zinc") materialEnvelope
        let alumina = renamed (LibraryEntryId (Guid.Parse "4d6f8a0c-2e1b-4c5d-8f7a-9b1d3f5a7c9e")) (EntryName "Alumina") materialEnvelope
        let byFileName = manifestOf [ zinc; materialEnvelope; alumina ] |> okOf
        Assert.Equal<FileName list>([ FileName "Alumina.material-json"; FileName "Langasite (La3Ga5SiO14).material-json"; FileName "Zinc.material-json" ], byFileName.entries |> List.map (fun r -> r.fileName))
        Assert.Equal<CatalogueManifest>(byFileName, manifestOf [ alumina; zinc; materialEnvelope ] |> okOf)

        Assert.Equal<FileName>(FileName ".catalogue-json", CatalogueManifest.fileName)
        Assert.Equal<FileExtension>(FileExtension ".catalogue-json", CatalogueManifest.extension)

    [<Fact>]
    let ``a manifest refuses two entries of one kind with one file name`` () =
        let silicon = renamed (LibraryEntryId (Guid.Parse "6f8a0c2e-4b1d-4e5f-a7c9-1d3f5b7a9c0e")) (EntryName "Silicon") materialEnvelope
        let lowerSilicon = renamed (LibraryEntryId (Guid.Parse "8a0c2e4b-6d1f-4a7c-b9e1-3f5a7c9b0d2f")) (EntryName "silicon") materialEnvelope

        match manifestOf [ silicon; lowerSilicon ] with
        | Error (DuplicateEntryFileNameErr reason) ->
            Assert.Contains("'Silicon'", reason)
            Assert.Contains("'silicon'", reason)
        | other -> Assert.Fail($"Silicon and silicon: expected DuplicateEntryFileNameErr, got %A{other}")

        let colon = renamed (LibraryEntryId (Guid.Parse "0c2e4b6d-8f1a-4c9e-a1b3-5d7f9a1c3e5b")) (EntryName "Si:O") materialEnvelope
        let underscore = renamed (LibraryEntryId (Guid.Parse "2e4b6d8f-0a1c-4e3b-b5d7-9f1a3c5e7b9d")) (EntryName "Si_O") materialEnvelope

        match manifestOf [ colon; materialEnvelope; underscore ] with
        | Error (DuplicateEntryFileNameErr _) -> ()
        | other -> Assert.Fail($"Si:O and Si_O: expected DuplicateEntryFileNameErr, got %A{other}")

        let glassMaterial = renamed (LibraryEntryId (Guid.Parse "4b6d8f0a-2c1e-4b5d-8e7f-1a3c5e7b9d0f")) (EntryName "Glass") materialEnvelope
        let glassSample = renamed (LibraryEntryId (Guid.Parse "6d8f0a2c-4e1b-4d7f-9a1c-3e5b7d9f1a2c")) (EntryName "Glass") sampleEnvelope
        let glasses = manifestOf [ glassSample; glassMaterial ] |> okOf
        Assert.Equal<FileName list>([ FileName "Glass.material-json"; FileName "Glass.sample-json" ], glasses.entries |> List.map (fun r -> r.fileName))

    [<Fact>]
    let ``a manifest refuses one entry version twice`` () =
        let sameVersionAgain = { materialEnvelope with content = { materialEnvelope.content with name = EntryName "Langasite again" } }

        match manifestOf [ materialEnvelope; sourceEnvelope; sameVersionAgain ] with
        | Error (DuplicateEntryVersionErr reason) ->
            Assert.Contains("'Langasite (La3Ga5SiO14)'", reason)
            Assert.Contains("'Langasite again'", reason)
        | other -> Assert.Fail($"one id at one version twice: expected DuplicateEntryVersionErr, got %A{other}")
