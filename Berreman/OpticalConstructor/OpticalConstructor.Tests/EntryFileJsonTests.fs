namespace OpticalConstructor.Tests

open System
open System.Numerics
open System.Text.Json.Nodes
open Xunit
open Berreman.Constants
open Berreman.Fields
open Berreman.Geometry
open Berreman.MaterialProperties
open Berreman.Dispersion
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.DispersionModels
open OpticalConstructor.Domain.MaterialComplexityEditor
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.EntryTextDiff
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.CanonicalJson
open OpticalConstructor.Storage.EntryFileJson

/// One library entry per file: every kind is written, read back through its schema, version and hash checks
/// and decoded to the value it was built from; each failure is its typed error (spec 0050 001 §B). The catalogue
/// manifest covers BuiltinCatalogue.materials/categories/samples/presets (001 §A, §B; S1.7).
module EntryFileJsonTests =

    let private okOf (result : Result<'Value, StorageError>) : 'Value =
        match result with
        | Ok value -> value
        | Error error -> failwith $"expected Ok, got %A{error}"

    let private decodedOf (result : Result<'Value, LibraryEntryError>) : 'Value =
        match result with
        | Ok value -> value
        | Error error -> failwith $"expected Ok, got %A{error}"

    let private testCatalogueId : CatalogueId = CatalogueId (Guid.Parse "9a3e5c71-2b84-4d6f-a0c9-6e1f3b7d5a28")

    let private builtInCatalogue : EntryProvenance = BuiltInProvenance testCatalogueId

    /// The file of `content`, and the envelope its text parses back to as a `kind` entry, which equals the
    /// sealed one.
    let private roundTrip (content : LibraryEntryContent) : LibraryEntryEnvelope =
        let envelope = content.``sealed`` |> okOf
        let text = envelope.fileText |> okOf
        let readBack = text.parseEntry content.schema.kind |> okOf
        Assert.Equal<LibraryEntryEnvelope>(envelope, readBack)
        readBack

    let private assertMaterialRoundTrips (provenance : EntryProvenance) (entry : MaterialEntry) : unit =
        let readBack = LibraryEntryContent.ofMaterialEntry VersionNumber.first provenance entry |> roundTrip
        Assert.Equal<MaterialEntry>(entry, readBack.content.toMaterialEntry |> decodedOf)
        Assert.Equal<EntryProvenance>(provenance, readBack.content.provenance)

    let private materialWith (idText : string) (name : string) (complexity : MaterialComplexity) : MaterialEntry =
        {
            id = Guid.Parse idText |> MaterialId
            name = EntryName name
            category = CategoryIds.crystal
            description = Some (EntryDescription $"{name}, a test material")
            complexity = complexity
        }

    let private epsOnly (eps : EpsWithDispValue) : MaterialComplexity = { eps = eps; magnetic = None; active = None }

    let private complexIndex (re : float) (im : float) : ComplexRefractionIndex = ComplexRefractionIndex (Complex(re, im))

    let private constantEpsValues : ConstantEpsValue list =
        [
            IsotropicTransparent (RefractionIndex 1.5234)
            IsotropicAbsorbing (complexIndex 0.99 0.045)
            UniaxialTransparent { ordinary = RefractionIndex 1.544; extraordinary = RefractionIndex 1.553 }
            UniaxialAbsorbing { ordinaryComplex = complexIndex 2.1 0.02; extraordinaryComplex = complexIndex 2.2 0.03 }
            BiaxialTransparent { nx = RefractionIndex 1.51; ny = RefractionIndex 1.52; nz = RefractionIndex 1.53 }
            BiaxialAbsorbing { nxComplex = complexIndex 1.61 0.001; nyComplex = complexIndex 1.62 0.002; nzComplex = complexIndex 1.63 0.003 }
        ]

    let private interval (lower : float<nm>) (upper : float<nm>) : WaveLengthInterval =
        { lower = WaveLength.nm lower; upper = WaveLength.nm upper }

    let private cauchyAxis : EpsAxisDispersion =
        (Cauchy { a = 1.4580; b = 0.00354; c = 0.0; wavelengthUnit = Micrometer; thermoOptic = None }).toEpsAxis

    let private sellmeierAxis : EpsAxisDispersion =
        (Sellmeier { b = [ 0.6961663; 0.4079426; 0.8974794 ]; c = [ 0.0046791; 0.0135121; 97.934 ]; wavelengthUnit = Micrometer; thermoOptic = None }).toEpsAxis

    let private tabulatedAxis : EpsAxisDispersion =
        match TabulatedNkTable.tryCreate [| 400e-9<meter>; 550e-9<meter>; 700e-9<meter> |] [| 1.47; 1.46; 1.455 |] [| 0.002; 0.001; 0.0 |] with
        | Ok table -> TabulatedNK table
        | Error error -> failwith $"the fixture's table is refused: %A{error}"

    let private taucLorentzAxis : EpsAxisDispersion =
        (TaucLorentz { epsInf = 1.15; amplitude = 122.0; resonance = 3.45; broadening = 2.54; bandGap = 1.12; wavelengthUnit = ElectronVolt; thermoOptic = None }).toEpsAxis

    let private gaussianAxis : EpsAxisDispersion =
        (GaussianOscillator { epsInf = 2.25; amplitude = 1.8; energy = 4.6; broadening = 0.75; wavelengthUnit = ElectronVolt; thermoOptic = None }).toEpsAxis

    let private forouhiBloomerAxis : EpsAxisDispersion =
        (ForouhiBloomer { nInf = 1.9; a = 0.12; b = 9.2; c = 22.4; bandGap = 3.1; wavelengthUnit = ElectronVolt; thermoOptic = None }).toEpsAxis

    let private brendelBormannAxis : EpsAxisDispersion =
        let coefficients : BrendelBormannCoefficients =
            {
                plasmaFrequency = 9.03
                intrabandStrength = 0.770
                intrabandDamping = 0.050
                strength = [ 0.054; 0.050 ]
                resonance = [ 0.218; 2.885 ]
                damping = [ 0.074; 0.035 ]
                broadening = [ 0.742; 0.349 ]
                wavelengthUnit = ElectronVolt
                thermoOptic = None
            }

        (BrendelBormann coefficients).toEpsAxis

    /// Their segments together carry every `EpsAxisDispersion` case and every `PublishedIndexLaw`.
    let private dispersiveEpsValues : EpsWithDispValue list =
        [
            IsotropicDispersive
                [
                    { wavelengthInterval = interval 300.0<nm> 600.0<nm>; dispersion = cauchyAxis }
                    { wavelengthInterval = interval 600.0<nm> 2000.0<nm>; dispersion = sellmeierAxis }
                    { wavelengthInterval = interval 2000.0<nm> 4000.0<nm>; dispersion = PublishedLawAxis SiliconIndexLaw }
                ]
            UniaxialDispersive
                [
                    { wavelengthInterval = interval 400.0<nm> 700.0<nm>; ordinaryDispersion = tabulatedAxis; extraordinaryDispersion = taucLorentzAxis }
                    { wavelengthInterval = interval 700.0<nm> 1000.0<nm>; ordinaryDispersion = PublishedLawAxis LangasiteOrdinaryIndexLaw; extraordinaryDispersion = PublishedLawAxis LangasiteExtraordinaryIndexLaw }
                ]
            BiaxialDispersive
                [
                    { wavelengthInterval = interval 250.0<nm> 1700.0<nm>; xDispersion = gaussianAxis; yDispersion = forouhiBloomerAxis; zDispersion = brendelBormannAxis }
                ]
        ]
        |> List.map EpsWithDispValue

    let private gyrationFormula : DispersionFormula =
        {
            terms = [ { lambda = 0.0; coefficients = [| 2.5e-5; -1.25e-6 |]; power = 1; multiplier = 1.0 } ]
            wavelengthScale = 1e-6<meter>
        }

    let private magneticAspects : MuWithDispValue list =
        [
            MuWithoutDispValue (GyromagneticMu { muDiagonal = MuValue 1.0; muParallel = MuValue 1.05; gyration = MuValue 0.1; axis = AlongX })
            MuWithDispValue defaultPolderDispersion
        ]

    let private activeAspects : RhoWithDispValue list =
        [
            RhoWithoutDispValue { gyration = UniaxialActive { g11 = RhoValue 1.2e-5; g33 = RhoValue (-3.4e-6) }; hand = RightHanded }
            RhoWithDispValue { gyration = CubicActive gyrationFormula; hand = LeftHanded }
            PublishedLawRho LangasiteGyrationLaw
        ]

    [<Fact>]
    let ``a material entry file round-trips every complexity shape`` () =
        let constantEps = EpsWithoutDispValue (IsotropicTransparent (RefractionIndex 1.5))

        let complexities =
            (constantEpsValues |> List.map (EpsWithoutDispValue >> epsOnly))
            @ (dispersiveEpsValues |> List.map epsOnly)
            @ (magneticAspects |> List.map (fun magnetic -> { epsOnly constantEps with magnetic = Some magnetic }))
            @ (activeAspects |> List.map (fun active -> { epsOnly constantEps with active = Some active }))

        Assert.Equal(6 + 3 + 2 + 3, List.length complexities)

        complexities
        |> List.iteri (fun i complexity ->
            materialWith $"5b8d2e4f-7a1c-4e93-b6d0-{i + 1:D12}" $"Complexity shape {i + 1}" complexity
            |> assertMaterialRoundTrips builtInCatalogue)

    [<Fact>]
    let ``a multilayer sample entry file round-trips`` () =
        for sample in [ BuiltinCatalogue.sample SampleIds.multilayerQwId; BuiltinCatalogue.sample SampleIds.langasiteSiliconId ] do
            let readBack = LibraryEntryContent.ofSample VersionNumber.first builtInCatalogue sample |> roundTrip
            Assert.Equal<Sample>(sample, readBack.content.toSample |> decodedOf)

        match (BuiltinCatalogue.sample SampleIds.multilayerQwId).structure.films with
        | Repeated _ :: _ -> ()
        | other -> Assert.Fail($"the quarter-wave stack opens with a period group, not %A{other}")

    /// A user's own source: a fixed line, so both tunings are written.
    let private userSource : SourcePreset =
        {
            id = ElementValueId "c6a1e9d4-3f72-4b58-9e0a-1d7c5b3f8e26"
            name = EntryName "My dye laser 590 nm"
            waveLength = WaveLength.nm 590.0<nm>
            tuning = FixedLine
            protection = UserManaged
        }

    [<Fact>]
    let ``a tunable source entry file round-trips`` () =
        let tunable = BuiltinCatalogue.source PresetIds.tiSapphireTunable

        for provenance, source in [ builtInCatalogue, tunable; UserProvenance, userSource ] do
            let readBack = LibraryEntryContent.ofSourcePreset VersionNumber.first provenance source |> decodedOf |> roundTrip
            Assert.Equal<SourcePreset>(source, readBack.content.toSourcePreset |> decodedOf)

    let private atDegrees (d : float) : Angle = Angle.degree (d * 1.0<deg>)

    [<Fact>]
    let ``a constant-Mueller polarizer entry file round-trips`` () =
        let lp = Propagation.analyzerMueller IdealLinear (atDegrees 0.0)
        let cp = Propagation.analyzerMueller IdealCircularRight (atDegrees 0.0)

        let polarizer : PolarizerPreset =
            {
                id = ElementValueId "e3b7f1a5-9c2d-4e86-b0f4-7a5d1c9e3b62"
                name = EntryName "My LP + CP at 30°"
                behavior = ConstantMueller [ { matrix = lp; offset = atDegrees 0.0 }; { matrix = cp; offset = atDegrees 30.0 } ]
                category = CustomMueller
                protection = UserManaged
            }

        let readBack = LibraryEntryContent.ofPolarizerPreset VersionNumber.first UserProvenance polarizer |> decodedOf |> roundTrip
        Assert.Equal<PolarizerPreset>(polarizer, readBack.content.toPolarizerPreset |> decodedOf)

    [<Fact>]
    let ``a detector and a category entry file round-trip`` () =
        let ellipsometer =
            BuiltinCatalogue.presets ()
            |> List.pick (fun entry ->
                match entry with
                | DetectorItem d when d.id = PresetIds.detectorEllipsometer -> Some d
                | _ -> None)

        let detectorBack = LibraryEntryContent.ofDetectorPreset VersionNumber.first builtInCatalogue ellipsometer |> decodedOf |> roundTrip
        Assert.Equal<DetectorPreset>(ellipsometer, detectorBack.content.toDetectorPreset |> decodedOf)

        let vacuum = BuiltinCatalogue.category CategoryIds.vacuumCategory
        let categoryBack = LibraryEntryContent.ofMaterialCategory VersionNumber.first builtInCatalogue vacuum |> decodedOf |> roundTrip
        Assert.Equal<MaterialCategory>(vacuum, categoryBack.content.toMaterialCategory |> decodedOf)

    let private pulledFromSource : SourcePullInfo =
        {
            source = MaterialSourceName "refractiveindex.info"
            record = MaterialSourceRecordId "main/SiO2/Malitson"
            sourceVersion = MaterialSourceVersion "2024-08-14"
            licence = EntryLicence "CC-BY-4.0"
            citation = EntryCitation "I. H. Malitson, J. Opt. Soc. Am. 55, 1205–1208 (1965)"
        }

    let private allProvenances : EntryProvenance list =
        [
            builtInCatalogue
            UserProvenance
            KitImportProvenance (KitId (Guid.Parse "4f8b2d6a-1e3c-4a7b-9d5f-0c2e6a8b4d17"))
            SourcePullProvenance pulledFromSource
        ]

    let private silica : MaterialEntry =
        materialWith "1d5f9b3e-7c2a-4e68-a4b0-3f9d7e1c5a82" "Fused silica (Malitson)" (epsOnly (EpsWithDispValue (IsotropicDispersive [ { wavelengthInterval = interval 210.0<nm> 3710.0<nm>; dispersion = sellmeierAxis } ])))

    [<Fact>]
    let ``every provenance round-trips through an entry file`` () =
        for provenance in allProvenances do
            assertMaterialRoundTrips provenance silica

        let forkInfo : EntryForkInfo =
            {
                originalId = silica.id.toLibraryEntryId
                originalVersion = VersionNumber.first
                originalProvenance = SourcePullProvenance pulledFromSource
            }

        let forkProvenance = ForkProvenance forkInfo
        let forkedSilica : MaterialEntry =
            {
                silica with
                    id = MaterialId (Guid.Parse "973bf1c6-84ed-483a-b520-e12f78079d63")
            }

        assertMaterialRoundTrips forkProvenance forkedSilica

        let nestedForkInfo : EntryForkInfo =
            {
                originalId = forkedSilica.id.toLibraryEntryId
                originalVersion = VersionNumber.first
                originalProvenance = forkProvenance
            }

        let nestedForkProvenance = ForkProvenance nestedForkInfo
        let nestedForkedSilica : MaterialEntry =
            {
                silica with
                    id = MaterialId (Guid.Parse "da827831-4e65-4fd8-952f-391e240f7eac")
            }

        assertMaterialRoundTrips nestedForkProvenance nestedForkedSilica

    /// A constant material with one distinctive number, the one a tampering test changes.
    let private tamperable : MaterialEntry =
        materialWith "7e2c4a9b-5d13-4f68-9a0e-3c1f7d2b8e64" "Transparent test glass" (epsOnly (EpsWithoutDispValue (IsotropicTransparent (RefractionIndex 1.5234))))

    let private tamperableContent : LibraryEntryContent = LibraryEntryContent.ofMaterialEntry VersionNumber.first UserProvenance tamperable

    let private fileOf (content : LibraryEntryContent) : LibraryFileText = content.``sealed`` |> Result.bind (fun envelope -> envelope.fileText) |> okOf

    [<Fact>]
    let ``an entry file is indented canonical JSON whose hash survives a write and a read`` () =
        let envelope = tamperableContent.``sealed`` |> okOf
        let text = envelope.fileText |> okOf

        let node = JsonNode.Parse text.value
        Assert.Equal<string>($"{node.indentedCanonicalText}\n", text.value)
        Assert.EndsWith("}\n", text.value)
        Assert.False(text.value.EndsWith "\n\n", "the file ends with exactly one line end")
        Assert.DoesNotContain("\r", text.value)

        let keys = [ for KeyValue (key, _) in node.AsObject() -> key ]
        Assert.Equal<string list>([ "categoryOpt"; "contentHash"; "descriptionOpt"; "id"; "name"; "payload"; "provenance"; "schema"; "version" ], keys)

        let readBack = text.parseEntry MaterialEntryKind |> okOf
        Assert.Equal<LibraryFileText>(text, readBack.fileText |> okOf)
        Assert.Equal<ContentHash>(contentHashOf tamperableContent |> okOf, readBack.contentHash)
        Assert.Equal<ContentHash>(contentHashOf readBack.content |> okOf, readBack.contentHash)

    [<Fact>]
    let ``two versions of an entry diff by the lines of their files`` () : unit =
        let held =
            (BuiltinCatalogue.catalogueData ()).envelopes
            |> List.find (fun envelope -> envelope.content.id = MaterialIds.silicon.toLibraryEntryId)
        let changedContent =
            {
                held.content with
                    descriptionOpt = Some (EntryDescription "Silicon with a revised description")
            }
        let incoming = changedContent.``sealed`` |> okOf

        let fileLines (envelope : LibraryEntryEnvelope) : EntryTextLine list =
            let text = envelope.fileText |> okOf
            let segments = text.value.Split '\n'
            Assert.Equal<string>("", Array.last segments)
            segments |> Array.take (segments.Length - 1) |> Array.map EntryTextLine |> Array.toList

        let changedLine (line : EntryTextLine) : bool =
            line.value.StartsWith("  \"contentHash\":", StringComparison.Ordinal)
            || line.value.StartsWith("  \"descriptionOpt\":", StringComparison.Ordinal)

        let heldLines = fileLines held
        let incomingLines = fileLines incoming
        let removed = heldLines |> List.filter changedLine
        let added = incomingLines |> List.filter changedLine
        let unchanged = heldLines |> List.filter (changedLine >> not)
        let actual = incoming.diffFrom held |> okOf

        Assert.Equal(2, removed.Length)
        Assert.Equal(2, added.Length)
        Assert.Equal<EntryTextLine list>(removed, actual |> List.choose (function RemovedEntryLine line -> Some line | _ -> None))
        Assert.Equal<EntryTextLine list>(added, actual |> List.choose (function AddedEntryLine line -> Some line | _ -> None))
        Assert.Equal<EntryTextLine list>(unchanged, actual |> List.choose (function SameEntryLine line -> Some line | _ -> None))
        Assert.Equal<EntryTextLine list>(heldLines, actual |> List.choose (function SameEntryLine line | RemovedEntryLine line -> Some line | _ -> None))
        Assert.Equal<EntryTextLine list>(incomingLines, actual |> List.choose (function SameEntryLine line | AddedEntryLine line -> Some line | _ -> None))

        let beforeChanges = heldLines |> List.takeWhile (changedLine >> not) |> List.map SameEntryLine
        let afterChanges = heldLines |> List.skipWhile (changedLine >> not) |> List.skipWhile changedLine |> List.map SameEntryLine
        Assert.Equal<EntryDiffLine list>(beforeChanges @ (removed |> List.map RemovedEntryLine) @ (added |> List.map AddedEntryLine) @ afterChanges, actual)
        Assert.Equal<EntryDiffLine list>(heldLines |> List.map SameEntryLine, held.diffFrom held |> okOf)

        let invalidIndex = RefractionIndex Double.NaN
        let invalidPayload = MaterialPayload (epsOnly (EpsWithoutDispValue (IsotropicTransparent invalidIndex)))
        let invalidEnvelope =
            {
                held with
                    content =
                        {
                            held.content with
                                payload = invalidPayload
                        }
            }

        match invalidEnvelope.fileText with
        | Error error ->
            Assert.Equal<Result<EntryDiffLine list, StorageError>>(Error error, incoming.diffFrom invalidEnvelope)
            Assert.Equal<Result<EntryDiffLine list, StorageError>>(Error error, invalidEnvelope.diffFrom held)
        | Ok _ -> Assert.Fail "a non-finite index must fail canonical rendering"

    /// `file` with its JSON changed by `edit`.
    let private editedText (file : LibraryFileText) (edit : JsonObject -> unit) : LibraryFileText =
        let root = (JsonNode.Parse file.value).AsObject()
        edit root
        LibraryFileText (root.ToJsonString())

    /// The file of `content` with its JSON changed by `edit`.
    let private editedFileOf (content : LibraryEntryContent) (edit : JsonObject -> unit) : LibraryFileText =
        editedText (fileOf content) edit

    [<Fact>]
    let ``an entry file of another schema version is UnsupportedSchemaVersionErr`` () =
        let atVersion2 (entry : JsonObject) : unit = entry["schema"]["version"] <- JsonValue.Create 2
        let expected = UnsupportedSchemaVersionErr { fileExtension = MaterialEntryKind.extension; found = FileSchemaVersion 2; expected = FileSchemaVersion 1 }

        let intact = editedFileOf tamperableContent atVersion2
        let emptyPayload = editedFileOf tamperableContent (fun entry -> atVersion2 entry; entry["payload"] <- JsonObject())

        for text in [ intact; emptyPayload ] do
            Assert.Equal<Result<LibraryEntryEnvelope, StorageError>>(Error expected, text.parseEntry MaterialEntryKind)

    [<Fact>]
    let ``a tampered entry file is ContentHashMismatchErr`` () =
        let envelope = tamperableContent.``sealed`` |> okOf
        let original = envelope.fileText |> okOf
        Assert.Equal(1, original.value.Split("1.5234").Length - 1)
        let tampered = LibraryFileText (original.value.Replace("1.5234", "1.5243"))

        let changed = { tamperableContent with payload = MaterialPayload (epsOnly (EpsWithoutDispValue (IsotropicTransparent (RefractionIndex 1.5243)))) }
        let expected = ContentHashMismatchErr { entryId = tamperableContent.id; recorded = envelope.contentHash; computed = contentHashOf changed |> okOf }
        Assert.NotEqual<ContentHash>(envelope.contentHash, contentHashOf changed |> okOf)
        Assert.Equal<Result<LibraryEntryEnvelope, StorageError>>(Error expected, tampered.parseEntry MaterialEntryKind)

    let private assertSchemaInvalid (place : string) (result : Result<'Value, StorageError>) : unit =
        match result with
        | Error (SchemaValidationErr messages) -> Assert.True(not (List.isEmpty messages), $"{place}: the error carries the schema's messages")
        | other -> Assert.Fail($"{place}: expected SchemaValidationErr, got %A{other}")

    [<Fact>]
    let ``a schema-invalid or wrong-kind entry file is SchemaValidationErr`` () =
        let withoutName = editedFileOf tamperableContent (fun entry -> entry.Remove "name" |> ignore)
        assertSchemaInvalid "a material file without its name" (withoutName.parseEntry MaterialEntryKind)

        let sampleFile = fileOf (LibraryEntryContent.ofSample VersionNumber.first builtInCatalogue (BuiltinCatalogue.sample SampleIds.langasiteSiliconId))
        assertSchemaInvalid "a sample's file read as a material" (sampleFile.parseEntry MaterialEntryKind)

        match (LibraryFileText "{ this is not JSON").parseEntry MaterialEntryKind with
        | Error (MalformedJsonErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason, "the error carries the parser's message")
        | other -> Assert.Fail($"a text that is not JSON: expected MalformedJsonErr, got %A{other}")

    /// A catalogue holding a material, a sample and a source.
    let private threeKindManifest : CatalogueManifest =
        let contents =
            [
                LibraryEntryContent.ofSourcePreset VersionNumber.first builtInCatalogue (BuiltinCatalogue.source PresetIds.tiSapphireTunable) |> decodedOf
                LibraryEntryContent.ofMaterialEntry VersionNumber.first builtInCatalogue silica
                LibraryEntryContent.ofSample VersionNumber.first builtInCatalogue (BuiltinCatalogue.sample SampleIds.langasiteSiliconId)
            ]

        contents
        |> List.map (fun content -> content.``sealed`` |> okOf)
        |> CatalogueManifest.tryCreate testCatalogueId (CatalogueName "Test catalogue") VersionNumber.first
        |> decodedOf

    let private rowWithHash (hash : ContentHash) (row : CatalogueManifestRow) : CatalogueManifestRow = { row with contentHash = hash }

    [<Fact>]
    let ``a catalogue manifest round-trips and its hash is stable`` () =
        let manifest = threeKindManifest
        let text = manifest.fileText |> okOf

        let node = JsonNode.Parse text.value
        Assert.Equal<string>($"{node.indentedCanonicalText}\n", text.value)
        Assert.False(text.value.EndsWith "\n\n", "the file ends with exactly one line end")
        Assert.Equal<string list>([ "entries"; "id"; "name"; "schemaVersion"; "version" ], [ for KeyValue (key, _) in node.AsObject() -> key ])

        let readBack = text.parseCatalogueManifest |> okOf
        Assert.Equal<CatalogueManifest>(manifest, readBack)
        Assert.Equal<LibraryFileText>(text, readBack.fileText |> okOf)

        let hash = manifest.manifestHash |> okOf
        Assert.Equal<ContentHash>(hash, readBack.manifestHash |> okOf)
        Assert.Equal<ContentHash>(contentHashOf manifest |> okOf, hash)

        match manifest.entries with
        | first :: rest ->
            let changedRow = first |> rowWithHash (contentHashOf "another entry's content" |> okOf)
            let changed = { manifest with entries = changedRow :: rest }
            Assert.NotEqual<ContentHash>(hash, changed.manifestHash |> okOf)
        | [] -> Assert.Fail "the three-kind manifest has no rows"

    /// The manifest's file with its JSON changed by `edit`.
    let private editedManifestFile (edit : JsonObject -> unit) : LibraryFileText =
        editedText (threeKindManifest.fileText |> okOf) edit

    [<Fact>]
    let ``a catalogue manifest of another schema version is UnsupportedSchemaVersionErr`` () =
        let atVersion2 (manifest : JsonObject) : unit = manifest["schemaVersion"] <- JsonValue.Create 2
        let expected = UnsupportedSchemaVersionErr { fileExtension = CatalogueManifest.extension; found = FileSchemaVersion 2; expected = FileSchemaVersion 1 }

        let intact = editedManifestFile atVersion2
        let junkBody = editedManifestFile (fun manifest -> atVersion2 manifest; manifest["entries"] <- JsonObject())

        for text in [ intact; junkBody ] do
            Assert.Equal<Result<CatalogueManifest, StorageError>>(Error expected, text.parseCatalogueManifest)

    [<Fact>]
    let ``a schema-invalid catalogue manifest is SchemaValidationErr`` () =
        let firstRow (manifest : JsonObject) : JsonObject = manifest.["entries"].[0].AsObject()

        let invalid : (string * (JsonObject -> unit)) list =
            [
                "a row without its contentHash", (fun manifest -> (firstRow manifest).Remove "contentHash" |> ignore)
                "a row of kind 0", (fun manifest -> (firstRow manifest)["kind"] <- JsonValue.Create 0)
                "a row of kind 7", (fun manifest -> (firstRow manifest)["kind"] <- JsonValue.Create 7)
                "a row with an empty fileName", (fun manifest -> (firstRow manifest)["fileName"] <- JsonValue.Create "")
                "a row with another key", (fun manifest -> (firstRow manifest)["extra"] <- JsonValue.Create 1)
                "an id that is not a GUID", (fun manifest -> manifest["id"] <- JsonValue.Create "not a GUID")
                "an empty name", (fun manifest -> manifest["name"] <- JsonValue.Create "")
                "version 0", (fun manifest -> manifest["version"] <- JsonValue.Create 0)
                "entries that are not an array", (fun manifest -> manifest["entries"] <- JsonObject())
                "another top-level key", (fun manifest -> manifest["extra"] <- JsonValue.Create 1)
                "schemaVersion 0", (fun manifest -> manifest["schemaVersion"] <- JsonValue.Create 0)
                "no schemaVersion", (fun manifest -> manifest.Remove "schemaVersion" |> ignore)
            ]

        for place, edit in invalid do
            assertSchemaInvalid place (editedManifestFile edit).parseCatalogueManifest

        let assertMalformed (place : string) (text : LibraryFileText) : unit =
            match text.parseCatalogueManifest with
            | Error (MalformedJsonErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason, $"{place}: the error carries the reader's message")
            | other -> Assert.Fail($"{place}: expected MalformedJsonErr, got %A{other}")

        assertMalformed "a text that is not JSON" (LibraryFileText "{ this is not JSON")
        assertMalformed "a version the schema allows but an int cannot hold" (editedManifestFile (fun manifest -> manifest["version"] <- JsonValue.Create 3000000000L))

    let private categoryContent (category : MaterialCategory) : LibraryEntryContent =
        LibraryEntryContent.ofMaterialCategory VersionNumber.first builtInCatalogue category |> decodedOf

    let private entryContent (entry : LibraryEntry) : LibraryEntryContent =
        match entry with
        | SampleItem sample -> LibraryEntryContent.ofSample VersionNumber.first builtInCatalogue sample
        | SourceItem source -> LibraryEntryContent.ofSourcePreset VersionNumber.first builtInCatalogue source |> decodedOf
        | DetectorItem detector -> LibraryEntryContent.ofDetectorPreset VersionNumber.first builtInCatalogue detector |> decodedOf
        | PolarizerItem polarizer -> LibraryEntryContent.ofPolarizerPreset VersionNumber.first builtInCatalogue polarizer |> decodedOf

    [<Fact>]
    let ``every built-in seals, writes, reads and decodes equal to its seed`` () =
        let materials = BuiltinCatalogue.materials ()
        let categories = BuiltinCatalogue.categories ()
        let entries = (BuiltinCatalogue.samples () |> List.map SampleItem) @ BuiltinCatalogue.presets ()
        Assert.NotEmpty materials
        Assert.NotEmpty categories
        Assert.NotEmpty entries

        for material in materials do
            assertMaterialRoundTrips builtInCatalogue material

        for category in categories do
            let readBack = categoryContent category |> roundTrip
            Assert.Equal<MaterialCategory>(category, readBack.content.toMaterialCategory |> decodedOf)

        for entry in entries do
            let readBack = entryContent entry |> roundTrip

            match entry with
            | SampleItem sample -> Assert.Equal<Sample>(sample, readBack.content.toSample |> decodedOf)
            | SourceItem source -> Assert.Equal<SourcePreset>(source, readBack.content.toSourcePreset |> decodedOf)
            | DetectorItem detector -> Assert.Equal<DetectorPreset>(detector, readBack.content.toDetectorPreset |> decodedOf)
            | PolarizerItem polarizer -> Assert.Equal<PolarizerPreset>(polarizer, readBack.content.toPolarizerPreset |> decodedOf)

    [<Fact>]
    let ``the built-ins form one catalogue with unique file names per kind`` () =
        let materials = BuiltinCatalogue.materials ()
        let categories = BuiltinCatalogue.categories ()
        let entries = (BuiltinCatalogue.samples () |> List.map SampleItem) @ BuiltinCatalogue.presets ()
        let contents =
            (materials |> List.map (LibraryEntryContent.ofMaterialEntry VersionNumber.first builtInCatalogue))
            @ (categories |> List.map categoryContent)
            @ (entries |> List.map entryContent)

        let envelopes = contents |> List.map (fun content -> content.``sealed`` |> okOf)
        let manifest = CatalogueManifest.tryCreate testCatalogueId (CatalogueName "Built-in") VersionNumber.first envelopes |> decodedOf

        Assert.Equal(List.length materials + List.length categories + List.length entries, List.length manifest.entries)

        let kinds = manifest.entries |> List.map (fun row -> row.kind) |> List.distinct
        Assert.Equal<LibraryEntryKind list>(LibraryEntryKind.all, kinds)

        let readBack = manifest.fileText |> Result.bind (fun text -> text.parseCatalogueManifest) |> okOf
        Assert.Equal<CatalogueManifest>(manifest, readBack)
