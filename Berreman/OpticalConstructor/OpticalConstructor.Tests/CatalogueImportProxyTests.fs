namespace OpticalConstructor.Tests

open Xunit
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Storage.CatalogueFolderStore

module internal CatalogueImportTestData =

    let decodeImportEntry (result : Result<'Entry, LibraryEntryError>) : 'Entry =
        match result with
        | Ok entry -> entry
        | Error error -> failwith error.describe

    let readImportCatalogue () : CatalogueData =
        match (CatalogueProxy.createBuiltIn ()).readCatalogue () with
        | Ok catalogue -> catalogue
        | Error error -> failwith error.describe

/// Pins import order, values and failure short-circuiting (spec 0038 step 041; spec 0050 §3.4).
module CatalogueImportProxyTests =

    open CatalogueImportTestData

    type private Channel =
        | CategoryChannel
        | MaterialChannel
        | SampleChannel
        | EntryChannel

    type private Recorder =
        {
            order : ResizeArray<Channel>
            categories : ResizeArray<MaterialCategory>
            materials : ResizeArray<MaterialEntry>
            samples : ResizeArray<Sample>
            entries : ResizeArray<LibraryEntry>
        }

        static member create () : Recorder =
            {
                order = ResizeArray<Channel>()
                categories = ResizeArray<MaterialCategory>()
                materials = ResizeArray<MaterialEntry>()
                samples = ResizeArray<Sample>()
                entries = ResizeArray<LibraryEntry>()
            }

    let private recordingProxy (r : Recorder) : CatalogueImportProxy =
        {
            saveCategory = fun c -> r.order.Add CategoryChannel; r.categories.Add c; Ok ()
            saveMaterial = fun m -> r.order.Add MaterialChannel; r.materials.Add m; Ok ()
            saveSample = fun s -> r.order.Add SampleChannel; r.samples.Add s; Ok ()
            saveLibraryEntry = fun e -> r.order.Add EntryChannel; r.entries.Add e; Ok ()
        }

    let private contentsFor (kind : LibraryEntryKind) (catalogue : CatalogueData) : LibraryEntryContent list =
        catalogue.envelopes
        |> List.map (fun envelope -> envelope.content)
        |> List.filter (fun content -> content.schema.kind = kind)

    let private assertPushedInOrder (expected : 'Entry list) (actual : ResizeArray<'Entry>) : unit =
        Assert.True((expected = List.ofSeq actual), "The channel must receive exactly the decoded values in row order.")

    [<Fact>]
    let ``importCatalogue pushes every entry once, categories then materials then samples then presets, through the exact signatures`` () : unit =
        let catalogue = readImportCatalogue ()
        let r = Recorder.create ()
        let import : CatalogueData -> Result<unit, CatalogueImportError> = (recordingProxy r).importCatalogue

        Assert.Equal<Result<unit, CatalogueImportError>>(Ok (), import catalogue)

        let categories = contentsFor CategoryEntryKind catalogue |> List.map (fun content -> content.toMaterialCategory |> decodeImportEntry)
        let materials = contentsFor MaterialEntryKind catalogue |> List.map (fun content -> content.toMaterialEntry |> decodeImportEntry)
        let samples = contentsFor SampleEntryKind catalogue |> List.map (fun content -> content.toSample |> decodeImportEntry)
        let presets =
            [
                yield! contentsFor SourceEntryKind catalogue |> List.map (fun content -> content.toSourcePreset |> decodeImportEntry |> SourceItem)
                yield! contentsFor PolarizerEntryKind catalogue |> List.map (fun content -> content.toPolarizerPreset |> decodeImportEntry |> PolarizerItem)
                yield! contentsFor DetectorEntryKind catalogue |> List.map (fun content -> content.toDetectorPreset |> decodeImportEntry |> DetectorItem)
            ]

        assertPushedInOrder categories r.categories
        assertPushedInOrder materials r.materials
        assertPushedInOrder samples r.samples
        assertPushedInOrder presets r.entries

        let expectedOrder =
            [
                yield! List.replicate categories.Length CategoryChannel
                yield! List.replicate materials.Length MaterialChannel
                yield! List.replicate samples.Length SampleChannel
                yield! List.replicate presets.Length EntryChannel
            ]
        Assert.Equal<Channel list>(expectedOrder, List.ofSeq r.order)
        Assert.Equal(catalogue.envelopes.Length, r.order.Count)

    [<Fact>]
    let ``importCatalogue stops at the first rejected entry and answers its ImportRejectedErr`` () : unit =
        let catalogue = readImportCatalogue ()
        let r = Recorder.create ()
        let rejection = ImportRejectedErr "the stub rejects every material"
        let proxy : CatalogueImportProxy =
            {
                recordingProxy r with
                    saveMaterial = fun _ -> r.order.Add MaterialChannel; Error rejection
            }

        Assert.Equal<Result<unit, CatalogueImportError>>(Error rejection, proxy.importCatalogue catalogue)

        let categories = contentsFor CategoryEntryKind catalogue |> List.map (fun content -> content.toMaterialCategory |> decodeImportEntry)
        assertPushedInOrder categories r.categories
        Assert.Empty r.materials
        Assert.Empty r.samples
        Assert.Empty r.entries
        Assert.Equal<Channel list>(List.replicate categories.Length CategoryChannel @ [ MaterialChannel ], List.ofSeq r.order)

    [<Fact>]
    let ``an entry that does not decode stops the import with UndecodableEntryErr`` () : unit =
        let catalogue = readImportCatalogue ()
        let firstEnvelope (kind : LibraryEntryKind) : LibraryEntryEnvelope =
            catalogue.envelopes |> List.find (fun envelope -> envelope.content.schema.kind = kind)

        let category = firstEnvelope CategoryEntryKind
        let material = firstEnvelope MaterialEntryKind
        let malformed =
            {
                material with
                    content =
                        {
                            material.content with
                                categoryOpt = None
                        }
            }
        let sample = firstEnvelope SampleEntryKind
        let partialCatalogue =
            match CatalogueData.tryCreate catalogue.manifest.id catalogue.manifest.name catalogue.manifest.version [ category; malformed; sample ] with
            | Ok data -> data
            | Error error -> failwith error.describe
        let r = Recorder.create ()

        match (recordingProxy r).importCatalogue partialCatalogue with
        | Error (UndecodableEntryErr (MissingEntryFieldErr reason)) ->
            Assert.Contains(material.content.name.value, reason)
        | Error error -> Assert.Fail($"Expected a missing entry field, got {error.describe}")
        | Ok () -> Assert.Fail("An undecodable material must stop the import.")

        assertPushedInOrder [ category.content.toMaterialCategory |> decodeImportEntry ] r.categories
        Assert.Equal<Channel list>([ CategoryChannel ], List.ofSeq r.order)
        Assert.Empty r.materials
        Assert.Empty r.samples
        Assert.Empty r.entries

    [<Fact>]
    let ``every import error renders its condition and its payload`` () : unit =
        let rejected = ImportRejectedErr "store refused Silicon"
        let entryError = MissingEntryFieldErr "Silicon has no category"
        let undecodable = UndecodableEntryErr entryError
        let catalogueError = CatalogueIoErr "the catalogue folder is unavailable"
        let unreadable = CatalogueUnreadableErr catalogueError

        Assert.Equal("a catalogue entry was rejected: store refused Silicon", rejected.describe)
        Assert.Equal($"a catalogue entry could not be decoded: {entryError.describe}", undecodable.describe)
        Assert.Equal($"the catalogue could not be read: {catalogueError.describe}", unreadable.describe)

    [<Fact>]
    let ``a CatalogueImportProxy compares by reference (the [<ReferenceEquality>] contract)`` () : unit =
        let make () : CatalogueImportProxy = recordingProxy (Recorder.create ())
        let p = make ()
        let same = p
        Assert.True((p = same))
        Assert.False((p = make ()))
