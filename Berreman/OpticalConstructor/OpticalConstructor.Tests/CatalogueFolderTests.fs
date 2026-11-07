namespace OpticalConstructor.Tests

open System
open System.IO
open System.Text.Json.Nodes
open Xunit
open Berreman.Constants
open Berreman.Fields
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.EntryFileJson
open OpticalConstructor.Storage.CatalogueFolderStore
open OpticalConstructor.Storage.CatalogueImportStore

/// The folder implementation of `CatalogueProxy`, over real folders under the test output (spec 0050 001 §A; .spec-md §4,
/// §5; S1.7). Delayed BuiltinCatalogue.source/material/category/sample/presets lookups fail their fact, not assembly start-up.
module CatalogueFolderTests =

    let private okOf (result : Result<'Value, 'Problem>) : 'Value =
        match result with
        | Ok value -> value
        | Error problem -> failwith $"expected Ok, got %A{problem}"

    let private builtIn : EntryProvenance = BuiltInProvenance CatalogueIds.builtInCatalogue

    let private tunableSource () : SourcePreset =
        BuiltinCatalogue.source PresetIds.xenonMonochromator

    /// The tunable source with new content under the same name, so the same file name and a new hash.
    let private retunedSource () : SourcePreset =
        let source : SourcePreset = tunableSource ()
        { source with waveLength = WaveLength.nm 610.0<nm> }

    /// One entry of every kind but the polarizer, holding `source`.
    let private catalogueOf (source : SourcePreset) : CatalogueData =
        let langasite = BuiltinCatalogue.material MaterialIds.langasite
        let glass = BuiltinCatalogue.category CategoryIds.glass

        let ellipsometer =
            BuiltinCatalogue.presets ()
            |> List.pick (fun entry ->
                match entry with
                | DetectorItem detector when detector.id = PresetIds.detectorEllipsometer -> Some detector
                | _ -> None)

        [
            LibraryEntryContent.ofMaterialEntry VersionNumber.first builtIn langasite
            LibraryEntryContent.ofSample VersionNumber.first builtIn (BuiltinCatalogue.sample SampleIds.multilayerQwId)
            LibraryEntryContent.ofSourcePreset VersionNumber.first builtIn source |> okOf
            LibraryEntryContent.ofDetectorPreset VersionNumber.first builtIn ellipsometer |> okOf
            LibraryEntryContent.ofMaterialCategory VersionNumber.first builtIn glass |> okOf
        ]
        |> List.map (fun (content : LibraryEntryContent) -> content.``sealed`` |> okOf)
        |> CatalogueData.tryCreate CatalogueIds.builtInCatalogue (CatalogueName "Built-in") VersionNumber.first
        |> okOf

    let private sourceEnvelopeIn (catalogue : CatalogueData) : LibraryEntryEnvelope =
        catalogue.envelopes |> List.find (fun (envelope : LibraryEntryEnvelope) -> envelope.content.schema.kind = SourceEntryKind)

    /// A folder that does not exist yet, under the test output, whatever an earlier run left there.
    let private freshFolder (name : string) : FolderName =
        let path = Path.Combine(AppContext.BaseDirectory, "CatalogueFolderTests", name)

        if Directory.Exists path then
            Directory.Delete(path, true)

        FolderName path

    let private writtenFolder (name : string) (catalogue : CatalogueData) : FolderName * CatalogueProxy =
        let folder = freshFolder name
        let proxy = CatalogueProxy.create folder
        proxy.writeCatalogue catalogue |> okOf
        folder, proxy

    let private entryPath (folder : FolderName) (envelope : LibraryEntryEnvelope) : string =
        (envelope.content.fileName.combine folder).value

    let private manifestPath (folder : FolderName) : string =
        (CatalogueManifest.fileName.combine folder).value

    /// Every top-level file of `folder` as its name and its text, sorted by name.
    let private textsIn (folder : FolderName) : (string * string) list =
        Directory.GetFiles(folder.value)
        |> Array.toList
        |> List.map (fun (path : string) -> Path.GetFileName(path), File.ReadAllText(path))
        |> List.sort

    let private assertUnlisted (file : string) (answer : Result<'Value, CatalogueError>) : unit =
        match answer with
        | Error (UnlistedEntryFileErr reason) -> Assert.Contains(file, reason)
        | other -> Assert.Fail($"expected UnlistedEntryFileErr naming '{file}', got %A{other}")

    [<Fact>]
    let ``a catalogue export target is named by its folder`` () : unit =
        let name = CatalogueName "Lab catalogue"
        let folders = [ FolderName "D:/Exports/Lab catalogue"; FolderName "D:/Exports/Lab catalogue/" ]
        for folder in folders do
            let target = CatalogueExportTarget.ofFolder folder
            Assert.Equal(folder, target.folder)
            Assert.Equal(name, target.name)

    [<Fact>]
    let ``the Built-in catalogue proxy reads the catalogue the build copies beside the assembly`` () : unit =
        let catalogue = (CatalogueProxy.createBuiltIn ()).readCatalogue () |> okOf
        let expectedName = CatalogueName "Built-in"
        let folder = FolderName (Path.Combine(System.AppContext.BaseDirectory, "Catalogue", "Builtin"))
        let expected = (CatalogueProxy.create folder).readCatalogue () |> okOf

        Assert.Equal<CatalogueId>(CatalogueIds.builtInCatalogue, catalogue.manifest.id)
        Assert.Equal<CatalogueName>(expectedName, catalogue.manifest.name)
        Assert.Equal<CatalogueData>(expected, catalogue)

    [<Fact>]
    let ``createBuiltInUnder reads the Builtin catalogue of the shipped-catalogue folder it is given`` () : unit =
        let shipped = FolderName (Path.Combine(System.AppContext.BaseDirectory, "Catalogue"))
        let builtInFolder = FolderName (Path.Combine(System.AppContext.BaseDirectory, "Catalogue", "Builtin"))
        let expected = (CatalogueProxy.create builtInFolder).readCatalogue () |> okOf
        let actual = (CatalogueProxy.createBuiltInUnder shipped).readCatalogue () |> okOf

        Assert.Equal<FolderName>(builtInFolder, CatalogueProxy.builtInFolderUnder shipped)
        Assert.Equal<CatalogueData>(expected, actual)

        let emptyFolder = freshFolder "empty-shipped-catalogue"
        Directory.CreateDirectory emptyFolder.value |> ignore

        match (CatalogueProxy.createBuiltInUnder emptyFolder).readCatalogue () with
        | Error (CatalogueIoErr reason) -> Assert.Contains(emptyFolder.value, reason)
        | other -> Assert.Fail($"expected CatalogueIoErr for an empty shipped-catalogue folder, got %A{other}")

    [<Fact>]
    let ``a catalogue folder written and read back is equal`` () =
        let first = catalogueOf (tunableSource ())
        let folder, proxy = writtenFolder "written-and-read" first

        let expectedFiles =
            CatalogueManifest.fileName.value :: (first.envelopes |> List.map (fun (envelope : LibraryEntryEnvelope) -> envelope.content.fileName.value))
            |> List.sort

        Assert.Equal<string list>(expectedFiles, textsIn folder |> List.map fst)
        Assert.Equal<CatalogueData>(first, proxy.readCatalogue () |> okOf)

        for path in Directory.GetFiles(folder.value) do
            Assert.Equal<byte>(byte '{', File.ReadAllBytes path |> Array.head)

        let second = catalogueOf (retunedSource ())
        Assert.Equal<FileName>((sourceEnvelopeIn first).content.fileName, (sourceEnvelopeIn second).content.fileName)
        Assert.NotEqual<ContentHash>((sourceEnvelopeIn first).contentHash, (sourceEnvelopeIn second).contentHash)

        proxy.writeCatalogue second |> okOf

        Assert.Equal<string list>(expectedFiles, textsIn folder |> List.map fst)
        Assert.Equal<CatalogueData>(second, proxy.readCatalogue () |> okOf)

    [<Fact>]
    let ``readEntryFile reads one entry file through its schema, version and hash checks`` () : unit =
        let catalogue = catalogueOf (tunableSource ())
        let folder, _ = writtenFolder "entry-file-alone" catalogue
        File.Delete(manifestPath folder)

        for row in catalogue.manifest.entries do
            let file = row.fileName.combine folder
            let expected = catalogue.envelopes |> List.find (fun envelope -> envelope.manifestRow = row)
            Assert.Equal<LibraryEntryEnvelope>(expected, LibraryFileText.readEntryFile row.kind file |> okOf)

        let missingName = FileName "Missing.source-json"
        let missingFile = missingName.combine folder

        match LibraryFileText.readEntryFile SourceEntryKind missingFile with
        | Error (FileIoExn error) -> Assert.IsType<FileNotFoundException>(error) |> ignore
        | Error error -> Assert.Fail($"expected FileIoExn, got {error.describe}")
        | Ok _ -> Assert.Fail "expected FileIoExn for a missing entry file"

        let sample = catalogue.envelopes |> List.find (fun envelope -> envelope.content.schema.kind = SampleEntryKind)
        let sampleFile = sample.content.fileName.combine folder

        match LibraryFileText.readEntryFile MaterialEntryKind sampleFile with
        | Error (SchemaValidationErr messages) -> Assert.NotEmpty messages
        | Error error -> Assert.Fail($"expected SchemaValidationErr, got {error.describe}")
        | Ok _ -> Assert.Fail "expected SchemaValidationErr for a sample read as a material"

        let source = sourceEnvelopeIn catalogue
        let sourceFile = source.content.fileName.combine folder

        do
            use lockedFile = new FileStream(sourceFile.value, FileMode.Open, FileAccess.Read, FileShare.None)

            match LibraryFileText.readEntryFile SourceEntryKind sourceFile with
            | Error (FileIoExn error) -> Assert.IsAssignableFrom<IOException>(error) |> ignore
            | Error error -> Assert.Fail($"expected FileIoExn, got {error.describe}")
            | Ok _ -> Assert.Fail "expected FileIoExn for an entry file locked against reading"

        let retuned = sourceEnvelopeIn (catalogueOf (retunedSource ()))
        let tampered =
            {
                retuned with contentHash = source.contentHash
            }

        (tampered.fileText |> okOf).writeAtomically sourceFile |> okOf

        match LibraryFileText.readEntryFile SourceEntryKind sourceFile with
        | Error (ContentHashMismatchErr info) ->
            Assert.Equal<LibraryEntryId>(source.content.id, info.entryId)
            Assert.Equal<ContentHash>(source.contentHash, info.recorded)
            Assert.Equal<ContentHash>(retuned.contentHash, info.computed)
            Assert.NotEqual<ContentHash>(info.recorded, info.computed)
        | Error error -> Assert.Fail($"expected ContentHashMismatchErr, got {error.describe}")
        | Ok _ -> Assert.Fail "expected ContentHashMismatchErr for a changed payload with its original hash"

        let unsupportedSchema =
            {
                source.content.schema with version = FileSchemaVersion 2
            }

        let unsupported =
            {
                source with
                    content =
                        {
                            source.content with schema = unsupportedSchema
                        }
            }

        (unsupported.fileText |> okOf).writeAtomically sourceFile |> okOf

        match LibraryFileText.readEntryFile SourceEntryKind sourceFile with
        | Error (UnsupportedSchemaVersionErr info) ->
            Assert.Equal<FileExtension>(SourceEntryKind.extension, info.fileExtension)
            Assert.Equal<FileSchemaVersion>(unsupportedSchema.version, info.found)
            Assert.Equal<FileSchemaVersion>(SourceEntryKind.currentSchemaVersion, info.expected)
        | Error error -> Assert.Fail($"expected UnsupportedSchemaVersionErr, got {error.describe}")
        | Ok _ -> Assert.Fail "expected UnsupportedSchemaVersionErr for a different schema version"

    [<Fact>]
    let ``readManifest reads the manifest without opening an entry file`` () : unit =
        let catalogue = catalogueOf (tunableSource ())
        let folder, proxy = writtenFolder "manifest-alone" catalogue
        let readBack = proxy.readCatalogue () |> okOf

        Assert.Equal<CatalogueManifest>(readBack.manifest, proxy.readManifest () |> okOf)

        let source = sourceEnvelopeIn catalogue
        File.Delete(entryPath folder source)

        match proxy.readCatalogue () with
        | Error (MissingEntryFileErr reason) -> Assert.Contains(source.content.fileName.value, reason)
        | other -> Assert.Fail($"expected MissingEntryFileErr, got %A{other}")

        Assert.Equal<CatalogueManifest>(readBack.manifest, proxy.readManifest () |> okOf)

        File.WriteAllText(manifestPath folder, "{ this is not JSON")

        match proxy.readManifest () with
        | Error (CatalogueFileRejectedErr reason) -> Assert.Contains(CatalogueManifest.fileName.value, reason)
        | other -> Assert.Fail($"expected CatalogueFileRejectedErr for an invalid manifest, got %A{other}")

        File.Delete(manifestPath folder)

        match proxy.readManifest () with
        | Error (CatalogueIoErr reason) -> Assert.Contains(folder.value, reason)
        | other -> Assert.Fail($"expected CatalogueIoErr for a missing manifest, got %A{other}")

    [<Fact>]
    let ``a manifest row with no file is MissingEntryFileErr`` () =
        let catalogue = catalogueOf (tunableSource ())
        let folder, proxy = writtenFolder "row-with-no-file" catalogue
        let source = sourceEnvelopeIn catalogue

        File.Delete(entryPath folder source)

        match proxy.readCatalogue () with
        | Error (MissingEntryFileErr reason) -> Assert.Contains(source.content.fileName.value, reason)
        | other -> Assert.Fail($"expected MissingEntryFileErr, got %A{other}")

    [<Fact>]
    let ``an entry file the manifest does not name is UnlistedEntryFileErr, and other files are ignored`` () =
        let catalogue = catalogueOf (tunableSource ())
        let folder, proxy = writtenFolder "unlisted-entry-file" catalogue
        let inFolder (name : string) : string = Path.Combine(folder.value, name)

        File.WriteAllText(inFolder "README.md", "Not an entry.")
        File.WriteAllText(inFolder "stale.tmp", "A write that never finished.")
        Assert.Equal<CatalogueData>(catalogue, proxy.readCatalogue () |> okOf)

        let copyName = $"A copy{SourceEntryKind.extension.value}"
        File.Copy(entryPath folder (sourceEnvelopeIn catalogue), inFolder copyName)
        assertUnlisted copyName (proxy.readCatalogue ())

        // A write that went ahead would change the source's file, so unchanged text proves the refusal came first.
        let before = textsIn folder
        assertUnlisted copyName (proxy.writeCatalogue (catalogueOf (retunedSource ())))
        Assert.Equal<(string * string) list>(before, textsIn folder)

    [<Fact>]
    let ``a tampered entry file is CatalogueFileRejectedErr`` () =
        let catalogue = catalogueOf (tunableSource ())
        let folder, proxy = writtenFolder "tampered-entry-file" catalogue
        let source = sourceEnvelopeIn catalogue
        let path = entryPath folder source

        let root = JsonNode.Parse(File.ReadAllText path)
        root.["payload"].["Fields"].[0].["waveLength"].["Fields"].[0] <- JsonValue.Create 601.0
        File.WriteAllText(path, root.ToJsonString())

        match proxy.readCatalogue () with
        | Error (CatalogueFileRejectedErr reason) ->
            Assert.Contains(source.content.fileName.value, reason)
            Assert.Contains(source.contentHash.value, reason)
        | other -> Assert.Fail($"expected CatalogueFileRejectedErr, got %A{other}")

        // The manifest's own refusal takes the same case and names the manifest (spec 0050 .spec-md §4).
        File.WriteAllText(manifestPath folder, "{ this is not JSON")

        match proxy.readCatalogue () with
        | Error (CatalogueFileRejectedErr reason) -> Assert.Contains(CatalogueManifest.fileName.value, reason)
        | other -> Assert.Fail($"a manifest that is not JSON: expected CatalogueFileRejectedErr, got %A{other}")

    [<Fact>]
    let ``a manifest that disagrees with its entry files is ManifestMismatchErr`` () =
        let catalogue = catalogueOf (tunableSource ())
        let folder, proxy = writtenFolder "manifest-disagrees" catalogue

        let validButOther : LibraryFileText = (sourceEnvelopeIn (catalogueOf (retunedSource ()))).fileText |> okOf
        File.WriteAllText(entryPath folder (sourceEnvelopeIn catalogue), validButOther.value)

        match proxy.readCatalogue () with
        | Error (ManifestMismatchErr reason) -> Assert.Contains("'Built-in'", reason)
        | other -> Assert.Fail($"expected ManifestMismatchErr, got %A{other}")

    [<Fact>]
    let ``a missing folder or manifest is CatalogueIoErr`` () =
        let assertIoErr (place : string) (proxy : CatalogueProxy) : unit =
            match proxy.readCatalogue () with
            | Error (CatalogueIoErr _) -> ()
            | other -> Assert.Fail($"{place}: expected CatalogueIoErr, got %A{other}")

        assertIoErr "a folder that does not exist" (CatalogueProxy.create (freshFolder "no-such-folder"))

        let folder, proxy = writtenFolder "no-manifest" (catalogueOf (tunableSource ()))
        File.Delete(manifestPath folder)
        assertIoErr "a written folder without its manifest" proxy

        // A write that fails on an entry leaves no `.tmp` and no manifest, because the manifest goes last (spec 0050
        // .spec-md §4). A folder under the source file's name makes the move over it fail.
        let catalogue = catalogueOf (tunableSource ())
        let source = sourceEnvelopeIn catalogue
        let blocked = freshFolder "write-blocked"
        let blockedProxy = CatalogueProxy.create blocked
        Directory.CreateDirectory(entryPath blocked source) |> ignore

        match blockedProxy.writeCatalogue catalogue with
        | Error (CatalogueIoErr reason) -> Assert.Contains(source.content.fileName.value, reason)
        | other -> Assert.Fail($"a write that cannot finish: expected CatalogueIoErr, got %A{other}")

        let leftBehind = textsIn blocked |> List.map fst
        Assert.Equal<string list>([], leftBehind |> List.filter (fun (name : string) -> name.EndsWith ".tmp"))
        assertIoErr "a folder whose write failed before its manifest" blockedProxy
