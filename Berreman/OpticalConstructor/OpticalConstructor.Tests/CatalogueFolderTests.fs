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
open OpticalConstructor.Storage.EntryFileJson
open OpticalConstructor.Storage.CatalogueFolderStore

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
    let ``the Built-in catalogue proxy reads the catalogue the build copies beside the assembly`` () : unit =
        let catalogue = (CatalogueProxy.createBuiltIn ()).readCatalogue () |> okOf
        let expectedName = CatalogueName "Built-in"
        let folder = FolderName (Path.Combine(System.AppContext.BaseDirectory, "Catalogue", "Builtin"))
        let expected = (CatalogueProxy.create folder).readCatalogue () |> okOf

        Assert.Equal<CatalogueId>(CatalogueIds.builtInCatalogue, catalogue.manifest.id)
        Assert.Equal<CatalogueName>(expectedName, catalogue.manifest.name)
        Assert.Equal<CatalogueData>(expected, catalogue)

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
