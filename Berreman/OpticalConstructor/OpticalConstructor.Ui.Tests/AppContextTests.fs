namespace OpticalConstructor.Ui.Tests

open System.IO
open System.Collections.Generic
open System.Reflection
open System.Threading.Tasks
open Microsoft.Data.Sqlite
open Softellect.Sys.Primitives
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Platform
open Avalonia.Headless
open Avalonia.Platform.Storage
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.ExperimentExport
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Controls
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.CatalogueFolderStore
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.EntryFileJson
open OpticalConstructor.Storage.LibraryDatabaseStore
open OpticalConstructor.Tests
open OpticalConstructor.Ui
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Ui.TableAndElementRotationView

// Spec 0050 S3.3: isolated picker sessions must not race the assembly's Avalonia session.
[<CollectionDefinition("Library picker isolation", DisableParallelization = true)>]
type LibraryPickerTestCollection() = class end

// Spec 0050 S3.3: Avalonia's NotClientImplementable storage contracts need runtime test doubles.
type LibraryPickerStorageTestProxy() =
    inherit DispatchProxy()

    let mutable invokeOpt : (MethodInfo -> obj array -> obj) option = None

    member private _.initialize (invoke : MethodInfo -> obj array -> obj) : unit =
        invokeOpt <- Some invoke

    override _.Invoke(methodInfo : MethodInfo, arguments : obj array) : obj =
        match invokeOpt with
        | Some invoke -> invoke methodInfo arguments
        | None -> invalidOp "The library picker storage proxy was not configured."

    static member create<'StorageContract> (invoke : MethodInfo -> obj array -> obj) : 'StorageContract =
        let storage = DispatchProxy.Create<'StorageContract, LibraryPickerStorageTestProxy>()
        let proxy = box storage :?> LibraryPickerStorageTestProxy
        proxy.initialize invoke
        storage

/// Pins shared database stores, startup errors and catalogue import across app scopes
/// and real windows (spec 0038 Part C; spec 0050 S2.4).
[<Collection("Library picker isolation")>]
module AppContextTests =

    let private owner = FolderName "AppContextTests"

    let private unexpectedPickerStorageCall (methodInfo : MethodInfo) : 'Item =
        invalidOp $"The library folder picker requested an unrelated storage operation: {methodInfo.Name}."

    type private LibraryPickerTestFolder =
        static member ofUri (location : System.Uri) : IStorageFolder =
            LibraryPickerStorageTestProxy.create<IStorageFolder> (fun methodInfo _ ->
                match methodInfo.Name with
                | "get_Name" -> box (Path.GetFileName location.AbsolutePath)
                | "get_Path" -> box location
                | "get_CanBookmark" -> box false
                | "Dispose" -> null
                | _ -> unexpectedPickerStorageCall methodInfo)

        static member create (folder : FolderName) : IStorageFolder =
            LibraryPickerTestFolder.ofUri (System.Uri(folder.value))

    type private LibraryPickerTestProvider
        (suggested : IStorageFolder,
         selected : IStorageFolder list,
         ?resolveFolder : unit -> Task<IStorageFolder>,
         ?openPicker : unit -> Task<IReadOnlyList<IStorageFolder>>) =
        let resolvedPaths = ResizeArray<System.Uri>()
        let pickerOptions = ResizeArray<FolderPickerOpenOptions>()
        let requestedCalls = ResizeArray<string>()
        let providerInstance =
            LibraryPickerStorageTestProxy.create<IStorageProvider> (fun methodInfo arguments ->
                requestedCalls.Add methodInfo.Name
                match methodInfo.Name with
                | "get_CanOpen" | "get_CanSave" -> box false
                | "get_CanPickFolder" -> box true
                | "TryGetFolderFromPathAsync" ->
                    let path = unbox<System.Uri> arguments.[0]
                    resolvedPaths.Add path
                    match resolveFolder with
                    | Some resolve -> box (resolve ())
                    | None -> box (Task.FromResult<IStorageFolder> suggested)
                | "OpenFolderPickerAsync" ->
                    let options = unbox<FolderPickerOpenOptions> arguments.[0]
                    pickerOptions.Add options
                    match openPicker with
                    | Some pick -> box (pick ())
                    | None -> box (Task.FromResult<IReadOnlyList<IStorageFolder>>(List.toArray selected))
                | _ -> unexpectedPickerStorageCall methodInfo)

        member _.storageProvider : IStorageProvider = providerInstance
        member _.paths : System.Uri list = List.ofSeq resolvedPaths
        member _.options : FolderPickerOpenOptions list = List.ofSeq pickerOptions
        member _.calls : string list = List.ofSeq requestedCalls

    type private LibraryPickerTestWindow(createProvider : TopLevel -> IStorageProvider) =
        inherit Window()

        member this.createStorageProvider () : IStorageProvider = createProvider this

    type private LibraryPickerTestAppBuilder() =
        static member BuildAvaloniaApp () : AppBuilder =
            let factory =
                {
                    new IStorageProviderFactory with
                        member _.CreateProvider topLevel =
                            match topLevel with
                            | :? LibraryPickerTestWindow as window -> window.createStorageProvider ()
                            | _ -> null
                }
            TestAppBuilder.BuildAvaloniaApp().With<IStorageProviderFactory>(factory)

    let private runLibraryPickerTest (action : unit -> unit) : unit =
        use session = HeadlessUnitTestSession.StartNew(typeof<LibraryPickerTestAppBuilder>)
        session.Dispatch(System.Action(action), System.Threading.CancellationToken.None).GetAwaiter().GetResult()

    // Manual 011: completed storage tasks keep the adapter on this isolated UI thread.
    let private pickKitFolderForTest (window : TopLevel) (suggested : FolderName) : KitFolder option =
        let picker = KitFolderPicker.ofWindow window suggested
        let task = picker.pickKitFolder () |> Async.StartImmediateAsTask
        task.GetAwaiter().GetResult()

    /// A control matches `id` by its `Name` OR its `AutomationProperties.AutomationId` (the
    /// workbench rows / verb buttons live in variable-membership lists, so they carry an
    /// AutomationId — the MaterialsControls precedent).
    let private matchesId (id : string) (c : Control) : bool =
        c.Name = id || Avalonia.Automation.AutomationProperties.GetAutomationId(c) = id

    let private tryFindControl (window : Window) (id : string) : Control option =
        window.GetVisualDescendants()
        |> Seq.tryPick (function :? Control as c when matchesId id c -> Some c | _ -> None)

    /// Present anywhere in the visual tree (visible or not) — the "is it listed" probe.
    let private isPresent (window : Window) (id : string) : bool =
        match tryFindControl window id with
        | Some _ -> true
        | None -> false

    /// Click the centre of the clickable Border carrying `id` (by Name or AutomationId).
    let private clickOn (window : Window) (id : string) : unit =
        let found =
            window.GetVisualDescendants()
            |> Seq.tryPick (function :? Border as b when matchesId id b && b.IsEffectivelyVisible -> Some b | _ -> None)
        match found with
        | None -> Assert.Fail($"%s{id} was not found (or not visible)")
        | Some b ->
            // Spec 0046: scroll the box into view first (the facet rows now WRAP, so the tree can sit below
            // the visible area of its scroller, where a click at the box's centre lands nowhere).
            b.BringIntoView() |> ignore
            Dispatcher.UIThread.RunJobs()
            let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
            if c.HasValue then
                window.MouseDown(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                // A Save click closes the editor during the press — skip the release then.
                if window.IsVisible then
                    window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
            else Assert.Fail($"%s{id} has no on-screen position")

    /// Set the text of the TextBox carrying `id` (fires the property-change subscription the
    /// control's `onTextChanged` binds).
    let private setText (window : Window) (id : string) (text : string) : unit =
        match tryFindControl window id with
        | Some (:? TextBox as tb) ->
            tb.Text <- text
            Dispatcher.UIThread.RunJobs()
        | Some c -> Assert.Fail($"%s{id} is a %s{c.GetType().Name}, not a TextBox")
        | None -> Assert.Fail($"%s{id} was not found")

    /// Population fixtures explicitly confirm startup choices before opening their surfaces (spec 0050 S3.1).
    let private freshContext () : AppContext = TestAppScope.freshAppContext owner

    let private isolatedShippedLocations () : DataLocations =
        let locations = TestAppScope.fileLocations owner
        let shipped = FolderName (Path.Combine(Path.GetDirectoryName locations.backupsFolder.value, "ShippedCatalogue"))
        {
            locations with
                shippedCatalogueFolder = shipped
        }

    let private writeShippedCatalogue (locations : DataLocations) (catalogue : CatalogueData) : unit =
        let folder = CatalogueProxy.builtInFolderUnder locations.shippedCatalogueFolder
        (CatalogueProxy.create folder).writeCatalogue catalogue
        |> Result.defaultWith (fun error -> failwith error.describe)

    let private assertEmptyStartup (context : AppContext) : unit =
        Assert.Equal<Result<Sample list, SampleError>>(Ok [], context.samples.listSamples IncludeInactive)
        Assert.Equal<Result<MaterialEntry list, MaterialError>>(Ok [], context.materials.listMaterials IncludeInactive)
        Assert.Equal<Result<MaterialCategory list, CategoryError>>(Ok [], context.categories.listCategories ())
        Assert.Equal<Result<HeldEntry list, CatalogueImportError>>(
            Ok [], context.catalogueImports.proxy.heldEntriesOf CatalogueIds.builtInCatalogue)
        Assert.Equal<Result<CatalogueImportLogEntry list, CatalogueImportError>>(
            Ok [], context.catalogueImports.proxy.loggedImports ())

    let private assertPendingCatalogue (context : AppContext) (catalogue : CatalogueData) : PendingCatalogueImport =
        let pending = Assert.Single context.pendingImports
        Assert.Equal(catalogue.manifest.id, pending.entry.catalogueId)
        Assert.Equal(catalogue.envelopes.Length, pending.decisions.Length)
        Assert.Equal<LibraryEntryId list>(
            catalogue.envelopes |> List.map (fun envelope -> envelope.content.id) |> List.sort,
            pending.decisions |> List.map _.entryId |> List.sort)
        pending

    /// A workbench surface shares the app library and works in its own kit (spec 0050 S4.3).
    let private surfaceOf (context : AppContext) : TableAndElementRotationModel =
        SceneTestFixture.mainSceneWith context.library context.experiments context.materials context.samples context.categories

    // ============================ pure contract (gate `ui-tests`) ============================

    [<Fact>]
    let ``create carries the settings record it was given, verbatim`` () =
        let context =
            AppContext.create (TestAppScope.testLocations owner) WorkbenchSettings.defaults
            |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal(WorkbenchSettings.defaults, context.settings)

    [<Fact>]
    let ``the app's import context suggests the configured catalogues folder for exports`` () : unit =
        let locations = TestAppScope.testLocations owner
        let context = TestAppScope.freshAppContext owner
        let standalone = CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy
        Assert.Equal<FolderName option>(Some locations.cataloguesFolder, context.catalogueImports.exportFolderOpt)
        Assert.Equal<FolderName option>(None, standalone.exportFolderOpt)

    [<Fact>]
    let ``the library folder picker resolves its start folder and delivers only the first selected folder on the UI thread`` () : unit =
        runLibraryPickerTest (fun () ->
            let start = FolderName (Path.Combine(System.AppContext.BaseDirectory, "Picker suggestions"))
            let chosen = FolderName (Path.Combine(System.AppContext.BaseDirectory, "Chosen catalogue #1"))
            let ignored = FolderName (Path.Combine(System.AppContext.BaseDirectory, "Ignored catalogue"))
            let suggested = LibraryPickerTestFolder.create start
            let first = LibraryPickerTestFolder.create chosen
            let second = LibraryPickerTestFolder.create ignored
            let provider = LibraryPickerTestProvider(suggested, [ first; second ])
            let window = LibraryPickerTestWindow(fun _ -> provider.storageProvider)
            try
                Assert.Same(provider.storageProvider, window.StorageProvider)
                let title = ControlCaption "Export test catalogue"
                let picked = ResizeArray<FolderName>()
                let callbackThreads = ResizeArray<System.Threading.Thread>()
                let uiThread = System.Threading.Thread.CurrentThread
                MaterialsWindowView.pickLibraryFolder window title (Some start) (fun folder ->
                    picked.Add folder
                    callbackThreads.Add System.Threading.Thread.CurrentThread)
                Assert.Empty picked
                Dispatcher.UIThread.RunJobs()
                Assert.Equal(start.value, Path.TrimEndingDirectorySeparator((Assert.Single provider.paths).LocalPath))
                let options = Assert.Single provider.options
                Assert.Equal(title.value, options.Title)
                Assert.False options.AllowMultiple
                Assert.Same(suggested, options.SuggestedStartLocation)
                Assert.Equal(chosen, Assert.Single picked)
                Assert.Same(uiThread, Assert.Single callbackThreads)
            finally
                window.Close()

            let root = FolderName (Path.Combine(System.AppContext.BaseDirectory, "KitPicker", System.Guid.NewGuid().ToString("N")))
            let kitStart = FolderName (Path.Combine(root.value, "Suggested kit"))
            let kitChosen = FolderName (Path.Combine(root.value, "Chosen kit #1"))
            let kitIgnored = FolderName (Path.Combine(root.value, "Ignored kit"))
            let expected = KitFolder kitChosen
            let suggestedKit = LibraryPickerTestFolder.create kitStart
            for resolved in [ suggestedKit; null ] do
                let kitProvider =
                    LibraryPickerTestProvider(resolved,
                        [ LibraryPickerTestFolder.create kitChosen; LibraryPickerTestFolder.create kitIgnored ])
                let kitWindow = LibraryPickerTestWindow(fun _ -> kitProvider.storageProvider)
                try
                    Assert.False(Directory.Exists root.value)
                    let result = pickKitFolderForTest kitWindow kitStart
                    Assert.Equal<KitFolder option>(Some expected, result)
                    Assert.Equal(kitStart.value, Path.TrimEndingDirectorySeparator((Assert.Single kitProvider.paths).LocalPath))
                    let options = Assert.Single kitProvider.options
                    Assert.Equal("Choose a kit folder", options.Title)
                    Assert.False options.AllowMultiple
                    Assert.Same(resolved, options.SuggestedStartLocation)
                    Assert.Equal<string list>([ "TryGetFolderFromPathAsync"; "OpenFolderPickerAsync" ], kitProvider.calls)
                    Assert.False(Directory.Exists root.value)
                finally
                    kitWindow.Close())

    [<Fact>]
    let ``cancelling the library folder picker leaves the selection unchanged`` () : unit =
        runLibraryPickerTest (fun () ->
            let previous = FolderName (Path.Combine(System.AppContext.BaseDirectory, "Previous catalogue"))
            let provider = LibraryPickerTestProvider(null, [])
            let window = LibraryPickerTestWindow(fun _ -> provider.storageProvider)
            try
                Assert.Same(provider.storageProvider, window.StorageProvider)
                let title = ControlCaption "Register catalogue folder"
                let picked = ResizeArray<FolderName>()
                let mutable selection = Some previous
                MaterialsWindowView.pickLibraryFolder window title None (fun folder ->
                    picked.Add folder
                    selection <- Some folder)
                Dispatcher.UIThread.RunJobs()
                Assert.Empty provider.paths
                let options = Assert.Single provider.options
                Assert.Equal(title.value, options.Title)
                Assert.False options.AllowMultiple
                Assert.Null options.SuggestedStartLocation
                Assert.Empty picked
                Assert.Equal<FolderName option>(Some previous, selection)
            finally
                window.Close()

            let root = FolderName (Path.Combine(System.AppContext.BaseDirectory, "KitPicker", System.Guid.NewGuid().ToString("N")))
            let local = LibraryPickerTestFolder.create root
            let remote = LibraryPickerTestFolder.ofUri (System.Uri("https://example.invalid/kit"))
            for selected in [ []; [ remote ]; [ remote; local ] ] do
                let kitProvider = LibraryPickerTestProvider(null, selected)
                let kitWindow = LibraryPickerTestWindow(fun _ -> kitProvider.storageProvider)
                try
                    Assert.False(Directory.Exists root.value)
                    let result = pickKitFolderForTest kitWindow root
                    Assert.Equal<KitFolder option>(None, result)
                    Assert.Equal(root.value, Path.TrimEndingDirectorySeparator((Assert.Single kitProvider.paths).LocalPath))
                    let options = Assert.Single kitProvider.options
                    Assert.Equal("Choose a kit folder", options.Title)
                    Assert.False options.AllowMultiple
                    Assert.Null options.SuggestedStartLocation
                    Assert.Equal<string list>([ "TryGetFolderFromPathAsync"; "OpenFolderPickerAsync" ], kitProvider.calls)
                    Assert.False(Directory.Exists root.value)
                finally
                    kitWindow.Close())

    [<Fact>]
    let ``an unavailable storage provider leaves the library folder selection unchanged`` () : unit =
        runLibraryPickerTest (fun () ->
            let requestedOwners = ResizeArray<TopLevel>()
            let window =
                LibraryPickerTestWindow(fun topLevel ->
                    requestedOwners.Add topLevel
                    raise (System.PlatformNotSupportedException("No storage provider is available in this test host.")))
            try
                let previous = FolderName (Path.Combine(System.AppContext.BaseDirectory, "Unavailable catalogue"))
                let title = ControlCaption "Export unavailable catalogue"
                let picked = ResizeArray<FolderName>()
                let mutable selection = Some previous
                MaterialsWindowView.pickLibraryFolder window title (Some previous) (fun folder ->
                    picked.Add folder
                    selection <- Some folder)
                Dispatcher.UIThread.RunJobs()
                Assert.Same(window, Assert.Single requestedOwners)
                Assert.Empty picked
                Assert.Equal<FolderName option>(Some previous, selection)
            finally
                window.Close()

            let root = FolderName (Path.Combine(System.AppContext.BaseDirectory, "KitPicker", System.Guid.NewGuid().ToString("N")))
            let kitOwners = ResizeArray<TopLevel>()
            let unavailableWindow =
                LibraryPickerTestWindow(fun topLevel ->
                    kitOwners.Add topLevel
                    raise (System.PlatformNotSupportedException("No kit storage provider is available.")))
            try
                Assert.False(Directory.Exists root.value)
                let result = pickKitFolderForTest unavailableWindow root
                Assert.Equal<KitFolder option>(None, result)
                Assert.Same(unavailableWindow, Assert.Single kitOwners)
                Assert.False(Directory.Exists root.value)
            finally
                unavailableWindow.Close()

            let resolved = LibraryPickerTestFolder.create root
            let resolutionFailure =
                LibraryPickerTestProvider(resolved, [], resolveFolder = (fun () ->
                    Task.FromException<IStorageFolder>(IOException("The suggested folder could not be resolved."))))
            let dialogFailure =
                LibraryPickerTestProvider(resolved, [], openPicker = (fun () ->
                    Task.FromException<IReadOnlyList<IStorageFolder>>(IOException("The folder dialog failed."))))
            for provider in [ resolutionFailure; dialogFailure ] do
                let kitWindow = LibraryPickerTestWindow(fun _ -> provider.storageProvider)
                try
                    Assert.False(Directory.Exists root.value)
                    let result = pickKitFolderForTest kitWindow root
                    Assert.Equal<KitFolder option>(None, result)
                    Assert.Equal(root.value, Path.TrimEndingDirectorySeparator((Assert.Single provider.paths).LocalPath))
                    Assert.False(Directory.Exists root.value)
                finally
                    kitWindow.Close()
            Assert.Empty resolutionFailure.options
            Assert.Equal<string list>([ "TryGetFolderFromPathAsync" ], resolutionFailure.calls)
            let options = Assert.Single dialogFailure.options
            Assert.Equal("Choose a kit folder", options.Title)
            Assert.False options.AllowMultiple
            Assert.Same(resolved, options.SuggestedStartLocation)
            Assert.Equal<string list>([ "TryGetFolderFromPathAsync"; "OpenFolderPickerAsync" ], dialogFailure.calls))

    [<Fact>]
    let ``create imports the Built-in catalogue into a new library database file and a reopen does not import again`` () : unit =
        let locations = TestAppScope.fileLocations owner
        let first =
            AppContext.create locations WorkbenchSettings.defaults
            |> Result.defaultWith (fun error -> failwith error.describe)
        let expected = BuiltinCatalogue.samples () |> List.sortBy _.id
        assertEmptyStartup first
        assertPendingCatalogue first (BuiltinCatalogue.catalogueData ()) |> ignore
        TestAppScope.settlePendingImports first
        Assert.Equal<Result<Sample list, SampleError>>(
            Ok expected,
            first.samples.listSamples IncludeInactive |> Result.map (List.sortBy _.id))
        Assert.Equal<Result<unit, SampleError>>(Ok (), first.samples.removeSample SampleIds.glassFilm600Id)
        let imports = first.catalogueImports.proxy.loggedImports () |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Single imports |> ignore
        let held = first.catalogueImports.proxy.heldEntriesOf CatalogueIds.builtInCatalogue

        let reopened =
            AppContext.create locations WorkbenchSettings.defaults
            |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Empty reopened.pendingImports
        Assert.Equal<Result<CatalogueImportLogEntry list, CatalogueImportError>>(Ok imports, reopened.catalogueImports.proxy.loggedImports ())
        Assert.Equal<Result<HeldEntry list, CatalogueImportError>>(held, reopened.catalogueImports.proxy.heldEntriesOf CatalogueIds.builtInCatalogue)
        TestAppScope.settlePendingImports reopened
        Assert.Equal<Result<Sample list, SampleError>>(
            Ok (expected |> List.filter (fun sample -> sample.id <> SampleIds.glassFilm600Id)),
            reopened.samples.listSamples IncludeInactive |> Result.map (List.sortBy _.id))

    [<Fact>]
    let ``create imports a changed shipped catalogue on the next start and skips an unchanged one`` () : unit =
        let locations = isolatedShippedLocations ()
        let catalogue = BuiltinCatalogue.catalogueData ()
        writeShippedCatalogue locations catalogue
        let first =
            AppContext.create locations WorkbenchSettings.defaults
            |> Result.defaultWith (fun error -> failwith error.describe)
        assertEmptyStartup first
        assertPendingCatalogue first catalogue |> ignore
        TestAppScope.settlePendingImports first
        let silicon = BuiltinCatalogue.material MaterialIds.silicon
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some silicon), first.materials.tryGetMaterial silicon.id)
        let importsBefore = first.catalogueImports.proxy.loggedImports () |> Result.defaultWith (fun error -> failwith error.describe)
        let heldBefore = first.catalogueImports.proxy.heldEntriesOf CatalogueIds.builtInCatalogue
        let original = catalogue.envelopes |> List.find (fun envelope -> envelope.content.id = silicon.id.toLibraryEntryId)
        let description = EntryDescription "Silicon description from the next shipped catalogue"
        let content =
            {
                original.content with
                    version = VersionNumber.first.next
                    descriptionOpt = Some description
            }
        let updated = content.``sealed`` |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal(original.content.name, updated.content.name)
        Assert.Equal(original.content.fileName, updated.content.fileName)
        let changed =
            catalogue.envelopes
            |> List.map (fun envelope -> if envelope.content.id = updated.content.id then updated else envelope)
            |> CatalogueData.tryCreate catalogue.manifest.id catalogue.manifest.name catalogue.manifest.version.next
            |> Result.defaultWith (fun error -> failwith error.describe)
        writeShippedCatalogue locations changed

        let second =
            AppContext.create locations WorkbenchSettings.defaults
            |> Result.defaultWith (fun error -> failwith error.describe)
        let pending = assertPendingCatalogue second changed
        Assert.Equal(CatalogueProxy.builtInFolderUnder locations.shippedCatalogueFolder, pending.entry.folder)
        Assert.Equal<Result<HeldEntry list, CatalogueImportError>>(heldBefore, second.catalogueImports.proxy.heldEntriesOf CatalogueIds.builtInCatalogue)
        Assert.Equal<Result<CatalogueImportLogEntry list, CatalogueImportError>>(Ok importsBefore, second.catalogueImports.proxy.loggedImports ())
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some silicon), second.materials.tryGetMaterial silicon.id)
        let nextVersion =
            {
                materialId = silicon.id
                version = content.version
            }
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok None, second.materials.resolveVersion nextVersion)
        TestAppScope.settlePendingImports second
        let expected =
            {
                silicon with
                    description = Some description
            }
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some expected), second.materials.tryGetMaterial silicon.id)
        let firstVersion = MaterialVersionId.firstOf silicon.id
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some silicon), second.materials.resolveVersion firstVersion)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some expected), second.materials.resolveVersion nextVersion)
        let importsAfter = second.catalogueImports.proxy.loggedImports () |> Result.defaultWith (fun error -> failwith error.describe)
        let logged = Assert.Single importsAfter
        Assert.Equal(CatalogueIds.builtInCatalogue, logged.catalogueId)
        Assert.Equal(pending.entry.manifestHash, logged.manifestHash)
        Assert.NotEqual((Assert.Single importsBefore).manifestHash, logged.manifestHash)
        Assert.Equal(pending.entry.entryCount, logged.entryCount)
        let heldAfter = second.catalogueImports.proxy.heldEntriesOf CatalogueIds.builtInCatalogue

        let third =
            AppContext.create locations WorkbenchSettings.defaults
            |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Empty third.pendingImports
        Assert.Equal<Result<CatalogueImportLogEntry list, CatalogueImportError>>(Ok importsAfter, third.catalogueImports.proxy.loggedImports ())
        Assert.Equal<Result<HeldEntry list, CatalogueImportError>>(heldAfter, third.catalogueImports.proxy.heldEntriesOf CatalogueIds.builtInCatalogue)
        TestAppScope.settlePendingImports third
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some expected), third.materials.tryGetMaterial silicon.id)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some silicon), third.materials.resolveVersion firstVersion)

    [<Fact>]
    let ``create holds every changed-catalogue row pending and writes nothing before confirmation`` () : unit =
        let locations = isolatedShippedLocations ()
        let catalogue = BuiltinCatalogue.catalogueData ()
        writeShippedCatalogue locations catalogue
        let first =
            AppContext.create locations WorkbenchSettings.defaults
            |> Result.defaultWith (fun error -> failwith error.describe)
        assertEmptyStartup first
        assertPendingCatalogue first catalogue |> ignore
        TestAppScope.settlePendingImports first
        let userMaterial =
            {
                BuiltinCatalogue.material MaterialIds.glass152 with
                    id = newMaterialId ()
                    name = EntryName "Lab glass"
            }
        Assert.Equal<Result<unit, MaterialError>>(Ok (), first.materials.saveMaterial userMaterial)
        let incomingMaterialId = newMaterialId ()
        let glass = catalogue.envelopes |> List.find (fun envelope -> envelope.content.id = MaterialIds.glass152.toLibraryEntryId)
        let incomingContent =
            {
                glass.content with
                    id = incomingMaterialId.toLibraryEntryId
                    name = userMaterial.name
                    provenance = BuiltInProvenance CatalogueIds.builtInCatalogue
            }
        let incomingMaterial = incomingContent.``sealed`` |> Result.defaultWith (fun error -> failwith error.describe)
        let source = catalogue.envelopes |> List.find (fun envelope -> envelope.content.schema.kind = SourceEntryKind)
        let sourceContent =
            {
                source.content with
                    id = LibraryEntryId (System.Guid.NewGuid())
                    name = EntryName "New shipped catalogue source"
                    provenance = BuiltInProvenance CatalogueIds.builtInCatalogue
            }
        let incomingSource = sourceContent.``sealed`` |> Result.defaultWith (fun error -> failwith error.describe)
        let sourcePreset = sourceContent.toSourcePreset |> Result.defaultWith (fun error -> failwith error.describe)
        let changed =
            CatalogueData.tryCreate catalogue.manifest.id catalogue.manifest.name catalogue.manifest.version.next
                (catalogue.envelopes @ [ incomingMaterial; incomingSource ])
            |> Result.defaultWith (fun error -> failwith error.describe)
        writeShippedCatalogue locations changed
        let importsBefore = first.catalogueImports.proxy.loggedImports () |> Result.defaultWith (fun error -> failwith error.describe)
        let heldBefore = first.catalogueImports.proxy.heldEntriesOf CatalogueIds.builtInCatalogue
        let userBefore = first.catalogueImports.proxy.tryHeldEntry userMaterial.id.toLibraryEntryId

        let second =
            AppContext.create locations WorkbenchSettings.defaults
            |> Result.defaultWith (fun error -> failwith error.describe)
        let pending = assertPendingCatalogue second changed
        let collision = pending.decisions |> List.find (fun decision -> decision.entryId = incomingMaterial.content.id)
        match collision.status with
        | NameCollision (incoming, sameName) ->
            Assert.Equal(incomingMaterial, incoming)
            Assert.Equal<LibraryEntryId list>([ userMaterial.id.toLibraryEntryId ], sameName |> List.map _.id)
        | _ -> Assert.Fail("Lab glass must be presented as a same-name material collision")
        Assert.Equal(catalogue.envelopes.Length, pending.decisions |> List.filter (fun decision -> decision.defaultAction = ReuseHeldEntry) |> List.length)
        let newSource = pending.decisions |> List.find (fun decision -> decision.entryId = incomingSource.content.id)
        match newSource.status with
        | EntryIdNotHeld incoming -> Assert.Equal(incomingSource, incoming)
        | _ -> Assert.Fail("the new source must be presented as an unheld entry")
        Assert.Equal(AddIncomingEntry, newSource.defaultAction)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok None, second.materials.tryGetMaterial incomingMaterialId)
        Assert.Equal<Result<LibraryEntry option, LibraryError>>(Ok None, second.library.tryGetEntry sourcePreset.id)
        Assert.Equal<Result<HeldEntry option, CatalogueImportError>>(Ok None, second.catalogueImports.proxy.tryHeldEntry incomingMaterial.content.id)
        Assert.Equal<Result<HeldEntry option, CatalogueImportError>>(Ok None, second.catalogueImports.proxy.tryHeldEntry incomingSource.content.id)
        Assert.Equal<Result<HeldEntry list, CatalogueImportError>>(heldBefore, second.catalogueImports.proxy.heldEntriesOf CatalogueIds.builtInCatalogue)
        Assert.Equal<Result<HeldEntry option, CatalogueImportError>>(userBefore, second.catalogueImports.proxy.tryHeldEntry userMaterial.id.toLibraryEntryId)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some userMaterial), second.materials.tryGetMaterial userMaterial.id)
        Assert.Equal<Result<CatalogueImportLogEntry list, CatalogueImportError>>(Ok importsBefore, second.catalogueImports.proxy.loggedImports ())

    [<Fact>]
    let ``create answers LibrarySchemaMismatchErr for a library of another schema and leaves the file as it was`` () : unit =
        let locations = TestAppScope.fileLocations owner
        do
            use database =
                LibraryDatabase.openOrCreate locations.libraryConnection
                |> Result.defaultWith (fun error -> failwith error.describe)
            Assert.Equal(CreatedLibraryDatabase, database.origin)

        let incompatible = LibrarySchemaVersion 2
        let builder = SqliteConnectionStringBuilder(locations.libraryConnection.value)
        builder.Pooling <- false
        do
            use connection = new SqliteConnection(builder.ConnectionString)
            connection.Open()
            use command = connection.CreateCommand()
            command.CommandText <- "UPDATE SchemaStamp SET SchemaVersion = $version WHERE Id = 1;"
            command.Parameters.AddWithValue("$version", incompatible.value) |> ignore
            Assert.Equal(1, command.ExecuteNonQuery())

        let before = File.ReadAllBytes builder.DataSource
        match AppContext.create locations WorkbenchSettings.defaults with
        | Error (LibrarySchemaMismatchErr mismatch) ->
            Assert.Equal(Some incompatible, mismatch.foundOpt)
            Assert.Equal(LibrarySchemaVersion.current, mismatch.expected)
        | Error error -> Assert.Fail(error.describe)
        | Ok _ -> Assert.Fail("an incompatible library must not open")
        Assert.Equal<byte>(before, File.ReadAllBytes builder.DataSource)

        use inspected = new SqliteConnection(builder.ConnectionString)
        inspected.Open()
        use stamp = inspected.CreateCommand()
        stamp.CommandText <- "SELECT SchemaVersion FROM SchemaStamp WHERE Id = 1;"
        Assert.Equal(int64 incompatible.value, unbox<int64> (stamp.ExecuteScalar()))

    [<Fact>]
    let ``create writes experiment exports into the configured exports folder`` () : unit =
        let locations = TestAppScope.testLocations owner
        Directory.CreateDirectory locations.exportsFolder.value |> ignore
        Directory.GetFiles locations.exportsFolder.value |> Array.iter File.Delete
        let run : ExportedRun =
            {
                title = ExportText "Configured export"
                xLabel = ExportText "Wavelength"
                yLabel = ExportText "Intensity"
                description = ExportText "App-scope export destination"
                series =
                    [
                        {
                            name = ExportText "Transmission"
                            points =
                                [
                                    {
                                        x = 500.0
                                        y = 0.5
                                    }
                                    {
                                        x = 600.0
                                        y = 0.75
                                    }
                                ]
                        }
                    ]
            }
        let context = freshContext ()
        Assert.Equal<Result<unit, ExportError>>(Ok (), context.experimentExport.exportRun run)
        Assert.Single(Directory.GetFiles(locations.exportsFolder.value, "experiment-*.csv")) |> ignore

    [<Fact>]
    let ``create imports the Built-in catalogue into empty stores`` () : unit =
        let context =
            AppContext.create (TestAppScope.testLocations owner) WorkbenchSettings.defaults
            |> Result.defaultWith (fun error -> failwith error.describe)
        assertEmptyStartup context
        assertPendingCatalogue context (BuiltinCatalogue.catalogueData ()) |> ignore
        TestAppScope.settlePendingImports context
        Assert.Equal<Result<Sample list, SampleError>>(
            Ok (BuiltinCatalogue.samples () |> List.sortBy _.id),
            context.samples.listSamples IncludeInactive |> Result.map (List.sortBy _.id))
        Assert.Equal<Result<MaterialEntry list, MaterialError>>(
            Ok (BuiltinCatalogue.materials () |> List.sortBy _.id),
            context.materials.listMaterials IncludeInactive |> Result.map (List.sortBy _.id))
        Assert.Equal<Result<MaterialCategory list, CategoryError>>(
            Ok (BuiltinCatalogue.categories () |> List.sortBy _.id),
            context.categories.listCategories () |> Result.map (List.sortBy _.id))
        for preset in BuiltinCatalogue.presets () do
            Assert.Equal<Result<LibraryEntry option, LibraryError>>(
                Ok (Some preset), context.library.tryGetEntry preset.entryId)

    [<Fact>]
    let ``create couples the materials store to the LIVE samples store: a referenced material is not removable`` () =
        let context = freshContext ()
        // BuiltinCatalogue.samples () reference glass152; removal must consult the SAME
        // samples store `create` built (the composition order matters).
        match context.materials.removeMaterial MaterialIds.glass152 with
        | Error (MaterialStillReferencedErr reason) -> Assert.Contains("is still referenced by", reason)
        | other -> Assert.Fail($"expected MaterialStillReferencedErr from the coupled store, got %A{other}")

    [<Fact>]
    let ``two workbench surfaces composed from ONE app scope observe the same store`` () =
        let context = freshContext ()
        let first = surfaceOf context
        let second = surfaceOf context
        // Mutate through the FIRST surface's injected seam…
        match first.samples.removeSample SampleIds.glassFilm600Id with
        | Ok () -> ()
        | Error e -> Assert.Fail($"removing the seeded sample must succeed, got %A{e}")
        // …and the SECOND surface no longer lists it — one store, not a copy per window.
        match second.samples.listSamples ActiveOnly with
        | Ok all -> Assert.DoesNotContain(SampleIds.glassFilm600Id, all |> List.map (fun s -> s.id))
        | Error e -> Assert.Fail($"listSamples failed: %A{e}")

    [<Fact>]
    let ``two separate app scopes do NOT share stores — each create imports its own`` () =
        let scopeA = freshContext ()
        let scopeB = freshContext ()
        match scopeA.samples.removeSample SampleIds.glassFilm600Id with
        | Ok () -> ()
        | Error e -> Assert.Fail($"removing the seeded sample must succeed, got %A{e}")
        match scopeB.samples.listSamples ActiveOnly with
        | Ok all -> Assert.Contains(SampleIds.glassFilm600Id, all |> List.map (fun s -> s.id))
        | Error e -> Assert.Fail($"listSamples failed: %A{e}")

    // ======================== headless acceptance (gate `ui-smoke`) ========================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: two Main windows over ONE app scope share the stores — a sample added through one is listed by the other`` () =
        HeadlessSession.run (fun () ->
            let context = freshContext ()
            // Two REAL Main windows over the SAME injected scope — the launcher's Main button
            // clicked twice (spec 0038 step 006: launcher-opened windows share the stores).
            let first = OpticalConstructor.App.MainConstructorWindow(context, TestAppScope.openedKitIn owner context.catalogueImports)
            first.Show()
            Dispatcher.UIThread.RunJobs()
            let second = OpticalConstructor.App.MainConstructorWindow(context, TestAppScope.openedKitIn owner context.catalogueImports)
            second.Show()
            Dispatcher.UIThread.RunJobs()
            // Observe the windows the REAL launcher seam opens (subscribed only after both Main
            // windows are shown — the WireUiCompositionTests seam).
            let opened = ResizeArray<Window>()
            use _sub =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _args)) ->
                    match sender with
                    | :? Window as w -> opened.Add w
                    | _ -> ())
            // Surface 1: the Library… strip button (spec 0038 step 015) → the REAL Library
            // window → Make-multilayer → the REAL Sample editor over the SHARED store (the
            // seeded-period creation path, so a plain name + Save persists).
            clickOn first ConstructorUiId.openLibraryWindowButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            let libraryOfFirst = opened.[0]
            Assert.True(matchesId ConstructorUiId.libraryWindow.value libraryOfFirst, "the strip button must open the Library window")
            clickOn libraryOfFirst ConstructorUiId.libraryMakeMultilayerButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(2, opened.Count)
            let editor = opened.[1]
            Assert.True(matchesId ConstructorUiId.sampleEditorWindow.value editor, "the opened window must be the Sample editor")
            setText editor ConstructorUiId.sampleNameBox.value "Shared-scope sample"
            clickOn editor ConstructorUiId.sampleEditorSaveButton.value
            Assert.False(editor.IsVisible, "Save must persist the entry and close the editor")
            // The id was minted at the verb dispatch — recover it through the SHARED app-scope store.
            let savedId =
                match context.samples.listSamples ActiveOnly with
                | Ok all ->
                    match all |> List.tryFind (fun s -> s.name.value = "Shared-scope sample") with
                    | Some sample -> sample.id
                    | None -> failwith "the saved sample must be in the app-scope store"
                | Error e -> failwith $"listSamples failed: %A{e}"
            // Close the first surface's Library window — the registry is app-global, so the
            // second surface's strip click must CREATE afresh over the same scope, not activate
            // a stale window.
            libraryOfFirst.Close()
            Dispatcher.UIThread.RunJobs()
            // Surface 2: ITS strip button opens a fresh Library window that re-queries the SAME
            // store — the added sample is listed by the other window.
            clickOn second ConstructorUiId.openLibraryWindowButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(3, opened.Count)
            let libraryOfSecond = opened.[2]
            Assert.True(matchesId ConstructorUiId.libraryWindow.value libraryOfSecond, "the strip button must open the Library window")
            // The tree opens collapsed (spec 0040 step 002) — expand entries to reach the leaf.
            clickOn libraryOfSecond (ConstructorUiId.facetTreeChevron (ConstructorUiKey "entries")).value
            Assert.True(isPresent libraryOfSecond ((ConstructorUiId.libraryEntryNode savedId.toElementValueId).value),
                        "the second Main window's Library window must list the sample added through the first")
            libraryOfSecond.Close()
            Dispatcher.UIThread.RunJobs()
            second.Close()
            first.Close())
