namespace OpticalConstructor.Tests

open System
open System.IO
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Ui.ConstructorUiIdOfDomain
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Ui.WindowLauncher

module KitOpeningTests =

    let private openingAuthor : KitAuthor = KitAuthor "test"

    let private openingValue (result : Result<'Value, KitLibraryError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private kitValue (result : Result<'Value, KitError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private importValue (result : Result<'Value, CatalogueImportError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private withOpeningFolder
        (run : KitFolder -> CatalogueImportContext -> InMemoryCatalogueStores -> unit)
        : unit =
        let root = FolderName (Path.Combine(AppContext.BaseDirectory, "KitOpeningTests", Guid.NewGuid().ToString("N")))
        let location = FolderName (Path.Combine(root.value, "Optics QZ"))
        let folder = KitFolder location
        let stores = BuiltinCatalogue.freshStores ()
        let imports = CatalogueImportContext.create stores.proxy
        try run folder imports stores
        finally
            if Directory.Exists root.value then Directory.Delete(root.value, true)

    let private withOpenedKit
        (folder : KitFolder)
        (access : KitAccess)
        (imports : CatalogueImportContext)
        (run : OpenedKit -> unit)
        : unit =
        let opened = openKitAt folder access ClockProvider.system openingAuthor imports |> openingValue
        try run opened
        finally opened.library.kit.close () |> kitValue

    let private folderBytes (folder : KitFolder) : Map<FileName, FileByteContent> =
        Directory.GetFiles(folder.value.value, "*", SearchOption.AllDirectories)
        |> Array.map (fun path ->
            let name = FileName (Path.GetRelativePath(folder.value.value, path))
            let content = FileByteContent (File.ReadAllBytes path)
            name, content)
        |> Map.ofArray

    let private openingExperiment : Experiment =
        {
            id = ExperimentId 1
            setup = { name = SetupName "Dark"; revision = SetupRevision 1 }
            varied = None
            measurement = CaptureTransmitted
            range = VariableRange.forVariable VaryR1
            dataFileOpt = None
            sourceWaveLengthOpt = None
        }

    [<Fact>]
    let ``opening an empty folder initialises a kit named for it and opens it read-write`` () : unit =
        withOpeningFolder (fun folder imports _ ->
            Assert.False(Directory.Exists folder.value.value)
            match openKitAt folder ReadOnlyKitAccess ClockProvider.system openingAuthor imports with
            | Error (KitLibraryKitErr (NotAKitFolderErr _)) -> ()
            | Error error -> Assert.Fail error.describe
            | Ok opened ->
                opened.library.kit.close () |> kitValue
                Assert.Fail "a read-only open must not initialize a missing kit"
            Assert.False(Directory.Exists folder.value.value)

            withOpenedKit folder ReadWriteKitAccess imports (fun opened ->
                let expectedName = KitName "Optics QZ"
                let context = opened.library.kit
                Assert.Equal<KitName>(expectedName, context.kitFile.name)
                Assert.Equal<KitAuthor>(openingAuthor, context.kitFile.author)
                Assert.Equal<KitAccess>(ReadWriteKitAccess, context.access)
                Assert.Equal(DateTimeKind.Utc, context.kitFile.created.value.Kind)
                Assert.Equal<KitFile>(context.kitFile, context.proxy.readKitFile () |> kitValue)
                Assert.Empty opened.experiments
                Assert.Empty opened.setups
                Assert.Empty opened.decisions
                Assert.Empty opened.pendingDecisions
                Assert.Empty opened.overlay.snapshots
                Assert.Empty opened.overlay.replacements
                Assert.True(File.Exists(Path.Combine(folder.value.value, ".kit-json")))
                Assert.True(Directory.Exists(Path.Combine(folder.value.value, "data")))
                Assert.True(Directory.Exists(Path.Combine(folder.value.value, "library")))
                Assert.True(File.Exists(Path.Combine(folder.value.value, ".oc", "lock"))))

            Assert.False(File.Exists(Path.Combine(folder.value.value, ".oc", "lock"))))

    [<Fact>]
    let ``an opened kit carries the folder it was opened from`` () : unit =
        withOpeningFolder (fun folder imports _ ->
            withOpenedKit folder ReadWriteKitAccess imports (fun opened ->
                Assert.Equal<KitFolder>(folder, opened.folder)))

        withOpeningFolder (fun folder imports _ ->
            withOpenedKit folder ReadWriteKitAccess imports ignore
            withOpenedKit folder ReadOnlyKitAccess imports (fun opened ->
                Assert.Equal<KitAccess>(ReadOnlyKitAccess, opened.library.kit.access)
                Assert.Equal<KitFolder>(folder, opened.folder)))

        withOpeningFolder (fun folder imports _ ->
            withOpenedKit folder ReadWriteKitAccess imports ignore
            let opened = openExistingKitAt folder ClockProvider.system imports |> openingValue
            try Assert.Equal<KitFolder>(folder, opened.folder)
            finally opened.library.kit.close () |> kitValue)

    [<Fact>]
    let ``opening a kit reads its experiments and setups`` () : unit =
        withOpeningFolder (fun folder imports _ ->
            let bench =
                Setup.create (SetupName "Bench") [] []
                |> Result.defaultWith (fun error -> failwith error.describe)
            withOpenedKit folder ReadWriteKitAccess imports (fun opened ->
                opened.library.kit.proxy.writeExperiment openingExperiment |> kitValue
                opened.library.kit.proxy.writeSetup bench |> kitValue)

            withOpenedKit folder ReadWriteKitAccess imports (fun opened ->
                Assert.Equal<Experiment list>([ openingExperiment ], opened.experiments)
                Assert.Equal<Setup list>([ bench ], opened.setups)
                Assert.Empty opened.pendingDecisions))

    let private openingCpl1 : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "2f6c9a41-7b3e-4d58-a1c0-8e5d3b97f264")
                label = ItemLabel "CPL1"
                model = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId)
                dial = DialMounted DialPositive
            }
        draft.toItem

    let private openingLp1 : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "b93e1d05-6a4f-4c27-8d18-f0c2a5e7b631")
                label = ItemLabel "LP1"
                model = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.realLinearPolarizerModelId)
                dial = DialMounted DialNegative
            }
        draft.toItem

    [<Fact>]
    let ``opening a kit reads its items, and an item file that does not read refuses the open`` () : unit =
        withOpeningFolder (fun folder imports _ ->
            // Written out of label order: the open answers them in label order.
            withOpenedKit folder ReadWriteKitAccess imports (fun opened ->
                Assert.Empty opened.items
                for item in [ openingLp1; openingCpl1 ] do
                    opened.library.kit.proxy.writeItem item |> kitValue)

            for access in [ ReadWriteKitAccess; ReadOnlyKitAccess ] do
                withOpenedKit folder access imports (fun opened ->
                    Assert.Equal<KitAccess>(access, opened.library.kit.access)
                    Assert.Equal<Item list>([ openingCpl1; openingLp1 ], opened.items))

            let broken = FileName (Path.Combine(folder.value.value, "Broken.item-json"))
            File.WriteAllText(broken.value, "{")
            for access in [ ReadWriteKitAccess; ReadOnlyKitAccess ] do
                match openKitAt folder access ClockProvider.system openingAuthor imports with
                | Error (KitLibraryKitErr (KitFileRejectedErr reason)) -> Assert.Contains("Broken.item-json", reason)
                | Error error -> Assert.Fail error.describe
                | Ok opened ->
                    opened.library.kit.close () |> kitValue
                    Assert.Fail "a kit whose item file does not read must refuse the open"
                Assert.False(File.Exists(Path.Combine(folder.value.value, ".oc", "lock")))

            // The refusal is that file's alone, and the refused open left the kit free to open read-write again.
            File.Delete broken.value
            withOpenedKit folder ReadWriteKitAccess imports (fun reopened ->
                Assert.Equal<Item list>([ openingCpl1; openingLp1 ], reopened.items)))

    [<Fact>]
    let ``a kit open elsewhere is KitLockedErr inside the open's error, and opening it read-only writes nothing`` () : unit =
        withOpeningFolder (fun folder imports _ ->
            withOpenedKit folder ReadWriteKitAccess imports (fun owner ->
                owner.library.kit.proxy.writeExperiment openingExperiment |> kitValue
                let before = folderBytes folder
                match openKitAt folder ReadWriteKitAccess ClockProvider.system openingAuthor imports with
                | Error (KitLibraryKitErr (KitLockedErr holder)) ->
                    Assert.Equal<ProcessId>(ProcessId.getCurrentProcessId (), holder.owner.processId)
                    Assert.Equal(Environment.MachineName, holder.owner.machine.value)
                    Assert.Equal(Environment.UserName, holder.owner.user.value)
                | Error error -> Assert.Fail error.describe
                | Ok opened ->
                    opened.library.kit.close () |> kitValue
                    Assert.Fail "a second writer must be refused with the existing lock holder"
                Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder)

                withOpenedKit folder ReadOnlyKitAccess imports (fun reader ->
                    Assert.Equal<KitAccess>(ReadOnlyKitAccess, reader.library.kit.access)
                    Assert.Equal<KitFile>(owner.library.kit.kitFile, reader.library.kit.kitFile)
                    Assert.Equal<Experiment list>([ openingExperiment ], reader.experiments)
                    Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder)
                    match reader.library.kit.proxy.writeExperiment openingExperiment with
                    | Error (KitReadOnlyErr _) -> ()
                    | Error error -> Assert.Fail error.describe
                    | Ok () -> Assert.Fail "a read-only kit must refuse content writes")

                Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder))

            let unlocked = folderBytes folder
            withOpenedKit folder ReadOnlyKitAccess imports (fun _ ->
                Assert.False(File.Exists(Path.Combine(folder.value.value, ".oc", "lock")))
                Assert.Equal<Map<FileName, FileByteContent>>(unlocked, folderBytes folder))
            Assert.Equal<Map<FileName, FileByteContent>>(unlocked, folderBytes folder))

    let private assertNotAKitFolder (folder : KitFolder) (imports : CatalogueImportContext) : unit =
        match openExistingKitAt folder ClockProvider.system imports with
        | Error (KitLibraryKitErr (NotAKitFolderErr _)) -> ()
        | Error error -> Assert.Fail error.describe
        | Ok opened ->
            opened.library.kit.close () |> kitValue
            Assert.Fail "opening an existing kit must never initialise a folder"

    [<Fact>]
    let ``opening an existing kit opens it read-write with its kit file and never initialises a folder`` () : unit =
        withOpeningFolder (fun folder imports _ ->
            let lockFile = FileName (Path.Combine(folder.value.value, ".oc", "lock"))
            let created = openKitAt folder ReadWriteKitAccess ClockProvider.system openingAuthor imports |> openingValue
            created.library.kit.close () |> kitValue
            Assert.False(File.Exists lockFile.value)

            let opened = openExistingKitAt folder ClockProvider.system imports |> openingValue
            try
                let context = opened.library.kit
                Assert.Equal<KitAccess>(ReadWriteKitAccess, context.access)
                Assert.Equal<KitFile>(created.library.kit.kitFile, context.kitFile)
                Assert.Equal<KitFile>(context.kitFile, context.proxy.readKitFile () |> kitValue)
                Assert.True(File.Exists lockFile.value)
            finally opened.library.kit.close () |> kitValue
            Assert.False(File.Exists lockFile.value))

        withOpeningFolder (fun folder imports _ ->
            let notes = FileName (Path.Combine(folder.value.value, "notes.txt"))
            Directory.CreateDirectory folder.value.value |> ignore
            File.WriteAllText(notes.value, "not a kit")
            let before = folderBytes folder
            assertNotAKitFolder folder imports
            Assert.Equal<string list>([ notes.value ], Directory.GetFileSystemEntries(folder.value.value, "*", SearchOption.AllDirectories) |> List.ofArray)
            Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder))

        withOpeningFolder (fun folder imports _ ->
            Assert.False(Directory.Exists folder.value.value)
            assertNotAKitFolder folder imports
            Assert.False(Directory.Exists folder.value.value))

    [<Fact>]
    let ``opening an existing kit open elsewhere is KitLockedErr and writes nothing`` () : unit =
        withOpeningFolder (fun folder imports _ ->
            withOpenedKit folder ReadWriteKitAccess imports (fun _ ->
                let before = folderBytes folder
                match openExistingKitAt folder ClockProvider.system imports with
                | Error (KitLibraryKitErr (KitLockedErr holder)) ->
                    Assert.Equal<ProcessId>(ProcessId.getCurrentProcessId (), holder.owner.processId)
                    Assert.Equal(Environment.MachineName, holder.owner.machine.value)
                    Assert.Equal(Environment.UserName, holder.owner.user.value)
                | Error error -> Assert.Fail error.describe
                | Ok opened ->
                    opened.library.kit.close () |> kitValue
                    Assert.Fail "opening a kit open elsewhere must be refused with the existing lock holder"
                Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder)))

    [<Fact>]
    let ``a kit whose experiment file does not read is refused and left unlocked`` () : unit =
        withOpeningFolder (fun folder imports _ ->
            withOpenedKit folder ReadWriteKitAccess imports ignore
            for name in [ FileName "E1.experiment-json"; FileName "Bench.setup-json" ] do
                let file = FileName (Path.Combine(folder.value.value, name.value))
                File.WriteAllText(file.value, "{")
                match openKitAt folder ReadWriteKitAccess ClockProvider.system openingAuthor imports with
                | Error (KitLibraryKitErr (KitFileRejectedErr reason)) -> Assert.Contains(name.value, reason)
                | Error error -> Assert.Fail error.describe
                | Ok opened ->
                    opened.library.kit.close () |> kitValue
                    Assert.Fail "a malformed content file must refuse the open"
                Assert.False(File.Exists(Path.Combine(folder.value.value, ".oc", "lock")))
                File.Delete file.value
                withOpenedKit folder ReadWriteKitAccess imports (fun reopened ->
                    Assert.Empty reopened.experiments
                    Assert.Empty reopened.setups))

    [<Fact>]
    let ``a kit holding a snapshot the library lacks opens with a pending row, and its overlay serves the entry`` () : unit =
        withOpeningFolder (fun folder imports stores ->
            let materialId = MaterialId (Guid.NewGuid ())
            let materialName = EntryName "Kit-only opening material"
            let envelope = BuiltinCatalogue.userMaterialContent materialId materialName |> imports.sealEntry |> importValue
            let silicon =
                (BuiltinCatalogue.catalogueData ()).envelopes
                |> List.find (fun entry -> entry.content.id = MaterialIds.silicon.toLibraryEntryId)
            let beforeLog = imports.proxy.loggedImports () |> importValue
            let beforeUsers = imports.proxy.heldUserEntries () |> importValue
            let beforeSilicon = imports.proxy.tryHeldEntry silicon.content.id |> importValue
            let writes = ResizeArray<string>()
            let watchedProxy =
                {
                    imports.proxy with
                        putEntryVersion = fun put ->
                            writes.Add "putEntryVersion"
                            imports.proxy.putEntryVersion put
                        pinEntryVersion = fun id version ->
                            writes.Add "pinEntryVersion"
                            imports.proxy.pinEntryVersion id version
                        retireEntry = fun id ->
                            writes.Add "retireEntry"
                            imports.proxy.retireEntry id
                        logImport = fun entry ->
                            writes.Add "logImport"
                            imports.proxy.logImport entry
                        forgetImport = fun id ->
                            writes.Add "forgetImport"
                            imports.proxy.forgetImport id
                }
            let watchedImports =
                {
                    imports with proxy = watchedProxy
                }
            withOpenedKit folder ReadWriteKitAccess watchedImports (fun opened ->
                opened.library.kit.proxy.writeSnapshot envelope |> kitValue)
            withOpenedKit folder ReadWriteKitAccess watchedImports (fun opened ->
                Assert.Equal<ImportDecision list>(opened.decisions, opened.pendingDecisions)
                match opened.pendingDecisions with
                | [ { status = EntryIdNotHeld incoming } ] -> Assert.Equal<LibraryEntryEnvelope>(envelope, incoming)
                | _ -> Assert.Fail "the unheld snapshot must remain a pending import row"
                Assert.Equal<LibraryEntryEnvelope list>([ envelope ], opened.overlay.snapshots)
                let materials = stores.materials.overlaidWith opened.overlay
                match materials.tryGetMaterial materialId with
                | Ok (Some material) ->
                    Assert.Equal<MaterialId>(materialId, material.id)
                    Assert.Equal<EntryName>(materialName, material.name)
                | Error (UnknownMaterialIdErr reason)
                | Error (DuplicateMaterialIdErr reason)
                | Error (MaterialStillReferencedErr reason)
                | Error (InvalidMaterialErr reason)
                | Error (MaterialVersionPinnedErr reason)
                | Error (MaterialStoreUnavailableErr reason) -> Assert.Fail reason
                | Ok None -> Assert.Fail "the overlay must resolve the kit-only material"

                let rebinding : EntryRebinding =
                    {
                        incoming = envelope.content.id
                        held = silicon.content.id
                    }
                let rebound = opened.withRebindings [ rebinding ] |> openingValue
                let expectedReplacement : EntryReplacement =
                    {
                        incoming = envelope.content.id
                        held = silicon
                    }
                Assert.Equal<EntryReplacement list>([ expectedReplacement ], rebound.overlay.replacements)
                Assert.Empty opened.overlay.replacements
                Assert.Equal<ImportDecision list>(opened.pendingDecisions, rebound.pendingDecisions)
                Assert.Same(opened.library, rebound.library)
                Assert.Same(opened.experimentData, rebound.experimentData)
                Assert.Equal<LibraryEntryEnvelope list>(opened.overlay.snapshots, rebound.overlay.snapshots)
                opened.library.kit.proxy.writeSnapshot silicon |> kitValue)

            withOpenedKit folder ReadOnlyKitAccess watchedImports (fun opened ->
                let snapshots = opened.library.kit.proxy.listSnapshots () |> kitValue
                Assert.Equal<LibraryEntryId list>(snapshots |> List.map _.content.id, opened.pendingDecisions |> List.map _.entryId)
                Assert.Equal<ImportDecision list>(opened.decisions, opened.pendingDecisions))

            let missingFile = FileName (Path.Combine(folder.value.value, envelope.snapshotPath.value))
            File.Delete missingFile.value
            let before = folderBytes folder
            withOpenedKit folder ReadOnlyKitAccess watchedImports (fun opened ->
                match opened.pendingDecisions with
                | [ { status = SameContentHash (incoming, _); defaultAction = ReuseHeldEntry } ] ->
                    Assert.Equal<LibraryEntryEnvelope>(silicon, incoming)
                | _ -> Assert.Fail "an all-Reuse kit must still require explicit confirmation"
                Assert.Equal<ImportDecision list>(opened.decisions, opened.pendingDecisions)
                Assert.Equal<LibraryEntryEnvelope list>([ silicon ], opened.overlay.snapshots))
            Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder)
            Assert.Empty writes
            Assert.Equal<CatalogueImportLogEntry list>(beforeLog, imports.proxy.loggedImports () |> importValue)
            Assert.Equal<HeldEntry list>(beforeUsers, imports.proxy.heldUserEntries () |> importValue)
            Assert.Equal(beforeSilicon, imports.proxy.tryHeldEntry silicon.content.id |> importValue)

            let refusal = ImportStoreUnavailableErr "opening snapshot decisions failed"
            let failingImports =
                {
                    imports with
                        proxy =
                            {
                                imports.proxy with tryHeldEntry = fun _ -> Error refusal
                            }
                }
            match openKitAt folder ReadWriteKitAccess ClockProvider.system openingAuthor failingImports with
            | Error (KitLibraryImportErr error) -> Assert.Equal<CatalogueImportError>(refusal, error)
            | Error error -> Assert.Fail error.describe
            | Ok opened ->
                opened.library.kit.close () |> kitValue
                Assert.Fail "snapshot decision failure must refuse the open"
            Assert.False(File.Exists(Path.Combine(folder.value.value, ".oc", "lock"))))

    [<Fact>]
    let ``the scratch kit is an empty in-memory kit opened read-write`` () : unit =
        let first = scratchKit () |> kitValue
        let second = scratchKit () |> kitValue
        try
            let expectedName = KitName "Scratch kit"
            let expectedAuthor = KitAuthor ""
            Assert.NotEqual<KitId>(first.kitFile.id, second.kitFile.id)
            Assert.NotSame(first.proxy, second.proxy)
            for context in [ first; second ] do
                Assert.Equal<KitAccess>(ReadWriteKitAccess, context.access)
                Assert.Equal<KitName>(expectedName, context.kitFile.name)
                Assert.Equal<KitAuthor>(expectedAuthor, context.kitFile.author)
                Assert.Equal(DateTimeKind.Utc, context.kitFile.created.value.Kind)
                Assert.Equal<ClockTime>(context.kitFile.created, context.kitFile.modified)
                Assert.Equal<KitFile>(context.kitFile, context.proxy.readKitFile () |> kitValue)
                Assert.Empty(context.proxy.listKindFiles ExperimentKitFile |> kitValue)
                Assert.Empty(context.proxy.listKindFiles SetupKitFile |> kitValue)
                Assert.Empty(context.proxy.listSnapshots () |> kitValue)
                match context.proxy.readDataFile KitRelativePath.kitLockPath with
                | Error (MissingKitFileErr _) -> ()
                | Error error -> Assert.Fail error.describe
                | Ok _ -> Assert.Fail "scratch construction must not acquire a lock"
            first.proxy.writeExperiment openingExperiment |> kitValue
            Assert.Equal<Experiment list>([ openingExperiment ], first.proxy.readExperiments () |> kitValue)
            Assert.Empty(second.proxy.readExperiments () |> kitValue)
        finally
            first.close () |> kitValue
            second.close () |> kitValue

    [<Fact>]
    let ``the open captions name the error and the lock holder`` () : unit =
        let error = KitLibraryKitErr (NotAKitFolderErr "C:/kits/x")
        let holder : KitLockHolder =
            {
                owner =
                    {
                        machine = MachineName "BENCH-1"
                        user = UserName "lab"
                        processId = ProcessId 4242
                    }
                acquiredAt = ClockTime (DateTime(2026, 11, 3, 12, 0, 0, DateTimeKind.Utc))
            }
        Assert.Equal($"The kit could not be opened: {error.describe}", error.openFailedCaption.value)
        Assert.Equal(
            "The kit is open elsewhere: lab on BENCH-1 (process 4242) since 2026-11-03 12:00:00 UTC.",
            holder.openElsewhereCaption.value)

    [<Fact>]
    let ``pickThen hands on the folder the picker answers and nothing on a cancel`` () : unit =
        withOpeningFolder (fun folder _ _ ->
            let answering : KitFolderPicker =
                {
                    pickKitFolder = fun () -> async { return Some folder }
                }
            let picked = ResizeArray<KitFolder>()
            answering.pickThen picked.Add
            Assert.Equal<KitFolder list>([ folder ], List.ofSeq picked)

            let cancelling : KitFolderPicker =
                {
                    pickKitFolder = fun () -> async { return None }
                }
            let cancelled = ResizeArray<KitFolder>()
            cancelling.pickThen cancelled.Add
            Assert.Empty cancelled
            Assert.False(Directory.Exists folder.value.value))

    [<Fact>]
    let ``two spellings of one kit folder are one window key, and no window holds a new kit`` () : unit =
        let forward = KitFolder (FolderName "C:/kits/a")
        let backslashed = KitFolder (FolderName @"C:\KITS\A\")
        Assert.Equal<WindowKey>(forward.windowKey, backslashed.windowKey)
        Assert.Equal<WindowKey>(KitWindowKey forward.canonical, backslashed.windowKey)

        let fresh = KitFolder (FolderName (Path.Combine(AppContext.BaseDirectory, "KitOpeningTests", Guid.NewGuid().ToString("N"))))
        Assert.Equal<KitWindowPresence>(NoKitWindowOpen, fresh.activateKitWindow ())
        Assert.False(Directory.Exists fresh.value.value)
