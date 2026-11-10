namespace OpticalConstructor.Ui.Tests

open System
open System.IO
open System.IO.Compression
open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitArchive
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitHistory
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Controls
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.KitArchiveStore
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Ui
open OpticalConstructor.Ui.ConstructorUiIdOfDomain
open OpticalConstructor.Ui.KitArchiveJob
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Ui.TableAndElementRotationView
open OpticalConstructor.Ui.KitStartView

module KitStartWindowTests =

    let private firstOpen : DateTime = DateTime(2026, 11, 3, 12, 0, 0, DateTimeKind.Utc)

    let private folderOf (kit : string) : KitFolder = KitFolder (FolderName $"C:/kits/{kit}")

    let private entryOf (kit : string) (id : string) (minutes : float) (star : KitStar) : KitHistoryEntry =
        {
            folder = folderOf kit
            kitId = KitId (Guid.Parse id)
            name = KitName $"Kit {kit}"
            lastOpened = ClockTime (firstOpen.AddMinutes minutes)
            star = star
        }

    // KitStartTests' five entries (step 176): that fixture is private to OpticalConstructor.Tests.
    let private kitA : KitHistoryEntry = entryOf "a" "6f1c2b8e-4d37-4a95-8e02-b7d4c9a13f56" 5.0 UnstarredKit
    let private kitB : KitHistoryEntry = entryOf "b" "a03d7e94-1b6c-4f28-9d5a-3c8e0f2b7a41" 1.0 StarredKit
    let private kitC : KitHistoryEntry = entryOf "c" "c58b1f06-7e29-4d43-b1a7-e6f9d204c83b" 9.0 UnstarredKit
    let private kitD : KitHistoryEntry = entryOf "d" "2e9a4c71-d085-4b6f-a3e8-81c5f7d0b924" 7.0 StarredKit
    let private kitE : KitHistoryEntry = entryOf "e" "d7f30a5b-96c2-4e81-8f4d-0b2a6e9c15d7" 5.0 UnstarredKit

    let private stored : KitHistory = { KitHistory.empty with entries = [ kitA; kitB; kitC; kitD; kitE ] }

    let private presentFolders : KitFolder list = [ folderOf "a"; folderOf "c"; folderOf "d" ]

    /// Step 173's holder of a kit open elsewhere.
    let private lockHolder : KitLockHolder =
        {
            owner =
                {
                    machine = MachineName "BENCH-1"
                    user = UserName "lab"
                    processId = ProcessId 4242
                }
            acquiredAt = ClockTime firstOpen
        }

    /// An in-memory store holding `seeded` that sees only `folders` on disk.
    let private historyOver (folders : KitFolder list) (seeded : KitHistory) : KitHistoryProxy =
        let store = KitHistoryProxy.createInMemory folders

        match store.writeHistory seeded with
        | Ok () -> store
        | Error error -> failwith error.describe

    /// An in-memory store holding the five entries, over the present folders `a`, `c` and `d`.
    let private storedHistory () : KitHistoryProxy = historyOver presentFolders stored

    /// A day after every stored open, on every reading.
    let private kitStartClock : ClockProvider =
        {
            now = fun () -> ClockTime (firstOpen.AddDays 1.0)
        }

    let private kitStartAuthor : KitAuthor = KitAuthor "test"

    /// A picker the user always cancels.
    let private cancellingPicker : KitFolderPicker =
        {
            pickKitFolder = fun () -> async { return None }
        }

    /// The screen's seams over `kitHistory` and `picker`; `showKit` records every kit it is given into `shown`.
    let private contextOver (kitHistory : KitHistoryProxy) (picker : KitFolderPicker) (shown : ResizeArray<OpenedKit>) : KitStartContext =
        {
            kitHistory = kitHistory
            limit = KitHistoryLimit.defaultValue
            clock = kitStartClock
            author = kitStartAuthor
            imports = CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy
            picker = picker
            showKit = shown.Add
            activateKitWindow = fun _ -> NoKitWindowOpen
            zipPicker = KitZipPicker.none
            archive = KitArchiveProxy.createInMemory (InMemoryArchiveDisk.create ())
            archiveJobs = KitArchiveJobHost.inert
        }

    /// Runs a fact over its own root under the test output. At its end every kit `shown` recorded is closed, then the
    /// root is deleted; a failed close fails the fact.
    let private withKitStartRoot (run : FolderName -> ResizeArray<OpenedKit> -> unit) : unit =
        let root = FolderName (Path.Combine(System.AppContext.BaseDirectory, "KitStartWindowTests", Guid.NewGuid().ToString("N")))
        let shown = ResizeArray<OpenedKit> ()

        try
            run root shown
        finally
            let closes = shown |> Seq.map (fun kit -> kit.library.kit.close ()) |> List.ofSeq
            if Directory.Exists root.value then Directory.Delete(root.value, true)
            closes |> List.iter (Result.defaultWith (fun (error : KitError) -> failwith error.describe))

    let private kitIn (root : FolderName) (name : string) : KitFolder = KitFolder (FolderName (Path.Combine(root.value, name)))

    /// Makes a kit in `folder` through `openKitAt` and closes it, answering its kit file.
    let private madeKitAt (folder : KitFolder) (imports : CatalogueImportContext) : KitFile =
        match openKitAt folder ReadWriteKitAccess kitStartClock kitStartAuthor imports with
        | Ok made ->
            match made.library.kit.close () with
            | Ok () -> made.library.kit.kitFile
            | Error error -> failwith error.describe
        | Error error -> failwith error.describe

    /// Mounts the screen over `model0` and `update` as `ScrollExtentTests` mounts a view, shown headless at 900 × 700,
    /// and closes it after `check`. Call it on the headless UI thread.
    let private withKitStartProgram
        (update : KitStartMsg -> KitStartModel -> KitStartModel)
        (model0 : KitStartModel)
        (check : Window -> unit)
        : unit =
        let host = HostWindow()
        Program.mkSimple (fun () -> model0) update kitStartView |> Program.withHost host |> Program.run
        let window = host :> Window
        window.Width <- 900.0
        window.Height <- 700.0
        window.Show()
        Dispatcher.UIThread.RunJobs()

        try
            check window
        finally
            window.Close()

    /// Mounts the screen over `model0` and its own update.
    let private withKitStartScreen (model0 : KitStartModel) (check : Window -> unit) : unit =
        withKitStartProgram updateKitStart model0 check

    /// The control carrying `id` as its AutomationId.
    let private tryFindControl (window : Window) (id : ConstructorUiId) : Control option =
        window.GetVisualDescendants()
        |> Seq.tryPick (function
            | :? Control as control when AutomationProperties.GetAutomationId control = id.value -> Some control
            | _ -> None)

    let private controlOf (window : Window) (id : ConstructorUiId) : Control =
        tryFindControl window id |> Option.defaultWith (fun () -> failwith $"Missing control {id.value}.")

    let private assertAbsent (window : Window) (id : ConstructorUiId) : unit =
        match tryFindControl window id with
        | Some _ -> Assert.Fail $"{id.value} must not be shown."
        | None -> ()

    /// Every text under `control`, in visual-tree order.
    let private textsUnder (control : Control) : string list =
        control.GetVisualDescendants()
        |> Seq.choose (function
            | :? TextBlock as text -> Option.ofObj text.Text
            | _ -> None)
        |> List.ofSeq

    /// The text of the TextBlock carrying `id`.
    let private textOf (window : Window) (id : ConstructorUiId) : string =
        match controlOf window id with
        | :? TextBlock as text -> Option.ofObj text.Text |> Option.defaultValue ""
        | other -> failwith $"{id.value} is a {other.GetType().Name}, not a TextBlock."

    /// The listed rows' kit names, position 0 first.
    let private listedKitNames (window : Window) : string list =
        [ 0 .. 5 ]
        |> List.choose (fun index ->
            let position = ConstructorUiPosition index
            tryFindControl window (ConstructorUiId.kitHistoryRow position)
            |> Option.map (fun row -> textsUnder row |> List.tryHead |> Option.defaultValue ""))

    /// `control`'s box in window coordinates.
    let private boundsIn (window : Window) (control : Control) : Rect =
        match control.TranslatePoint(Point(0.0, 0.0), window) |> Option.ofNullable with
        | Some topLeft -> Rect(topLeft, control.Bounds.Size)
        | None -> failwith "The control has no on-screen position."

    /// Presses the box carrying `id` at its centre, as `LibraryWindowTests.clickOn` does.
    let private pressAt (window : Window) (id : ConstructorUiId) : unit =
        let target = controlOf window id
        target.BringIntoView() |> ignore
        Dispatcher.UIThread.RunJobs()

        match target.TranslatePoint(Point(target.Bounds.Width / 2.0, target.Bounds.Height / 2.0), window) |> Option.ofNullable with
        | Some centre ->
            window.MouseDown(centre, MouseButton.Left, RawInputModifiers.None)
            Dispatcher.UIThread.RunJobs()
            if window.IsVisible then
                window.MouseUp(centre, MouseButton.Left, RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
        | None -> Assert.Fail $"{id.value} has no on-screen position."

    /// Types `text` into the filter box.
    let private setFilter (window : Window) (text : string) : unit =
        match controlOf window ConstructorUiId.kitHistoryFilter with
        | :? TextBox as filterBox ->
            filterBox.Text <- text
            Dispatcher.UIThread.RunJobs()
        | other -> Assert.Fail $"The filter is a {other.GetType().Name}, not a TextBox."

    /// The offer's accepting box and Cancel share a row, Cancel to the right of the accepting box.
    let private assertPairOnOneRow (window : Window) (accept : ConstructorUiId) : unit =
        let accepting = boundsIn window (controlOf window accept)
        let cancelling = boundsIn window (controlOf window ConstructorUiId.cancelKitStartOfferButton)
        Assert.Equal(accepting.Top, cancelling.Top)
        Assert.True(cancelling.Left > accepting.Right, $"Cancel starts at {cancelling.Left}, inside {accept.value}, which ends at {accepting.Right}.")

    /// The running extraction's progress row: its bar, its count and its Cancel.
    let private zipProgressIds : ConstructorUiId list =
        [ ConstructorUiId.openKitZipProgressBar; ConstructorUiId.openKitZipProgressText; ConstructorUiId.openKitZipCancelButton ]

    /// Which of Open kit from zip's two dialogs was asked, in the order asked.
    type private OpenKitZipDialog =
        | ZipDialogAsked
        | FolderDialogAsked

    [<Fact>]
    let ``the kit start screen UiIds are the stable intent-named ids`` () : unit =
        let first = ConstructorUiPosition 0
        let fourth = ConstructorUiPosition 3

        Assert.Equal("NewKitButton", ConstructorUiId.newKitButton.value)
        Assert.Equal("OpenKitButton", ConstructorUiId.openKitButton.value)
        Assert.Equal("KitStartStatusText", ConstructorUiId.kitStartStatusText.value)
        Assert.Equal("KitStartOfferText", ConstructorUiId.kitStartOfferText.value)
        Assert.Equal("MakeThisAKitButton", ConstructorUiId.makeThisAKitButton.value)
        Assert.Equal("OpenKitReadOnlyButton", ConstructorUiId.openKitReadOnlyButton.value)
        Assert.Equal("CancelKitStartOfferButton", ConstructorUiId.cancelKitStartOfferButton.value)
        Assert.Equal("KitHistoryFilter", ConstructorUiId.kitHistoryFilter.value)
        Assert.Equal("KitHistoryList", ConstructorUiId.kitHistoryList.value)

        Assert.Equal("KitHistoryRow_0", (ConstructorUiId.kitHistoryRow first).value)
        Assert.Equal("KitHistoryRow_3", (ConstructorUiId.kitHistoryRow fourth).value)
        Assert.Equal("KitHistoryStarToggle_0", (ConstructorUiId.kitHistoryStarToggle first).value)
        Assert.Equal("KitHistoryStarToggle_3", (ConstructorUiId.kitHistoryStarToggle fourth).value)
        Assert.Equal("KitHistoryLocateButton_0", (ConstructorUiId.kitHistoryLocateButton first).value)
        Assert.Equal("KitHistoryLocateButton_3", (ConstructorUiId.kitHistoryLocateButton fourth).value)
        Assert.Equal("KitHistoryRemoveButton_0", (ConstructorUiId.kitHistoryRemoveButton first).value)
        Assert.Equal("KitHistoryRemoveButton_3", (ConstructorUiId.kitHistoryRemoveButton fourth).value)

    [<Fact>]
    let ``the Open kit from zip UiIds are the stable intent-named ids`` () : unit =
        Assert.Equal("OpenKitZipButton", ConstructorUiId.openKitZipButton.value)
        Assert.Equal("OpenKitZipProgressBar", ConstructorUiId.openKitZipProgressBar.value)
        Assert.Equal("OpenKitZipProgressText", ConstructorUiId.openKitZipProgressText.value)
        Assert.Equal("OpenKitZipCancelButton", ConstructorUiId.openKitZipCancelButton.value)

    [<Fact>]
    let ``the history rows show name, folder and last open in display order, each with its star`` () : unit =
        HeadlessSession.run (fun () ->
            let shown = ResizeArray<OpenedKit> ()
            let opened = initKitStart (contextOver (storedHistory ()) cancellingPicker shown) ForwardConstructor

            withKitStartScreen opened (fun window ->
                Assert.Equal<string list>([ "New kit…" ], textsUnder (controlOf window ConstructorUiId.newKitButton))
                Assert.Equal<string list>([ "Open kit…" ], textsUnder (controlOf window ConstructorUiId.openKitButton))

                match controlOf window ConstructorUiId.kitHistoryFilter with
                | :? TextBox as filter -> Assert.Equal("Filter kits by name or folder", filter.PlaceholderText)
                | other -> Assert.Fail $"The filter is a {other.GetType().Name}, not a TextBox."

                assertAbsent window ConstructorUiId.kitStartStatusText
                assertAbsent window ConstructorUiId.kitStartOfferText
                controlOf window ConstructorUiId.kitHistoryList |> ignore

                let expected : (string list * string) list =
                    [
                        ([ "Kit d"; "C:/kits/d"; "Last opened 2026-11-03 12:07 UTC" ], "★")
                        ([ "Kit b"; "C:/kits/b"; "Last opened 2026-11-03 12:01 UTC" ], "★")
                        ([ "Kit c"; "C:/kits/c"; "Last opened 2026-11-03 12:09 UTC" ], "☆")
                        ([ "Kit a"; "C:/kits/a"; "Last opened 2026-11-03 12:05 UTC" ], "☆")
                        ([ "Kit e"; "C:/kits/e"; "Last opened 2026-11-03 12:05 UTC" ], "☆")
                    ]

                expected
                |> List.iteri (fun index (texts, star) ->
                    let position = ConstructorUiPosition index
                    Assert.Equal<string list>(texts, textsUnder (controlOf window (ConstructorUiId.kitHistoryRow position)))
                    Assert.Equal<string list>([ star ], textsUnder (controlOf window (ConstructorUiId.kitHistoryStarToggle position))))

                Assert.Equal("Last opened 2026-11-03 12:07 UTC", kitD.lastOpenedCaption.value)
                Assert.Equal<string list>([ "Kit d"; "Kit b"; "Kit c"; "Kit a"; "Kit e" ], listedKitNames window))

            let refused = (KitHistoryWriteErr "C:/prefs: the disk is full").notSavedCaption

            withKitStartScreen { opened with status = Some refused } (fun window ->
                Assert.Equal(refused.value, textOf window ConstructorUiId.kitStartStatusText)
                Assert.Equal<string list>([ "Kit d"; "Kit b"; "Kit c"; "Kit a"; "Kit e" ], listedKitNames window))

            Assert.Empty shown)

    [<Fact>]
    let ``a missing folder's row is greyed with Locate and Remove from history, and a present row offers neither`` () : unit =
        HeadlessSession.run (fun () ->
            let shown = ResizeArray<OpenedKit> ()
            let opened = initKitStart (contextOver (storedHistory ()) cancellingPicker shown) ForwardConstructor

            withKitStartScreen opened (fun window ->
                for index in [ 1; 4 ] do
                    let position = ConstructorUiPosition index
                    let row = controlOf window (ConstructorUiId.kitHistoryRow position)
                    Assert.Equal(0.5, row.Opacity)

                    let verbs = [ (ConstructorUiId.kitHistoryLocateButton position, "Locate…"); (ConstructorUiId.kitHistoryRemoveButton position, "Remove from history") ]

                    for (verb, caption) in verbs do
                        let button = controlOf window verb
                        Assert.Equal<string list>([ caption ], textsUnder button)
                        Assert.Equal(1.0, button.Opacity)
                        let insideRow = row.GetVisualDescendants() |> Seq.exists (fun visual -> obj.ReferenceEquals(visual, button))
                        Assert.False(insideRow, $"{verb.value} sits inside the greyed row.")

                for index in [ 0; 2; 3 ] do
                    let position = ConstructorUiPosition index
                    Assert.Equal(1.0, (controlOf window (ConstructorUiId.kitHistoryRow position)).Opacity)
                    assertAbsent window (ConstructorUiId.kitHistoryLocateButton position)
                    assertAbsent window (ConstructorUiId.kitHistoryRemoveButton position))

            Assert.Empty shown)

    [<Fact>]
    let ``each offer shows its text with its two buttons on one row, Cancel to the right`` () : unit =
        HeadlessSession.run (fun () ->
            let shown = ResizeArray<OpenedKit> ()
            let opened = initKitStart (contextOver (storedHistory ()) cancellingPicker shown) ForwardConstructor
            let notes = KitFolder (FolderName "C:/kits/notes")

            withKitStartScreen { opened with offer = MakeKitOffer notes } (fun window ->
                Assert.Equal("C:/kits/notes holds no kit.", notes.notAKitCaption.value)
                Assert.Equal(notes.notAKitCaption.value, textOf window ConstructorUiId.kitStartOfferText)
                Assert.Equal<string list>([ "Make this a kit" ], textsUnder (controlOf window ConstructorUiId.makeThisAKitButton))
                Assert.Equal<string list>([ "Cancel" ], textsUnder (controlOf window ConstructorUiId.cancelKitStartOfferButton))
                assertAbsent window ConstructorUiId.openKitReadOnlyButton
                assertPairOnOneRow window ConstructorUiId.makeThisAKitButton)

            withKitStartScreen { opened with offer = ReadOnlyKitOffer (notes, lockHolder) } (fun window ->
                Assert.Equal(lockHolder.openElsewhereCaption.value, textOf window ConstructorUiId.kitStartOfferText)
                Assert.Equal<string list>([ "Open read-only" ], textsUnder (controlOf window ConstructorUiId.openKitReadOnlyButton))
                Assert.Equal<string list>([ "Cancel" ], textsUnder (controlOf window ConstructorUiId.cancelKitStartOfferButton))
                assertAbsent window ConstructorUiId.makeThisAKitButton
                assertPairOnOneRow window ConstructorUiId.openKitReadOnlyButton)

            Assert.Empty shown)

    [<Fact>]
    let ``typing into the filter box narrows the list`` () : unit =
        HeadlessSession.run (fun () ->
            let shown = ResizeArray<OpenedKit> ()
            let store = storedHistory ()
            let opened = initKitStart (contextOver store cancellingPicker shown) ForwardConstructor

            withKitStartScreen opened (fun window ->
                setFilter window "kit c"
                Assert.Equal<string list>([ "Kit c" ], listedKitNames window)

                setFilter window ""
                Assert.Equal<string list>([ "Kit d"; "Kit b"; "Kit c"; "Kit a"; "Kit e" ], listedKitNames window))

            Assert.Equal<KitHistory>(stored, store.readHistory ())
            Assert.Empty shown)

    [<Fact>]
    let ``a running extraction shows its progress bar, its count and Cancel, and hides Open kit from zip`` () : unit =
        HeadlessSession.run (fun () ->
            let shown = ResizeArray<OpenedKit> ()
            let idle = initKitStart (contextOver (storedHistory ()) cancellingPicker shown) ForwardConstructor
            let handle : KitArchiveJobHandle =
                {
                    cancelArchiveJob = ignore
                }
            let extracted : KitArchiveProgress =
                {
                    filesDone = KitArchiveFileCount 3
                    filesTotal = KitArchiveFileCount 4
                }
            let running =
                {
                    idle with zipJob = (idle.zipJob.started handle).progressed (extracted.jobProgress (JobRunner.JobElapsed TimeSpan.Zero))
                }

            withKitStartScreen running (fun window ->
                Assert.StartsWith("3 / 4", textOf window ConstructorUiId.openKitZipProgressText)
                let bar = Assert.IsType<ProgressBar>(controlOf window ConstructorUiId.openKitZipProgressBar)
                Assert.Equal(0.75, bar.Value, 12)
                Assert.False(bar.IsIndeterminate, "an extraction that told its files must not show an indeterminate bar")
                Assert.Equal<string list>([ "Cancel" ], textsUnder (controlOf window ConstructorUiId.openKitZipCancelButton))
                assertAbsent window ConstructorUiId.openKitZipButton

                // The row sits directly under New kit… and Open kit…, above the filter box.
                let firstRow = boundsIn window (controlOf window ConstructorUiId.openKitButton)
                let progressBar = boundsIn window bar
                let filterBox = boundsIn window (controlOf window ConstructorUiId.kitHistoryFilter)
                Assert.True(progressBar.Top >= firstRow.Bottom, $"The bar starts at {progressBar.Top}, above the first row's end at {firstRow.Bottom}.")
                Assert.True(filterBox.Top > progressBar.Bottom, $"The filter box starts at {filterBox.Top}, above the bar's end at {progressBar.Bottom}.")
                Assert.Equal<string list>([ "Kit d"; "Kit b"; "Kit c"; "Kit a"; "Kit e" ], listedKitNames window))

            withKitStartScreen idle (fun window ->
                Assert.Equal<string list>([ "Open kit from zip…" ], textsUnder (controlOf window ConstructorUiId.openKitZipButton))
                for id in zipProgressIds do
                    assertAbsent window id)

            Assert.Empty shown)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``pressing a row's star, its Remove from history and a present row sends StarKit, ForgetKit and OpenKit`` () : unit =
        HeadlessSession.run (fun () ->
            withKitStartRoot (fun root shown ->
                let store = storedHistory ()
                let opened = initKitStart (contextOver store cancellingPicker shown) ForwardConstructor
                let starredA = { kitA with star = StarredKit }

                withKitStartScreen opened (fun window ->
                    let fourth = ConstructorUiPosition 3
                    Assert.Equal("Kit a", listedKitNames window |> List.item fourth.value)
                    pressAt window (ConstructorUiId.kitHistoryStarToggle fourth)
                    Assert.Equal<KitHistoryEntry list>([ starredA; kitB; kitC; kitD; kitE ], (store.readHistory ()).entries)

                    let fifth = ConstructorUiPosition 4
                    Assert.Equal<string list>([ "Kit d"; "Kit a"; "Kit b"; "Kit c"; "Kit e" ], listedKitNames window)
                    pressAt window (ConstructorUiId.kitHistoryRemoveButton fifth)
                    Assert.Equal<KitHistoryEntry list>([ starredA; kitB; kitC; kitD ], (store.readHistory ()).entries)
                    Assert.Equal<string list>([ "Kit d"; "Kit a"; "Kit b"; "Kit c" ], listedKitNames window))

                Assert.Empty shown

                let folder = kitIn root "Bench A"
                let context = contextOver (historyOver [ folder ] KitHistory.empty) cancellingPicker shown
                let made = madeKitAt folder context.imports
                let entry : KitHistoryEntry =
                    {
                        folder = folder
                        kitId = made.id
                        name = made.name
                        lastOpened = ClockTime firstOpen
                        star = UnstarredKit
                    }

                match context.kitHistory.writeHistory { KitHistory.empty with entries = [ entry ] } with
                | Ok () -> ()
                | Error error -> failwith error.describe

                withKitStartScreen (initKitStart context ForwardConstructor) (fun window ->
                    let first = ConstructorUiPosition 0
                    pressAt window (ConstructorUiId.kitHistoryRow first)
                    let kit = Assert.Single<OpenedKit>(shown)
                    Assert.Equal<KitId>(made.id, kit.library.kit.kitFile.id)
                    Assert.Equal<KitAccess>(ReadWriteKitAccess, kit.library.kit.access))))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``New kit and Open kit open the folder the picker answers`` () : unit =
        HeadlessSession.run (fun () ->
            withKitStartRoot (fun root shown ->
                let fresh = kitIn root "Optics QZ"
                let creating = contextOver (historyOver [ fresh ] KitHistory.empty) (TestAppScope.kitPickerOf fresh) shown

                withKitStartScreen (initKitStart creating ForwardConstructor) (fun window ->
                    pressAt window ConstructorUiId.newKitButton
                    let kit = Assert.Single<OpenedKit>(shown)
                    Assert.Equal<KitName>(KitName "Optics QZ", kit.library.kit.kitFile.name)
                    Assert.Equal<KitAccess>(ReadWriteKitAccess, kit.library.kit.access)
                    Assert.True(File.Exists(Path.Combine(fresh.value.value, ".kit-json"))))

                let notes = kitIn root "Lab notes"
                let notesFile = Path.Combine(notes.value.value, "notes.txt")
                Directory.CreateDirectory(notes.value.value) |> ignore
                File.WriteAllText(notesFile, "not a kit")
                let opening = contextOver (historyOver [ notes ] KitHistory.empty) (TestAppScope.kitPickerOf notes) shown

                withKitStartScreen (initKitStart opening ForwardConstructor) (fun window ->
                    assertAbsent window ConstructorUiId.kitStartOfferText
                    pressAt window ConstructorUiId.openKitButton
                    Assert.Single<OpenedKit>(shown) |> ignore
                    Assert.Equal(notes.notAKitCaption.value, textOf window ConstructorUiId.kitStartOfferText)
                    controlOf window ConstructorUiId.makeThisAKitButton |> ignore
                    Assert.Equal<string list>([ notesFile ], Directory.GetFileSystemEntries(notes.value.value) |> List.ofArray))))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``Open kit from zip asks for a zip, then a folder, and starts one extraction of that zip into that folder`` () : unit =
        HeadlessSession.run (fun () ->
            withKitStartRoot (fun root shown ->
                let zip = FileName (Path.Combine(root.value, "exports", "Bench A.zip"))
                let target = kitIn root "received"
                let asked = ResizeArray<OpenKitZipDialog> ()
                let jobs = ResizeArray<KitArchiveObserver -> Result<KitArchiveOutcome, KitArchiveError>> ()
                let cancels = ResizeArray<unit> ()
                let sent = ResizeArray<KitStartMsg> ()

                let zipPicker : KitZipPicker =
                    {
                        KitZipPicker.none with
                            pickZipToOpen =
                                fun () ->
                                    async {
                                        asked.Add ZipDialogAsked
                                        return Some zip
                                    }
                    }
                let folderPicker : KitFolderPicker =
                    {
                        pickKitFolder =
                            fun () ->
                                async {
                                    asked.Add FolderDialogAsked
                                    return Some target
                                }
                    }
                // Keeps each job unrun, so nothing is extracted, and answers a handle that records each cancel.
                let deferred : KitArchiveJobHost =
                    {
                        startArchiveJob =
                            fun job ->
                                jobs.Add job
                                { cancelArchiveJob = fun () -> cancels.Add () }
                    }
                let context =
                    {
                        contextOver (storedHistory ()) folderPicker shown with
                            zipPicker = zipPicker
                            archiveJobs = deferred
                    }
                let recording (msg : KitStartMsg) (m : KitStartModel) : KitStartModel =
                    sent.Add msg
                    updateKitStart msg m

                withKitStartProgram recording (initKitStart context ForwardConstructor) (fun window ->
                    for id in zipProgressIds do
                        assertAbsent window id

                    pressAt window ConstructorUiId.openKitZipButton
                    Assert.Equal<OpenKitZipDialog list>([ ZipDialogAsked; FolderDialogAsked ], List.ofSeq asked)
                    Assert.Single(jobs) |> ignore

                    match List.ofSeq sent with
                    | [ OpenKitZip opening ] ->
                        Assert.Equal<FileName>(zip, opening.zip)
                        Assert.Equal<KitFolder>(target, opening.target)
                    | messages -> Assert.Fail $"Open kit from zip sent {messages.Length} message(s), not one OpenKitZip."

                    let progressText = controlOf window ConstructorUiId.openKitZipProgressText
                    Assert.True(progressText.IsEffectivelyVisible, "a started extraction must show its progress")
                    Assert.Equal("Starting…", textOf window ConstructorUiId.openKitZipProgressText)
                    assertAbsent window ConstructorUiId.openKitZipButton
                    Assert.Empty(cancels)

                    pressAt window ConstructorUiId.openKitZipCancelButton
                    Assert.Equal(1, cancels.Count)
                    Assert.Single(jobs) |> ignore)

                // A cancel of either dialog sends nothing: a cancelled zip dialog does not ask for the folder.
                asked.Clear ()
                sent.Clear ()

                withKitStartProgram recording (initKitStart { context with zipPicker = KitZipPicker.none } ForwardConstructor) (fun window ->
                    pressAt window ConstructorUiId.openKitZipButton
                    Assert.Empty(asked)
                    Assert.Empty(sent))

                withKitStartProgram recording (initKitStart { context with picker = cancellingPicker } ForwardConstructor) (fun window ->
                    pressAt window ConstructorUiId.openKitZipButton
                    Assert.Equal<OpenKitZipDialog list>([ ZipDialogAsked ], List.ofSeq asked)
                    Assert.Empty(sent)
                    for id in zipProgressIds do
                        assertAbsent window id)

                Assert.Single(jobs) |> ignore
                Assert.Empty shown
                Assert.False(Directory.Exists target.value.value, "a kept job extracts nothing")))

    /// The window's app scope: a fresh database scope whose kit history is a fresh in-memory one.
    let private windowScope () : AppContext =
        {
            TestAppScope.freshAppContext (FolderName "KitStartWindowTests") with
                kitHistory = KitHistoryProxy.createInMemory []
        }

    /// Runs a fact over fresh kit folders and a `showConstructor` record of every mode and kit it is given. At its end
    /// every recorded kit is closed, then every fresh folder on disk is deleted; a failed close fails the fact.
    let private withKitStartWindows (run : (unit -> KitFolder) -> ResizeArray<ConstructorMode * OpenedKit> -> unit) : unit =
        let folders = ResizeArray<KitFolder> ()
        let shown = ResizeArray<ConstructorMode * OpenedKit> ()

        let freshFolder () : KitFolder =
            let folder = TestAppScope.freshKitFolder (FolderName "KitStartWindowTests")
            folders.Add folder
            folder

        try
            run freshFolder shown
        finally
            let closes = shown |> Seq.map (fun (_, kit) -> kit.library.kit.close ()) |> List.ofSeq
            for folder in folders do
                if Directory.Exists folder.value.value then Directory.Delete(folder.value.value, true)
            closes |> List.iter (Result.defaultWith (fun (error : KitError) -> failwith error.describe))

    [<Fact>]
    let ``the start screen's title names its mode and it carries its stable id`` () : unit =
        HeadlessSession.run (fun () ->
            withKitStartWindows (fun freshFolder shown ->
                Assert.Equal("KitStartWindow", ConstructorUiId.kitStartWindow.value)
                let titles = [ (ForwardConstructor, "Open a kit — Main"); (InverseConstructor, "Open a kit — Inverse") ]

                for (mode, title) in titles do
                    let picker = TestAppScope.kitPickerOf (freshFolder ())
                    let window = KitStartWindow(windowScope (), mode, (fun _ -> picker), (fun _ -> KitZipPicker.none), (fun shownMode kit -> shown.Add (shownMode, kit)))
                    window.Show()
                    Dispatcher.UIThread.RunJobs()

                    try
                        Assert.Equal(title, window.Title)
                        Assert.Equal(ConstructorUiId.kitStartWindow.value, window.Name)
                        Assert.Equal(ConstructorUiId.kitStartWindow.value, AutomationProperties.GetAutomationId window)
                    finally
                        window.Close()

                Assert.Empty shown))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: New kit opens the new kit in the constructor of the screen's mode, bumps the history and closes the screen`` () : unit =
        HeadlessSession.run (fun () ->
            withKitStartWindows (fun freshFolder shown ->
                for mode in [ ForwardConstructor; InverseConstructor ] do
                    let folder = freshFolder ()
                    let context = windowScope ()
                    let window = KitStartWindow(context, mode, (fun _ -> TestAppScope.kitPickerOf folder), (fun _ -> KitZipPicker.none), (fun shownMode kit -> shown.Add (shownMode, kit)))
                    let closings = ref 0
                    window.Closed.Add(fun _ -> closings.Value <- closings.Value + 1)
                    window.Show()
                    Dispatcher.UIThread.RunJobs()
                    let before = shown.Count

                    try
                        pressAt window ConstructorUiId.newKitButton
                    finally
                        if window.IsVisible then window.Close()

                    Assert.Equal(before + 1, shown.Count)
                    let shownMode, kit = shown[before]
                    Assert.Equal<ConstructorMode>(mode, shownMode)
                    Assert.Equal<KitAccess>(ReadWriteKitAccess, kit.library.kit.access)
                    Assert.Equal<KitName>(KitName (Path.GetFileName folder.value.value), kit.library.kit.kitFile.name)
                    Assert.True(File.Exists(Path.Combine(folder.value.value, ".kit-json")))
                    Assert.Equal(1, closings.Value)
                    Assert.False(window.IsVisible, "The start screen stays open after New kit.")
                    Assert.Equal<KitFolder list>([ folder ], (context.kitHistory.readHistory ()).entries |> List.map _.folder)))

    /// The experiment `E1` the sent kit holds (spec 0050 S6).
    let private sentExperiment : Experiments.Experiment =
        {
            id = Experiments.ExperimentId 1
            setup = { name = SetupName "Dark"; revision = SetupRevision 1 }
            varied = None
            measurement = Experiments.CaptureTransmitted
            range = Experiments.VariableRange.forVariable Experiments.VaryR1
            dataFileOpt = None
            sourceWaveLengthOpt = None
        }

    /// A sent kit's kit file, and the zip it was sent into (spec 0050 S6).
    type private SentKitZip =
        {
            sentKitFile : KitFile
            sentZip : FileName
        }

    /// Makes `Optics QZ` under `root` through `openKitAt`, gives it `E1` and closes it, then sends it through the real
    /// archive into `<root>/exports/Optics QZ.zip`, as Send kit… writes it (spec 0050 S6).
    let private sentKitZipUnder (root : FolderName) (imports : CatalogueImportContext) : SentKitZip =
        let folder = kitIn root "Optics QZ"

        let kitFile =
            match openKitAt folder ReadWriteKitAccess kitStartClock kitStartAuthor imports with
            | Ok made ->
                let written = made.library.kit.proxy.writeExperiment sentExperiment
                let closed = made.library.kit.close ()

                match written |> Result.bind (fun () -> closed) with
                | Ok () -> made.library.kit.kitFile
                | Error error -> failwith error.describe
            | Error error -> failwith error.describe

        let exports = root.combine (FolderName "exports")
        Directory.CreateDirectory exports.value |> ignore
        let zip = kitFile.name.zipFileName.combine exports

        match KitArchiveProxy.create().sendKit kitFile folder zip KitArchiveObserver.unobserved with
        | Ok (KitArchiveFinished _) -> { sentKitFile = kitFile; sentZip = zip }
        | Ok KitArchiveCancelled -> failwith "the kit's send was cancelled"
        | Error error -> failwith error.describe

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: Open kit from zip extracts a sent kit and opens it in the constructor of the screen's mode`` () : unit =
        HeadlessSession.run (fun () ->
            withKitStartRoot (fun root shown ->
                let context = windowScope ()
                let sent = sentKitZipUnder root context.catalogueImports
                let target = KitFolder (FolderName (Path.Combine(root.value, "received", "k")))
                let modes = ResizeArray<ConstructorMode> ()
                let zipPicker : KitZipPicker =
                    {
                        KitZipPicker.none with
                            pickZipToOpen = fun () -> async { return Some sent.sentZip }
                    }
                let showConstructor (shownMode : ConstructorMode) (kit : OpenedKit) : unit =
                    modes.Add shownMode
                    shown.Add kit
                let window = KitStartWindow(context, ForwardConstructor, (fun _ -> TestAppScope.kitPickerOf target), (fun _ -> zipPicker), showConstructor)
                let closings = ref 0
                window.Closed.Add(fun _ -> closings.Value <- closings.Value + 1)
                window.Show()
                Dispatcher.UIThread.RunJobs()
                Assert.False(Directory.Exists target.value.value)

                try
                    pressAt window ConstructorUiId.openKitZipButton
                    // The real host extracts on a worker and posts each file and the end to this thread (spec 0050 S6).
                    let waited = Diagnostics.Stopwatch.StartNew ()

                    while shown.Count = 0 && waited.Elapsed < TimeSpan.FromSeconds 10.0 do
                        Dispatcher.UIThread.RunJobs()
                finally
                    if window.IsVisible then window.Close()

                Assert.Equal<ConstructorMode list>([ ForwardConstructor ], List.ofSeq modes)
                let kit = Assert.Single<OpenedKit>(shown)
                Assert.Equal<KitFolder>(target, kit.folder)
                Assert.Equal<KitAccess>(ReadWriteKitAccess, kit.library.kit.access)
                Assert.Equal<KitFile>(sent.sentKitFile, kit.library.kit.kitFile)
                Assert.Equal<Experiments.Experiment list>([ sentExperiment ], kit.experiments)

                let sentNames =
                    using (ZipFile.OpenRead sent.sentZip.value) (fun archive -> archive.Entries |> Seq.map _.FullName |> List.ofSeq)

                Assert.Contains<string>(".kit-json", sentNames)

                for name in sentNames do
                    let received = Path.Combine(target.value.value, name)
                    let isReceived = if name.EndsWith '/' then Directory.Exists received else File.Exists received
                    Assert.True(isReceived, $"{name} was sent, but the received folder does not hold it.")

                Assert.Equal(1, closings.Value)
                Assert.False(window.IsVisible, "The start screen stays open after Open kit from zip.")
                Assert.Equal<KitFolder list>([ target ], (context.kitHistory.readHistory ()).entries |> List.map _.folder)))
