namespace OpticalConstructor.Ui.Tests

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
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryBackup
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.EntryFileJson
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitHistory
open OpticalConstructor.Controls
open OpticalConstructor.Tests
open OpticalConstructor.Ui
open OpticalConstructor.Ui.ImportDialogView
open OpticalConstructor.Ui.TableAndElementRotationView

/// Spec 0046 (operator): "if we put a tall control above another, the window must have enough scroll length
/// to accommodate both — a simple arithmetic that must hold", in every window. Proven as a LAW over each
/// window at two small-screen client sizes (800 × 600, and a 125–175 %-scaled laptop's 1000 × 540): every
/// control the window shows can be scrolled so that BOTH its top-left and its bottom-right corner are on
/// screen — nothing is ever clipped out of reach. Plus the two rules that make the law hold on a real
/// screen: a window never opens larger than its screen's working area (`WindowFit`), and a plain wheel over
/// the Main table scrolls the page until the table is fully in view (`wheelTarget`).
module ScrollExtentTests =

    /// The small-screen client sizes the law is checked at.
    let private smallScreens : (float * float) list = [ 800.0, 600.0; 1000.0, 540.0 ]

    let private labelOf (c : Control) : string =
        let text =
            match c with
            | :? TextBlock as t -> t.Text
            | _ -> ""
        $"%s{c.GetType().Name} id=%s{AutomationProperties.GetAutomationId c} name=%s{c.Name} text=%s{text}"

    /// The controls the law speaks about: every visible text, input, button, or named / AutomationId'd
    /// control of OURS (template parts excluded), except the absolutely-positioned drawing inside a canvas
    /// (the table schematic may lie beyond its canvas when zoomed — that is panning, not scrolling) and the
    /// plot's own internals.
    let private subjects (window : Window) : Control list =
        window.GetVisualDescendants()
        |> Seq.choose (function :? Control as c -> Some c | _ -> None)
        |> Seq.filter (fun c ->
            c.IsEffectivelyVisible
            && c.Bounds.Width > 1.0 && c.Bounds.Height > 1.0
            && isNull c.TemplatedParent
            && not (c.GetVisualAncestors() |> Seq.exists (fun a -> a :? Canvas || a.GetType().Name = "AvaPlot"))
            && (match c with
                | :? TextBlock as t -> not (System.String.IsNullOrEmpty t.Text)
                | :? TextBox | :? Button | :? CheckBox | :? ComboBox -> true
                | _ -> not (System.String.IsNullOrEmpty (AutomationProperties.GetAutomationId c)) || not (System.String.IsNullOrEmpty c.Name)))
        |> List.ofSeq

    /// The point `p` of control `c` is on screen: inside the window's client area AND inside every clipping
    /// ancestor (a scroll viewer's viewport clips).
    let private onScreen (window : Window) (c : Control) (p : Point) : bool =
        let within (width : float) (height : float) (q : Point) : bool =
            q.X >= -1.0 && q.Y >= -1.0 && q.X <= width + 1.0 && q.Y <= height + 1.0
        let insideOf (v : Visual) : bool =
            match c.TranslatePoint(p, v) |> Option.ofNullable with
            | Some q -> within v.Bounds.Width v.Bounds.Height q
            | None -> false
        (match c.TranslatePoint(p, window) |> Option.ofNullable with
         | Some q -> within window.ClientSize.Width window.ClientSize.Height q
         | None -> false)
        && (c.GetVisualAncestors() |> Seq.filter (fun a -> a.ClipToBounds) |> Seq.forall insideOf)

    /// Every subject corner that cannot be scrolled on screen (`BringIntoView` scrolls every enclosing viewer).
    let private unreachable (window : Window) : string list =
        subjects window
        |> List.collect (fun c ->
            let w, h = c.Bounds.Width, c.Bounds.Height
            [ Point(0.5, 0.5), "top-left"; Point(w - 0.5, h - 0.5), "bottom-right" ]
            |> List.choose (fun (p, corner) ->
                c.BringIntoView(Rect(p.X - 0.5, p.Y - 0.5, 1.0, 1.0))
                Dispatcher.UIThread.RunJobs()
                match onScreen window c p with
                | true -> None
                | false -> Some $"%s{corner} of %s{labelOf c} %A{c.Bounds}"))

    /// Assert the law for a freshly built window at every small-screen size.
    let private assertReachable (name : string) (makeWindow : unit -> Window) : unit =
        HeadlessSession.run (fun () ->
            for w, h in smallScreens do
                let window = makeWindow ()
                window.Width <- w
                window.Height <- h
                window.Show()
                Dispatcher.UIThread.RunJobs()
                let found = try unreachable window finally window.Close()
                let firstFew = found |> List.truncate 8 |> String.concat "; "
                Assert.True(List.isEmpty found, $"%s{name} at %g{w}×%g{h}: %d{List.length found} unreachable — %s{firstFew}"))

    let private stores () : MaterialProxy * SampleProxy * CategoryProxy * LibraryProxy =
        let samples = BuiltinCatalogue.sampleStore ()
        let materials = BuiltinCatalogue.materialStore (samplesReferencing samples)
        let categories = BuiltinCatalogue.categoryStore (materialsReferencingCategory materials)
        materials, samples, categories, Library.liveLibrary (BuiltinCatalogue.library ()) samples (SourceProxy.createInMemory ())

    /// Hosts `view` over `model0` in a FuncUI window: the workbench's `mainView`, or the kit window over it.
    let private viewWindow
        (view : TableAndElementRotationModel -> (TableAndElementRotationMsg -> unit) -> Avalonia.FuncUI.Types.IView)
        (model0 : TableAndElementRotationModel)
        : Window =
        let window = HostWindow()
        Program.mkSimple (fun () -> model0) updateTableAndElementRotation view |> Program.withHost window |> Program.run
        window :> Window

    let private constructorWindow (model0 : TableAndElementRotationModel) : Window = viewWindow mainView model0

    /// Main with a polarizer whose R1 run is collected AND being edited — the tallest Experiments bay (editor,
    /// chart, description, collection row, export, collection builder) above the table.
    let private filledExperiments () : TableAndElementRotationModel =
        let m = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement Placement.LinearPolarizer)
        let pid = (List.item 2 m.elements).id
        m |> updateTableAndElementRotation (ExpChooseElement pid) |> updateTableAndElementRotation ExpCommit |> updateTableAndElementRotation (ExpChooseElement pid) |> updateTableAndElementRotation (SelectBay BayNames.experiments)

    // ------------------------------ the law, window by window ------------------------------

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``Main: every bay leaves the whole page reachable — the table below a tall bay included`` () =
        for bay in BayNames.allBayNames do
            assertReachable $"Main/%s{bay.value}" (fun () -> constructorWindow ({ SceneTestFixture.mainScene () with selection = ElementSelected (SceneElementIndex 0) } |> updateTableAndElementRotation (SelectBay bay)))
        assertReachable "Main/Experiments (collected + chart)" (fun () -> constructorWindow (filledExperiments ()))

    [<Fact>]
    let ``Inverse: the Experiments bay with its data-file section leaves everything reachable`` () =
        let materials, samples, categories, library = stores ()
        assertReachable "Inverse/Experiments" (fun () ->
            constructorWindow
                { SceneTestFixture.inverseScene library (Experiments.createInMemoryExperiments ()) materials samples categories with
                    ribbon = BayNames.experiments
                    experimentCollection = (filledExperiments ()).experimentCollection })

    [<Fact>]
    let ``Library and Materials windows: the facet rows wrap, nothing is clipped`` () =
        let materials, samples, categories, library = stores ()
        assertReachable "Library" (fun () -> LibraryWindow(library, samples, SourceProxy.createInMemory (), materials, categories, LibraryBackupProxy.createInMemory (), CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy))
        assertReachable "Materials" (fun () -> MaterialsWindow(materials, categories, CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy))

    [<Fact>]
    let ``the import dialog leaves everything reachable`` () : unit =
        let envelopes = (BuiltinCatalogue.catalogueData ()).envelopes
        let decisions =
            envelopes |> List.map (fun envelope ->
                let origin = DroppedFileOrigin envelope.content.fileName
                ImportDecision.decide origin envelope None [])
        let material = envelopes |> List.find (fun envelope -> envelope.content.schema.kind = MaterialEntryKind)
        let collisionContent =
            {
                material.content with
                    id = LibraryEntryId (System.Guid.Parse "31801b01-444f-43c8-8440-000000000108")
                    provenance = UserProvenance
            }
        let collision = collisionContent.``sealed`` |> Result.defaultWith (fun error -> failwith error.describe)
        let firstHeld : HeldEntry =
            {
                id = material.content.id
                kind = material.content.schema.kind
                name = material.content.name
                latest = material.content.version
                lifecycle = ActiveEntry
                provenance = material.content.provenance
                versions =
                    [
                        {
                            version = material.content.version
                            contentHash = material.contentHash
                        }
                    ]
            }
        let secondHeld =
            {
                firstHeld with
                    id = LibraryEntryId (System.Guid.Parse "31801b01-444f-43c8-8440-000000000208")
                    provenance = UserProvenance
            }
        let collisionFile = FileName "collision.material-json"
        let collisionOrigin = DroppedFileOrigin collisionFile
        let collisionDecision = ImportDecision.decide collisionOrigin collision None [ firstHeld; secondHeld ]
        let request = ApplyDroppedEntries (decisions @ [ collisionDecision ])
        assertReachable "ImportDialog" (fun () ->
            let imports = CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy
            ImportDialogWindow(imports, request, ignore))

    [<Fact>]
    let ``Material, Sample, Category and Source editors leave everything reachable`` () =
        let materials, samples, categories, _ = stores ()
        assertReachable "MaterialEditor" (fun () -> MaterialEditorWindow(materials, MaterialEditorView.NewMaterial (newMaterialId ()), BuiltinCatalogue.categoryStore (fun _ -> [])))
        assertReachable "SampleEditor" (fun () -> SampleEditorWindow(materials, samples, categories, CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy, SampleEditorView.NewSeededMultilayer (newSampleId ())))
        assertReachable "CategoryEditor" (fun () -> CategoryEditorWindow(categories))
        assertReachable "SourceEditor" (fun () -> LightSourceEditorWindow(SourceProxy.createInMemory (), LightSourceEditorView.NewSource (Placement.ElementValueId "src-x")))

    [<Fact>]
    let ``the chart and solver-handoff windows leave everything reachable`` () =
        let _, _, _, library = stores ()
        let chart : ExperimentChart.ExperimentChart =
            {
                series =
                    [
                        {
                            ExperimentControls.ChartSeries.name = ControlCaption "Ψ"
                            points = [ { ExperimentControls.ChartPoint.x = 0.0; y = 10.0 }; { x = 1.0; y = 12.0 } ]
                        }
                    ]
                xLabel = ControlCaption "x"
                yLabel = ControlCaption "y"
                title = ControlCaption "t"
                description = ControlCaption "d"
                xAxisKind = ExperimentChart.AngularXAxis
            }
        let noData : ExperimentData.ExperimentDataProxy =
            {
                attachDataFile = fun _ -> Error (Kit.KitIoErr "no data files in the layout fixture")
                attachmentStatus = fun _ -> Ok Kit.AttachedDataMissing
                tryLoadIntensity = fun _ -> Error (MeasuredData.MalformedDataFileErr "none")
                tryLoadEllipsometric = fun _ -> Error (MeasuredData.MalformedDataFileErr "none")
            }
        assertReachable "Chart" (fun () -> ChartWindow(chart))
        assertReachable "Handoff" (fun () ->
            SolverHandoffWindow(library, noData, { name = ExperimentCollectionStore.CollectionName "run"; experiments = (filledExperiments ()).experimentCollection.experiments }))

    /// Forty kits in memory, every fifth folder missing and every seventh kit starred, so the start screen's list is
    /// longer than the screen (spec 0050 S5.2).
    let private fortyKitHistory () : KitHistoryProxy =
        let firstOpen = System.DateTime(2026, 11, 3, 12, 0, 0, System.DateTimeKind.Utc)
        let folderOf (index : int) : KitFolder = KitFolder (FolderName $"C:/kits/bench-%02d{index}")
        let entryOf (index : int) : KitHistoryEntry =
            {
                folder = folderOf index
                kitId = KitId (System.Guid.NewGuid ())
                name = KitName $"Bench kit %02d{index}"
                lastOpened = ClockTime (firstOpen.AddMinutes (float index))
                star =
                    match index % 7 with
                    | 0 -> StarredKit
                    | _ -> UnstarredKit
            }
        let indices = [ 1 .. 40 ]
        let present = indices |> List.filter (fun index -> index % 5 <> 0) |> List.map folderOf
        let store = KitHistoryProxy.createInMemory present
        store.writeHistory { KitHistory.empty with entries = indices |> List.map entryOf }
        |> Result.defaultWith (fun error -> failwith error.describe)
        store

    [<Fact>]
    let ``the kit start screen leaves everything reachable`` () : unit =
        let owner = FolderName "ScrollExtentTests"
        let context = { TestAppScope.freshAppContext owner with kitHistory = fortyKitHistory () }
        let picker = TestAppScope.kitPickerOf (TestAppScope.freshKitFolder owner)
        assertReachable "KitStart" (fun () ->
            KitStartWindow(context, ForwardConstructor, (fun _ -> picker), (fun _ _ -> ())))

    /// The tallest Experiments bay under the kit bar at its tallest: a read-only kit with a folder, so its caption is
    /// longest and Show in Explorer is offered, three files offered for reload and a status (spec 0050 S5.3).
    let private tallestKitBar () : TableAndElementRotationModel =
        let filled = filledExperiments ()
        let readOnlyKit =
            {
                filled.kit with access = ReadOnlyKitAccess
            }
        {
            filled with
                kit = readOnlyKit
                kitFolderOpt = Some (KitFolder (FolderName "C:/kits/ScrollExtentTests/a bench kit with a long folder name"))
                kitChanges = [ FileName ".kit-json"; FileName "E1.experiment-json"; FileName "E2.experiment-json" ]
                kitStatus = Some (ControlCaption "The kit is open read-only: nothing is saved.")
        }

    [<Fact>]
    let ``the kit window: the kit bar and the page stay reachable`` () : unit =
        assertReachable "KitWindow" (fun () -> viewWindow KitWindowView.kitWindowView (tallestKitBar ()))

    // ------------------------------ a window never opens larger than its screen ------------------------------

    [<Fact>]
    let ``fitWithin shrinks a too-large window to the working area and moves it inside`` () =
        // A 1080p laptop at 150 %: 1920 × 1032 px of working area = 1280 × 688 DIP; the frame adds 16 × 39 DIP.
        let work = PixelRect(0, 0, 1920, 1032)
        let decoration = Size(16.0, 39.0)
        let fitted, at = WindowFit.fitWithin work (WindowFit.ScreenScaling 1.5) decoration (PixelPoint(300, 200)) (Size(1400.0, 1000.0))
        Assert.Equal(Size(1264.0, 649.0), fitted)
        Assert.Equal(PixelPoint(0, 0), at)
        // A window that fits keeps its size and its position.
        let small, same = WindowFit.fitWithin work (WindowFit.ScreenScaling 1.5) decoration (PixelPoint(300, 200)) (Size(620.0, 280.0))
        Assert.Equal(Size(620.0, 280.0), small)
        Assert.Equal(PixelPoint(300, 200), same)
        // One that fits but hangs off the bottom-right is moved back inside, unshrunk.
        let _, moved = WindowFit.fitWithin work (WindowFit.ScreenScaling 1.0) decoration (PixelPoint(1800, 1000)) (Size(620.0, 280.0))
        Assert.Equal(PixelPoint(1920 - 636, 1032 - 319), moved)
        // A second monitor's work area to the right is honoured (positions are virtual-screen pixels).
        let _, onSecond = WindowFit.fitWithin (PixelRect(1920, 0, 1280, 1024)) (WindowFit.ScreenScaling 1.0) decoration (PixelPoint(100, 100)) (Size(620.0, 280.0))
        Assert.Equal(PixelPoint(1920, 100), onSecond)

    [<Fact>]
    let ``a window opened larger than the screen is fitted to it`` () =
        HeadlessSession.run (fun () ->
            let window = Window(Width = 4000.0, Height = 3000.0)
            WindowFit.keepOnScreen window
            window.Show()
            Dispatcher.UIThread.RunJobs()
            try
                match Option.ofObj window.Screens |> Option.bind (fun s -> Option.ofObj s.Primary) with
                | Some screen ->
                    let area = screen.WorkingArea
                    Assert.True(window.ClientSize.Width <= float area.Width / screen.Scaling + 0.5, $"width %g{window.ClientSize.Width}")
                    Assert.True(window.ClientSize.Height <= float area.Height / screen.Scaling + 0.5, $"height %g{window.ClientSize.Height}")
                | None -> Assert.Fail "the headless host reports no screen"
            finally
                window.Close())

    // ------------------------------ the wheel over the Main table ------------------------------

    [<Fact>]
    let ``wheelTarget: the page scrolls first until the table is in view; a modifier is always the table's`` () =
        let page (top : float) (offset : float) : PageScroll option =
            Some { tableTop = PagePixels top; tableBottom = PagePixels (top + 560.0); viewportHeight = PagePixels 600.0; offset = PagePixels offset; maxOffset = PagePixels 400.0 }
        Assert.Equal(WheelToPage, wheelTarget Set.empty (page 458.0 0.0) (WheelNotches -1))          // table cut off, page can go down
        Assert.Equal(WheelToTable, wheelTarget Set.empty (page 20.0 400.0) (WheelNotches -1))        // table fully in view
        Assert.Equal(WheelToTable, wheelTarget Set.empty (page 458.0 0.0) (WheelNotches 1))          // cut off, but the page is at its top
        Assert.Equal(WheelToPage, wheelTarget Set.empty (page -200.0 300.0) (WheelNotches 1))        // cut off at the top, page can go up
        Assert.Equal(WheelToTable, wheelTarget (Set.ofList [ ModCtrl ]) (page 458.0 0.0) (WheelNotches -1))
        Assert.Equal(WheelToTable, wheelTarget (Set.ofList [ ModShift ]) (page 458.0 0.0) (WheelNotches -1))
        Assert.Equal(WheelToTable, wheelTarget Set.empty None (WheelNotches -1))
        // A table taller than the viewport is never fully in view: the page scrolls to its end, then the table zooms.
        let tall = Some { tableTop = PagePixels -40.0; tableBottom = PagePixels 520.0; viewportHeight = PagePixels 480.0; offset = PagePixels 600.0; maxOffset = PagePixels 600.0 }
        Assert.Equal(WheelToTable, wheelTarget Set.empty tall (WheelNotches -1))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``wheeling down over the table scrolls the page until the whole table is visible, then zooms it`` () =
        HeadlessSession.run (fun () ->
            let model0 = filledExperiments ()
            let mutable latest = model0
            let window = HostWindow(Width = tableAndElementRotationCanvasWidth, Height = 600.0)
            Program.mkSimple (fun () -> model0) (fun msg m -> let next = updateTableAndElementRotation msg m in latest <- next; next) mainView
            |> Program.withHost window
            |> Program.run
            window.Show()
            Dispatcher.UIThread.RunJobs()
            try
                let viewer = window.GetVisualDescendants() |> Seq.pick (function :? ScrollViewer as s when s.Name = ConstructorUiId.tableElementScrollViewer.value -> Some s | _ -> None)
                let canvas = window.GetVisualDescendants() |> Seq.pick (function :? Canvas as c when c.Name = ConstructorUiId.tableElementCanvas.value -> Some c | _ -> None)
                let tableBottomInWindow () : float =
                    match canvas.TranslatePoint(Point(0.0, canvas.Bounds.Height), window) |> Option.ofNullable with
                    | Some p -> p.Y
                    | None -> infinity
                Assert.True(tableBottomInWindow () > window.ClientSize.Height + 1.0, "precondition: the tall Experiments bay pushes the table's bottom off the window")
                let zoom0 = latest.view.zoom.value
                // Wheel over the visible strip of the table, near the window's bottom edge, until it stops scrolling.
                let mutable notches = 0
                while tableBottomInWindow () > window.ClientSize.Height + 1.0 && notches < 200 do
                    window.MouseWheel(Point(tableAndElementRotationCanvasWidth / 2.0, window.ClientSize.Height - 10.0), Vector(0.0, -1.0), RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    notches <- notches + 1
                Assert.True(tableBottomInWindow () <= window.ClientSize.Height + 1.0, $"the wheel never brought the table's bottom into view (offset %g{viewer.Offset.Y} of %g{viewer.Extent.Height - viewer.Viewport.Height})")
                Assert.Equal(zoom0, latest.view.zoom.value)                                      // the table did not zoom meanwhile
                // Now the table is in view: the next notch zooms it.
                window.MouseWheel(Point(tableAndElementRotationCanvasWidth / 2.0, window.ClientSize.Height - 10.0), Vector(0.0, -1.0), RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                Assert.NotEqual(zoom0, latest.view.zoom.value)
            finally
                window.Close())

    // ------------------------------ Main / Inverse ribbon and Experiments bay (operator items) ------------------------------

    [<Fact>]
    let ``the Main window opens on Add; the Attach data file section belongs to the Inverse window alone`` () =
        let materials, samples, categories, library = stores ()
        Assert.Equal(BayNames.add, (SceneTestFixture.mainScene ()).ribbon)
        Assert.Equal(BayNames.add, (SceneTestFixture.inverseScene library (Experiments.createInMemoryExperiments ()) materials samples categories).ribbon)
        HeadlessSession.run (fun () ->
            let texts (model0 : TableAndElementRotationModel) : string list =
                let window = constructorWindow model0
                window.Width <- 1000.0
                window.Height <- 1100.0
                window.Show()
                Dispatcher.UIThread.RunJobs()
                try window.GetVisualDescendants() |> Seq.choose (function :? TextBlock as t -> Option.ofObj t.Text | _ -> None) |> List.ofSeq
                finally window.Close()
            let forward = texts (filledExperiments ())
            Assert.DoesNotContain("Attach data file…", forward)
            Assert.DoesNotContain("Data files:", forward)
            let inverse =
                let m = { SceneTestFixture.inverseScene library (Experiments.createInMemoryExperiments ()) materials samples categories with ribbon = BayNames.experiments }
                texts { m with experimentCollection = (filledExperiments ()).experimentCollection }
            Assert.Contains("Data files:", inverse)
            Assert.Contains("Attach data file…", inverse))
