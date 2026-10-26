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
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.MaterialStore
open OpticalConstructor.Domain.SampleStore
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Controls
open OpticalConstructor.Ui
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
        let samples = SampleProxy.createInMemory VersionsInUse.empty
        let materials = MaterialProxy.createInMemory (samplesReferencing samples) VersionsInUse.empty
        let categories = CategoryProxy.createInMemory (materialsReferencingCategory materials)
        materials, samples, categories, Library.liveLibrary (Library.createInMemory ()) samples (SourceProxy.createInMemory ())

    let private constructorWindow (model0 : Model) : Window =
        let window = HostWindow()
        Program.mkSimple (fun () -> model0) update mainView |> Program.withHost window |> Program.run
        window :> Window

    /// Main with a polarizer whose R1 run is collected AND being edited — the tallest Experiments bay (editor,
    /// chart, description, collection row, export, collection builder) above the table.
    let private filledExperiments () : Model =
        let m = initMain () |> update (AddElement Placement.LinearPolarizer)
        let pid = (List.item 2 m.elements).id.value
        m |> update (ExpChooseElement pid) |> update ExpCommit |> update (ExpChooseElement pid) |> update (SelectBay BayNames.experiments)

    // ------------------------------ the law, window by window ------------------------------

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``Main: every bay leaves the whole page reachable — the table below a tall bay included`` () =
        for bay in BayNames.all do
            assertReachable $"Main/%s{bay}" (fun () -> constructorWindow ({ initMain () with selection = ElementSelected 0 } |> update (SelectBay bay)))
        assertReachable "Main/Experiments (collected + chart)" (fun () -> constructorWindow (filledExperiments ()))

    [<Fact>]
    let ``Inverse: the Experiments bay with its data-file section leaves everything reachable`` () =
        let materials, samples, categories, library = stores ()
        assertReachable "Inverse/Experiments" (fun () ->
            constructorWindow
                { initInverse library (Experiments.createInMemory ()) materials samples categories with
                    ribbon = BayNames.experiments
                    experimentCollection = (filledExperiments ()).experimentCollection })

    [<Fact>]
    let ``Library and Materials windows: the facet rows wrap, nothing is clipped`` () =
        let materials, samples, categories, library = stores ()
        assertReachable "Library" (fun () -> LibraryWindow(library, samples, SourceProxy.createInMemory (), materials, categories))
        assertReachable "Materials" (fun () -> MaterialsWindow(materials, categories))

    [<Fact>]
    let ``Material, Sample, Category and Source editors leave everything reachable`` () =
        let materials, samples, categories, _ = stores ()
        assertReachable "MaterialEditor" (fun () -> MaterialEditorWindow(materials, MaterialEditorView.NewMaterial (newMaterialId ())))
        assertReachable "SampleEditor" (fun () -> SampleEditorWindow(materials, samples, categories, SampleEditorView.NewSeededMultilayer (newSampleId ())))
        assertReachable "CategoryEditor" (fun () -> CategoryEditorWindow(categories))
        assertReachable "SourceEditor" (fun () -> LightSourceEditorWindow(SourceProxy.createInMemory (), LightSourceEditorView.NewSource "src-x"))

    [<Fact>]
    let ``the chart and solver-handoff windows leave everything reachable`` () =
        let _, _, _, library = stores ()
        let chart : ExperimentChart.ExperimentChart =
            { series = [ { ExperimentChart.ChartSeries.name = "Ψ"; points = [ 0.0, 10.0; 1.0, 12.0 ] } ]
              xLabel = "x"; yLabel = "y"; title = "t"; description = "d"; angular = true }
        let noData : ExperimentData.ExperimentDataProxy =
            { tryLoadIntensity = fun _ -> Error (MeasuredData.MalformedDataFile "none")
              tryLoadEllipsometric = fun _ -> Error (MeasuredData.MalformedDataFile "none") }
        assertReachable "Chart" (fun () -> ChartWindow(chart))
        assertReachable "Handoff" (fun () ->
            SolverHandoffWindow(library, noData, { name = ExperimentCollectionStore.CollectionName "run"; experiments = (filledExperiments ()).experimentCollection.experiments }))

    // ------------------------------ a window never opens larger than its screen ------------------------------

    [<Fact>]
    let ``fitWithin shrinks a too-large window to the working area and moves it inside`` () =
        // A 1080p laptop at 150 %: 1920 × 1032 px of working area = 1280 × 688 DIP; the frame adds 16 × 39 DIP.
        let work = PixelRect(0, 0, 1920, 1032)
        let decoration = Size(16.0, 39.0)
        let fitted, at = WindowFit.fitWithin work 1.5 decoration (PixelPoint(300, 200)) (Size(1400.0, 1000.0))
        Assert.Equal(Size(1264.0, 649.0), fitted)
        Assert.Equal(PixelPoint(0, 0), at)
        // A window that fits keeps its size and its position.
        let small, same = WindowFit.fitWithin work 1.5 decoration (PixelPoint(300, 200)) (Size(620.0, 280.0))
        Assert.Equal(Size(620.0, 280.0), small)
        Assert.Equal(PixelPoint(300, 200), same)
        // One that fits but hangs off the bottom-right is moved back inside, unshrunk.
        let _, moved = WindowFit.fitWithin work 1.0 decoration (PixelPoint(1800, 1000)) (Size(620.0, 280.0))
        Assert.Equal(PixelPoint(1920 - 636, 1032 - 319), moved)
        // A second monitor's work area to the right is honoured (positions are virtual-screen pixels).
        let _, onSecond = WindowFit.fitWithin (PixelRect(1920, 0, 1280, 1024)) 1.0 decoration (PixelPoint(100, 100)) (Size(620.0, 280.0))
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
            Some { tableTop = top; tableBottom = top + 560.0; viewportHeight = 600.0; offset = offset; maxOffset = 400.0 }
        Assert.Equal(WheelToPage, wheelTarget Set.empty (page 458.0 0.0) -1.0)          // table cut off, page can go down
        Assert.Equal(WheelToTable, wheelTarget Set.empty (page 20.0 400.0) -1.0)        // table fully in view
        Assert.Equal(WheelToTable, wheelTarget Set.empty (page 458.0 0.0) 1.0)          // cut off, but the page is at its top
        Assert.Equal(WheelToPage, wheelTarget Set.empty (page -200.0 300.0) 1.0)        // cut off at the top, page can go up
        Assert.Equal(WheelToTable, wheelTarget (Set.ofList [ ModCtrl ]) (page 458.0 0.0) -1.0)
        Assert.Equal(WheelToTable, wheelTarget (Set.ofList [ ModShift ]) (page 458.0 0.0) -1.0)
        Assert.Equal(WheelToTable, wheelTarget Set.empty None -1.0)
        // A table taller than the viewport is never fully in view: the page scrolls to its end, then the table zooms.
        let tall = Some { tableTop = -40.0; tableBottom = 520.0; viewportHeight = 480.0; offset = 600.0; maxOffset = 600.0 }
        Assert.Equal(WheelToTable, wheelTarget Set.empty tall -1.0)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``wheeling down over the table scrolls the page until the whole table is visible, then zooms it`` () =
        HeadlessSession.run (fun () ->
            let model0 = filledExperiments ()
            let mutable latest = model0
            let window = HostWindow(Width = canvasWidth, Height = 600.0)
            Program.mkSimple (fun () -> model0) (fun msg m -> let next = update msg m in latest <- next; next) mainView
            |> Program.withHost window
            |> Program.run
            window.Show()
            Dispatcher.UIThread.RunJobs()
            try
                let viewer = window.GetVisualDescendants() |> Seq.pick (function :? ScrollViewer as s when s.Name = UiIds.TableAndElementRotation.scrollViewer -> Some s | _ -> None)
                let canvas = window.GetVisualDescendants() |> Seq.pick (function :? Canvas as c when c.Name = UiIds.TableAndElementRotation.canvas -> Some c | _ -> None)
                let tableBottomInWindow () : float =
                    match canvas.TranslatePoint(Point(0.0, canvas.Bounds.Height), window) |> Option.ofNullable with
                    | Some p -> p.Y
                    | None -> infinity
                Assert.True(tableBottomInWindow () > window.ClientSize.Height + 1.0, "precondition: the tall Experiments bay pushes the table's bottom off the window")
                let zoom0 = latest.view.zoom
                // Wheel over the visible strip of the table, near the window's bottom edge, until it stops scrolling.
                let mutable notches = 0
                while tableBottomInWindow () > window.ClientSize.Height + 1.0 && notches < 200 do
                    window.MouseWheel(Point(canvasWidth / 2.0, window.ClientSize.Height - 10.0), Vector(0.0, -1.0), RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    notches <- notches + 1
                Assert.True(tableBottomInWindow () <= window.ClientSize.Height + 1.0, $"the wheel never brought the table's bottom into view (offset %g{viewer.Offset.Y} of %g{viewer.Extent.Height - viewer.Viewport.Height})")
                Assert.Equal(zoom0, latest.view.zoom)                                      // the table did not zoom meanwhile
                // Now the table is in view: the next notch zooms it.
                window.MouseWheel(Point(canvasWidth / 2.0, window.ClientSize.Height - 10.0), Vector(0.0, -1.0), RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                Assert.NotEqual(zoom0, latest.view.zoom)
            finally
                window.Close())

    // ------------------------------ Main / Inverse ribbon and Experiments bay (operator items) ------------------------------

    [<Fact>]
    let ``the Main window opens on Add; the Attach data file section belongs to the Inverse window alone`` () =
        let materials, samples, categories, library = stores ()
        Assert.Equal(BayNames.add, (initMain ()).ribbon)
        Assert.Equal(BayNames.add, (initInverse library (Experiments.createInMemory ()) materials samples categories).ribbon)
        HeadlessSession.run (fun () ->
            let texts (model0 : Model) : string list =
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
                let m = { initInverse library (Experiments.createInMemory ()) materials samples categories with ribbon = BayNames.experiments }
                texts { m with experimentCollection = (filledExperiments ()).experimentCollection }
            Assert.Contains("Data files:", inverse)
            Assert.Contains("Attach data file…", inverse))
