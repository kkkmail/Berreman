namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Media
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open Berreman.Constants
open Berreman.Fields
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.MaterialStore
open OpticalConstructor.Domain.SampleStore
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.TableView
open OpticalConstructor.Domain.WindowMode
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Ui
open OpticalConstructor.Ui.TableAndElementRotationView

/// Spec 0046 — what an element is (the hover tooltip and the element menu's header, the menu sized to its
/// text), the Library opening with the element's current binding shown as chosen, tunable vs fixed sources
/// in the Experiments bay, the DETECTOR deciding what each branch captures, the capture step appearing only
/// once something to vary is chosen, Export data, the run following the light's real path (a polarizer after
/// the sample analyzes), and the source editor's tunable mode.
module WorkbenchInfoAndCaptureTests =

    module LW = LibraryWindowView
    module SE = LightSourceEditorView

    let private elem (i : int) (m : TableAndElementRotationModel) : TestElement = List.item i m.elements
    let private bind (i : int) (entryId : string) (m : TableAndElementRotationModel) : TableAndElementRotationModel = updateTableAndElementRotation (BindValueIdTo ((elem i m).id, entryId)) m
    let private glassFilm200Id : string = (SampleItem SeedSamples.glassFilm200).entryId.value
    let private glassPlate1mmId : string = (SampleItem SeedSamples.glassPlate1mm).entryId.value

    let private canvasPointOf (i : int) (m : TableAndElementRotationModel) : ScreenPoint =
        TableView.project pixelsPerMeter center m.view ((snappedCentres m).[i])

    let private mountView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : Window =
        let window = Window(Width = 1200.0, Height = 1000.0)
        window.Content <- Avalonia.FuncUI.Component(fun _ -> mainView model dispatch)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window

    let private findControl (window : Window) (autoId : string) : Control option =
        window.GetVisualDescendants()
        |> Seq.tryPick (fun v ->
            match v with
            | :? Control as c when AutomationProperties.GetAutomationId c = autoId -> Some c
            | _ -> None)

    let private findBorder (window : Window) (autoId : string) : Border option =
        findControl window autoId |> Option.bind (fun c -> match c with :? Border as b -> Some b | _ -> None)

    // ------------------------------ 1–2: what an element is; the menu's width ------------------------------

    [<Fact>]
    let ``elementInfo names the element and its bound entry, or says it is unbound`` () =
        let m = initMain () |> bind 0 "src-hene-633" |> updateTableAndElementRotation (AddElement CatalogueKind.Sample)
        let heading, detail = elementInfo m 0
        Assert.Equal("Light source #1", heading)
        Assert.Contains("HeNe laser 632.8 nm", detail)
        Assert.Contains("632.8 nm", detail)
        Assert.Contains("Unbound", snd (elementInfo m 2))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``hovering an element shows what it is, and its menu says the same, wrapped, at most 460 px wide`` () =
        HeadlessSession.run (fun () ->
            let m = initMain () |> bind 0 "src-arf-193"
            let window = mountView m ignore
            match findControl window (ConstructorUiId.elementHover (ElementId "src")).value with
            | Some target -> Assert.Contains("ArF excimer 193 nm", string (ToolTip.GetTip target))
            | None -> Assert.Fail "the source must carry a hover target"
            window.Close()
            let opened = updateTableAndElementRotation (OpenElementMenuAt (canvasPointOf 0 m)) m
            let menuWindow = mountView opened ignore
            match findBorder menuWindow ConstructorUiId.elementMenu.value, (findControl menuWindow ConstructorUiId.elementMenuInfo.value |> Option.bind (fun c -> match c with :? TextBlock as t -> Some t | _ -> None)) with
            | Some menu, Some info ->
                Assert.Equal(460.0, menu.MaxWidth)
                Assert.True(menu.MinWidth >= 260.0)
                Assert.Contains("ArF excimer 193 nm", info.Text)
                Assert.Equal(TextWrapping.Wrap, info.TextWrapping)
            | _ -> Assert.Fail "the menu and its info line must be shown"
            menuWindow.Close()
            Dispatcher.UIThread.RunJobs())

    // ------------------------------ 4: the Library shows the current binding as chosen ------------------------------

    let private libraryContext () : LW.LibraryWindowContext =
        let samples = SampleProxy.createInMemory VersionsInUse.empty
        {
            library = Library.createInMemory ()
            samples = samples
            materials = MaterialProxy.createInMemory (samplesReferencing samples) VersionsInUse.empty
            treeAutoBuildThreshold = TreeAutoBuildThreshold.defaultValue
            thicknessBucketCap = ThicknessBucketCap.defaultValue
            openSampleEditor = ignore
            sources = SourceProxy.createInMemory ()
            openSourceEditor = ignore
            requestClose = ignore
        }

    [<Fact>]
    let ``a Select session opens with the target's current entry chosen and its row visible`` () =
        let session (current : ElementValueId option) : SelectionContext<LibraryEntry> =
            {
                kindConstraint = KindConstraint LightSource
                target = TableElementTarget (ElementId "src")
                currentEntryIdOpt = current
                onSelected = ignore
                onCancelled = ignore
            }
        let m = LW.initLibraryWindow (libraryContext ()) (Select (session (Some (ElementValueId "src-hene-633"))))
        Assert.Equal(Some "src-hene-633", m.selectedEntryId)
        Assert.Equal(LW.entryNodeCode "src-hene-633", m.selectedNodeCode)
        Assert.True(m.expandedNodes.isExpanded "entries", "the entries group opens so the chosen row shows")
        // Nothing bound (or a binding of another kind) shows nothing chosen.
        Assert.Equal(None, (LW.initLibraryWindow (libraryContext ()) (Select (session None))).selectedEntryId)
        Assert.Equal(None, (LW.initLibraryWindow (libraryContext ()) (Select (session (Some (ElementValueId "det-intensity"))))).selectedEntryId)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the menu's Choose… hands the Library the element's current binding`` () =
        HeadlessSession.run (fun () ->
            let captured : SelectionContext<LibraryEntry> option ref = ref None
            let launchers = { EditorLaunchers.defaults with openLibrarySelectWindow = fun _ _ _ _ _ _ _ ctx -> captured.Value <- Some ctx; None }
            let m0 = { (initMain () |> bind 0 "src-krf-248") with launchers = launchers }
            let opened = updateTableAndElementRotation (OpenElementMenuAt (canvasPointOf 0 m0)) m0
            let mutable model = opened
            let window = mountView opened (fun msg -> model <- updateTableAndElementRotation msg model)
            match findBorder window ConstructorUiId.elementMenuChoose.value with
            | Some row ->
                match Option.ofNullable (row.TranslatePoint(Point(row.Bounds.Width / 2.0, row.Bounds.Height / 2.0), window)) with
                | Some c ->
                    window.MouseDown(c, MouseButton.Left, RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                | None -> Assert.Fail "Choose… has no on-screen position"
            | None -> Assert.Fail "the menu must offer Choose…"
            match captured.Value with
            | Some ctx -> Assert.Equal(Some (ElementValueId "src-krf-248"), ctx.currentEntryIdOpt)
            | None -> Assert.Fail "Choose… must open the Library in Select state"
            window.Close()
            Dispatcher.UIThread.RunJobs())

    // ------------------------------ 5: tunable vs fixed sources in the bay ------------------------------

    /// A bound glass film (index 2) varied on R2, captured in T, with the given source.
    let private filmOnR2 (source : string) : TableAndElementRotationModel =
        let m = initMain () |> bind 0 source |> updateTableAndElementRotation (AddElement CatalogueKind.Sample) |> bind 2 glassFilm200Id
        m
        |> updateTableAndElementRotation (ExpChooseElement (elem 2 m).id.value)
        |> updateTableAndElementRotation (ExpChooseVariable ExperimentControls.VaryR2)
        |> updateTableAndElementRotation (ExpChooseMeasurement ExperimentControls.CaptureT)

    [<Fact>]
    let ``a tunable source is set within its limits while R2 varies; a fixed line shows read-only and ignores a setting`` () =
        let tunable = filmOnR2 "src-xe-mono"
        match (experimentState tunable).sourceWaveLength with
        | ExperimentControls.TunableSourceWaveLength (setting, limits) ->
            Assert.True(abs (setting.value - 600.0) < 1e-9 && abs (limits.lowest.value - 190.0) < 1e-9 && abs (limits.highest.value - 1000.0) < 1e-9)
        | other -> Assert.Fail($"expected a tunable setting, got %A{other}")
        let clamped = updateTableAndElementRotation (ExpSetSourceWaveLength 2000.0) tunable
        match clamped.experimentCollection.draft.sourceWaveLengthOpt with
        | Some w -> Assert.True(abs (w.value / nmToMeter / oneNanometer - 1000.0) < 1e-9)
        | None -> Assert.Fail "the setting must be kept (clamped)"
        let fixedLine = filmOnR2 "src-hene-633"
        match (experimentState fixedLine).sourceWaveLength with
        | ExperimentControls.FixedSourceWaveLength setting -> Assert.True(abs (setting.value - 632.8) < 1e-9)
        | other -> Assert.Fail($"expected a fixed line, got %A{other}")
        Assert.Equal<TableAndElementRotationModel>(fixedLine, updateTableAndElementRotation (ExpSetSourceWaveLength 500.0) fixedLine)

    [<Fact>]
    let ``the run uses the source's wavelength: a tunable source set to 632.8 nm equals the HeNe line`` () =
        let tuned = filmOnR2 "src-xe-mono" |> updateTableAndElementRotation (ExpSetSourceWaveLength 632.8)
        let line = filmOnR2 "src-hene-633"
        let ys (m : TableAndElementRotationModel) : float list = (List.head (experimentResult m).series).points |> List.map (fun p -> p.y)
        List.zip (ys tuned) (ys line)
        |> List.iter (fun (a, b) -> Assert.True(abs (a - b) < 1e-12, $"%g{a} vs %g{b}"))
        // ... and it differs from the 600 nm default (the wavelength really is used).
        let default600 = filmOnR2 "src-600"
        Assert.True(List.exists2 (fun a b -> abs (a - b) > 1e-6) (ys line) (ys default600))

    // ------------------------------ 6–7: the detector decides; the capture step waits ------------------------------

    [<Fact>]
    let ``each branch reads what its detector reads: T intensity, R Psi and Delta`` () =
        let m =
            initMain ()
            |> updateTableAndElementRotation (AddElement CatalogueKind.Sample) |> bind 2 glassPlate1mmId
            |> updateTableAndElementRotation (RotSetAxis (RotationControls.R2, 30.0))
            |> updateTableAndElementRotation (AddElement Detector)
            |> fun m -> bind 3 "det-ellipsometer" m
        Assert.Equal(BeamLayout.OnReflectedBeamOf (elem 2 m).id, (elem 3 m).beam)
        let chosen =
            m
            |> updateTableAndElementRotation (ExpChooseElement (elem 2 m).id.value)
            |> updateTableAndElementRotation (ExpChooseVariable ExperimentControls.VaryR2)
            |> updateTableAndElementRotation (ExpChooseMeasurement ExperimentControls.CaptureBoth)
        let offers = (experimentState chosen).measurementOffers |> List.map (fun o -> o.label.value)
        Assert.Contains("Transmitted (T) — intensity", offers)
        Assert.Contains("Reflected (R) — Ψ, Δ (ellipsometer)", offers)
        let chart = experimentResult chosen
        Assert.Equal<string list>([ "Intensity (T)"; "Ψ (R)"; "Δ (R)" ], chart.series |> List.map (fun s -> s.name.value))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the capture step appears only once something to vary is chosen`` () =
        HeadlessSession.run (fun () ->
            let visibleIn (state : ExperimentControls.ExperimentControlsState) : bool =
                let handlers : ExperimentControls.ExperimentControlsHandlers =
                    {
                        chooseElement = ignore; chooseVariable = ignore; chooseMeasurement = ignore
                        setRangeMin = ignore; setRangeMax = ignore; setRangePoints = ignore; setSourceWaveLength = ignore
                        addOrUpdate = ignore; newExperiment = ignore; editExperiment = ignore; removeExperiment = ignore
                        viewExperiment = ignore; openChartWindow = ignore
                    }
                let window = Window(Width = 600.0, Height = 800.0)
                window.Content <- Avalonia.FuncUI.Component(fun _ -> ExperimentControls.experimentControlsView state handlers)
                window.Show()
                Dispatcher.UIThread.RunJobs()
                let shown =
                    match findBorder window (ConstructorUiId.experimentMeasurement (ConstructorUiKey "t")).value with
                    | Some box -> box.IsEffectivelyVisible
                    | None -> false
                window.Close()
                shown
            let offered =
                { ExperimentControls.emptyExperimentControls with
                    availability = ControlOffered
                    candidates = [ { elementId = ConstructorUiKey "s"; label = ControlCaption "Sample #3" } ]
                    chosenId = Some (ConstructorUiKey "s")
                    variableChoices = [ ExperimentControls.VaryR2 ]
                    measurementOffers = [ { choice = ExperimentControls.CaptureT; label = ControlCaption "Transmitted (T) — intensity" } ] }
            Assert.False(visibleIn offered, "no variable chosen yet — the capture step is hidden")
            Assert.True(visibleIn { offered with chosenVariable = Some ExperimentControls.VaryR2 }, "a variable chosen — the capture step shows"))

    // ------------------------------ export ------------------------------

    [<Fact>]
    let ``Export data hands the draft's run to the export seam, and says so when there is nothing to export`` () =
        let store = ExperimentExport.createInMemoryExperimentExport ()
        let nothing = updateTableAndElementRotation ExportExperimentData { initMain () with experimentExport = store.proxy }
        Assert.StartsWith("Nothing exported", nothing.exportStatus |> Option.defaultValue "")
        let exported = updateTableAndElementRotation ExportExperimentData { filmOnR2 "src-600" with experimentExport = store.proxy }
        Assert.StartsWith("Exported 1 series", exported.exportStatus |> Option.defaultValue "")
        match store.exported () with
        | [ run ] ->
            Assert.Equal("Incidence angle R2 (°)", run.xLabel.value)
            Assert.Equal<string list>([ "Intensity" ], run.series |> List.map (fun s -> s.name.value))
        | other -> Assert.Fail($"expected one exported run, got %d{List.length other}")

    // ------------------------------ the run follows the light's real path ------------------------------

    [<Fact>]
    let ``a polarizer AFTER the sample analyzes: crossed polarizers around a clear film pass nothing`` () =
        let m =
            initMain ()
            |> updateTableAndElementRotation (AddElement LinearPolarizer)                       // index 2, R1 = 0, before the sample
            |> updateTableAndElementRotation (AddElement CatalogueKind.Sample) |> bind 3 glassFilm200Id
            |> updateTableAndElementRotation (AddElement LinearPolarizer)                       // index 4, after the sample
            |> updateTableAndElementRotation (RotSetAxis (RotationControls.R1, 90.0))
        let run =
            m
            |> updateTableAndElementRotation (ExpChooseElement (elem 3 m).id.value)
            |> updateTableAndElementRotation (ExpChooseVariable ExperimentControls.VaryR2)
            |> updateTableAndElementRotation (ExpChooseMeasurement ExperimentControls.CaptureT)
        let ys = (List.head (experimentResult run).series).points |> List.map (fun p -> p.y)
        Assert.True(List.forall (fun y -> abs y < 1e-9) ys, $"crossed polarizers must pass nothing, max %g{List.max ys}")
        // Parallel instead: the film's own transmittance comes through.
        let parallel' = run |> fun r -> { r with selection = ElementSelected 4 } |> updateTableAndElementRotation (RotSetAxis (RotationControls.R1, 0.0))
        Assert.True((List.head (experimentResult parallel').series).points |> List.exists (fun p -> p.y > 0.5))

    // ------------------------------ the source editor's tunable mode ------------------------------

    [<Fact>]
    let ``the source editor saves a tunable source and refuses limits in the wrong order`` () =
        let sources = SourceProxy.createInMemory ()
        let context : SE.SourceEditorContext = { sources = sources; requestClose = ignore }
        let m = SE.initLightSourceEditor context (SE.NewSource "src-tune") |> SE.updateLightSourceEditor (SE.SetLightSourceName "My OPO") |> SE.updateLightSourceEditor (SE.SetTuning SE.TunableChoice)
        let saved = SE.updateLightSourceEditor SE.LightSourceSaveClicked m
        Assert.Equal(None, saved.status)
        match sources.tryGetSource (ElementValueId "src-tune") with
        | Ok (Some s) -> Assert.Equal(Some (400.0, 800.0), s.tunableLimitsOpt |> Option.map (fun l -> round (l.lower.value / nmToMeter / oneNanometer), round (l.upper.value / nmToMeter / oneNanometer)))
        | other -> Assert.Fail($"expected the tunable source, got %A{other}")
        let reversed = m |> SE.updateLightSourceEditor (SE.SetMinimumText "900") |> SE.updateLightSourceEditor SE.LightSourceSaveClicked
        Assert.True(Option.isSome reversed.status, "limits in the wrong order are refused with a reason")
