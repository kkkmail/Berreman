namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
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

/// Spec 0038 Part L (step 039) — the SolverHandoffWindow component (UICOMP_XDUO_0011): the inverse
/// flow's terminal summary + BASIC-validation screen, driven over MOCK proxies. Pure per-experiment
/// validation facts (every non-sample element specified; a data file attached; the file reads
/// schema-valid and range-consistent through the `ExperimentDataProxy`) plus headless semantic-tree
/// proofs that DRIVE THE REAL WINDOW by its UiIds: a fully valid collection lists green (ready)
/// statuses; a missing file and a range mismatch each show their typed message; NO solver math runs
/// anywhere. The component is declared, not wired — no parent view opens it here (step 047 owns the
/// launcher wiring).
module SolverHandoffWindowTests =

    // ---- headless semantic-tree helpers (the CategoryEditorWindowTests precedent) --------------------

    let private matchesId (id : string) (c : Control) : bool =
        c.Name = id || Avalonia.Automation.AutomationProperties.GetAutomationId(c) = id

    let private tryFindControl (window : Window) (id : string) : Control option =
        window.GetVisualDescendants()
        |> Seq.tryPick (function :? Control as c when matchesId id c -> Some c | _ -> None)

    let private isPresent (window : Window) (id : string) : bool =
        match tryFindControl window id with
        | Some _ -> true
        | None -> false

    let private textOf (window : Window) (id : string) : string =
        match tryFindControl window id with
        | Some (:? TextBlock as tb) -> tb.Text
        | Some c -> failwith $"%s{id} is a %s{c.GetType().Name}, not a TextBlock"
        | None -> failwith $"%s{id} was not found in the visual tree"

    // ---- fixtures: build real experiments (with captured setups) through the inverse model ----------

    /// Locate the seeded inverse endpoints by kind (the InverseConstructorTests precedent).
    let private endpoints (m : Model) : TestElement * TestElement * TestElement =
        let byKind (k : Placement.CatalogueKind) : TestElement = m.elements |> List.find (fun e -> e.placement.catalogueKind = k)
        byKind Placement.LightSource, byKind Placement.Sample, byKind Placement.Detector

    /// One inverse experiment with a REAL captured setup: the seeded source / detector optionally bound
    /// to a preset entry id (`None` leaves the endpoint UNSPECIFIED), the sample the varied unknown
    /// (an R2 intensity sweep, range 0…89°). Returns the (in-memory) Library the setup was captured
    /// against and the committed experiment. The caller decorates it (id / attached file) directly on
    /// the record — the handoff validation is a pure function of the experiment + proxies.
    let private baseExperiment (srcBind : string option) (detBind : string option) : LibraryProxy * Experiments.Experiment =
        let samples = SampleProxy.createInMemory VersionsInUse.empty
        let materials = MaterialProxy.createInMemory (samplesReferencing samples) VersionsInUse.empty
        let categories = CategoryProxy.createInMemory (materialsReferencingCategory materials)
        let m0 = initInverse (Library.createInMemory ()) (Experiments.createInMemory ()) materials samples categories
        let src, sample, det = endpoints m0
        let bindOne (elementId : Library.ElementId) (entryOpt : string option) (m : Model) : Model =
            match entryOpt with
            | Some entryId -> update (BindValueIdTo (elementId, entryId)) m
            | None -> m
        let committed =
            m0
            |> bindOne src.id srcBind
            |> bindOne det.id detBind
            |> update (ExpChooseElement sample.id.value)
            |> update (ExpChooseVariable ExperimentControls.VaryR2)
            |> update ExpCommit
        committed.library, List.head committed.experimentCollection.experiments

    let private withId (n : int) (exp : Experiments.Experiment) : Experiments.Experiment =
        // The single-case `ExperimentId` case constructor directly — `ExperimentId.create` would bind
        // the same-named union CASE in expression position (the step-037 shadowing gotcha).
        { exp with id = Experiments.ExperimentId n }

    let private withFile (path : string) (exp : Experiments.Experiment) : Experiments.Experiment =
        { exp with dataFileOpt = Some (Experiments.DataFilePath path) }

    let private noFile (exp : Experiments.Experiment) : Experiments.Experiment =
        { exp with dataFileOpt = None }

    // ---- mock ExperimentDataProxy shapes ------------------------------------------------------------

    let private intensitySeries (points : (float * float) list) : MeasuredData.IntensitySeries =
        { points = points |> List.map (fun (x, i) -> { x = MeasuredData.MeasuredAbscissa x; intensity = MeasuredData.MeasuredIntensity i }) }

    /// An intensity load proxy that answers by the attached path — so ONE proxy serves a collection
    /// whose experiments each attach a different file (valid / out-of-range / …).
    let private mockIntensityByPath (byPath : Map<string, Result<MeasuredData.IntensitySeries, MeasuredData.ExperimentDataError>>) : ExperimentData.ExperimentDataProxy =
        {
            tryLoadIntensity =
                fun (p : Experiments.DataFilePath) ->
                    match byPath |> Map.tryFind p.value with
                    | Some r -> r
                    | None -> Error (MeasuredData.MalformedDataFile $"no mock intensity registered for '%s{p.value}'")
            tryLoadEllipsometric = fun _ -> Error (MeasuredData.MalformedDataFile "the mock has no ellipsometric data")
        }

    let private mockIntensity (result : Result<MeasuredData.IntensitySeries, MeasuredData.ExperimentDataError>) : ExperimentData.ExperimentDataProxy =
        { tryLoadIntensity = fun _ -> result
          tryLoadEllipsometric = fun _ -> Error (MeasuredData.MalformedDataFile "the mock has no ellipsometric data") }

    let private mockEllipsometric (result : Result<MeasuredData.EllipsometricSeries, MeasuredData.ExperimentDataError>) : ExperimentData.ExperimentDataProxy =
        { tryLoadIntensity = fun _ -> Error (MeasuredData.MalformedDataFile "the mock has no intensity data")
          tryLoadEllipsometric = fun _ -> result }

    /// An intensity proxy that COUNTS its loads — the "no computation beyond validation" proof.
    let private recordingIntensity (result : Result<MeasuredData.IntensitySeries, MeasuredData.ExperimentDataError>) : int ref * ExperimentData.ExperimentDataProxy =
        let calls = ref 0
        let proxy : ExperimentData.ExperimentDataProxy =
            { tryLoadIntensity = fun _ -> calls.Value <- calls.Value + 1; result
              tryLoadEllipsometric = fun _ -> Error (MeasuredData.MalformedDataFile "the mock has no ellipsometric data") }
        calls, proxy

    let private snapshotOf (experiments : Experiments.Experiment list) : ExperimentCollectionStore.ExperimentCollectionSnapshot =
        { name = ExperimentCollectionStore.CollectionName "run-A"; experiments = experiments }

    let private contextOf (library : LibraryProxy) (proxy : ExperimentData.ExperimentDataProxy) (experiments : Experiments.Experiment list) : SolverHandoffView.SolverHandoffContext =
        { library = library; experimentData = proxy; collection = snapshotOf experiments; requestClose = ignore }

    // ================================ pure per-experiment validation ================================

    [<Fact>]
    let ``a fully valid experiment validates green (HandoffReady) with the raw point count`` () =
        let library, exp0 = baseExperiment (Some "src-600") (Some "det-intensity")
        let exp = exp0 |> withFile "good.csv"
        let proxy = mockIntensity (Ok (intensitySeries [ 0.0, 1.0; 45.0, 0.6; 89.0, 0.2 ]))
        match SolverHandoffView.validateExperiment library proxy exp with
        | SolverHandoffView.HandoffReady 3 -> ()
        | other -> Assert.Fail($"expected HandoffReady 3, got %A{other}")

    [<Fact>]
    let ``an unbound non-sample element yields UnspecifiedElements`` () =
        // A fresh inverse leaves the seeded source AND detector UNBOUND — only the sample may be
        // unspecified, so the source / detector flag the typed error.
        let library, exp0 = baseExperiment None None
        let exp = exp0 |> withFile "good.csv"
        let proxy = mockIntensity (Ok (intensitySeries [ 0.0, 1.0 ]))
        match SolverHandoffView.validateExperiment library proxy exp with
        | SolverHandoffView.UnspecifiedElements reason -> Assert.Contains("every element except the sample", reason)
        | other -> Assert.Fail($"expected UnspecifiedElements, got %A{other}")

    [<Fact>]
    let ``a missing data file yields MissingDataFile`` () =
        let library, exp0 = baseExperiment (Some "src-600") (Some "det-intensity")
        let exp = exp0 |> noFile
        let proxy = mockIntensity (Ok (intensitySeries [ 0.0, 1.0 ]))
        match SolverHandoffView.validateExperiment library proxy exp with
        | SolverHandoffView.MissingDataFile _ -> ()
        | other -> Assert.Fail($"expected MissingDataFile, got %A{other}")

    [<Fact>]
    let ``a range mismatch yields DataFileValidationError`` () =
        // The R2 experiment's range is 0…89°; an abscissa of 200 is a LOUD range mismatch.
        let library, exp0 = baseExperiment (Some "src-600") (Some "det-intensity")
        let exp = exp0 |> withFile "oor.csv"
        let proxy = mockIntensity (Ok (intensitySeries [ 0.0, 1.0; 200.0, 0.5 ]))
        match SolverHandoffView.validateExperiment library proxy exp with
        | SolverHandoffView.DataFileValidationError reason -> Assert.Contains("outside the experiment range", reason)
        | other -> Assert.Fail($"expected DataFileValidationError, got %A{other}")

    [<Fact>]
    let ``a malformed data file yields DataFileParseError`` () =
        let library, exp0 = baseExperiment (Some "src-600") (Some "det-intensity")
        let exp = exp0 |> withFile "bad.csv"
        let proxy = mockIntensity (Error (MeasuredData.MalformedDataFile "row 3 is not a valid X,Y pair"))
        match SolverHandoffView.validateExperiment library proxy exp with
        | SolverHandoffView.DataFileParseError reason -> Assert.Contains("row 3", reason)
        | other -> Assert.Fail($"expected DataFileParseError, got %A{other}")

    [<Fact>]
    let ``the detector kind selects the ellipsometric load path`` () =
        let library, exp0 = baseExperiment (Some "src-600") (Some "det-ellipsometer")
        let exp = exp0 |> withFile "ellipso.csv"
        let ellipsometric =
            match MeasuredData.parseEllipsometricSeries "wavelength_nm,psi_deg,delta_deg\n500,20,120\n600,25,110" with
            | Ok series -> series
            | Error e -> failwith $"the fixture parser failed: %A{e}"
        let proxy = mockEllipsometric (Ok ellipsometric)
        match SolverHandoffView.validateExperiment library proxy exp with
        | SolverHandoffView.HandoffReady 2 -> ()
        | other -> Assert.Fail($"expected HandoffReady 2 from the ellipsometric path, got %A{other}")

    [<Fact>]
    let ``validation runs the data load exactly once and reports the raw point count (no solver math, no normalization)`` () =
        let library, exp0 = baseExperiment (Some "src-600") (Some "det-intensity")
        let exp = exp0 |> withFile "good.csv"
        let raw = [ 0.0, 1.0; 30.0, 0.7; 60.0, 0.4; 89.0, 0.1 ]
        let calls, proxy = recordingIntensity (Ok (intensitySeries raw))
        match SolverHandoffView.validateExperiment library proxy exp with
        | SolverHandoffView.HandoffReady n ->
            Assert.Equal(List.length raw, n)          // the raw point count — nothing normalized away
            Assert.Equal(1, calls.Value)              // exactly one load — the only computation the screen runs
        | other -> Assert.Fail($"expected HandoffReady, got %A{other}")

    // ================================ init builds every row + carries the name ================================

    [<Fact>]
    let ``init validates every received experiment and carries the collection name`` () =
        let library, valid0 = baseExperiment (Some "src-600") (Some "det-intensity")
        let _, missing0 = baseExperiment (Some "src-600") (Some "det-intensity")
        let _, oor0 = baseExperiment (Some "src-600") (Some "det-intensity")
        let experiments =
            [ valid0 |> withId 1 |> withFile "good.csv"
              missing0 |> withId 2 |> noFile
              oor0 |> withId 3 |> withFile "oor.csv" ]
        let proxy =
            mockIntensityByPath (Map.ofList [ "good.csv", Ok (intensitySeries [ 0.0, 1.0; 89.0, 0.2 ])
                                              "oor.csv", Ok (intensitySeries [ 0.0, 1.0; 200.0, 0.5 ]) ])
        let model : SolverHandoffView.Model = SolverHandoffView.init (contextOf library proxy experiments)
        Assert.Equal("run-A", model.collectionName.value)
        Assert.Equal(3, List.length model.rows)
        let statusById (n : int) : SolverHandoffView.ExperimentHandoffStatus =
            (model.rows |> List.find (fun r -> r.experiment.id.value = n)).status
        Assert.True((statusById 1).isReady, "experiment 1 must be ready")
        match statusById 2 with
        | SolverHandoffView.MissingDataFile _ -> ()
        | other -> Assert.Fail($"experiment 2 expected MissingDataFile, got %A{other}")
        match statusById 3 with
        | SolverHandoffView.DataFileValidationError _ -> ()
        | other -> Assert.Fail($"experiment 3 expected DataFileValidationError, got %A{other}")

    // ================================ headless semantic-tree proofs (gate ui-smoke) ================================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the window mounts carrying its id, the summary list, the solver-comes-later message, and each status row`` () =
        HeadlessSession.run (fun () ->
            let library, valid0 = baseExperiment (Some "src-600") (Some "det-intensity")
            let _, missing0 = baseExperiment (Some "src-600") (Some "det-intensity")
            let experiments = [ valid0 |> withId 1 |> withFile "good.csv"; missing0 |> withId 2 |> noFile ]
            let proxy = mockIntensityByPath (Map.ofList [ "good.csv", Ok (intensitySeries [ 0.0, 1.0; 89.0, 0.2 ]) ])
            let window = SolverHandoffWindow(library, proxy, snapshotOf experiments)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(matchesId UiIds.Handoff.window window, "the window itself carries the SolverHandoffWindow id")
            Assert.True(isPresent window UiIds.Handoff.summaryList, "the summary list is missing")
            Assert.True(isPresent window UiIds.Handoff.solverMessage, "the solver-comes-later message is missing")
            Assert.Contains("separately", textOf window UiIds.Handoff.solverMessage)
            Assert.True(isPresent window (UiIds.Handoff.statusRow 1), "experiment 1 status row is missing")
            Assert.True(isPresent window (UiIds.Handoff.statusRow 2), "experiment 2 status row is missing")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a valid experiment shows a ready status; a missing file and a range mismatch show their typed message`` () =
        HeadlessSession.run (fun () ->
            let library, valid0 = baseExperiment (Some "src-600") (Some "det-intensity")
            let _, missing0 = baseExperiment (Some "src-600") (Some "det-intensity")
            let _, oor0 = baseExperiment (Some "src-600") (Some "det-intensity")
            let experiments =
                [ valid0 |> withId 1 |> withFile "good.csv"
                  missing0 |> withId 2 |> noFile
                  oor0 |> withId 3 |> withFile "oor.csv" ]
            let proxy =
                mockIntensityByPath (Map.ofList [ "good.csv", Ok (intensitySeries [ 0.0, 1.0; 89.0, 0.2 ])
                                                  "oor.csv", Ok (intensitySeries [ 0.0, 1.0; 200.0, 0.5 ]) ])
            let window = SolverHandoffWindow(library, proxy, snapshotOf experiments)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.Contains("Ready", textOf window (UiIds.Handoff.statusRow 1))
            Assert.Contains("Missing data file", textOf window (UiIds.Handoff.statusRow 2))
            Assert.Contains("Validation error", textOf window (UiIds.Handoff.statusRow 3))
            window.Close())
