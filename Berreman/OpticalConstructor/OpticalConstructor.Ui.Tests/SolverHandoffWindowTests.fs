namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Controls
open OpticalConstructor.Ui
open OpticalConstructor.Ui.TableAndElementRotationView
open OpticalConstructor.Tests

/// Spec 0038 Part L (step 039) — the SolverHandoffWindow component (UICOMP_XDUO_0011): the inverse
/// flow's terminal summary + BASIC-validation screen, driven over MOCK proxies. Pure per-experiment
/// validation facts (every non-sample element specified; a data file attached; the file reads
/// schema-valid and range-consistent through the `ExperimentDataProxy`) plus headless semantic-tree
/// proofs that DRIVE THE REAL WINDOW by its UiIds: a fully valid collection lists green (ready)
/// statuses; a missing file and a range mismatch each show their typed message; NO solver math runs
/// anywhere. The component is declared, not wired — no parent view opens it here (step 047 owns the
/// launcher wiring).
module SolverHandoffWindowTests =

    let private attachmentHash : ContentHash = ContentHash "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

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
    let private endpoints (m : TableAndElementRotationModel) : Setups.SetupSlot * Setups.SetupSlot * Setups.SetupSlot =
        let byKind (k : Placement.CatalogueKind) : Setups.SetupSlot = m.elements |> List.find (fun e -> e.placement.catalogueKind = k)
        byKind Placement.LightSource, byKind Placement.Sample, byKind Placement.Detector

    /// One inverse experiment over the setup its commit recorded: the seeded source / detector optionally bound
    /// to a preset entry id (`None` leaves the endpoint UNSPECIFIED), the sample the varied unknown
    /// (an R2 intensity sweep, range 0…89°). Returns the (in-memory) Library the setup was captured
    /// against, the committed model's setups and the committed experiment. The caller decorates it (id / attached
    /// file) directly on the record — the handoff validation is a pure function of the experiment, its setups and
    /// the proxies (spec 0050 S8.2).
    let private baseExperiment
        (srcBind : Placement.ElementValueId option)
        (detBind : Placement.ElementValueId option)
        : LibraryProxy * Setups.Setup list * Experiments.Experiment =
        let samples = BuiltinCatalogue.sampleStore ()
        let materials = BuiltinCatalogue.materialStore (samplesReferencing samples)
        let categories = BuiltinCatalogue.categoryStore (materialsReferencingCategory materials)
        let owner = FolderName "SolverHandoffWindowTests"
        let m0 =
            {
                SceneTestFixture.inverseScene (BuiltinCatalogue.library ()) (Experiments.createInMemoryExperiments ()) materials samples categories with
                    catalogueImports = (TestAppScope.freshAppContext owner).catalogueImports
            }
        let src, sample, det = endpoints m0
        let bindOne (elementId : Library.ElementId) (entryOpt : Placement.ElementValueId option) (m : TableAndElementRotationModel) : TableAndElementRotationModel =
            match entryOpt with
            | Some entryId -> updateTableAndElementRotation (BindValueIdTo (elementId, entryId)) m
            | None -> m
        let committed =
            m0
            |> bindOne src.id srcBind
            |> bindOne det.id detBind
            |> updateTableAndElementRotation (ExpChooseElement sample.id)
            |> updateTableAndElementRotation (ExpChooseVariable ExperimentControls.VaryR2)
            |> updateTableAndElementRotation ExpCommit
        committed.library, committed.kitSetups, List.head committed.experimentCollection.experiments

    let private withId (n : int) (exp : Experiments.Experiment) : Experiments.Experiment =
        // The single-case `ExperimentId` case constructor directly — `ExperimentId.create` would bind
        // the same-named union CASE in expression position (the step-037 shadowing gotcha).
        { exp with id = Experiments.ExperimentId n; name = (Experiments.ExperimentId n).defaultName }

    /// The experiment with `data/<name>` read through `profile` over every row (spec 0050 S9.1).
    let private withMappedFile (profile : ImportProfileName) (name : string) (exp : Experiments.Experiment) : Experiments.Experiment =
        let attachment : DataAttachment =
            {
                path = KitRelativePath $"data/{name}"
                contentHash = attachmentHash
            }
        {
            exp with
                dataOpt =
                    Some
                        {
                            attachment = attachment
                            profile = profile
                            rows = AllDataRows
                        }
        }

    let private withFile (name : string) (exp : Experiments.Experiment) : Experiments.Experiment =
        withMappedFile ImportProfileName.intensitySeries name exp

    let private noFile (exp : Experiments.Experiment) : Experiments.Experiment =
        { exp with dataOpt = None }

    // ---- mock ExperimentDataProxy shapes ------------------------------------------------------------

    let private intensitySeries (points : (float * float) list) : MeasuredData.IntensitySeries =
        { points = points |> List.map (fun (x, i) -> { x = MeasuredData.MeasuredAbscissa x; intensity = MeasuredData.MeasuredIntensity i }) }

    /// The series as the mapped points an X,Y built-in reads it to (spec 0050 S9.2).
    let private intensityPoints
        (result : Result<MeasuredData.IntensitySeries, MeasuredData.ExperimentDataError>)
        : Result<ImportProfiles.MappedPoints, MeasuredData.ExperimentDataError> =
        result |> Result.map (MeasuredData.IntensityData >> ImportProfiles.SeriesPoints)

    /// An intensity load proxy that answers by the attached path — so ONE proxy serves a collection
    /// whose experiments each attach a different file (valid / out-of-range / …).
    let private mockIntensityByPath (byPath : Map<KitRelativePath, Result<MeasuredData.IntensitySeries, MeasuredData.ExperimentDataError>>) : ExperimentData.ExperimentDataProxy =
        {
            readPickedFile = fun file -> Error (KitIoErr $"the mock reads no picked file '{file.value}'")
            attachDataFile =
                fun file ->
                    Ok
                        {
                            path = KitRelativePath $"data/{System.IO.Path.GetFileName file.value}"
                            contentHash = attachmentHash
                        }
            attachmentStatus = fun _ -> Ok AttachedDataUnchanged
            readMapped =
                fun (data : MappedAttachment) ->
                    match byPath |> Map.tryFind data.attachment.path with
                    | Some r -> intensityPoints r
                    | None -> Error (MeasuredData.MalformedDataFileErr $"no mock intensity registered for '%s{data.attachment.path.value}'")
        }

    let private mockIntensity (result : Result<MeasuredData.IntensitySeries, MeasuredData.ExperimentDataError>) : ExperimentData.ExperimentDataProxy =
        {
            mockIntensityByPath Map.empty with
                readMapped = fun _ -> intensityPoints result
        }

    let private mockEllipsometric (result : Result<MeasuredData.EllipsometricSeries, MeasuredData.ExperimentDataError>) : ExperimentData.ExperimentDataProxy =
        {
            mockIntensity (Error (MeasuredData.MalformedDataFileErr "the mock has no intensity data")) with
                readMapped = fun _ -> result |> Result.map (MeasuredData.EllipsometricData >> ImportProfiles.SeriesPoints)
        }

    let private recordingIntensity (result : Result<MeasuredData.IntensitySeries, MeasuredData.ExperimentDataError>) : int ref * ExperimentData.ExperimentDataProxy =
        let calls = ref 0
        let proxy : ExperimentData.ExperimentDataProxy =
            {
                mockIntensity result with
                    readMapped =
                        fun _ ->
                            calls.Value <- calls.Value + 1
                            intensityPoints result
            }
        calls, proxy

    /// The received collection, carrying the committed model's setups (spec 0050 S8.2).
    let private snapshotOf (setups : Setups.Setup list) (experiments : Experiments.Experiment list) : ExperimentCollectionStore.ExperimentCollectionSnapshot =
        { name = ExperimentCollectionStore.CollectionName "run-A"; experiments = experiments; setups = setups }

    let private contextOf
        (library : LibraryProxy)
        (proxy : ExperimentData.ExperimentDataProxy)
        (setups : Setups.Setup list)
        (experiments : Experiments.Experiment list)
        : SolverHandoffView.SolverHandoffContext =
        { library = library; experimentData = proxy; collection = snapshotOf setups experiments; requestClose = ignore }

    // ================================ pure per-experiment validation ================================

    [<Fact>]
    let ``a fully valid experiment validates green (HandoffReady) with the raw point count`` () =
        let library, setups, exp0 = baseExperiment (Some PresetIds.source600) (Some PresetIds.detectorIntensity)
        let exp = exp0 |> withFile "good.csv"
        let proxy = mockIntensity (Ok (intensitySeries [ 0.0, 1.0; 45.0, 0.6; 89.0, 0.2 ]))
        match SolverHandoffView.validateExperiment library proxy setups exp with
        | SolverHandoffView.HandoffReady (MeasuredData.MeasuredPointCount 3) -> ()
        | other -> Assert.Fail($"expected HandoffReady 3, got %A{other}")

    [<Fact>]
    let ``an unbound non-sample element yields UnspecifiedElements`` () =
        // A fresh inverse leaves the seeded source AND detector UNBOUND — only the sample may be
        // unspecified, so the source / detector flag the typed error.
        let library, setups, exp0 = baseExperiment None None
        let exp = exp0 |> withFile "good.csv"
        let proxy = mockIntensity (Ok (intensitySeries [ 0.0, 1.0 ]))
        match SolverHandoffView.validateExperiment library proxy setups exp with
        | SolverHandoffView.UnspecifiedElements reason ->
            Assert.Contains("every element except the sample", reason.value)
            // The slots are named by their roles in light order (spec 0050 S8.2).
            Assert.Contains("Source", reason.value)
            Assert.Contains("Detector 1", reason.value)
        | other -> Assert.Fail($"expected UnspecifiedElements, got %A{other}")

    [<Fact>]
    let ``an experiment whose setup the collection lacks yields UnspecifiedElements naming the setup`` () =
        let library, setups, exp0 = baseExperiment (Some PresetIds.source600) (Some PresetIds.detectorIntensity)
        let exp = exp0 |> withFile "good.csv"
        Assert.Equal<SetupName list>([ SetupName "Setup 1" ], setups |> List.map (fun setup -> setup.name))
        let proxy = mockIntensity (Ok (intensitySeries [ 0.0, 1.0; 89.0, 0.2 ]))
        match SolverHandoffView.validateExperiment library proxy [] exp with
        | SolverHandoffView.UnspecifiedElements reason -> Assert.Contains("'Setup 1' revision 1", reason.value)
        | other -> Assert.Fail($"expected UnspecifiedElements naming the setup, got %A{other}")
        let model : SolverHandoffView.SolverHandoffModel = (contextOf library proxy [] [ exp ]).initSolverHandoff
        match (List.exactlyOne model.rows).status with
        | SolverHandoffView.UnspecifiedElements reason -> Assert.Contains("'Setup 1' revision 1", reason.value)
        | other -> Assert.Fail($"expected the row to name the missing setup, got %A{other}")

    [<Fact>]
    let ``a missing data file yields MissingDataFile`` () =
        let library, setups, exp0 = baseExperiment (Some PresetIds.source600) (Some PresetIds.detectorIntensity)
        let exp = exp0 |> noFile
        let proxy = mockIntensity (Ok (intensitySeries [ 0.0, 1.0 ]))
        match SolverHandoffView.validateExperiment library proxy setups exp with
        | SolverHandoffView.MissingDataFile _ -> ()
        | other -> Assert.Fail($"expected MissingDataFile, got %A{other}")

    [<Fact>]
    let ``a range mismatch yields DataFileValidationError`` () =
        // The R2 experiment's range is 0…89°; an abscissa of 200 is a LOUD range mismatch.
        let library, setups, exp0 = baseExperiment (Some PresetIds.source600) (Some PresetIds.detectorIntensity)
        let exp = exp0 |> withFile "oor.csv"
        let proxy = mockIntensity (Ok (intensitySeries [ 0.0, 1.0; 200.0, 0.5 ]))
        match SolverHandoffView.validateExperiment library proxy setups exp with
        | SolverHandoffView.DataFileValidationError reason -> Assert.Contains("outside the experiment range", reason.value)
        | other -> Assert.Fail($"expected DataFileValidationError, got %A{other}")

    [<Fact>]
    let ``a malformed data file yields DataFileParseError`` () =
        let library, setups, exp0 = baseExperiment (Some PresetIds.source600) (Some PresetIds.detectorIntensity)
        let exp = exp0 |> withFile "bad.csv"
        let proxy = mockIntensity (Error (MeasuredData.MalformedDataFileErr "row 3 is not a valid X,Y pair"))
        match SolverHandoffView.validateExperiment library proxy setups exp with
        | SolverHandoffView.DataFileParseError reason -> Assert.Contains("row 3", reason.value)
        | other -> Assert.Fail($"expected DataFileParseError, got %A{other}")

    [<Fact>]
    let ``the detector kind selects the ellipsometric load path`` () =
        let library, setups, exp0 = baseExperiment (Some PresetIds.source600) (Some PresetIds.detectorEllipsometer)
        let exp = exp0 |> withMappedFile ImportProfileName.ellipsometricSeries "ellipso.csv"
        let ellipsometric =
            match (MeasuredData.DataFileText "wavelength_nm,psi_deg,delta_deg\n500,20,120\n600,25,110").parseEllipsometricSeries with
            | Ok series -> series
            | Error e -> failwith $"the fixture parser failed: %A{e}"
        let proxy = mockEllipsometric (Ok ellipsometric)
        match SolverHandoffView.validateExperiment library proxy setups exp with
        | SolverHandoffView.HandoffReady (MeasuredData.MeasuredPointCount 2) -> ()
        | other -> Assert.Fail($"expected HandoffReady 2 from the ellipsometric path, got %A{other}")

    [<Fact>]
    let ``validation runs the data load exactly once and reports the raw point count (no solver math, no normalization)`` () =
        let library, setups, exp0 = baseExperiment (Some PresetIds.source600) (Some PresetIds.detectorIntensity)
        let exp = exp0 |> withFile "good.csv"
        let raw = [ 0.0, 1.0; 30.0, 0.7; 60.0, 0.4; 89.0, 0.1 ]
        let calls, proxy = recordingIntensity (Ok (intensitySeries raw))
        match SolverHandoffView.validateExperiment library proxy setups exp with
        | SolverHandoffView.HandoffReady n ->
            Assert.Equal(List.length raw, n.value)          // the raw point count — nothing normalized away
            Assert.Equal(1, calls.Value)              // exactly one load — the only computation the screen runs
        | other -> Assert.Fail($"expected HandoffReady, got %A{other}")

    // ================================ init builds every row + carries the name ================================

    [<Fact>]
    let ``init validates every received experiment and carries the collection name`` () =
        let library, setups, valid0 = baseExperiment (Some PresetIds.source600) (Some PresetIds.detectorIntensity)
        let _, _, missing0 = baseExperiment (Some PresetIds.source600) (Some PresetIds.detectorIntensity)
        let _, _, oor0 = baseExperiment (Some PresetIds.source600) (Some PresetIds.detectorIntensity)
        let experiments =
            [ valid0 |> withId 1 |> withFile "good.csv"
              missing0 |> withId 2 |> noFile
              oor0 |> withId 3 |> withFile "oor.csv" ]
        let proxy =
            mockIntensityByPath
                (Map.ofList
                    [ KitRelativePath "data/good.csv", Ok (intensitySeries [ 0.0, 1.0; 89.0, 0.2 ])
                      KitRelativePath "data/oor.csv", Ok (intensitySeries [ 0.0, 1.0; 200.0, 0.5 ]) ])
        let model : SolverHandoffView.SolverHandoffModel = (contextOf library proxy setups experiments).initSolverHandoff
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
            let library, setups, valid0 = baseExperiment (Some PresetIds.source600) (Some PresetIds.detectorIntensity)
            let _, _, missing0 = baseExperiment (Some PresetIds.source600) (Some PresetIds.detectorIntensity)
            let experiments = [ valid0 |> withId 1 |> withFile "good.csv"; missing0 |> withId 2 |> noFile ]
            let proxy = mockIntensityByPath (Map.ofList [ KitRelativePath "data/good.csv", Ok (intensitySeries [ 0.0, 1.0; 89.0, 0.2 ]) ])
            let window = SolverHandoffWindow(library, proxy, snapshotOf setups experiments)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(matchesId ConstructorUiId.solverHandoffWindow.value window, "the window itself carries the SolverHandoffWindow id")
            Assert.True(isPresent window ConstructorUiId.solverHandoffSummaryList.value, "the summary list is missing")
            Assert.True(isPresent window ConstructorUiId.solverHandoffSolverMessage.value, "the solver-comes-later message is missing")
            Assert.Contains("separately", textOf window ConstructorUiId.solverHandoffSolverMessage.value)
            Assert.True(isPresent window (ConstructorUiId.solverHandoffStatus (Experiments.ExperimentId 1)).value, "experiment 1 status row is missing")
            Assert.True(isPresent window (ConstructorUiId.solverHandoffStatus (Experiments.ExperimentId 2)).value, "experiment 2 status row is missing")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a valid experiment shows a ready status; a missing file and a range mismatch show their typed message`` () =
        HeadlessSession.run (fun () ->
            let library, setups, valid0 = baseExperiment (Some PresetIds.source600) (Some PresetIds.detectorIntensity)
            let _, _, missing0 = baseExperiment (Some PresetIds.source600) (Some PresetIds.detectorIntensity)
            let _, _, oor0 = baseExperiment (Some PresetIds.source600) (Some PresetIds.detectorIntensity)
            let experiments =
                [ valid0 |> withId 1 |> withFile "good.csv"
                  missing0 |> withId 2 |> noFile
                  oor0 |> withId 3 |> withFile "oor.csv" ]
            let proxy =
                mockIntensityByPath
                    (Map.ofList
                        [ KitRelativePath "data/good.csv", Ok (intensitySeries [ 0.0, 1.0; 89.0, 0.2 ])
                          KitRelativePath "data/oor.csv", Ok (intensitySeries [ 0.0, 1.0; 200.0, 0.5 ]) ])
            let window = SolverHandoffWindow(library, proxy, snapshotOf setups experiments)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.Contains("Ready", textOf window (ConstructorUiId.solverHandoffStatus (Experiments.ExperimentId 1)).value)
            Assert.Contains("Missing data file", textOf window (ConstructorUiId.solverHandoffStatus (Experiments.ExperimentId 2)).value)
            Assert.Contains("Validation error", textOf window (ConstructorUiId.solverHandoffStatus (Experiments.ExperimentId 3)).value)
            window.Close())
