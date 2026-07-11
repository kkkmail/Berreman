namespace OpticalConstructor.Ui.Tests

open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.FuncUI
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

/// Spec 0038 Part L (step 037) — the inverse constructor state and the experiment-collection builder,
/// driven over MOCK proxies (the file picker is off the tested path; the tests dispatch
/// `AttachDataFileTo` directly). Four acceptance shapes: `initInverse` opens with an UNBOUND sample and
/// NO experiment chart; binding a HINT sample restores the chart; attaching a file records the typed
/// per-experiment parse/validation status; a named collection round-trips through save / list / load.
module InverseConstructorTests =

    // ---- fresh, isolated in-memory stores (the App composition order) -------------------------------

    let private freshStores () : MaterialProxy * SampleProxy * CategoryProxy =
        let samples = SampleProxy.createInMemory VersionsInUse.empty
        let materials = MaterialProxy.createInMemory (samplesReferencing samples) VersionsInUse.empty
        let categories = CategoryProxy.createInMemory (materialsReferencingCategory materials)
        materials, samples, categories

    /// A fresh inverse Main model over isolated stores (default experiment proxies).
    let private inverse () : Model =
        let materials, samples, categories = freshStores ()
        initInverse (Library.createInMemory ()) (Experiments.createInMemory ()) materials samples categories

    /// The seeded 200 nm glass-film sample entry id (bound as the inverse HINT).
    let private glassFilm200Id : string = (Library.SampleItem Library.SeedSamples.glassFilm200).entryId

    let private theSample (m : Model) : TestElement =
        m.elements |> List.find (fun e -> e.placement.catalogueKind = Placement.Sample)

    let private theDetector (m : Model) : TestElement =
        m.elements |> List.find (fun e -> e.placement.catalogueKind = Placement.Detector)

    // ---- mock ExperimentDataProxy shapes ------------------------------------------------------------

    let private intensitySeries (points : (float * float) list) : MeasuredData.IntensitySeries =
        { points = points |> List.map (fun (x, i) -> { x = MeasuredData.MeasuredAbscissa x; intensity = MeasuredData.MeasuredIntensity i }) }

    /// A data proxy that answers intensity loads with `result` and refuses ellipsometric loads.
    let private mockIntensity (result : Result<MeasuredData.IntensitySeries, MeasuredData.ExperimentDataError>) : ExperimentData.ExperimentDataProxy =
        { tryLoadIntensity = fun _ -> result
          tryLoadEllipsometric = fun _ -> Error (MeasuredData.MalformedDataFile "the mock has no ellipsometric data") }

    /// A data proxy that answers ellipsometric loads with `result` and refuses intensity loads.
    let private mockEllipsometric (result : Result<MeasuredData.EllipsometricSeries, MeasuredData.ExperimentDataError>) : ExperimentData.ExperimentDataProxy =
        { tryLoadIntensity = fun _ -> Error (MeasuredData.MalformedDataFile "the mock has no intensity data")
          tryLoadEllipsometric = fun _ -> result }

    /// An inverse model whose data proxy is the given mock, with ONE committed intensity experiment
    /// (the sample varied over R2 — an intensity sweep; the detector defaults to Intensity).
    let private withIntensityExperiment (proxy : ExperimentData.ExperimentDataProxy) : Model * Experiments.ExperimentId =
        let m0 = { inverse () with experimentData = proxy }
        let sample = theSample m0
        let m1 =
            m0
            |> update (ExpChooseElement sample.id.value)
            |> update (ExpChooseVariable ExperimentControls.VaryR2)
            |> update ExpCommit
        m1, (List.head m1.experimentCollection.experiments).id

    // ================================ inverse init + hint gate ================================

    [<Fact>]
    let ``initInverse seeds an unbound sample and hides the experiment chart with no hint`` () =
        let m = inverse ()
        Assert.Equal(InverseConstructor, m.constructorMode)
        let sample = theSample m
        match sample.placement.valueId with
        | Some entry -> Assert.Fail($"the inverse sample must start UNBOUND, but was bound to %s{entry}")
        | None -> ()
        // No hint ⇒ the forward experiment chart surface is absent.
        Assert.False(experimentChartVisible m, "the chart surface must be absent while the sample is unbound")

    [<Fact>]
    let ``the forward Main constructor always shows the chart surface (regression)`` () =
        // The gate is inverse-only: the ordinary Main scene keeps the chart surface present.
        Assert.True(experimentChartVisible (initMain ()))

    [<Fact>]
    let ``binding a hint sample restores the inverse experiment chart`` () =
        let m0 = inverse ()
        let sample = theSample m0
        // A draft that varies the sample over R2 (an intensity sweep that re-solves the sample).
        let m1 =
            m0
            |> update (ExpChooseElement sample.id.value)
            |> update (ExpChooseVariable ExperimentControls.VaryR2)
        // No hint (sample unbound) ⇒ the chart surface is gated off, even with a full draft.
        Assert.False(experimentChartVisible m1)
        Assert.Empty((experimentState m1).series)
        // Bind a HINT sample through the SAME targeted bind path the Library Select state uses.
        let m2 = m1 |> update (BindValueIdTo (sample.id, glassFilm200Id))
        Assert.True(experimentChartVisible m2, "a bound hint sample must restore the chart surface")
        Assert.NotEmpty((experimentResult m2).series)

    // ================================ per-experiment file attach status ================================

    [<Fact>]
    let ``attaching a valid intensity file records a typed Loaded status and the attachment`` () =
        let proxy = mockIntensity (Ok (intensitySeries [ 0.0, 1.0; 45.0, 0.6; 89.0, 0.2 ]))
        let m, expId = withIntensityExperiment proxy
        let m1 = m |> update (AttachDataFileTo (expId, Experiments.DataFilePath "measured.csv"))
        match m1.experimentDataStatus |> Map.tryFind expId.value with
        | Some (DataFileValidated 3) -> ()
        | other -> Assert.Fail($"expected DataFileValidated 3, got %A{other}")
        // the picked path is recorded on the experiment (rides the collection save/load).
        let exp = List.head m1.experimentCollection.experiments
        Assert.Equal<string option>(Some "measured.csv", exp.dataFileOpt |> Option.map (fun p -> p.value))

    [<Fact>]
    let ``attaching a malformed intensity file records a typed parse-error status`` () =
        let proxy = mockIntensity (Error (MeasuredData.MalformedDataFile "row 3 is not a valid X,Y pair"))
        let m, expId = withIntensityExperiment proxy
        let m1 = m |> update (AttachDataFileTo (expId, Experiments.DataFilePath "bad.csv"))
        match m1.experimentDataStatus |> Map.tryFind expId.value with
        | Some (DataFileParseError reason) -> Assert.Contains("row 3", reason)
        | other -> Assert.Fail($"expected DataFileParseError, got %A{other}")

    [<Fact>]
    let ``attaching an out-of-range intensity file records a typed validation-error status`` () =
        // The R2 experiment's range is 0…89°; an abscissa of 200 is a LOUD range mismatch.
        let proxy = mockIntensity (Ok (intensitySeries [ 0.0, 1.0; 200.0, 0.5 ]))
        let m, expId = withIntensityExperiment proxy
        let m1 = m |> update (AttachDataFileTo (expId, Experiments.DataFilePath "oor.csv"))
        match m1.experimentDataStatus |> Map.tryFind expId.value with
        | Some (DataFileValidationError reason) -> Assert.Contains("outside the experiment range", reason)
        | other -> Assert.Fail($"expected DataFileValidationError, got %A{other}")

    [<Fact>]
    let ``the detector kind selects the ellipsometric load path`` () =
        // Bind the detector to the ellipsometer, commit an experiment, then attach an ellipsometric
        // file — the load must go through `tryLoadEllipsometric` and validate as an ellipsometric shape.
        let ellipsometric =
            match MeasuredData.parseEllipsometricSeries "wavelength_nm,psi_deg,delta_deg\n500,20,120\n600,25,110" with
            | Ok series -> series
            | Error e -> failwith $"the fixture parser failed: %A{e}"
        let m0 = { inverse () with experimentData = mockEllipsometric (Ok ellipsometric) }
        let sample = theSample m0
        let detector = theDetector m0
        let m1 =
            m0
            |> update (BindValueIdTo (detector.id, "det-ellipsometer"))
            |> update (ExpChooseElement sample.id.value)
            |> update (ExpChooseVariable ExperimentControls.VaryR2)
            |> update ExpCommit
        let expId = (List.head m1.experimentCollection.experiments).id
        let m2 = m1 |> update (AttachDataFileTo (expId, Experiments.DataFilePath "ellipso.csv"))
        match m2.experimentDataStatus |> Map.tryFind expId.value with
        | Some (DataFileValidated 2) -> ()
        | other -> Assert.Fail($"expected DataFileValidated 2 from the ellipsometric path, got %A{other}")

    // ================================ named collection save / list / load ================================

    [<Fact>]
    let ``a named collection round-trips through save, list, and load`` () =
        // Build a one-experiment collection with an attached file, over a shared collection proxy.
        let proxy = mockIntensity (Ok (intensitySeries [ 0.0, 1.0; 45.0, 0.5 ]))
        let mBuilt, expId = withIntensityExperiment proxy
        let mSaved =
            mBuilt
            |> update (AttachDataFileTo (expId, Experiments.DataFilePath "run.csv"))
            |> update (CollectionSetName "run-A")
            |> update CollectionSave
        // list: the saved name appears.
        Assert.Contains("run-A", mSaved.savedCollections |> List.map (fun n -> n.value))
        // load into a FRESH inverse model sharing the SAME (stateful) collection proxy.
        let mFresh = { inverse () with experimentCollections = mSaved.experimentCollections }
        let mLoaded = mFresh |> update (CollectionLoad "run-A")
        Assert.Equal(1, List.length mLoaded.experimentCollection.experiments)
        // the experiments round-trip value-identically (setup + varied + attachment).
        Assert.Equal<Experiments.Experiment list>(mSaved.experimentCollection.experiments, mLoaded.experimentCollection.experiments)
        let loaded = List.head mLoaded.experimentCollection.experiments
        Assert.Equal<string option>(Some "run.csv", loaded.dataFileOpt |> Option.map (fun p -> p.value))

    [<Fact>]
    let ``saving a collection with a blank name reports a typed status and stores nothing`` () =
        let m1 = inverse () |> update (CollectionSetName "   ") |> update CollectionSave
        match m1.collectionStatus with
        | Some text -> Assert.Contains("Save failed", text)
        | None -> Assert.Fail("a blank collection name must surface a typed save-failed status")
        Assert.Empty(m1.savedCollections)

    [<Fact>]
    let ``loading an unknown collection reports a typed status and leaves the live collection alone`` () =
        let m0 = inverse ()
        let m1 = m0 |> update (CollectionLoad "no-such-collection")
        match m1.collectionStatus with
        | Some text -> Assert.Contains("No collection named", text)
        | None -> Assert.Fail("an unknown collection load must surface a typed status")
        Assert.Empty(m1.experimentCollection.experiments)

    // ================================ headless render (gate ui-smoke) ================================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the inverse workbench renders the collection builder without throwing`` () =
        HeadlessSession.run (fun () ->
            let m0 = { inverse () with experimentData = mockIntensity (Ok (intensitySeries [ 0.0, 1.0; 45.0, 0.5 ])) }
            let sample = theSample m0
            let expId0Model =
                m0
                |> update (ExpChooseElement sample.id.value)
                |> update (ExpChooseVariable ExperimentControls.VaryR2)
                |> update ExpCommit
            let expId = (List.head expId0Model.experimentCollection.experiments).id
            let seed =
                expId0Model
                |> update (AttachDataFileTo (expId, Experiments.DataFilePath "m.csv"))
                |> update (CollectionSetName "run-A")
                |> update CollectionSave
                |> update (SelectBay BayNames.experiments)
            let window = Window(Width = 1000.0, Height = 980.0)
            window.Content <- Component(fun _ -> mainView seed ignore)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            window.Close())
