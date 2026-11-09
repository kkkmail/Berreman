namespace OpticalConstructor.Ui.Tests

open System.IO
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.FuncUI
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Controls
open OpticalConstructor.Ui
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Ui.TableAndElementRotationView
open OpticalConstructor.Tests

/// Inverse experiments persist in folder kits and revalidate measured data on open (spec 0050 S4.3).
module InverseConstructorTests =

    let private owner = FolderName "InverseConstructorTests"

    let private inverseWithKit (openKit : CatalogueImportContext -> OpenedKit) : TableAndElementRotationModel =
        let stores = TestAppScope.freshAppContext owner
        let model =
            {
                SceneTestFixture.inverseScene stores.library stores.experiments stores.materials stores.samples stores.categories with
                    catalogueImports = stores.catalogueImports
            }
        model.loadKit (openKit model.catalogueImports)

    let private inverse () : TableAndElementRotationModel =
        inverseWithKit (TestAppScope.openedKitIn owner)

    let private openFolder (folder : KitFolder) (imports : CatalogueImportContext) : OpenedKit =
        let author = KitAuthor "test"
        openKitAt folder ReadWriteKitAccess ClockProvider.system author imports
        |> Result.defaultWith (fun error -> failwith error.describe)

    let private inverseIn (folder : KitFolder) : TableAndElementRotationModel =
        inverseWithKit (openFolder folder)

    /// BuiltinCatalogue.sample's 200 nm glass-film entry id, bound as the inverse HINT.
    let private glassFilm200Id : string = (Library.SampleItem (BuiltinCatalogue.sample SampleIds.glassFilm200Id)).entryId.value

    let private theSample (m : TableAndElementRotationModel) : TestElement =
        m.elements |> List.find (fun e -> e.placement.catalogueKind = Placement.Sample)

    let private theDetector (m : TableAndElementRotationModel) : TestElement =
        m.elements |> List.find (fun e -> e.placement.catalogueKind = Placement.Detector)

    // ---- mock ExperimentDataProxy shapes ------------------------------------------------------------

    let private intensitySeries (points : (float * float) list) : MeasuredData.IntensitySeries =
        { points = points |> List.map (fun (x, i) -> { x = MeasuredData.MeasuredAbscissa x; intensity = MeasuredData.MeasuredIntensity i }) }

    let private mockIntensity (result : Result<MeasuredData.IntensitySeries, MeasuredData.ExperimentDataError>) : ExperimentData.ExperimentDataProxy =
        {
            attachDataFile =
                fun file ->
                    Ok
                        {
                            path = KitRelativePath $"data/{System.IO.Path.GetFileName file.value}"
                            contentHash = ContentHash "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
                        }
            attachmentStatus = fun _ -> Ok AttachedDataUnchanged
            tryLoadIntensity = fun _ -> result
            tryLoadEllipsometric = fun _ -> Error (MeasuredData.MalformedDataFileErr "the mock has no ellipsometric data")
        }

    let private mockEllipsometric (result : Result<MeasuredData.EllipsometricSeries, MeasuredData.ExperimentDataError>) : ExperimentData.ExperimentDataProxy =
        {
            mockIntensity (Error (MeasuredData.MalformedDataFileErr "the mock has no intensity data")) with
                tryLoadEllipsometric = fun _ -> result
        }

    /// Commits one R2 intensity experiment in the supplied inverse model (spec 0050 S4.3).
    let private withIntensityExperiment (m0 : TableAndElementRotationModel) : TableAndElementRotationModel * Experiments.ExperimentId =
        let sample = theSample m0
        let m1 =
            m0
            |> updateTableAndElementRotation (ExpChooseElement sample.id)
            |> updateTableAndElementRotation (ExpChooseVariable ExperimentControls.VaryR2)
            |> updateTableAndElementRotation ExpCommit
        m1, (List.head m1.experimentCollection.experiments).id

    // ================================ inverse init + hint gate ================================

    [<Fact>]
    let ``initInverse seeds an unbound sample and hides the experiment chart with no hint`` () =
        let m = inverse ()
        Assert.Equal(InverseConstructor, m.constructorMode)
        let sample = theSample m
        match sample.placement.valueId with
        | Some entry -> Assert.Fail($"the inverse sample must start UNBOUND, but was bound to %s{entry.value}")
        | None -> ()
        // No hint ⇒ the forward experiment chart surface is absent.
        Assert.False(m.experimentChartVisible, "the chart surface must be absent while the sample is unbound")

    [<Fact>]
    let ``the forward Main constructor always shows the chart surface (regression)`` () =
        // The gate is inverse-only: the ordinary Main scene keeps the chart surface present.
        Assert.True((SceneTestFixture.mainScene ()).experimentChartVisible)

    [<Fact>]
    let ``binding a hint sample restores the inverse experiment chart`` () =
        let m0 = inverse ()
        let sample = theSample m0
        // A draft that varies the sample over R2 (an intensity sweep that re-solves the sample).
        let m1 =
            m0
            |> updateTableAndElementRotation (ExpChooseElement sample.id)
            |> updateTableAndElementRotation (ExpChooseVariable ExperimentControls.VaryR2)
        // No hint (sample unbound) ⇒ the chart surface is gated off, even with a full draft.
        Assert.False(m1.experimentChartVisible)
        Assert.Empty((m1.experimentState).series)
        // Bind a HINT sample through the SAME targeted bind path the Library Select state uses.
        let m2 = m1 |> updateTableAndElementRotation (BindValueIdTo (sample.id, Placement.ElementValueId glassFilm200Id))
        Assert.True(m2.experimentChartVisible, "a bound hint sample must restore the chart surface")
        Assert.NotEmpty((m2.experimentResult).series)

    // ================================ per-experiment file attach status ================================

    [<Fact>]
    let ``attaching a valid intensity file records a typed Loaded status and the attachment`` () =
        let proxy = mockIntensity (Ok (intensitySeries [ 0.0, 1.0; 45.0, 0.6; 89.0, 0.2 ]))
        let m, expId = withIntensityExperiment { inverse () with experimentData = proxy }
        let m1 = m |> updateTableAndElementRotation (AttachDataFileTo (expId, FileName "measured.csv"))
        match m1.experimentDataStatus |> Map.tryFind expId with
        | Some (DataFileValidated (MeasuredData.MeasuredPointCount 3)) -> ()
        | other -> Assert.Fail($"expected DataFileValidated (MeasuredData.MeasuredPointCount 3), got %A{other}")
        let exp = List.head m1.experimentCollection.experiments
        Assert.Equal<string option>(Some "data/measured.csv", exp.dataFileOpt |> Option.map (fun attachment -> attachment.path.value))

    [<Fact>]
    let ``attaching a malformed intensity file records a typed parse-error status`` () =
        let proxy = mockIntensity (Error (MeasuredData.MalformedDataFileErr "row 3 is not a valid X,Y pair"))
        let m, expId = withIntensityExperiment { inverse () with experimentData = proxy }
        let m1 = m |> updateTableAndElementRotation (AttachDataFileTo (expId, FileName "bad.csv"))
        match m1.experimentDataStatus |> Map.tryFind expId with
        | Some (AttachedFileParseError reason) -> Assert.Contains("row 3", reason.value)
        | other -> Assert.Fail($"expected AttachedFileParseError, got %A{other}")

    [<Fact>]
    let ``attaching an out-of-range intensity file records a typed validation-error status`` () =
        // The R2 experiment's range is 0…89°; an abscissa of 200 is a LOUD range mismatch.
        let proxy = mockIntensity (Ok (intensitySeries [ 0.0, 1.0; 200.0, 0.5 ]))
        let m, expId = withIntensityExperiment { inverse () with experimentData = proxy }
        let m1 = m |> updateTableAndElementRotation (AttachDataFileTo (expId, FileName "oor.csv"))
        match m1.experimentDataStatus |> Map.tryFind expId with
        | Some (AttachedFileValidationError reason) -> Assert.Contains("outside the experiment range", reason.value)
        | other -> Assert.Fail($"expected AttachedFileValidationError, got %A{other}")

    [<Fact>]
    let ``the detector kind selects the ellipsometric load path`` () =
        // Bind the detector to the ellipsometer, commit an experiment, then attach an ellipsometric
        // file — the load must go through `tryLoadEllipsometric` and validate as an ellipsometric shape.
        let ellipsometric =
            match (MeasuredData.DataFileText "wavelength_nm,psi_deg,delta_deg\n500,20,120\n600,25,110").parseEllipsometricSeries with
            | Ok series -> series
            | Error e -> failwith $"the fixture parser failed: %A{e}"
        let m0 = { inverse () with experimentData = mockEllipsometric (Ok ellipsometric) }
        let sample = theSample m0
        let detector = theDetector m0
        let m1 =
            m0
            |> updateTableAndElementRotation (BindValueIdTo (detector.id, PresetIds.detectorEllipsometer))
            |> updateTableAndElementRotation (ExpChooseElement sample.id)
            |> updateTableAndElementRotation (ExpChooseVariable ExperimentControls.VaryR2)
            |> updateTableAndElementRotation ExpCommit
        let expId = (List.head m1.experimentCollection.experiments).id
        let m2 = m1 |> updateTableAndElementRotation (AttachDataFileTo (expId, FileName "ellipso.csv"))
        match m2.experimentDataStatus |> Map.tryFind expId with
        | Some (DataFileValidated (MeasuredData.MeasuredPointCount 2)) -> ()
        | other -> Assert.Fail($"expected DataFileValidated (MeasuredData.MeasuredPointCount 2) from the ellipsometric path, got %A{other}")

    [<Fact>]
    let ``a picked file the kit cannot attach records a typed attach-failed status and no attachment`` () =
        let readyProxy = mockIntensity (Ok (intensitySeries [ 0.0, 1.0; 45.0, 0.5 ]))
        let attachError = KitIoErr "picked measured.csv could not be read"
        let picked = FileName "measured.csv"
        let refusingProxy : ExperimentData.ExperimentDataProxy =
            {
                readyProxy with
                    attachDataFile =
                        fun file ->
                            Assert.Equal(picked, file)
                            Error attachError
                    tryLoadIntensity = fun _ -> failwith "an attachment failure must not load intensity data"
                    tryLoadEllipsometric = fun _ -> failwith "an attachment failure must not load ellipsometric data"
            }
        let model, expId = withIntensityExperiment { inverse () with experimentData = refusingProxy }
        let failed = model |> updateTableAndElementRotation (AttachDataFileTo (expId, picked))
        Assert.Equal<DataAttachment option>(None, (List.head failed.experimentCollection.experiments).dataFileOpt)
        Assert.Equal(model.experimentCollection, failed.experimentCollection)
        match Map.tryFind expId failed.experimentDataStatus with
        | Some (DataFileAttachFailed reason as status) ->
            Assert.Equal(attachError.describe, reason.value)
            Assert.Contains("Attach failed", status.caption.value)
            Assert.Contains(attachError.describe, status.caption.value)
        | other -> Assert.Fail($"expected DataFileAttachFailed, got %A{other}")

        let attached =
            {
                model with experimentData = readyProxy
            }
            |> updateTableAndElementRotation (AttachDataFileTo (expId, picked))
        let refusedReplacement =
            {
                attached with experimentData = refusingProxy
            }
            |> updateTableAndElementRotation (AttachDataFileTo (expId, picked))
        Assert.Equal(attached.experimentCollection, refusedReplacement.experimentCollection)
        match Map.tryFind expId refusedReplacement.experimentDataStatus with
        | Some (DataFileAttachFailed reason) -> Assert.Equal(attachError.describe, reason.value)
        | other -> Assert.Fail($"expected DataFileAttachFailed for replacement, got %A{other}")

    [<Fact>]
    let ``committed experiments and their attachments are kept in the kit and reopen with it`` () =
        let folder = TestAppScope.freshKitFolder owner
        let proxy = mockIntensity (Ok (intensitySeries [ 0.0, 1.0; 45.0, 0.5 ]))
        let mBuilt, expId = withIntensityExperiment { inverseIn folder with experimentData = proxy }
        let mSaved = mBuilt |> updateTableAndElementRotation (AttachDataFileTo (expId, FileName "run.csv"))
        Assert.Equal<Experiments.Experiment list>(mSaved.experimentCollection.experiments, mSaved.kit.proxy.readExperiments () |> SceneTestFixture.kitSuccess)
        mSaved.kit.close () |> SceneTestFixture.kitSuccess
        let mLoaded = inverseIn folder
        Assert.Equal(1, List.length mLoaded.experimentCollection.experiments)
        Assert.Equal<Experiments.Experiment list>(mSaved.experimentCollection.experiments, mLoaded.experimentCollection.experiments)
        let loaded = List.head mLoaded.experimentCollection.experiments
        Assert.Equal<string option>(Some "data/run.csv", loaded.dataFileOpt |> Option.map (fun attachment -> attachment.path.value))
        mLoaded.kit.close () |> SceneTestFixture.kitSuccess

    [<Fact>]
    let ``opening a kit re-validates an attachment whose data is unchanged`` () =
        let proxy = mockIntensity (Ok (intensitySeries [ 0.0, 1.0; 45.0, 0.5 ]))
        let folder = TestAppScope.freshKitFolder owner
        let built, expId = withIntensityExperiment { inverseIn folder with experimentData = proxy }
        let picked = FileName "unchanged.csv"
        let saved =
            built
            |> updateTableAndElementRotation (AttachDataFileTo (expId, picked))
            |> updateTableAndElementRotation ExpNew
            |> updateTableAndElementRotation (ExpChooseElement (theSample built).id)
            |> updateTableAndElementRotation (ExpChooseVariable ExperimentControls.VaryR2)
            |> updateTableAndElementRotation ExpCommit
        let attachment =
            match (List.head saved.experimentCollection.experiments).dataFileOpt with
            | Some attached -> attached
            | None -> failwith "the saved experiment must carry its attachment"
        let unattachedId = (List.last saved.experimentCollection.experiments).id
        Assert.NotEqual(expId, unattachedId)

        let loadWith (result : Result<MeasuredData.IntensitySeries, MeasuredData.ExperimentDataError>) : TableAndElementRotationModel =
            let inspected = ResizeArray<DataAttachment>()
            let loadedAttachments = ResizeArray<DataAttachment>()
            let loadingProxy =
                {
                    proxy with
                        attachDataFile = fun _ -> failwith "opening a kit must not re-attach its files"
                        attachmentStatus =
                            fun attached ->
                                inspected.Add attached
                                Ok AttachedDataUnchanged
                        tryLoadIntensity =
                            fun attached ->
                                loadedAttachments.Add attached
                                result
                }
            let opened = openFolder folder saved.catalogueImports
            let loaded = saved.loadKit { opened with experimentData = loadingProxy }
            loaded.kit.close () |> SceneTestFixture.kitSuccess
            Assert.Equal<DataAttachment list>([ attachment ], inspected |> Seq.toList)
            Assert.Equal<DataAttachment list>([ attachment ], loadedAttachments |> Seq.toList)
            Assert.Equal<Experiments.Experiment list>(saved.experimentCollection.experiments, loaded.experimentCollection.experiments)
            Assert.Equal(1, Map.count loaded.experimentDataStatus)
            Assert.Equal<ExperimentFileStatus option>(None, Map.tryFind unattachedId loaded.experimentDataStatus)
            loaded

        saved.kit.close () |> SceneTestFixture.kitSuccess
        let loaded = loadWith (Ok (intensitySeries [ 0.0, 1.0; 45.0, 0.6; 89.0, 0.2 ]))
        let expected = DataFileValidated (MeasuredData.MeasuredPointCount 3)
        Assert.Equal<ExperimentFileStatus option>(Some expected, Map.tryFind expId loaded.experimentDataStatus)

        let parseError = MeasuredData.MalformedDataFileErr "row 3 is not a valid X,Y pair"
        let malformed = loadWith (Error parseError)
        Assert.Equal<ExperimentFileStatus option>(Some (AttachedFileParseError parseError.caption), Map.tryFind expId malformed.experimentDataStatus)

        let outOfRange = loadWith (Ok (intensitySeries [ 0.0, 1.0; 200.0, 0.5 ]))
        match Map.tryFind expId outOfRange.experimentDataStatus with
        | Some (AttachedFileValidationError reason) -> Assert.Contains("outside the experiment range", reason.value)
        | other -> Assert.Fail($"expected AttachedFileValidationError on open, got %A{other}")

    [<Fact>]
    let ``opening a kit flags an attachment whose data changed since attach or is missing`` () =
        let proxy = mockIntensity (Ok (intensitySeries [ 0.0, 1.0; 45.0, 0.5 ]))
        let folder = TestAppScope.freshKitFolder owner
        let built, firstId = withIntensityExperiment { inverseIn folder with experimentData = proxy }
        let firstFile = FileName "a.csv"
        let secondFile = FileName "b.csv"
        let firstPath = KitRelativePath "data/a.csv"
        let secondPath = KitRelativePath "data/b.csv"
        let withSecond =
            built
            |> updateTableAndElementRotation (AttachDataFileTo (firstId, firstFile))
            |> updateTableAndElementRotation ExpNew
            |> updateTableAndElementRotation (ExpChooseElement (theSample built).id)
            |> updateTableAndElementRotation (ExpChooseVariable ExperimentControls.VaryR2)
            |> updateTableAndElementRotation ExpCommit
        let secondId = (List.last withSecond.experimentCollection.experiments).id
        Assert.NotEqual(firstId, secondId)
        let saved =
            withSecond
            |> updateTableAndElementRotation (AttachDataFileTo (secondId, secondFile))
        let attachments = saved.experimentCollection.experiments |> List.choose (fun experiment -> experiment.dataFileOpt)
        Assert.Equal<KitRelativePath list>([ firstPath; secondPath ], attachments |> List.map (fun attachment -> attachment.path))
        let currentHash = ContentHash "abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789"
        let inspected = ResizeArray<DataAttachment>()
        let loadingProxy =
            {
                proxy with
                    attachDataFile = fun _ -> failwith "opening a kit must preserve its recorded attachments"
                    attachmentStatus =
                        fun attachment ->
                            inspected.Add attachment
                            if attachment.path = firstPath then Ok (AttachedDataChanged currentHash)
                            elif attachment.path = secondPath then Ok AttachedDataMissing
                            else failwith "only the saved attachments may be inspected"
                    tryLoadIntensity = fun _ -> failwith "changed or missing data must not be validated"
                    tryLoadEllipsometric = fun _ -> failwith "changed or missing data must not be validated"
            }
        saved.kit.close () |> SceneTestFixture.kitSuccess
        let opened = openFolder folder saved.catalogueImports
        let loaded = saved.loadKit { opened with experimentData = loadingProxy }
        let missing = DataFileMissingFromKit secondPath.toControlCaption
        Assert.Equal<DataAttachment list>(attachments, inspected |> Seq.toList)
        Assert.Equal<Map<Experiments.ExperimentId, ExperimentFileStatus>>(
            Map.ofList [ firstId, DataFileChangedSinceAttach; secondId, missing ], loaded.experimentDataStatus)
        Assert.Equal("Data changed since attach — attach it again to validate it", DataFileChangedSinceAttach.caption.value)
        Assert.Equal("Data file missing — data/b.csv", missing.caption.value)
        Assert.Equal<Experiments.Experiment list>(saved.experimentCollection.experiments, loaded.experimentCollection.experiments)

        let readError = KitIoErr "cannot read data/a.csv or data/b.csv"
        let unreadable =
            saved.loadKit
                {
                    opened with
                        experimentData =
                            {
                                loadingProxy with attachmentStatus = fun _ -> Error readError
                            }
                }
        opened.library.kit.close () |> SceneTestFixture.kitSuccess
        let readFailure = AttachedFileParseError readError.caption
        Assert.Equal<Map<Experiments.ExperimentId, ExperimentFileStatus>>(
            Map.ofList [ firstId, readFailure; secondId, readFailure ], unreadable.experimentDataStatus)
        Assert.Equal<Experiments.Experiment list>(saved.experimentCollection.experiments, unreadable.experimentCollection.experiments)

    [<Fact>]
    let ``a commit, an attach and a removal write the kit's experiment files`` () =
        let folder = TestAppScope.freshKitFolder owner
        let model, expId = withIntensityExperiment (inverseIn folder)
        let file = FileName "E1.experiment-json"
        let filePath = file.combine folder.value
        Assert.True(File.Exists filePath.value)
        Assert.Equal<Experiments.Experiment list>(model.experimentCollection.experiments, model.kit.proxy.readExperiments () |> SceneTestFixture.kitSuccess)
        let dataFile = FileName "data/measured.csv"
        let picked = dataFile.combine folder.value
        File.WriteAllText(picked.value, "x,intensity\n0,1\n45,0.5\n89,0.2\n")
        let before = File.ReadAllText filePath.value
        let attached = model |> updateTableAndElementRotation (AttachDataFileTo (expId, picked))
        Assert.NotEqual<string>(before, File.ReadAllText filePath.value)
        let experiment = List.head attached.experimentCollection.experiments
        match experiment.dataFileOpt with
        | Some attachment -> Assert.Equal("data/measured.csv", attachment.path.value)
        | None -> Assert.Fail("The kit file must carry the new attachment.")
        Assert.Equal<Experiments.Experiment list>(attached.experimentCollection.experiments, attached.kit.proxy.readExperiments () |> SceneTestFixture.kitSuccess)
        let edited =
            attached
            |> updateTableAndElementRotation (ExpEdit expId)
            |> updateTableAndElementRotation (ExpSetRangePoints (ExperimentControls.ExperimentPointCount 7))
        Assert.Equal<Experiments.Experiment list>(attached.experimentCollection.experiments, edited.kit.proxy.readExperiments () |> SceneTestFixture.kitSuccess)
        let updated = edited |> updateTableAndElementRotation ExpCommit
        Assert.Equal<Experiments.Experiment list>(updated.experimentCollection.experiments, updated.kit.proxy.readExperiments () |> SceneTestFixture.kitSuccess)
        Assert.NotEqual<Experiments.Experiment list>(attached.experimentCollection.experiments, updated.experimentCollection.experiments)
        let removed = updated |> updateTableAndElementRotation (ExpRemove expId)
        Assert.False(File.Exists filePath.value)
        Assert.Empty(removed.experimentCollection.experiments)
        Assert.Empty(removed.kit.proxy.readExperiments () |> SceneTestFixture.kitSuccess)
        Assert.Equal<ControlCaption option>(None, removed.kitStatus)
        removed.kit.close () |> SceneTestFixture.kitSuccess

    [<Fact>]
    let ``a kit write that fails keeps the experiment and shows why`` () =
        let initial = inverse ()
        let readOnly =
            {
                initial with
                    kit =
                        {
                            initial.kit with
                                proxy = initial.kit.proxy.readOnlyView
                        }
            }
        let sample = theSample readOnly
        let draft =
            readOnly
            |> updateTableAndElementRotation (ExpChooseElement sample.id)
            |> updateTableAndElementRotation (ExpChooseVariable ExperimentControls.VaryR2)
        let failed = draft |> updateTableAndElementRotation ExpCommit
        Assert.Equal(draft.experimentCollection, failed.experimentCollection)
        let refusal = KitReadOnlyErr "writeKindFile"
        match failed.kitStatus with
        | Some status ->
            Assert.StartsWith("Not saved to the kit — ", status.value)
            Assert.Contains(refusal.describe, status.value)
        | None -> Assert.Fail("A refused commit must explain why it was not saved.")
        Assert.Empty(initial.kit.proxy.readExperiments () |> SceneTestFixture.kitSuccess)

        let data = mockIntensity (Ok (intensitySeries [ 0.0, 1.0; 45.0, 0.5 ]))
        let committed, expId = withIntensityExperiment { initial with experimentData = data }
        let attached = committed |> updateTableAndElementRotation (AttachDataFileTo (expId, FileName "original.csv"))
        let protectedModel =
            {
                attached with
                    kit =
                        {
                            attached.kit with
                                proxy = attached.kit.proxy.readOnlyView
                        }
            }
        let edited = protectedModel |> updateTableAndElementRotation (ExpSetRangePoints (ExperimentControls.ExperimentPointCount 7))
        let refusedEdit = edited |> updateTableAndElementRotation ExpCommit
        Assert.Equal(edited.experimentCollection, refusedEdit.experimentCollection)
        let refusedAttach = protectedModel |> updateTableAndElementRotation (AttachDataFileTo (expId, FileName "replacement.csv"))
        Assert.Equal(protectedModel.experimentCollection, refusedAttach.experimentCollection)
        Assert.Equal<Map<Experiments.ExperimentId, ExperimentFileStatus>>(protectedModel.experimentDataStatus, refusedAttach.experimentDataStatus)
        let refusedRemove = protectedModel |> updateTableAndElementRotation (ExpRemove expId)
        Assert.Equal(protectedModel.experimentCollection, refusedRemove.experimentCollection)
        for refused in [ refusedEdit; refusedAttach; refusedRemove ] do
            match refused.kitStatus with
            | Some status -> Assert.Contains("read-only", status.value)
            | None -> Assert.Fail("A refused kit write must show its reason.")
        Assert.Equal<Experiments.Experiment list>(attached.experimentCollection.experiments, initial.kit.proxy.readExperiments () |> SceneTestFixture.kitSuccess)
        initial.kit.proxy.deleteExperiment expId |> SceneTestFixture.kitSuccess
        let missing = attached |> updateTableAndElementRotation (ExpRemove expId)
        Assert.Empty(missing.experimentCollection.experiments)
        Assert.Equal<ControlCaption option>(None, missing.kitStatus)
        initial.kit.close () |> SceneTestFixture.kitSuccess

    // ================================ headless render (gate ui-smoke) ================================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the inverse workbench renders the collection builder without throwing`` () =
        HeadlessSession.run (fun () ->
            let m0 = { inverse () with experimentData = mockIntensity (Ok (intensitySeries [ 0.0, 1.0; 45.0, 0.5 ])) }
            let sample = theSample m0
            let expId0Model =
                m0
                |> updateTableAndElementRotation (ExpChooseElement sample.id)
                |> updateTableAndElementRotation (ExpChooseVariable ExperimentControls.VaryR2)
                |> updateTableAndElementRotation ExpCommit
            let expId = (List.head expId0Model.experimentCollection.experiments).id
            let seed =
                expId0Model
                |> updateTableAndElementRotation (AttachDataFileTo (expId, FileName "m.csv"))
                |> updateTableAndElementRotation (SelectBay BayNames.experiments)
            let window = Window(Width = 1000.0, Height = 980.0)
            window.Content <- Component(fun _ -> mainView seed ignore)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            window.Close())
