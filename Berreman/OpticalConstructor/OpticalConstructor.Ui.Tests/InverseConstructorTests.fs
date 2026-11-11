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
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.ExperimentDataLoad
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

    let private theSample (m : TableAndElementRotationModel) : Setups.SetupSlot =
        m.elements |> List.find (fun e -> e.placement.catalogueKind = Placement.Sample)

    let private theDetector (m : TableAndElementRotationModel) : Setups.SetupSlot =
        m.elements |> List.find (fun e -> e.placement.catalogueKind = Placement.Detector)

    // ---- mock ExperimentDataProxy shapes ------------------------------------------------------------

    let private intensitySeries (points : (float * float) list) : MeasuredData.IntensitySeries =
        { points = points |> List.map (fun (x, i) -> { x = MeasuredData.MeasuredAbscissa x; intensity = MeasuredData.MeasuredIntensity i }) }

    /// The series as the mapped points an X,Y built-in reads it to (spec 0050 S9.2).
    let private intensityPoints
        (result : Result<MeasuredData.IntensitySeries, MeasuredData.ExperimentDataError>)
        : Result<ImportProfiles.MappedPoints, MeasuredData.ExperimentDataError> =
        result |> Result.map (MeasuredData.IntensityData >> ImportProfiles.SeriesPoints)

    let private mockIntensity (result : Result<MeasuredData.IntensitySeries, MeasuredData.ExperimentDataError>) : ExperimentData.ExperimentDataProxy =
        {
            readPickedFile = fun file -> Error (KitIoErr $"the mock reads no picked file '{file.value}'")
            attachDataFile =
                fun file ->
                    Ok
                        {
                            path = KitRelativePath $"data/{System.IO.Path.GetFileName file.value}"
                            contentHash = ContentHash "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
                        }
            attachmentStatus = fun _ -> Ok AttachedDataUnchanged
            readMapped = fun _ -> intensityPoints result
        }

    let private mockEllipsometric (result : Result<MeasuredData.EllipsometricSeries, MeasuredData.ExperimentDataError>) : ExperimentData.ExperimentDataProxy =
        {
            mockIntensity (Error (MeasuredData.MalformedDataFileErr "the mock has no intensity data")) with
                readMapped = fun _ -> result |> Result.map (MeasuredData.EllipsometricData >> ImportProfiles.SeriesPoints)
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
        | Some (ValidatedDataBadge (MeasuredData.MeasuredPointCount 3) as badge) ->
            Assert.Equal("Loaded — 3 point(s), schema-valid", badge.caption.value)
        | other -> Assert.Fail($"expected ValidatedDataBadge (MeasuredData.MeasuredPointCount 3), got %A{other}")
        let exp = List.head m1.experimentCollection.experiments
        Assert.Equal<string option>(Some "data/measured.csv", exp.dataOpt |> Option.map (fun data -> data.attachment.path.value))
        // Spec 0050 S9.1: an attach reads the file through the intensity detector's built-in profile, every row the run's.
        Assert.Equal<ImportProfileName option>(Some (ImportProfileName "Intensity series"), exp.dataOpt |> Option.map (fun data -> data.profile))
        Assert.Equal<DataRowSelection option>(Some AllDataRows, exp.dataOpt |> Option.map (fun data -> data.rows))

    [<Fact>]
    let ``attaching a malformed intensity file records a typed parse-error status`` () =
        let proxy = mockIntensity (Error (MeasuredData.MalformedDataFileErr "row 3 is not a valid X,Y pair"))
        let m, expId = withIntensityExperiment { inverse () with experimentData = proxy }
        let m1 = m |> updateTableAndElementRotation (AttachDataFileTo (expId, FileName "bad.csv"))
        match m1.experimentDataStatus |> Map.tryFind expId with
        | Some (MappedDataBadge (Some problem) as badge) ->
            Assert.Contains("row 3", problem.caption.value)
            Assert.Equal($"Parse error — {problem.caption.value}", badge.caption.value)
        | other -> Assert.Fail($"expected MappedDataBadge (Some _), got %A{other}")

    [<Fact>]
    let ``attaching an out-of-range intensity file records a typed validation-error status`` () =
        // The R2 experiment's range is 0…89°; an abscissa of 200 is a LOUD range mismatch.
        let proxy = mockIntensity (Ok (intensitySeries [ 0.0, 1.0; 200.0, 0.5 ]))
        let m, expId = withIntensityExperiment { inverse () with experimentData = proxy }
        let m1 = m |> updateTableAndElementRotation (AttachDataFileTo (expId, FileName "oor.csv"))
        match m1.experimentDataStatus |> Map.tryFind expId with
        | Some (OutOfRangeBadge problem as badge) ->
            Assert.Contains("outside the experiment range", problem.caption.value)
            Assert.Equal($"Validation error — {problem.caption.value}", badge.caption.value)
        | other -> Assert.Fail($"expected OutOfRangeBadge, got %A{other}")

    [<Fact>]
    let ``the detector kind selects the ellipsometric load path`` () =
        // Bind the detector to the ellipsometer, commit an experiment, then attach an ellipsometric
        // file — the load must read it through the ellipsometric built-in profile and validate it as an ellipsometric shape.
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
        let committed = List.head m1.experimentCollection.experiments
        let expId = committed.id
        // The detector kind is read from the slots of the setup revision the experiment references (spec 0050 S8.2).
        match committed.slotsIn m1.kitSetups with
        | Ok slots -> Assert.Equal<Library.DetectorKind>(Library.Ellipsometer, ExperimentDataLoad.detectorKindOf m1.library slots)
        | Error error -> Assert.Fail($"the committed experiment's setup must be the window's: {error.describe}")
        let m2 = m1 |> updateTableAndElementRotation (AttachDataFileTo (expId, FileName "ellipso.csv"))
        match m2.experimentDataStatus |> Map.tryFind expId with
        | Some (ValidatedDataBadge (MeasuredData.MeasuredPointCount 2)) -> ()
        | other -> Assert.Fail($"expected ValidatedDataBadge (MeasuredData.MeasuredPointCount 2) from the ellipsometric path, got %A{other}")

    [<Fact>]
    let ``a picked file the kit cannot attach records no attachment and tells the kit status why`` () =
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
                    readMapped = fun _ -> failwith "an attachment failure must not read data"
            }
        let model, expId = withIntensityExperiment { inverse () with experimentData = refusingProxy }
        let failed = model |> updateTableAndElementRotation (AttachDataFileTo (expId, picked))
        let failedExperiment = List.head failed.experimentCollection.experiments
        Assert.Equal<MappedAttachment option>(None, failedExperiment.dataOpt)
        Assert.Equal(model.experimentCollection, failed.experimentCollection)
        // Spec 0050 S9.1: the refusal keeps the experiment's badge and tells the kit status why.
        Assert.Equal<ExperimentDataBadge>(NoDataBadge, failed.dataBadgeOf failedExperiment)
        Assert.Equal("No data", (failed.dataBadgeOf failedExperiment).caption.value)
        Assert.Equal<Map<Experiments.ExperimentId, ExperimentDataBadge>>(model.experimentDataStatus, failed.experimentDataStatus)
        let notAttached = ControlCaption $"Not attached — {attachError.describe}"
        Assert.Equal<ControlCaption option>(Some notAttached, failed.kitStatus)

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
        let attachedExperiment = List.head attached.experimentCollection.experiments
        Assert.Equal<ExperimentDataBadge>(ValidatedDataBadge (MeasuredData.MeasuredPointCount 2), attached.dataBadgeOf attachedExperiment)
        Assert.Equal<ExperimentDataBadge>(attached.dataBadgeOf attachedExperiment, refusedReplacement.dataBadgeOf attachedExperiment)
        Assert.Equal<ControlCaption option>(Some notAttached, refusedReplacement.kitStatus)

    [<Fact>]
    let ``committed experiments and their attachments are kept in the kit and reopen with it`` () =
        let folder = TestAppScope.freshKitFolder owner
        let proxy = mockIntensity (Ok (intensitySeries [ 0.0, 1.0; 45.0, 0.5 ]))
        let mBuilt, expId = withIntensityExperiment { inverseIn folder with experimentData = proxy }
        let mSaved = mBuilt |> updateTableAndElementRotation (AttachDataFileTo (expId, FileName "run.csv"))
        Assert.Equal<Experiments.Experiment list>(mSaved.experimentCollection.experiments, mSaved.kit.proxy.readExperiments () |> SceneTestFixture.kitSuccess)
        let setupFile = FileName "Setup 1.setup-json"
        Assert.True(File.Exists((setupFile.combine folder.value).value), "the commit must record the table as the kit's 'Setup 1'")
        mSaved.kit.close () |> SceneTestFixture.kitSuccess
        let mLoaded = inverseIn folder
        Assert.Equal(1, List.length mLoaded.experimentCollection.experiments)
        Assert.Equal<Experiments.Experiment list>(mSaved.experimentCollection.experiments, mLoaded.experimentCollection.experiments)
        let loaded = List.head mLoaded.experimentCollection.experiments
        Assert.Equal<string option>(Some "data/run.csv", loaded.dataOpt |> Option.map (fun data -> data.attachment.path.value))
        Assert.Equal<Setup list>(mSaved.kitSetups, mLoaded.kitSetups)
        Assert.True(Result.isOk (loaded.slotsIn mLoaded.kitSetups), "the reopened experiment's setup must reopen with it")
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
            match (List.head saved.experimentCollection.experiments).dataOpt with
            | Some attached -> attached.attachment
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
                        readMapped =
                            fun (data : MappedAttachment) ->
                                loadedAttachments.Add data.attachment
                                intensityPoints result
                }
            let opened = openFolder folder saved.catalogueImports
            let loaded = saved.loadKit { opened with experimentData = loadingProxy }
            loaded.kit.close () |> SceneTestFixture.kitSuccess
            Assert.Equal<DataAttachment list>([ attachment ], inspected |> Seq.toList)
            Assert.Equal<DataAttachment list>([ attachment ], loadedAttachments |> Seq.toList)
            Assert.Equal<Experiments.Experiment list>(saved.experimentCollection.experiments, loaded.experimentCollection.experiments)
            Assert.Equal(1, Map.count loaded.experimentDataStatus)
            Assert.Equal<ExperimentDataBadge option>(None, Map.tryFind unattachedId loaded.experimentDataStatus)
            loaded

        saved.kit.close () |> SceneTestFixture.kitSuccess
        let loaded = loadWith (Ok (intensitySeries [ 0.0, 1.0; 45.0, 0.6; 89.0, 0.2 ]))
        let expected = ValidatedDataBadge (MeasuredData.MeasuredPointCount 3)
        Assert.Equal<ExperimentDataBadge option>(Some expected, Map.tryFind expId loaded.experimentDataStatus)

        let parseError = MeasuredData.MalformedDataFileErr "row 3 is not a valid X,Y pair"
        let malformed = loadWith (Error parseError)
        Assert.Equal<ExperimentDataBadge option>(Some (MappedDataBadge (Some parseError)), Map.tryFind expId malformed.experimentDataStatus)

        let outOfRange = loadWith (Ok (intensitySeries [ 0.0, 1.0; 200.0, 0.5 ]))
        match Map.tryFind expId outOfRange.experimentDataStatus with
        | Some (OutOfRangeBadge problem) -> Assert.Contains("outside the experiment range", problem.caption.value)
        | other -> Assert.Fail($"expected OutOfRangeBadge on open, got %A{other}")

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
        let attachments = saved.experimentCollection.experiments |> List.choose (fun experiment -> experiment.dataOpt |> Option.map (fun data -> data.attachment))
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
                    readMapped = fun _ -> failwith "changed or missing data must not be validated"
            }
        saved.kit.close () |> SceneTestFixture.kitSuccess
        let opened = openFolder folder saved.catalogueImports
        let loaded = saved.loadKit { opened with experimentData = loadingProxy }
        let changed = DataChangedBadge (List.head attachments, Some currentHash)
        let missing = DataChangedBadge (List.last attachments, None)
        Assert.Equal<DataAttachment list>(attachments, inspected |> Seq.toList)
        Assert.Equal<Map<Experiments.ExperimentId, ExperimentDataBadge>>(
            Map.ofList [ firstId, changed; secondId, missing ], loaded.experimentDataStatus)
        Assert.Equal("Data changed since attach — attach it again to validate it", changed.caption.value)
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
        let readFailure = MappedDataBadge (Some (MeasuredData.MalformedDataFileErr readError.describe))
        Assert.Equal<Map<Experiments.ExperimentId, ExperimentDataBadge>>(
            Map.ofList [ firstId, readFailure; secondId, readFailure ], unreadable.experimentDataStatus)
        Assert.Equal($"Parse error — {readError.describe}", readFailure.caption.value)
        Assert.Equal<Experiments.Experiment list>(saved.experimentCollection.experiments, unreadable.experimentCollection.experiments)

    [<Fact>]
    let ``a commit, an attach and a removal write the kit's experiment files`` () =
        let folder = TestAppScope.freshKitFolder owner
        let model, expId = withIntensityExperiment (inverseIn folder)
        let file = FileName "E1.experiment-json"
        let filePath = file.combine folder.value
        Assert.True(File.Exists filePath.value)
        let setupFile = FileName "Setup 1.setup-json"
        Assert.True(File.Exists((setupFile.combine folder.value).value), "the commit must record the table as the kit's 'Setup 1'")
        Assert.Equal<Experiments.Experiment list>(model.experimentCollection.experiments, model.kit.proxy.readExperiments () |> SceneTestFixture.kitSuccess)
        let dataFile = FileName "data/measured.csv"
        let picked = dataFile.combine folder.value
        File.WriteAllText(picked.value, "x,intensity\n0,1\n45,0.5\n89,0.2\n")
        let before = File.ReadAllText filePath.value
        let attached = model |> updateTableAndElementRotation (AttachDataFileTo (expId, picked))
        Assert.NotEqual<string>(before, File.ReadAllText filePath.value)
        let experiment = List.head attached.experimentCollection.experiments
        match experiment.dataOpt with
        | Some data -> Assert.Equal("data/measured.csv", data.attachment.path.value)
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
        Assert.Equal<Map<Experiments.ExperimentId, ExperimentDataBadge>>(protectedModel.experimentDataStatus, refusedAttach.experimentDataStatus)
        let refusedRemove = protectedModel |> updateTableAndElementRotation (ExpRemove expId)
        Assert.Equal(protectedModel.experimentCollection, refusedRemove.experimentCollection)
        for refused in [ refusedEdit; refusedAttach; refusedRemove ] do
            match refused.kitStatus with
            | Some status -> Assert.Contains("read-only", status.value)
            | None -> Assert.Fail("A refused kit write must show its reason.")
        Assert.Equal<Experiments.Experiment list>(attached.experimentCollection.experiments, initial.kit.proxy.readExperiments () |> SceneTestFixture.kitSuccess)
        let removed = attached.experimentCollection.experiments |> List.find (fun experiment -> experiment.id = expId)
        initial.kit.proxy.deleteExperiment removed.name |> SceneTestFixture.kitSuccess
        let missing = attached |> updateTableAndElementRotation (ExpRemove expId)
        Assert.Empty(missing.experimentCollection.experiments)
        Assert.Equal<ControlCaption option>(None, missing.kitStatus)
        initial.kit.close () |> SceneTestFixture.kitSuccess

    // ================================ copied physical items (spec 0050 S7.2) ================================

    [<Fact>]
    let ``recommitting a copied experiment preserves its assigned physical ids`` () =
        let folder = TestAppScope.freshKitFolder owner
        let plate = BuiltinCatalogue.sample SampleIds.glassPlate1mmId
        let plateEntry = (Library.SampleItem plate).entryId
        let initial = inverseIn folder
        let initialSample = theSample initial
        // Two linear polarizers on the one preset: two physical pieces that only their ids tell apart.
        let built, expId =
            initial
            |> updateTableAndElementRotation (BindValueIdTo (initialSample.id, plateEntry))
            |> updateTableAndElementRotation (AddElement Placement.LinearPolarizer)
            |> updateTableAndElementRotation (AddElement Placement.LinearPolarizer)
            |> withIntensityExperiment
        let polarizers = built.elements |> List.filter (fun e -> e.placement.catalogueKind = Placement.LinearPolarizer)
        Assert.Equal<Placement.ElementValueId option list>(
            [ Some PresetIds.polarizerLp; Some PresetIds.polarizerLp ], polarizers |> List.map (fun e -> e.placement.valueId))
        let lpA = List.item 0 polarizers
        let lpB = List.item 1 polarizers
        let sample = theSample built
        let pieceA = ItemId (System.Guid.Parse "3c9e1f47-8a2d-4b06-9e53-1f7a2c8d4b60")
        let pieceB = ItemId (System.Guid.Parse "4daf2058-9b3e-4c17-af64-208b3d9e5c71")
        let samplePiece = ItemId (System.Guid.Parse "5eb03169-ac4f-4d28-b075-319c4eaf6d82")
        let assigned = Map.ofList [ lpA.id, pieceA; lpB.id, pieceB; sample.id, samplePiece ]
        let polarizer =
            match BuiltinCatalogue.preset PresetIds.polarizerLp with
            | PolarizerItem value -> value
            | _ -> failwith "The linear polarizer preset must hold a polarizer."
        let polarizerChoice =
            PolarizerPresetChoice (BuiltinCatalogue.itemModel ItemModels.ItemModelIds.idealPolarizerItemModelId, polarizer)
        let sampleChoice =
            StackSampleChoice (BuiltinCatalogue.itemModel ItemModels.ItemModelIds.stackSampleItemModelId, SampleVersionId.firstOf plate.id, plate)
        let drafts : ItemDraft list =
            [
                {
                    id = pieceA
                    label = ItemLabel "Copied polarizer A"
                    model = polarizerChoice
                    dial = FixedMounting
                }
                {
                    id = pieceB
                    label = ItemLabel "Copied polarizer B"
                    model = polarizerChoice
                    dial = FixedMounting
                }
                {
                    id = samplePiece
                    label = ItemLabel "Copied sample"
                    model = sampleChoice
                    dial = FixedMounting
                }
            ]
        let library : KitLibrary.KitLibraryContext =
            {
                kit = built.kit
                imports = built.catalogueImports
            }
        // A real copy stores its items before the experiment (spec 0050 .manual/016).
        for draft in drafts do
            library.recordItem draft.toItem |> Result.defaultWith (fun error -> failwith error.describe)

        // A copy writes the experiment's setup into the kit with the pieces its item mapping assigned (spec 0050 S6, S7.2,
        // S8.2).
        let copied = Assert.Single(built.experimentCollection.experiments)
        let captured = Assert.Single(built.kitSetups)
        Assert.Equal<SetupReference>(captured.reference, copied.setup)
        Assert.All(captured.slots, fun slot -> Assert.Equal<ItemId option>(None, slot.content.itemOpt))
        let copiedSetup =
            {
                captured with
                    slots =
                        captured.slots
                        |> List.map (fun slot ->
                            match Map.tryFind slot.id assigned with
                            | Some piece ->
                                {
                                    slot with content = ItemInSlot (piece, PlacementTurn)
                                }
                            | None -> slot)
            }
        built.kit.proxy.writeSetup copiedSetup |> SceneTestFixture.kitSuccess
        built.kit.proxy.writeExperiment copied |> SceneTestFixture.kitSuccess
        built.kit.close () |> SceneTestFixture.kitSuccess

        // The reopened bench is the copied setup drawn on the table.
        let loaded = inverseIn folder |> updateTableAndElementRotation (ActivateSetup captured.name)
        Assert.Equal<SetupSlot list>(copiedSetup.slots, loaded.elements)
        Assert.Equal<Experiments.Experiment list>([ copied ], loaded.experimentCollection.experiments)

        // Edit the range, rebind the sample to a hint and add a third piece of the same preset, then recommit.
        let recommitted =
            loaded
            |> updateTableAndElementRotation (ExpEdit expId)
            |> updateTableAndElementRotation (ExpSetRangePoints (ExperimentControls.ExperimentPointCount 7))
            |> updateTableAndElementRotation (BindValueIdTo (sample.id, Placement.ElementValueId glassFilm200Id))
            |> updateTableAndElementRotation (AddElement Placement.LinearPolarizer)
            |> updateTableAndElementRotation ExpCommit
        Assert.Equal<ControlCaption option>(None, recommitted.kitStatus)
        let added = List.last recommitted.elements
        Assert.Equal<Placement.ElementValueId option>(Some PresetIds.polarizerLp, added.placement.valueId)
        recommitted.kit.close () |> SceneTestFixture.kitSuccess

        let reopened = inverseIn folder
        let stored = Assert.Single(reopened.experimentCollection.experiments)
        reopened.kit.close () |> SceneTestFixture.kitSuccess
        Assert.Equal(expId, stored.id)
        Assert.Equal<int list>([ 7 ], stored.scan.axes |> List.map (fun axis -> axis.range.points.value))
        // The recommit over the referenced revision minted the next one; the stored experiment references it.
        Assert.Equal<SetupReference>({ captured.reference with revision = SetupRevision 2 }, stored.setup)
        let storedSlots =
            match stored.slotsIn reopened.kitSetups with
            | Ok slots -> slots
            | Error error -> failwith $"the recommitted experiment's setup must reopen with the kit: {error.describe}"
        let pieceOf (elementId : ElementId) : ItemId option =
            storedSlots |> List.tryFind (fun slot -> slot.id = elementId) |> Option.bind (fun slot -> slot.content.itemOpt)
        Assert.Equal<ItemId option>(Some pieceA, pieceOf lpA.id)
        Assert.Equal<ItemId option>(Some pieceB, pieceOf lpB.id)
        Assert.NotEqual<ItemId>(pieceA, pieceB)
        // The rebound sample and the new polarizer carry no piece: none is guessed from a binding or a preset.
        Assert.Equal<ItemId option>(None, pieceOf sample.id)
        Assert.Equal<ItemId option>(None, pieceOf added.id)
        Assert.Equal<ItemId list>([ pieceA; pieceB ], storedSlots |> List.choose (fun slot -> slot.content.itemOpt))

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
