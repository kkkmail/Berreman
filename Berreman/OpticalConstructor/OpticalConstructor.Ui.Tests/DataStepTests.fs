namespace OpticalConstructor.Ui.Tests

open System.IO
open System.Text
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
open Softellect.Sys.Primitives
open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.Media
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.DataSplits
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.NamingTemplates
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Tests
open OpticalConstructor.Ui
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Ui.TableAndElementRotationView
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open global.Elmish

/// The Inverse constructor's Data step over a folder kit holding `E1`: a held split names its key column and lists each
/// group matched, offered with its toggle or unmatched with its reason, above Apply split and Cancel in one row; a
/// reviewed experiment lists and plots its points, an excluded one marked (spec 0050 S9.3; 001 §K, §O). Its windows are
/// found and pressed through `SlotBarTests`' helpers.
module DataStepTests =

    let private owner : FolderName = FolderName "DataStepTests"

    /// The setup `E1` references, holding no slot, so the kit records `E1` holding no item (spec 0050 S8.2).
    let private darkSetup : Setup =
        {
            name = SetupName "Dark"
            revision = SetupRevision.first
            slots = []
            history = []
        }

    /// The kit's experiment `E1`, the dark line over `darkSetup`.
    let private e1 : Experiment =
        {
            id = ExperimentId 1
            name = (ExperimentId 1).defaultName
            setup = darkSetup.reference
            scan = ExperimentScan.darkLine CaptureTransmitted
            role = ScienceRun
            sessionOpt = None
            notes = ExperimentNotes.none
            dataOpt = None
            corrections = []
        }

    /// A Phone ROI file whose two rows both carry `E1`'s name.
    let private phoneRoiText : string =
        String.concat
            ""
            [
                "experiment,capture_index,captured_at,description,avg_total\n"
                "E1,0,2026-07-15T12:00:00+00:00,0,869.5\n"
                "E1,1,2026-07-15T12:01:00+00:00,45,912.25\n"
            ]

    let private k1 : DataRowKey = DataRowKey "K1"

    let private k2 : DataRowKey = DataRowKey "K2"

    let private k3 : DataRowKey = DataRowKey "K3"

    /// The setup `K2` is offered, which the kit lacks.
    let private benchB : Setup =
        {
            name = SetupName "Bench B"
            revision = SetupRevision.first
            slots = []
            history = []
        }

    /// Why `K3` matches nothing.
    let private k3Problem : DataSplitError = SplitKeyNameErr (NameMismatchErr "'K3' does not read as the template writes")

    /// The test-local split of `file` by `experiment` through Phone ROI CSV: `K1` (2 points) matched to `E1`, `K2` (1)
    /// offered `Bench B` and the experiment `K2`, and `K3` (1) unmatched; nothing ticked.
    let private phoneSplitOf (file : FileName) : DataSplitState =
        {
            file = file
            profile = ImportProfileName.phoneRoiCsv
            key = SplitKeyColumn "experiment"
            split =
                {
                    groups =
                        [
                            {
                                key = k1
                                points = MeasuredPointCount 2
                                matching = ExperimentForGroup e1.name
                            }
                            {
                                key = k2
                                points = MeasuredPointCount 1
                                matching =
                                    SetupOfferForGroup
                                        {
                                            setup = benchB
                                            experiment = ExperimentName "K2"
                                        }
                            }
                            {
                                key = k3
                                points = MeasuredPointCount 1
                                matching = GroupWithoutMatch k3Problem
                            }
                        ]
                }
            creating = []
        }

    /// The Data step's kit: the inverse model loaded from it and the Phone ROI file written beside it.
    type private DataStepKit =
        {
            loaded : TableAndElementRotationModel
            phoneFile : FileName
        }

    /// Runs over the inverse model, built as `KitWindowTests` builds it, loaded from a fresh folder kit under the class's
    /// test directory that holds `Dark` and `E1`, opened read-write; `phone.csv` is written in a GUID-named folder under
    /// the test output. A kit's experiments are read when it opens, so `E1` is written first and the kit reopened.
    let private withDataStepKit (run : DataStepKit -> unit) : unit =
        let ctx = TestAppScope.freshAppContext owner
        let created = TestAppScope.openedKitIn owner ctx.catalogueImports
        try
            created.library.kit.proxy.writeSetup darkSetup |> SceneTestFixture.kitSuccess
            created.library.kit.proxy.writeExperiment e1 |> SceneTestFixture.kitSuccess
        finally created.library.kit.close () |> SceneTestFixture.kitSuccess
        let root = Path.Combine(System.AppContext.BaseDirectory, "DataStepTests", System.Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory root |> ignore
        let phoneFile = FileName (Path.Combine(root, "phone.csv"))
        File.WriteAllBytes(phoneFile.value, Encoding.UTF8.GetBytes phoneRoiText)
        let opened =
            openKitAt created.folder ReadWriteKitAccess ClockProvider.system (KitAuthor "test") ctx.catalogueImports
            |> Result.defaultWith (fun error -> failwith error.describe)
        try
            let seed = initInverseWithKit ctx.library ctx.experiments ctx.materials ctx.samples ctx.categories opened.library.kit
            let seeded =
                {
                    seed with
                        catalogueImports = ctx.catalogueImports
                        sources = ctx.sources
                }
            let loaded = seeded.loadKit opened
            Assert.Equal<Experiment list>([ e1 ], loaded.experimentCollection.experiments)
            Assert.Equal<Setup list>([ darkSetup ], loaded.kitSetups)
            Assert.Equal<ControlAvailability>(ControlOffered, loaded.kitWrites)
            run
                {
                    loaded = loaded
                    phoneFile = phoneFile
                }
        finally
            opened.library.kit.close () |> SceneTestFixture.kitSuccess
            if Directory.Exists root then Directory.Delete(root, true)

    /// Shows `dataStepView` over `model` headless at 1000 × 700 in a `HostWindow` running `Program.mkSimple` with
    /// `updateTableAndElementRotation`, keeping every model it answers, as `SlotBarTests.withMainWindow` mounts `mainView`.
    let private withDataStepWindow
        (model : TableAndElementRotationModel)
        (check : Window -> ResizeArray<TableAndElementRotationModel> -> unit)
        : unit =
        let answered = ResizeArray<TableAndElementRotationModel> ()
        let recordingUpdate (msg : TableAndElementRotationMsg) (current : TableAndElementRotationModel) : TableAndElementRotationModel =
            let next = updateTableAndElementRotation msg current
            answered.Add next
            next
        let window = HostWindow()
        Program.mkSimple (fun () -> model) recordingUpdate DataStepView.dataStepView |> Program.withHost window |> Program.run
        window.Width <- 1000.0
        window.Height <- 700.0
        try
            window.Show()
            Dispatcher.UIThread.RunJobs()
            check window answered
        finally
            window.Close()
            Dispatcher.UIThread.RunJobs()

    /// Shows `dataStepView` over `model` headless at 1000 × 700 with a dispatch that records every message and changes
    /// nothing, as `SetupSaveLoadTests` mounts `mainView`.
    let private withRecordingWindow
        (model : TableAndElementRotationModel)
        (check : Window -> ResizeArray<TableAndElementRotationMsg> -> unit)
        : unit =
        let messages = ResizeArray<TableAndElementRotationMsg> ()
        let window = Window(Width = 1000.0, Height = 700.0)
        window.Content <- Avalonia.FuncUI.Component(fun _ -> DataStepView.dataStepView model messages.Add)
        try
            window.Show()
            Dispatcher.UIThread.RunJobs()
            check window messages
        finally
            window.Close()
            Dispatcher.UIThread.RunJobs()

    /// E1's attachment, which `reviewData` reads as `e1Series`.
    let private e1Attachment : DataAttachment =
        {
            path = KitRelativePath "data/e1.csv"
            contentHash = LibraryEntryFiles.ContentHash "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
        }

    /// E1's three points: 0 → 1.0, 45 → 0.5 and 90 → 0.0.
    let private e1Series : ImportProfiles.MappedPoints =
        ImportProfiles.SeriesPoints (
            IntensityData
                {
                    points =
                        [
                            { x = MeasuredAbscissa 0.0; intensity = MeasuredIntensity 1.0 }
                            { x = MeasuredAbscissa 45.0; intensity = MeasuredIntensity 0.5 }
                            { x = MeasuredAbscissa 90.0; intensity = MeasuredIntensity 0.0 }
                        ]
                }
        )

    /// `E1` mapped to its attachment through Intensity series, point 2 excluded for `beam blocked`.
    let private e1WithData : Experiment =
        {
            e1 with
                dataOpt =
                    Some
                        {
                            attachment = e1Attachment
                            profile = ImportProfileName.intensitySeries
                            rows = AllDataRows
                            exclusions =
                                [
                                    {
                                        point = MappedPointNumber 2
                                        reason = ExclusionReason "beam blocked"
                                    }
                                ]
                        }
        }

    /// The mock data proxy: E1's attachment reads as `e1Series`, and every other read is refused.
    let private reviewData : ExperimentData.ExperimentDataProxy =
        {
            readPickedFile = fun file -> Error (KitIoErr $"the mock reads no picked file '{file.value}'")
            attachDataFile = fun file -> Error (KitIoErr $"the mock attaches no file '{file.value}'")
            attachmentStatus = fun _ -> Ok AttachedDataUnchanged
            readMapped =
                fun data ->
                    if data.attachment = e1Attachment then Ok e1Series
                    else Error (MalformedDataFileErr $"the mock reads only E1's attachment, not {data.attachment.path.value}")
        }

    /// Runs over the class's model reading `e1WithData` through `reviewData`, its badge two points kept and one excluded.
    let private withReviewModel (run : TableAndElementRotationModel -> unit) : unit =
        withDataStepKit (fun kit ->
            let counts : ExperimentDataLoad.KeptPointsInfo = { kept = MeasuredPointCount 2; excluded = MeasuredPointCount 1 }
            let badge = ExperimentDataLoad.ExcludedPointsBadge counts
            run
                {
                    kit.loaded with
                        experimentData = reviewData
                        experimentCollection = { kit.loaded.experimentCollection with experiments = [ e1WithData ] }
                        experimentDataStatus = Map.ofList [ e1.id, badge ]
                })

    /// The AutomationIds under the control carrying `id` that start with `prefix`, in visual order.
    let private idsUnder (window : Window) (id : ConstructorUiId) (prefix : string) : string list =
        (SlotBarTests.controlById window id).GetVisualDescendants()
        |> Seq.choose (function :? Control as control -> Option.ofObj (AutomationProperties.GetAutomationId control) | _ -> None)
        |> Seq.filter (fun automationId -> automationId.StartsWith prefix)
        |> List.ofSeq

    /// The fill of the box carrying `id`.
    let private fillOf (window : Window) (id : ConstructorUiId) : Color =
        Assert.IsType<SolidColorBrush>(Assert.IsType<Border>(SlotBarTests.controlById window id).Background).Color

    /// The texts of the `TextBlock`s under the control carrying `id`, in visual order.
    let private textsUnder (window : Window) (id : ConstructorUiId) : string list =
        (SlotBarTests.controlById window id).GetVisualDescendants()
        |> Seq.choose (function :? TextBlock as text -> Some text.Text | _ -> None)
        |> List.ofSeq

    [<Fact>]
    let ``the Data step UiIds are the stable intent-named ids`` () : unit =
        Assert.Equal("FlowDataStep", ConstructorUiId.flowDataStep.value)
        Assert.Equal("DataStepPanel", ConstructorUiId.dataStepPanel.value)
        Assert.Equal("DataSplitFileButton", ConstructorUiId.dataSplitFileButton.value)
        Assert.Equal("DataSplitKeyColumn", ConstructorUiId.dataSplitKeyColumn.value)
        Assert.Equal("DataSplitApplyButton", ConstructorUiId.dataSplitApplyButton.value)
        Assert.Equal("DataSplitCancelButton", ConstructorUiId.dataSplitCancelButton.value)
        Assert.Equal<ConstructorUiKey>(ConstructorUiKey "K1", k1.uiKey)
        Assert.Equal("DataSplitGroup_K1", (ConstructorUiId.dataSplitGroupRow k1).value)
        Assert.Equal("DataSplitOffer_K2", (ConstructorUiId.dataSplitOfferToggle k2).value)
        Assert.Equal("DataReviewPanel", ConstructorUiId.dataReviewPanel.value)
        Assert.Equal("DataExclusionReasonField", ConstructorUiId.dataExclusionReasonField.value)
        Assert.Equal("DataPointsChart", ConstructorUiId.dataPointsChart.value)
        Assert.Equal("DataPointDot_2", (ConstructorUiId.dataPointDot (ConstructorUiPosition 2)).value)
        Assert.Equal("DataPointExcluded_2", (ConstructorUiId.dataExcludedPointMark (ConstructorUiPosition 2)).value)
        Assert.Equal("DataReview_1", (ConstructorUiId.dataReviewButton (ExperimentId 1)).value)
        Assert.Equal<ConstructorUiPosition>(ConstructorUiPosition 2, (MappedPointNumber 2).uiPosition)
        Assert.Equal("DataPoint_2", (ConstructorUiId.dataPointRow (MappedPointNumber 2)).value)
        Assert.Equal("DataExcludePoint_2", (ConstructorUiId.dataExcludePointButton (MappedPointNumber 2)).value)
        Assert.Equal("DataIncludePoint_2", (ConstructorUiId.dataIncludePointButton (MappedPointNumber 2)).value)

    [<Fact>]
    let ``the split panel names its key column and lists each group matched, offered or unmatched, with Apply and Cancel in one row`` () : unit =
        HeadlessSession.run (fun () ->
            withDataStepKit (fun kit ->
                let model =
                    {
                        kit.loaded with dataSplitOpt = Some (phoneSplitOf kit.phoneFile)
                    }
                withDataStepWindow model (fun window _ ->
                    Assert.Equal("Split phone.csv by 'experiment' through 'Phone ROI CSV'", Assert.IsType<TextBlock>(SlotBarTests.controlById window ConstructorUiId.dataSplitKeyColumn).Text)
                    // Each group's row, in file order: its key and points, then its match.
                    Assert.Equal<string list>([ "K1 — 2 point(s) — "; "→ E1" ], textsUnder window (ConstructorUiId.dataSplitGroupRow k1))
                    Assert.Equal<string list>(
                        [ "K2 — 1 point(s) — "; "Create K2 over 'Bench B' (new setup)" ],
                        textsUnder window (ConstructorUiId.dataSplitGroupRow k2))
                    match textsUnder window (ConstructorUiId.dataSplitGroupRow k3) with
                    | [ label; reason ] ->
                        Assert.Equal("K3 — 1 point(s) — ", label)
                        Assert.StartsWith("unmatched —", reason)
                        Assert.Equal($"unmatched — {k3Problem.describe}", reason)
                    | texts -> Assert.Fail $"K3's row must read its label and its reason, not {List.length texts} text(s)"
                    let reasonText =
                        (SlotBarTests.controlById window (ConstructorUiId.dataSplitGroupRow k3)).GetVisualDescendants()
                        |> Seq.choose (function :? TextBlock as text when text.Text.StartsWith "unmatched" -> Some text | _ -> None)
                        |> Seq.exactlyOne
                    Assert.Equal(MaterialsWindowView.messageColor, Assert.IsAssignableFrom<ISolidColorBrush>(reasonText.Foreground).Color)
                    let rowTops =
                        [ k1; k2; k3 ]
                        |> List.map (fun key -> (SlotBarTests.topLeftOf window (SlotBarTests.controlById window (ConstructorUiId.dataSplitGroupRow key))).Y)
                    Assert.Equal<float list>(List.sort rowTops, rowTops)
                    Assert.Equal(3, rowTops |> List.distinct |> List.length)
                    // Only the offered group has a toggle, unticked in the verb look.
                    SlotBarTests.assertAbsent window (ConstructorUiId.dataSplitOfferToggle k1)
                    SlotBarTests.assertAbsent window (ConstructorUiId.dataSplitOfferToggle k3)
                    Assert.Equal("Create K2 over 'Bench B' (new setup)", SlotBarTests.boxLabelOf window (ConstructorUiId.dataSplitOfferToggle k2))
                    Assert.NotEqual(MaterialsWindowView.positiveBackground, fillOf window (ConstructorUiId.dataSplitOfferToggle k2))
                    // Apply split and Cancel share a row, Cancel to the right after a gap, positive and negative.
                    let apply = SlotBarTests.controlById window ConstructorUiId.dataSplitApplyButton
                    let cancel = SlotBarTests.controlById window ConstructorUiId.dataSplitCancelButton
                    Assert.Equal("Apply split", SlotBarTests.boxLabelOf window ConstructorUiId.dataSplitApplyButton)
                    Assert.Equal("Cancel", SlotBarTests.boxLabelOf window ConstructorUiId.dataSplitCancelButton)
                    let applyCorner = SlotBarTests.topLeftOf window apply
                    let cancelCorner = SlotBarTests.topLeftOf window cancel
                    Assert.Equal(applyCorner.Y, cancelCorner.Y)
                    let applyRight = applyCorner.X + apply.Bounds.Width
                    Assert.True(cancelCorner.X > applyRight, $"Cancel's left {cancelCorner.X} must be beyond Apply split's right {applyRight}")
                    Assert.True(rowTops |> List.forall (fun top -> top < applyCorner.Y), "Apply split must sit below every group's row")
                    Assert.Equal(MaterialsWindowView.positiveBackground, fillOf window ConstructorUiId.dataSplitApplyButton)
                    Assert.Equal(MaterialsWindowView.negativeBackground, fillOf window ConstructorUiId.dataSplitCancelButton)
                    Assert.True(apply.IsEffectivelyEnabled, "Apply split must be enabled over a read-write kit"))
                // Spec 0050 S9.3: while the kit takes no writes, Apply split is disabled and Cancel still forgets the split.
                let readOnly =
                    {
                        model with kit = { model.kit with access = ReadOnlyKitAccess }
                    }
                Assert.Equal<ControlAvailability>(ControlWithheld, readOnly.kitWrites)
                withDataStepWindow readOnly (fun window _ ->
                    Assert.False((SlotBarTests.controlById window ConstructorUiId.dataSplitApplyButton).IsEffectivelyEnabled, "Apply split must be disabled while the kit takes no writes")
                    Assert.True((SlotBarTests.controlById window ConstructorUiId.dataSplitCancelButton).IsEffectivelyEnabled, "Cancel must stay enabled"))))

    [<Fact>]
    let ``pressing an offer ticks it, and Cancel forgets the split`` () : unit =
        HeadlessSession.run (fun () ->
            withDataStepKit (fun kit ->
                let model =
                    {
                        kit.loaded with dataSplitOpt = Some (phoneSplitOf kit.phoneFile)
                    }
                withDataStepWindow model (fun window answered ->
                    let toggle = ConstructorUiId.dataSplitOfferToggle k2
                    SlotBarTests.pressBox window toggle
                    match (SlotBarTests.latestOf model answered).dataSplitOpt with
                    | Some held -> Assert.Equal<DataRowKey list>([ k2 ], held.creating)
                    | None -> Assert.Fail "Pressing an offer must keep the split"
                    Assert.Equal(MaterialsWindowView.positiveBackground, fillOf window toggle)
                    Assert.Equal("Create K2 over 'Bench B' (new setup)", SlotBarTests.boxLabelOf window toggle)

                    SlotBarTests.pressBox window ConstructorUiId.dataSplitCancelButton
                    Assert.Equal<DataSplitState option>(None, (SlotBarTests.latestOf model answered).dataSplitOpt)
                    SlotBarTests.assertAbsent window ConstructorUiId.dataSplitKeyColumn
                    for key in [ k1; k2; k3 ] do
                        SlotBarTests.assertAbsent window (ConstructorUiId.dataSplitGroupRow key)
                    SlotBarTests.assertAbsent window ConstructorUiId.dataSplitApplyButton
                    SlotBarTests.assertAbsent window ConstructorUiId.dataSplitCancelButton
                    Assert.True((SlotBarTests.controlById window ConstructorUiId.dataSplitFileButton).IsEffectivelyVisible, "the Split verb must stay"))))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Data step renders a split and Apply split attaches its matched group`` () : unit =
        HeadlessSession.run (fun () ->
            withDataStepKit (fun kit ->
                let split = kit.loaded |> updateTableAndElementRotation (SplitByColumn (kit.phoneFile, ImportProfileName.phoneRoiCsv))
                let e1Key = DataRowKey "E1"
                match split.dataSplitOpt with
                | Some held ->
                    let expected : SplitGroup list =
                        [
                            {
                                key = e1Key
                                points = MeasuredPointCount 2
                                matching = ExperimentForGroup e1.name
                            }
                        ]
                    Assert.Equal<SplitGroup list>(expected, held.split.groups)
                | None ->
                    let told = split.kitStatus |> Option.map (fun status -> status.value) |> Option.defaultValue "nothing told"
                    Assert.Fail $"The file must be split, not refused: {told}"
                withDataStepWindow split (fun window answered ->
                    Assert.True((SlotBarTests.controlById window ConstructorUiId.dataSplitKeyColumn).IsEffectivelyVisible, "the split panel must show")
                    Assert.Equal<string list>([ "E1 — 2 point(s) — "; "→ E1" ], textsUnder window (ConstructorUiId.dataSplitGroupRow e1Key))

                    SlotBarTests.pressBox window ConstructorUiId.dataSplitApplyButton

                    let applied = SlotBarTests.latestOf split answered
                    let attachment =
                        match applied.kit.proxy.readDataFile (KitRelativePath $"data/{kit.phoneFile.leafName.value}") with
                        | Ok stored -> stored.attachment
                        | Error error -> failwith error.describe
                    let mapped =
                        {
                            e1 with
                                dataOpt =
                                    Some
                                        {
                                            attachment = attachment
                                            profile = ImportProfileName.phoneRoiCsv
                                            rows = KeyedDataRows e1Key
                                            exclusions = []
                                        }
                        }
                    Assert.Equal<Experiment list>([ mapped ], applied.experimentCollection.experiments)
                    Assert.Equal<Experiment list>([ mapped ], applied.kit.proxy.readExperiments () |> SceneTestFixture.kitSuccess)
                    Assert.Equal<DataSplitState option>(None, applied.dataSplitOpt)
                    SlotBarTests.assertAbsent window ConstructorUiId.dataSplitKeyColumn
                    SlotBarTests.assertAbsent window (ConstructorUiId.dataSplitGroupRow e1Key)
                    SlotBarTests.assertAbsent window ConstructorUiId.dataSplitApplyButton)))

    [<Fact>]
    let ``each mapped point reads as its caption and plots at its number`` () : unit =
        let at (number : float) (reading : float) : ExperimentControls.ChartPoint = { x = number; y = reading }
        let captionsOf (texts : string list) : ControlCaption list = texts |> List.map ControlCaption

        let intensity =
            ImportProfiles.SeriesPoints (
                IntensityData
                    {
                        points =
                            [
                                { x = MeasuredAbscissa 0.0; intensity = MeasuredIntensity 1.0 }
                                { x = MeasuredAbscissa 45.0; intensity = MeasuredIntensity 0.5 }
                            ]
                    }
            )
        Assert.Equal<ControlCaption list>(captionsOf [ "x = 0, I = 1"; "x = 45, I = 0.5" ], intensity.pointCaptions)
        Assert.Equal<ExperimentControls.ChartPoint list>([ at 1.0 1.0; at 2.0 0.5 ], intensity.chartPoints)

        // The two rows of `phoneRoiText`.
        let capture (index : int) (description : float<deg>) (avgTotal : float) : MuellerReconstruction.MuellerRawRow =
            {
                experiment = MuellerReconstruction.MuellerExperimentLabel "E1"
                captureIndex = MuellerReconstruction.CaptureIndex index
                capturedAt = System.DateTimeOffset(2026, 7, 15, 12, index, 0, System.TimeSpan.Zero)
                description = Angle.degree description
                avgTotal = MeasuredIntensity avgTotal
            }
        let captures = ImportProfiles.CapturePoints [ capture 0 0.0<deg> 869.5; capture 1 45.0<deg> 912.25 ]
        Assert.Equal<ControlCaption list>(captionsOf [ "E1 #0: 0° → 869.5"; "E1 #1: 45° → 912.25" ], captures.pointCaptions)
        Assert.Equal<ExperimentControls.ChartPoint list>([ at 1.0 869.5; at 2.0 912.25 ], captures.chartPoints)

        // m00 is 0.8125 and every other element differs from it.
        let measured =
            RealMatrix4x4.create [ for i in 0 .. 3 -> [ for j in 0 .. 3 -> 0.8125 - 0.0625 * float (4 * i + j) ] ]
            |> MuellerMatrix
        let configuration : MuellerInverse.MeasurementConfiguration =
            {
                cut = MuellerInverse.ZCut
                thickness = Thickness.mm 1.0<mm>
                incidenceAngle = IncidenceAngle.create (Angle.degree 45.0<deg>)
                azimuth = MuellerInverse.SampleAzimuth (Angle.degree 22.5<deg>)
                observable = MuellerInverse.ReflectedMueller
                waveLength = WaveLength.nm 632.8<nm>
            }
        let zCut : MuellerInverse.MuellerObservation = { configuration = configuration; measured = measured }
        let observation : ImportProfiles.KeyedObservation = { key = DataRowKey "Z-cut"; observation = zCut }
        let observations = ImportProfiles.ObservationPoints [ observation ]
        Assert.Equal<ControlCaption list>(captionsOf [ "Z-cut R: θ = 45°, φ = 22.5°, m00 = 0.8125" ], observations.pointCaptions)
        Assert.Equal<ExperimentControls.ChartPoint list>([ at 1.0 0.8125 ], observations.chartPoints)

        // The other two shapes: an ellipsometric point plots its Ψ in degrees, a mapped row its signal or m00.
        let psi = Angle.degree 30.0<deg>
        let ellipsometric =
            ImportProfiles.SeriesPoints (
                EllipsometricData
                    {
                        points =
                            [
                                {
                                    waveLength = WaveLength.nm 632.8<nm>
                                    psi = psi
                                    delta = Angle.degree 90.0<deg>
                                    aoiOpt = None
                                }
                            ]
                    }
            )
        Assert.Equal<ControlCaption list>(captionsOf [ "λ = 632.8 nm, Ψ = 30°, Δ = 90°" ], ellipsometric.pointCaptions)
        Assert.Equal<ExperimentControls.ChartPoint list>([ at 1.0 psi.degrees ], ellipsometric.chartPoints)
        let column : ScanColumn =
            {
                column = DataColumnNumber 1
                valueUnit = DegreeColumnUnit
                reading = PhysicalScanValue
            }
        let mappedRow (degrees : float) (reading : ImportProfiles.MappedReading) : ImportProfiles.MappedRow =
            let cell : ImportProfiles.ScanCell = { column = column; magnitude = Units.UnitMagnitude degrees }
            {
                keyOpt = None
                scan = [ cell ]
                reading = reading
                timestampOpt = None
                captureIndexOpt = None
                observableOpt = None
            }
        let rows =
            ImportProfiles.ColumnPoints
                [
                    mappedRow 22.5 (ImportProfiles.SignalReading (MeasuredIntensity 0.25, None))
                    mappedRow 45.0 (ImportProfiles.MuellerReading measured)
                ]
        Assert.Equal<ControlCaption list>(captionsOf [ "22.5: 0.25"; "45: 0.8125" ], rows.pointCaptions)
        Assert.Equal<ExperimentControls.ChartPoint list>([ at 1.0 0.25; at 2.0 0.8125 ], rows.chartPoints)

    [<Fact>]
    let ``the review panel lists every point, an excluded one marked with its reason, and plots the excluded points as rings`` () : unit =
        HeadlessSession.run (fun () ->
            withReviewModel (fun model ->
                let reviewed = model |> updateTableAndElementRotation (ReviewExperimentData (ExperimentId 1))
                withDataStepWindow reviewed (fun window _ ->
                    Assert.Equal(
                        "Points of E1 — excluded points (1 excluded, 2 validated)",
                        List.head (textsUnder window ConstructorUiId.dataReviewPanel))
                    Assert.IsType<TextBox>(SlotBarTests.controlById window ConstructorUiId.dataExclusionReasonField) |> ignore

                    // Every point has its row, in order; the excluded one reads its reason, in the message colour, and Include.
                    Assert.Equal<string list>([ "DataPoint_1"; "DataPoint_2"; "DataPoint_3" ], idsUnder window ConstructorUiId.dataReviewPanel "DataPoint_")
                    let rowOf (number : int) : ConstructorUiId = ConstructorUiId.dataPointRow (MappedPointNumber number)
                    Assert.Equal<string list>([ "#1  x = 0, I = 1"; "Exclude" ], textsUnder window (rowOf 1))
                    Assert.Equal<string list>([ "#2  x = 45, I = 0.5"; "excluded — beam blocked"; "Include" ], textsUnder window (rowOf 2))
                    Assert.Equal<string list>([ "#3  x = 90, I = 0"; "Exclude" ], textsUnder window (rowOf 3))
                    let reasonText =
                        (SlotBarTests.controlById window (rowOf 2)).GetVisualDescendants()
                        |> Seq.choose (function :? TextBlock as text when text.Text.StartsWith "excluded" -> Some text | _ -> None)
                        |> Seq.exactlyOne
                    Assert.Equal(MaterialsWindowView.messageColor, Assert.IsAssignableFrom<ISolidColorBrush>(reasonText.Foreground).Color)
                    for number in [ 1; 3 ] do
                        Assert.Equal("Exclude", SlotBarTests.boxLabelOf window (ConstructorUiId.dataExcludePointButton (MappedPointNumber number)))
                        SlotBarTests.assertAbsent window (ConstructorUiId.dataIncludePointButton (MappedPointNumber number))
                    Assert.Equal("Include", SlotBarTests.boxLabelOf window (ConstructorUiId.dataIncludePointButton (MappedPointNumber 2)))
                    SlotBarTests.assertAbsent window (ConstructorUiId.dataExcludePointButton (MappedPointNumber 2))

                    // The chart keeps every point unjoined: dots for 1 and 3, a ring for 2.
                    let chart = Assert.IsType<Canvas>(SlotBarTests.controlById window ConstructorUiId.dataPointsChart)
                    let marks =
                        chart.GetVisualDescendants()
                        |> Seq.choose (function :? Ellipse as mark -> Some (AutomationProperties.GetAutomationId mark) | _ -> None)
                        |> List.ofSeq
                    Assert.Equal<string list>([ "DataPointDot_1"; "DataPointExcluded_2"; "DataPointDot_3" ], marks)
                    Assert.Empty(chart.GetVisualDescendants() |> Seq.filter (fun visual -> visual :? Polyline))
                    SlotBarTests.assertAbsent window (ConstructorUiId.dataPointDot (ConstructorUiPosition 2))
                    let markOf (id : ConstructorUiId) : Ellipse = Assert.IsType<Ellipse>(SlotBarTests.controlById window id)
                    let first = markOf (ConstructorUiId.dataPointDot (ConstructorUiPosition 1))
                    let third = markOf (ConstructorUiId.dataPointDot (ConstructorUiPosition 3))
                    let ring = markOf (ConstructorUiId.dataExcludedPointMark (ConstructorUiPosition 2))
                    for dot in [ first; third ] do
                        Assert.Equal(5.0, dot.Width)
                        Assert.Equal(5.0, dot.Height)
                        Assert.IsAssignableFrom<ISolidColorBrush>(dot.Fill) |> ignore
                    Assert.Equal(9.0, ring.Width)
                    Assert.Equal(9.0, ring.Height)
                    Assert.Null(ring.Fill)
                    Assert.Equal(1.5, ring.StrokeThickness)
                    let stroke = Assert.IsAssignableFrom<ISolidColorBrush>(ring.Stroke).Color
                    Assert.True(stroke.R > 150uy && stroke.G < 80uy && stroke.B < 80uy, $"the ring must be red, not {stroke}")
                    // The three points lie on one line, so the ring's centre is midway between the dots' centres.
                    let centreOf (mark : Ellipse) : Point = Point(Canvas.GetLeft mark + mark.Width / 2.0, Canvas.GetTop mark + mark.Height / 2.0)
                    let midway = (centreOf first + centreOf third) / 2.0
                    Assert.Equal(midway, centreOf ring))))

    [<Fact>]
    let ``pressing Exclude sends the point with the typed reason, and Include takes it back`` () : unit =
        HeadlessSession.run (fun () ->
            withReviewModel (fun model ->
                let reviewed = model |> updateTableAndElementRotation (ReviewExperimentData (ExperimentId 1))
                let reasonsOf (messages : ResizeArray<TableAndElementRotationMsg>) : ControlCaption list =
                    messages |> Seq.choose (function SetExclusionReason reason -> Some reason | _ -> None) |> List.ofSeq
                let exclusionsOf (messages : ResizeArray<TableAndElementRotationMsg>) : (ExperimentId * MappedPointNumber * ControlCaption) list =
                    messages |> Seq.choose (function ExcludePoint (id, point, reason) -> Some (id, point, reason) | _ -> None) |> List.ofSeq
                let inclusionsOf (messages : ResizeArray<TableAndElementRotationMsg>) : (ExperimentId * MappedPointNumber) list =
                    messages |> Seq.choose (function IncludePoint (id, point) -> Some (id, point) | _ -> None) |> List.ofSeq
                let strayLight = ControlCaption "stray light"
                withRecordingWindow reviewed (fun window messages ->
                    // The field commits its text on Enter and on losing focus; a commit is idempotent, so what it sends is
                    // held to, not how often the key's routed event delivers it.
                    let field = Assert.IsType<TextBox>(SlotBarTests.controlById window ConstructorUiId.dataExclusionReasonField)
                    field.Text <- strayLight.value
                    field.RaiseEvent(KeyEventArgs(RoutedEvent = InputElement.KeyDownEvent, Key = Key.Enter))
                    Assert.Equal<ControlCaption list>([ strayLight ], reasonsOf messages |> List.distinct)
                    messages.Clear()
                    field.RaiseEvent(FocusChangedEventArgs(InputElement.LostFocusEvent))
                    Assert.Equal<ControlCaption list>([ strayLight ], reasonsOf messages |> List.distinct)
                    messages.Clear()

                    // Spec 0050 S4.3, S9.3: a pressed verb does not blur the field, so Exclude commits the reason it holds.
                    SlotBarTests.pressBox window (ConstructorUiId.dataExcludePointButton (MappedPointNumber 3))
                    Assert.Equal<(ExperimentId * MappedPointNumber * ControlCaption) list>(
                        [ ExperimentId 1, MappedPointNumber 3, strayLight ],
                        exclusionsOf messages)
                    Assert.Equal<ControlCaption list>([ strayLight ], reasonsOf messages)
                    Assert.Empty(inclusionsOf messages)
                    messages.Clear()

                    SlotBarTests.pressBox window (ConstructorUiId.dataIncludePointButton (MappedPointNumber 2))
                    Assert.Equal<(ExperimentId * MappedPointNumber) list>([ ExperimentId 1, MappedPointNumber 2 ], inclusionsOf messages)
                    Assert.Empty(exclusionsOf messages))

                // Spec 0050 S9.3: while the kit takes no writes, Exclude and Include are disabled and Review stays offered.
                let readOnly =
                    {
                        reviewed with kit = { reviewed.kit with access = ReadOnlyKitAccess }
                    }
                Assert.Equal<ControlAvailability>(ControlWithheld, readOnly.kitWrites)
                withRecordingWindow readOnly (fun window messages ->
                    let exclude = ConstructorUiId.dataExcludePointButton (MappedPointNumber 3)
                    let includeBack = ConstructorUiId.dataIncludePointButton (MappedPointNumber 2)
                    Assert.False((SlotBarTests.controlById window exclude).IsEffectivelyEnabled, "Exclude must be disabled while the kit takes no writes")
                    Assert.False((SlotBarTests.controlById window includeBack).IsEffectivelyEnabled, "Include must be disabled while the kit takes no writes")
                    Assert.True(
                        (SlotBarTests.controlById window (ConstructorUiId.dataReviewButton (ExperimentId 1))).IsEffectivelyEnabled,
                        "Review must stay offered over a read-only kit")
                    SlotBarTests.pressBox window exclude
                    SlotBarTests.pressBox window includeBack
                    Assert.Empty(exclusionsOf messages)
                    Assert.Empty(inclusionsOf messages))))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``pressing Review shows the experiment's points and their chart`` () : unit =
        HeadlessSession.run (fun () ->
            withReviewModel (fun model ->
                withDataStepWindow model (fun window answered ->
                    SlotBarTests.assertAbsent window ConstructorUiId.dataReviewPanel
                    SlotBarTests.assertAbsent window ConstructorUiId.dataPointsChart

                    SlotBarTests.pressBox window (ConstructorUiId.dataReviewButton (ExperimentId 1))

                    let expected : ReviewedData =
                        {
                            experimentId = ExperimentId 1
                            points = Ok e1Series
                        }
                    Assert.Equal<ReviewedData option>(Some expected, (SlotBarTests.latestOf model answered).reviewedDataOpt)
                    Assert.True((SlotBarTests.controlById window ConstructorUiId.dataReviewPanel).IsEffectivelyVisible, "the review panel must show")
                    Assert.True((SlotBarTests.controlById window ConstructorUiId.dataPointsChart).IsEffectivelyVisible, "the points chart must show")
                    Assert.Equal<string list>([ "DataPoint_1"; "DataPoint_2"; "DataPoint_3" ], idsUnder window ConstructorUiId.dataReviewPanel "DataPoint_"))))
