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
open OpticalConstructor.Domain.ExperimentMatrices
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

/// The Inverse constructor's Data step over a folder kit holding `E1`: the matrix places each experiment by its setup
/// with its data badge and reviews it when pressed; a held split names its key column and lists each group matched,
/// offered with its toggle or unmatched with its reason, above Apply split and Cancel in one row; a reviewed experiment
/// lists and plots its points, an excluded one marked, and re-points its setup with an audit note (spec 0050 S9.3; 001
/// §K, §O). Its windows are found and pressed through `SlotBarTests`' helpers.
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

    let private circularPolarizerChoice : ItemModelChoice =
        LibraryModelChoice (BuiltinCatalogue.itemModel ItemModels.ItemModelIds.circularPolarizerModelId)

    let private blackBoxChoice : ItemModelChoice = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModels.ItemModelIds.blackBoxMuellerModelId)

    /// The bench's polarizer before its object.
    let private cpl1 : Item =
        let draft : ItemDraft =
            {
                id = ItemId (System.Guid.Parse "32700000-0000-4000-8000-000000000001")
                label = ItemLabel "CPL1"
                model = circularPolarizerChoice
                dial = FixedMounting
            }
        draft.toItem

    /// The polarizer under study, a Mueller black box in the bench's object slot, so its turn enters the matrix row.
    let private lp1 : Item =
        let draft : ItemDraft =
            {
                id = ItemId (System.Guid.Parse "32700000-0000-4000-8000-000000000002")
                label = ItemLabel "LP1"
                model = blackBoxChoice
                dial = FixedMounting
            }
        draft.toItem

    /// The bench's analyzer, behind its object.
    let private cpl2 : Item =
        let draft : ItemDraft =
            {
                id = ItemId (System.Guid.Parse "32700000-0000-4000-8000-000000000003")
                label = ItemLabel "CPL2"
                model = circularPolarizerChoice
                dial = FixedMounting
            }
        draft.toItem

    let private benchItems : Item list = [ cpl1; lp1; cpl2 ]

    /// The bench's slots in table order, each fixed-mounted item held as `withItem` holds an unbound one: an empty
    /// source, CPL1, LP1 turned to `lp1Turn`, CPL2 and an empty detector. Its matrix row is `CPL1 · LP1@<lp1Turn>` and
    /// its column `CPL2` (spec 0050 S9.3).
    let private benchSlots (lp1Turn : Angle) : SetupSlot list =
        let slotOf (id : Library.ElementId) (kind : Placement.CatalogueKind) (point : Placement.TablePoint) (content : SlotContent) : SetupSlot =
            {
                id = id
                placement = Placement.ElementPlacement.create kind point
                zoom = BeamLayout.ElementZoom 5.0
                beam = BeamLayout.OnMainBeam
                content = content
            }
        let held (item : Item) : SlotContent = ItemInSlot (item.id, PlacementTurn)
        let onBeamAt (x : float<meter>) : Placement.TablePoint = { x = x; y = 0.0<meter> }
        let studied = slotOf (Library.ElementId "object") Placement.Sample (onBeamAt -0.1<meter>) (held lp1)
        [
            slotOf (Library.ElementId "src") Placement.LightSource RayModel.defaultSourcePoint EmptySlotContent
            slotOf (Library.ElementId "prep") Placement.LinearPolarizer (onBeamAt -0.3<meter>) (held cpl1)
            { studied with placement = Placement.withR1 lp1Turn studied.placement }
            slotOf (Library.ElementId "analyzer") Placement.LinearPolarizer (onBeamAt 0.3<meter>) (held cpl2)
            slotOf (Library.ElementId "det") Placement.Detector RayModel.defaultDetectorPoint EmptySlotContent
        ]

    /// The setup `K2` is offered, which the kit lacks; in the matrix facts, E2's setup: the bench with its polarizer
    /// under study turned to 30°.
    let private benchB : Setup =
        {
            name = SetupName "Bench B"
            revision = SetupRevision.first
            slots = benchSlots (Angle.degree 30.0<deg>)
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

    /// The mock data proxy: every file attaches as E1's attachment, which reads as `e1Series`, and every other read is
    /// refused.
    let private reviewData : ExperimentData.ExperimentDataProxy =
        {
            readPickedFile = fun file -> Error (KitIoErr $"the mock reads no picked file '{file.value}'")
            attachDataFile = fun _ -> Ok e1Attachment
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

    /// E1's setup in the correction facts, the one its correction re-pointed it to, and in the matrix facts: the bench with
    /// its polarizer under study at 0°.
    let private setup1 : Setup =
        {
            name = SetupName "Setup 1"
            revision = SetupRevision.first
            slots = benchSlots (Angle.degree 0.0<deg>)
            history = []
        }

    /// E1's one correction: from `'Setup 0' revision 1` to `setup1`, because `LP1 was at 30`.
    let private lp1Correction : SetupCorrection =
        let setup0 : SetupReference =
            {
                name = SetupName "Setup 0"
                revision = SetupRevision.first
            }
        {
            previous = setup0
            corrected = setup1.reference
            note = CorrectionNote "LP1 was at 30"
        }

    /// Runs over the class's review model whose kit holds `setup1` and `benchB`, with `e1WithData` over `setup1` holding
    /// `lp1Correction`, reviewed.
    let private withCorrectionModel (run : TableAndElementRotationModel -> unit) : unit =
        withReviewModel (fun model ->
            let corrected =
                {
                    e1WithData with
                        setup = setup1.reference
                        corrections = [ lp1Correction ]
                }
            let held =
                {
                    model with
                        kitSetups = [ setup1; benchB ]
                        experimentCollection = { model.experimentCollection with experiments = [ corrected ] }
                }
            run (held |> updateTableAndElementRotation (ReviewExperimentData (ExperimentId 1))))

    /// Runs over the class's kit, which now also holds `setup1`, `benchB` and the bench's items, its model reading through
    /// `reviewData`: E1 over `setup1`, E2 over `benchB` and E3 over `Gone` revision 1, which the kit lacks, none with data.
    let private withMatrixKit (run : DataStepKit -> unit) : unit =
        withDataStepKit (fun kit ->
            let proxy = kit.loaded.kit.proxy
            for item in benchItems do
                proxy.writeItem item |> SceneTestFixture.kitSuccess
            for setup in [ setup1; benchB ] do
                proxy.writeSetup setup |> SceneTestFixture.kitSuccess
            let gone : SetupReference =
                {
                    name = SetupName "Gone"
                    revision = SetupRevision.first
                }
            let over (id : ExperimentId) (setup : SetupReference) : Experiment =
                {
                    e1 with
                        id = id
                        name = id.defaultName
                        setup = setup
                }
            let experiments = [ over (ExperimentId 1) setup1.reference; over (ExperimentId 2) benchB.reference; over (ExperimentId 3) gone ]
            run
                {
                    kit with
                        loaded =
                            {
                                kit.loaded with
                                    experimentData = reviewData
                                    kitItems = benchItems
                                    kitSetups = [ darkSetup; setup1; benchB ]
                                    experimentCollection = { kit.loaded.experimentCollection with experiments = experiments }
                            }
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
        Assert.Equal("DataCorrectionNoteField", ConstructorUiId.dataCorrectionNoteField.value)
        Assert.Equal("DataCorrectionTrail", ConstructorUiId.dataCorrectionTrail.value)
        Assert.Equal("DataCorrectSetup_Bench B", (ConstructorUiId.dataCorrectSetupChoice benchB.name).value)
        Assert.Equal("ExperimentMatrix", ConstructorUiId.experimentMatrix.value)
        Assert.Equal("ExperimentMatrixEntry_1", (ConstructorUiId.experimentMatrixEntry (ExperimentId 1)).value)

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

    [<Fact>]
    let ``the correction panel offers every other setup of the kit and lists the audit trail`` () : unit =
        HeadlessSession.run (fun () ->
            withCorrectionModel (fun reviewed ->
                let choiceOf (setup : Setup) : ConstructorUiId = ConstructorUiId.dataCorrectSetupChoice setup.name
                let setupLine = "Setup: 'Setup 1' revision 1"
                let trailLine = "'Setup 0' revision 1 → 'Setup 1' revision 1: LP1 was at 30"
                withDataStepWindow reviewed (fun window _ ->
                    let panelTexts = textsUnder window ConstructorUiId.dataReviewPanel
                    Assert.Contains(setupLine, panelTexts)
                    Assert.Contains("Audit note:", panelTexts)
                    let field = Assert.IsType<TextBox>(SlotBarTests.controlById window ConstructorUiId.dataCorrectionNoteField)
                    Assert.Equal("", string field.Text)

                    // One choice, Bench B's, offered over a read-write kit: E1's own setup is not offered.
                    Assert.Equal<string list>([ "DataCorrectSetup_Bench B" ], idsUnder window ConstructorUiId.dataReviewPanel "DataCorrectSetup_")
                    Assert.Equal("Re-point to 'Bench B' revision 1", SlotBarTests.boxLabelOf window (choiceOf benchB))
                    SlotBarTests.assertAbsent window (choiceOf setup1)
                    let choice = SlotBarTests.controlById window (choiceOf benchB)
                    Assert.True(choice.IsEffectivelyEnabled, "the re-point must be offered over a read-write kit")

                    // The trail's one line, oldest first.
                    Assert.Equal<string list>([ trailLine ], textsUnder window ConstructorUiId.dataCorrectionTrail)

                    // The block closes the panel, under the last point: the setup, the note, the choice, then the trail.
                    let lineOf (text : string) : Control =
                        (SlotBarTests.controlById window ConstructorUiId.dataReviewPanel).GetVisualDescendants()
                        |> Seq.choose (function :? TextBlock as block when block.Text = text -> Some (block :> Control) | _ -> None)
                        |> Seq.exactlyOne
                    let tops =
                        [
                            SlotBarTests.controlById window (ConstructorUiId.dataPointRow (MappedPointNumber 3))
                            lineOf setupLine
                            field :> Control
                            choice
                            SlotBarTests.controlById window ConstructorUiId.dataCorrectionTrail
                        ]
                        |> List.map (fun control -> (SlotBarTests.topLeftOf window control).Y)
                    Assert.Equal<float list>(List.sort tops, tops)
                    Assert.Equal(5, tops |> List.distinct |> List.length))

                // Spec 0050 S9.3: an experiment never corrected has no trail, and every setup of the kit but its own is offered.
                let uncorrected =
                    {
                        reviewed with
                            experimentCollection =
                                {
                                    reviewed.experimentCollection with
                                        experiments = reviewed.experimentCollection.experiments |> List.map (fun held -> { held with corrections = [] })
                                }
                            kitSetups = [ darkSetup; setup1; benchB ]
                    }
                withDataStepWindow uncorrected (fun window _ ->
                    Assert.Contains(setupLine, textsUnder window ConstructorUiId.dataReviewPanel)
                    SlotBarTests.assertAbsent window ConstructorUiId.dataCorrectionTrail
                    Assert.Equal<string list>(
                        [ "DataCorrectSetup_Dark"; "DataCorrectSetup_Bench B" ],
                        idsUnder window ConstructorUiId.dataReviewPanel "DataCorrectSetup_")
                    Assert.Equal("Re-point to 'Dark' revision 1", SlotBarTests.boxLabelOf window (choiceOf darkSetup)))

                // Corrected again to Bench B: the trail reads oldest first, and Setup 1 is now offered instead.
                let toBenchB : SetupCorrection =
                    {
                        previous = setup1.reference
                        corrected = benchB.reference
                        note = CorrectionNote "LR was at -45"
                    }
                let twice =
                    {
                        reviewed with
                            experimentCollection =
                                {
                                    reviewed.experimentCollection with
                                        experiments =
                                            reviewed.experimentCollection.experiments
                                            |> List.map (fun held ->
                                                {
                                                    held with
                                                        setup = benchB.reference
                                                        corrections = [ lp1Correction; toBenchB ]
                                                })
                                }
                    }
                withDataStepWindow twice (fun window _ ->
                    Assert.Contains("Setup: 'Bench B' revision 1", textsUnder window ConstructorUiId.dataReviewPanel)
                    Assert.Equal<string list>(
                        [ trailLine; "'Setup 1' revision 1 → 'Bench B' revision 1: LR was at -45" ],
                        textsUnder window ConstructorUiId.dataCorrectionTrail)
                    Assert.Equal<string list>([ "DataCorrectSetup_Setup 1" ], idsUnder window ConstructorUiId.dataReviewPanel "DataCorrectSetup_"))))

    [<Fact>]
    let ``pressing a setup sends CorrectSetup with the typed audit note`` () : unit =
        HeadlessSession.run (fun () ->
            withCorrectionModel (fun reviewed ->
                let notesOf (messages : ResizeArray<TableAndElementRotationMsg>) : ControlCaption list =
                    messages |> Seq.choose (function SetCorrectionNote note -> Some note | _ -> None) |> List.ofSeq
                let correctionsOf (messages : ResizeArray<TableAndElementRotationMsg>) : (ExperimentId * SetupReference * ControlCaption) list =
                    messages |> Seq.choose (function CorrectSetup (id, reference, note) -> Some (id, reference, note) | _ -> None) |> List.ofSeq
                let lrNote = ControlCaption "LR was at -45"
                let choice = ConstructorUiId.dataCorrectSetupChoice benchB.name
                withRecordingWindow reviewed (fun window messages ->
                    // The note commits on Enter and on losing focus; a commit is idempotent (step 325's reason field).
                    let field = Assert.IsType<TextBox>(SlotBarTests.controlById window ConstructorUiId.dataCorrectionNoteField)
                    field.Text <- lrNote.value
                    field.RaiseEvent(KeyEventArgs(RoutedEvent = InputElement.KeyDownEvent, Key = Key.Enter))
                    Assert.Equal<ControlCaption list>([ lrNote ], notesOf messages |> List.distinct)
                    messages.Clear()
                    field.RaiseEvent(FocusChangedEventArgs(InputElement.LostFocusEvent))
                    Assert.Equal<ControlCaption list>([ lrNote ], notesOf messages |> List.distinct)
                    messages.Clear()

                    // Spec 0050 S4.3, S9.3: the recording dispatch keeps the draft blank, so the note sent is the field's
                    // live text, and not the reason field's.
                    let reasonField = Assert.IsType<TextBox>(SlotBarTests.controlById window ConstructorUiId.dataExclusionReasonField)
                    reasonField.Text <- "stray light"
                    SlotBarTests.pressBox window choice
                    Assert.Equal<(ExperimentId * SetupReference * ControlCaption) list>(
                        [ ExperimentId 1, benchB.reference, lrNote ],
                        correctionsOf messages)
                    Assert.Equal<ControlCaption list>([ lrNote ], notesOf messages)
                    Assert.Empty(messages |> Seq.filter (function SetExclusionReason _ | ExcludePoint _ -> true | _ -> false)))

                // Spec 0050 S9.3: while the kit takes no writes, the re-point is disabled and sends nothing.
                let readOnly =
                    {
                        reviewed with kit = { reviewed.kit with access = ReadOnlyKitAccess }
                    }
                Assert.Equal<ControlAvailability>(ControlWithheld, readOnly.kitWrites)
                withRecordingWindow readOnly (fun window messages ->
                    Assert.False((SlotBarTests.controlById window choice).IsEffectivelyEnabled, "the re-point must be disabled while the kit takes no writes")
                    Assert.Equal("Re-point to 'Bench B' revision 1", SlotBarTests.boxLabelOf window choice)
                    let field = Assert.IsType<TextBox>(SlotBarTests.controlById window ConstructorUiId.dataCorrectionNoteField)
                    field.Text <- lrNote.value
                    SlotBarTests.pressBox window choice
                    Assert.Empty(correctionsOf messages))))

    [<Fact>]
    let ``the matrix shows each experiment in its row and column with its data badge, and lists the unplaced ones`` () : unit =
        // Spec 0050 S9.3: sources and analyzers joined by ` + `, objects at their angles by ` → `, and `—` for none.
        let quartz : MatrixObject = { item = ItemLabel "QZ"; angle = Angle.degree -45.0<deg> }
        let tilted : MatrixObject = { item = lp1.label; angle = Angle.degree 22.5<deg> }
        let both : MatrixRowKey = { sources = [ cpl1.label; cpl2.label ]; objects = [ tilted; quartz ] }
        let bare : MatrixRowKey = { sources = []; objects = [] }
        Assert.Equal("CPL1 + CPL2 · LP1@22.5° → QZ@-45°", both.caption.value)
        Assert.Equal("— · —", bare.caption.value)
        Assert.Equal("CPL2 + CPL1", ({ analyzers = [ cpl2.label; cpl1.label ] } : MatrixColumnKey).caption.value)
        Assert.Equal("—", ({ analyzers = [] } : MatrixColumnKey).caption.value)

        let entryOf (id : ExperimentId) : ConstructorUiId = ConstructorUiId.experimentMatrixEntry id
        let assertInCell (row : int) (column : int) (control : Control) : unit =
            Assert.Equal(row, Grid.GetRow control)
            Assert.Equal(column, Grid.GetColumn control)
        let setup1Row = "CPL1 · LP1@0°"
        let benchBRow = "CPL1 · LP1@30°"
        HeadlessSession.run (fun () ->
            withMatrixKit (fun kit ->
                withDataStepWindow kit.loaded (fun window _ ->
                    // The step opens on the matrix, above the data files.
                    let panelTexts = textsUnder window ConstructorUiId.dataStepPanel
                    Assert.Equal("Experiment matrix:", List.head panelTexts)
                    let grid = Assert.IsType<Grid>(SlotBarTests.controlById window ConstructorUiId.experimentMatrix)
                    let gridBottom = (SlotBarTests.topLeftOf window grid).Y + grid.Bounds.Height
                    let filesTop = (SlotBarTests.topLeftOf window (SlotBarTests.controlById window (ConstructorUiId.dataFileStatus (ExperimentId 1)))).Y
                    Assert.True(gridBottom <= filesTop, $"the matrix's bottom {gridBottom} must be above the data files' {filesTop}")

                    // The analyzer column's caption, then each row's caption and its experiment, its badge after its name.
                    Assert.Equal<string list>(
                        [ "CPL2"; setup1Row; "E1 · no data"; benchBRow; "E2 · no data" ],
                        textsUnder window ConstructorUiId.experimentMatrix)
                    let captionOf (text : string) : Control =
                        grid.Children |> Seq.find (function :? TextBlock as caption -> caption.Text = text | _ -> false)
                    assertInCell 0 1 (captionOf "CPL2")
                    assertInCell 1 0 (captionOf setup1Row)
                    assertInCell 2 0 (captionOf benchBRow)
                    let cellOf (id : ExperimentId) : Control = Assert.IsType<StackPanel>((SlotBarTests.controlById window (entryOf id)).Parent)
                    assertInCell 1 1 (cellOf (ExperimentId 1))
                    assertInCell 2 1 (cellOf (ExperimentId 2))
                    // Nothing is reviewed, so no entry is on the positive fill.
                    for id in [ ExperimentId 1; ExperimentId 2 ] do
                        Assert.NotEqual(MaterialsWindowView.positiveBackground, fillOf window (entryOf id))

                    // E3's setup is not in the kit: it has no entry and is named apart.
                    SlotBarTests.assertAbsent window (entryOf (ExperimentId 3))
                    Assert.Contains("Not in the matrix (setup missing): E3", panelTexts))

                // Attached through the mock, E1's data is validated: its entry reads the new badge, and E2's is unchanged.
                let attached =
                    kit.loaded |> updateTableAndElementRotation (AttachDataFile (ExperimentId 1, kit.phoneFile, ImportProfileName.intensitySeries))
                match attached.kitStatus with
                | Some told -> Assert.Fail $"E1's data must be attached: {told.value}"
                | None -> ()
                withDataStepWindow attached (fun window _ ->
                    Assert.Equal("E1 · validated (3 points)", SlotBarTests.boxLabelOf window (entryOf (ExperimentId 1)))
                    Assert.Equal("E2 · no data", SlotBarTests.boxLabelOf window (entryOf (ExperimentId 2))))

                // With no experiment, the matrix gives way to its placeholder.
                let empty =
                    {
                        kit.loaded with experimentCollection = { kit.loaded.experimentCollection with experiments = [] }
                    }
                withDataStepWindow empty (fun window _ ->
                    SlotBarTests.assertAbsent window ConstructorUiId.experimentMatrix
                    let texts = textsUnder window ConstructorUiId.dataStepPanel
                    Assert.Equal<string list>([ "Experiment matrix:"; "(no experiments yet)" ], List.truncate 2 texts)
                    Assert.DoesNotContain(texts, fun text -> text.StartsWith "Not in the matrix"))))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``pressing a matrix entry reviews that experiment`` () : unit =
        HeadlessSession.run (fun () ->
            withMatrixKit (fun kit ->
                withDataStepWindow kit.loaded (fun window answered ->
                    SlotBarTests.assertAbsent window ConstructorUiId.dataReviewPanel

                    SlotBarTests.pressBox window (ConstructorUiId.experimentMatrixEntry (ExperimentId 2))

                    match (SlotBarTests.latestOf kit.loaded answered).reviewedDataOpt with
                    | Some reviewed -> Assert.Equal<ExperimentId>(ExperimentId 2, reviewed.experimentId)
                    | None -> Assert.Fail "Pressing E2's entry must review E2"
                    Assert.True((SlotBarTests.controlById window ConstructorUiId.dataReviewPanel).IsEffectivelyVisible, "the review panel must show")
                    Assert.Equal("Points of E2 — no data", List.head (textsUnder window ConstructorUiId.dataReviewPanel))
                    Assert.Equal(MaterialsWindowView.positiveBackground, fillOf window (ConstructorUiId.experimentMatrixEntry (ExperimentId 2)))
                    Assert.NotEqual(MaterialsWindowView.positiveBackground, fillOf window (ConstructorUiId.experimentMatrixEntry (ExperimentId 1))))))
