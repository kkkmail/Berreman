namespace OpticalConstructor.Ui.Tests

open System.IO
open System.Text
open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Media
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open Softellect.Sys.Primitives
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
/// group matched, offered with its toggle or unmatched with its reason, above Apply split and Cancel in one row (spec 0050
/// S9.3; 001 §O). Its windows are found and pressed through `SlotBarTests`' helpers.
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
