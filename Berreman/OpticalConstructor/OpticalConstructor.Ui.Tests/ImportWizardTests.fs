namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Hosts
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Media
open Avalonia.Threading
open Avalonia.VisualTree
open Elmish
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Ui
open OpticalConstructor.Ui.ImportWizardView

/// Spec 0050 S9.2 (001 §K): the column-mapping wizard changes only its own state until Attach hands the kit window
/// the profile to attach by and any new one to save.
module ImportWizardTests =

    /// One call the wizard made on its context, in the order it made them.
    type private WizardCall =
        | AppliedCall of ImportWizardApplied
        | CloseCall

    type private WizardObservation =
        {
            context : ImportWizardContext
            reads : ResizeArray<FileName>
            calls : ResizeArray<WizardCall>
        }

    let private labFile : FileName = FileName "C:/Lab/2026-07-15/phone-roi.csv"

    let private phoneRoiText : string =
        "experiment,capture_index,captured_at,description,avg_total\n"
        + "CPL-(AIR#0)-CPL,0,2026-07-15T12:00:00+00:00,0,869.5\n"
        + "CPL-(AIR#0)-CPL,1,2026-07-15T12:05:00+00:00,45,912.25\n"

    /// The lab's bytes: the UTF-8 byte-order mark EF BB BF, then the text.
    let private phoneRoiSource : LabFileSource =
        {
            file = labFile
            content = FileByteContent (Array.append [| 0xEFuy; 0xBBuy; 0xBFuy |] (System.Text.Encoding.UTF8.GetBytes phoneRoiText))
        }

    /// Step 299's `Lab scan` fixture.
    let private labScanProfile : ImportProfile =
        {
            name = ImportProfileName "Lab scan"
            delimiter = CommaDelimiter
            header = HeaderOnFirstRow
            bom = ByteOrderMarkAbsent
            roles =
                {
                    scan =
                        [
                            {
                                column = DataColumnNumber 1
                                valueUnit = DegreeColumnUnit
                                reading = PhysicalScanValue
                            }
                        ]
                    reading = SignalColumns (DataColumnNumber 2, Some (DataColumnNumber 3))
                    timestampOpt = None
                    captureIndexOpt = None
                    observableOpt = None
                    keyOpt = Some (DataColumnNumber 4)
                }
        }

    let private intensitySeries : ImportProfileName = ImportProfileName "Intensity series"
    let private phoneRoiCsv : ImportProfileName = ImportProfileName "Phone ROI CSV"

    let private observeWizard (read : FileName -> Result<LabFileSource, KitError>) : WizardObservation =
        let reads = ResizeArray<FileName>()
        let calls = ResizeArray<WizardCall>()

        let readPickedFile (file : FileName) : Result<LabFileSource, KitError> =
            reads.Add file
            read file

        let request : ImportWizardRequest =
            {
                experimentId = ExperimentId 1
                file = labFile
                readPickedFile = readPickedFile
                kitProfiles = [ labScanProfile ]
                defaultProfile = intensitySeries
            }

        let context : ImportWizardContext =
            {
                request = request
                onApplied = fun applied -> calls.Add (AppliedCall applied)
                requestClose = fun () -> calls.Add CloseCall
            }

        { context = context; reads = reads; calls = calls }

    let private observePhoneRoi () : WizardObservation = observeWizard (fun _ -> Ok phoneRoiSource)

    let private updatedBy (msgs : ImportWizardMsg list) (m : ImportWizardModel) : ImportWizardModel =
        msgs |> List.fold (fun model msg -> updateImportWizard msg model) m

    let private choiceOf (column : int) (role : ColumnRoleChoice) : ColumnChoice = { column = DataColumnNumber column; role = role }

    let private unmapped (count : int) : ColumnChoice list = [ for n in 1 .. count -> choiceOf n UnmappedColumnRole ]

    let private describedError (errorOpt : ImportProfileError option) : string =
        match errorOpt with
        | Some error -> error.describe
        | None -> "no error"

    /// One cell of the mounted preview grid: where it sits and what it shows.
    type private GridCellShown =
        {
            row : int
            column : int
            text : string
        }

    let private tryControl (root : Control) (id : ConstructorUiId) : Control option =
        root.GetVisualDescendants()
        |> Seq.tryPick (function
            | :? Control as control when AutomationProperties.GetAutomationId control = id.value -> Some control
            | _ -> None)

    let private controlOf (root : Control) (id : ConstructorUiId) : Control =
        match tryControl root id with
        | Some control -> control
        | None -> failwith $"Missing control {id.value}."

    let private assertAbsent (root : Control) (id : ConstructorUiId) : unit =
        match tryControl root id with
        | Some _ -> Assert.Fail $"{id.value} must be absent."
        | None -> ()

    let private textsUnder (control : Control) : string list =
        control.GetVisualDescendants()
        |> Seq.choose (function :? TextBlock as text -> Some text.Text | _ -> None)
        |> Seq.toList

    let private fillOf (control : Control) : Color =
        match control with
        | :? Border as box ->
            match box.Background with
            | :? SolidColorBrush as fill -> fill.Color
            | _ -> failwith $"{AutomationProperties.GetAutomationId control} has no solid fill."
        | _ -> failwith $"{AutomationProperties.GetAutomationId control} is not a box."

    let private topLeftOf (window : Window) (control : Control) : Point =
        match control.TranslatePoint(Point(0.0, 0.0), window) |> Option.ofNullable with
        | Some point -> point
        | None -> failwith $"{AutomationProperties.GetAutomationId control} has no position in its window."

    /// Presses the box carrying `id` at its centre, as `ImportDialogTests` presses one.
    let private pressControl (window : Window) (id : ConstructorUiId) : unit =
        let control = controlOf window id
        control.BringIntoView() |> ignore
        Dispatcher.UIThread.RunJobs()
        match control.TranslatePoint(Point(control.Bounds.Width / 2.0, control.Bounds.Height / 2.0), window) |> Option.ofNullable with
        | Some centre ->
            window.MouseDown(centre, MouseButton.Left, RawInputModifiers.None)
            Dispatcher.UIThread.RunJobs()
            window.MouseUp(centre, MouseButton.Left, RawInputModifiers.None)
            Dispatcher.UIThread.RunJobs()
        | None -> Assert.Fail $"{id.value} has no position in its window."

    /// Picks the item shown as `caption` in the list carrying `id`, as a user's pick sets it.
    let private pickIn (window : Window) (id : ConstructorUiId) (caption : string) : unit =
        let list = controlOf window id :?> ComboBox
        let index = list.Items |> Seq.cast<obj> |> Seq.findIndex (fun item -> string item = caption)
        list.SelectedIndex <- index
        Dispatcher.UIThread.RunJobs()

    let private shownIn (window : Window) (id : ConstructorUiId) : string = string (controlOf window id :?> ComboBox).SelectedItem

    let private itemsIn (window : Window) (id : ConstructorUiId) : string list =
        (controlOf window id :?> ComboBox).Items |> Seq.cast<obj> |> Seq.map string |> List.ofSeq

    let private cellsOf (grid : Grid) : GridCellShown list =
        grid.Children
        |> Seq.choose (function
            | :? TextBlock as text -> Some { row = Grid.GetRow text; column = Grid.GetColumn text; text = text.Text }
            | _ -> None)
        |> List.ofSeq

    /// Mounts `importWizardView` over `model` as `ImportDialogTests` mounts the import dialog's view, recording every
    /// message the view sends.
    let private withMountedWizard (model : ImportWizardModel) (check : Window -> ResizeArray<ImportWizardMsg> -> unit) : unit =
        let sent = ResizeArray<ImportWizardMsg>()

        HeadlessSession.run (fun () ->
            let window = HostWindow(Width = 900.0, Height = 760.0)

            try
                Program.mkSimple (fun () -> model) (fun msg held -> sent.Add msg; updateImportWizard msg held) importWizardView
                |> Program.withHost window
                |> Program.run

                window.Show()
                Dispatcher.UIThread.RunJobs()
                check window sent
            finally
                window.Close())

    /// Columns 1 to 5 as key, capture index, timestamp, a raw dial reading in degrees and the signal.
    let private phoneRoiRoles : ImportWizardMsg list =
        [
            SetColumnRole (DataColumnNumber 1, KeyColumnRole)
            SetColumnRole (DataColumnNumber 2, CaptureIndexColumnRole)
            SetColumnRole (DataColumnNumber 3, TimestampColumnRole)
            SetColumnRole (DataColumnNumber 4, ScanColumnRole (DegreeColumnUnit, RawDialReading))
            SetColumnRole (DataColumnNumber 5, SignalColumnRole)
        ]

    [<Fact>]
    let ``the wizard opens on the picked file with its sniffed layout and the detector's profile chosen`` () : unit =
        let observed = observePhoneRoi ()
        let model = observed.context.initImportWizard

        Assert.Equal<DataDelimiter>(CommaDelimiter, model.delimiter)
        Assert.Equal<HeaderRowChoice>(HeaderOnFirstRow, model.header)
        Assert.Equal<ByteOrderMark>(ByteOrderMarkPresent, model.bom)
        Assert.Equal<WizardProfileChoice>(PickedImportProfile intensitySeries, model.choice)
        Assert.Equal<ColumnChoice list>(unmapped 5, model.columns)
        Assert.Equal<ControlCaption>(ControlCaption "", model.profileName)
        Assert.Equal<ImportProfileError option>(None, model.lastError)
        Assert.Equal<Result<LabFileSource, KitError>>(Ok phoneRoiSource, model.source)

        match model.preview with
        | Ok preview ->
            Assert.Equal<string list>(
                [ "experiment"; "capture_index"; "captured_at"; "description"; "avg_total" ],
                preview.columnNames |> List.map _.value)

            Assert.Equal(2, preview.rows.Length)
        | Error error -> Assert.Fail($"the picked file did not preview: {error.caption.value}")

        Assert.Equal<FileName list>([ labFile ], List.ofSeq observed.reads)
        Assert.Empty observed.calls

    [<Fact>]
    let ``picking a profile, changing the layout or mapping a column changes nothing outside the wizard`` () : unit =
        let observed = observePhoneRoi ()
        let opened = observed.context.initImportWizard

        let phoneRoi = opened |> updateImportWizard (PickImportProfile phoneRoiCsv)
        Assert.Equal<ImportWizardModel>({ opened with choice = PickedImportProfile phoneRoiCsv }, phoneRoi)

        let labScan = phoneRoi |> updateImportWizard (PickImportProfile labScanProfile.name)
        Assert.Equal<WizardProfileChoice>(PickedImportProfile labScanProfile.name, labScan.choice)
        Assert.Equal<DataDelimiter>(labScanProfile.delimiter, labScan.delimiter)
        Assert.Equal<HeaderRowChoice>(labScanProfile.header, labScan.header)
        Assert.Equal<ByteOrderMark>(labScanProfile.bom, labScan.bom)
        Assert.Equal<ColumnChoice list>(unmapped 5, labScan.columns)

        Assert.Equal<ImportWizardModel>(labScan, labScan |> updateImportWizard (PickImportProfile (ImportProfileName "Nope")))

        let mapped =
            labScan
            |> updatedBy [ SetColumnRole (DataColumnNumber 1, KeyColumnRole); SetColumnRole (DataColumnNumber 2, SignalColumnRole) ]

        Assert.Equal<WizardProfileChoice>(NewColumnMapping, mapped.choice)

        Assert.Equal<ColumnChoice list>(
            choiceOf 1 KeyColumnRole :: choiceOf 2 SignalColumnRole :: (unmapped 5 |> List.skip 2),
            mapped.columns)

        Assert.Equal<ImportWizardModel>(mapped, mapped |> updateImportWizard (SetColumnRole (DataColumnNumber 9, SignalColumnRole)))

        let semicolon = mapped |> updateImportWizard (SetImportDelimiter SemicolonDelimiter)
        Assert.Equal<DataDelimiter>(SemicolonDelimiter, semicolon.delimiter)
        Assert.Equal<ColumnChoice list>([ choiceOf 1 KeyColumnRole ], semicolon.columns)

        let comma = semicolon |> updateImportWizard (SetImportDelimiter CommaDelimiter)
        Assert.Equal<ColumnChoice list>(choiceOf 1 KeyColumnRole :: (unmapped 5 |> List.skip 1), comma.columns)

        let headless = comma |> updateImportWizard (SetImportHeader NoHeaderRow)
        Assert.Equal<HeaderRowChoice>(NoHeaderRow, headless.header)
        Assert.Equal<ColumnChoice list>(comma.columns, headless.columns)

        let named =
            headless
            |> updatedBy [ PickImportProfile phoneRoiCsv; StartColumnMapping; SetImportProfileName (ControlCaption "Bench A") ]

        Assert.Equal<WizardProfileChoice>(NewColumnMapping, named.choice)
        Assert.Equal<ControlCaption>(ControlCaption "Bench A", named.profileName)

        Assert.Equal<FileName list>([ labFile ], List.ofSeq observed.reads)
        Assert.Empty observed.calls

    [<Fact>]
    let ``Attach with a picked profile hands it to the kit window once and closes`` () : unit =
        let observed = observePhoneRoi ()

        observed.context.initImportWizard
        |> updatedBy [ PickImportProfile phoneRoiCsv; ApplyImportWizard ]
        |> ignore

        let expected : ImportWizardApplied =
            {
                experimentId = ExperimentId 1
                file = labFile
                profile = phoneRoiCsv
                savedOpt = None
            }

        Assert.Equal<WizardCall list>([ AppliedCall expected; CloseCall ], List.ofSeq observed.calls)

    [<Fact>]
    let ``Attach with a new mapping hands the kit window the profile to save and attaches by it`` () : unit =
        let observed = observePhoneRoi ()
        let profileName = ImportProfileName "Phone ROI by columns"

        observed.context.initImportWizard
        |> updatedBy (phoneRoiRoles @ [ SetImportProfileName (ControlCaption "Phone ROI by columns"); ApplyImportWizard ])
        |> ignore

        let saved : ImportProfile =
            {
                name = profileName
                delimiter = CommaDelimiter
                header = HeaderOnFirstRow
                bom = ByteOrderMarkPresent
                roles =
                    {
                        scan =
                            [
                                {
                                    column = DataColumnNumber 4
                                    valueUnit = DegreeColumnUnit
                                    reading = RawDialReading
                                }
                            ]
                        reading = SignalColumns (DataColumnNumber 5, None)
                        timestampOpt = Some (DataColumnNumber 3)
                        captureIndexOpt = Some (DataColumnNumber 2)
                        observableOpt = None
                        keyOpt = Some (DataColumnNumber 1)
                    }
            }

        let expected : ImportWizardApplied =
            {
                experimentId = ExperimentId 1
                file = labFile
                profile = profileName
                savedOpt = Some saved
            }

        Assert.Equal<WizardCall list>([ AppliedCall expected; CloseCall ], List.ofSeq observed.calls)

    [<Fact>]
    let ``a mapping that cannot be a profile is told and nothing is applied`` () : unit =
        let observed = observePhoneRoi ()
        let mapped = observed.context.initImportWizard |> updatedBy phoneRoiRoles

        let refusedAs (name : string) (extra : ImportWizardMsg list) : ImportWizardModel =
            let refused = mapped |> updatedBy (extra @ [ SetImportProfileName (ControlCaption name); ApplyImportWizard ])
            Assert.Empty observed.calls
            refused

        match (refusedAs "" []).lastError with
        | Some (BlankProfileNameErr reason) -> Assert.Equal("the profile name is empty", reason)
        | other -> Assert.Fail($"expected BlankProfileNameErr: {describedError other}")

        match (refusedAs "Intensity series" []).lastError with
        | Some (BuiltInProfileNameErr reason) -> Assert.Contains("Intensity series", reason)
        | other -> Assert.Fail($"expected BuiltInProfileNameErr: {describedError other}")

        let duplicate = refusedAs "Lab scan" []

        match duplicate.lastError with
        | Some (DuplicateProfileNameErr reason) -> Assert.Contains("Lab scan", reason)
        | other -> Assert.Fail($"expected DuplicateProfileNameErr: {describedError other}")

        match (refusedAs "Two signals" [ SetColumnRole (DataColumnNumber 4, SignalColumnRole) ]).lastError with
        | Some (SignalColumnCountErr reason) -> Assert.Contains("columns 4, 5", reason)
        | other -> Assert.Fail($"expected SignalColumnCountErr: {describedError other}")

        let renamed = duplicate |> updateImportWizard (SetImportProfileName (ControlCaption "Lab scan, bench B"))
        Assert.Equal<ImportProfileError option>(None, renamed.lastError)
        Assert.Empty observed.calls

    [<Fact>]
    let ``a picked file that cannot be read offers only Cancel, and Cancel applies nothing`` () : unit =
        let unread = KitIoErr "the picked file is locked by another program"
        let observed = observeWizard (fun _ -> Error unread)
        let model = observed.context.initImportWizard

        Assert.Equal<Result<LabFileSource, KitError>>(Error unread, model.source)
        Assert.Empty model.columns
        Assert.Equal<DataDelimiter>(CommaDelimiter, model.delimiter)
        Assert.Equal<HeaderRowChoice>(HeaderOnFirstRow, model.header)
        Assert.Equal<ByteOrderMark>(ByteOrderMarkAbsent, model.bom)

        match model.preview with
        | Error (MalformedDataFileErr reason) -> Assert.Equal(unread.describe, reason)
        | Error error -> Assert.Fail($"expected the read's refusal as MalformedDataFileErr: {error.caption.value}")
        | Ok _ -> Assert.Fail "an unread file previewed"

        let attached = model |> updateImportWizard ApplyImportWizard
        Assert.Equal<ImportWizardModel>(model, attached)

        let mappedAttach = model |> updatedBy [ StartColumnMapping; SetImportProfileName (ControlCaption "Bench A"); ApplyImportWizard ]
        Assert.Equal<ImportProfileError option>(None, mappedAttach.lastError)
        Assert.Empty observed.calls

        model |> updateImportWizard CancelImportWizard |> ignore
        Assert.Equal<WizardCall list>([ CloseCall ], List.ofSeq observed.calls)
        Assert.Equal<FileName list>([ labFile ], List.ofSeq observed.reads)

    [<Fact>]
    let ``the import wizard UiIds are the stable intent-named ids`` () : unit =
        let column = (DataColumnNumber 4).uiPosition

        Assert.Equal<string list>(
            [
                "ImportWizardWindow"
                "ImportWizardHeading"
                "ImportWizardDelimiter_comma"
                "ImportWizardHeader_first-row"
                "ImportWizardBomText"
                "ImportWizardProfile_Phone ROI CSV"
                "ImportWizardNewMappingButton"
                "ImportWizardProfileNameField"
                "ImportWizardPreviewGrid"
                "ImportWizardColumnRole_4"
                "ImportWizardColumnUnit_4"
                "ImportWizardColumnReading_4"
                "ImportWizardMessage"
                "ImportWizardAttachButton"
                "ImportWizardCancelButton"
            ],
            [
                ConstructorUiId.importWizardWindow
                ConstructorUiId.importWizardHeading
                ConstructorUiId.importWizardDelimiterChoice CommaDelimiter.uiKey
                ConstructorUiId.importWizardHeaderChoice HeaderOnFirstRow.uiKey
                ConstructorUiId.importWizardBomText
                ConstructorUiId.importWizardProfileChoice phoneRoiCsv.uiKey
                ConstructorUiId.importWizardNewMappingButton
                ConstructorUiId.importWizardProfileNameField
                ConstructorUiId.importWizardPreviewGrid
                ConstructorUiId.importWizardColumnRoleChoice column
                ConstructorUiId.importWizardColumnUnitChoice column
                ConstructorUiId.importWizardColumnReadingChoice column
                ConstructorUiId.importWizardMessage
                ConstructorUiId.importWizardAttachButton
                ConstructorUiId.importWizardCancelButton
            ]
            |> List.map _.value)

        Assert.Equal<string list>([ "comma"; "semicolon"; "tab" ], DataDelimiter.all |> List.map _.uiKey.value)
        Assert.Equal<string list>([ "first-row"; "no-header" ], HeaderRowChoice.all |> List.map _.uiKey.value)

    [<Fact>]
    let ``the wizard shows the sniffed layout, every profile and the preview, with Attach and Cancel in one row`` () : unit =
        let observed = observePhoneRoi ()

        withMountedWizard observed.context.initImportWizard (fun window sent ->
            let heading = controlOf window ConstructorUiId.importWizardHeading :?> TextBlock
            Assert.Equal("Import phone-roi.csv\nNothing is attached until you press Attach.", heading.Text)

            let delimiterBox (delimiter : DataDelimiter) : Control = controlOf window (ConstructorUiId.importWizardDelimiterChoice delimiter.uiKey)
            let headerBox (header : HeaderRowChoice) : Control = controlOf window (ConstructorUiId.importWizardHeaderChoice header.uiKey)
            Assert.Equal(MaterialsWindowView.positiveBackground, fillOf (delimiterBox CommaDelimiter))
            Assert.Equal<string list>([ "Comma" ], textsUnder (delimiterBox CommaDelimiter))

            for other in [ SemicolonDelimiter; TabDelimiter ] do
                Assert.NotEqual(MaterialsWindowView.positiveBackground, fillOf (delimiterBox other))

            Assert.Equal(MaterialsWindowView.positiveBackground, fillOf (headerBox HeaderOnFirstRow))
            Assert.NotEqual(MaterialsWindowView.positiveBackground, fillOf (headerBox NoHeaderRow))
            Assert.Equal("Byte-order mark: yes", (controlOf window ConstructorUiId.importWizardBomText :?> TextBlock).Text)

            for name in ImportProfileName.builtIns @ [ labScanProfile.name ] do
                Assert.Equal<string list>([ name.value ], textsUnder (controlOf window (ConstructorUiId.importWizardProfileChoice name.uiKey)))

            Assert.Equal(MaterialsWindowView.positiveBackground, fillOf (controlOf window (ConstructorUiId.importWizardProfileChoice intensitySeries.uiKey)))
            Assert.Equal<string list>([ "New column mapping" ], textsUnder (controlOf window ConstructorUiId.importWizardNewMappingButton))
            Assert.NotEqual(MaterialsWindowView.positiveBackground, fillOf (controlOf window ConstructorUiId.importWizardNewMappingButton))

            let grid = controlOf window ConstructorUiId.importWizardPreviewGrid :?> Grid
            Assert.Equal(5, grid.ColumnDefinitions.Count)

            let row (index : int) (texts : string list) : GridCellShown list =
                texts |> List.mapi (fun column text -> { row = index; column = column; text = text })

            Assert.Equal<GridCellShown list>(
                row 0 [ "experiment"; "capture_index"; "captured_at"; "description"; "avg_total" ]
                @ row 1 [ "CPL-(AIR#0)-CPL"; "0"; "2026-07-15T12:00:00+00:00"; "0"; "869.5" ]
                @ row 2 [ "CPL-(AIR#0)-CPL"; "1"; "2026-07-15T12:05:00+00:00"; "45"; "912.25" ],
                cellsOf grid)

            assertAbsent window ConstructorUiId.importWizardMessage

            let attach = controlOf window ConstructorUiId.importWizardAttachButton
            let cancel = controlOf window ConstructorUiId.importWizardCancelButton
            Assert.Equal(MaterialsWindowView.positiveBackground, fillOf attach)
            Assert.Equal(MaterialsWindowView.negativeBackground, fillOf cancel)
            Assert.Equal((topLeftOf window attach).Y, (topLeftOf window cancel).Y)
            Assert.True((topLeftOf window cancel).X > (topLeftOf window attach).X + attach.Bounds.Width, "Cancel must be to the right of Attach with a gap.")
            Assert.Empty sent)

        Assert.Empty observed.calls

    [<Fact>]
    let ``a new column mapping shows a role per column, and a scan role its unit and reading`` () : unit =
        let observed = observePhoneRoi ()
        let positions = [ 1 .. 5 ] |> List.map (fun number -> (DataColumnNumber number).uiPosition)
        let column4 = (DataColumnNumber 4).uiPosition

        withMountedWizard observed.context.initImportWizard (fun window sent ->
            for position in positions do
                assertAbsent window (ConstructorUiId.importWizardColumnRoleChoice position)

            assertAbsent window ConstructorUiId.importWizardProfileNameField

            pressControl window ConstructorUiId.importWizardNewMappingButton

            let grid = controlOf window ConstructorUiId.importWizardPreviewGrid

            let roleCaptions =
                [ "—"; "scan variable"; "signal"; "σ"; "timestamp"; "capture index"; "observable"; "experiment key"; "throughput" ]
                @ [ for i in 0 .. 3 do for j in 0 .. 3 -> $"m{i}{j}" ]

            for position in positions do
                controlOf grid (ConstructorUiId.importWizardColumnRoleChoice position) |> ignore
                Assert.Equal<string list>(roleCaptions, itemsIn window (ConstructorUiId.importWizardColumnRoleChoice position))
                Assert.Equal("—", shownIn window (ConstructorUiId.importWizardColumnRoleChoice position))
                assertAbsent window (ConstructorUiId.importWizardColumnUnitChoice position)
                assertAbsent window (ConstructorUiId.importWizardColumnReadingChoice position)

            Assert.Equal("", (controlOf window ConstructorUiId.importWizardProfileNameField :?> TextBox).Text)
            Assert.Equal(MaterialsWindowView.positiveBackground, fillOf (controlOf window ConstructorUiId.importWizardNewMappingButton))

            pickIn window (ConstructorUiId.importWizardColumnRoleChoice column4) "scan variable"

            for position in positions do
                let role = controlOf grid (ConstructorUiId.importWizardColumnRoleChoice position)

                if position = column4 then
                    Assert.Equal("scan variable", shownIn window (ConstructorUiId.importWizardColumnRoleChoice position))
                    Assert.Equal(3, Grid.GetColumn (role.GetVisualParent() :?> Control))
                    controlOf grid (ConstructorUiId.importWizardColumnUnitChoice position) |> ignore
                    controlOf grid (ConstructorUiId.importWizardColumnReadingChoice position) |> ignore
                else
                    Assert.Equal("—", shownIn window (ConstructorUiId.importWizardColumnRoleChoice position))
                    assertAbsent window (ConstructorUiId.importWizardColumnUnitChoice position)
                    assertAbsent window (ConstructorUiId.importWizardColumnReadingChoice position)

            Assert.Equal<string list>(ScanColumnUnit.all |> List.map _.text, itemsIn window (ConstructorUiId.importWizardColumnUnitChoice column4))
            Assert.Equal<string list>([ "raw dial reading"; "physical value" ], itemsIn window (ConstructorUiId.importWizardColumnReadingChoice column4))
            Assert.Equal("°", shownIn window (ConstructorUiId.importWizardColumnUnitChoice column4))
            Assert.Equal("physical value", shownIn window (ConstructorUiId.importWizardColumnReadingChoice column4))

            pickIn window (ConstructorUiId.importWizardColumnUnitChoice column4) RadianColumnUnit.text
            pickIn window (ConstructorUiId.importWizardColumnReadingChoice column4) "raw dial reading"
            Assert.Equal("scan variable", shownIn window (ConstructorUiId.importWizardColumnRoleChoice column4))

            // Leaving the mapping and coming back recreates the lists over the held roles and sends nothing.
            pressControl window (ConstructorUiId.importWizardProfileChoice phoneRoiCsv.uiKey)
            assertAbsent window (ConstructorUiId.importWizardColumnRoleChoice column4)
            assertAbsent window ConstructorUiId.importWizardProfileNameField
            pressControl window ConstructorUiId.importWizardNewMappingButton
            Assert.Equal("scan variable", shownIn window (ConstructorUiId.importWizardColumnRoleChoice column4))
            Assert.Equal(RadianColumnUnit.text, shownIn window (ConstructorUiId.importWizardColumnUnitChoice column4))
            Assert.Equal("raw dial reading", shownIn window (ConstructorUiId.importWizardColumnReadingChoice column4))
            Assert.Equal("—", shownIn window (ConstructorUiId.importWizardColumnRoleChoice (DataColumnNumber 1).uiPosition))

            Assert.Equal<ImportWizardMsg list>(
                [
                    StartColumnMapping
                    SetColumnRole (DataColumnNumber 4, ScanColumnRole (DegreeColumnUnit, PhysicalScanValue))
                    SetColumnRole (DataColumnNumber 4, ScanColumnRole (RadianColumnUnit, PhysicalScanValue))
                    SetColumnRole (DataColumnNumber 4, ScanColumnRole (RadianColumnUnit, RawDialReading))
                    PickImportProfile phoneRoiCsv
                    StartColumnMapping
                ],
                List.ofSeq sent))

        Assert.Empty observed.calls

    [<Fact>]
    let ``a refused Attach shows its reason in the wizard`` () : unit =
        let observed = observePhoneRoi ()

        withMountedWizard observed.context.initImportWizard (fun window sent ->
            assertAbsent window ConstructorUiId.importWizardMessage

            pressControl window ConstructorUiId.importWizardNewMappingButton
            pressControl window ConstructorUiId.importWizardAttachButton

            let message = controlOf window ConstructorUiId.importWizardMessage :?> TextBlock
            Assert.Contains("the import profile name is blank", message.Text)
            Assert.Equal(MaterialsWindowView.messageColor, (message.Foreground :?> SolidColorBrush).Color)
            Assert.Equal<ImportWizardMsg list>([ StartColumnMapping; ApplyImportWizard ], List.ofSeq sent)

            (controlOf window ConstructorUiId.importWizardProfileNameField :?> TextBox).Text <- "Bench A"
            Dispatcher.UIThread.RunJobs()
            assertAbsent window ConstructorUiId.importWizardMessage

            pressControl window ConstructorUiId.importWizardAttachButton
            let unmappedRefusal = controlOf window ConstructorUiId.importWizardMessage :?> TextBlock
            Assert.Contains("a signal mapping needs exactly one signal column", unmappedRefusal.Text)

            // Leaving the mapping and coming back recreates the name field over the held name and sends nothing.
            pressControl window (ConstructorUiId.importWizardProfileChoice phoneRoiCsv.uiKey)
            assertAbsent window ConstructorUiId.importWizardMessage
            pressControl window ConstructorUiId.importWizardNewMappingButton
            Assert.Equal("Bench A", (controlOf window ConstructorUiId.importWizardProfileNameField :?> TextBox).Text)

            Assert.Equal<ImportWizardMsg list>(
                [
                    StartColumnMapping
                    ApplyImportWizard
                    SetImportProfileName (ControlCaption "Bench A")
                    ApplyImportWizard
                    PickImportProfile phoneRoiCsv
                    StartColumnMapping
                ],
                List.ofSeq sent))

        Assert.Empty observed.calls

        let unread = KitIoErr "the picked file is locked by another program"
        let locked = observeWizard (fun _ -> Error unread)

        withMountedWizard locked.context.initImportWizard (fun window sent ->
            let message = controlOf window ConstructorUiId.importWizardMessage :?> TextBlock
            Assert.Equal($"The file could not be read — {unread.describe}", message.Text)
            assertAbsent window ConstructorUiId.importWizardAttachButton
            controlOf window ConstructorUiId.importWizardCancelButton |> ignore
            assertAbsent window ConstructorUiId.importWizardPreviewGrid
            Assert.Empty sent)

        Assert.Empty locked.calls
