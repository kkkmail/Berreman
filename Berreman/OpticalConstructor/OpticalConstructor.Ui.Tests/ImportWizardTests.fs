namespace OpticalConstructor.Ui.Tests

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
