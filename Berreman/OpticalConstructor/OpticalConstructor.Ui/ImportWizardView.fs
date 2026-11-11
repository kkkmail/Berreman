/// The column-mapping import wizard's MVU (001 §K; spec 0050 S9.2).
/// Nothing is attached or saved until Attach hands the kit window its choice.
module OpticalConstructor.Ui.ImportWizardView

open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Storage.LabFileImport

/// What the kit window hands the wizard: the experiment, the picked file, its read and the profiles to offer.
[<ReferenceEquality>]
type ImportWizardRequest =
    {
        /// The experiment the file is attached to.
        experimentId : Experiments.ExperimentId
        /// The picked lab file's absolute name.
        file : FileName
        /// Reads the picked file's bytes, the wizard's one read.
        readPickedFile : FileName -> Result<LabFileSource, KitError>
        /// The kit's own import profiles, in the kit's order.
        kitProfiles : ImportProfile list
        /// The profile the experiment's detector reads by, chosen when the wizard opens.
        defaultProfile : ImportProfileName
    }

/// What Attach chose: the profile to attach the file by, and the new profile to save first.
type ImportWizardApplied =
    {
        /// The experiment the file is attached to.
        experimentId : Experiments.ExperimentId
        /// The picked lab file's absolute name.
        file : FileName
        /// The profile the file is mapped by.
        profile : ImportProfileName
        /// The new column mapping, saved in the kit before the file is mapped by it.
        savedOpt : ImportProfile option
    }

/// The kit window's request and callbacks, in the shape of step 107's `ImportDialogContext`.
[<ReferenceEquality>]
type ImportWizardContext =
    {
        /// The picked file, its read and the profiles the wizard offers.
        request : ImportWizardRequest
        /// Hands the kit window what Attach chose.
        onApplied : ImportWizardApplied -> unit
        /// Closes the owning window after Attach or Cancel.
        requestClose : unit -> unit
    }

/// The profile Attach maps the file by.
type WizardProfileChoice =
    /// A built-in or kit profile of that name.
    | PickedImportProfile of ImportProfileName
    /// The wizard's own column roles, saved as a new kit profile on Attach.
    | NewColumnMapping

/// The complete wizard state, independent of a window.
type ImportWizardModel =
    {
        /// The kit window's request and callbacks.
        context : ImportWizardContext
        /// The picked file as read, or why it could not be.
        source : Result<LabFileSource, KitError>
        /// The character between a row's cells.
        delimiter : DataDelimiter
        /// Whether the first row names the columns.
        header : HeaderRowChoice
        /// Whether the lab's files start with the UTF-8 byte-order mark.
        bom : ByteOrderMark
        /// The profile Attach maps the file by.
        choice : WizardProfileChoice
        /// One role per preview column, in column order.
        columns : ColumnChoice list
        /// The new mapping's profile name as typed.
        profileName : ControlCaption
        /// The latest Attach refusal, kept until the next change.
        lastError : ImportProfileError option
    }

    /// The file's column names and first rows as the layout reads them; an unread file is `MalformedDataFileErr`.
    member this.preview : Result<LabFilePreview, ExperimentDataError> =
        match this.source with
        | Ok source -> source.content.kitFileText.previewWith this.delimiter this.header
        | Error error -> Error (MalformedDataFileErr error.describe)

/// Semantic actions shared by the controls and model tests.
type ImportWizardMsg =
    /// Reads the file by this delimiter.
    | SetImportDelimiter of DataDelimiter
    /// Reads the file with or without a header row.
    | SetImportHeader of HeaderRowChoice
    /// Maps the file by this built-in or kit profile.
    | PickImportProfile of ImportProfileName
    /// Maps the file by the wizard's own column roles.
    | StartColumnMapping
    /// Gives this column this role, mapping the file by the wizard's own column roles.
    | SetColumnRole of DataColumnNumber * ColumnRoleChoice
    /// Names the new mapping's profile.
    | SetImportProfileName of ControlCaption
    /// Hands the kit window the chosen profile and any new one to save, then closes.
    | ApplyImportWizard
    /// Closes without attaching or saving anything.
    | CancelImportWizard

/// One choice per preview column, numbered from 1: a held column keeps its role, a new one is unmapped, and a failed
/// preview has none (spec 0050 S9.2).
let private wizardColumnsOver (held : ColumnChoice list) (preview : Result<LabFilePreview, ExperimentDataError>) : ColumnChoice list =
    match preview with
    | Error _ -> []
    | Ok shown ->
        shown.columnNames
        |> List.mapi (fun index _ ->
            let column = DataColumnNumber (index + 1)

            let role =
                held
                |> List.tryFind (fun choice -> choice.column = column)
                |> Option.map _.role
                |> Option.defaultValue UnmappedColumnRole

            let choice : ColumnChoice = { column = column; role = role }
            choice)

type ImportWizardContext with

    /// Opens on the picked file's one read and its sniffed layout (an unread file: comma, a header row, no mark), the
    /// detector's profile chosen, one unmapped column per preview column, no name and no error (spec 0050 S9.2).
    member this.initImportWizard : ImportWizardModel =
        let source = this.request.readPickedFile this.request.file

        let layout : LabFileSniff =
            match source with
            | Ok read -> read.sniffedLayout
            | Error _ -> { bom = ByteOrderMarkAbsent; delimiter = CommaDelimiter; header = HeaderOnFirstRow }

        let opened : ImportWizardModel =
            {
                context = this
                source = source
                delimiter = layout.delimiter
                header = layout.header
                bom = layout.bom
                choice = PickedImportProfile this.request.defaultProfile
                columns = []
                profileName = ControlCaption ""
                lastError = None
            }

        { opened with columns = wizardColumnsOver [] opened.preview }

/// Changes only the wizard's state; Attach over a read file and Cancel alone reach the context (spec 0050 S9.2; .spec-md
/// §10.3: nothing is attached or saved until Attach).
let updateImportWizard (msg : ImportWizardMsg) (m : ImportWizardModel) : ImportWizardModel =
    let request = m.context.request

    // A changed layout changes the preview, so its columns are remade (spec 0050 S9.2).
    let relaid (laid : ImportWizardModel) : ImportWizardModel = { laid with columns = wizardColumnsOver laid.columns laid.preview }

    let applied (profile : ImportProfileName) (savedOpt : ImportProfile option) : ImportWizardModel =
        let chosen : ImportWizardApplied =
            {
                experimentId = request.experimentId
                file = request.file
                profile = profile
                savedOpt = savedOpt
            }

        m.context.onApplied chosen
        m.context.requestClose ()
        { m with lastError = None }

    match msg with
    | SetImportDelimiter delimiter -> relaid { m with delimiter = delimiter; lastError = None }
    | SetImportHeader header -> relaid { m with header = header; lastError = None }
    | PickImportProfile name ->
        match ChosenImportProfile.tryOfName request.kitProfiles name with
        | Some (BuiltInProfileChoice _) -> { m with choice = PickedImportProfile name; lastError = None }
        | Some (KitProfileChoice profile) ->
            relaid
                {
                    m with
                        choice = PickedImportProfile name
                        delimiter = profile.delimiter
                        header = profile.header
                        bom = profile.bom
                        lastError = None
                }
        | None -> m
    | StartColumnMapping -> { m with choice = NewColumnMapping; lastError = None }
    | SetColumnRole (column, role) ->
        if m.columns |> List.exists (fun choice -> choice.column = column) then
            {
                m with
                    columns = m.columns |> List.map (fun choice -> if choice.column = column then { choice with role = role } else choice)
                    choice = NewColumnMapping
                    lastError = None
            }
        else
            m
    | SetImportProfileName name -> { m with profileName = name; lastError = None }
    | ApplyImportWizard ->
        match m.source, m.choice with
        | Error _, _ -> m
        | Ok _, PickedImportProfile name -> applied name None
        | Ok _, NewColumnMapping ->
            let nameResult : Result<ImportProfileName, ImportProfileError> =
                match ImportProfileName.tryCreate m.profileName.value with
                | Some name -> name.availableAmong request.kitProfiles
                | None -> Error (BlankProfileNameErr "the profile name is empty")

            let profileResult : Result<ImportProfile, ImportProfileError> =
                nameResult
                |> Result.bind (fun name ->
                    ColumnRoles.tryOfChoices m.columns
                    |> Result.map (fun roles ->
                        let profile : ImportProfile =
                            {
                                name = name
                                delimiter = m.delimiter
                                header = m.header
                                bom = m.bom
                                roles = roles
                            }

                        profile))

            match profileResult with
            | Ok profile -> applied profile.name (Some profile)
            | Error error -> { m with lastError = Some error }
    | CancelImportWizard ->
        m.context.requestClose ()
        m
