/// The column-mapping import wizard's MVU and projection (001 §K; spec 0050 S9.2).
/// Nothing is attached or saved until Attach hands the kit window its choice.
module OpticalConstructor.Ui.ImportWizardView

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Storage.LabFileImport
open OpticalConstructor.Ui.MaterialsWindowView

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

/// Every role a column's role choice lists, in its order; one scan item stands for every unit and reading
/// (spec 0050 S9.2).
let private wizardRoleChoices : ColumnRoleChoice list =
    [
        UnmappedColumnRole
        ScanColumnRole (DegreeColumnUnit, PhysicalScanValue)
        SignalColumnRole
        SigmaColumnRole
        TimestampColumnRole
        CaptureIndexColumnRole
        ObservableColumnRole
        KeyColumnRole
        ThroughputColumnRole
    ]
    @ (MuellerElementIndex.all |> List.map MuellerElementRole)

/// The wizard over `m`: the file, its layout and mark, every profile, the preview with a role per column under a new
/// mapping, the refusal, and Attach beside Cancel (spec 0050 S9.2; .spec-md §10.5).
let importWizardView (m : ImportWizardModel) (dispatch : ImportWizardMsg -> unit) : IView =
    // The chosen box of a group is filled. `actionButton` keeps its first press handler, safe as a box's message never changes.
    let choiceBox (chosen : ConstructorUiId) (id : ConstructorUiId) (label : ControlCaption) (msg : ImportWizardMsg) : IView =
        if id = chosen then actionButton id label positiveBackground (fun () -> dispatch msg)
        else verbButton id label (fun () -> dispatch msg)

    // A label docked beside its wrapping boxes (AGENTS.md: never a WrapPanel in a horizontal StackPanel).
    let labelled (label : ControlCaption) (children : IView list) : IView =
        DockPanel.create [
            DockPanel.margin (Thickness(0.0, 0.0, 0.0, 6.0))
            DockPanel.children [
                TextBlock.create [
                    TextBlock.dock Dock.Left
                    TextBlock.minWidth 60.0
                    TextBlock.margin (Thickness(0.0, 5.0, 6.0, 0.0))
                    TextBlock.verticalAlignment VerticalAlignment.Top
                    TextBlock.text label.value
                ]
                WrapPanel.create [
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.children children
                ]
            ]
        ] :> IView

    // Resubscribed on every patch, so a unit or reading pick keeps the column's current other half.
    let choiceList (id : ConstructorUiId) (captions : ControlCaption list) (shown : int) (msgAt : int -> ImportWizardMsg option) : IView =
        ComboBox.create [
            id.automationAttribute<ComboBox> ()
            ComboBox.margin (Thickness(0.0, 0.0, 12.0, 4.0))
            ComboBox.dataItems (captions |> List.map _.value)
            ComboBox.selectedIndex shown
            ComboBox.onSelectedIndexChanged ((fun picked -> msgAt picked |> Option.iter dispatch), SubPatchOptions.Always)
        ]
        |> View.withKey id.value
        :> IView

    let roleCell (choice : ColumnChoice) : IView =
        let position = choice.column.uiPosition
        let shownRole = wizardRoleChoices |> List.tryFindIndex (fun role -> role.caption = choice.role.caption) |> Option.defaultValue -1

        let roleList =
            choiceList
                (ConstructorUiId.importWizardColumnRoleChoice position)
                (wizardRoleChoices |> List.map _.caption)
                shownRole
                (fun picked -> List.tryItem picked wizardRoleChoices |> Option.map (fun role -> SetColumnRole (choice.column, role)))

        let scanLists : IView list =
            match choice.role with
            | ScanColumnRole (valueUnit, reading) ->
                [
                    choiceList
                        (ConstructorUiId.importWizardColumnUnitChoice position)
                        (ScanColumnUnit.all |> List.map _.caption)
                        (ScanColumnUnit.all |> List.findIndex (fun unit -> unit = valueUnit))
                        (fun picked ->
                            List.tryItem picked ScanColumnUnit.all
                            |> Option.map (fun unit -> SetColumnRole (choice.column, ScanColumnRole (unit, reading))))
                    choiceList
                        (ConstructorUiId.importWizardColumnReadingChoice position)
                        (ScanReading.all |> List.map _.caption)
                        (ScanReading.all |> List.findIndex (fun held -> held = reading))
                        (fun picked ->
                            List.tryItem picked ScanReading.all
                            |> Option.map (fun picks -> SetColumnRole (choice.column, ScanColumnRole (valueUnit, picks))))
                ]
            | UnmappedColumnRole
            | SignalColumnRole
            | SigmaColumnRole
            | TimestampColumnRole
            | CaptureIndexColumnRole
            | ObservableColumnRole
            | KeyColumnRole
            | ThroughputColumnRole
            | MuellerElementRole _ -> []

        StackPanel.create [
            StackPanel.row 1
            StackPanel.column choice.column.index
            StackPanel.orientation Orientation.Vertical
            StackPanel.children (roleList :: scanLists)
        ]
        |> View.withKey $"ImportWizardColumnCell_{choice.column.value}"
        :> IView

    let cell (row : int) (weight : FontWeight) (column : int) (shown : PreviewCell) : IView =
        TextBlock.create [
            TextBlock.row row
            TextBlock.column column
            TextBlock.fontWeight weight
            TextBlock.verticalAlignment VerticalAlignment.Center
            TextBlock.margin (Thickness(0.0, 0.0, 12.0, 4.0))
            TextBlock.text shown.value
        ] :> IView

    let preview : IView =
        match m.preview with
        | Error error ->
            TextBlock.create [
                TextBlock.textWrapping TextWrapping.Wrap
                TextBlock.foreground (brush messageColor)
                TextBlock.text error.caption.value
            ] :> IView
        | Ok shown ->
            let width = shown.columnNames.Length

            let roleCells : IView list =
                match m.choice with
                | NewColumnMapping -> m.columns |> List.map roleCell
                | PickedImportProfile _ -> []

            let firstDataRow = if List.isEmpty roleCells then 1 else 2

            let dataCells =
                shown.rows
                |> List.mapi (fun index row -> row.cells |> List.truncate width |> List.mapi (cell (firstDataRow + index) FontWeight.Normal))
                |> List.concat

            Grid.create [
                ConstructorUiId.importWizardPreviewGrid.automationAttribute<Grid> ()
                Grid.columnDefinitions (List.replicate (max width 1) "Auto" |> String.concat ",")
                Grid.rowDefinitions (List.replicate (firstDataRow + shown.rows.Length) "Auto" |> String.concat ",")
                Grid.children ((shown.columnNames |> List.mapi (cell 0 FontWeight.SemiBold)) @ roleCells @ dataCells)
            ] :> IView

    let message : IView list =
        let captionOpt =
            match m.source, m.lastError with
            | Error error, _ -> Some (ControlCaption $"The file could not be read — {error.describe}")
            | Ok _, Some error -> Some error.caption
            | Ok _, None -> None

        match captionOpt with
        | Some caption ->
            [
                TextBlock.create [
                    ConstructorUiId.importWizardMessage.automationAttribute<TextBlock> ()
                    TextBlock.dock Dock.Bottom
                    TextBlock.margin (Thickness(0.0, 6.0, 0.0, 6.0))
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.foreground (brush messageColor)
                    TextBlock.text caption.value
                ]
                |> View.withKey ConstructorUiId.importWizardMessage.value
                :> IView
            ]
        | None -> []

    let attach : IView list =
        match m.source with
        | Ok _ -> [ actionButton ConstructorUiId.importWizardAttachButton (ControlCaption "Attach") positiveBackground (fun () -> dispatch ApplyImportWizard) ]
        | Error _ -> []

    let layoutBlock : IView =
        let chosenDelimiter = ConstructorUiId.importWizardDelimiterChoice m.delimiter.uiKey
        let chosenHeader = ConstructorUiId.importWizardHeaderChoice m.header.uiKey

        let delimiterBoxes =
            DataDelimiter.all
            |> List.map (fun delimiter ->
                choiceBox chosenDelimiter (ConstructorUiId.importWizardDelimiterChoice delimiter.uiKey) delimiter.caption (SetImportDelimiter delimiter))

        let headerBoxes =
            HeaderRowChoice.all
            |> List.map (fun header -> choiceBox chosenHeader (ConstructorUiId.importWizardHeaderChoice header.uiKey) header.caption (SetImportHeader header))

        let bomText =
            TextBlock.create [
                ConstructorUiId.importWizardBomText.automationAttribute<TextBlock> ()
                TextBlock.verticalAlignment VerticalAlignment.Center
                TextBlock.margin (Thickness(4.0, 0.0, 8.0, 4.0))
                TextBlock.text m.bom.caption.value
            ] :> IView

        labelled (ControlCaption "Layout:") (delimiterBoxes @ headerBoxes @ [ bomText ])

    let profileBlock : IView =
        let chosen =
            match m.choice with
            | PickedImportProfile name -> ConstructorUiId.importWizardProfileChoice name.uiKey
            | NewColumnMapping -> ConstructorUiId.importWizardNewMappingButton

        let names = (BuiltInImportProfile.all |> List.map _.name) @ (m.context.request.kitProfiles |> List.map _.name)

        let profileBoxes =
            names |> List.map (fun name -> choiceBox chosen (ConstructorUiId.importWizardProfileChoice name.uiKey) name.caption (PickImportProfile name))

        let newMapping = choiceBox chosen ConstructorUiId.importWizardNewMappingButton (ControlCaption "New column mapping") StartColumnMapping
        labelled (ControlCaption "Profile:") (profileBoxes @ [ newMapping ])

    let profileNameRow : IView list =
        match m.choice with
        | NewColumnMapping ->
            [
                DockPanel.create [
                    DockPanel.margin (Thickness(0.0, 0.0, 0.0, 6.0))
                    DockPanel.children [
                        TextBlock.create [
                            TextBlock.dock Dock.Left
                            TextBlock.verticalAlignment VerticalAlignment.Center
                            TextBlock.margin (Thickness(0.0, 0.0, 6.0, 0.0))
                            TextBlock.text "Profile name:"
                        ]
                        TextBox.create [
                            ConstructorUiId.importWizardProfileNameField.automationAttribute<TextBox> ()
                            TextBox.text m.profileName.value
                            TextBox.onTextChanged (Option.ofObj >> Option.map ControlCaption >> Option.iter (SetImportProfileName >> dispatch))
                        ]
                    ]
                ]
                :> IView
            ]
        | PickedImportProfile _ -> []

    let heading : IView =
        TextBlock.create [
            ConstructorUiId.importWizardHeading.automationAttribute<TextBlock> ()
            TextBlock.textWrapping TextWrapping.Wrap
            TextBlock.margin (Thickness(0.0, 0.0, 0.0, 12.0))
            TextBlock.text $"Import {m.context.request.file.leafName.value}\nNothing is attached until you press Attach."
        ]
        |> View.withKey ConstructorUiId.importWizardHeading.value
        :> IView

    let cancel = actionButton ConstructorUiId.importWizardCancelButton (ControlCaption "Cancel") negativeBackground (fun () -> dispatch CancelImportWizard)

    let actions : IView =
        StackPanel.create [
            StackPanel.dock Dock.Bottom
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children (attach @ [ cancel ])
        ] :> IView

    let choices : IView =
        StackPanel.create [
            StackPanel.dock Dock.Top
            StackPanel.orientation Orientation.Vertical
            StackPanel.children ([ heading; layoutBlock; profileBlock ] @ profileNameRow)
        ] :> IView

    let previewScroll : IView =
        ScrollViewer.create [
            ScrollViewer.horizontalScrollBarVisibility ScrollBarVisibility.Auto
            ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
            ScrollViewer.content preview
        ] :> IView

    DockPanel.create [
        DockPanel.margin 12.0
        DockPanel.children ([ actions ] @ message @ [ choices; previewScroll ])
    ] :> IView
