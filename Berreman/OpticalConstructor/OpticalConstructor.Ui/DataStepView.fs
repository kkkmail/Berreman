/// The Inverse constructor's Data page: the experiment matrix as a table, each experiment's data opening under its matrix
/// row, and the split of one lab file into many (001 §O "Experiment matrix view"; spec 0050 S9.3; 0051 `.manual/007`
/// Phase 7).
module OpticalConstructor.Ui.DataStepView

open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.VisualTree
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.DataSplits
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Ui.TableAndElementRotationView
open OpticalConstructor.Ui.KitPageFrame

/// The "Attach data file…" verb for one collected experiment — resolve the owner window from the
/// click's visual tree and open the picker, dispatching
/// `OpenImportWizard` on a confirmed selection (spec 0050 S9.2). Keyed + AutomationId'd by the experiment id.
/// Withheld, it is disabled and dimmed and opens no picker (spec 0050 S5.3).
let private attachDataFileButton (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) (experiment : Experiments.Experiment) : IView =
    CommandControls.commandButtonOn (ConstructorUiId.attachDataFile experiment.id) (ControlCaption "Attach data file…") NeutralCommand model.kitWrites
        (fun pressed ->
            match TopLevel.GetTopLevel pressed with
            | :? Window as owner -> pickDataFile owner model PickedLabFile (ControlCaption "Attach measured-data file") (fun path -> dispatch (OpenImportWizard (experiment.id, path)))
            | _ -> ())
        [ Button.padding (Thickness(10.0, 4.0)); Button.margin (Thickness(0.0, 0.0, 8.0, 0.0)) ]

/// One group of the held split: its key and point count, then the experiment it matched, its offer's toggle (on the
/// positive fill while ticked, `(new setup)` when the kit lacks the setup) or why it matches nothing (spec 0050 S9.3).
/// An unreadable name or setup also offers the kit's setups for manual recording (spec 0050 S12.1).
let private splitGroupRow
    (model : TableAndElementRotationModel)
    (dispatch : TableAndElementRotationMsg -> unit)
    (held : DataSplitState)
    (group : SplitGroup)
    : IView =
    let matching : IView =
        match group.matching with
        | ExperimentForGroup experiment ->
            TextBlock.create [ TextBlock.text $"→ {experiment.value}"; TextBlock.verticalAlignment VerticalAlignment.Center ] :> IView
        | SetupOfferForGroup offer ->
            let id = ConstructorUiId.dataSplitOfferToggle group.key
            // The setup Apply split records, as `splitWritesOver` decides it (step 320).
            let newSetup = if List.contains offer.setup model.kitSetups then "" else " (new setup)"
            let label = ControlCaption $"Create {offer.experiment.value} over '{offer.setup.name.value}'{newSetup}"
            let toggle () : unit = dispatch (ToggleSplitOffer group.key)
            CommandControls.choiceCommand id label (if List.contains group.key held.creating then ChosenChoice else UnchosenChoice) toggle
        | GroupWithoutMatch problem ->
            TextBlock.create [
                TextBlock.text $"unmatched — {problem.caption.value}"
                TextBlock.textWrapping TextWrapping.Wrap
                TextBlock.maxWidth 360.0
                TextBlock.verticalAlignment VerticalAlignment.Center
                TextBlock.foreground (MaterialsWindowView.brush MaterialsWindowView.messageColor)
            ]
            :> IView
    let setupChoice : IView list =
        match group.matching, model.kitSetups with
        | GroupWithoutMatch (SplitKeyNameErr _ | SplitKeySetupErr _), _ :: _ ->
            let choiceId = ConstructorUiId.dataSplitGroupSetupChoice group.key
            let accessibleName = ControlCaption $"Record {group.key.value} over setup"
            [
                TextBlock.create [
                    TextBlock.text "Record over:"
                    TextBlock.margin (Thickness(8.0, 0.0, 6.0, 0.0))
                    TextBlock.verticalAlignment VerticalAlignment.Center
                ]
                ComboBox.create [
                    choiceId.automationAttribute<ComboBox> ()
                    AttrBuilder<ComboBox>.CreateProperty<string>(AutomationProperties.NameProperty, accessibleName.value, ValueNone)
                    ComboBox.dataItems (model.kitSetups |> List.map (fun setup -> setup.name.value))
                    ComboBox.selectedIndex -1
                    ComboBox.verticalAlignment VerticalAlignment.Center
                    ComboBox.onSelectedIndexChanged ((fun picked ->
                        model.kitSetups
                        |> List.tryItem picked
                        |> Option.iter (fun setup -> dispatch (OfferSplitGroupOver (group.key, setup.name)))), SubPatchOptions.Always)
                ]
                |> Avalonia.FuncUI.DSL.View.withKey choiceId.value
            ]
        | _ -> []
    let id = ConstructorUiId.dataSplitGroupRow group.key
    // The label docked beside its wrapping reason (AGENTS.md); no fill, so a toggle keeps its caption's width.
    DockPanel.create [
        yield! id.groupAttributes<DockPanel> ()
        DockPanel.lastChildFill false
        DockPanel.margin (Thickness(0.0, 0.0, 0.0, 4.0))
        DockPanel.children ([
            TextBlock.create [
                TextBlock.dock Dock.Left
                TextBlock.verticalAlignment VerticalAlignment.Center
                // Spec 0051 `.manual/003` 5.4: one arrow leads from the group to what it matches.
                TextBlock.text $"{group.key.value}, {group.points.value} point(s) "
            ]
            matching
        ] @ setupChoice)
    ]
    |> Avalonia.FuncUI.DSL.View.withKey id.value
    :> IView

/// The frame of a Data step panel under the verbs (spec 0050 S9.3).
let private stepPanelFrame : IAttr<Border> list =
    [
        Border.borderBrush (MaterialsWindowView.brush (Color.FromRgb(120uy, 120uy, 120uy)))
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Thickness 8.0)
        Border.margin (Thickness(0.0, 6.0, 0.0, 0.0))
        Border.horizontalAlignment HorizontalAlignment.Left
    ]

/// The verbs in a box that is disabled and dimmed while withheld, for a verb look with no availability of its own
/// (spec 0050 S5.3, S9.3).
let private availabilityBox (availability : ControlAvailability) (verbs : IView) : IView =
    Border.create [
        Border.isEnabled availability.isEnabled
        Border.opacity (match availability with ControlOffered -> 1.0 | ControlWithheld -> 0.4)
        Border.child verbs
    ]
    :> IView

/// The held split: its file, key column and profile, one row per group in file order, then Apply split and, after a
/// gap, Cancel; Apply is disabled while the kit takes no writes (spec 0050 S9.3; .spec-md §10.3, §10.5).
let private dataSplitPanel (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) (held : DataSplitState) : IView =
    let keyColumn =
        SelectableTextBlock.create [
            ConstructorUiId.dataSplitKeyColumn.automationAttribute<SelectableTextBlock> ()
            TextBlock.text $"Split {held.file.leafName.value} by '{held.key.value}' through '{held.profile.value}'"
            TextBlock.textWrapping TextWrapping.Wrap
            TextBlock.maxWidth 360.0
            TextBlock.fontWeight FontWeight.SemiBold
            TextBlock.margin (Thickness(0.0, 0.0, 0.0, 6.0))
        ]
        :> IView
    // `MatchByTemplate` guards the kit's writes itself (step 320).
    let apply =
        CommandControls.actionCommand
            ConstructorUiId.dataSplitApplyButton
            (ControlCaption "Apply split")
            PositiveCommand
            (fun () -> dispatch MatchByTemplate)
        |> availabilityBox model.kitWrites
    let cancel =
        CommandControls.actionCommand
            ConstructorUiId.dataSplitCancelButton
            (ControlCaption "Cancel")
            NegativeCommand
            (fun () -> dispatch CancelDataSplit)
    let actions =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.margin (Thickness(0.0, 4.0, 0.0, 0.0))
            StackPanel.children [ apply; cancel ]
        ]
        :> IView
    Border.create (
        stepPanelFrame
        @ [
            Border.child (
                StackPanel.create [
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.children ([ keyColumn ] @ (held.split.groups |> List.map (splitGroupRow model dispatch held)) @ [ actions ])
                ]
            )
        ]
    )
    :> IView

/// The exclusion reason as typed, for the next Exclude in the edit (spec 0050 S9.3).
let private exclusionReasonField (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    DockPanel.create [
        DockPanel.margin (Thickness(0.0, 0.0, 0.0, 6.0))
        DockPanel.children [
            TextBlock.create [
                TextBlock.dock Dock.Left
                TextBlock.verticalAlignment VerticalAlignment.Center
                TextBlock.margin (Thickness(0.0, 0.0, 6.0, 0.0))
                TextBlock.text "Reason:"
            ]
            TextBox.create [
                ConstructorUiId.dataExclusionReasonField.automationAttribute<TextBox> ()
                AttrBuilder<TextBox>.CreateProperty<string>(AutomationProperties.NameProperty, "Exclusion reason", ValueNone)
                TextBox.minWidth 220.0
                TextBox.text model.dataDraft.exclusionReason.value
                TextBox.onTextChanged (Option.ofObj >> Option.map ControlCaption >> Option.iter (SetExclusionReason >> dispatch))
            ]
        ]
    ]
    :> IView

/// One point's row in the edit: its number and caption, then Exclude, or why it is excluded and Include, each changing
/// the draft (spec 0050 S9.3; 0051 `.manual/007` 7.3).
let private editedPointRow
    (dispatch : TableAndElementRotationMsg -> unit)
    (exclusionOpt : PointExclusion option)
    (point : MappedPointNumber)
    (caption : ControlCaption)
    : IView =
    let verb : IView list =
        match exclusionOpt with
        | Some exclusion ->
            [
                TextBlock.create [
                    TextBlock.text $"excluded — {exclusion.reason.value}"
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    TextBlock.foreground (MaterialsWindowView.brush MaterialsWindowView.messageColor)
                ]
                :> IView
                CommandControls.verbCommand (ConstructorUiId.dataIncludePointButton point) (ControlCaption "Include") (fun () -> dispatch (DraftIncludePoint point))
            ]
        | None -> [ CommandControls.verbCommand (ConstructorUiId.dataExcludePointButton point) (ControlCaption "Exclude") (fun () -> dispatch (DraftExcludePoint point)) ]
    let id = ConstructorUiId.dataPointRow point
    // Spec 0051 `.manual/003` 4.3: the row wraps at the page's width, so a long reason never leaves the window.
    WrapPanel.create [
        yield! id.groupAttributes<WrapPanel> ()
        WrapPanel.orientation Orientation.Horizontal
        WrapPanel.itemSpacing 8.0
        WrapPanel.children ((PageControls.copyableLine (ControlCaption $"#{point.value}  {caption.value}") [ SelectableTextBlock.verticalAlignment VerticalAlignment.Center ]) :: verb)
    ]
    |> Avalonia.FuncUI.DSL.View.withKey id.value
    :> IView

/// The chosen experiment's data under its matrix row: its file, profile and badge, View chart ↗ and Edit; its points
/// chart, each excluded point a ring; in Editing, Attach data file…, the reason and a row per point with Exclude or
/// Include over the draft, then Save and Cancel. Nothing here names a setup (spec 0050 S9.3; 0051 `.manual/007` 7.1, 7.3).
let private dataPanel (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) (experiment : Experiments.Experiment) : IView =
    let editing =
        match model.pageMode, model.dataEditOpt with
        | EditingPageEntry, Some edit when edit.experimentId = experiment.id -> Some edit
        | _ -> None
    let exclusions =
        match editing, experiment.dataOpt with
        | Some edit, _ -> edit.exclusions
        | None, Some data -> data.exclusions
        | None, None -> []
    let exclusionOf (point : MappedPointNumber) : PointExclusion option = exclusions |> List.tryFind (fun held -> held.point = point)
    let fileLine =
        match experiment.dataOpt with
        | Some data -> ControlCaption $"File: {data.attachment.path.value}, read through {data.profile.value}"
        | None -> ControlCaption "No data file is attached."
    let view : IView list =
        match model.chartAvailabilityOf experiment with
        | ChartReady -> [ CommandControls.verbCommand (ConstructorUiId.experimentViewButton experiment.id.uiKey) (ControlCaption "View chart ↗") (fun () -> dispatch (ViewExperiment experiment.id)) ]
        | ChartWithheldFor reason ->
            [
                CommandControls.commandButton (ConstructorUiId.experimentViewButton experiment.id.uiKey) (ControlCaption "View chart ↗") NeutralCommand ControlWithheld ignore
                    [ Button.margin (Thickness(0.0, 0.0, 8.0, 4.0)) ]
                PageControls.copyableText ConstructorUiId.experimentChartReason (ControlCaption $"No chart: {reason.value}") [ SelectableTextBlock.verticalAlignment VerticalAlignment.Center ]
            ]
    let verbs : IView list =
        match editing, model.kitWrites, model.pageMode with
        | Some _, _, _ -> [ attachDataFileButton model dispatch experiment ]
        | None, ControlOffered, BrowsingPage -> view @ [ CommandControls.verbCommand ConstructorUiId.pageEditButton (ControlCaption "Edit") (fun () -> dispatch BeginPageEdit) ]
        | None, _, _ -> view
    let points : IView list =
        match model.reviewedDataOpt with
        | Some reviewed when reviewed.experimentId = experiment.id ->
            match reviewed.points with
            | Ok mapped ->
                let numbered = mapped.pointCaptions |> List.mapi (fun index caption -> MappedPointNumber (index + 1), caption)
                let plotted =
                    List.zip numbered mapped.chartPoints
                    |> List.map (fun ((point, _), chartPoint) ->
                        let plottedPoint : ExperimentControls.PlottedPoint =
                            {
                                position = point.uiPosition
                                point = chartPoint
                                mark =
                                    match exclusionOf point with
                                    | Some _ -> ExperimentControls.ExcludedPointMark
                                    | None -> ExperimentControls.KeptPointMark
                            }
                        plottedPoint)
                let chart = ExperimentControls.pointsChartView ConstructorUiId.dataPointsChart plotted (ControlCaption "point") (ControlCaption "reading")
                match editing with
                | Some _ -> chart :: exclusionReasonField model dispatch :: (numbered |> List.map (fun (point, caption) -> editedPointRow dispatch (exclusionOf point) point caption))
                | None ->
                    let excluded =
                        match exclusions with
                        | [] -> ControlCaption $"{numbered.Length} point(s), none excluded."
                        | held ->
                            let listed = held |> List.map (fun exclusion -> $"#{exclusion.point.value} ({exclusion.reason.value})") |> String.concat ", "
                            ControlCaption $"{numbered.Length} point(s), {held.Length} excluded: {listed}"
                    [ chart; PageControls.copyableLine excluded [] ]
            | Error problem ->
                [ PageControls.copyableLine problem.caption [ SelectableTextBlock.foreground (MaterialsWindowView.brush MaterialsWindowView.messageColor) ] ]
        | Some _
        | None -> []
    let actions : IView list =
        match editing with
        | Some _ -> [ model.draftActions (ControlCaption "Save") dispatch ]
        | None -> []
    Border.create (
        stepPanelFrame
        @ [
            yield! ConstructorUiId.dataReviewPanel.groupAttributes<Border> ()
            Border.child (
                StackPanel.create [
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.spacing 2.0
                    StackPanel.children (
                        [
                            PageControls.copyableText (ConstructorUiId.dataFileHeading experiment.id) (ControlCaption $"{experiment.name.value} [{experiment.role.text}]") [
                                SelectableTextBlock.fontWeight FontWeight.SemiBold
                            ]
                            PageControls.copyableText (ConstructorUiId.dataFileStatus experiment.id) ((model.dataBadgeOf experiment).caption) []
                            PageControls.copyableLine fileLine []
                            WrapPanel.create [ WrapPanel.orientation Orientation.Horizontal; WrapPanel.children verbs ] :> IView
                        ]
                        @ points
                        @ actions)
                ])
        ]
    )
    |> Avalonia.FuncUI.DSL.View.withKey $"DataPanel_{experiment.id.uiKey.value}_{model.pageMode.uiKey.value}"
    :> IView

/// An experiment's dynamic button in the matrix: its name and badge, chosen while its data is open; one with no data
/// has its own fill and begins with "⚠ " (spec 0050 S9.1; 0051 `.manual/007` 7.3, 7.4).
let private matrixEntry (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) (experiment : Experiments.Experiment) : IView =
    let id = ConstructorUiId.experimentMatrixEntry experiment.id
    let look, prefix =
        match experiment.dataOpt with
        | Some _ -> NeutralCommand, ""
        | None -> AttentionCommand, "⚠ "
    let label = ControlCaption $"{prefix}{experiment.name.value} · {(model.dataBadgeOf experiment).text}"
    let mark =
        match model.reviewedDataOpt with
        | Some reviewed when reviewed.experimentId = experiment.id -> ChosenChoice
        | Some _
        | None -> UnchosenChoice
    let availability =
        match model.pageMode with
        | BrowsingPage -> ControlOffered
        | EditingPageEntry
        | CreatingPageEntry -> ControlWithheld
    let menu = model.entryMenuOf experiment.name.caption (Some (RenameExperimentTarget experiment.id)) RenameAvailable dispatch
    match model.renameRowsOf (RenameExperimentTarget experiment.id) dispatch with
    | Some rows -> StackPanel.create [ StackPanel.children rows ] :> IView
    | None -> PageControls.entryToggle id label mark look availability menu (fun () -> dispatch (ChooseDataExperiment experiment.id)) []

/// The experiment matrix as a table with gridlines: one column per Prep slot and per object slot (the item at its
/// angle), then one per analyzer combination holding the experiments measured there; the chosen experiment's data opens
/// in a row right under its matrix row; the experiments it cannot place follow it (spec 0050 S9.3; 0051 `.manual/007`
/// 7.2, 7.3, D4).
let private experimentMatrixBlock (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    match model.experimentCollection.experiments with
    | [] -> [ PageControls.copyableLine (ControlCaption "This kit has no experiment yet: make them on the Experiments page.") [] ]
    | experiments ->
        let matrix = ExperimentMatrices.ExperimentMatrix.ofExperiments experiments model.kitSetups model.kitItems
        let experimentsOf (ids : Experiments.ExperimentId list) : Experiments.Experiment list =
            ids |> List.choose (fun id -> experiments |> List.tryFind (fun experiment -> experiment.id = id))
        let chosenOpt =
            model.reviewedDataOpt |> Option.bind (fun reviewed -> experiments |> List.tryFind (fun experiment -> experiment.id = reviewed.experimentId))
        let prepColumns = matrix.rows |> List.map (fun row -> row.sources.Length) |> List.fold max 0
        let objectColumns = matrix.rows |> List.map (fun row -> row.objects.Length) |> List.fold max 0
        let analyzerCaption (column : ExperimentMatrices.MatrixColumnKey) : ControlCaption =
            match column.analyzers with
            | [] -> ControlCaption "No analyzer"
            | analyzers -> ControlCaption ("Analyzer: " + (analyzers |> List.map (fun label -> label.value) |> String.concat " · "))
        let header =
            (List.init prepColumns (fun index -> ControlCaption $"Prep {index + 1}"))
            @ (List.init objectColumns (fun index -> ControlCaption $"Obj {index + 1}"))
            @ (matrix.columns |> List.map analyzerCaption)
        let padded (cells : ControlCaption list) (count : int) : GridCell list =
            (cells |> List.map TextGridCell) @ List.replicate (count - cells.Length) (TextGridCell (ControlCaption "—"))
        let rows =
            matrix.rows
            |> List.collect (fun row ->
                let sources = padded (row.sources |> List.map (fun label -> label.caption)) prepColumns
                let objects = padded (row.objects |> List.map (fun held -> ControlCaption $"{held.item.value} @ {held.angle.turnCaption.value}")) objectColumns
                let held =
                    matrix.columns
                    |> List.map (fun column ->
                        match matrix.tryFindCell row column with
                        | Some cell ->
                            let inCell = experimentsOf cell.experiments
                            ViewGridCell (
                                ControlCaption (inCell |> List.map (fun experiment -> experiment.name.value) |> String.concat " | "),
                                StackPanel.create [
                                    StackPanel.orientation Orientation.Vertical
                                    StackPanel.children (inCell |> List.map (matrixEntry model dispatch))
                                ])
                        | None -> TextGridCell (ControlCaption ""))
                let panel =
                    match chosenOpt, matrix.cells |> List.tryFind (fun cell -> cell.row = row && chosenOpt |> Option.exists (fun chosen -> List.contains chosen.id cell.experiments)) with
                    | Some chosen, Some _ -> [ SpanningGridRow (dataPanel model dispatch chosen) ]
                    | _ -> []
                CellsGridRow (sources @ objects @ held) :: panel)
        let table : GridTable =
            {
                id = ConstructorUiId.experimentMatrix
                header = header
                rows = rows
            }
        let unplaced : IView list =
            match experimentsOf matrix.unplaced with
            | [] -> []
            | missing ->
                PageControls.copyableLine (ControlCaption "Not in the matrix, as the kit lacks their setup:") [ SelectableTextBlock.margin (Thickness(0.0, 6.0, 0.0, 2.0)) ]
                :: (missing
                    |> List.collect (fun experiment ->
                        matrixEntry model dispatch experiment
                        :: (match chosenOpt with
                            | Some chosen when chosen.id = experiment.id -> [ dataPanel model dispatch chosen ]
                            | Some _
                            | None -> [])))
        [
            PageControls.copyableLine
                (ControlCaption "Rows: what prepares the light (the Prep slots) and what it crosses (each object at its angle). Columns: what analyses it. A cell lists the experiments measured there; press one to open its data.")
                [ SelectableTextBlock.margin (Thickness(0.0, 0.0, 0.0, 4.0)) ]
            PageControls.gridTableView table (fun text -> dispatch (CopyToClipboard text))
        ]
        @ unplaced

/// The Data page: the matrix, the chosen experiment's data under its row, and Split a file by key…, whose held split is
/// the page's Creating (spec 0050 S9.3; 0051 `.manual/007` Phase 7).
let dataStepView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    // A withheld box is disabled, so it takes no press; `OpenSplitWizard` guards the kit's writes itself (step 321).
    let splitFile (pressed : CommandButton) : unit =
        match TopLevel.GetTopLevel pressed with
        | :? Window as owner -> pickDataFile owner model PickedLabFile (ControlCaption "Split a lab file by key") (OpenSplitWizard >> dispatch)
        | _ -> ()
    let body : IView list =
        match model.pageMode, model.dataSplitOpt with
        | CreatingPageEntry, Some held -> [ dataSplitPanel model dispatch held ]
        | _ ->
            let split : IView list =
                match model.pageMode with
                | BrowsingPage ->
                    [
                        Border.create [
                            Border.horizontalAlignment HorizontalAlignment.Left
                            Border.margin (Thickness(0.0, 0.0, 0.0, 6.0))
                            Border.child (availableWorkbenchButton model.kitWrites ConstructorUiId.dataSplitFileButton (ControlCaption "Split a file by key…") splitFile)
                        ]
                        :> IView
                    ]
                | EditingPageEntry
                | CreatingPageEntry -> []
            split @ experimentMatrixBlock model dispatch
    pageScroll ConstructorUiId.dataStepPanel (model.pageHeader dispatch @ body)
