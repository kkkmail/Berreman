/// The Inverse constructor's Data step: each experiment's data file, the split of one lab file into many, then the review
/// of one experiment's points (001 §O "the Experiments bay's Data files section moves into Data"; spec 0050 S9.3).
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

/// The "Attach data file…" verb for one collected experiment — resolve the owner window from the
/// click's visual tree (the `selectorChooseButton` precedent) and open the picker, dispatching
/// `OpenImportWizard` on a confirmed selection (spec 0050 S9.2). Keyed + AutomationId'd by the experiment id.
/// Withheld, it is disabled and dimmed and opens no picker (spec 0050 S5.3).
let private attachDataFileButton (availability : ControlAvailability) (dispatch : TableAndElementRotationMsg -> unit) (experiment : Experiments.Experiment) : IView =
    let id = ConstructorUiId.attachDataFile experiment.id
    Border.create [
        id.automationAttribute<Border> ()
        Border.isEnabled availability.isEnabled
        Border.opacity (match availability with ControlOffered -> 1.0 | ControlWithheld -> 0.4)
        Border.background (MaterialsWindowView.brush (Color.FromRgb(232uy, 232uy, 232uy)))
        Border.borderBrush (MaterialsWindowView.brush (Color.FromRgb(120uy, 120uy, 120uy)))
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Thickness(10.0, 4.0))
        Border.margin (Thickness(0.0, 0.0, 8.0, 0.0))
        Border.verticalAlignment VerticalAlignment.Center
        Border.child (TextBlock.create [ TextBlock.text "Attach data file…" ])
        Border.onPointerPressed ((fun e ->
            e.Handled <- true
            match availability with
            | ControlWithheld -> ()
            | ControlOffered ->
                match e.Source with
                | :? Visual as source ->
                    match TopLevel.GetTopLevel source with
                    | :? Window as owner ->
                        pickDataFile owner (ControlCaption "Attach measured-data file") (fun path -> dispatch (OpenImportWizard (experiment.id, path)))
                    | _ -> ()
                | _ -> ()), SubPatchOptions.OnChangeOf (box (experiment.id.uiKey.value, availability)))
    ]
    |> Avalonia.FuncUI.DSL.View.withKey id.value
    :> IView

/// One experiment's data-file row: its description, the Attach and Review verbs + the currently-attached file
/// name, and its data badge (spec 0050 S9.1, S9.3).
let private dataFileRow (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) (experiment : Experiments.Experiment) : IView =
    let attachedText =
        match experiment.dataOpt with
        | Some data -> $"file: {data.attachment.path.value} ({data.profile.value})"
        | None -> "no file attached"
    let statusText = (model.dataBadgeOf experiment).caption.value
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.margin (Thickness(0.0, 0.0, 0.0, 6.0))
        StackPanel.children [
            TextBlock.create [ TextBlock.text experiment.description; TextBlock.textWrapping TextWrapping.Wrap; TextBlock.maxWidth 360.0 ]
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.spacing 6.0
                StackPanel.children [
                    attachDataFileButton model.kitWrites dispatch experiment
                    // Spec 0050 S9.3: the review writes nothing, so a read-only kit reviews too.
                    availableWorkbenchButton
                        ControlOffered
                        (ConstructorUiId.dataReviewButton experiment.id)
                        (ControlCaption "Review")
                        (fun _ -> dispatch (ReviewExperimentData experiment.id))
                    TextBlock.create [ TextBlock.text attachedText; TextBlock.foreground (MaterialsWindowView.brush (Color.FromRgb(120uy, 120uy, 120uy))); TextBlock.verticalAlignment VerticalAlignment.Center ]
                ]
            ]
            TextBlock.create [
                (ConstructorUiId.dataFileStatus experiment.id).automationAttribute<TextBlock> ()
                TextBlock.text statusText
                TextBlock.textWrapping TextWrapping.Wrap
                TextBlock.maxWidth 360.0
                TextBlock.foreground (MaterialsWindowView.brush (Color.FromRgb(90uy, 90uy, 90uy)))
            ]
        ]
    ]
    |> Avalonia.FuncUI.DSL.View.withKey ("DataFileRow_" + experiment.id.uiKey.value)
    :> IView

/// One group of the held split: its key and point count, then the experiment it matched, its offer's toggle (on the
/// positive fill while ticked, `(new setup)` when the kit lacks the setup) or why it matches nothing (spec 0050 S9.3).
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
            if List.contains group.key held.creating then MaterialsWindowView.actionButton id label MaterialsWindowView.positiveBackground toggle
            else MaterialsWindowView.verbButton id label toggle
        | GroupWithoutMatch problem ->
            TextBlock.create [
                TextBlock.text $"unmatched — {problem.caption.value}"
                TextBlock.textWrapping TextWrapping.Wrap
                TextBlock.maxWidth 360.0
                TextBlock.verticalAlignment VerticalAlignment.Center
                TextBlock.foreground (MaterialsWindowView.brush MaterialsWindowView.messageColor)
            ]
            :> IView
    let id = ConstructorUiId.dataSplitGroupRow group.key
    // The label docked beside its wrapping reason (AGENTS.md); no fill, so a toggle keeps its caption's width.
    DockPanel.create [
        id.automationAttribute<DockPanel> ()
        DockPanel.lastChildFill false
        DockPanel.margin (Thickness(0.0, 0.0, 0.0, 4.0))
        DockPanel.children [
            TextBlock.create [
                TextBlock.dock Dock.Left
                TextBlock.verticalAlignment VerticalAlignment.Center
                TextBlock.text $"{group.key.value} — {group.points.value} point(s) — "
            ]
            matching
        ]
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
        TextBlock.create [
            ConstructorUiId.dataSplitKeyColumn.automationAttribute<TextBlock> ()
            TextBlock.text $"Split {held.file.leafName.value} by '{held.key.value}' through '{held.profile.value}'"
            TextBlock.textWrapping TextWrapping.Wrap
            TextBlock.maxWidth 360.0
            TextBlock.fontWeight FontWeight.SemiBold
            TextBlock.margin (Thickness(0.0, 0.0, 0.0, 6.0))
        ]
        :> IView
    // `MatchByTemplate` guards the kit's writes itself (step 320).
    let apply =
        MaterialsWindowView.actionButton
            ConstructorUiId.dataSplitApplyButton
            (ControlCaption "Apply split")
            MaterialsWindowView.positiveBackground
            (fun () -> dispatch MatchByTemplate)
        |> availabilityBox model.kitWrites
    let cancel =
        MaterialsWindowView.actionButton
            ConstructorUiId.dataSplitCancelButton
            (ControlCaption "Cancel")
            MaterialsWindowView.negativeBackground
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

/// The live text of the field carrying `fieldId`, else the draft's: a verb box takes no focus, so a typed text is not yet
/// committed when the verb is pressed (the Create setup precedent, spec 0050 S4.3, S9.3).
let private typedText (event : PointerPressedEventArgs) (fieldId : ConstructorUiId) (draft : ControlCaption) : ControlCaption =
    match event.Source with
    | :? Visual as source ->
        match TopLevel.GetTopLevel source with
        | null -> draft
        | owner ->
            owner.GetVisualDescendants()
            |> Seq.tryPick (function
                | :? TextBox as field when AutomationProperties.GetAutomationId field = fieldId.value ->
                    let typed = ControlCaption (string field.Text)
                    Some typed
                | _ -> None)
            |> Option.defaultValue draft
    | _ -> draft

/// One field of the review: its label docked beside a text box showing `text`, which commits what is typed through
/// `toMsg` on Enter and on losing focus (spec 0050 S9.3).
let private reviewField
    (dispatch : TableAndElementRotationMsg -> unit)
    (fieldId : ConstructorUiId)
    (label : ControlCaption)
    (accessibleName : ControlCaption)
    (text : ControlCaption)
    (toMsg : ControlCaption -> TableAndElementRotationMsg)
    : IView =
    let commit (source : obj) : unit =
        match source with
        | :? TextBox as field ->
            let typed = ControlCaption (string field.Text)
            dispatch (toMsg typed)
        | _ -> ()
    DockPanel.create [
        DockPanel.margin (Thickness(0.0, 0.0, 0.0, 6.0))
        DockPanel.children [
            TextBlock.create [
                TextBlock.dock Dock.Left
                TextBlock.verticalAlignment VerticalAlignment.Center
                TextBlock.margin (Thickness(0.0, 0.0, 6.0, 0.0))
                TextBlock.text label.value
            ]
            TextBox.create [
                fieldId.automationAttribute<TextBox> ()
                AttrBuilder<TextBox>.CreateProperty<string>(AutomationProperties.NameProperty, accessibleName.value, ValueNone)
                TextBox.minWidth 220.0
                TextBox.text text.value
                TextBox.onKeyDown ((fun event -> if event.Key = Key.Enter then commit event.Source), SubPatchOptions.Always)
                TextBox.onLostFocus ((fun event -> commit event.Source), SubPatchOptions.Always)
            ]
        ]
    ]
    :> IView

/// One reviewed point's row: its number and caption, then Exclude, or why it is excluded and Include; both are disabled
/// while the kit takes no writes (spec 0050 S9.3; 001 §K; .spec-md §10.5).
let private reviewedPointRow
    (model : TableAndElementRotationModel)
    (dispatch : TableAndElementRotationMsg -> unit)
    (experimentId : Experiments.ExperimentId)
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
                availableWorkbenchButton
                    model.kitWrites
                    (ConstructorUiId.dataIncludePointButton point)
                    (ControlCaption "Include")
                    (fun _ -> dispatch (IncludePoint (experimentId, point)))
            ]
        | None ->
            let exclude (event : PointerPressedEventArgs) : unit =
                let reason = typedText event ConstructorUiId.dataExclusionReasonField model.dataDraft.exclusionReason
                dispatch (SetExclusionReason reason)
                dispatch (ExcludePoint (experimentId, point, reason))
            [ availableWorkbenchButton model.kitWrites (ConstructorUiId.dataExcludePointButton point) (ControlCaption "Exclude") exclude ]
    let id = ConstructorUiId.dataPointRow point
    StackPanel.create [
        id.automationAttribute<StackPanel> ()
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 8.0
        StackPanel.children (
            (TextBlock.create [ TextBlock.text $"#{point.value}  {caption.value}"; TextBlock.verticalAlignment VerticalAlignment.Center ] :> IView)
            :: verb
        )
    ]
    |> Avalonia.FuncUI.DSL.View.withKey id.value
    :> IView

/// The reviewed experiment's setup, the audit note, a re-point verb per other setup of the kit, disabled while the kit
/// takes no writes, and the trail of its corrections, oldest first. Only the experiment's file is corrected, never its
/// lab file (spec 0050 S9.3; 001 §K, §P `CorrectSetup`).
let private setupCorrectionBlock
    (model : TableAndElementRotationModel)
    (dispatch : TableAndElementRotationMsg -> unit)
    (experiment : Experiments.Experiment)
    : IView list =
    let setupLine =
        TextBlock.create [
            TextBlock.text $"Setup: {experiment.setup.describe}"
            TextBlock.textWrapping TextWrapping.Wrap
            TextBlock.maxWidth 360.0
            TextBlock.margin (Thickness(0.0, 8.0, 0.0, 4.0))
        ]
        :> IView
    let noteField =
        reviewField
            dispatch
            ConstructorUiId.dataCorrectionNoteField
            (ControlCaption "Audit note:")
            (ControlCaption "Correction audit note")
            model.dataDraft.correctionNote
            SetCorrectionNote
    // `verbButtonOn` re-patches its press on every render, so a setup re-recorded under its name re-points to the new revision.
    let repointTo (setup : Setups.Setup) : IView =
        let repoint (event : PointerPressedEventArgs) : unit =
            let note = typedText event ConstructorUiId.dataCorrectionNoteField model.dataDraft.correctionNote
            dispatch (SetCorrectionNote note)
            dispatch (CorrectSetup (experiment.id, setup.reference, note))
        let label = ControlCaption $"Re-point to {setup.reference.describe}"
        MaterialsWindowView.verbButtonOn (ConstructorUiId.dataCorrectSetupChoice setup.name) label repoint
    let choices =
        WrapPanel.create [
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.maxWidth 360.0
            WrapPanel.children (model.kitSetups |> List.filter (fun setup -> setup.reference <> experiment.setup) |> List.map repointTo)
        ]
        :> IView
        |> availabilityBox model.kitWrites
    let trail : IView list =
        match experiment.corrections with
        | [] -> []
        | corrections ->
            [
                StackPanel.create [
                    ConstructorUiId.dataCorrectionTrail.automationAttribute<StackPanel> ()
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.margin (Thickness(0.0, 4.0, 0.0, 0.0))
                    StackPanel.children (
                        corrections
                        |> List.map (fun correction ->
                            TextBlock.create [
                                TextBlock.text $"{correction.previous.describe} → {correction.corrected.describe}: {correction.note.value}"
                                TextBlock.textWrapping TextWrapping.Wrap
                                TextBlock.maxWidth 360.0
                            ]
                            :> IView)
                    )
                ]
                :> IView
            ]
    setupLine :: noteField :: choices :: trail

/// The reviewed experiment's points: its name and badge, the exclusion reason, the points chart with each excluded point
/// a ring, and a row per point, else why it has none; then its setup's correction. Keyed by the experiment, so its verbs
/// never act for another (spec 0050 S9.3; 001 §K, §O).
let private dataReviewPanel
    (model : TableAndElementRotationModel)
    (dispatch : TableAndElementRotationMsg -> unit)
    (experiment : Experiments.Experiment)
    (reviewed : ReviewedData)
    : IView =
    let heading =
        TextBlock.create [
            TextBlock.text $"Points of {experiment.name.value} — {(model.dataBadgeOf experiment).text}"
            TextBlock.textWrapping TextWrapping.Wrap
            TextBlock.maxWidth 360.0
            TextBlock.fontWeight FontWeight.SemiBold
            TextBlock.margin (Thickness(0.0, 0.0, 0.0, 6.0))
        ]
        :> IView
    let reasonField =
        reviewField
            dispatch
            ConstructorUiId.dataExclusionReasonField
            (ControlCaption "Reason:")
            (ControlCaption "Exclusion reason")
            model.dataDraft.exclusionReason
            SetExclusionReason
    let points : IView list =
        match reviewed.points with
        | Ok mapped ->
            let exclusionOf (point : MappedPointNumber) : PointExclusion option =
                experiment.dataOpt |> Option.bind (fun data -> data.tryFindExclusion point)
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
            ExperimentControls.pointsChartView ConstructorUiId.dataPointsChart plotted (ControlCaption "point") (ControlCaption "reading")
            :: (numbered |> List.map (fun (point, caption) -> reviewedPointRow model dispatch experiment.id (exclusionOf point) point caption))
        | Error problem ->
            [
                TextBlock.create [
                    TextBlock.text problem.caption.value
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.maxWidth 360.0
                    TextBlock.foreground (MaterialsWindowView.brush MaterialsWindowView.messageColor)
                ]
                :> IView
            ]
    Border.create (
        stepPanelFrame
        @ [
            ConstructorUiId.dataReviewPanel.automationAttribute<Border> ()
            Border.child (
                StackPanel.create [
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.spacing 2.0
                    StackPanel.children (heading :: reasonField :: points @ setupCorrectionBlock model dispatch experiment)
                ]
            )
        ]
    )
    |> Avalonia.FuncUI.DSL.View.withKey ("DataReviewPanel_" + experiment.id.uiKey.value)
    :> IView

/// The Data step's surface: the data files, one row per experiment, then Split a file by key…, which opens the split
/// wizard over a picked lab file, under it the held split's panel, then the reviewed experiment's points and setup
/// correction; the attach and split verbs are withheld while the kit takes no writes (spec 0050 S9.3).
let dataStepView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let dataFiles : IView =
        match model.experimentCollection.experiments with
        | [] ->
            TextBlock.create [
                TextBlock.text "(add experiments in the Experiments step, then attach one data file to each)"
                TextBlock.foreground (MaterialsWindowView.brush (Color.FromRgb(120uy, 120uy, 120uy)))
            ]
            :> IView
        | experiments ->
            StackPanel.create [
                StackPanel.name ConstructorUiId.experimentDataFiles.value
                StackPanel.orientation Orientation.Vertical
                StackPanel.children (experiments |> List.map (dataFileRow model dispatch))
            ]
            :> IView
    // A withheld box is disabled, so it takes no press; `OpenSplitWizard` guards the kit's writes itself (step 321).
    let splitFile (event : PointerPressedEventArgs) : unit =
        match event.Source with
        | :? Visual as source ->
            match TopLevel.GetTopLevel source with
            | :? Window as owner -> pickDataFile owner (ControlCaption "Split a lab file by key") (OpenSplitWizard >> dispatch)
            | _ -> ()
        | _ -> ()
    let splitPanel : IView list =
        match model.dataSplitOpt with
        | Some held -> [ dataSplitPanel model dispatch held ]
        | None -> []
    // An experiment removed since it was reviewed has nothing to show.
    let reviewPanel : IView list =
        match model.reviewedDataOpt with
        | Some reviewed ->
            match model.experimentCollection.experiments |> List.tryFind (fun held -> held.id = reviewed.experimentId) with
            | Some experiment -> [ dataReviewPanel model dispatch experiment reviewed ]
            | None -> []
        | None -> []
    ScrollViewer.create [
        ScrollViewer.horizontalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.content (
            StackPanel.create [
                ConstructorUiId.dataStepPanel.automationAttribute<StackPanel> ()
                StackPanel.orientation Orientation.Vertical
                StackPanel.margin (Thickness 8.0)
                StackPanel.children (
                    [
                        TextBlock.create [
                            TextBlock.text "Data files:"
                            TextBlock.fontWeight FontWeight.SemiBold
                            TextBlock.margin (Thickness(0.0, 0.0, 0.0, 2.0))
                        ]
                        :> IView
                        dataFiles
                        StackPanel.create [
                            StackPanel.orientation Orientation.Horizontal
                            StackPanel.margin (Thickness(0.0, 6.0, 0.0, 0.0))
                            StackPanel.children [
                                availableWorkbenchButton model.kitWrites ConstructorUiId.dataSplitFileButton (ControlCaption "Split a file by key…") splitFile
                            ]
                        ]
                    ]
                    @ splitPanel
                    @ reviewPanel
                )
            ])
    ]
    :> IView
