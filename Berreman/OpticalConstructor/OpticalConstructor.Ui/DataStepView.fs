/// The Inverse constructor's Data step: each experiment's data file, then the split of one lab file into many (001 §O
/// "the Experiments bay's Data files section moves into Data"; spec 0050 S9.3).
module OpticalConstructor.Ui.DataStepView

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.DataSplits
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

/// One experiment's data-file row: its description, the Attach verb + the currently-attached file
/// name, and its data badge (spec 0050 S9.1).
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
    // `actionButton` has no availability; `MatchByTemplate` guards the kit's writes itself (step 320).
    let apply =
        Border.create [
            Border.isEnabled model.kitWrites.isEnabled
            Border.opacity (match model.kitWrites with ControlOffered -> 1.0 | ControlWithheld -> 0.4)
            Border.child (
                MaterialsWindowView.actionButton
                    ConstructorUiId.dataSplitApplyButton
                    (ControlCaption "Apply split")
                    MaterialsWindowView.positiveBackground
                    (fun () -> dispatch MatchByTemplate)
            )
        ]
        :> IView
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
    Border.create [
        Border.borderBrush (MaterialsWindowView.brush (Color.FromRgb(120uy, 120uy, 120uy)))
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Thickness 8.0)
        Border.margin (Thickness(0.0, 6.0, 0.0, 0.0))
        Border.horizontalAlignment HorizontalAlignment.Left
        Border.child (
            StackPanel.create [
                StackPanel.orientation Orientation.Vertical
                StackPanel.children ([ keyColumn ] @ (held.split.groups |> List.map (splitGroupRow model dispatch held)) @ [ actions ])
            ]
        )
    ]
    :> IView

/// The Data step's surface: the data files, one row per experiment, then Split a file by key…, which opens the split
/// wizard over a picked lab file, and under it the held split's panel; both verbs are withheld while the kit takes no
/// writes (spec 0050 S9.3).
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
    let splitFile (event : Input.PointerPressedEventArgs) : unit =
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
                )
            ])
    ]
    :> IView
