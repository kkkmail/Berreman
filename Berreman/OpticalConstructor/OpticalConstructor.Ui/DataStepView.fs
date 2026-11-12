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

/// The Data step's surface: the data files, one row per experiment, then Split a file by key…, which opens the split
/// wizard over a picked lab file; both verbs are withheld while the kit takes no writes (spec 0050 S9.3).
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
    ScrollViewer.create [
        ScrollViewer.horizontalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.content (
            StackPanel.create [
                ConstructorUiId.dataStepPanel.automationAttribute<StackPanel> ()
                StackPanel.orientation Orientation.Vertical
                StackPanel.margin (Thickness 8.0)
                StackPanel.children [
                    TextBlock.create [
                        TextBlock.text "Data files:"
                        TextBlock.fontWeight FontWeight.SemiBold
                        TextBlock.margin (Thickness(0.0, 0.0, 0.0, 2.0))
                    ]
                    dataFiles
                    StackPanel.create [
                        StackPanel.orientation Orientation.Horizontal
                        StackPanel.margin (Thickness(0.0, 6.0, 0.0, 0.0))
                        StackPanel.children [
                            availableWorkbenchButton model.kitWrites ConstructorUiId.dataSplitFileButton (ControlCaption "Split a file by key…") splitFile
                        ]
                    ]
                ]
            ])
    ]
    :> IView
