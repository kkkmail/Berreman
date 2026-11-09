/// The kit window: the kit bar above the workbench, showing the kit, its Kit menu, the constructor and its switch, the
/// reload offer and the kit status, every control under its `ConstructorUiId` (spec 0050 S5.3).
module OpticalConstructor.Ui.KitWindowView

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Controls
open OpticalConstructor.Domain.KitArchive
open OpticalConstructor.Ui.KitArchiveJob
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Ui.TableAndElementRotationView

type TableAndElementRotationModel with

    /// `Kit:` and the kit's caption, which wraps: it is the dock's fill, never a horizontal stack's child
    /// (spec 0050 S5.3; AGENTS.md).
    member internal model.kitCaptionRow : IView =
        DockPanel.create [
            DockPanel.children [
                TextBlock.create [
                    TextBlock.dock Dock.Left
                    TextBlock.fontWeight FontWeight.SemiBold
                    TextBlock.margin (Thickness(0.0, 0.0, 6.0, 0.0))
                    TextBlock.text "Kit:"
                ]
                TextBlock.create [
                    ConstructorUiId.kitCaptionText.automationAttribute<TextBlock> ()
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.text model.kitCaption.value
                ]
            ]
        ] :> IView

    /// The last kit read or write that needs telling, in the library windows' refusal colour; the one control
    /// carrying `kitStatus`, whichever bay is open (spec 0050 S5.3).
    member internal model.kitStatusRows : IView list =
        match model.kitStatus with
        | Some status ->
            [
                TextBlock.create [
                    ConstructorUiId.kitStatus.automationAttribute<TextBlock> ()
                    TextBlock.foreground (MaterialsWindowView.brush MaterialsWindowView.messageColor)
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.margin (Thickness(0.0, 2.0, 0.0, 2.0))
                    TextBlock.text status.value
                ]
                |> Avalonia.FuncUI.DSL.View.withKey ConstructorUiId.kitStatus.value
                :> IView
            ]
        | None -> []

/// The Kit menu as the bar's verb row, then the constructor and its switch (spec 0050 S5.3; 001 §O). A row, not an
/// Avalonia `Menu`: a popup cannot be proven headless, as the model-driven element menu shows.
let private kitMenuRow (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    // Spec 0050 S6: the save dialog suggests `<kit name>.zip`, and a cancelled dialog sends nothing. Hidden while a
    // send runs, one per window.
    let sendKit : IView list =
        match model.kitFolderOpt with
        | Some _ when not model.kitSend.job.isRunning ->
            [
                MaterialsWindowView.verbButton ConstructorUiId.sendKitButton (ControlCaption "Send kit…") (fun () ->
                    model.kitShell.zipPicker.pickZipToWriteThen model.kit.kitFile.name.zipFileName (SendKit >> dispatch))
            ]
        | Some _
        | None -> []
    let showInExplorer : IView list =
        match model.kitFolderOpt with
        | Some _ ->
            [ MaterialsWindowView.verbButton ConstructorUiId.kitShowInExplorerButton (ControlCaption "Show in Explorer") (fun () -> dispatch ShowKitInExplorer) ]
        | None -> []
    // Spec 0050 S6: offered while the kit takes writes and no source is open; a cancelled dialog sends nothing.
    let copyExperiments : IView list =
        match model.kitWrites, model.kitCopyOpt with
        | ControlOffered, None ->
            [
                MaterialsWindowView.verbButton ConstructorUiId.kitCopyExperimentsButton (ControlCaption "Copy experiments from another kit…") (fun () ->
                    model.kitShell.kitPicker.pickThen (OpenCopySource >> dispatch))
            ]
        | ControlOffered, Some _
        | ControlWithheld, _ -> []
    let modeText (caption : ControlCaption) : IView =
        TextBlock.create [
            ConstructorUiId.kitModeText.automationAttribute<TextBlock> ()
            TextBlock.verticalAlignment VerticalAlignment.Center
            TextBlock.margin (Thickness(8.0, 0.0, 8.0, 4.0))
            TextBlock.text caption.value
        ]
        |> Avalonia.FuncUI.DSL.View.withKey ConstructorUiId.kitModeText.value
        :> IView
    let switchButton (label : ControlCaption) (target : ConstructorMode) : IView =
        MaterialsWindowView.verbButton ConstructorUiId.kitSwitchModeButton label (fun () -> dispatch (SwitchConstructorMode target))
    let kitVerbs : IView list =
        [
            MaterialsWindowView.verbButton ConstructorUiId.kitReloadChangesButton (ControlCaption "Reload changes") (fun () -> dispatch (CheckKitChanges KitCheckOnRequest))
            MaterialsWindowView.verbButton ConstructorUiId.kitCloseButton (ControlCaption "Close kit") (fun () -> dispatch CloseKit)
        ]
    let constructorItems : IView list =
        match model.constructorMode with
        | ForwardConstructor -> [ modeText (ControlCaption "Main constructor"); switchButton (ControlCaption "Switch to Inverse") InverseConstructor ]
        | InverseConstructor -> [ modeText (ControlCaption "Inverse constructor"); switchButton (ControlCaption "Switch to Main") ForwardConstructor ]
    WrapPanel.create [
        WrapPanel.orientation Orientation.Horizontal
        WrapPanel.children (sendKit @ showInExplorer @ copyExperiments @ kitVerbs @ constructorItems)
    ] :> IView

/// The running send's progress row directly under the Kit menu, its Cancel ending the send before its next file
/// (spec 0050 S6; .spec-md §10.2). Nothing while no send runs.
let private kitSendRows (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    if model.kitSend.job.isRunning then
        let ids : KitArchiveProgressIds =
            {
                bar = ConstructorUiId.sendKitProgressBar
                text = ConstructorUiId.sendKitProgressText
                cancel = ConstructorUiId.sendKitCancelButton
            }
        [ kitArchiveProgressView model.kitSend ids (fun () -> dispatch CancelSendKit) ]
    else
        []

/// The open copy source: its name, a toggle per experiment in a list that scrolls inside its own height, because the
/// bar sits outside the page's scroll, then Copy and Cancel on one row, the Copy box's right margin parting the pair
/// (spec 0050 S6; .spec-md §10.5). Nothing while no source is open.
let private kitCopyRows (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    match model.kitCopyOpt with
    | Some copySource ->
        let toggle (experiment : OpticalConstructor.Domain.Experiments.Experiment) : IView =
            let id = ConstructorUiId.kitCopyExperimentToggle experiment.id
            CheckBox.create [
                id.automationAttribute<CheckBox> ()
                CheckBox.isChecked (List.contains experiment.id copySource.chosen)
                CheckBox.content (
                    TextBlock.create [
                        TextBlock.textWrapping TextWrapping.Wrap
                        TextBlock.text $"E{experiment.id.value} — {experiment.description}"
                    ])
                // `onClick` fires on the user's press alone, never on the render's `IsChecked` (the `pivotCheckBox` precedent).
                CheckBox.onClick ((fun _ -> dispatch (ToggleCopiedExperiment experiment.id)), SubPatchOptions.OnChangeOf (box id.value))
            ]
            |> Avalonia.FuncUI.DSL.View.withKey id.value
            :> IView
        let copy : IView list =
            match copySource.chosen with
            | [] -> []
            | _ :: _ ->
                [
                    MaterialsWindowView.actionButton
                        ConstructorUiId.kitCopyChosenButton
                        (ControlCaption "Copy")
                        MaterialsWindowView.positiveBackground
                        (fun () -> dispatch CopyChosenExperiments)
                ]
        let cancel : IView =
            MaterialsWindowView.actionButton
                ConstructorUiId.kitCopyCancelButton
                (ControlCaption "Cancel")
                MaterialsWindowView.negativeBackground
                (fun () -> dispatch CloseCopySource)
        [
            StackPanel.create [
                StackPanel.orientation Orientation.Vertical
                StackPanel.children [
                    TextBlock.create [
                        ConstructorUiId.kitCopySourceText.automationAttribute<TextBlock> ()
                        TextBlock.textWrapping TextWrapping.Wrap
                        TextBlock.margin (Thickness(0.0, 2.0, 0.0, 4.0))
                        TextBlock.text $"Copy from {copySource.source.kitFile.name.value}: choose the experiments to copy."
                    ]
                    ScrollViewer.create [
                        ConstructorUiId.kitCopyExperimentList.automationAttribute<ScrollViewer> ()
                        ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
                        ScrollViewer.maxHeight 160.0
                        ScrollViewer.margin (Thickness(0.0, 0.0, 0.0, 4.0))
                        ScrollViewer.content (
                            StackPanel.create [
                                StackPanel.orientation Orientation.Vertical
                                StackPanel.children (copySource.experiments |> List.map toggle)
                            ])
                    ]
                    StackPanel.create [
                        StackPanel.orientation Orientation.Horizontal
                        StackPanel.children (copy @ [ cancel ])
                    ]
                ]
            ]
            |> Avalonia.FuncUI.DSL.View.withKey ConstructorUiId.kitCopySourceText.value
            :> IView
        ]
    | None -> []

/// The reload offer: the files changed outside the app over Reload and Keep mine on one row, the Reload box's right
/// margin parting the opposing pair (spec 0050 S5.3; .spec-md §10.5). Nothing while nothing is offered.
let private kitChangesRows (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    match model.kitChanges with
    | [] -> []
    | files ->
        let names = files |> List.map (fun file -> file.value) |> String.concat ", "
        [
            StackPanel.create [
                StackPanel.orientation Orientation.Vertical
                StackPanel.children [
                    TextBlock.create [
                        ConstructorUiId.kitChangesText.automationAttribute<TextBlock> ()
                        TextBlock.textWrapping TextWrapping.Wrap
                        TextBlock.margin (Thickness(0.0, 2.0, 0.0, 4.0))
                        TextBlock.text $"Changed outside the app: {names}. Reload them, or keep your version?"
                    ]
                    StackPanel.create [
                        StackPanel.orientation Orientation.Horizontal
                        StackPanel.children [
                            MaterialsWindowView.actionButton
                                ConstructorUiId.kitChangesReloadButton
                                (ControlCaption "Reload")
                                MaterialsWindowView.positiveBackground
                                (fun () -> dispatch ReloadKitChanges)
                            MaterialsWindowView.actionButton
                                ConstructorUiId.kitChangesKeepMineButton
                                (ControlCaption "Keep mine")
                                MaterialsWindowView.negativeBackground
                                (fun () -> dispatch KeepMyKitChanges)
                        ]
                    ]
                ]
            ]
            |> Avalonia.FuncUI.DSL.View.withKey ConstructorUiId.kitChangesText.value
            :> IView
        ]

/// The kit window as a pure projection of the workbench model: the kit bar docked above `mainView`, so the bar stays
/// in view while the page scrolls (spec 0050 S5.3).
let kitWindowView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    DockPanel.create [
        DockPanel.children [
            StackPanel.create [
                StackPanel.dock Dock.Top
                ConstructorUiId.kitBar.automationAttribute<StackPanel> ()
                StackPanel.orientation Orientation.Vertical
                StackPanel.margin (Thickness(8.0, 6.0, 8.0, 2.0))
                StackPanel.children (
                    [ model.kitCaptionRow; kitMenuRow model dispatch ]
                    @ kitSendRows model dispatch
                    @ kitCopyRows model dispatch
                    @ kitChangesRows model dispatch
                    @ model.kitStatusRows)
            ]
            mainView model dispatch
        ]
    ] :> IView
