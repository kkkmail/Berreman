/// The frame every kit page shares: its header with its help and its own status, the discard question, Save and Cancel,
/// the in-place rename and the dynamic buttons' menus (spec 0051 `.manual/007` Phase 1).
module OpticalConstructor.Ui.KitPageFrame

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Controls
open OpticalConstructor.Ui.TableAndElementRotationView
open OpticalConstructor.Ui.PageHelpTexts

type TableAndElementRotationModel with

    /// The page's status line: the last kit status, shown only on the page it was raised on, a refusal in firebrick
    /// (spec 0050 S5.3; 0051 `.manual/003` 5.1, `.manual/007` 1.7).
    member model.pageStatusRows : IView list =
        match model.kitStatus with
        | Some status when model.kitStatusStep = model.flowStep ->
            let colour =
                match status with
                | KitStatusDone _ -> Color.FromRgb(40uy, 40uy, 40uy)
                | KitStatusRefused _ -> Color.FromRgb(178uy, 34uy, 34uy)
            [
                PageControls.copyableText ConstructorUiId.kitStatus status.caption [
                    SelectableTextBlock.foreground (SolidColorBrush colour)
                    SelectableTextBlock.margin (Thickness(0.0, 2.0, 0.0, 4.0))
                ]
            ]
        | Some _
        | None -> []

    /// "Discard your changes?" with Yes and No, while a leave waits for it (spec 0051 `.manual/007` 1.2, D11).
    member model.leaveQuestionRows (dispatch : TableAndElementRotationMsg -> unit) : IView list =
        match model.leaveQuestionOpt with
        | Some leave ->
            let question =
                match leave with
                | LeaveByCancel -> "Discard your changes?"
                | LeaveForStep step -> $"Discard your changes and go to {step.caption.value}?"
                | LeaveForMode ForwardConstructor -> "Discard your changes and switch to Main?"
                | LeaveForMode InverseConstructor -> "Discard your changes and switch to Inverse?"
                | LeaveByClosingKit -> "Discard your changes and close the kit?"
            [
                Border.create [
                    Border.background (SolidColorBrush(Color.FromRgb(255uy, 244uy, 214uy)))
                    Border.borderBrush (SolidColorBrush(Color.FromRgb(200uy, 160uy, 60uy)))
                    Border.borderThickness (Thickness 1.0)
                    Border.padding (Thickness(8.0, 4.0))
                    Border.margin (Thickness(0.0, 2.0, 0.0, 6.0))
                    Border.child (
                        WrapPanel.create [
                            WrapPanel.orientation Orientation.Horizontal
                            WrapPanel.children [
                                PageControls.copyableText ConstructorUiId.pageLeaveQuestionText (ControlCaption question) [
                                    SelectableTextBlock.verticalAlignment VerticalAlignment.Center
                                    SelectableTextBlock.margin (Thickness(0.0, 0.0, 10.0, 4.0))
                                ]
                                CommandControls.actionCommand ConstructorUiId.pageLeaveYesButton (ControlCaption "Yes, discard") NegativeCommand (fun () ->
                                    dispatch ConfirmPageLeave)
                                CommandControls.actionCommand ConstructorUiId.pageLeaveNoButton (ControlCaption "No, keep editing") NeutralCommand (fun () ->
                                    dispatch KeepPageDraft)
                            ]
                        ])
                ]
                |> View.withKey ConstructorUiId.pageLeaveQuestionText.value
                :> IView
            ]
        | None -> []

    /// The page's header: its heading and its `?` help, then its own status and the discard question (spec 0051
    /// `.manual/007` 1.6, 1.7).
    member model.pageHeader (dispatch : TableAndElementRotationMsg -> unit) : IView list =
        let step = model.flowStep
        [
            TextBlock.create [
                TextBlock.fontWeight FontWeight.SemiBold
                TextBlock.fontSize 15.0
                TextBlock.margin (Thickness(0.0, 0.0, 0.0, 2.0))
                TextBlock.text step.caption.value
            ]
            :> IView
            PageControls.pageHelpView (ConstructorUiId.pageHelpPanel step.caption.uiKey) step.help
        ]
        @ model.pageStatusRows
        @ model.leaveQuestionRows dispatch

    /// Save (or Create) withheld until the draft changed, then Cancel, which asks first when it did (spec 0051
    /// `.manual/007` 1.2).
    member model.draftActions (commit : ControlCaption) (dispatch : TableAndElementRotationMsg -> unit) : IView =
        let availability =
            match model.pageDraftState, model.kitWrites with
            | DraftChanged, ControlOffered -> ControlOffered
            | DraftChanged, ControlWithheld
            | DraftUnchanged, _ -> ControlWithheld
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.margin (Thickness(0.0, 4.0, 0.0, 6.0))
            StackPanel.children [
                CommandControls.actionCommandWhen availability ConstructorUiId.pageSaveButton commit PositiveCommand (fun () -> dispatch CommitPageDraft)
                CommandControls.actionCommand ConstructorUiId.pageCancelButton (ControlCaption "Cancel") NegativeCommand (fun () -> dispatch CancelPageDraft)
            ]
        ]
        |> View.withKey $"DraftActions_{availability.isEnabled}"
        :> IView

    /// A dynamic button's menu: Copy name through the window's clipboard, then Rename… of `target` where the page
    /// browses and the kit takes writes (spec 0051 `.manual/007` 1.3).
    member model.entryMenuOf
        (caption : ControlCaption)
        (targetOpt : RenameTarget option)
        (availability : RenameAvailability)
        (dispatch : TableAndElementRotationMsg -> unit)
        : EntryMenu =
        let rename =
            match targetOpt, availability, model.pageMode, model.kitWrites with
            | None, _, _, _ -> RenameNotOffered
            | Some _, _, _, ControlWithheld -> RenameWithheld (ControlCaption "The kit is open read-only.")
            | Some _, RenameRefused reason, _, ControlOffered -> RenameWithheld reason
            | Some target, RenameAvailable, BrowsingPage, ControlOffered -> RenameOffered (fun () -> dispatch (BeginRename target))
            | Some _, RenameAvailable, (EditingPageEntry | CreatingPageEntry), ControlOffered ->
                RenameWithheld (ControlCaption "Finish the edit on this page first.")
        {
            copyName = fun () -> dispatch (CopyToClipboard caption)
            rename = rename
        }

    /// The name field in place of `target`'s caption while it is renamed, with Save withheld until the name is new and
    /// not blank, and Cancel (spec 0051 `.manual/007` 1.3); none while it is not renamed.
    member model.renameRowsOf (target : RenameTarget) (dispatch : TableAndElementRotationMsg -> unit) : IView list option =
        match model.renameOpt, model.renameCurrentName target with
        | Some rename, Some current when rename.target = target ->
            let availability =
                if rename.nameText.value.Trim() <> "" && rename.nameText <> current then ControlOffered else ControlWithheld
            Some [
                WrapPanel.create [
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.margin (Thickness(0.0, 0.0, 0.0, 4.0))
                    WrapPanel.children [
                        TextBox.create [
                            yield! ConstructorUiId.renameField.inputAttributes<TextBox> ()
                            TextBox.width 320.0
                            TextBox.margin (Thickness(0.0, 0.0, 8.0, 4.0))
                            TextBox.text rename.nameText.value
                            TextBox.onTextChanged (Option.ofObj >> Option.map ControlCaption >> Option.iter (SetRenameText >> dispatch))
                            TextBox.onKeyDown (fun event -> if event.Key = Key.Escape then dispatch CancelRename)
                        ]
                        CommandControls.actionCommandWhen availability ConstructorUiId.renameSaveButton (ControlCaption "Save name") PositiveCommand (fun () ->
                            dispatch CommitRename)
                        CommandControls.actionCommand ConstructorUiId.renameCancelButton (ControlCaption "Cancel") NegativeCommand (fun () ->
                            dispatch CancelRename)
                    ]
                ]
                |> View.withKey $"Rename_{current.value}"
                :> IView
            ]
        | _ -> None

/// The page's content in a scroller at the window's width, never narrower than 520 px (spec 0051 `.manual/003` 4.3),
/// grouped under `id`.
let pageScroll (id : ConstructorUiId) (content : IView list) : IView =
    ScrollViewer.create [
        AttrBuilder<ScrollViewer>.CreateProperty<bool>(ScrollViewer.AllowAutoHideProperty, false, ValueNone)
        ScrollViewer.horizontalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.content (
            MinWidthPanel.create [
                (PanelMinWidth 520.0).minWidthPanelAttribute ()
                MinWidthPanel.child (
                    StackPanel.create [
                        yield! id.groupAttributes<StackPanel> ()
                        StackPanel.orientation Orientation.Vertical
                        StackPanel.margin (Thickness 8.0)
                        StackPanel.children content
                    ])
            ])
    ]
    :> IView

/// A dynamic list's rows: each entry's row, then, under the chosen one, what the page shows for it (spec 0051
/// `.manual/007` 1.2).
let entryRows<'Entry> (entries : 'Entry list) (row : 'Entry -> IView) (under : 'Entry -> IView list) : IView list =
    entries |> List.collect (fun entry -> row entry :: under entry)

/// The block under a chosen entry, indented and framed so it reads as the entry's (spec 0051 `.manual/007` 1.2).
let underEntry (key : ConstructorUiKey) (children : IView list) : IView list =
    [
        Border.create [
            Border.margin (Thickness(16.0, 0.0, 0.0, 10.0))
            Border.padding (Thickness(10.0, 6.0))
            Border.borderBrush (SolidColorBrush(Color.FromRgb(200uy, 200uy, 200uy)))
            Border.borderThickness (Thickness(2.0, 0.0, 0.0, 0.0))
            Border.child (
                StackPanel.create [
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.children children
                ])
        ]
        |> View.withKey key.value
        :> IView
    ]
