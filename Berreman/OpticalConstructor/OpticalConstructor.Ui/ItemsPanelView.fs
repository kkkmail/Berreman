/// The Items page: the kit's items as dynamic buttons, the chosen one open under its row, edited in place, and New item…
/// through the library's own window (spec 0050 S7.2; 001 §O; 0051 `.manual/007` Phase 4).
module OpticalConstructor.Ui.ItemsPanelView

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Ui.TableAndElementRotationView
open OpticalConstructor.Ui.KitPageFrame

type ControlCaption with

    /// A semi-bold heading of the panel or of an offer group (spec 0050 S7.2).
    member private this.itemsPanelHeading : IView =
        TextBlock.create [
            TextBlock.fontWeight FontWeight.SemiBold
            TextBlock.margin (Thickness(0.0, 6.0, 0.0, 2.0))
            TextBlock.text this.value
        ] :> IView

    /// A wrapping line of the panel (spec 0050 S7.2).
    member private this.itemsPanelLine : IView =
        TextBlock.create [
            TextBlock.textWrapping TextWrapping.Wrap
            TextBlock.margin (Thickness(0.0, 2.0, 0.0, 2.0))
            TextBlock.text this.value
        ] :> IView

/// A choice box: the chosen one an action box on the positive fill, the others verbs, the import dialog's chosen-option
/// look (spec 0050 S7.2, step 108).
let private itemsPanelChoice
    (chosenOpt : ConstructorUiId option)
    (id : ConstructorUiId)
    (caption : ControlCaption)
    (onChosen : unit -> unit)
    : IView =
    CommandControls.choiceCommand id caption (if chosenOpt = Some id then ChosenChoice else UnchosenChoice) onChosen

/// A box of a vertical list at its caption's width, so a long caption never stretches a short one past the window's
/// edge (spec 0050 S7.2).
let private itemsPanelListBox (box : IView) : IView =
    Border.create [
        Border.horizontalAlignment HorizontalAlignment.Left
        Border.child box
    ] :> IView

/// A labelled text box of the open form or editor, docked so its label never leaves the row (spec 0050 S7.2).
let private itemsPanelField
    (label : ControlCaption)
    (id : ConstructorUiId)
    (text : ControlCaption)
    (onChanged : ControlCaption -> unit)
    : IView =
    DockPanel.create [
        DockPanel.margin (Thickness(0.0, 2.0, 0.0, 2.0))
        DockPanel.children [
            TextBlock.create [
                TextBlock.dock Dock.Left
                TextBlock.verticalAlignment VerticalAlignment.Center
                TextBlock.margin (Thickness(0.0, 0.0, 6.0, 0.0))
                TextBlock.text label.value
            ]
            TextBox.create [
                yield! id.inputAttributes<TextBox> ()
                TextBox.width 220.0
                TextBox.horizontalAlignment HorizontalAlignment.Left
                TextBox.text text.value
                TextBox.onTextChanged (Option.ofObj >> Option.map ControlCaption >> Option.iter onChanged)
            ]
        ]
    ]
    |> View.withKey id.value
    :> IView

/// The shared item or material editor: Known or Unknown, its numbers and refusals, then Set through `setWith` and
/// Cancel on one row (spec 0050 S7.2, S11.5; .spec-md §10.5).
let parameterEditorView
    (model : TableAndElementRotationModel)
    (draft : ParameterEditDraft)
    (setWith : ParameterState -> TableAndElementRotationMsg)
    (dispatch : TableAndElementRotationMsg -> unit)
    : IView =
    let chosenOpt =
        match draft.choice with
        | KnownStateChoice -> Some ConstructorUiId.parameterEditKnownChoice
        | UnknownStateChoice -> Some ConstructorUiId.parameterEditUnknownChoice
    let field (label : ControlCaption) (id : ConstructorUiId) (editField : ParameterEditField) (text : ControlCaption) : IView =
        itemsPanelField label id text (fun typed -> dispatch (SetParameterEditText (editField, typed)))
    let numbers : IView list =
        match draft.choice with
        | KnownStateChoice -> [ field (ControlCaption "Value:") ConstructorUiId.parameterEditValueField ValueEditField draft.valueText ]
        | UnknownStateChoice ->
            [
                field (ControlCaption "Start:") ConstructorUiId.parameterEditValueField ValueEditField draft.valueText
                field (ControlCaption "Lower:") ConstructorUiId.parameterEditLowerField LowerEditField draft.lowerText
                field (ControlCaption "Upper:") ConstructorUiId.parameterEditUpperField UpperEditField draft.upperText
                field (ControlCaption "Prior σ (blank: none):") ConstructorUiId.parameterEditPriorSigmaField PriorSigmaEditField draft.priorSigmaText
            ]
    let refusal : IView list =
        match draft.tryState with
        | Ok _ -> []
        | Error error ->
            [
                TextBlock.create [
                    TextBlock.foreground (MaterialsWindowView.brush MaterialsWindowView.messageColor)
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.margin (Thickness(0.0, 2.0, 0.0, 2.0))
                    TextBlock.text error.describe
                ] :> IView
            ]
    let set : IView list =
        match draft.tryState, model.kitWrites with
        | Ok state, ControlOffered ->
            [
                CommandControls.actionCommand
                    ConstructorUiId.parameterEditSetButton
                    (ControlCaption "Set")
                    PositiveCommand
                    (fun () -> dispatch (setWith state))
            ]
        | Ok _, ControlWithheld
        | Error _, _ -> []
    let cancel =
        CommandControls.actionCommand
            ConstructorUiId.parameterEditCancelButton
            (ControlCaption "Cancel")
            NegativeCommand
            (fun () -> dispatch CancelParameterEdit)
    // `actionButton` subscribes its press once per key, so the row is keyed by the texts the state is read from.
    let actionsKey =
        let choice = chosenOpt |> Option.map (fun id -> id.value) |> Option.defaultValue ""
        $"ParameterEditActions_{draft.parameter.name.value}_{choice}_{draft.valueText.value}_{draft.lowerText.value}_{draft.upperText.value}_{draft.priorSigmaText.value}"
    StackPanel.create [
        yield! ConstructorUiId.parameterEditPanel.groupAttributes<StackPanel> ()
        StackPanel.orientation Orientation.Vertical
        StackPanel.margin (Thickness(0.0, 4.0, 0.0, 4.0))
        StackPanel.children (
            [
                (ControlCaption $"Edit {draft.parameter.name.value}").itemsPanelHeading
                WrapPanel.create [
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.children [
                        itemsPanelChoice chosenOpt ConstructorUiId.parameterEditKnownChoice (ControlCaption "Known") (fun () ->
                            dispatch (SetParameterEditChoice KnownStateChoice))
                        itemsPanelChoice chosenOpt ConstructorUiId.parameterEditUnknownChoice (ControlCaption "Unknown") (fun () ->
                            dispatch (SetParameterEditChoice UnknownStateChoice))
                    ]
                ]
            ]
            @ numbers
            @ refusal
            @ [
                StackPanel.create [
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.children (set @ [ cancel ])
                ]
                |> View.withKey actionsKey
            ])
    ]
    |> View.withKey $"{ConstructorUiId.parameterEditPanel.value}_{draft.parameter.name.value}"
    :> IView

/// The chosen item's parameters as a grid: in Editing each name is a button that opens the editor in a row right under
/// it; the editor's Set changes the draft (spec 0050 S7.2; 0051 `.manual/007` 4.2).
let private itemParameterGridView
    (model : TableAndElementRotationModel)
    (item : Item)
    (parameters : Parameter list)
    (dispatch : TableAndElementRotationMsg -> unit)
    : IView =
    let editing =
        match model.pageMode with
        | EditingPageEntry -> true
        | BrowsingPage
        | CreatingPageEntry -> false
    let rows =
        parameters
        |> List.collect (fun parameter ->
            let cells = parameter.gridCells
            let nameCell =
                if editing then
                    ViewGridCell (
                        parameter.name.caption,
                        CommandControls.verbCommand (ConstructorUiId.itemParameterRow parameter.name) parameter.name.caption (fun () ->
                            dispatch (EditItemParameter parameter.name)))
                else TextGridCell parameter.name.caption
            let row = CellsGridRow [ nameCell; TextGridCell cells.state; TextGridCell cells.value; TextGridCell cells.bounds; TextGridCell cells.sigma; TextGridCell cells.provenance ]
            let editorRows =
                match model.parameterEditOpt with
                | Some draft when editing ->
                    match draft.parameter with
                    | ItemParameterRef (id, name) when id = item.id && name = parameter.name ->
                        [ SpanningGridRow (parameterEditorView model draft (fun state -> SetItemDraftParameter (name, state)) dispatch) ]
                    | ItemParameterRef _
                    | MaterialParameterRef _ -> []
                | Some _
                | None -> []
            row :: editorRows)
    let table : GridTable =
        {
            id = ConstructorUiId.itemParameterGrid
            header =
                [
                    ControlCaption "Parameter"
                    ControlCaption "State"
                    ControlCaption "Value"
                    ControlCaption "Bounds"
                    ControlCaption "σ"
                    ControlCaption "Provenance"
                ]
            rows = rows
        }
    Border.create [ Border.child (PageControls.gridTableView table (fun text -> dispatch (CopyToClipboard text))) ]
    |> View.withKey $"{ConstructorUiId.itemParameterGrid.value}_{item.id.value}_{editing}"
    :> IView

/// The photo as a thumbnail at most 160 px wide, read at this edge; a missing file, or one that does not decode, shows
/// its name alone (spec 0051 `.manual/003` 5.5).
let private itemPhotoThumbnail (model : TableAndElementRotationModel) (item : Item) : IView list =
    match item.photoOpt, model.kitFolderOpt with
    | Some path, Some folder ->
        let file = System.IO.Path.Combine(folder.value.value, path.value)
        let bitmapOpt =
            if System.IO.File.Exists file then
                try Some (new Avalonia.Media.Imaging.Bitmap(file))
                with _ -> None
            else None
        match bitmapOpt with
        | Some bitmap ->
            [
                Image.create [
                    yield! ConstructorUiId.itemPhotoImage.figureAttributes<Image> ()
                    Image.source bitmap
                    Image.maxWidth 160.0
                    Image.maxHeight 120.0
                    Image.stretch Stretch.Uniform
                    Image.horizontalAlignment HorizontalAlignment.Left
                    Image.margin (Thickness(0.0, 2.0, 0.0, 4.0))
                ]
                |> View.withKey $"ItemPhoto_{path.value}"
                :> IView
            ]
        | None -> []
    | Some _, None
    | None, _ -> []

/// The chosen item's revisions, the current one first (spec 0050 S7.2).
let private itemRevisionsView (item : Item) : IView =
    let revisionLine (revision : ItemRevision) (caption : ControlCaption) : IView =
        let id = ConstructorUiId.itemRevisionRow revision
        PageControls.copyableText id caption [ SelectableTextBlock.margin (Thickness(0.0, 1.0, 0.0, 1.0)) ]
    StackPanel.create [
        yield! ConstructorUiId.itemRevisionList.groupAttributes<StackPanel> ()
        StackPanel.orientation Orientation.Vertical
        StackPanel.margin (Thickness(0.0, 4.0, 0.0, 4.0))
        StackPanel.children (
            (ControlCaption "Revisions:").itemsPanelHeading
            :: revisionLine item.revision (ControlCaption $"Revision {item.revision.value} (current)")
            :: (item.history |> List.rev |> List.map (fun record -> revisionLine record.revision record.caption)))
    ]
    :> IView

/// A labelled line of copyable text under the chosen item (spec 0051 `.manual/007` 1.4).
let private itemInfoLine (id : ConstructorUiId) (caption : ControlCaption) : IView =
    PageControls.copyableText id caption [ SelectableTextBlock.margin (Thickness(0.0, 2.0, 0.0, 2.0)) ]

/// The chosen item read-only: Edit on its first line, its model, dial, notes, photo, parameters and revisions (spec 0051
/// `.manual/007` 4.2).
let private itemDetailsRows (model : TableAndElementRotationModel) (item : Item) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    let edit : IView list =
        match model.kitWrites with
        | ControlOffered -> [ CommandControls.verbCommand ConstructorUiId.pageEditButton (ControlCaption "Edit") (fun () -> dispatch BeginPageEdit) ]
        | ControlWithheld -> []
    let photoText =
        match item.photoOpt with
        | Some path -> ControlCaption $"Photo: {path.value}"
        | None -> ControlCaption "No photo"
    let notes =
        match item.notes.value with
        | "" -> ControlCaption "Notes: (none)"
        | text -> ControlCaption $"Notes: {text}"
    edit
    @ [
        itemInfoLine ConstructorUiId.itemModelText (ControlCaption $"Model: {(model.itemModelCaption item).value}")
        itemInfoLine ConstructorUiId.itemDialText (ControlCaption $"Dial: {item.dial.caption.value}")
        itemInfoLine ConstructorUiId.itemNotesText notes
        itemInfoLine ConstructorUiId.itemPhotoText photoText
    ]
    @ itemPhotoThumbnail model item
    @ [ itemParameterGridView model item item.parameters dispatch; itemRevisionsView item ]

/// The chosen item's edit in place: its label, notes and photo as drafted, its parameters with their editor under the
/// pressed one, then Save and Cancel (spec 0051 `.manual/007` 4.2).
let private itemEditRows (model : TableAndElementRotationModel) (item : Item) (draft : ItemEditDraft) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    let notesBox =
        TextBox.create [
            yield! ConstructorUiId.itemNotesField.inputAttributes<TextBox> ()
            TextBox.acceptsReturn true
            TextBox.textWrapping TextWrapping.Wrap
            TextBox.minHeight 48.0
            TextBox.maxWidth 520.0
            TextBox.minWidth 320.0
            TextBox.horizontalAlignment HorizontalAlignment.Left
            TextBox.text draft.notesText.value
            TextBox.onTextChanged (Option.ofObj >> Option.map ControlCaption >> Option.iter (SetItemDraftNotes >> dispatch))
        ]
        |> View.withKey $"ItemNotesDraft_{item.id.value}"
        :> IView
    let photoText =
        match draft.photo, item.photoOpt with
        | KeepItemPhoto, Some path -> ControlCaption $"Photo: {path.value}"
        | KeepItemPhoto, None
        | DroppedItemPhoto, _ -> ControlCaption "No photo"
        | PickedItemPhoto file, _ -> ControlCaption $"Photo: {file.value} (attached at Save)"
    // Spec 0050 S7.2: the photo is picked as a lab file is attached, resolving the owner window from the press.
    let choosePhoto (pressed : CommandButton) : unit =
        match TopLevel.GetTopLevel pressed with
        | :? Window as owner -> pickDataFile owner model PickedPhotoFile (ControlCaption "Choose item photo") (fun file -> dispatch (SetItemDraftPhoto (PickedItemPhoto file)))
        | _ -> ()
    let photoVerbs : IView list =
        let remove =
            match draft.photo, item.photoOpt with
            | KeepItemPhoto, Some _
            | PickedItemPhoto _, _ ->
                [ CommandControls.verbCommand ConstructorUiId.itemRemovePhotoButton (ControlCaption "Remove photo") (fun () -> dispatch (SetItemDraftPhoto DroppedItemPhoto)) ]
            | KeepItemPhoto, None
            | DroppedItemPhoto, _ -> []
        CommandControls.verbCommandOn ConstructorUiId.itemChoosePhotoButton (ControlCaption "Choose photo…") choosePhoto :: remove
    [
        itemsPanelField (ControlCaption "Label:") ConstructorUiId.itemLabelField draft.labelText (SetItemDraftLabel >> dispatch)
        itemInfoLine ConstructorUiId.itemModelText (ControlCaption $"Model: {(model.itemModelCaption item).value}")
        itemInfoLine ConstructorUiId.itemDialText (ControlCaption $"Dial: {item.dial.caption.value}")
        (ControlCaption "Notes:").itemsPanelLine
        notesBox
        itemInfoLine ConstructorUiId.itemPhotoText photoText
        WrapPanel.create [ WrapPanel.orientation Orientation.Horizontal; WrapPanel.children photoVerbs ] :> IView
        (ControlCaption "Parameters: press a name to edit it.").itemsPanelLine
        itemParameterGridView model item draft.parameters dispatch
        model.draftActions (ControlCaption "Save") dispatch
    ]

/// The creation form: the model chosen in the library window, Choose another…, the label, the dial, then Create and
/// Cancel (spec 0051 `.manual/007` 4.3).
let private itemCreateRows (model : TableAndElementRotationModel) (form : AddItemForm) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    let chooseModel () : unit =
        model.kitShell.chooseItemModel
            {
                offers = form.offers
                chosenOpt = form.chosenOpt
                onChosen = fun offer -> dispatch (ChooseNewItemModel offer)
            }
    let chosen =
        match form.chosenOpt with
        | Some offer -> ControlCaption $"Model: {offer.caption.value}"
        | None -> ControlCaption "Model: none chosen yet — choose one in the library."
    let chooseCaption =
        match form.chosenOpt with
        | Some _ -> ControlCaption "Choose another model…"
        | None -> ControlCaption "Choose a model from the library…"
    let dialBox (dial : ItemDial) : IView =
        itemsPanelChoice (Some (ConstructorUiId.newItemDialChoice form.dial)) (ConstructorUiId.newItemDialChoice dial) dial.caption (fun () ->
            dispatch (SetNewItemDial dial))
    [
        (ControlCaption "New item").itemsPanelHeading
        itemInfoLine ConstructorUiId.addItemChosenText chosen
        CommandControls.verbCommand ConstructorUiId.addItemChooseModelButton chooseCaption chooseModel |> itemsPanelListBox
        itemsPanelField (ControlCaption "Label:") ConstructorUiId.newItemLabelField form.labelText (SetNewItemLabel >> dispatch)
        (ControlCaption "Mounting:").itemsPanelLine
        WrapPanel.create [
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.children [ dialBox FixedMounting; dialBox (DialMounted ItemModels.DialPositive); dialBox (DialMounted ItemModels.DialNegative) ]
        ]
        model.draftActions (ControlCaption "Create") dispatch
    ]

/// The Items page: New item… while the kit takes writes, then the kit's items, the chosen one open under its row; in
/// Creating only the creation form (spec 0050 S7.2; 0051 `.manual/007` 1.2, 4.1–4.3).
let itemsPanelView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let body : IView list =
        match model.pageMode, model.addItemFormOpt with
        | CreatingPageEntry, Some form -> itemCreateRows model form dispatch
        | _ ->
            let newVerb : IView list =
                match model.kitWrites, model.pageMode with
                | ControlOffered, BrowsingPage ->
                    [
                        CommandControls.verbCommand ConstructorUiId.newItemButton (ControlCaption "New item…") (fun () ->
                            dispatch BeginPageCreate
                            model.kitShell.chooseItemModel
                                {
                                    offers = model.addItemOffersOver (model.itemModels.listItemModels () |> Result.defaultValue [])
                                    chosenOpt = None
                                    onChosen = fun offer -> dispatch (ChooseNewItemModel offer)
                                })
                        |> itemsPanelListBox
                    ]
                | ControlOffered, (EditingPageEntry | CreatingPageEntry)
                | ControlWithheld, _ -> []
            let row (item : Item) : IView =
                let caption = ControlCaption $"{item.label.value} — {(model.itemModelCaption item).value} — revision {item.revision.value}"
                let menu = model.entryMenuOf item.label.caption (Some (RenameItemTarget item.id)) RenameAvailable dispatch
                let mark = if model.selectedItemOpt = Some item.id then ChosenChoice else UnchosenChoice
                match model.renameRowsOf (RenameItemTarget item.id) dispatch with
                | Some rows -> StackPanel.create [ StackPanel.children rows ] :> IView
                | None ->
                    let availability =
                        match model.pageMode with
                        | BrowsingPage -> ControlOffered
                        | EditingPageEntry
                        | CreatingPageEntry -> ControlWithheld
                    PageControls.entryToggle (ConstructorUiId.kitItemRow item.id) caption mark NeutralCommand availability menu (fun () -> dispatch (SelectKitItem item.id)) []
            let under (item : Item) : IView list =
                match model.selectedItemOpt with
                | Some id when id = item.id ->
                    let key = ConstructorUiKey $"ChosenKitItem_{item.id.value}_{model.pageMode.uiKey.value}"
                    match model.pageMode, model.itemEditOpt with
                    | EditingPageEntry, Some draft when draft.itemId = item.id -> underEntry key (itemEditRows model item draft dispatch)
                    | _ -> underEntry key (itemDetailsRows model item dispatch)
                | Some _
                | None -> []
            let items : IView list =
                match model.kitItems with
                | [] -> [ (ControlCaption "This kit holds no item yet: New item… adds one from the library.").itemsPanelLine ]
                | held ->
                    [
                        StackPanel.create [
                            yield! ConstructorUiId.kitItemList.groupAttributes<StackPanel> ()
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.margin (Thickness(0.0, 4.0, 0.0, 4.0))
                            StackPanel.children (entryRows held row under)
                        ]
                        :> IView
                    ]
            newVerb @ items
    pageScroll ConstructorUiId.itemsPanel (model.pageHeader dispatch @ body)
