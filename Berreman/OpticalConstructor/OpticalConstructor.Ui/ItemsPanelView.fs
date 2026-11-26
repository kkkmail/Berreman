/// The Items panel, a constructor surface of its own: the kit's items, "Add item from library model…" and the selected
/// item's label, model, dial, notes, photo, parameter grid and revisions (spec 0050 S7.2; 001 §O).
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
    if chosenOpt = Some id then MaterialsWindowView.actionButton id caption MaterialsWindowView.positiveBackground onChosen
    else MaterialsWindowView.verbButton id caption onChosen

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
                id.automationAttribute<TextBox> ()
                TextBox.width 220.0
                TextBox.horizontalAlignment HorizontalAlignment.Left
                TextBox.text text.value
                TextBox.onTextChanged (Option.ofObj >> Option.map ControlCaption >> Option.iter onChanged)
            ]
        ]
    ]
    |> View.withKey id.value
    :> IView

/// The open add form: the offers grouped by kind in their own bounded scroller, the label, the dial, then Add and
/// Cancel on one row, the Add box's right margin parting the pair (spec 0050 S7.2; .spec-md §10.5).
let private addItemFormRows (model : TableAndElementRotationModel) (form : AddItemForm) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    let chosenOpt = form.chosenOpt |> Option.map ConstructorUiId.addItemOffer
    let offerBox (offer : ItemModelChoice) : IView =
        itemsPanelChoice chosenOpt (ConstructorUiId.addItemOffer offer) offer.caption (fun () -> dispatch (ChooseNewItemModel offer))
        |> itemsPanelListBox
    let group (heading : ControlCaption) (offers : ItemModelChoice list) : IView list =
        match offers with
        | [] -> []
        | _ :: _ -> heading.itemsPanelHeading :: (offers |> List.map offerBox)
    let itemModels =
        form.offers
        |> List.filter (fun offer ->
            match offer with
            | LibraryModelChoice _ -> true
            | SourcePresetChoice _ | PolarizerPresetChoice _ | StackSampleChoice _ -> false)
    let sources =
        form.offers
        |> List.filter (fun offer ->
            match offer with
            | SourcePresetChoice _ -> true
            | LibraryModelChoice _ | PolarizerPresetChoice _ | StackSampleChoice _ -> false)
    let polarizers =
        form.offers
        |> List.filter (fun offer ->
            match offer with
            | PolarizerPresetChoice _ -> true
            | LibraryModelChoice _ | SourcePresetChoice _ | StackSampleChoice _ -> false)
    let samples =
        form.offers
        |> List.filter (fun offer ->
            match offer with
            | StackSampleChoice _ -> true
            | LibraryModelChoice _ | SourcePresetChoice _ | PolarizerPresetChoice _ -> false)
    let dialBox (dial : ItemDial) : IView =
        itemsPanelChoice (Some (ConstructorUiId.newItemDialChoice form.dial)) (ConstructorUiId.newItemDialChoice dial) dial.caption (fun () ->
            dispatch (SetNewItemDial dial))
    let add : IView list =
        match form.itemDraftOpt, model.kitWrites with
        | Some draft, ControlOffered ->
            [
                MaterialsWindowView.actionButton
                    ConstructorUiId.addItemConfirmButton
                    (ControlCaption "Add")
                    MaterialsWindowView.positiveBackground
                    (fun () -> dispatch (AddItem draft))
            ]
        | Some _, ControlWithheld
        | None, _ -> []
    let cancel =
        MaterialsWindowView.actionButton
            ConstructorUiId.addItemCancelButton
            (ControlCaption "Cancel")
            MaterialsWindowView.negativeBackground
            (fun () -> dispatch CancelAddItemForm)
    // `actionButton` subscribes its press once per key, so the row is keyed by the draft it sends.
    let actionsKey =
        match form.itemDraftOpt with
        | Some draft ->
            let offer = ConstructorUiId.addItemOffer draft.model
            let dial = ConstructorUiId.newItemDialChoice draft.dial
            $"AddItemActions_{draft.id.value}_{offer.value}_{dial.value}_{draft.label.value}"
        | None -> "AddItemActions"
    [
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.margin (Thickness(0.0, 4.0, 0.0, 4.0))
            StackPanel.children [
                (ControlCaption "Choose a model:").itemsPanelLine
                ScrollViewer.create [
                    ConstructorUiId.addItemOfferList.automationAttribute<ScrollViewer> ()
                    ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
                    ScrollViewer.maxHeight 220.0
                    ScrollViewer.content (
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.children (
                                group (ControlCaption "Item models") itemModels
                                @ group (ControlCaption "Light sources") sources
                                @ group (ControlCaption "Polarizers") polarizers
                                @ group (ControlCaption "Samples") samples)
                        ])
                ]
                itemsPanelField (ControlCaption "Label:") ConstructorUiId.newItemLabelField form.labelText (SetNewItemLabel >> dispatch)
                (ControlCaption "Dial:").itemsPanelLine
                WrapPanel.create [
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.children [ dialBox FixedMounting; dialBox (DialMounted ItemModels.DialPositive); dialBox (DialMounted ItemModels.DialNegative) ]
                ]
                StackPanel.create [
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.children (add @ [ cancel ])
                ]
                |> View.withKey actionsKey
            ]
        ]
        |> View.withKey ConstructorUiId.addItemOfferList.value
        :> IView
    ]

/// The kit's items, one row each, the selected one filled, or the line that the kit holds none (spec 0050 S7.2).
let private kitItemListView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let selectedOpt = model.selectedItemOpt |> Option.map ConstructorUiId.kitItemRow
    let row (item : Item) : IView =
        let caption = ControlCaption $"{item.label.value} — {(model.itemModelCaption item).value} — revision {item.revision.value}"
        itemsPanelChoice selectedOpt (ConstructorUiId.kitItemRow item.id) caption (fun () -> dispatch (SelectKitItem item.id))
        |> itemsPanelListBox
    let rows =
        match model.kitItems with
        | [] -> [ (ControlCaption "This kit holds no item yet.").itemsPanelLine ]
        | items -> items |> List.map row
    StackPanel.create [
        ConstructorUiId.kitItemList.automationAttribute<StackPanel> ()
        StackPanel.orientation Orientation.Vertical
        StackPanel.margin (Thickness(0.0, 4.0, 0.0, 4.0))
        StackPanel.children rows
    ]
    |> View.withKey ConstructorUiId.kitItemList.value
    :> IView

/// The selected item's parameters: a semi-bold header row, then one row per parameter in the item's order, its name a
/// box that opens the editor and its other cells `gridCells` (spec 0050 S7.2; 001 §O).
let private itemParameterGridView (item : Item) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let cell (row : int) (column : int) (weight : FontWeight) (caption : ControlCaption) : IView =
        TextBlock.create [
            TextBlock.row row
            TextBlock.column column
            TextBlock.fontWeight weight
            TextBlock.verticalAlignment VerticalAlignment.Center
            TextBlock.margin (Thickness(0.0, 0.0, 12.0, 4.0))
            TextBlock.text caption.value
        ] :> IView
    let header =
        [
            ControlCaption "Parameter"
            ControlCaption "State"
            ControlCaption "Value"
            ControlCaption "Bounds"
            ControlCaption "σ"
            ControlCaption "Provenance"
        ]
        |> List.mapi (fun column caption -> cell 0 column FontWeight.SemiBold caption)
    let parameterRow (index : int) (parameter : Parameter) : IView list =
        let row = index + 1
        let cells = parameter.gridCells
        [
            Border.create [
                Border.row row
                Border.column 0
                Border.child (
                    MaterialsWindowView.verbButton (ConstructorUiId.itemParameterRow parameter.name) parameter.name.caption (fun () ->
                        dispatch (EditItemParameter parameter.name)))
            ]
            cell row 1 FontWeight.Normal cells.state
            cell row 2 FontWeight.Normal cells.value
            cell row 3 FontWeight.Normal cells.bounds
            cell row 4 FontWeight.Normal cells.sigma
            cell row 5 FontWeight.Normal cells.provenance
        ]
    Grid.create [
        ConstructorUiId.itemParameterGrid.automationAttribute<Grid> ()
        Grid.margin (Thickness(0.0, 6.0, 0.0, 4.0))
        Grid.columnDefinitions "Auto,Auto,Auto,Auto,Auto,Auto"
        Grid.rowDefinitions (List.replicate (List.length item.parameters + 1) "Auto" |> String.concat ",")
        Grid.children (header @ (item.parameters |> List.mapi parameterRow |> List.concat))
    ]
    |> View.withKey $"{ConstructorUiId.itemParameterGrid.value}_{item.id.value}"
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
                MaterialsWindowView.actionButton
                    ConstructorUiId.parameterEditSetButton
                    (ControlCaption "Set")
                    MaterialsWindowView.positiveBackground
                    (fun () -> dispatch (setWith state))
            ]
        | Ok _, ControlWithheld
        | Error _, _ -> []
    let cancel =
        MaterialsWindowView.actionButton
            ConstructorUiId.parameterEditCancelButton
            (ControlCaption "Cancel")
            MaterialsWindowView.negativeBackground
            (fun () -> dispatch CancelParameterEdit)
    // `actionButton` subscribes its press once per key, so the row is keyed by the texts the state is read from.
    let actionsKey =
        let choice = chosenOpt |> Option.map (fun id -> id.value) |> Option.defaultValue ""
        $"ParameterEditActions_{draft.parameter.name.value}_{choice}_{draft.valueText.value}_{draft.lowerText.value}_{draft.upperText.value}_{draft.priorSigmaText.value}"
    StackPanel.create [
        ConstructorUiId.parameterEditPanel.automationAttribute<StackPanel> ()
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

/// The selected item: its label and notes, edited while the kit takes writes, its model, dial and photo, the parameter
/// grid with its open editor, and its revisions, the current one first (spec 0050 S7.2; 001 §O).
let private selectedItemRows (model : TableAndElementRotationModel) (item : Item) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    // Spec 0050 S7.2: a blank or unchanged label is ignored, as `ExperimentControls`' number fields ignore an unreadable one.
    let commitLabel (source : obj) : unit =
        match source with
        | :? TextBox as textBox ->
            match ItemLabel.tryCreate textBox.Text with
            | Ok label when label <> item.label -> dispatch (RelabelKitItem (item.id, label))
            | Ok _
            | Error _ -> ()
        | _ -> ()
    let commitNotes (source : obj) : unit =
        match source with
        | :? TextBox as textBox ->
            let notes = ItemNotes (Option.ofObj textBox.Text |> Option.defaultValue "")
            if notes <> item.notes then dispatch (SetKitItemNotes (item.id, notes))
        | _ -> ()
    let labelRow =
        DockPanel.create [
            DockPanel.margin (Thickness(0.0, 6.0, 0.0, 2.0))
            DockPanel.children [
                TextBlock.create [
                    TextBlock.dock Dock.Left
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    TextBlock.margin (Thickness(0.0, 0.0, 6.0, 0.0))
                    TextBlock.text "Label:"
                ]
                TextBox.create [
                    ConstructorUiId.itemLabelField.automationAttribute<TextBox> ()
                    TextBox.width 220.0
                    TextBox.horizontalAlignment HorizontalAlignment.Left
                    TextBox.isEnabled model.kitWrites.isEnabled
                    TextBox.text item.label.value
                    TextBox.onKeyDown ((fun e -> if e.Key = Key.Enter then commitLabel e.Source), SubPatchOptions.Always)
                    TextBox.onLostFocus ((fun e -> commitLabel e.Source), SubPatchOptions.Always)
                ]
            ]
        ]
    let infoText (id : ConstructorUiId) (caption : ControlCaption) : IView =
        TextBlock.create [
            id.automationAttribute<TextBlock> ()
            TextBlock.textWrapping TextWrapping.Wrap
            TextBlock.margin (Thickness(0.0, 2.0, 0.0, 2.0))
            TextBlock.text caption.value
        ] :> IView
    let notesBox =
        TextBox.create [
            ConstructorUiId.itemNotesField.automationAttribute<TextBox> ()
            TextBox.acceptsReturn true
            TextBox.textWrapping TextWrapping.Wrap
            TextBox.minHeight 48.0
            TextBox.maxWidth 520.0
            TextBox.horizontalAlignment HorizontalAlignment.Left
            TextBox.minWidth 320.0
            TextBox.isEnabled model.kitWrites.isEnabled
            TextBox.text item.notes.value
            TextBox.onLostFocus ((fun e -> commitNotes e.Source), SubPatchOptions.Always)
        ]
    let photoText =
        match item.photoOpt with
        | Some path -> ControlCaption $"Photo: {path.value}"
        | None -> ControlCaption "No photo"
    // Spec 0050 S7.2: the photo is picked as a lab file is attached, resolving the owner window from the press.
    let choosePhoto (e : PointerPressedEventArgs) : unit =
        match e.Source with
        | :? Visual as source ->
            match TopLevel.GetTopLevel source with
            | :? Window as owner -> pickDataFile owner (ControlCaption "Choose item photo") (fun file -> dispatch (SetKitItemPhoto (item.id, file)))
            | _ -> ()
        | _ -> ()
    let photoVerbs : IView list =
        match model.kitWrites, item.photoOpt with
        | ControlOffered, Some _ ->
            [
                MaterialsWindowView.verbButtonOn ConstructorUiId.itemChoosePhotoButton (ControlCaption "Choose photo…") choosePhoto
                MaterialsWindowView.verbButton ConstructorUiId.itemRemovePhotoButton (ControlCaption "Remove photo") (fun () ->
                    dispatch (RemoveKitItemPhoto item.id))
            ]
        | ControlOffered, None -> [ MaterialsWindowView.verbButtonOn ConstructorUiId.itemChoosePhotoButton (ControlCaption "Choose photo…") choosePhoto ]
        | ControlWithheld, _ -> []
    let editor : IView list =
        match model.parameterEditOpt with
        | Some draft ->
            match draft.parameter with
            | ItemParameterRef (id, name) when id = item.id ->
                [ parameterEditorView model draft (fun state -> SetParameterState (item.id, name, state)) dispatch ]
            | ItemParameterRef _
            | MaterialParameterRef _ -> []
        | None -> []
    let revisionLine (revision : ItemRevision) (caption : ControlCaption) : IView =
        let id = ConstructorUiId.itemRevisionRow revision
        TextBlock.create [
            id.automationAttribute<TextBlock> ()
            TextBlock.textWrapping TextWrapping.Wrap
            TextBlock.margin (Thickness(0.0, 1.0, 0.0, 1.0))
            TextBlock.text caption.value
        ]
        |> View.withKey id.value
        :> IView
    let revisions =
        StackPanel.create [
            ConstructorUiId.itemRevisionList.automationAttribute<StackPanel> ()
            StackPanel.orientation Orientation.Vertical
            StackPanel.margin (Thickness(0.0, 4.0, 0.0, 4.0))
            StackPanel.children (
                revisionLine item.revision (ControlCaption $"Revision {item.revision.value} (current)")
                :: (item.history |> List.rev |> List.map (fun record -> revisionLine record.revision record.caption)))
        ]
    [
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.children (
                [
                    labelRow :> IView
                    infoText ConstructorUiId.itemModelText (ControlCaption $"Model: {(model.itemModelCaption item).value}")
                    infoText ConstructorUiId.itemDialText (ControlCaption $"Dial: {item.dial.caption.value}")
                    (ControlCaption "Notes:").itemsPanelLine
                    notesBox :> IView
                    infoText ConstructorUiId.itemPhotoText photoText
                    WrapPanel.create [
                        WrapPanel.orientation Orientation.Horizontal
                        WrapPanel.children photoVerbs
                    ]
                    itemParameterGridView item dispatch
                ]
                @ editor
                @ [ revisions :> IView ])
        ]
        // Keyed by the item, so a press handler or a text box never carries another item's state.
        |> View.withKey $"SelectedKitItem_{item.id.value}"
        :> IView
    ]

/// The Items panel over the constructor window's model, every write control withheld while the kit is open read-only
/// (spec 0050 S7.2, S5.3).
let itemsPanelView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let addVerb : IView list =
        match model.kitWrites, model.addItemFormOpt with
        | ControlOffered, None ->
            [
                MaterialsWindowView.verbButton ConstructorUiId.addItemButton (ControlCaption "Add item from library model…") (fun () ->
                    dispatch OpenAddItemForm)
                |> itemsPanelListBox
            ]
        | ControlOffered, Some _
        | ControlWithheld, _ -> []
    let form : IView list =
        match model.addItemFormOpt with
        | Some opened -> addItemFormRows model opened dispatch
        | None -> []
    let selected : IView list =
        match model.selectedItemOpt |> Option.bind (fun id -> model.kitItems |> List.tryFind (fun item -> item.id = id)) with
        | Some item -> selectedItemRows model item dispatch
        | None -> []
    ScrollViewer.create [
        ScrollViewer.horizontalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.content (
            StackPanel.create [
                ConstructorUiId.itemsPanel.automationAttribute<StackPanel> ()
                StackPanel.orientation Orientation.Vertical
                StackPanel.margin (Thickness 8.0)
                StackPanel.children (
                    [
                        (ControlCaption "Items").itemsPanelHeading
                        (ControlCaption "The physical pieces of this kit: each instantiates a library model and holds its own parameters.").itemsPanelLine
                    ]
                    @ addVerb
                    @ form
                    @ [ kitItemListView model dispatch ]
                    @ selected)
            ])
    ] :> IView
