namespace OpticalConstructor.Controls

open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

/// Copies text to the system clipboard: the window's at the composition root, a recording double in a test (spec 0051
/// `.manual/007` 1.3, 1.4).
[<ReferenceEquality>]
type ClipboardProxy =
    {
        copyText : ControlCaption -> unit
    }

    /// The test scenes' clipboard: it copies nothing.
    static member inert : ClipboardProxy = { copyText = ignore }

    /// The clipboard of `top`; a clipboard that refuses the text is logged, never raised, since a copy is a convenience.
    static member ofTopLevel (top : TopLevel) : ClipboardProxy =
        {
            copyText =
                fun text ->
                    try
                        match Option.ofObj top.Clipboard with
                        | Some clipboard -> Avalonia.Input.Platform.ClipboardExtensions.SetTextAsync(clipboard, text.value) |> ignore
                        | None -> ()
                    with error -> Softellect.Sys.Logging.Logger.logError $"ClipboardProxy: could not copy: {error.Message}"
        }

/// The one CSV field writer, which the chart's export shares (spec 0051 `.manual/007` 1.4).
[<AutoOpen>]
module ControlCaptionCsv =

    type ControlCaption with

        /// The text as one RFC 4180 field: quoted, its quotes doubled, when it holds a comma, a quote or a line break.
        member this.csvField : string =
            if this.value.IndexOfAny [| ','; '"'; '\n'; '\r' |] >= 0 then "\"" + this.value.Replace("\"", "\"\"") + "\""
            else this.value

/// Whether a dynamic button's entry can be renamed from its menu (spec 0051 `.manual/007` 1.3).
type EntryRenaming =
    /// Rename… is offered and starts the in-place rename.
    | RenameOffered of startRename : (unit -> unit)
    /// Rename… is shown, disabled, its reason as the item's tooltip.
    | RenameWithheld of reason : ControlCaption
    /// The entry has no Rename…: a fit run.
    | RenameNotOffered

/// A dynamic button's context menu: Copy name, then Rename… where its entry renames (spec 0051 `.manual/007` 1.3).
[<ReferenceEquality>]
type EntryMenu =
    {
        copyName : unit -> unit
        rename : EntryRenaming
    }

/// One cell of a grid table: its text, which Copy as CSV copies, drawn as selectable text or as its own view (spec 0051
/// `.manual/007` 1.4).
type GridCell =
    | TextGridCell of ControlCaption
    | ViewGridCell of text : ControlCaption * view : IView

    member this.text : ControlCaption =
        match this with
        | TextGridCell text
        | ViewGridCell (text, _) -> text

/// One row of a grid table: cells under the header's columns, or a view spanning them all that the CSV leaves out (the
/// Data page's panel under its matrix row, spec 0051 `.manual/007` 7.3).
type GridRow =
    | CellsGridRow of GridCell list
    | SpanningGridRow of IView

/// A table with gridlines and a header row, copyable as CSV (spec 0051 `.manual/007` 1.4).
type GridTable =
    {
        id : ConstructorUiId
        header : ControlCaption list
        rows : GridRow list
    }

    /// The header, then each row of cells, as RFC 4180 lines; a spanning row is no line (spec 0051 `.manual/007` 1.4).
    member this.csvText : ControlCaption =
        let line (cells : ControlCaption list) : string = cells |> List.map (fun cell -> cell.csvField) |> String.concat ","
        let rows =
            this.rows
            |> List.choose (fun row ->
                match row with
                | CellsGridRow cells -> Some (line (cells |> List.map (fun cell -> cell.text)))
                | SpanningGridRow _ -> None)
        ControlCaption ((line this.header :: rows |> String.concat "\n") + "\n")

/// What a page or a window is for, what it needs and what to do next (spec 0051 `.manual/007` 1.6).
type PageHelp =
    {
        paragraphs : ControlCaption list
    }

/// The builders every page shares: dynamic buttons with their menu, copyable text, grid tables and the help panel (spec
/// 0051 `.manual/007` Phase 1).
module PageControls =

    let private gridLine : Color = Color.FromRgb(170uy, 170uy, 170uy)
    let private headerFill : Color = Color.FromRgb(238uy, 238uy, 238uy)

    /// Text showing kit or model data: selectable, and copied with Ctrl+C (spec 0051 `.manual/007` 1.4).
    let copyableText (id : ConstructorUiId) (text : ControlCaption) (extras : IAttr<SelectableTextBlock> list) : IView =
        SelectableTextBlock.create (
            [
                id.automationAttribute<SelectableTextBlock> ()
                SelectableTextBlock.textWrapping TextWrapping.Wrap
                SelectableTextBlock.text text.value
            ]
            @ extras)
        |> Avalonia.FuncUI.DSL.View.withKey id.value
        :> IView

    /// Selectable text that needs no id of its own: a cell, a caption among many (spec 0051 `.manual/007` 1.4).
    let copyableLine (text : ControlCaption) (extras : IAttr<SelectableTextBlock> list) : IView =
        SelectableTextBlock.create ([ SelectableTextBlock.textWrapping TextWrapping.Wrap; SelectableTextBlock.text text.value ] @ extras) :> IView

    /// A dynamic button: a choice whose caption is kit data, its full caption as its tooltip and a context menu with Copy
    /// name and, where offered, Rename… (spec 0051 `.manual/007` 1.3). `look` is its unchosen fill.
    let entryToggle
        (id : ConstructorUiId)
        (caption : ControlCaption)
        (mark : ChoiceMark)
        (look : CommandEmphasis)
        (availability : ControlAvailability)
        (menu : EntryMenu)
        (onChoose : unit -> unit)
        (extras : IAttr<ChoiceToggle> list)
        : IView =
        let copyItem : IView =
            MenuItem.create [
                ConstructorUiId.entryCopyNameItem.automationAttribute<MenuItem> ()
                MenuItem.header "Copy name"
                MenuItem.onClick ((fun _ -> menu.copyName ()), SubPatchOptions.Always)
            ]
            :> IView
        let renameItems : IView list =
            match menu.rename with
            | RenameOffered startRename ->
                [
                    MenuItem.create [
                        ConstructorUiId.entryRenameItem.automationAttribute<MenuItem> ()
                        MenuItem.header "Rename…"
                        MenuItem.onClick ((fun _ -> startRename ()), SubPatchOptions.Always)
                    ]
                    :> IView
                ]
            | RenameWithheld reason ->
                [
                    MenuItem.create [
                        ConstructorUiId.entryRenameItem.automationAttribute<MenuItem> ()
                        MenuItem.header "Rename…"
                        MenuItem.isEnabled false
                        AttrBuilder<MenuItem>.CreateProperty<obj>(ToolTip.TipProperty, box reason.value, ValueNone)
                        AttrBuilder<MenuItem>.CreateProperty<bool>(ToolTip.ShowOnDisabledProperty, true, ValueNone)
                    ]
                    :> IView
                ]
            | RenameNotOffered -> []
        CommandControls.choiceToggleShowing id caption caption.wrappingText mark PositiveCommand availability onChoose
            ([
                AttrBuilder<ChoiceToggle>.CreateProperty<CommandEmphasis>(ChoiceToggle.EmphasisProperty, look, ValueNone)
                AttrBuilder<ChoiceToggle>.CreateProperty<obj>(ToolTip.TipProperty, box caption.value, ValueNone)
                AttrBuilder<ChoiceToggle>.CreateProperty<string>(AutomationProperties.HelpTextProperty, caption.value, ValueNone)
                ToggleButton.contextMenu (ContextMenu.create [ ContextMenu.viewItems (copyItem :: renameItems) ])
                ToggleButton.margin (Thickness(0.0, 0.0, 8.0, 4.0))
                ToggleButton.horizontalAlignment HorizontalAlignment.Left
                ToggleButton.horizontalContentAlignment HorizontalAlignment.Left
            ]
             @ extras)

    /// The table with a gridline round every cell, the header on a grey fill, every text cell selectable, and Copy as
    /// CSV above it copying `csvText` through `copy` (spec 0051 `.manual/007` 1.4). It scrolls sideways in its own
    /// scroller, never the page.
    let gridTableView (table : GridTable) (copy : ControlCaption -> unit) : IView =
        let columns = max 1 (List.length table.header)
        let cellBorder (row : int) (column : int) (span : int) (fill : Color option) (child : IView) : IView =
            Border.create (
                [
                    Grid.row row
                    Grid.column column
                    Grid.columnSpan span
                    Border.borderBrush (SolidColorBrush gridLine)
                    Border.borderThickness (Thickness(0.0, 0.0, 1.0, 1.0))
                    Border.padding (Thickness(6.0, 3.0))
                    Border.child child
                ]
                @ (match fill with
                   | Some colour -> [ Border.background (SolidColorBrush colour) ]
                   | None -> []))
            :> IView
        let headerCells =
            table.header
            |> List.mapi (fun column caption ->
                cellBorder 0 column 1 (Some headerFill) (copyableLine caption [ SelectableTextBlock.fontWeight FontWeight.SemiBold ]))
        let bodyCells =
            table.rows
            |> List.mapi (fun index row ->
                let gridRow = index + 1
                match row with
                | CellsGridRow cells ->
                    cells
                    |> List.mapi (fun column cell ->
                        let child =
                            match cell with
                            | TextGridCell text -> copyableLine text []
                            | ViewGridCell (_, view) -> view
                        cellBorder gridRow column 1 None child)
                | SpanningGridRow view -> [ cellBorder gridRow 0 columns None view ])
            |> List.concat
        let grid =
            Border.create [
                yield! table.id.groupAttributes<Border> ()
                Border.borderBrush (SolidColorBrush gridLine)
                Border.borderThickness (Thickness(1.0, 1.0, 0.0, 0.0))
                Border.horizontalAlignment HorizontalAlignment.Left
                Border.child (
                    Grid.create [
                        Grid.columnDefinitions (List.replicate columns "Auto" |> String.concat ",")
                        Grid.rowDefinitions (List.replicate (List.length table.rows + 1) "Auto" |> String.concat ",")
                        Grid.children (headerCells @ bodyCells)
                    ])
            ]
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.margin (Thickness(0.0, 2.0, 0.0, 6.0))
            StackPanel.children [
                Border.create [
                    Border.horizontalAlignment HorizontalAlignment.Left
                    Border.child (
                        CommandControls.verbCommand (ConstructorUiId.copyTableCsvButton table.id) (ControlCaption "Copy as CSV") (fun () ->
                            copy table.csvText))
                ]
                ScrollViewer.create [
                    ScrollViewer.horizontalScrollBarVisibility ScrollBarVisibility.Auto
                    ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Disabled
                    ScrollViewer.content grid
                ]
            ]
        ]
        |> Avalonia.FuncUI.DSL.View.withKey table.id.value
        :> IView

    /// The `?` Help panel: what the page is for, what it needs and what to do next, collapsed until opened; an
    /// `Expander`, so UI Automation expands it through its ExpandCollapse pattern (spec 0051 `.manual/007` 1.6).
    let pageHelpView (id : ConstructorUiId) (help : PageHelp) : IView =
        Expander.create [
            id.automationAttribute<Expander> ()
            AttrBuilder<Expander>.CreateProperty<string>(AutomationProperties.NameProperty, "Help", ValueNone)
            Expander.header "? Help — what this page does"
            Expander.horizontalAlignment HorizontalAlignment.Stretch
            Expander.margin (Thickness(0.0, 0.0, 0.0, 6.0))
            Expander.content (
                StackPanel.create [
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.children (
                        help.paragraphs
                        |> List.map (fun paragraph -> copyableLine paragraph [ SelectableTextBlock.margin (Thickness(0.0, 0.0, 0.0, 4.0)) ]))
                ])
        ]
        |> Avalonia.FuncUI.DSL.View.withKey id.value
        :> IView
