/// The Data locations screen: every location with its path and Open folder, an update that reaches the file browser
/// only through its injected context, and the view projecting it (spec 0050 S5.3).
module OpticalConstructor.Ui.DataLocationsView

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Storage.LibraryDatabaseStore
open OpticalConstructor.Storage.LibraryBackupStore
open OpticalConstructor.Controls

/// What a listed location shows, and the folder its Open folder opens (spec 0050 S5.3).
type DataLocationRow =
    {
        /// The location the row lists (spec 0050 S5.3).
        kind : DataLocationKind
        /// The location's path as the row shows it (spec 0050 S5.3).
        path : ControlCaption
        /// The folder Open folder opens; `None` when there is none, as for an in-memory library (spec 0050 S5.3).
        folderOpt : FolderName option
    }

type DataLocations with

    /// One row per `DataLocationKind.all`, in that order. It reads no disk: a folder not made yet is listed, and Open
    /// folder tells it (spec 0050 S5.3).
    member this.rows : DataLocationRow list =
        let folderRow (kind : DataLocationKind) (folder : FolderName) : DataLocationRow =
            {
                kind = kind
                path = folder.pathCaption
                folderOpt = Some folder
            }

        DataLocationKind.all
        |> List.map (fun kind ->
            match kind with
            | LibraryDatabaseLocation ->
                match LibraryDatabase.tryFileOf this.libraryConnection with
                | Some file ->
                    {
                        kind = kind
                        path = file.pathCaption
                        folderOpt = file.tryGetFolderName
                    }
                | None ->
                    {
                        kind = kind
                        path = ControlCaption $"In memory ({this.libraryConnection.value})"
                        folderOpt = None
                    }
            | LibraryBackupsLocation -> folderRow kind this.backupsFolder
            | PreferencesLocation -> folderRow kind this.preferencesFolder
            | KitsLocation -> folderRow kind this.kitsFolder
            | ExportsLocation -> folderRow kind this.exportsFolder
            | CataloguesLocation -> folderRow kind this.cataloguesFolder
            | ShippedCatalogueLocation -> folderRow kind this.shippedCatalogueFolder)

/// The screen's injected seams; the composition root bakes the real IO into them and tests substitute doubles.
/// Function-valued fields compare by reference (spec 0050 S5.3).
[<ReferenceEquality>]
type DataLocationsContext =
    {
        /// The locations the screen lists (spec 0050 S5.3).
        locations : DataLocations
        /// The file browser Open folder hands a row's folder to (spec 0050 S5.3).
        explorer : ExplorerProxy
    }

/// The screen's model: the listed rows and the last refusal that needs telling (spec 0050 S5.3).
type DataLocationsModel =
    {
        /// The seams the update reaches IO through (spec 0050 S5.3).
        context : DataLocationsContext
        /// The listed locations, in `DataLocationKind.all` order (spec 0050 S5.3).
        rows : DataLocationRow list
        /// Why the last Open folder failed; `None` once one succeeds (spec 0050 S5.3).
        status : ControlCaption option
    }

type DataLocationsContext with

    /// The screen as it opens over these seams: their rows and no status (spec 0050 S5.3).
    member this.initDataLocations : DataLocationsModel =
        {
            context = this
            rows = this.locations.rows
            status = None
        }

/// The screen's semantic messages: the button and the test send the same one (spec 0050 S5.3).
type DataLocationsMsg =
    /// Open folder: hand the location's folder to the file browser; a row without a folder does nothing (spec 0050 S5.3).
    | OpenDataLocationFolder of DataLocationKind

/// Hands the kind's folder to the explorer: `Ok` clears the status and a refusal tells why; a row without a folder
/// answers the model unchanged (spec 0050 S5.3).
let updateDataLocations (msg : DataLocationsMsg) (m : DataLocationsModel) : DataLocationsModel =
    match msg with
    | OpenDataLocationFolder kind ->
        match m.rows |> List.tryFind (fun row -> row.kind = kind) |> Option.bind _.folderOpt with
        | Some folder ->
            match m.context.explorer.showFolder folder with
            | Ok () -> { m with status = None }
            | Error error ->
                {
                    m with
                        status = Some (ControlCaption $"The folder could not be opened — {error.describe}")
                }
        | None -> m

/// The screen lays out at least this wide and scrolls sideways below it, as the kit start screen does (spec 0050 S5.3).
let private dataLocationsMinWidth : PanelMinWidth = PanelMinWidth 480.0

/// One location: Open folder on the right for a row with a folder, its caption and wrapping path in the fill. Keyed by
/// its id, so a status shown above it never patches one row into another (spec 0050 S5.3).
let private dataLocationRowView (dispatch : DataLocationsMsg -> unit) (row : DataLocationRow) : IView =
    let kind = row.kind
    let rowId = ConstructorUiId.dataLocationRow kind

    let openFolder : IView list =
        match row.folderOpt with
        | Some _ ->
            [
                StackPanel.create [
                    StackPanel.dock Dock.Right
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.verticalAlignment VerticalAlignment.Center
                    StackPanel.children [
                        MaterialsWindowView.verbButton (ConstructorUiId.dataLocationOpenFolderButton kind) (ControlCaption "Open folder") (fun () ->
                            dispatch (OpenDataLocationFolder kind))
                    ]
                ]
            ]
        | None -> []

    let captionAndPath : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.children [
                TextBlock.create [
                    TextBlock.fontWeight FontWeight.SemiBold
                    TextBlock.text kind.caption.value
                ]
                TextBlock.create [
                    (ConstructorUiId.dataLocationPathText kind).automationAttribute<TextBlock> ()
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.text row.path.value
                ]
            ]
        ]

    DockPanel.create [
        rowId.automationAttribute<DockPanel> ()
        DockPanel.margin (Thickness(0.0, 0.0, 0.0, 6.0))
        DockPanel.children (openFolder @ [ captionAndPath ])
    ]
    |> Avalonia.FuncUI.DSL.View.withKey rowId.value
    :> IView

/// The screen as a pure projection of its model: the status, then one row per location, every control under its
/// `ConstructorUiId`. Nothing is edited here: the locations are edited in `appsettings.json` (001 §G, spec 0050 S5.3).
let dataLocationsView (m : DataLocationsModel) (dispatch : DataLocationsMsg -> unit) : IView =
    // The last refusal, in the library windows' refusal colour.
    let statusRows : IView list =
        match m.status with
        | Some status ->
            [
                TextBlock.create [
                    ConstructorUiId.dataLocationsStatusText.automationAttribute<TextBlock> ()
                    TextBlock.foreground (MaterialsWindowView.brush MaterialsWindowView.messageColor)
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.text status.value
                ] :> IView
            ]
        | None -> []

    ScrollViewer.create [
        ScrollViewer.horizontalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.content (
            MinWidthPanel.create [
                dataLocationsMinWidth.minWidthPanelAttribute ()
                MinWidthPanel.child (
                    StackPanel.create [
                        StackPanel.orientation Orientation.Vertical
                        StackPanel.margin 12.0
                        StackPanel.spacing 8.0
                        StackPanel.children (statusRows @ (m.rows |> List.map (dataLocationRowView dispatch)))
                    ])
            ])
    ] :> IView
