namespace OpticalConstructor.Ui

open Avalonia.Automation
open Avalonia.Controls
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Hosts
open Avalonia.Input
open Elmish
open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Ui.ImportDialogView

/// Hosts pending entry decisions; only Apply imports them (spec 0050 S3.2; 001 §B).
type ImportDialogWindow(imports : CatalogueImportContext, request : ImportDialogRequest, onApplied : ImportDialogApplied -> unit) as this =
    inherit HostWindow()

    do
        this.Title <- "Import decisions"
        this.Name <- ConstructorUiId.importDialogWindow.value
        AutomationProperties.SetAutomationId(this, ConstructorUiId.importDialogWindow.value)
        AutomationProperties.SetName(this, this.Title)
        this.Width <- 900.0
        this.Height <- 640.0
        WindowFit.keepOnScreen this
        let context : ImportDialogContext =
            {
                imports = imports
                request = request
                onApplied = onApplied
                requestClose = this.Close
            }
        Program.mkProgram
            (fun () -> context.initImportDialog, Cmd.none)
            (fun msg model -> updateImportDialog msg model, Cmd.none)
            importDialogView
        |> Program.withHost this
        |> Program.run

module EntryFileDrop =

    /// Delivers nonempty drops as ordered local file names; unavailable platform data leaves state unchanged (spec 0050 S3.2; 001 §H).
    let acceptEntryFiles (window : Window) (onDropped : FileName list -> unit) : unit =
        DragDrop.SetAllowDrop(window, true)
        DragDrop.AddDragOverHandler(window, fun _ event ->
            try
                event.DragEffects <-
                    if event.DataTransfer.Contains DataFormat.File then DragDropEffects.Copy
                    else DragDropEffects.None
            with _ -> ())
        DragDrop.AddDropHandler(window, fun _ event ->
            try
                match event.DataTransfer.TryGetFiles() |> Option.ofObj with
                | None -> ()
                | Some files ->
                    let paths = files |> Seq.map (fun file -> FileName file.Path.LocalPath) |> Seq.toList
                    match paths with
                    | [] -> ()
                    | _ -> onDropped paths
            with _ -> ())
