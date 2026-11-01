namespace OpticalConstructor.Ui

open Avalonia.Automation
open Avalonia.Controls
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open OpticalConstructor.Controls
open OpticalConstructor.Domain.Library

/// Spec 0046 — the Library's light-source editor window: a FuncUI `HostWindow` mounting the pure
/// `LightSourceEditorView` MVU loop over the app-scope user-sources store (the Sample editor window's
/// sibling). Save stores through `SourceProxy.saveSource` and closes; Cancel closes, confirming first
/// when there are unsaved edits — and the title-bar X is gated the same way.
type LightSourceEditorWindow(sources : SourceProxy, intent : LightSourceEditorView.SourceEditorIntent) as this =
    inherit HostWindow()

    let mutable dispatch : LightSourceEditorView.Msg -> unit = ignore
    // The latest model, mirrored on every step — the `OnClosing` chrome gate reads dirtiness off it.
    let mutable latestModel : LightSourceEditorView.Model option = None

    do
        this.Title <-
            match intent with
            | LightSourceEditorView.EditSource s -> $"Source Editor — %s{s.name}"
            | LightSourceEditorView.NewSource _ -> "Source Editor — new source"
        this.Name <- UiIds.SourceEditor.window
        AutomationProperties.SetAutomationId(this, UiIds.SourceEditor.window)
        this.Width <- 620.0
        this.Height <- 280.0
        // Spec 0046: never larger than the screen it opens on (its content scrolls).
        WindowFit.keepOnScreen this
        let context : LightSourceEditorView.SourceEditorContext =
            {
                sources = sources
                requestClose = fun () -> this.Close()
            }
        let initModel = LightSourceEditorView.init context intent
        latestModel <- Some initModel
        Program.mkProgram
            (fun () -> initModel, Cmd.ofEffect (fun d -> dispatch <- d))
            (fun msg m -> let next = LightSourceEditorView.update msg m in latestModel <- Some next; next, Cmd.none)
            LightSourceEditorView.view
        |> Program.withHost this
        |> Program.run

    /// The window-chrome close gate (the Sample editor's): true — and the discard confirm shows — when a
    /// dirty editor must NOT close through the chrome; false when the close may proceed.
    member _.ChromeCloseIntercepted() : bool =
        match latestModel with
        | Some m when LightSourceEditorView.isDirty m ->
            dispatch LightSourceEditorView.CancelClicked
            true
        | _ -> false

    override this.OnClosing(e : WindowClosingEventArgs) =
        if not e.IsProgrammatic && this.ChromeCloseIntercepted() then e.Cancel <- true
        else base.OnClosing(e)
