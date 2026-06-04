/// Headless construction-edit view tests (spec 0024 Part U2 §U2.1–U2.3, gate
/// `ui-tests`). Trait `Category=ui-tests` (the `ui-tests` gate runs
/// `--filter Category!=ui-smoke`).
///
/// Asserts the Part U2 edit path end-to-end:
///   • AC-U2.1 — a simulated stack edit dispatches `Construction (EditStack …)`; the
///     root `Shell.update` marks the node busy (`isNodeBusy`) and attaches the async
///     node-solve `Cmd`, which (run) marshals `Construction (NodeSolved …)` back onto
///     the UI thread (§0.4) so the busy clears and per-node results refresh with no
///     manual reload.
///   • AC-U2.2 — the delete-confirmation gate renders same-row Confirm/Cancel CTAs +
///     an Undo affordance; Confirm dispatches `ConfirmDeleteNode`, and the frozen
///     `update` removes the sub-tree leaving single-level undo available.
namespace OpticalConstructor.Ui.Tests

open System.Collections.Generic
open System.Threading
open Avalonia.Controls
open Avalonia.Interactivity
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open OpticalConstructor.Domain
open OpticalConstructor.Domain.BeamTree
open OpticalConstructor.Ui

module ConstructionEditTests =

    /// The first button in the live visual tree whose content equals `label`.
    let private buttonByContent (window : Window) (label : string) : Button option =
        window.GetVisualDescendants()
        |> Seq.choose (fun v -> match v with | :? Button as b -> Some b | _ -> None)
        |> Seq.tryFind (fun b -> string b.Content = label)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``stack edit dispatches EditStack, marks node busy, and async solve refreshes via NodeSolved`` () =
        HeadlessSession.run (fun () ->
            // The stack panel routes a simulated "Add layer" through EditStack (R-1).
            let m0 = fst Shell.init
            let path = m0.construction.selected
            let captured = List<ConstructionPage.Msg>()
            let window = Window()
            window.Content <- Component(fun _ctx -> ConstructionView.stackPanel m0.construction captured.Add)
            window.Show()
            Dispatcher.UIThread.RunJobs()

            (buttonByContent window "Add layer").Value.RaiseEvent(RoutedEventArgs(Button.ClickEvent))
            Dispatcher.UIThread.RunJobs()
            Assert.Contains(captured, fun m ->
                match m with
                | ConstructionPage.EditStack (p, StackEditor.AddLayer _) -> p = path
                | _ -> false)
            window.Close()

            // The root update marks the edited node busy and attaches the async solve
            // Cmd; run it and assert the marshaled NodeSolved lands (§0.4 / AC-U2.1).
            let editMsg = Shell.RootMsg.Construction (ConstructionPage.EditStack (path, StackEditor.AddLayer StackEditor.defaultNewLayer))
            let m1, cmd = Shell.update editMsg m0
            Assert.True(ConstructionPage.isNodeBusy path m1.construction, "edited node must be busy")
            Assert.NotEmpty(cmd)

            let dispatched = List<Shell.RootMsg>()
            for effect in cmd do effect dispatched.Add
            // Pump the UI thread until the off-thread solve posts NodeSolved back.
            let mutable spins = 0
            while dispatched.Count = 0 && spins < 300 do
                Dispatcher.UIThread.RunJobs()
                Thread.Sleep 10
                spins <- spins + 1
            Dispatcher.UIThread.RunJobs()

            let solvedMsg =
                dispatched
                |> Seq.tryPick (fun m ->
                    match m with
                    | Shell.RootMsg.Construction (ConstructionPage.NodeSolved _ as nm) -> Some (Shell.RootMsg.Construction nm)
                    | _ -> None)
            Assert.True(solvedMsg.IsSome, "expected a marshaled NodeSolved message")

            // Applying NodeSolved clears the busy flag and refreshes the per-node
            // results with no manual reload (AC-U2.1).
            let m2, _ = Shell.update solvedMsg.Value m1
            Assert.False(ConstructionPage.isNodeBusy path m2.construction, "busy must clear after NodeSolved")
            Assert.True(m2.construction.results.ContainsKey path, "results must refresh for the solved node"))

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``delete-node gate renders same-row confirm/cancel + undo and confirm removes the sub-tree`` () =
        HeadlessSession.run (fun () ->
            // Build a tree whose middle node has a child, so deleting it routes through
            // the pre-confirmation gate (a childless node would delete directly).
            let m0 = fst Shell.init
            let leaf = { m0.construction.project.beamTree.root with children = Map.empty }
            let c1 = ConstructionPage.update (ConstructionPage.AttachChild ([], Reflected, leaf)) m0.construction
            let c2 = ConstructionPage.update (ConstructionPage.AttachChild ([ Reflected ], Reflected, leaf)) c1
            let c3 = ConstructionPage.update (ConstructionPage.SelectNode [ Reflected ]) c2
            let pending = ConstructionPage.update (ConstructionPage.RequestDeleteNode [ Reflected ]) c3

            // The gate is open (live descendant count) and an undo snapshot exists.
            Assert.True((ConstructionPage.confirmationPrompt pending).IsSome, "gate must be open")
            Assert.True(ConstructionPage.canUndo pending, "an undo snapshot must exist after the attaches")

            let captured = List<ConstructionPage.Msg>()
            let window = Window()
            window.Content <- Component(fun _ctx -> ConstructionView.stackPanel pending captured.Add)
            window.Show()
            Dispatcher.UIThread.RunJobs()

            // Same-row Confirm/Cancel CTAs (commitment 5) + the Undo affordance render.
            Assert.True((buttonByContent window "Confirm").IsSome, "Confirm CTA must render")
            Assert.True((buttonByContent window "Cancel").IsSome, "Cancel CTA must render")
            Assert.True((buttonByContent window "Undo").IsSome, "Undo affordance must render")

            // Confirm dispatches ConfirmDeleteNode (the view reuses the frozen update).
            (buttonByContent window "Confirm").Value.RaiseEvent(RoutedEventArgs(Button.ClickEvent))
            Dispatcher.UIThread.RunJobs()
            Assert.Contains(captured, fun m -> match m with | ConstructionPage.ConfirmDeleteNode -> true | _ -> false)
            window.Close()

            // The frozen update removes the sub-tree and leaves single-level undo (AC-U2.2).
            let removed = ConstructionPage.update ConstructionPage.ConfirmDeleteNode pending
            Assert.True((ConstructionPage.tryGetNode [ Reflected ] removed.project.beamTree.root).IsNone, "sub-tree must be removed")
            Assert.True(ConstructionPage.canUndo removed, "undo must remain available after delete"))
