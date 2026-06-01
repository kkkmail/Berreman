/// Read-only stack-panel view (spec 0024 Part U1 §U1.5 / R-5). Renders the selected
/// node's layer rows from the Avalonia-free presentation helper
/// `StackEditor.layerRowLabels` (`StackEditor.fs:217`) with thickness strings from
/// `StackEditor.displayThickness` (`StackEditor.fs:210`), proving the MVU read path
/// end-to-end. U1 wires NO edit dispatch — the edit path (`EditStack`) lands in
/// slice 003 / Part U2; this view takes only the `ConstructionPage.Model` it reads.
///
/// New sibling `*View.fs` module per §0.1: it adds view code without editing any
/// frozen Avalonia-free logic module. Authored against the public MIT
/// `Avalonia.FuncUI` 1.6.0 DSL surface (§0.2) — no clone reference.
module OpticalConstructor.Ui.ConstructionView

open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Domain.BeamTree

/// The selected node, falling back to the tree root when the selected path no longer
/// resolves (e.g. after a deletion) so the read path is total.
let private selectedNode (model : ConstructionPage.Model) : BeamNode =
    match ConstructionPage.tryGetNode model.selected model.project.beamTree.root with
    | Some node -> node
    | None -> model.project.beamTree.root

/// The read-only `stack` panel (R-5 / AC-U1.2): the selected node's layer rows, each
/// a thickness-labelled `TextBlock` from `StackEditor.layerRowLabels` rendered in the
/// node's default unit. An empty stack renders a placeholder note rather than nothing,
/// so the panel always renders one frame without throwing (AC-U1.3).
let stackPanel (model : ConstructionPage.Model) : IView =
    let node = selectedNode model
    let labels = StackEditor.layerRowLabels node.defaultUnit node.system
    let header = TextBlock.create [ TextBlock.text "Stack"; TextBlock.fontWeight FontWeight.Bold ] :> IView
    let rows =
        match labels with
        | [] -> [ TextBlock.create [ TextBlock.text "(no layers)" ] :> IView ]
        | _ -> labels |> List.map (fun l -> TextBlock.create [ TextBlock.text l ] :> IView)
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.children (header :: rows)
    ] :> IView
