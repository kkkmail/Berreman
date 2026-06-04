/// Editable stack-panel view (spec 0024 Part U2 §U2.1–U2.3 / R-1..R-3). U1 (slice
/// 001) created this module for the read-only path; this slice adds the edit-dispatch
/// path plus the per-node busy indicator, the delete-confirmation gate, and
/// single-level undo.
///
/// Every stack mutation lives in the frozen, Avalonia-free
/// `StackEditor`/`ConstructionPage` modules (§0.1): this view re-implements NO
/// mutation. It dispatches each `StackEditor.StackMsg` as
/// `ConstructionPage.EditStack (path, stackMsg)` and the destructive-edit /
/// undo actions as the matching `ConstructionPage.Msg`, routed through the frozen
/// `ConstructionPage.update` (R-1 / Non-requirements). Like the slice-002
/// `ChartView.chartPanel`, it takes the `ConstructionPage.Model` it renders plus a
/// `ConstructionPage.Msg -> unit` dispatch (NOT `RootModel`, which lives in
/// `Shell.fs`), so it composes under the root without a module cycle.
///
/// New sibling `*View.fs` module per §0.1; authored against the public MIT
/// `Avalonia.FuncUI` 1.6.0 DSL surface (§0.2) — no clone reference.
module OpticalConstructor.Ui.ConstructionView

open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open OpticalConstructor.Domain.BeamTree

/// The selected node, falling back to the tree root when the selected path no longer
/// resolves (e.g. after a deletion) so the view is total.
let private selectedNode (model : ConstructionPage.Model) : BeamNode =
    match ConstructionPage.tryGetNode model.selected model.project.beamTree.root with
    | Some node -> node
    | None -> model.project.beamTree.root

/// Distinct positive / negative CTA brushes for the destructive-edit gate
/// (UX commitment 5: same-row Confirm/Cancel with distinct positive/negative colours).
let private positiveCta : IBrush = Brushes.SeaGreen :> IBrush
let private negativeCta : IBrush = Brushes.IndianRed :> IBrush

/// A button dispatching one stack edit through `EditStack` (R-1). The view
/// re-implements NO stack mutation — every edit routes the frozen
/// `ConstructionPage.update`, which applies `StackEditor.applyStackMsg`.
let private editButton
    (dispatch : ConstructionPage.Msg -> unit)
    (path : ConstructionPage.NodePath)
    (label : string)
    (sm : StackEditor.StackMsg)
    : IView =
    Button.create [
        Button.content label
        Button.onClick (fun _ -> dispatch (ConstructionPage.EditStack (path, sm)))
    ] :> IView

/// Stack-level edit toolbar: add a layer, group all layers, set the incident / exit
/// medium, and the three-state substrate switch — one control per `StackMsg` that is
/// not layer-indexed (R-1).
let private stackToolbar
    (dispatch : ConstructionPage.Msg -> unit)
    (path : ConstructionPage.NodePath)
    (node : BeamNode)
    : IView =
    let allIndices = [ 0 .. List.length node.system.films - 1 ]
    let wedge = StackEditor.AsWedge { layer = StackEditor.defaultNewLayer; angle = WedgeAngle.defaultValue }
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 4.0
        StackPanel.children [
            editButton dispatch path "Add layer" (StackEditor.AddLayer StackEditor.defaultNewLayer)
            editButton dispatch path "Group all" (StackEditor.GroupLayers allIndices)
            editButton dispatch path "Incident: vacuum" (StackEditor.SetIncidentMedium OpticalProperties.vacuum)
            editButton dispatch path "Exit: vacuum" (StackEditor.SetExitMedium OpticalProperties.vacuum)
            editButton dispatch path "Substrate: film" (StackEditor.SetSubstrate StackEditor.AsThinFilm)
            editButton dispatch path "Substrate: plate" (StackEditor.SetSubstrate (StackEditor.AsPlate StackEditor.defaultNewLayer))
            editButton dispatch path "Substrate: wedge" (StackEditor.SetSubstrate wedge)
        ]
    ] :> IView

/// One layer row: its thickness label plus the per-layer edits (delete, duplicate,
/// reorder, rotate, set material) — each routed through `EditStack` (R-1).
let private layerRow
    (dispatch : ConstructionPage.Msg -> unit)
    (path : ConstructionPage.NodePath)
    (u : OpticalConstructor.Domain.Units.UnitOfMeasure)
    (index : int)
    (layer : Layer)
    : IView =
    let label = sprintf "Layer %d — %s" index (StackEditor.displayThickness u layer.thickness)
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 4.0
        StackPanel.children [
            TextBlock.create [ TextBlock.text label; TextBlock.verticalAlignment VerticalAlignment.Center ]
            editButton dispatch path "Delete" (StackEditor.DeleteLayer index)
            editButton dispatch path "Duplicate" (StackEditor.DuplicateLayer index)
            editButton dispatch path "Up" (StackEditor.ReorderLayer (index, max 0 (index - 1)))
            editButton dispatch path "Down" (StackEditor.ReorderLayer (index, index + 1))
            editButton dispatch path "Rotate" (StackEditor.RotateLayer (index, Rotation.rotatePiX))
            editButton dispatch path "Material: vacuum" (StackEditor.SetLayerMaterial (index, OpticalProperties.vacuum))
        ]
    ] :> IView

/// Node-level structural controls (R-3): the delete-confirmation gate and undo. When
/// a sub-tree deletion is pending, the `confirmationPrompt` text shows above same-row
/// Confirm/Cancel buttons with distinct positive/negative CTAs (commitment 5);
/// otherwise a `Delete node` entry routes `RequestDeleteNode`. The single-level Undo
/// is shown only while `canUndo` holds.
let private nodeControls (model : ConstructionPage.Model) (dispatch : ConstructionPage.Msg -> unit) : IView =
    let gateOrDelete : IView list =
        match ConstructionPage.confirmationPrompt model with
        | Some prompt ->
            [ TextBlock.create [ TextBlock.text prompt; TextBlock.verticalAlignment VerticalAlignment.Center ] :> IView
              StackPanel.create [
                  StackPanel.orientation Orientation.Horizontal
                  StackPanel.spacing 12.0
                  StackPanel.children [
                      Button.create [
                          Button.content "Confirm"
                          Button.background positiveCta
                          Button.onClick (fun _ -> dispatch ConstructionPage.ConfirmDeleteNode)
                      ]
                      Button.create [
                          Button.content "Cancel"
                          Button.background negativeCta
                          Button.onClick (fun _ -> dispatch ConstructionPage.CancelDeleteNode)
                      ]
                  ]
              ] :> IView ]
        | None ->
            [ Button.create [
                  Button.content "Delete node"
                  Button.onClick (fun _ -> dispatch (ConstructionPage.RequestDeleteNode model.selected))
              ] :> IView ]
    let undo : IView list =
        if ConstructionPage.canUndo model then
            [ Button.create [
                  Button.content "Undo"
                  Button.onClick (fun _ -> dispatch ConstructionPage.Undo)
              ] :> IView ]
        else []
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 8.0
        StackPanel.children (gateOrDelete @ undo)
    ] :> IView

/// The editable `stack` panel (R-1/R-2/R-3 / AC-U2.1 / AC-U2.2): the selected node's
/// layer rows with per-layer + stack-level edit controls, a per-node busy indicator
/// driven by `ConstructionPage.isNodeBusy` (§U2.2), and the delete-confirmation gate +
/// single-level undo. An empty stack still renders a placeholder note so the panel
/// always lays out one frame without throwing.
let stackPanel (model : ConstructionPage.Model) (dispatch : ConstructionPage.Msg -> unit) : IView =
    let node = selectedNode model
    let path = model.selected
    let busy = ConstructionPage.isNodeBusy path model
    let headerText = if busy then "Stack (solving…)" else "Stack"
    let header = TextBlock.create [ TextBlock.text headerText; TextBlock.fontWeight FontWeight.Bold ] :> IView
    let rows =
        match node.system.films with
        | [] -> [ TextBlock.create [ TextBlock.text "(no layers)" ] :> IView ]
        | films -> films |> List.mapi (fun i l -> layerRow dispatch path node.defaultUnit i l)
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 4.0
        StackPanel.children (
            [ header; stackToolbar dispatch path node ]
            @ rows
            @ [ nodeControls model dispatch ])
    ] :> IView
