namespace OpticalConstructor.Ui.Tests

open System.Collections.Generic
open Avalonia
open Avalonia.Automation
open Avalonia.Automation.Peers
open Avalonia.Automation.Provider
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.VisualTree

open OpticalConstructor.Controls

/// The automation contract (spec 0051 `.manual/003` 1.2, 1.4): what UI Automation must be able to find and drive in a
/// window. `violations` lists every breach of one shown window; an empty list is the law.
module AutomationContract =

    /// A control the user presses, types into, picks or toggles. `CheckBox` and `RadioButton` are toggle buttons.
    let private isInputControl (control : Control) : bool =
        match control with
        | :? Button | :? ToggleButton | :? TextBox | :? ComboBox | :? Slider | :? ListBox -> true
        | _ -> false

    /// A control drawn by another control's template (a scroll bar's arrows), which the contract leaves to its owner.
    let private isTemplatePart (control : Control) : bool =
        match control.TemplatedParent with
        | null -> false
        | _ -> true

    let private peerOf (control : Control) : AutomationPeer = ControlAutomationPeer.CreatePeerForElement control

    /// Every control whose peer the window's automation tree reaches, as Windows UI Automation walks it.
    let private reachableOwners (window : Window) : HashSet<Control> =
        let owners = HashSet<Control>(HashIdentity.Reference)
        let rec walk (peer : AutomationPeer) : unit =
            match peer with
            | :? ControlAutomationPeer as controlPeer -> owners.Add controlPeer.Owner |> ignore
            | _ -> ()
            for child in peer.GetChildren() do
                walk child
        walk (peerOf window)
        owners

    /// Every breach of the contract in `window`, which must be shown.
    let violations (window : Window) : string list =
        let shown =
            window.GetVisualDescendants()
            |> Seq.choose (function
                | :? Control as control when control.IsEffectivelyVisible && not (isTemplatePart control) -> Some control
                | _ -> None)
            |> List.ofSeq
        let reachable = reachableOwners window
        let typeName (control : Control) : string = control.GetType().Name
        let breaches =
            [
                for control in shown do
                    let automationId = AutomationProperties.GetAutomationId control
                    let peer = peerOf control
                    match automationId with
                    | null | "" -> ()
                    | id ->
                        if not (peer.IsControlElement()) then
                            yield $"{id}: an id on a {typeName control} UI Automation does not expose"
                    if isInputControl control then
                        let id = UiDriver.idOf control
                        match id with
                        | null | "" -> yield $"a {typeName control} showing '{UiDriver.textOfControl control}' has no id"
                        | _ ->
                            match peer.GetName() with
                            | null | "" -> yield $"{id}: no automation Name"
                            | _ -> ()
                            if control.IsEffectivelyEnabled && not control.Focusable then
                                yield $"{id}: an enabled {typeName control} the keyboard cannot reach"
                            if not (reachable.Contains control) then
                                yield $"{id}: not reachable from the window's automation root"
                    match control with
                    | :? ChoiceToggle as choice ->
                        match box peer with
                        | :? IToggleProvider -> ()
                        | _ -> yield $"{UiDriver.idOf choice}: a choice without a toggle pattern"
                    | _ -> ()
            ]
        let duplicates =
            shown
            |> List.map AutomationProperties.GetAutomationId
            |> List.filter (fun id -> not (System.String.IsNullOrEmpty id))
            |> List.countBy id
            |> List.filter (fun (_, count) -> count > 1)
            |> List.map (fun (id, count) -> $"{id}: {count} controls carry it")
        breaches @ duplicates
