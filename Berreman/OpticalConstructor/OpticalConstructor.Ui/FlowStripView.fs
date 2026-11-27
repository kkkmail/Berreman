/// The constructor's flow strip: one box per step, each reachable directly, the window's step filled (spec 0050 S7.2;
/// 001 §O).
module OpticalConstructor.Ui.FlowStripView

open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Controls
open OpticalConstructor.Ui.TableAndElementRotationView

/// The strip over the constructor window's model, top to bottom in its constructor's steps (`ConstructorFlowStep.stepsOf`),
/// each box sending `ShowFlowStep`; the shown step is the chosen-option look of the Items panel (spec 0050 S7.2, step
/// 241; S9.3).
let flowStripView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let stepBox (step : ConstructorFlowStep) : IView =
        let show () = dispatch (ShowFlowStep step)
        CommandControls.choiceCommand step.flowUiId step.caption (if step = model.flowStep then ChosenChoice else UnchosenChoice) show
    Border.create [
        yield! ConstructorUiId.flowStrip.groupAttributes<Border> ()
        Border.margin (Thickness(8.0, 4.0, 0.0, 4.0))
        Border.child (
            StackPanel.create [
                StackPanel.orientation Orientation.Vertical
                StackPanel.children (ConstructorFlowStep.stepsOf model.constructorMode |> List.map stepBox)
            ])
    ] :> IView
