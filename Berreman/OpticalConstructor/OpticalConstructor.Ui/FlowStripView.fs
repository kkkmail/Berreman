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

/// The strip over the constructor window's model, top to bottom in `ConstructorFlowStep.all`'s order, each box sending
/// `ShowFlowStep`; the shown step is the chosen-option look of the Items panel (spec 0050 S7.2, step 241).
let flowStripView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let stepBox (step : ConstructorFlowStep) : IView =
        let show () = dispatch (ShowFlowStep step)
        if step = model.flowStep then
            MaterialsWindowView.actionButton step.flowUiId step.caption MaterialsWindowView.positiveBackground show
        else
            MaterialsWindowView.verbButton step.flowUiId step.caption show
    Border.create [
        ConstructorUiId.flowStrip.automationAttribute<Border> ()
        Border.margin (Thickness(8.0, 4.0, 0.0, 4.0))
        Border.child (
            StackPanel.create [
                StackPanel.orientation Orientation.Vertical
                StackPanel.children (ConstructorFlowStep.all |> List.map stepBox)
            ])
    ] :> IView
