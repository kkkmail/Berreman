namespace OpticalConstructor.Ui.Tests

open Avalonia.Controls
open Avalonia.Input
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open OpticalConstructor.Controls
open OpticalConstructor.TestWindows

/// Tests for the shared "position-on-the-ray" control bar (Spec 0027, task 014): the pure step / parse
/// contract, plus a headless proof that the element-movement screen actually mounts the SHARED control
/// (by its stable automation ids) rather than a bespoke one — i.e. the standardization took.
module RayPositionControlsTests =

    [<Fact>]
    let ``the along-ray step is 0.05 m, or a larger 0.20 m with Shift`` () =
        Assert.Equal(BarMeters 0.05, RayPositionControls.stepMeters KeyModifiers.None)
        Assert.Equal(BarMeters 0.20, RayPositionControls.stepMeters KeyModifiers.Shift)

    [<Fact>]
    let ``the metres parser reads invariant-culture decimals and rejects junk`` () =
        Assert.Equal(Some (BarMeters 1.5), BarMeters.tryCreate "1.5")
        Assert.Equal(Some (BarMeters -0.25), BarMeters.tryCreate "-0.25")
        Assert.Equal(None, BarMeters.tryCreate "abc")

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the element-movement screen mounts the shared ray-position control`` () =
        HeadlessSession.run (fun () ->
            let mutable model = ElementMovementView.initElementMovement ()
            let dispatch (m : ElementMovementView.ElementMovementMsg) = model <- ElementMovementView.updateElementMovement m model
            let window = Window(Width = 820.0, Height = 660.0)
            window.Content <- Component(fun _ -> ElementMovementView.elementMovementView model dispatch)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // The clickable boxes carry an `AutomationProperties.AutomationId` (spec 0038 sweep); the
            // text field keeps its `Name` — collect BOTH id kinds and assert against the union.
            let ids =
                window.GetVisualDescendants()
                |> Seq.collect (fun v ->
                    match v with
                    | :? Control as c ->
                        seq {
                            if not (isNull c.Name) then yield c.Name
                            let autoId = Avalonia.Automation.AutomationProperties.GetAutomationId c
                            if not (isNull autoId) then yield autoId
                        }
                    | _ -> Seq.empty)
                |> Set.ofSeq
            window.Close()
            Assert.Contains(ConstructorUiId.rayPositionMinusButton.value, ids)
            Assert.Contains(ConstructorUiId.rayPositionPlusButton.value, ids)
            Assert.Contains(ConstructorUiId.rayPositionField.value, ids)
            Assert.Contains(ConstructorUiId.rayPositionResetButton.value, ids))
