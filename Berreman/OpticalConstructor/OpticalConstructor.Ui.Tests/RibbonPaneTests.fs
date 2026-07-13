namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open Xunit
open OpticalConstructor.Controls

/// Spec 0035 (007): the generic Controls ribbon hosts ONLY the active bay's content in ONE
/// keyed slot (`Ribbon.view`), instead of rendering every bay's pane with `IsVisible` toggles.
/// The slot is keyed by the active bay name (`View.withKey` → FuncUI's `IView.ViewKey`), so
/// switching bays makes FuncUI CREATE a fresh pane rather than recycle a styled, named control
/// across two DIFFERENT bays ("Cannot set Name : styled element already styled"). These headless
/// tests drive the REAL FuncUI patch path over a live Elmish loop: selecting each bay in turn
/// realizes exactly that bay's content and no other pane, with no styled-element recycling error.
module RibbonPaneTests =

    /// A bay's content is a single, uniquely-named marker control, so "exactly this bay's content
    /// is realized" reduces to a presence check on the marker's `Name`.
    let private marker (bayName : string) : string = "BayBody_" + bayName

    /// Four bays whose contents are all the SAME view type (TextBlock) with DIFFERENT names — the
    /// exact shape that recycles a styled, named control if a single content slot is not keyed. All are
    /// `InRibbonPane` bays (their content docks in the ribbon's keyed pane), so "exactly this bay's pane
    /// is realized" is the property under test; a `FullSurface` bay would render no in-ribbon pane at all.
    let private bayNames : string list = [ "Rotation"; "Move"; "Render"; "Materials" ]

    let private bays : Ribbon.Bay list =
        bayNames
        |> List.map (fun n ->
            ({ name = n
               content = TextBlock.create [ TextBlock.name (marker n); TextBlock.text n ] :> IView
               mode = Ribbon.InRibbonPane } : Ribbon.Bay))

    /// Every non-empty control `Name` realized anywhere in the window's visual tree.
    let private realizedNames (window : Window) : Set<string> =
        window.GetVisualDescendants()
        |> Seq.choose (function :? Control as c when not (isNull c.Name) -> Some c.Name | _ -> None)
        |> Set.ofSeq

    /// Click the centre of the (always-present, visible) ribbon tab for `bayName`, driving the live
    /// Elmish loop so the view re-renders in the same pass — the patch path that would recycle a
    /// styled control if the content slot were not keyed.
    let private clickTab (window : Window) (bayName : string) : unit =
        let id = UiIds.Ribbon.tab bayName
        let found =
            window.GetVisualDescendants()
            |> Seq.tryPick (function :? Control as c when c.Name = id && c.IsEffectivelyVisible -> Some c | _ -> None)
        match found with
        | None -> Assert.Fail($"ribbon tab %s{bayName} was not found (or not visible)")
        | Some b ->
            let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
            if c.HasValue then
                window.MouseDown(c.Value, MouseButton.Left, RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                window.MouseUp(c.Value, MouseButton.Left, RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
            else Assert.Fail($"ribbon tab %s{bayName} has no on-screen position")

    /// Mount the generic ribbon over a live Elmish loop whose model IS the selected bay name (a tab
    /// click dispatches `SelectBay` = the new name), so re-renders exercise the real FuncUI patch.
    let private mountRibbon (initial : string) : HostWindow =
        let window = HostWindow(Width = 640.0, Height = 480.0)
        let update (name : string) (_ : string) : string = name
        let view (selected : string) (dispatch : string -> unit) : IView =
            Ribbon.view ({ bays = bays; selected = selected } : Ribbon.State) dispatch
        Program.mkSimple (fun () -> initial) update view
        |> Program.withHost window
        |> Program.run
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window

    /// Assert that exactly `bayName`'s pane slot + content is realized, and no OTHER bay's pane or
    /// content is, while every bay's TAB stays present (the tab strip is unchanged).
    let private assertOnlyBayRealized (window : Window) (bayName : string) : unit =
        let names = realizedNames window
        Assert.Contains(UiIds.Ribbon.pane bayName, names)
        Assert.Contains(marker bayName, names)
        for other in bayNames do
            if other <> bayName then
                Assert.DoesNotContain(UiIds.Ribbon.pane other, names)
                Assert.DoesNotContain(marker other, names)
        for b in bayNames do
            Assert.Contains(UiIds.Ribbon.tab b, names)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the ribbon realizes only the initially-selected bay's content in one keyed slot`` () =
        HeadlessSession.run (fun () ->
            let window = mountRibbon "Render"
            assertOnlyBayRealized window "Render"
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``selecting each bay in turn shows exactly that bay's content and no other pane, with no recycling error`` () =
        HeadlessSession.run (fun () ->
            let window = mountRibbon "Rotation"
            // Walk every bay (including back to the first) over the live patch path: each switch
            // recreates the keyed slot, so it must never throw the styled-element recycling error.
            for bayName in bayNames @ [ "Rotation" ] do
                clickTab window bayName
                Assert.True(window.IsVisible, $"selecting %s{bayName} must render without throwing")
                assertOnlyBayRealized window bayName
            window.Close())
