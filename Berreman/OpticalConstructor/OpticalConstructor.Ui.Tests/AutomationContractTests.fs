namespace OpticalConstructor.Ui.Tests

open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open Xunit

open OpticalConstructor.Controls
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryBackup
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Tests
open OpticalConstructor.Ui
open OpticalConstructor.Ui.TableAndElementRotationView
open OpticalConstructor.Ui.PageHelpTexts

/// The automation contract on the main windows (spec 0051 `.manual/003` 1.4). `ScrollExtentTests` applies the same
/// contract to every window it builds; these facts keep the main ones in the fast `ui-smoke` set.
module AutomationContractTests =

    let private breachesOf (makeWindow : unit -> Window) : string list =
        let mutable breaches : string list = []
        HeadlessSession.run (fun () ->
            let window = makeWindow ()
            window.Width <- 1200.0
            window.Height <- 900.0
            window.Show()
            Dispatcher.UIThread.RunJobs()
            breaches <- try AutomationContract.violations window finally window.Close())
        breaches

    let private assertMeetsContract (name : string) (makeWindow : unit -> Window) : unit =
        let breaches = breachesOf makeWindow
        let firstFew = breaches |> List.truncate 12 |> String.concat "; "
        Assert.True(List.isEmpty breaches, $"%s{name}: %d{List.length breaches} breaches — %s{firstFew}")

    /// A host window over one view.
    let private hostOf (view : Avalonia.FuncUI.Types.IView) : Window =
        let window = HostWindow()
        Program.mkSimple (fun () -> ()) (fun () () -> ()) (fun () _ -> view) |> Program.withHost window |> Program.run
        window :> Window

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the contract refuses an id on a border UI Automation does not expose`` () : unit =
        let breaches =
            breachesOf (fun () ->
                let border = Border()
                AutomationProperties.SetAutomationId(border, "PressableBox")
                Window(Content = border))
        Assert.Contains(breaches, fun breach -> breach.StartsWith "PressableBox: an id on a Border")

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a command button, a choice and an exposed group meet the contract`` () : unit =
        assertMeetsContract "commands" (fun () ->
            let group = ConstructorUiId "ContractGroup"
            hostOf (
                StackPanel.create [
                    yield! group.groupAttributes<StackPanel> ()
                    StackPanel.children [
                        CommandControls.commandButton (ConstructorUiId "ContractCommand") (ControlCaption "Do it") PositiveCommand ControlOffered ignore []
                        CommandControls.choiceToggle (ConstructorUiId "ContractChoice") (ControlCaption "Pick me") ChosenChoice ChosenCommand ControlOffered ignore []
                    ]
                ]))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Main window meets the contract in every bay of a setup's Edit and on the Experiments page`` () : unit =
        for bay in BayNames.allBayNames do
            assertMeetsContract $"Main/%s{bay.value}" (fun () ->
                ScrollExtentTests.constructorWindow
                    ({ ScrollExtentTests.editingSetup () with selection = ElementSelected (SceneElementIndex 0) } |> updateTableAndElementRotation (SelectBay bay)))
        assertMeetsContract "Main/Experiments (chosen)" (fun () -> ScrollExtentTests.constructorWindow (ScrollExtentTests.filledExperiments ()))
        assertMeetsContract "Main/Experiments (creating)" (fun () -> ScrollExtentTests.constructorWindow (ScrollExtentTests.creatingExperiment ()))

    /// Spec 0051 `.manual/007` 1.6: every page of either constructor meets the contract and carries its Help panel.
    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``every page of both constructors meets the contract and carries its help`` () : unit =
        let materials, samples, categories, library = ScrollExtentTests.stores ()
        let forward = ScrollExtentTests.filledExperiments ()
        let inverse =
            {
                SceneTestFixture.inverseScene library (OpticalConstructor.Domain.Experiments.createInMemoryExperiments ()) materials samples categories with
                    kitSetups = forward.kitSetups
                    activeSetupOpt = forward.activeSetupOpt
                    elements = forward.elements
                    experimentCollection = forward.experimentCollection
            }
        for model, modeName in [ forward, "Main"; inverse, "Inverse" ] do
            for step in ConstructorFlowStep.stepsOf model.constructorMode do
                let onPage = model |> updateTableAndElementRotation (ShowFlowStep step)
                assertMeetsContract $"{modeName}/{step.caption.value}" (fun () -> ScrollExtentTests.constructorWindow onPage)
                HeadlessSession.run (fun () ->
                    let window = ScrollExtentTests.constructorWindow onPage
                    window.Show()
                    Dispatcher.UIThread.RunJobs()
                    try
                        let help = ConstructorUiId.pageHelpPanel step.caption.uiKey
                        match UiDriver.tryFindControl window help.value with
                        | Some _ -> ()
                        | None -> Assert.Fail $"{step.caption.value} must carry {help.value}"
                        Assert.NotEmpty(step.help.paragraphs)
                    finally window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the kit window meets the contract with its bar at its tallest and on its Items step`` () : unit =
        assertMeetsContract "KitWindow" (fun () -> ScrollExtentTests.viewWindow KitWindowView.kitWindowView (ScrollExtentTests.tallestKitBar ()))
        assertMeetsContract "KitWindowItems" ScrollExtentTests.itemsStepKitWindow

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Library and Materials windows meet the contract`` () : unit =
        let materials, samples, categories, library = ScrollExtentTests.stores ()
        assertMeetsContract "Library" (fun () ->
            LibraryWindow(library, samples, SourceProxy.createInMemory (), materials, categories, LibraryBackupProxy.createInMemory (),
                CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy))
        assertMeetsContract "Materials" (fun () ->
            MaterialsWindow(materials, categories, CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy))
