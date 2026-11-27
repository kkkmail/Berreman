namespace OpticalConstructor.Desktop.Tests

open System
open System.Diagnostics
open System.IO
open System.Management
open FlaUI.Core.AutomationElements
open FlaUI.Core.Definitions
open Xunit

open OpticalConstructor.Controls
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Ui
open OpticalConstructor.Ui.ConstructorUiIdOfDomain
open OpticalConstructor.Ui.TableAndElementRotationView
open OpticalConstructor.Desktop.Tests.DesktopSession

/// The real app on the real desktop, found and driven by its ids alone (spec 0051 `.manual/003` 1.6). Every fact skips
/// in session 0, so the arc's gates never run it; the operator runs it with `--filter-trait "Category=desktop"`.
[<Collection(DesktopSession.RealDesktop)>]
module DesktopTests =

    /// Asserts `element` is a control UI Automation can drive: a button, a toggle or an edit.
    let private assertDrivable (id : ConstructorUiId) (element : AutomationElement) : unit =
        Assert.True(
            element.Patterns.Invoke.IsSupported || element.Patterns.Toggle.IsSupported || element.Patterns.Value.IsSupported,
            $"{id.value} is a {element.ControlType} without an Invoke, Toggle or Value pattern")

    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``the Launcher's buttons are found and invoked by id`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "launcher"
        use app = launch sandbox
        let launcher = app.window "Launcher"
        for id in [ ConstructorUiId.openMainButton; ConstructorUiId.openInverseButton; ConstructorUiId.openMaterialsButton; ConstructorUiId.openLibraryButton; ConstructorUiId.launcherDataLocationsLink ] do
            assertDrivable id (app.find launcher id)
        app.invoke launcher ConstructorUiId.launcherDataLocationsLink
        let locations = app.window "Data locations"
        let openFolders = locations.FindAllDescendants(condition (fun factory -> factory.ByControlType(ControlType.Button))) |> Array.filter (fun (button : AutomationElement) -> button.Name = "Open folder")
        Assert.Equal(8, openFolders.Length)

    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``a kit opens through the real folder dialog and every step and bay is driven by id`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "kit"
        use app = launch sandbox
        let kitWindow = openKit app ConstructorUiId.openMainButton sandbox.opticsMueller
        let kitBar =
            [ ConstructorUiId.sendKitButton; ConstructorUiId.kitShowInExplorerButton; ConstructorUiId.kitCopyExperimentsButton
              ConstructorUiId.kitReloadChangesButton; ConstructorUiId.kitCloseButton; ConstructorUiId.kitSwitchModeButton
              ConstructorUiId.saveNamingTemplateButton; ConstructorUiId.namingTemplateField ]
        for id in kitBar do
            assertDrivable id (app.find kitWindow id)
        let walkSteps (mode : ConstructorMode) : unit =
            for step in ConstructorFlowStep.stepsOf mode do
                app.invoke kitWindow step.flowUiId
                let shown = app.find kitWindow step.flowUiId
                Assert.Equal(ToggleState.On, shown.Patterns.Toggle.Pattern.ToggleState.Value)
        walkSteps ForwardConstructor
        app.invoke kitWindow ConstructorFlowStep.ExperimentsFlowStep.flowUiId
        for bay in BayNames.allBayNames do
            let tab = ConstructorUiId.ribbonTab bay.uiKey
            app.invoke kitWindow tab
            Assert.Equal(ToggleState.On, (app.find kitWindow tab).Patterns.Toggle.Pattern.ToggleState.Value)
        app.invoke kitWindow ConstructorUiId.kitSwitchModeButton
        walkSteps InverseConstructor

    /// The account that owns the desktop's shell, so an Explorer window it opens is visible to this account.
    let private shellOwnedByThisAccount () : bool =
        use searcher = new ManagementObjectSearcher($"SELECT Handle, ProcessId FROM Win32_Process WHERE Name = 'explorer.exe' AND SessionId = {Process.GetCurrentProcess().SessionId}")
        searcher.Get()
        |> Seq.cast<ManagementObject>
        |> Seq.exists (fun shell ->
            let owner = shell.InvokeMethod("GetOwner", null, null)
            string owner.["User"] = Environment.UserName)

    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``Show in Explorer opens the kit folder in an Explorer window`` () : unit =
        requireDesktop ()
        if not (shellOwnedByThisAccount ()) then
            Assert.Skip "The desktop's shell belongs to another account, whose Explorer this account cannot open (spec 0051 .manual/002)."
        let sandbox = createSandbox "explorer"
        use app = launch sandbox
        let kitWindow = openKit app ConstructorUiId.openMainButton sandbox.opticsMueller
        app.invoke kitWindow ConstructorUiId.kitShowInExplorerButton
        let explorer =
            waitFor "an Explorer window on the kit folder" 20.0 (fun () ->
                app.automation.GetDesktop().FindAllChildren()
                |> Array.tryFind (fun window -> window.ClassName = "CabinetWClass" && window.Name.Contains "optics-mueller"))
        explorer.AsWindow().Close()

    // ======================== spec 0051 `.manual/003` Phase 6: what only the real desktop shows ========================

    /// The PE subsystem of `exe`: 2 for a windowed program, 3 for a console one.
    let private subsystemOf (exe : string) : int =
        let bytes = File.ReadAllBytes exe
        let header = BitConverter.ToInt32(bytes, 0x3C)
        int (BitConverter.ToUInt16(bytes, header + 24 + 68))

    /// Spec 0051 `.manual/003` 5.7, 5.2 (D5).
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``the app is a windowed program that logs to its Preferences location`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "windowed"
        use app = launch sandbox
        app.window "Launcher" |> ignore
        Assert.Equal(2, subsystemOf (Path.Combine(sandbox.app, "Softellect.Berreman.OpticalConstructor.App.exe")))
        let log = Path.Combine(sandbox.root, "data", "Preferences", "optical-constructor.log")
        waitFor "the log file" 20.0 (fun () -> if File.Exists log then Some () else None)
        for folder in [ "Exports"; "Catalogues" ] do
            Assert.True(Directory.Exists(Path.Combine(sandbox.root, "data", folder)), $"{folder} must exist after the first start")

    /// Spec 0051 `.manual/003` 5.2.
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``each native dialog starts in its own folder and says what it asks for`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "dialogs"
        use app = launch sandbox
        let launcher = app.window "Launcher"
        // New kit… asks for a kit folder by its title.
        app.invoke launcher ConstructorUiId.openMainButton
        let start = app.window "Open a kit"
        app.invoke start ConstructorUiId.newKitButton
        Assert.Contains("Choose a kit folder", app.nativeDialog.Name)
        app.cancelNativeDialog ()
        start.Close()
        // Register catalogue folder… starts in the Catalogues location.
        app.invoke launcher ConstructorUiId.openLibraryButton
        let library = app.window "Library"
        app.invoke library ConstructorUiId.libraryRegisterCatalogueFolderButton
        Assert.EndsWith("Catalogues", trimmed app.nativeDialogFolder)
        app.cancelNativeDialog ()
        library.Close()
        // Attach data file… starts in the kit's data folder and lists lab data.
        let kitWindow = openKit app ConstructorUiId.openInverseButton sandbox.opticsMueller
        app.invoke kitWindow ConstructorUiId.flowDataStep
        app.invoke kitWindow (ConstructorUiId.attachDataFile (ExperimentId 1))
        Assert.EndsWith(Path.Combine("optics-mueller", "data"), trimmed app.nativeDialogFolder)
        let filters = app.nativeDialog.FindAllDescendants(condition (fun factory -> factory.ByControlType(ControlType.ComboBox)))
        Assert.Contains(filters, fun (combo : AutomationElement) -> (combo.Name + " " + shownText combo).Contains "Lab data")
        app.cancelNativeDialog ()

    /// Spec 0051 `.manual/003` 3.1, 3.2.
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``a kit reopens on the setup it last had`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "reopen"
        use app = launch sandbox
        let first = openKit app ConstructorUiId.openMainButton sandbox.opticsMueller
        let chosen = SetupName "CPL-(LR#0)-CPL"
        app.invoke first (ConstructorUiId.savedSetup chosen)
        Assert.Equal(ToggleState.On, (app.find first (ConstructorUiId.savedSetup chosen)).Patterns.Toggle.Pattern.ToggleState.Value)
        Assert.StartsWith($"Active setup: {chosen.value}", shownText (app.find first ConstructorUiId.activeSetupText))
        app.invoke first ConstructorUiId.kitCloseButton
        // The Launcher's title also reads "Optical Constructor —": the kit window is the one naming the kit's folder.
        waitFor "the kit window to close" 15.0 (fun () ->
            if app.windows |> List.exists (fun window -> window.Title.Contains(Path.GetFileName sandbox.opticsMueller)) then None else Some ())
        let reopened = openKit app ConstructorUiId.openMainButton sandbox.opticsMueller
        Assert.StartsWith($"Active setup: {chosen.value}", shownText (app.find reopened ConstructorUiId.activeSetupText))

    /// Spec 0051 `.manual/003` 2.3, 3.6, 5.8.
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``Remove asks first, the element list selects, and a chart export is saved where the user says`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "actions"
        use app = launch sandbox
        let kitWindow = openKit app ConstructorUiId.openMainButton sandbox.opticsMueller
        // The Select bay's list selects an element without the canvas.
        app.invoke kitWindow (ConstructorUiId.ribbonTab BayNames.selector.uiKey)
        let list = app.find kitWindow ConstructorUiId.tableElementList
        let items = list.FindAllDescendants(condition (fun factory -> factory.ByControlType(ControlType.ListItem)))
        Assert.NotEmpty(items)
        let last = items.[items.Length - 1]
        last.Patterns.SelectionItem.Pattern.Select()
        waitFor "the readout to name the selected slot" 10.0 (fun () ->
            if (shownText (app.find kitWindow ConstructorUiId.teReadout)).Contains last.Name then Some () else None)
        // Remove asks before anything goes; No keeps the experiment and its file.
        let filesBefore = Directory.GetFiles(sandbox.opticsMueller, "*.experiment-json").Length
        app.invoke kitWindow ConstructorUiId.flowExperimentsStep
        let first = ConstructorUiKey "1"
        app.invoke kitWindow (ConstructorUiId.experimentRemoveButton first)
        Assert.Equal("Remove this experiment and its file?", shownText (app.find kitWindow ConstructorUiId.experimentRemovePrompt))
        app.invoke kitWindow ConstructorUiId.experimentRemoveCancelButton
        app.find kitWindow (ConstructorUiId.experimentRemoveButton first) |> ignore
        Assert.Equal(filesBefore, Directory.GetFiles(sandbox.opticsMueller, "*.experiment-json").Length)
        // View opens the chart; Export CSV… asks where to save, starting in Exports, and says what it wrote.
        app.invoke kitWindow (ConstructorUiId.experimentViewButton first)
        // A chart window is titled by its chart: it is the window holding the export buttons.
        let chart =
            waitFor "the chart window" 30.0 (fun () ->
                app.windows |> List.tryFind (fun window -> app.tryFind window ConstructorUiId.chartWindowExportCsv |> Option.isSome))
        app.invoke chart ConstructorUiId.chartWindowExportCsv
        Assert.EndsWith("Exports", trimmed app.nativeDialogFolder)
        let target = Path.Combine(sandbox.root, "chart.csv")
        app.typeIntoNativeDialog target
        try
            waitFor "the exported file" 15.0 (fun () -> if File.Exists target then Some () else None)
        with error ->
            let status = app.tryFind chart ConstructorUiId.chartWindowExportStatus |> Option.map shownText
            let written = Directory.GetFiles(sandbox.root, "*.csv", SearchOption.AllDirectories)
            let dialogs = app.windows |> List.map (fun window -> window.Title)
            let files = String.Join("; ", written)
            failwith $"{error.Message}; status {status}; csv files {files}; windows {dialogs}"
        Assert.Equal($"Saved {target}.", shownText (app.find chart ConstructorUiId.chartWindowExportStatus))

