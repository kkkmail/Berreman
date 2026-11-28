namespace OpticalConstructor.Desktop.Tests

open System
open System.Diagnostics
open System.IO
open System.Text.Json
open FlaUI.Core.AutomationElements
open FlaUI.Core.Definitions
open Xunit

open OpticalConstructor.Controls
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Ui
open OpticalConstructor.Ui.ConstructorUiIdOfDomain
open OpticalConstructor.Ui.TableAndElementRotationView
open OpticalConstructor.Desktop.Tests.DesktopSession

/// The fifteen comments of 0051 `.manual/006` re-driven on the real desktop by id, a fact per comment (spec 0051
/// `.manual/007` Phase 10). The report is 0051 `.manual/009`.
[<Collection(DesktopSession.RealDesktop)>]
module DesktopPagesSurveyTests =

    let private automationIdOf (element : AutomationElement) : string =
        element.Properties.AutomationId.ValueOrDefault |> Option.ofObj |> Option.defaultValue ""

    /// The elements under `root` whose id starts with `prefix`, in reading order.
    let private startingWith (root : AutomationElement) (prefix : string) : AutomationElement list =
        root.FindAllDescendants()
        |> Array.filter (fun element -> (automationIdOf element).StartsWith prefix)
        |> Array.sortBy (fun element -> element.BoundingRectangle.Top, element.BoundingRectangle.Left)
        |> List.ofArray

    let private isOn (element : AutomationElement) : bool = element.Patterns.Toggle.Pattern.ToggleState.Value = ToggleState.On

    /// `lower` starts at or under `upper`'s bottom edge.
    let private assertUnder (what : string) (upper : AutomationElement) (lower : AutomationElement) : unit =
        let gap = lower.BoundingRectangle.Top - upper.BoundingRectangle.Bottom
        Assert.True(gap >= -2, $"{what}: it starts {gap} px from the bottom of what it belongs to")

    /// `upper` ends at or above `lower`'s top edge.
    let private assertAbove (what : string) (upper : AutomationElement) (lower : AutomationElement) : unit =
        let gap = lower.BoundingRectangle.Top - upper.BoundingRectangle.Bottom
        Assert.True(gap >= -2, $"{what}: it ends {-gap} px below the next entry's top")

    let private verticalCentre (element : AutomationElement) : int = element.BoundingRectangle.Top + element.BoundingRectangle.Height / 2

    /// The control answering to `id` in any window or popup of the app, a context menu's items included.
    let private findAnywhere (app : DesktopApp) (id : ConstructorUiId) : AutomationElement =
        waitFor $"the control '{id.value}' in any of the app's windows" 15.0 (fun () ->
            app.automation.GetDesktop().FindAllChildren(condition (fun factory -> factory.ByProcessId app.application.ProcessId))
            |> Array.tryPick (fun window ->
                if automationIdOf window = id.value then Some window
                else window.FindFirstDescendant(condition (fun factory -> factory.ByAutomationId id.value)) |> Option.ofObj))

    /// Presses a menu item through its pattern, else at its centre.
    let private press (element : AutomationElement) : unit =
        if element.Patterns.Invoke.IsSupported then element.Patterns.Invoke.Pattern.Invoke() else element.Click()

    let private clipboardFirstLine () : string =
        (clipboardText ()).Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries) |> Array.tryHead |> Option.defaultValue ""

    /// The commonest pixel of `element` on screen: its fill.
    let private fillOf (element : AutomationElement) : int =
        Screen.capture element.BoundingRectangle |> Array.countBy id |> Array.maxBy snd |> fst

    /// Chooses `study` on the Unknowns page unless it is chosen: a press on the chosen study unchooses it.
    let private chooseStudy (app : DesktopApp) (window : Window) (study : StudyName) : unit =
        app.invoke window ConstructorUiId.flowUnknownsStep
        if not (isOn (app.find window (ConstructorUiId.studyChoice study))) then app.invoke window (ConstructorUiId.studyChoice study)

    /// Cancels the shown page's draft, answering Yes when it asks.
    let private cancelDraft (app : DesktopApp) (window : Window) : unit =
        app.invoke window ConstructorUiId.pageCancelButton
        match app.tryFind window ConstructorUiId.pageLeaveYesButton with
        | Some _ -> app.invoke window ConstructorUiId.pageLeaveYesButton
        | None -> ()

    /// Closes the kit from its Kit page and waits for its window to go.
    let private closeKit (app : DesktopApp) (window : Window) (kitFolder : string) : unit =
        app.invoke window ConstructorUiId.flowKitStep
        app.invoke window ConstructorUiId.kitCloseButton
        waitFor "the kit window to close" 15.0 (fun () ->
            if app.windows |> List.exists (fun shown -> shown.Title.Contains(Path.GetFileName kitFolder)) then None else Some ())

    let private qzLr = StudyName "QZ-LR"

    let private inversePages : ConstructorFlowStep list = ConstructorFlowStep.stepsOf InverseConstructor

    /// 006-1 (a–d, l): the page row, the Kit page, the table's tabs and the help (spec 0051 `.manual/007` 1.6, 2.1–2.3).
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``006-1: the page row leads the kit window, the kit's verbs and Publish are the Kit page's, the table's tabs a setup's Edit's, and every page has its help`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "pages-row"
        use app = launch sandbox
        let inverse = openKit app ConstructorUiId.openInverseButton sandbox.opticsMueller
        // 006-1a, 006-1b: one row in this order, the switch named by the other constructor alone.
        let row = app.find inverse ConstructorUiId.pageRow
        let flowIds (steps : ConstructorFlowStep list) : ConstructorUiId list = steps |> List.map (fun step -> step.flowUiId)
        let order =
            flowIds [ ItemsFlowStep; SetupsFlowStep; ExperimentsFlowStep ]
            @ [ ConstructorUiId.kitSwitchModeButton ]
            @ flowIds [ DataFlowStep; UnknownsFlowStep; PlanFlowStep; FitFlowStep; ResultsFlowStep; KitFlowStep ]
        let placed = order |> List.map (app.find row)
        for (first, second) in List.pairwise placed do
            let sameLine = abs (first.BoundingRectangle.Top - second.BoundingRectangle.Top) < 10
            Assert.True(
                (sameLine && first.BoundingRectangle.Left < second.BoundingRectangle.Left) || second.BoundingRectangle.Top > first.BoundingRectangle.Top + 10,
                $"'{first.Name}' must come before '{second.Name}' in the page row")
        Assert.Equal("Main", (app.find row ConstructorUiId.kitSwitchModeButton).Name)
        Assert.DoesNotContain(textsUnder inverse @ (placed |> List.map (fun button -> button.Name)), fun text -> text.StartsWith "Switch to")
        // 006-1c: the kit's verbs and Publish are on the Kit page alone.
        let kitVerbs =
            [ ConstructorUiId.sendKitButton; ConstructorUiId.kitShowInExplorerButton; ConstructorUiId.kitCopyExperimentsButton
              ConstructorUiId.kitReloadChangesButton; ConstructorUiId.kitCloseButton; ConstructorUiId.kitGuideButton ]
        for step in inversePages |> List.filter (fun step -> step <> KitFlowStep) do
            app.invoke inverse step.flowUiId
            for id in kitVerbs do
                Assert.True((app.tryFind inverse id).IsNone, $"{id.value} must not show on {step.caption.value}")
            Assert.True((startingWith inverse "FitRunRow_" |> List.isEmpty) || step = FitFlowStep, $"Publish's runs must not show on {step.caption.value}")
            // 006-1d: no table tab outside a setup's Edit.
            Assert.True((app.tryFind inverse (ConstructorUiId.ribbonTab BayNames.add.uiKey)).IsNone, $"the table's tabs must not show on {step.caption.value}")
        app.invoke inverse ConstructorUiId.flowKitStep
        for id in kitVerbs do
            app.find inverse id |> ignore
        Assert.Equal(2, (startingWith (app.find inverse ConstructorUiId.kitPagePanel) "FitRunRow_").Length)
        // 006-1d: the table's tabs are a setup's Edit's, and none of them is Experiments.
        app.invoke inverse ConstructorUiId.flowSetupsStep
        app.invoke inverse ConstructorUiId.pageEditButton
        let tabs = BayNames.allBayNames |> List.map (fun bay -> app.find inverse (ConstructorUiId.ribbonTab bay.uiKey))
        Assert.DoesNotContain(tabs, fun tab -> tab.Name = "Experiments")
        cancelDraft app inverse
        // 006-1l: every page heads with its help.
        for step in inversePages do
            app.invoke inverse step.flowUiId
            let help = app.find inverse (ConstructorUiId.pageHelpPanel step.caption.uiKey)
            if help.Patterns.ExpandCollapse.IsSupported then help.Patterns.ExpandCollapse.Pattern.Expand()
            let said = waitFor $"the help of {step.caption.value}" 10.0 (fun () -> match textsUnder help |> List.filter (fun text -> text.Length > 40) with [] -> None | texts -> Some texts)
            Assert.NotEmpty(said)

    /// 006-1 (g–k), 006-2: one state at a time, details in place, Edit first, Save on a change, Cancel asks (spec 0051
    /// `.manual/007` 1.2, 4.1, 4.2).
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``006-1, 006-2: an item opens under its row and closes on a second press, Edit comes first, Save waits for a change, and Cancel asks`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "pages-items"
        use app = launch sandbox
        let kitWindow = openKit app ConstructorUiId.openMainButton sandbox.opticsMueller
        let before = kitContent sandbox.opticsMueller
        app.invoke kitWindow ConstructorUiId.flowItemsStep
        let rowIds = startingWith kitWindow "KitItemRow_" |> List.map (fun row -> ConstructorUiId (automationIdOf row))
        Assert.True(rowIds.Length >= 2, "the kit holds its items")
        let first, second = rowIds.[0], rowIds.[1]
        Assert.True((app.tryFind kitWindow ConstructorUiId.itemModelText).IsNone, "nothing is open before a press")
        app.invoke kitWindow first
        Assert.True(isOn (app.find kitWindow first), "a pressed item is chosen")
        let details = app.find kitWindow ConstructorUiId.itemModelText
        assertUnder "the item's details" (app.find kitWindow first) details
        assertAbove "the item's details" details (app.find kitWindow second)
        Assert.True((app.tryFind kitWindow ConstructorUiId.itemLabelField).IsNone, "nothing is editable before Edit")
        app.invoke kitWindow first
        waitFor "the details to close" 10.0 (fun () -> if (app.tryFind kitWindow ConstructorUiId.itemModelText).IsNone then Some () else None)
        Assert.False(isOn (app.find kitWindow first), "a second press unchooses the item")
        app.invoke kitWindow first
        app.invoke kitWindow ConstructorUiId.pageEditButton
        Assert.False((app.find kitWindow ConstructorUiId.pageSaveButton).IsEnabled, "Save waits for a change")
        let label = app.find kitWindow ConstructorUiId.itemLabelField
        label.Patterns.Value.Pattern.SetValue(shownText label + " changed")
        waitFor "Save to be offered" 10.0 (fun () -> if (app.find kitWindow ConstructorUiId.pageSaveButton).IsEnabled then Some () else None)
        app.invoke kitWindow ConstructorUiId.pageCancelButton
        Assert.StartsWith("Discard your changes", shownText (app.find kitWindow ConstructorUiId.pageLeaveQuestionText))
        app.invoke kitWindow ConstructorUiId.pageLeaveNoButton
        app.find kitWindow ConstructorUiId.itemLabelField |> ignore
        app.invoke kitWindow ConstructorUiId.pageCancelButton
        app.invoke kitWindow ConstructorUiId.pageLeaveYesButton
        waitFor "the edit to close" 10.0 (fun () -> if (app.tryFind kitWindow ConstructorUiId.itemLabelField).IsNone then Some () else None)
        Assert.True((before = kitContent sandbox.opticsMueller), "Cancel writes nothing")

    /// 006-1 (e, f), 006-12: copyable names and tables, and the dynamic buttons' menu (spec 0051 `.manual/007` 1.3, 1.4).
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``006-12, 006-1: a dynamic button's menu copies its name and renames it in place, and a table copies as CSV`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "pages-copy"
        use app = launch sandbox
        let inverse = openKit app ConstructorUiId.openInverseButton sandbox.opticsMueller
        app.invoke inverse ConstructorUiId.flowItemsStep
        let rowId = ConstructorUiId (automationIdOf (List.head (startingWith inverse "KitItemRow_")))
        let label = (app.find inverse rowId).Name.Split(" — ").[0]
        let openMenu () : unit =
            inverse.SetForeground()
            (app.find inverse rowId).RightClick()
        openMenu ()
        findAnywhere app ConstructorUiId.entryRenameItem |> ignore
        press (findAnywhere app ConstructorUiId.entryCopyNameItem)
        waitFor "the name on the clipboard" 10.0 (fun () -> if clipboardText () = label then Some () else None)
        openMenu ()
        press (findAnywhere app ConstructorUiId.entryRenameItem)
        let field = app.find inverse ConstructorUiId.renameField
        Assert.Equal(label, shownText field)
        app.invoke inverse ConstructorUiId.renameCancelButton
        // Every table copies as CSV, its header first: the Data matrix and a fit's values.
        app.invoke inverse ConstructorUiId.flowDataStep
        app.invoke inverse (ConstructorUiId.copyTableCsvButton ConstructorUiId.experimentMatrix)
        waitFor "the matrix on the clipboard" 10.0 (fun () -> if clipboardFirstLine().StartsWith "Prep 1" then Some () else None)
        chooseStudy app inverse qzLr
        app.invoke inverse ConstructorUiId.flowFitStep
        app.invoke inverse (ConstructorUiId (automationIdOf (List.head (startingWith inverse "FitRunRow_"))))
        app.invoke inverse ConstructorUiId.openFitResultsButton
        app.invoke inverse (ConstructorUiId.copyTableCsvButton ConstructorUiId.fitValuesGrid)
        waitFor "the fitted values on the clipboard" 10.0 (fun () -> if clipboardFirstLine () = "Parameter,Start,Fitted,σ" then Some () else None)

    /// 006-2: the library chooser is a window of its own, as large as its screen allows (spec 0051 `.manual/007` 4.3).
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``006-2: New item… opens the library chooser in its own window, as large as its screen allows`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "pages-chooser"
        use app = launch sandbox
        let kitWindow = openKit app ConstructorUiId.openMainButton sandbox.opticsMueller
        app.invoke kitWindow ConstructorUiId.flowItemsStep
        app.invoke kitWindow ConstructorUiId.newItemButton
        let chooser = app.window "Choose from the library"
        let area = workAreaOf chooser
        let shown = chooser.BoundingRectangle
        // 1600 × 1200, shrunk to the working area where that is smaller.
        Assert.True(shown.Width >= min 1500 (area.Width - 40) && shown.Height >= min 1100 (area.Height - 40), $"the chooser opens at {shown} on a {area} working area")
        app.find chooser ConstructorUiId.itemModelChooserFilter |> ignore
        app.invoke chooser ConstructorUiId.itemModelChooserCancelButton
        waitFor "the chooser to close" 10.0 (fun () -> if app.windows |> List.exists (fun window -> window.Title = "Choose from the library") then None else Some ())
        match app.tryFind kitWindow ConstructorUiId.pageCancelButton with
        | Some _ -> cancelDraft app kitWindow
        | None -> ()

    /// 006-3: the Setups page (spec 0051 `.manual/007` 5.1–5.4, 5.7).
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``006-3: a chosen setup shows its line with Edit and Copy… on it and its read-only table under it, a second press unchooses it, and Copy… prefills # 1`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "pages-setups"
        use app = launch sandbox
        let kitWindow = openKit app ConstructorUiId.openMainButton sandbox.opticsMueller
        let before = kitContent sandbox.opticsMueller
        app.invoke kitWindow ConstructorUiId.flowSetupsStep
        let chosen = startingWith kitWindow "SavedSetup_" |> List.filter isOn |> List.exactlyOne
        let chosenId = ConstructorUiId (automationIdOf chosen)
        let name = chosen.Name
        let line = app.find kitWindow ConstructorUiId.setupLineText
        for id in [ ConstructorUiId.pageEditButton; ConstructorUiId.copySetupButton ] do
            let shown = app.find kitWindow id
            Assert.True(abs (verticalCentre shown - verticalCentre line) < 20, $"{id.value} must sit on the setup's line")
        assertUnder "the setup's table" line (app.find kitWindow ConstructorUiId.tableElementCanvas)
        Assert.True((app.tryFind kitWindow (ConstructorUiId.ribbonTab BayNames.add.uiKey)).IsNone, "the chosen setup's table is read-only")
        // 006-3a: no Record experiment and no setup grid here.
        Assert.True((app.tryFind kitWindow ConstructorUiId.newExperimentGridButton).IsNone, "the setup grid is the Experiments page's")
        let buttons = kitWindow.FindAllDescendants(condition (fun factory -> factory.ByControlType(ControlType.Button))) |> Array.map (fun button -> button.Name)
        Assert.DoesNotContain(buttons, fun caption -> caption.StartsWith "Record experiment" || caption.StartsWith "Setup grid")
        // 006-3e: a second press unchooses it.
        app.invoke kitWindow chosenId
        waitFor "the setup to close" 10.0 (fun () -> if (app.tryFind kitWindow ConstructorUiId.setupLineText).IsNone then Some () else None)
        Assert.False(isOn (app.find kitWindow chosenId), "a second press unchooses the setup")
        app.invoke kitWindow chosenId
        // 006-3c: Copy… prefills `<name> # 1`.
        app.invoke kitWindow ConstructorUiId.copySetupButton
        Assert.Equal($"{name} # 1", shownText (app.find kitWindow ConstructorUiId.setupNameField))
        cancelDraft app kitWindow
        // 006-3g: Edit, Save withheld until a change; an unchanged Cancel leaves at once.
        app.invoke kitWindow ConstructorUiId.pageEditButton
        Assert.False((app.find kitWindow ConstructorUiId.pageSaveButton).IsEnabled, "Save waits for a change")
        app.invoke kitWindow ConstructorUiId.pageCancelButton
        Assert.True((app.tryFind kitWindow ConstructorUiId.pageLeaveQuestionText).IsNone, "an unchanged Cancel asks nothing")
        Assert.True((before = kitContent sandbox.opticsMueller), "nothing was written")

    /// 006-4: the Experiments page (spec 0051 `.manual/007` 6.1–6.4).
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``006-4: Experiments has no table, opens an experiment's details under it, asks before another experiment over one setup, and wants a setup chosen first`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "pages-experiments"
        use app = launch sandbox
        let kitWindow = openKit app ConstructorUiId.openMainButton sandbox.opticsMueller
        let before = kitContent sandbox.opticsMueller
        app.invoke kitWindow ConstructorUiId.flowExperimentsStep
        Assert.True((app.tryFind kitWindow ConstructorUiId.tableElementCanvas).IsNone, "no table on Experiments")
        Assert.True((app.tryFind kitWindow (ConstructorUiId.ribbonTab BayNames.add.uiKey)).IsNone, "no table tabs on Experiments")
        Assert.StartsWith("New experiments use setup", shownText (app.find kitWindow ConstructorUiId.experimentsContextText))
        let entryIds = startingWith kitWindow "ExperimentEdit_" |> List.map (fun entry -> ConstructorUiId (automationIdOf entry))
        let first, next = entryIds.[0], entryIds.[1]
        app.invoke kitWindow first
        let details = app.find kitWindow ConstructorUiId.experimentDetailsText
        assertUnder "the experiment's details" (app.find kitWindow first) details
        assertAbove "the experiment's details" details (app.find kitWindow next)
        app.invoke kitWindow first
        // 006-4d: a second experiment over the chosen setup asks OK / Cancel / Show.
        app.invoke kitWindow ConstructorUiId.newExperimentButton
        app.invoke kitWindow (ConstructorUiId (automationIdOf (List.head (startingWith kitWindow "ExperimentCandidate_"))))
        waitFor "Create to be offered" 10.0 (fun () -> if (app.find kitWindow ConstructorUiId.pageSaveButton).IsEnabled then Some () else None)
        app.invoke kitWindow ConstructorUiId.pageSaveButton
        Assert.Contains("already use", shownText (app.find kitWindow ConstructorUiId.sameSetupWarningText))
        for id in [ ConstructorUiId.sameSetupOkButton; ConstructorUiId.sameSetupShowButton ] do
            app.find kitWindow id |> ignore
        app.invoke kitWindow ConstructorUiId.sameSetupCancelButton
        waitFor "the warning to close" 10.0 (fun () -> if (app.tryFind kitWindow ConstructorUiId.sameSetupWarningText).IsNone then Some () else None)
        cancelDraft app kitWindow
        Assert.True((before = kitContent sandbox.opticsMueller), "Cancel created nothing")
        // 006-4b: with no setup chosen, nothing can be added, and the page says why.
        app.invoke kitWindow ConstructorUiId.flowSetupsStep
        app.invoke kitWindow (ConstructorUiId (automationIdOf (startingWith kitWindow "SavedSetup_" |> List.filter isOn |> List.exactlyOne)))
        app.invoke kitWindow ConstructorUiId.flowExperimentsStep
        Assert.StartsWith("Choose a setup in Setups first", shownText (app.find kitWindow ConstructorUiId.experimentsContextText))
        Assert.True((app.tryFind kitWindow ConstructorUiId.newExperimentButton).IsNone, "New experiment waits for a chosen setup")

    /// 006-5: the Data page (spec 0051 `.manual/007` 7.1–7.4, D4).
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``006-5: Data shows no setup, a gridded matrix whose entries open their data under them, no-data entries marked apart, and copies the matrix`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "pages-data"
        let direct = addCommittedKit sandbox "multilayer-thin-film"
        use app = launch sandbox
        let inverse = openKit app ConstructorUiId.openInverseButton sandbox.opticsMueller
        app.invoke inverse ConstructorUiId.flowDataStep
        Assert.DoesNotContain(textsUnder inverse, fun text -> text.StartsWith "Active setup" || text.StartsWith "Loaded setup")
        let headers = textsUnder (app.find inverse ConstructorUiId.experimentMatrix)
        Assert.Contains("Prep 1", headers)
        Assert.Contains(headers, fun text -> text.StartsWith "Analyzer")
        let entryId = ConstructorUiId.experimentMatrixEntry (ExperimentId 1)
        Assert.True((app.tryFind inverse ConstructorUiId.dataReviewPanel).IsNone, "the data is hidden until a press")
        app.invoke inverse entryId
        assertUnder "the experiment's data" (app.find inverse entryId) (app.find inverse ConstructorUiId.dataReviewPanel)
        app.invoke inverse entryId
        waitFor "the data to close" 10.0 (fun () -> if (app.tryFind inverse ConstructorUiId.dataReviewPanel).IsNone then Some () else None)
        inverse.SetForeground()
        let withData = fillOf (app.find inverse entryId)
        // 006-5f, 006-1f: the matrix copies as CSV, its header first.
        app.invoke inverse (ConstructorUiId.copyTableCsvButton ConstructorUiId.experimentMatrix)
        waitFor "the matrix on the clipboard" 10.0 (fun () -> if clipboardFirstLine().StartsWith "Prep 1" then Some () else None)
        // 006-5e: a kit's experiments with no data have their own fill and begin with ⚠.
        let other = openKit app ConstructorUiId.openInverseButton direct
        app.invoke other ConstructorUiId.flowDataStep
        let bare = startingWith other "ExperimentMatrixEntry_"
        Assert.NotEmpty(bare)
        for entry in bare do
            Assert.StartsWith("⚠ ", entry.Name)
        other.SetForeground()
        Assert.NotEqual(withData, fillOf (List.head bare))

    /// 006-6: the Unknowns page (spec 0051 `.manual/007` 8.1, 8.2).
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``006-6: Unknowns shows no setup, New study is the page alone, and a parameter's scope and link are edited under it alone`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "pages-unknowns"
        use app = launch sandbox
        let inverse = openKit app ConstructorUiId.openInverseButton sandbox.opticsMueller
        let before = kitContent sandbox.opticsMueller
        chooseStudy app inverse qzLr
        Assert.DoesNotContain(textsUnder inverse, fun text -> text.StartsWith "Active setup" || text.StartsWith "Loaded setup")
        Assert.Empty(startingWith inverse "StudyParameterScope_")
        app.invoke inverse ConstructorUiId.pageEditButton
        let rowIds = startingWith inverse "StudyParameterRow_" |> List.map (fun row -> ConstructorUiId (automationIdOf row))
        Assert.True(rowIds.Length >= 2, "the study holds its parameters")
        // Only an unknown has a scope and a link: the rows are pressed in turn until one shows its scope, alone.
        let scopeShown () : AutomationElement option =
            try Some (waitFor "a parameter's scope" 2.0 (fun () -> match startingWith inverse "StudyParameterScope_" with [ scope ] -> Some scope | _ -> None))
            with _ -> None
        let pressedUnknown =
            rowIds |> List.indexed |> List.tryPick (fun (index, rowId) ->
                app.invoke inverse rowId
                scopeShown () |> Option.map (fun scope -> index, scope))
        match pressedUnknown with
        | Some (index, scope) ->
            assertUnder "the parameter's scope" (app.find inverse rowIds.[index]) scope
            match List.tryItem (index + 1) rowIds with
            | Some nextId -> assertAbove "the parameter's scope" scope (app.find inverse nextId)
            | None -> ()
        | None -> Assert.Fail "no parameter of the study showed its scope under it"
        cancelDraft app inverse
        // 006-6c: New study is the page alone: the studies are hidden while one is made.
        app.invoke inverse ConstructorUiId.newStudyButton
        app.find inverse ConstructorUiId.newStudyNameField |> ignore
        Assert.True((app.tryFind inverse (ConstructorUiId.studyChoice qzLr)).IsNone, "the studies hide while one is made")
        cancelDraft app inverse
        Assert.True((before = kitContent sandbox.opticsMueller), "nothing was written")

    /// 006-7, 006-8: what Plan and Fit do, and how a fit run is made (spec 0051 `.manual/007` 1.6, 8.3, 8.4).
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``006-7, 006-8: Plan and Fit say what they do, a fit run is made by New fit run…, and the runs are listed newest first`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "pages-fit"
        use app = launch sandbox
        let inverse = openKit app ConstructorUiId.openInverseButton sandbox.opticsMueller
        chooseStudy app inverse qzLr
        let helpOf (step : ConstructorFlowStep) : string list =
            app.invoke inverse step.flowUiId
            let help = app.find inverse (ConstructorUiId.pageHelpPanel step.caption.uiKey)
            if help.Patterns.ExpandCollapse.IsSupported then help.Patterns.ExpandCollapse.Pattern.Expand()
            waitFor $"the help of {step.caption.value}" 10.0 (fun () -> match textsUnder help with [] -> None | texts -> Some texts)
        Assert.Contains(helpOf PlanFlowStep, fun text -> text.StartsWith "The Measurement Planner says")
        Assert.Contains(helpOf FitFlowStep, fun text -> text.Contains "New fit run…")
        let runs = startingWith inverse "FitRunRow_" |> List.map automationIdOf
        Assert.Equal<string list>(List.sortDescending runs, runs)
        app.invoke inverse ConstructorUiId.newFitRunButton
        for id in [ ConstructorUiId.studyRunStageOneButton; ConstructorUiId.studyCalibrationFixedChoice; ConstructorUiId.studyCalibrationPriorChoice ] do
            app.find inverse id |> ignore
        cancelDraft app inverse

    /// 006-9: the step-by-step guides (spec 0051 `.manual/007` 9.2).
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``006-9: Guide… opens the step-by-step inverse guide from the Kit page, and the Launcher's opens it too`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "pages-guide"
        use app = launch sandbox
        let inverse = openKit app ConstructorUiId.openInverseButton sandbox.opticsMueller
        app.invoke inverse ConstructorUiId.flowKitStep
        app.invoke inverse ConstructorUiId.kitGuideButton
        let guide = app.window "Guide"
        Assert.True(isOn (app.find guide ConstructorUiId.guideInverseChoice), "the inverse kit opens the inverse guide")
        let steps = textsUnder (app.find guide ConstructorUiId.guideSteps)
        Assert.True(steps.Length >= 10, $"the inverse guide has {steps.Length} steps")
        for page in [ "Data"; "Unknowns"; "Fit"; "Results" ] do
            Assert.Contains(steps, fun text -> text.Contains page)
        Assert.StartsWith("1. ", List.head steps)
        guide.Close()
        app.invoke (app.window "Launcher") ConstructorUiId.launcherGuideButton
        app.window "Guide" |> ignore

    /// 006-10: the folder above the last kit (spec 0051 `.manual/007` 3.4).
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``006-10: Open kit… starts in the folder above the last kit, after a restart too`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "pages-folder"
        let elsewhere = Path.Combine(sandbox.root, "elsewhere")
        let kit = addCommittedKitUnder elsewhere "optics-mueller"
        let assertStartsAbove (app : DesktopApp) : unit =
            app.invoke (app.window "Launcher") ConstructorUiId.openMainButton
            let start = app.window "Open a kit"
            app.invoke start ConstructorUiId.openKitButton
            Assert.EndsWith("elsewhere", trimmed app.nativeDialogFolder)
            app.cancelNativeDialog ()
            // The window's UIA element can be disconnected once the dialog closes, so it is found again.
            (app.window "Open a kit").Close()
        let first = launch sandbox
        try
            let kitWindow = openKit first ConstructorUiId.openMainButton kit
            closeKit first kitWindow kit
            assertStartsAbove first
        finally (first :> IDisposable).Dispose()
        use restarted = launch sandbox
        assertStartsAbove restarted

    /// 006-11: tooltips, their delay and Options (spec 0051 `.manual/007` 1.5).
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``006-11: every static command carries its help, shown as its tooltip after the delay Options holds, 1 s by default`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "pages-tooltips"
        use app = launch sandbox
        let launcher = app.window "Launcher"
        app.invoke launcher ConstructorUiId.launcherOptionsButton
        let options = app.window "Options"
        Assert.Equal("1000", shownText (app.find options ConstructorUiId.optionsToolTipDelayField))
        app.invoke options ConstructorUiId.optionsCancelButton
        let kitWindow = openKit app ConstructorUiId.openMainButton sandbox.opticsMueller
        app.invoke kitWindow ConstructorUiId.flowKitStep
        for id in [ ConstructorUiId.sendKitButton; ConstructorUiId.kitCloseButton; ConstructorUiId.kitGuideButton; ConstructorUiId.flowItemsStep ] do
            Assert.False(String.IsNullOrWhiteSpace (app.find kitWindow id).HelpText, $"{id.value} must carry its help")
        app.invoke kitWindow ConstructorUiId.flowItemsStep
        let button = app.find kitWindow ConstructorUiId.newItemButton
        let help = button.HelpText
        Assert.False(String.IsNullOrWhiteSpace help, "New item… must carry its help")
        // The tooltip is the help, shown after the delay: hover, then find it in any window of the app.
        kitWindow.SetForeground()
        let bounds = button.BoundingRectangle
        FlaUI.Core.Input.Mouse.MoveTo(bounds.Left + bounds.Width / 2, bounds.Top + bounds.Height / 2)
        let hovered = Stopwatch.StartNew()
        let shownTip () : AutomationElement option =
            app.automation.GetDesktop().FindAllChildren(condition (fun factory -> factory.ByProcessId app.application.ProcessId))
            |> Array.tryPick (fun window ->
                window.FindAllDescendants(condition (fun factory -> factory.ByControlType(ControlType.Text)))
                |> Array.tryFind (fun (text : AutomationElement) -> text.Name = help))
        waitFor "the tooltip" 6.0 shownTip |> ignore
        Assert.True(hovered.ElapsedMilliseconds >= 700L, $"the tooltip showed after {hovered.ElapsedMilliseconds} ms, before the 1 s delay")

    /// 006-13: a Mueller polarimeter's View (spec 0051 `.manual/007` 6.6, D6).
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``006-13: in absorbing-biaxial-noisy every experiment's View chart is offered and opens its chart`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "pages-mueller"
        let kit = addCommittedKit sandbox "absorbing-biaxial-noisy"
        use app = launch sandbox
        let kitWindow = openKit app ConstructorUiId.openMainButton kit
        app.invoke kitWindow ConstructorUiId.flowExperimentsStep
        let keys = startingWith kitWindow "ExperimentEdit_" |> List.map (fun entry -> ConstructorUiKey ((automationIdOf entry).Substring "ExperimentEdit_".Length))
        Assert.Equal(5, keys.Length)
        for key in keys do
            app.invoke kitWindow (ConstructorUiId.experimentEditButton key)
            let view = app.find kitWindow (ConstructorUiId.experimentViewButton key)
            Assert.True(view.IsEnabled, $"View chart of {key.value} must be offered")
            app.invoke kitWindow (ConstructorUiId.experimentViewButton key)
            let chart =
                waitFor "the chart window" 60.0 (fun () ->
                    app.windows |> List.tryFind (fun window -> app.tryFind window ConstructorUiId.chartWindowExportCsv |> Option.isSome))
            chart.Close()
            waitFor "the chart to close" 10.0 (fun () ->
                if app.windows |> List.exists (fun window -> app.tryFind window ConstructorUiId.chartWindowExportCsv |> Option.isSome) then None else Some ())
            Assert.DoesNotContain(textsUnder kitWindow, fun text -> text.StartsWith "No chart for")
            app.invoke kitWindow (ConstructorUiId.experimentEditButton key)

    /// 006-14: the two direct kits and the direct guide (spec 0051 `.manual/007` 9.1, 9.2).
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``006-14: the two direct kits chart their scripts' R and T, and the direct guide says how they are built`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "pages-direct"
        let kits = [ addCommittedKit sandbox "multilayer-thin-film"; addCommittedKit sandbox "langasite-on-silicon" ]
        use app = launch sandbox
        for kit in kits do
            let kitWindow = openKit app ConstructorUiId.openMainButton kit
            app.invoke kitWindow ConstructorUiId.flowExperimentsStep
            let keys = startingWith kitWindow "ExperimentEdit_" |> List.map (fun entry -> ConstructorUiKey ((automationIdOf entry).Substring "ExperimentEdit_".Length))
            Assert.Equal(2, keys.Length)
            for key in keys do
                app.invoke kitWindow (ConstructorUiId.experimentEditButton key)
                app.invoke kitWindow (ConstructorUiId.experimentViewButton key)
                let chart =
                    waitFor "the chart window" 60.0 (fun () ->
                        app.windows |> List.tryFind (fun window -> app.tryFind window ConstructorUiId.chartWindowExportCsv |> Option.isSome))
                app.invoke chart ConstructorUiId.chartWindowExportCsv
                let target = Path.Combine(sandbox.root, $"{Path.GetFileName kit}-{key.value}.csv")
                app.typeIntoNativeDialog target
                waitFor "the exported chart" 15.0 (fun () -> if File.Exists target then Some () else None)
                let header = File.ReadLines(target) |> Seq.head
                Assert.Contains("Intensity (R)", header)
                Assert.Contains("Intensity (T)", header)
                chart.Close()
                app.invoke kitWindow (ConstructorUiId.experimentEditButton key)
            closeKit app kitWindow kit
        app.invoke (app.window "Launcher") ConstructorUiId.launcherGuideButton
        let guide = app.window "Guide"
        if not (isOn (app.find guide ConstructorUiId.guideDirectChoice)) then app.invoke guide ConstructorUiId.guideDirectChoice
        let steps = textsUnder (app.find guide ConstructorUiId.guideSteps)
        for page in [ "Items"; "Setups"; "Experiments" ] do
            Assert.Contains(steps, fun text -> text.Contains page)

    /// 006-15: the `.optics-kit` marker (spec 0051 `.manual/007` 3.2, 3.3, D3).
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``006-15: every committed kit is marked by its .optics-kit, with created and modified, and Open kit… asks for that file`` () : unit =
        requireDesktop ()
        let committed =
            Directory.GetDirectories(Path.Combine(repositoryRoot, "data"))
            |> Array.filter (fun folder -> Path.GetFileName folder <> "chroma")
        Assert.Equal(4, committed.Length)
        for folder in committed do
            let marker = Path.Combine(folder, ".optics-kit")
            Assert.True(File.Exists marker, $"{Path.GetFileName folder} must hold .optics-kit")
            use document = JsonDocument.Parse(File.ReadAllText marker)
            for stamp in [ "created"; "modified" ] do
                Assert.True(DateTime.TryParse(document.RootElement.GetProperty(stamp).GetString()) |> fst, $"{Path.GetFileName folder}: {stamp}")
        let sandbox = createSandbox "pages-marker"
        use app = launch sandbox
        app.invoke (app.window "Launcher") ConstructorUiId.openMainButton
        let start = app.window "Open a kit"
        app.invoke start ConstructorUiId.openKitButton
        let dialog = app.nativeDialog
        Assert.Contains("Open a kit: choose its .optics-kit file", dialog.Name)
        let filters = dialog.FindAllDescendants(condition (fun factory -> factory.ByControlType(ControlType.ComboBox)))
        Assert.Contains(filters, fun (combo : AutomationElement) -> (combo.Name + " " + shownText combo).Contains "Optics kit (*.optics-kit)")
        app.typeIntoNativeDialog (Path.Combine(sandbox.opticsMueller, ".optics-kit"))
        app.window (Path.GetFileName sandbox.opticsMueller) |> ignore
