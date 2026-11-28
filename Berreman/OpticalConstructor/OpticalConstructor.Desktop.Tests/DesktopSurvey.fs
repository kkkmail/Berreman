namespace OpticalConstructor.Desktop.Tests

open System
open System.IO
open System.Security.Cryptography
open System.Text.Json
open System.Text.RegularExpressions
open FlaUI.Core.AutomationElements
open FlaUI.Core.Definitions
open Xunit

open OpticalConstructor.Controls
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Ui
open OpticalConstructor.Ui.ConstructorUiIdOfDomain
open OpticalConstructor.Ui.TableAndElementRotationView
open OpticalConstructor.Desktop.Tests.DesktopSession

/// The 0050 `.manual/038` and 0051 `.manual/002` survey, re-driven on the real desktop by id (spec 0051 `.manual/003`
/// Phase 6). Each fact names the findings it closes; the report is 0051 `.manual/005`.
[<Collection(DesktopSession.RealDesktop)>]
module DesktopSurveyTests =

    /// How much redder than green and blue `element`'s text is on screen, averaged over the darkest 2 % of its pixels
    /// (the glyphs' cores, not their anti-aliased edges): near 0 for the done tone (40, 40, 40), about 144 for refused.
    let private redOfText (element : AutomationElement) : int =
        let channels =
            Screen.capture element.BoundingRectangle
            |> Array.map (fun pixel -> (pixel >>> 16) &&& 0xFF, (pixel >>> 8) &&& 0xFF, pixel &&& 0xFF)
            |> Array.sortBy (fun (red, green, blue) -> red + green + blue)
        let ink = channels |> Array.truncate (max 1 (channels.Length / 50))
        let mean (pick : int * int * int -> int) : int = ink |> Array.averageBy (pick >> float) |> int
        let red, green, blue = mean (fun (r, _, _) -> r), mean (fun (_, g, _) -> g), mean (fun (_, _, b) -> b)
        red - max green blue

    /// A kit experiment's name and its setup's name, read from its file.
    type private StoredExperiment =
        {
            name : string
            setupName : string
        }

    let private storedExperiment (kit : string) (id : int) : StoredExperiment =
        Directory.GetFiles(kit, "*.experiment-json")
        |> Array.pick (fun file ->
            use document = JsonDocument.Parse(File.ReadAllText file)
            let experiment = document.RootElement.GetProperty "experiment"
            if experiment.GetProperty("id").GetInt32() = id then
                Some
                    {
                        name = experiment.GetProperty("name").GetString()
                        setupName = experiment.GetProperty("setup").GetProperty("name").GetString()
                    }
            else None)

    /// The lowest experiment id of a kit.
    let private firstExperimentId (kit : string) : int =
        Directory.GetFiles(kit, "*.experiment-json")
        |> Array.map (fun file ->
            use document = JsonDocument.Parse(File.ReadAllText file)
            document.RootElement.GetProperty("experiment").GetProperty("id").GetInt32())
        |> Array.min

    /// A slot named the old way, by its kind and a number ("Linear polarizer #6"), not by the kit's item.
    let private namedByKindAndNumber : Regex = Regex @" #\d"

    let private guid : Regex = Regex @"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}"

    /// 038-1, 038-3, 038-6, 038-7, 038-8, 038-9, 038-10; 002-2, 002-5, 002-11 (spec 0051 `.manual/003` 3.1-3.3, 3.5,
    /// 3.6, 5.1, 5.3).
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``survey: the first start asks nothing, a kit opens on a setup, a row loads its own, and every name is the kit's`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "survey-setups"
        use app = launch sandbox
        // 038-9, 002-11 (D6): the shipped catalogue's first import asks nothing; the Launcher says what it imported.
        let launcher = app.window "Launcher"
        let settled = shownText (app.find launcher ConstructorUiId.launcherSettledImportsText)
        Assert.Matches(@"^Imported \d+ catalogue entries on start; none needed a decision\.$", settled)
        Assert.True((app.tryFind launcher ConstructorUiId.launcherPendingImportsText).IsNone, "nothing must wait for an import decision")
        // Every snapshot of optics-mueller is a shipped entry the library now holds, so opening it asks nothing (002-11).
        let kitWindow, questions = openKitSeeing app ConstructorUiId.openMainButton sandbox.opticsMueller
        Assert.True(List.isEmpty questions, $"opening the kit asked {questions}")
        // 038-1 (D2), 038-6, 002-5: the kit opens on one of its setups, chosen on the Setups page beside every other.
        let setupNames = Directory.GetFiles(sandbox.opticsMueller, "*.setup-json") |> Array.map Path.GetFileNameWithoutExtension
        app.invoke kitWindow ConstructorUiId.flowSetupsStep
        let saved = app.find kitWindow ConstructorUiId.savedSetups
        let toggles =
            saved.FindAllDescendants()
            |> Array.filter (fun (element : AutomationElement) -> (element.Properties.AutomationId.ValueOrDefault |> Option.ofObj |> Option.defaultValue "").StartsWith "SavedSetup_")
        Assert.Equal(setupNames.Length, toggles.Length)
        let chosen = toggles |> Array.filter (fun toggle -> toggle.Patterns.Toggle.Pattern.ToggleState.Value = ToggleState.On)
        Assert.Equal(1, chosen.Length)
        Assert.Contains(chosen.[0].Name, setupNames)
        // 038-8: the chosen setup's element list names each slot by its role and the kit's item ("Source: Laser").
        let labels = Directory.GetFiles(sandbox.opticsMueller, "*.item-json") |> Array.map Path.GetFileNameWithoutExtension
        let listed =
            (app.find kitWindow ConstructorUiId.tableElementList).FindAllDescendants(condition (fun factory -> factory.ByControlType(ControlType.ListItem)))
            |> Array.map (fun (item : AutomationElement) -> item.Name)
        Assert.NotEmpty(listed)
        for name in listed do
            Assert.Contains(labels, fun label -> name.EndsWith $": {label}")
        // 038-3: a chosen experiment's details name its own setup.
        let e1 = storedExperiment sandbox.opticsMueller 1
        app.invoke kitWindow ConstructorUiId.flowExperimentsStep
        app.invoke kitWindow (ConstructorUiId.experimentEditButton (ConstructorUiKey "1"))
        Assert.StartsWith($"Setup: '{e1.setupName}' revision", shownText (app.find kitWindow ConstructorUiId.experimentDetailsText))
        // 038-8: the experiments describe their scans by the kit's names too.
        for text in textsUnder kitWindow do
            Assert.DoesNotMatch(namedByKindAndNumber, text)
        // 038-7, 002-2: a status shows on the page it was raised on, and one that says it worked is not red.
        app.invoke kitWindow ConstructorUiId.flowKitStep
        app.invoke kitWindow ConstructorUiId.kitReloadChangesButton
        let reloaded = "No kit file changed outside the app."
        waitFor "the reload status" 10.0 (fun () -> if shownText (app.find kitWindow ConstructorUiId.kitStatus) = reloaded then Some () else None)
        kitWindow.SetForeground()
        let red = redOfText (app.find kitWindow ConstructorUiId.kitStatus)
        Assert.True(red < 40, $"'{reloaded}' is drawn red (red over green and blue: {red})")
        app.invoke kitWindow ConstructorUiId.flowItemsStep
        match app.tryFind kitWindow ConstructorUiId.kitStatus with
        | Some _ -> Assert.Fail "a status shows on its own page alone"
        | None -> ()
        // 038-10: New experiment starts with nothing chosen, and the editor says why Create is withheld.
        app.invoke kitWindow ConstructorUiId.flowExperimentsStep
        app.invoke kitWindow ConstructorUiId.newExperimentButton
        Assert.False(String.IsNullOrWhiteSpace(shownText (app.find kitWindow ConstructorUiId.experimentAddReason)))
        app.invoke kitWindow ConstructorUiId.pageCancelButton

    /// 038-5, 002 "Fit mode re-press rewrites the study", 002 "as priors not saved" (spec 0051 `.manual/003` 2.1, 2.2,
    /// 2.5).
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``survey: browsing a kit writes nothing, and a fit run records a changed calibration use in the study`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "survey-writes"
        use app = launch sandbox
        let before = kitContent sandbox.opticsMueller
        let inverse = openKit app ConstructorUiId.openInverseButton sandbox.opticsMueller
        for step in
            [ ConstructorUiId.flowItemsStep; ConstructorUiId.flowSetupsStep; ConstructorUiId.flowExperimentsStep
              ConstructorUiId.flowDataStep; ConstructorUiId.flowUnknownsStep; ConstructorUiId.flowPlanStep
              ConstructorUiId.flowFitStep; ConstructorUiId.flowResultsStep; ConstructorUiId.flowKitStep ] do
            app.invoke inverse step
        // Choosing an experiment and leaving its Edit unchanged is no change either.
        app.invoke inverse ConstructorUiId.flowExperimentsStep
        app.invoke inverse (ConstructorUiId.experimentEditButton (ConstructorUiKey "1"))
        app.invoke inverse ConstructorUiId.pageEditButton
        app.invoke inverse ConstructorUiId.pageCancelButton
        // Spec 0051 `.manual/007` 8.4: the New fit run… form's choices are its draft, so pressing them writes nothing,
        // the one the study has included, and Cancel asks once they changed.
        let study = StudyName "QZ-LR"
        let chooseStudy (window : Window) : unit =
            app.invoke window ConstructorUiId.flowUnknownsStep
            if (app.find window (ConstructorUiId.studyChoice study)).Patterns.Toggle.Pattern.ToggleState.Value <> ToggleState.On then
                app.invoke window (ConstructorUiId.studyChoice study)
        chooseStudy inverse
        app.invoke inverse ConstructorUiId.flowFitStep
        app.invoke inverse ConstructorUiId.newFitRunButton
        let fixedChoice = app.find inverse ConstructorUiId.studyCalibrationFixedChoice
        Assert.Equal(ToggleState.On, fixedChoice.Patterns.Toggle.Pattern.ToggleState.Value)
        app.invoke inverse ConstructorUiId.studyCalibrationFixedChoice
        app.invoke inverse ConstructorUiId.studyCalibrationPriorChoice
        app.invoke inverse ConstructorUiId.pageCancelButton
        app.invoke inverse ConstructorUiId.pageLeaveYesButton
        let browsed = kitContent sandbox.opticsMueller
        let written = String.Join("; ", changedFiles before browsed)
        Assert.True((before = browsed), $"browsing wrote {written}")
        // As priors is the study's own: a run started with it records it in the study, read back after the kit is closed
        // and opened again; the run itself is cancelled.
        app.invoke inverse ConstructorUiId.newFitRunButton
        app.invoke inverse ConstructorUiId.studyCalibrationPriorChoice
        app.invoke inverse ConstructorUiId.studyRunStageOneButton
        let studyFile = Path.Combine(sandbox.opticsMueller, "QZ-LR.study-json")
        waitFor "the study to save as priors" 10.0 (fun () -> if File.ReadAllText(studyFile).Contains "\"calibrationUse\": 2" then Some () else None)
        match app.tryFind inverse ConstructorUiId.studyFitCancelButton with
        | Some _ -> app.invoke inverse ConstructorUiId.studyFitCancelButton
        | None -> ()
        waitFor "the fit to end" 60.0 (fun () -> if (app.tryFind inverse ConstructorUiId.studyFitCancelButton).IsNone then Some () else None)
        Assert.Contains("QZ-LR.study-json", changedFiles before (kitContent sandbox.opticsMueller))
        app.invoke inverse ConstructorUiId.flowKitStep
        app.invoke inverse ConstructorUiId.kitCloseButton
        waitFor "the kit window to close" 15.0 (fun () ->
            if app.windows |> List.exists (fun window -> window.Title.Contains(Path.GetFileName sandbox.opticsMueller)) then None else Some ())
        let reopened = openKit app ConstructorUiId.openInverseButton sandbox.opticsMueller
        chooseStudy reopened
        app.invoke reopened ConstructorUiId.flowFitStep
        app.invoke reopened ConstructorUiId.newFitRunButton
        Assert.Equal(ToggleState.On, (app.find reopened ConstructorUiId.studyCalibrationPriorChoice).Patterns.Toggle.Pattern.ToggleState.Value)

    /// 002-4, 002-7, 002-8 (spec 0051 `.manual/003` 3.4, 4.1, 4.2, 5.8).
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``survey: in Inverse a Data matrix entry opens its data under it, headed by its experiment, and View charts the measured points`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "survey-data"
        use app = launch sandbox
        let inverse = openKit app ConstructorUiId.openInverseButton sandbox.opticsMueller
        let e1 = storedExperiment sandbox.opticsMueller 1
        // 002-4, 006-5c (spec 0051 `.manual/007` 7.3): the matrix entry opens the data right under its row, headed by its
        // experiment.
        app.invoke inverse ConstructorUiId.flowDataStep
        let entry = app.find inverse (ConstructorUiId.experimentMatrixEntry (ExperimentId 1))
        app.invoke inverse (ConstructorUiId.experimentMatrixEntry (ExperimentId 1))
        let review = app.find inverse ConstructorUiId.dataReviewPanel
        Assert.StartsWith($"{e1.name} [", (app.find review (ConstructorUiId.dataFileHeading (ExperimentId 1))).Name)
        let gap = review.BoundingRectangle.Top - entry.BoundingRectangle.Bottom
        Assert.True(gap >= 0 && gap < 200, $"the data opens {gap} px below its matrix entry")
        // 002-7, 002-8: View charts in Inverse too, with the measured points beside the model's.
        app.invoke inverse ConstructorUiId.flowExperimentsStep
        app.invoke inverse (ConstructorUiId.experimentEditButton (ConstructorUiKey "1"))
        app.invoke inverse (ConstructorUiId.experimentViewButton (ConstructorUiKey "1"))
        let chart =
            waitFor "the chart window" 30.0 (fun () ->
                app.windows |> List.tryFind (fun window -> app.tryFind window ConstructorUiId.chartWindowExportCsv |> Option.isSome))
        app.invoke chart ConstructorUiId.chartWindowExportCsv
        Assert.EndsWith("Exports", trimmed app.nativeDialogFolder)
        let target = Path.Combine(sandbox.root, "inverse-chart.csv")
        app.typeIntoNativeDialog target
        waitFor "the exported chart" 15.0 (fun () -> if File.Exists target then Some () else None)
        let header = File.ReadLines(target) |> Seq.head
        Assert.Contains($"Measured — {e1.name}", header)
        Assert.Equal($"Saved {target}.", shownText (app.find chart ConstructorUiId.chartWindowExportStatus))

    /// The View chart's Polar view and back (spec 0051 `.manual/004`): polar drew only the measured series, and back in
    /// XY the modelled one lay flat at zero.
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``survey: a View chart draws both series in its Polar view and is unchanged back in XY`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "survey-polar"
        use app = launch sandbox
        let kitWindow = openKit app ConstructorUiId.openMainButton sandbox.opticsMueller
        app.invoke kitWindow ConstructorUiId.flowExperimentsStep
        app.invoke kitWindow (ConstructorUiId.experimentEditButton (ConstructorUiKey "1"))
        app.invoke kitWindow (ConstructorUiId.experimentViewButton (ConstructorUiKey "1"))
        let chart =
            waitFor "the chart window" 30.0 (fun () ->
                app.windows |> List.tryFind (fun window -> app.tryFind window ConstructorUiId.chartWindowPolar |> Option.isSome))
        let plot = app.find chart ConstructorUiId.chartWindowPlot
        // The plot as drawn once it changed from `shown` and held still for one poll.
        let redrawnFrom (shown : int[]) : int[] =
            let last = ref shown
            waitFor "the chart to redraw" 15.0 (fun () ->
                let now = Screen.capture plot.BoundingRectangle
                let held = now = last.Value && now <> shown
                last.Value <- now
                if held then Some now else None)
        let pixelsOf (hex : int) (pixels : int[]) : int =
            let channel (shift : int) (pixel : int) : int = (pixel >>> shift) &&& 0xFF
            pixels |> Array.filter (fun pixel -> [ 16; 8; 0 ] |> List.forall (fun shift -> abs (channel shift pixel - channel shift hex) < 40)) |> Array.length
        let xy = redrawnFrom Array.empty
        app.invoke chart ConstructorUiId.chartWindowPolar
        let polar = redrawnFrom xy
        // The modelled series first (ChartStyle.defaultColors), the measured one second.
        let modelled, measured = pixelsOf 0x1E5AC8 polar, pixelsOf 0xDC7814 polar
        Assert.True(modelled * 4 >= measured, $"the polar view drew {modelled} px of the modelled series beside {measured} px of the measured one")
        app.invoke chart ConstructorUiId.chartWindowPolar
        let back = redrawnFrom polar
        let changed = Array.map2 (fun (before : int) (after : int) -> before <> after) xy back |> Array.filter id |> Array.length
        Assert.True((changed = 0), $"{changed} of {xy.Length} plot pixels differ from the XY view before Polar")

    /// 002-10 (spec 0051 `.manual/003` 4.4).
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``survey: Copy experiments from another kit shows each item by its label and model, and Cancel copies nothing`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "survey-copy"
        let other = addCommittedKit sandbox "absorbing-biaxial-noisy"
        use app = launch sandbox
        let kitWindow = openKit app ConstructorUiId.openMainButton sandbox.opticsMueller
        let before = kitContent sandbox.opticsMueller
        app.invoke kitWindow ConstructorUiId.flowKitStep
        app.invoke kitWindow ConstructorUiId.kitCopyExperimentsButton
        Assert.Contains("Open a kit: choose its .optics-kit file", app.nativeDialog.Name)
        Assert.EndsWith(Path.GetFileName sandbox.kits, trimmed app.nativeDialogFolder)
        app.typeIntoNativeDialog (Path.Combine(other, ".optics-kit"))
        // The window's UIA element can be disconnected once the dialog closes, so the kit window is found again.
        let kitWindow = app.window (Path.GetFileName sandbox.opticsMueller)
        app.find kitWindow ConstructorUiId.kitCopyExperimentList |> ignore
        app.invoke kitWindow (ConstructorUiId.kitCopyExperimentToggle (ExperimentId (firstExperimentId other)))
        let items = textsUnder (app.find kitWindow ConstructorUiId.kitCopyItemList)
        Assert.NotEmpty(items)
        for text in items do
            Assert.DoesNotMatch(guid, text)
            Assert.DoesNotContain("(model not in", text)
            Assert.DoesNotContain("(model in neither", text)
        app.invoke kitWindow ConstructorUiId.kitCopyCancelButton
        Assert.True((before = kitContent sandbox.opticsMueller), "Cancel must copy nothing")

    /// 002-3 and its wording (spec 0051 `.manual/003` 5.2 (D5)).
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``survey: Send kit saves a zip in Exports, and Open kit from zip asks for an empty folder by name`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "survey-zip"
        use app = launch sandbox
        let kitWindow = openKit app ConstructorUiId.openMainButton sandbox.opticsMueller
        app.invoke kitWindow ConstructorUiId.flowKitStep
        app.invoke kitWindow ConstructorUiId.sendKitButton
        let exports = Path.Combine(sandbox.root, "data", "Exports")
        Assert.EndsWith("Exports", trimmed app.nativeDialogFolder)
        let zip = Path.Combine(exports, "sent.zip")
        app.typeIntoNativeDialog zip
        waitFor "the sent kit" 30.0 (fun () -> if File.Exists zip && (shownText (app.find kitWindow ConstructorUiId.kitStatus)).StartsWith "The kit was sent" then Some () else None)
        app.invoke kitWindow ConstructorUiId.kitCloseButton
        waitFor "the kit window to close" 15.0 (fun () ->
            if app.windows |> List.exists (fun window -> window.Title.Contains(Path.GetFileName sandbox.opticsMueller)) then None else Some ())
        app.invoke (app.window "Launcher") ConstructorUiId.openMainButton
        let start = app.window "Open a kit"
        app.invoke start ConstructorUiId.openKitZipButton
        Assert.EndsWith("Exports", trimmed app.nativeDialogFolder)
        app.typeIntoNativeDialog zip
        waitFor "the unpack question" 15.0 (fun () ->
            match app.nativeDialog.Name with
            | name when name.Contains "Choose an empty folder to unpack the kit into" -> Some ()
            | _ -> None)
        app.cancelNativeDialog ()

    /// 002 "photo never displayed" (spec 0051 `.manual/003` 5.5). Headless drawing stubs every bitmap, so only the real
    /// renderer shows that the photo itself is decoded and drawn.
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``survey: Choose photo… shows the photo itself beside its path`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "survey-photo"
        let picture = Path.Combine(sandbox.root, "mount.png")
        File.Copy(Path.Combine(repositoryRoot, "specs", "0046", ".manual", "008-pivot-incident-face.png"), picture)
        let cpl1 =
            use document = JsonDocument.Parse(File.ReadAllText(Path.Combine(sandbox.opticsMueller, "CPL1.item-json")))
            ItemId (Guid.Parse(document.RootElement.GetProperty("item").GetProperty("id").GetString()))
        use app = launch sandbox
        let kitWindow = openKit app ConstructorUiId.openMainButton sandbox.opticsMueller
        app.invoke kitWindow ConstructorUiId.flowItemsStep
        app.invoke kitWindow (ConstructorUiId.kitItemRow cpl1)
        app.invoke kitWindow ConstructorUiId.pageEditButton
        app.invoke kitWindow ConstructorUiId.itemChoosePhotoButton
        app.typeIntoNativeDialog picture
        // The window's UIA element can be disconnected once the dialog closes, so the kit window is found again.
        let kitWindow = app.window (Path.GetFileName sandbox.opticsMueller)
        waitFor "the picked photo in the draft" 10.0 (fun () ->
            if (shownText (app.find kitWindow ConstructorUiId.itemPhotoText)).EndsWith "(attached at Save)" then Some () else None)
        app.invoke kitWindow ConstructorUiId.pageSaveButton
        let image = app.find kitWindow ConstructorUiId.itemPhotoImage
        let shown = image.BoundingRectangle
        Assert.True(shown.Width > 20 && shown.Height > 20 && shown.Width <= 170, $"the photo shows at {shown}")
        Assert.Equal("Photo: data/mount.png", shownText (app.find kitWindow ConstructorUiId.itemPhotoText))
        Assert.True(File.Exists(Path.Combine(sandbox.opticsMueller, "data", "mount.png")), "the photo is copied into the kit's data folder")
