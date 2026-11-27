namespace OpticalConstructor.Desktop.Tests

open System
open System.IO
open System.Runtime.InteropServices
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

    module private Screen =
        [<Struct; StructLayout(LayoutKind.Sequential)>]
        type BitmapInfoHeader =
            val mutable size : uint32
            val mutable width : int
            val mutable height : int
            val mutable planes : uint16
            val mutable bitCount : uint16
            val mutable compression : uint32
            val mutable sizeImage : uint32
            val mutable xPelsPerMeter : int
            val mutable yPelsPerMeter : int
            val mutable colorsUsed : uint32
            val mutable colorsImportant : uint32

        [<DllImport("user32.dll")>]
        extern bool SetProcessDPIAware()

        [<DllImport("user32.dll")>]
        extern IntPtr GetDC(IntPtr window)

        [<DllImport("user32.dll")>]
        extern int ReleaseDC(IntPtr window, IntPtr context)

        [<DllImport("gdi32.dll")>]
        extern IntPtr CreateCompatibleDC(IntPtr context)

        [<DllImport("gdi32.dll")>]
        extern IntPtr CreateCompatibleBitmap(IntPtr context, int width, int height)

        [<DllImport("gdi32.dll")>]
        extern IntPtr SelectObject(IntPtr context, IntPtr gdiObject)

        [<DllImport("gdi32.dll")>]
        extern bool BitBlt(IntPtr target, int x, int y, int width, int height, IntPtr source, int sourceX, int sourceY, uint32 operation)

        [<DllImport("gdi32.dll")>]
        extern int GetDIBits(IntPtr context, IntPtr bitmap, uint32 start, uint32 lines, int[] bits, BitmapInfoHeader& info, uint32 usage)

        [<DllImport("gdi32.dll")>]
        extern bool DeleteObject(IntPtr gdiObject)

        [<DllImport("gdi32.dll")>]
        extern bool DeleteDC(IntPtr context)

        /// The pixels of `bounds` on screen, top row first, each 0x00RRGGBB.
        let capture (bounds : Drawing.Rectangle) : int[] =
            SetProcessDPIAware() |> ignore
            let screen = GetDC IntPtr.Zero
            let memory = CreateCompatibleDC screen
            let bitmap = CreateCompatibleBitmap(screen, bounds.Width, bounds.Height)
            try
                SelectObject(memory, bitmap) |> ignore
                BitBlt(memory, 0, 0, bounds.Width, bounds.Height, screen, bounds.Left, bounds.Top, 0x00CC0020u) |> ignore
                let mutable header = BitmapInfoHeader()
                header.size <- uint32 (Marshal.SizeOf<BitmapInfoHeader>())
                header.width <- bounds.Width
                header.height <- -bounds.Height
                header.planes <- 1us
                header.bitCount <- 32us
                let pixels = Array.zeroCreate<int> (bounds.Width * bounds.Height)
                GetDIBits(memory, bitmap, 0u, uint32 bounds.Height, pixels, &header, 0u) |> ignore
                pixels
            finally
                DeleteObject bitmap |> ignore
                DeleteDC memory |> ignore
                ReleaseDC(IntPtr.Zero, screen) |> ignore

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

    /// Every file of a kit outside `.oc/`, by its path in the kit, with its SHA-256.
    let private kitContent (folder : string) : Map<string, string> =
        Directory.GetFiles(folder, "*", SearchOption.AllDirectories)
        |> Array.filter (fun file -> not (Path.GetRelativePath(folder, file).StartsWith ".oc"))
        |> Array.map (fun file -> Path.GetRelativePath(folder, file), Convert.ToHexString(SHA256.HashData(File.ReadAllBytes file)))
        |> Map.ofArray

    /// What changed between two `kitContent` reads, for a failure message.
    let private changedFiles (before : Map<string, string>) (after : Map<string, string>) : string list =
        Set.union (before.Keys |> Set.ofSeq) (after.Keys |> Set.ofSeq)
        |> Set.filter (fun path -> Map.tryFind path before <> Map.tryFind path after)
        |> Set.toList

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

    /// The texts UI Automation shows under `root`.
    let private textsUnder (root : AutomationElement) : string list =
        root.FindAllDescendants(condition (fun factory -> factory.ByControlType(ControlType.Text)))
        |> Array.map (fun (text : AutomationElement) -> text.Name)
        |> Array.filter (String.IsNullOrEmpty >> not)
        |> List.ofArray

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
        // 038-1 (D2): the kit opens on one of its setups, never an empty table.
        let setupNames = Directory.GetFiles(sandbox.opticsMueller, "*.setup-json") |> Array.map Path.GetFileNameWithoutExtension
        let active = shownText (app.find kitWindow ConstructorUiId.activeSetupText)
        Assert.Contains(setupNames, fun name -> active.StartsWith $"Active setup: {name}")
        // 038-6, 002-5: the Setups step lists every setup, the active one chosen.
        app.invoke kitWindow ConstructorUiId.flowSetupsStep
        let saved = app.find kitWindow ConstructorUiId.savedSetups
        let toggles =
            saved.FindAllDescendants()
            |> Array.filter (fun (element : AutomationElement) -> (element.Properties.AutomationId.ValueOrDefault |> Option.ofObj |> Option.defaultValue "").StartsWith "SavedSetup_")
        Assert.Equal(setupNames.Length, toggles.Length)
        let chosen = toggles |> Array.filter (fun toggle -> toggle.Patterns.Toggle.Pattern.ToggleState.Value = ToggleState.On)
        Assert.Equal(1, chosen.Length)
        Assert.StartsWith($"Active setup: {chosen.[0].Name}", active)
        // 038-3: a row loads its experiment on that experiment's own setup, and says so.
        let e1 = storedExperiment sandbox.opticsMueller 1
        app.invoke kitWindow ConstructorUiId.flowExperimentsStep
        app.invoke kitWindow (ConstructorUiId.experimentEditButton (ConstructorUiKey "1"))
        let loaded = $"Loaded {e1.name} on its setup '{e1.setupName}'."
        waitFor "the Loaded status" 10.0 (fun () -> if shownText (app.find kitWindow ConstructorUiId.kitStatus) = loaded then Some () else None)
        // The active setup is the Setups step's (3.2); 038-7: the status stays while the user moves between steps.
        app.invoke kitWindow ConstructorUiId.flowSetupsStep
        Assert.StartsWith($"Active setup: {e1.setupName}", shownText (app.find kitWindow ConstructorUiId.activeSetupText))
        Assert.Equal(loaded, shownText (app.find kitWindow ConstructorUiId.kitStatus))
        app.invoke kitWindow ConstructorUiId.flowExperimentsStep
        Assert.Equal(loaded, shownText (app.find kitWindow ConstructorUiId.kitStatus))
        // 002-2: a status that says it worked is not red.
        kitWindow.SetForeground()
        let red = redOfText (app.find kitWindow ConstructorUiId.kitStatus)
        Assert.True(red < 40, $"'{loaded}' is drawn red (red over green and blue: {red})")
        // 038-8: the Select bay's list names each slot by its role and the kit's item ("Source: Laser").
        let labels = Directory.GetFiles(sandbox.opticsMueller, "*.item-json") |> Array.map Path.GetFileNameWithoutExtension
        app.invoke kitWindow (ConstructorUiId.ribbonTab BayNames.selector.uiKey)
        let listed =
            (app.find kitWindow ConstructorUiId.tableElementList).FindAllDescendants(condition (fun factory -> factory.ByControlType(ControlType.ListItem)))
            |> Array.map (fun (item : AutomationElement) -> item.Name)
        Assert.NotEmpty(listed)
        for name in listed do
            Assert.Contains(labels, fun label -> name.EndsWith $": {label}")
        // 038-8: the experiment rows describe their scans by the kit's names too.
        for text in textsUnder kitWindow do
            Assert.DoesNotMatch(namedByKindAndNumber, text)
        // 038-10: New leaves nothing chosen, and Add says why it is withheld.
        app.invoke kitWindow ConstructorUiId.flowExperimentsStep
        app.invoke kitWindow ConstructorUiId.experimentNewButton
        Assert.False(String.IsNullOrWhiteSpace(shownText (app.find kitWindow ConstructorUiId.experimentAddReason)))

    /// 038-5, 002 "Fit mode re-press rewrites the study", 002 "as priors not saved" (spec 0051 `.manual/003` 2.1, 2.2,
    /// 2.5).
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``survey: browsing a kit writes nothing, and only a changed fit mode is saved in the study`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "survey-writes"
        use app = launch sandbox
        let before = kitContent sandbox.opticsMueller
        let inverse = openKit app ConstructorUiId.openInverseButton sandbox.opticsMueller
        for step in
            [ ConstructorUiId.flowItemsStep; ConstructorUiId.flowSetupsStep; ConstructorUiId.flowExperimentsStep
              ConstructorUiId.flowDataStep; ConstructorUiId.flowUnknownsStep; ConstructorUiId.flowPlanStep
              ConstructorUiId.flowFitStep; ConstructorUiId.flowResultsStep; ConstructorUiId.flowPublishStep ] do
            app.invoke inverse step
        // Loading a row and committing it over its unchanged setup is no change either.
        app.invoke inverse ConstructorUiId.flowExperimentsStep
        app.invoke inverse (ConstructorUiId.experimentEditButton (ConstructorUiKey "1"))
        app.invoke inverse ConstructorUiId.experimentAddButton
        // Pressing the fit mode the study already has writes nothing.
        let study = StudyName "QZ-LR"
        app.invoke inverse ConstructorUiId.flowUnknownsStep
        app.invoke inverse (ConstructorUiId.studyChoice study)
        app.invoke inverse ConstructorUiId.flowFitStep
        let fixedChoice = app.find inverse ConstructorUiId.studyCalibrationFixedChoice
        Assert.Equal(ToggleState.On, fixedChoice.Patterns.Toggle.Pattern.ToggleState.Value)
        app.invoke inverse ConstructorUiId.studyCalibrationFixedChoice
        let browsed = kitContent sandbox.opticsMueller
        let written = String.Join("; ", changedFiles before browsed)
        Assert.True((before = browsed), $"browsing wrote {written}")
        // As priors is the study's own: it is saved, and read back after the kit is closed and opened again.
        app.invoke inverse ConstructorUiId.studyCalibrationPriorChoice
        let studyFile = Path.Combine(sandbox.opticsMueller, "QZ-LR.study-json")
        waitFor "the study to save as priors" 10.0 (fun () -> if File.ReadAllText(studyFile).Contains "\"calibrationUse\": 2" then Some () else None)
        Assert.Equal<string list>([ "QZ-LR.study-json" ], changedFiles before (kitContent sandbox.opticsMueller))
        app.invoke inverse ConstructorUiId.kitCloseButton
        waitFor "the kit window to close" 15.0 (fun () ->
            if app.windows |> List.exists (fun window -> window.Title.Contains(Path.GetFileName sandbox.opticsMueller)) then None else Some ())
        let reopened = openKit app ConstructorUiId.openInverseButton sandbox.opticsMueller
        app.invoke reopened ConstructorUiId.flowUnknownsStep
        app.invoke reopened (ConstructorUiId.studyChoice study)
        app.invoke reopened ConstructorUiId.flowFitStep
        Assert.Equal(ToggleState.On, (app.find reopened ConstructorUiId.studyCalibrationPriorChoice).Patterns.Toggle.Pattern.ToggleState.Value)

    /// 002-4, 002-7, 002-8 (spec 0051 `.manual/003` 3.4, 4.1, 4.2, 5.8).
    [<Fact>]
    [<Trait("Category", "desktop")>]
    let ``survey: in Inverse each Data row is headed by its experiment, Review opens under it, and View charts the measured points`` () : unit =
        requireDesktop ()
        let sandbox = createSandbox "survey-data"
        use app = launch sandbox
        let inverse = openKit app ConstructorUiId.openInverseButton sandbox.opticsMueller
        let e1 = storedExperiment sandbox.opticsMueller 1
        // 002-4: the row is headed by its experiment, and Review opens right under it.
        app.invoke inverse ConstructorUiId.flowDataStep
        let heading = app.find inverse (ConstructorUiId.dataFileHeading (ExperimentId 1))
        Assert.StartsWith($"{e1.name} [", heading.Name)
        app.invoke inverse (ConstructorUiId.dataReviewButton (ExperimentId 1))
        let review = app.find inverse ConstructorUiId.dataReviewPanel
        let gap = review.BoundingRectangle.Top - heading.BoundingRectangle.Bottom
        Assert.True(gap >= 0 && gap < 200, $"Review opens {gap} px below its row's heading")
        // 002-7, 002-8: View charts in Inverse too, with the measured points beside the model's.
        app.invoke inverse ConstructorUiId.flowExperimentsStep
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
        let other = Path.Combine(sandbox.kits, "absorbing-biaxial-noisy")
        let committed = Path.Combine(repositoryRoot, "data", "absorbing-biaxial-noisy")
        for file in Directory.GetFiles(committed, "*", SearchOption.AllDirectories) do
            let relative = Path.GetRelativePath(committed, file)
            if not (relative.StartsWith ".oc") then
                let target = Path.Combine(other, relative)
                Directory.CreateDirectory(Path.GetDirectoryName target) |> ignore
                File.Copy(file, target)
        use app = launch sandbox
        let kitWindow = openKit app ConstructorUiId.openMainButton sandbox.opticsMueller
        let before = kitContent sandbox.opticsMueller
        app.invoke kitWindow ConstructorUiId.kitCopyExperimentsButton
        Assert.Contains("Choose a kit folder", app.nativeDialog.Name)
        Assert.EndsWith(Path.GetFileName sandbox.kits, trimmed app.nativeDialogFolder)
        app.answerNativeDialog other |> ignore
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
        app.invoke kitWindow ConstructorUiId.itemChoosePhotoButton
        app.typeIntoNativeDialog picture
        // The window's UIA element can be disconnected once the dialog closes, so the kit window is found again.
        let kitWindow = app.window (Path.GetFileName sandbox.opticsMueller)
        let image = app.find kitWindow ConstructorUiId.itemPhotoImage
        let shown = image.BoundingRectangle
        Assert.True(shown.Width > 20 && shown.Height > 20 && shown.Width <= 170, $"the photo shows at {shown}")
        Assert.Equal("Photo: data/mount.png", shownText (app.find kitWindow ConstructorUiId.itemPhotoText))
        Assert.True(File.Exists(Path.Combine(sandbox.opticsMueller, "data", "mount.png")), "the photo is copied into the kit's data folder")
