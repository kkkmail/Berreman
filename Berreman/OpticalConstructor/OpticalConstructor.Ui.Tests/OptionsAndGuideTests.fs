namespace OpticalConstructor.Ui.Tests

open System
open System.IO
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open FsCheck
open FsCheck.FSharp
open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Tests
open OpticalConstructor.Ui
open OpticalConstructor.Ui.UserEnvironment
open OpticalConstructor.Ui.OptionsView
open OpticalConstructor.Ui.GuideView
open OpticalConstructor.Ui.TableAndElementRotationView

/// The tooltip delay, the Options window's update over a recording proxy, and the two guides against the pages they name
/// (spec 0051 `.manual/007` 1.5, 9.2).
module OptionsAndGuideTests =

    // ================================ the tooltip delay and the Options window ================================

    [<Fact>]
    let ``a tooltip delay is a whole number of milliseconds from 0 to 5000, one second by default`` () : unit =
        Assert.Equal(1000, ToolTipDelay.defaultValue.value)
        for text, expected in [ "0", Some 0; "5000", Some 5000; "1000", Some 1000; " 250 ", Some 250 ] do
            Assert.Equal(expected, ToolTipDelay.tryParse text |> Option.map (fun delay -> delay.value))
        for text in [ "-1"; "5001"; "abc"; "1.5"; ""; "1e3" ] do
            Assert.Equal(None, ToolTipDelay.tryParse text |> Option.map (fun delay -> delay.value))
        let readsBack (milliseconds : int) : bool =
            match ToolTipDelay.tryParse (string milliseconds) with
            | Some delay -> milliseconds >= 0 && milliseconds <= 5000 && delay.value = milliseconds
            | None -> milliseconds < 0 || milliseconds > 5000
        Check.One(Config.QuickThrowOnFailure.WithMaxTest(200), Prop.forAll (Arb.fromGen (Gen.choose (-10000, 10000))) readsBack)

    [<Fact>]
    let ``the tooltip delay is kept in environment.json and read back`` () : unit =
        let file = FileName (Path.Combine(AppContext.BaseDirectory, "OptionsAndGuideTests", Guid.NewGuid().ToString("N"), "environment.json"))
        let settings = EnvironmentSettings.load file
        Assert.Equal(ToolTipDelay.defaultValue, settings.preferences.toolTipDelay)
        let changed = { settings with preferences = { settings.preferences with toolTipDelay = ToolTipDelay 250 } }
        match changed.save file with
        | Ok () -> ()
        | Error error -> Assert.Fail $"saving the environment must succeed: {error.describe}"
        Assert.Equal(ToolTipDelay 250, (EnvironmentSettings.load file).preferences.toolTipDelay)

    /// An Options proxy over a held delay, recording every save and every apply.
    let private recordingProxy (held : ToolTipDelay) (refusalOpt : ControlCaption option) : OptionsProxy * ResizeArray<ToolTipDelay> * ResizeArray<ToolTipDelay> =
        let saves = ResizeArray<ToolTipDelay>()
        let applies = ResizeArray<ToolTipDelay>()
        let proxy : OptionsProxy =
            {
                loadDelay = fun () -> held
                saveDelay =
                    fun delay ->
                        match refusalOpt with
                        | Some reason -> Error reason
                        | None ->
                            saves.Add delay
                            Ok ()
                applyDelay = applies.Add
            }
        proxy, saves, applies

    /// The model an Options message leaves the window open on; a close fails the fact.
    let private stayingOpen (outcome : OptionsOutcome) : OptionsModel =
        match outcome with
        | OptionsStayOpen model -> model
        | OptionsClose -> failwith "the Options window must stay open"

    [<Fact>]
    let ``Options writes and applies a changed delay once at Save, and Cancel asks only when the delay changed`` () : unit =
        let proxy, saves, applies = recordingProxy (ToolTipDelay 1000) None
        let opened = OptionsModel.ofProxy proxy
        Assert.Equal(ControlCaption "1000", opened.delayText)
        Assert.Equal<DraftState>(DraftUnchanged, opened.draftState)
        Assert.Equal(OptionsClose, updateOptions CancelOptions opened)
        // An unchanged Save writes nothing.
        Assert.Equal(opened, stayingOpen (updateOptions SaveOptions opened))
        Assert.Empty(saves)
        let typed = stayingOpen (updateOptions (SetToolTipDelayText (ControlCaption "250")) opened)
        Assert.Equal<DraftState>(DraftChanged, typed.draftState)
        // Cancel with a change asks; No keeps the draft, Yes closes.
        let asked = stayingOpen (updateOptions CancelOptions typed)
        Assert.Equal(DiscardOptionsQuestion, asked.question)
        Assert.Equal(NoOptionsQuestion, (stayingOpen (updateOptions KeepEditingOptions asked)).question)
        Assert.Equal(OptionsClose, updateOptions ConfirmDiscardOptions asked)
        Assert.Empty(saves)
        Assert.Empty(applies)
        let saved = stayingOpen (updateOptions SaveOptions typed)
        Assert.Equal<ToolTipDelay list>([ ToolTipDelay 250 ], List.ofSeq saves)
        Assert.Equal<ToolTipDelay list>([ ToolTipDelay 250 ], List.ofSeq applies)
        Assert.Equal(ToolTipDelay 250, saved.saved)
        Assert.Equal(Some (ControlCaption "Saved: a tooltip now waits 250 ms."), saved.statusOpt)
        Assert.Equal<DraftState>(DraftUnchanged, saved.draftState)
        Assert.Equal(OptionsClose, updateOptions CancelOptions saved)

    [<Fact>]
    let ``Options refuses a delay out of range, and a refused write applies nothing`` () : unit =
        let proxy, saves, applies = recordingProxy (ToolTipDelay 1000) None
        let outOfRange = stayingOpen (updateOptions (SetToolTipDelayText (ControlCaption "9000")) (OptionsModel.ofProxy proxy))
        let refused = stayingOpen (updateOptions SaveOptions outOfRange)
        Assert.Equal(Some (ControlCaption "The delay is a whole number of milliseconds from 0 to 5000."), refused.statusOpt)
        Assert.Equal(ToolTipDelay 1000, refused.saved)
        Assert.Empty(saves)
        let failing, _, failingApplies = recordingProxy (ToolTipDelay 1000) (Some (ControlCaption "the disk is full"))
        let typed = stayingOpen (updateOptions (SetToolTipDelayText (ControlCaption "300")) (OptionsModel.ofProxy failing))
        let notSaved = stayingOpen (updateOptions SaveOptions typed)
        Assert.Equal(Some (ControlCaption "Not saved — the disk is full"), notSaved.statusOpt)
        Assert.Equal(ToolTipDelay 1000, notSaved.saved)
        Assert.Empty(failingApplies)
        Assert.Empty(applies)

    // ================================ the guides ================================

    [<Fact>]
    let ``every guide step names a page of the guide's constructor, by its caption, and a command with help`` () : unit =
        for guide in [ DirectProblemGuide; InverseProblemGuide ] do
            let pages = ConstructorFlowStep.stepsOf guide.mode
            Assert.NotEmpty(guide.steps)
            for step in guide.steps do
                match step.command.helpOpt with
                | Some help -> Assert.False(String.IsNullOrWhiteSpace help.value)
                | None -> Assert.Fail $"{guide.caption.value}: {step.command.value} has no help text"
                match step.pageOpt with
                | Some page ->
                    Assert.Contains(page, pages)
                    Assert.StartsWith(page.caption.value, step.text.value)
                | None ->
                    Assert.Contains(step.command, [ ConstructorUiId.openMainButton; ConstructorUiId.openInverseButton ])
                    Assert.StartsWith("On the Launcher", step.text.value)

    /// Whether the page `model` shows carries `id`.
    let private pageHolds (model : TableAndElementRotationModel) (id : ConstructorUiId) : bool =
        let mutable held = false
        HeadlessSession.run (fun () ->
            let window = ScrollExtentTests.constructorWindow model
            window.Width <- 1200.0
            window.Height <- 900.0
            window.Show()
            Dispatcher.UIThread.RunJobs()
            try
                held <-
                    match UiDriver.tryFindControl window id.value with
                    | Some _ -> true
                    | None -> false
            finally window.Close())
        held

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the direct guide's every command is on the page it names`` () : unit =
        // Spec 0051 `.manual/007` 9.2: a renamed or moved button fails here instead of misleading the user.
        let filled = ScrollExtentTests.filledExperiments ()
        for step in DirectProblemGuide.steps do
            match step.pageOpt with
            | Some page ->
                let onPage = { filled with flowStep = page }
                Assert.True(pageHolds onPage step.command, $"{page.caption.value} must hold {step.command.value}")
            | None -> ()

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the guide window shows a guide's steps in order and switches between the two`` () : unit =
        HeadlessSession.run (fun () ->
            let chosen = ResizeArray<GuideKind>()
            let window = Window(Width = 900.0, Height = 700.0)
            window.Content <- Avalonia.FuncUI.Component(fun _ -> guideView DirectProblemGuide chosen.Add)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            try
                let texts =
                    window.GetVisualDescendants()
                    |> Seq.choose (function :? TextBlock as text -> Option.ofObj text.Text | _ -> None)
                    |> List.ofSeq
                for step in DirectProblemGuide.steps do
                    Assert.Contains(texts, fun text -> text.Contains step.text.value)
                UiDriver.ById.clickOn window InverseProblemGuide.choiceUiId
                Assert.Equal<GuideKind list>([ InverseProblemGuide ], List.ofSeq chosen)
            finally window.Close())
