/// Headless lifecycle + environment-persistence view tests (spec 0024 Part U8
/// §U8.1–U8.3, gate `ui-tests`). Trait `Category=ui-tests` (the `ui-tests` gate runs
/// `--filter Category!=ui-smoke`).
///
/// Asserts the Part U8 lifecycle wiring:
///   • R-1 / AC-U8.1 — Save writes `<name>.ocproj.json` under the working folder via the
///     §0.4-marshaled `Cmd`, and Open of that file round-trips through the
///     schema-validated `ProjectFile.openProject` path. The Save toolbar button
///     dispatches `Io SaveRequested`.
///   • R-2 / AC-U8.2 — choosing a template loads via `Templates.loadTemplate` and a
///     gallery entry via `Help.openEntry` (the existing schema-validated factories).
///   • R-3 / AC-U8.3 — toggling theme / panel visibility / dock flips the root `env` and
///     attaches a fire-and-forget `UserEnvironment.save` persist `Cmd`; the persisted env
///     survives a reload (an app restart).
///
/// The marshaled background `Cmd`s are exercised exactly as `ConstructionEditTests` /
/// `FitPanelTests` do: run the effect, pump the dispatcher until the marshaled message
/// lands. The headless session leaves `Shell`'s host `IStorageProvider` cleared, so the
/// IO `Cmd`s take their deterministic working-folder fallback path.
namespace OpticalConstructor.Ui.Tests

open System
open System.Collections.Generic
open System.IO
open System.Threading
open Avalonia.Controls
open Avalonia.Interactivity
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open OpticalConstructor.Ui

module LifecycleTests =

    let private mount (content : Avalonia.FuncUI.Types.IView) : Window =
        let window = Window()
        window.Content <- Component(fun _ctx -> content)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window

    let private buttonByContent (window : Window) (label : string) : Button =
        window.GetVisualDescendants()
        |> Seq.choose (fun v -> match v with | :? Button as b -> Some b | _ -> None)
        |> Seq.find (fun b -> string b.Content = label)

    /// Run all of `cmd`'s effects with a collecting dispatch, then pump the UI thread
    /// until `landed` holds (the off-thread continuation marshaled its message back) or
    /// the spin budget is exhausted.
    let private runUntil (cmd : Elmish.Cmd<Shell.RootMsg>) (landed : List<Shell.RootMsg> -> bool) : List<Shell.RootMsg> =
        let dispatched = List<Shell.RootMsg>()
        for effect in cmd do effect dispatched.Add
        let mutable spins = 0
        while not (landed dispatched) && spins < 500 do
            Dispatcher.UIThread.RunJobs()
            Thread.Sleep 10
            spins <- spins + 1
        Dispatcher.UIThread.RunJobs()
        dispatched

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``Save writes the ocproj.json and Open round-trips through the schema-validated path`` () =
        HeadlessSession.run (fun () ->
            // No host provider in the headless session → the deterministic working-folder
            // fallback path (AC-U8.1).
            Shell.clearStorageProviderForTests ()
            let dir = Path.Combine(Path.GetTempPath(), "oc-u8-" + Guid.NewGuid().ToString("N"))
            try
                let seed = fst Shell.init
                let construction = { seed.construction with workingFolder = dir; projectName = "u8-roundtrip" }
                let model = { seed with construction = construction }

                // Save: the marshaled write posts Io (Saved path) back onto the UI thread.
                let _, saveCmd = Shell.update (Shell.RootMsg.Io LifecycleView.SaveRequested) model
                let saved =
                    runUntil saveCmd (fun ds ->
                        ds |> Seq.exists (fun m -> match m with | Shell.RootMsg.Io (LifecycleView.Saved _) -> true | _ -> false))
                let savedPath = Path.Combine(dir, "u8-roundtrip.ocproj.json")
                Assert.True(File.Exists savedPath, sprintf "Save must write %s" savedPath)
                Assert.Contains(saved, fun m -> match m with | Shell.RootMsg.Io (LifecycleView.Saved _) -> true | _ -> false)

                // Open of the saved file round-trips through ProjectFile.openProject.
                let _, openCmd = Shell.update (Shell.RootMsg.Io (LifecycleView.OpenPath savedPath)) model
                let opened =
                    runUntil openCmd (fun ds ->
                        ds |> Seq.exists (fun m -> match m with | Shell.RootMsg.Io (LifecycleView.Loaded _) -> true | _ -> false))
                let loadedMsg =
                    opened
                    |> Seq.pick (fun m ->
                        match m with
                        | Shell.RootMsg.Io (LifecycleView.Loaded _ as im) -> Some (Shell.RootMsg.Io im)
                        | _ -> None)
                let m2, _ = Shell.update loadedMsg model
                // The reopened project carries the seed's bandpass-filter system, proving the
                // schema-validated round-trip bound a real project (not a placeholder).
                Assert.True(
                    m2.construction.project.systems
                    |> List.exists (fun s -> s.description = Some "Bandpass filter"),
                    "the reopened project must round-trip the seed system")
            finally
                try Directory.Delete(dir, true) with _ -> ())

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``the Save toolbar button dispatches Io SaveRequested`` () =
        HeadlessSession.run (fun () ->
            let captured = List<LifecycleView.IoMsg>()
            let window = mount (LifecycleView.lifecycleBar None captured.Add)
            (buttonByContent window "Save").RaiseEvent(RoutedEventArgs(Button.ClickEvent))
            Dispatcher.UIThread.RunJobs()
            Assert.Contains(captured, fun m -> match m with | LifecycleView.SaveRequested -> true | _ -> false)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``choosing a template loads via Templates.loadTemplate`` () =
        // AC-U8.2: LoadTemplate routes through the schema-validated Templates.loadTemplate
        // factory and the loaded project replaces the construction/workspace models.
        let model = fst Shell.init
        let entry = Templates.all |> List.find (fun e -> e.title = "AR coating")
        let m2, _ = Shell.update (Shell.RootMsg.Io (LifecycleView.LoadTemplate entry)) model
        Assert.True(
            m2.construction.project.systems |> List.exists (fun s -> s.description = Some "AR coating"),
            "the template must load through Templates.loadTemplate")
        Assert.Equal(Shell.Page.Constructor, m2.page)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``choosing a gallery entry loads via Help.openEntry`` () =
        // AC-U8.2: LoadGallery routes through Help.openEntry (which reuses Templates.loadTemplate).
        let model = fst Shell.init
        let entry = Help.gallery |> List.find (fun e -> e.title = "Multilayer thin film")
        let m2, _ = Shell.update (Shell.RootMsg.Io (LifecycleView.LoadGallery entry)) model
        Assert.True(
            m2.construction.project.systems |> List.exists (fun s -> s.description = Some "Multilayer thin film"),
            "the gallery sample must load through Help.openEntry")

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``theme / panel / dock changes flip env, attach a persist Cmd, and survive a reload`` () =
        // AC-U8.3: the change updates the root env through the pure AppShell reducers AND
        // attaches a fire-and-forget UserEnvironment.save Cmd; the persisted env survives a
        // reload (an app restart). The persist target is redirected to a scratch path so the
        // round-trip is verified without touching real per-user state.
        let dir = Path.Combine(Path.GetTempPath(), "oc-u8env-" + Guid.NewGuid().ToString("N"))
        let path = Path.Combine(dir, "environment.json")
        try
            Shell.setEnvironmentPathForTests path
            let model = fst Shell.init

            // Theme toggle: env flips and a persist Cmd is attached.
            let m1, cmd = Shell.update (Shell.RootMsg.Shell Shell.ToggleTheme) model
            Assert.NotEqual(model.env.theme, m1.env.theme)
            Assert.NotEmpty(cmd)
            // Run the persist Cmd and wait for the write to land, then reload (restart).
            for effect in cmd do effect ignore
            let mutable spins = 0
            while not (File.Exists path) && spins < 300 do
                Thread.Sleep 10
                spins <- spins + 1
            let restored = UserEnvironment.load path
            Assert.Equal(m1.env.theme, restored.theme)

            // Panel visibility: env layout flips and a persist Cmd is attached.
            let m2, cmd2 = Shell.update (Shell.RootMsg.Shell (Shell.SetPanelVisible ("stack", false))) model
            Assert.NotEmpty(cmd2)
            Assert.True(
                m2.env.layout.panels |> List.exists (fun p -> p.panel = "stack" && not p.visible),
                "panel visibility must flip through AppShell.setPanelVisible")

            // Panel dock: env layout re-docks and a persist Cmd is attached.
            let m3, cmd3 = Shell.update (Shell.RootMsg.Shell (Shell.SetPanelDock ("results", UserEnvironment.Bottom))) model
            Assert.NotEmpty(cmd3)
            Assert.True(
                m3.env.layout.panels |> List.exists (fun p -> p.panel = "results" && p.dock = UserEnvironment.Bottom),
                "panel dock must change through AppShell.dockPanel")
        finally
            try Directory.Delete(dir, true) with _ -> ()
