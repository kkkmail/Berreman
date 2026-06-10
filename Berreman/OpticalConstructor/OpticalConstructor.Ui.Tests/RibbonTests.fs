/// Ribbon-and-menu shell + catalogue + value-id modal tests (Spec 0026 Part D + Part F,
/// slice 006, gate `ui-tests` / `ui-smoke`). The pure cases carry `Category=ui-tests`
/// (the `ui-tests` gate runs `--filter Category!=ui-smoke`); the mount-and-render case
/// carries `Category=ui-smoke`. Coverage:
///   * AC-D1 — the ribbon shows Build/Element/Trace-View/Experiment/Settings, each
///             populated from the centralized command registry.
///   * AC-D2 — collapse exposes every command through the menus; every command is
///             surfaced exactly once across the tabs (one projection, no second site),
///             and a known command renders in BOTH the expanded ribbon and the menus.
///   * AC-D3 — selecting an element appends the contextual Element tab beside the
///             persistent tabs.
///   * AC-D4 — ribbon labels resolve under both EN and RU; setting the language updates
///             the persisted env language.
///   * AC-F1 — the catalogue lists OT/LS/LP/CP/S/Lens/Flat Mirror/Curved Mirror/D with no
///             analyzer entry; LP/CP both map to Polarizer; a chain may hold >2 polarizers.
///   * AC-F2 — the element local menu exposes the value-id action; it opens a working,
///             dismissible empty modal.
///   * D.5/D.6 — the constructor is the default landing; the Legacy entry navigates to the
///             legacy dockable screen.
namespace OpticalConstructor.Ui.Tests

open System
open Avalonia.Controls
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open Berreman.Constants
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Project
open OpticalConstructor.Ui
open OpticalConstructor.Ui.Commands

module RibbonTests =

    // --- Fixtures -----------------------------------------------------------

    let private at (x : float) (y : float) : TablePoint = { x = x * 1.0<meter>; y = y * 1.0<meter> }

    let private sampleAt (x : float) (y : float) : ElementPlacement = ElementPlacement.create Sample (at x y)

    /// A constructor page model seeded over the canonical template project with `ps` as
    /// the on-table placements (the rest of the project is irrelevant here).
    let private model (ps : ElementPlacement list) : ConstructorView.Model =
        let proj = { Templates.bandpassFilter () with placements = ps; table = Table.defaultTable }
        ConstructorView.init UserEnvironment.defaults proj

    /// The shipped EN/RU string resource (build-copied beside the test assembly).
    let private resource : Localization.Resource =
        match Localization.loadFromFile (Localization.resourcePath ()) with
        | Ok r -> r
        | Error _ -> { entries = Map.empty }

    /// A no-op ribbon dispatch bundle for headless render tests.
    let private noopDispatch : Ribbon.Dispatch =
        {
            onRibbon = ignore
            onConstructor = ignore
            onNavigateLegacy = ignore
            onSetLanguage = ignore
            onToggleTheme = ignore
        }

    /// Mount a view headlessly and collect the string content of every rendered Button
    /// (the ribbon labels its command controls / menu items with their localized text).
    let private buttonLabels (content : Avalonia.FuncUI.Types.IView) : string list =
        let window = Window()
        window.Content <- Component(fun _ -> content)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        let labels =
            window.GetVisualDescendants()
            |> Seq.choose (fun v ->
                match v with
                | :? Button as b -> (match b.Content with | :? string as s -> Some s | _ -> None)
                | _ -> None)
            |> List.ofSeq
        window.Close()
        labels

    // =======================================================================
    // AC-D1 — the ribbon tab set, each populated from the command registry.
    // =======================================================================

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-D1 the ribbon shows Build, Element, Trace-View, Experiment and Settings tabs from the registry`` () =
        Assert.Equal<Ribbon.RibbonTab list>(
            [ Ribbon.Build; Ribbon.TraceView; Ribbon.Experiment; Ribbon.Settings ], Ribbon.persistentTabs)
        Assert.Equal<Ribbon.RibbonTab list>(
            [ Ribbon.Build; Ribbon.ElementTab; Ribbon.TraceView; Ribbon.Experiment; Ribbon.Settings ], Ribbon.allTabs)
        // Each tab's controls are GENERATED from the registry (D.1.2): non-empty, and
        // every projected command is a real registry command.
        let registryCommands = Commands.registry |> List.map (fun d -> d.command) |> Set.ofList
        for tab in Ribbon.allTabs do
            let cmds = Ribbon.tabCommands tab
            Assert.NotEmpty cmds
            for c in cmds do Assert.True(registryCommands.Contains c, sprintf "%A is not a registry command" c)

    // =======================================================================
    // AC-D2 — single-source ribbon ↔ menu equivalence (constraint 0.4).
    // =======================================================================

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-D2 every registry command is surfaced exactly once across the tabs`` () =
        // The ribbon and the collapsed menus are the SAME `tabCommands` projection, so a
        // command added once to the registry appears in both with no second wiring site.
        let registryCommands = Commands.registry |> List.map (fun d -> d.command)
        let projected = Ribbon.allTabs |> List.collect Ribbon.tabCommands
        // No command appears on two tabs...
        Assert.Equal(List.length projected, List.length (List.distinct projected))
        // ...and the projection covers exactly the registry (every command surfaced once).
        Assert.Equal<Command list>(List.sort registryCommands, List.sort projected)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-D2 a command renders in both the expanded ribbon and the collapsed menus`` () =
        HeadlessSession.run (fun () ->
            let cv = model [ sampleAt 0.0 0.0 ]
            let zoom = LocalHelp.commandLabel resource Localization.English ZoomView
            // The expanded ribbon on the Trace/View tab shows the Zoom command...
            let expanded =
                Ribbon.view resource Localization.English { Ribbon.init with activeTab = Ribbon.TraceView }
                    cv UserEnvironment.defaults noopDispatch
            Assert.Contains(zoom, buttonLabels expanded)
            // ...and the collapsed menus expose it too (the SAME projection, no second site).
            let collapsed =
                Ribbon.view resource Localization.English { Ribbon.init with collapsed = true }
                    cv UserEnvironment.defaults noopDispatch
            Assert.Contains(zoom, buttonLabels collapsed))

    // =======================================================================
    // AC-D3 — the contextual Element tab appears on selection, appended beside.
    // =======================================================================

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-D3 selecting an element appends the contextual Element tab beside the persistent tabs`` () =
        let noSel = model [ sampleAt 0.0 0.0 ]
        Assert.Equal<Ribbon.RibbonTab list>(Ribbon.persistentTabs, Ribbon.visibleTabs noSel)
        Assert.DoesNotContain(Ribbon.ElementTab, Ribbon.visibleTabs noSel)
        let selected = { noSel with selection = ConstructorView.ElementSelected 0 }
        // The contextual Element tab is APPENDED (after) the persistent tabs — not taking over.
        Assert.Equal<Ribbon.RibbonTab list>(Ribbon.persistentTabs @ [ Ribbon.ElementTab ], Ribbon.visibleTabs selected)

    // =======================================================================
    // AC-D4 — bilingual labels (EN/RU) + functional language switch.
    // =======================================================================

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-D4 ribbon tab labels resolve under both English and Russian`` () =
        for tab in Ribbon.allTabs do
            Assert.False(String.IsNullOrWhiteSpace(Ribbon.tabTitle resource Localization.English tab))
            Assert.False(String.IsNullOrWhiteSpace(Ribbon.tabTitle resource Localization.Russian tab))
        // The Russian label is a distinct translation (Cyrillic runs longer), so the
        // layout reads the active-language string rather than a fixed one (D.4).
        Assert.NotEqual<string>(
            Ribbon.tabTitle resource Localization.English Ribbon.Build,
            Ribbon.tabTitle resource Localization.Russian Ribbon.Build)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-D4 setting the UI language updates the persisted env language`` () =
        let m0 = fst Shell.init
        Assert.Equal(Localization.English, m0.env.language)
        let m1, _ = Shell.update (Shell.RootMsg.Shell (Shell.SetLanguage Localization.Russian)) m0
        Assert.Equal(Localization.Russian, m1.env.language)

    // =======================================================================
    // AC-F1 — the catalogue roster (no analyzer; LP/CP → Polarizer; >2 polarizers).
    // =======================================================================

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-F1 the catalogue lists OT, LS, LP, CP, S, Lens, Flat Mirror, Curved Mirror and D with no analyzer`` () =
        Assert.Equal<CatalogueKind list>(
            [ LightSource; LinearPolarizer; CircularPolarizer; Sample; Lens; FlatMirror; CurvedMirror; Detector ],
            Ribbon.catalogueKinds)
        // The optical table is in the catalogue as the workspace role (not a placeable kind).
        Assert.Contains(Ribbon.TableRole, Ribbon.catalogue)
        // No catalogue kind maps to the engine Analyzer case (no analyzer entry, F.1.2/F.3).
        for kind in Ribbon.catalogueKinds do
            match toConstructorElement kind with
            | BeamTree.Analyzer -> Assert.Fail(sprintf "catalogue kind %A must not map to Analyzer" kind)
            | _ -> ()
        // LP and CP both map to Polarizer (no new DU case, F.3).
        match toConstructorElement LinearPolarizer with
        | BeamTree.Polarizer -> ()
        | other -> Assert.Fail(sprintf "Linear polarizer must map to Polarizer, got %A" other)
        match toConstructorElement CircularPolarizer with
        | BeamTree.Polarizer -> ()
        | other -> Assert.Fail(sprintf "Circular polarizer must map to Polarizer, got %A" other)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-F1 a chain may hold more than two polarizers`` () =
        // Drop three polarizers through the catalogue's RibbonDrop path.
        let m0 = model []
        let m1 = ConstructorView.update (ConstructorView.RibbonDrop (LinearPolarizer, at 0.0 0.0)) m0
        let m2 = ConstructorView.update (ConstructorView.RibbonDrop (CircularPolarizer, at 0.0 0.0)) m1
        let m3 = ConstructorView.update (ConstructorView.RibbonDrop (LinearPolarizer, at 0.0 0.0)) m2
        Assert.Equal(3, List.length m3.project.placements)
        for p in m3.project.placements do
            match toConstructorElement p.catalogueKind with
            | BeamTree.Polarizer -> ()
            | other -> Assert.Fail(sprintf "expected Polarizer, got %A" other)

    // =======================================================================
    // AC-F2 — the value-id binding action + working empty modal.
    // =======================================================================

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-F2 the element local menu exposes the value-id action and it opens a dismissible empty modal`` () =
        HeadlessSession.run (fun () ->
            let m0 = { model [ sampleAt 0.0 0.0 ] with selection = ConstructorView.ElementSelected 0 }
            // The element's local menu (the contextual Element tab) exposes the value-id
            // binding action (F.2.1) — assert the REAL rendered exposure path: the
            // bind-value button is present on the Element tab the shell shows.
            let bindLabel = Localization.lookup resource Localization.English "valueId.action"
            let elementTab =
                Ribbon.view resource Localization.English { Ribbon.init with activeTab = Ribbon.ElementTab }
                    m0 UserEnvironment.defaults noopDispatch
            Assert.Contains(bindLabel, buttonLabels elementTab)
            Assert.False m0.valueIdModalOpen
            // The action opens a working modal...
            let opened = ConstructorView.update ConstructorView.OpenValueIdModal m0
            Assert.True opened.valueIdModalOpen
            // ...Close dismisses it (a fully working, dismissible modal)...
            let closed = ConstructorView.update ConstructorView.CloseValueIdModal opened
            Assert.False closed.valueIdModalOpen
            // ...and Esc / cancel also dismisses it (it folds into CancelOrDeselect).
            let dismissed = ConstructorView.update (ConstructorView.KeyPress (key Escape)) opened
            Assert.False dismissed.valueIdModalOpen)

    // =======================================================================
    // Surfaceless-button guard — the commands with no visible front-door effect render
    // DISABLED in both the ribbon and the collapsed menus rather than as silent no-op clicks.
    // The remaining surfaceless commands are gesture-only / parameterized commands
    // (drag/pan/zoom/drop/group controls). Element-edit and rotation commands are
    // parameterless-invokable because they now render real surfaces or apply one step.
    // =======================================================================

    /// The disabled-button set, DERIVED from the one source of truth in `ConstructorView`
    /// (`commandsWithoutFrontDoorSurface`), so this test can never drift from the predicate
    /// the ribbon reads. The ribbon must render exactly these disabled.
    let private surfacelessCommands : Command list = ConstructorView.commandsWithoutFrontDoorSurface

    /// The four element-edit commands routed to the contextual Element tab. Slice 007 renders
    /// their front-door overlays (element dialog, right-click context menu, same-row confirm gate),
    /// so they are RE-ENABLED — no longer surfaceless, and parameterless-invokable.
    let private reEnabledElementCommands : Command list =
        [ OpenElementDialog; ElementContextMenu; ResetRotation; DeleteElement ]

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``the surfaceless commands are not parameterless-invokable; every other registry command is`` () =
        for cmd in surfacelessCommands do
            Assert.False(ConstructorView.isParameterlessInvokable cmd, sprintf "%A has no front-door surface (must render disabled)" cmd)
        let registryCommands = Commands.registry |> List.map (fun d -> d.command)
        for cmd in registryCommands do
            if not (List.contains cmd surfacelessCommands) then
                Assert.True(ConstructorView.isParameterlessInvokable cmd, sprintf "%A must be parameterless-invokable" cmd)
        // The four element-edit commands are RE-ENABLED this slice (their overlays now render),
        // so they are NOT in the surfaceless set and ARE parameterless-invokable.
        for cmd in reEnabledElementCommands do
            Assert.False(List.contains cmd surfacelessCommands, sprintf "%A now has a rendered front-door surface" cmd)
            Assert.True(ConstructorView.isParameterlessInvokable cmd, sprintf "%A must be parameterless-invokable" cmd)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``the ribbon renders gesture-only commands disabled and invokable ones enabled`` () =
        HeadlessSession.run (fun () ->
            // The Trace/View tab carries Pan (gesture-only), Zoom (gesture-only) and Reset
            // view (parameterless-invokable) — a tab that mixes both kinds.
            let cv = model [ sampleAt 0.0 0.0 ]
            let window = Window()
            window.Content <-
                Component(fun _ ->
                    Ribbon.view resource Localization.English { Ribbon.init with activeTab = Ribbon.TraceView }
                        cv UserEnvironment.defaults noopDispatch)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let enabledByLabel =
                window.GetVisualDescendants()
                |> Seq.choose (fun v ->
                    match v with
                    | :? Button as b -> (match b.Content with | :? string as s -> Some (s, b.IsEnabled) | _ -> None)
                    | _ -> None)
                |> Map.ofSeq
            let labelOf cmd = LocalHelp.commandLabel resource Localization.English cmd
            // The gesture-only Pan/Zoom render as DISABLED buttons (discoverable, not no-ops)...
            Assert.Equal(Some false, Map.tryFind (labelOf PanView) enabledByLabel)
            Assert.Equal(Some false, Map.tryFind (labelOf ZoomView) enabledByLabel)
            // ...while the invokable Reset view stays enabled and clickable.
            Assert.Equal(Some true, Map.tryFind (labelOf ResetView) enabledByLabel)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``the contextual Element tab renders the re-enabled element-edit commands enabled`` () =
        HeadlessSession.run (fun () ->
            // The contextual Element tab (an element selected) carries the four element-edit
            // commands — Element dialog, Element menu, Reset rotation, Delete — which slice 007
            // RE-ENABLES (their front-door overlays now render: the element dialog, the right-click
            // context menu, and the same-row confirm gate). They must render ENABLED and clickable.
            let cv = { model [ sampleAt 0.0 0.0 ] with selection = ConstructorView.ElementSelected 0 }
            let window = Window()
            window.Content <-
                Component(fun _ ->
                    Ribbon.view resource Localization.English { Ribbon.init with activeTab = Ribbon.ElementTab }
                        cv UserEnvironment.defaults noopDispatch)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let enabledByLabel =
                window.GetVisualDescendants()
                |> Seq.choose (fun v ->
                    match v with
                    | :? Button as b -> (match b.Content with | :? string as s -> Some (s, b.IsEnabled) | _ -> None)
                    | _ -> None)
                |> Map.ofSeq
            let labelOf cmd = LocalHelp.commandLabel resource Localization.English cmd
            // Each of the four re-enabled element-edit commands renders as an ENABLED button...
            for cmd in reEnabledElementCommands do
                Assert.Equal(Some true, Map.tryFind (labelOf cmd) enabledByLabel)
            // ...alongside another invokable Element-tab command (Duplicate).
            Assert.Equal(Some true, Map.tryFind (labelOf DuplicateElement) enabledByLabel)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``CloseHelp dismisses the help overlay without deselecting the active element`` () =
        // Open help over an active element, then close it via the dedicated CloseHelp
        // message: the overlay clears but the selection / contextual Element tab survive
        // (Invoke CancelOrDeselect used to silently deselect on help close).
        let m0 = { model [ sampleAt 0.0 0.0 ] with selection = ConstructorView.ElementSelected 0 }
        let helpOpen = ConstructorView.update (ConstructorView.Invoke Commands.LocalHelp) m0
        Assert.True helpOpen.helpOpen
        Assert.Equal(ConstructorView.ElementSelected 0, helpOpen.selection)
        let closed = ConstructorView.update ConstructorView.CloseHelp helpOpen
        Assert.False closed.helpOpen
        Assert.Equal(ConstructorView.ElementSelected 0, closed.selection)
        Assert.Equal<Ribbon.RibbonTab list>(Ribbon.persistentTabs @ [ Ribbon.ElementTab ], Ribbon.visibleTabs closed)

    // =======================================================================
    // D.5 / D.6 — the constructor front door + the Legacy entry.
    // =======================================================================

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``D6 the constructor is the default landing page`` () =
        Assert.Equal(Shell.Page.Constructor, (fst Shell.init).page)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``D5 the Legacy entry navigates to the legacy dockable screen and back`` () =
        let m0 = fst Shell.init
        let m1 = fst (Shell.update (Shell.RootMsg.Shell (Shell.Navigate Shell.Page.Legacy)) m0)
        Assert.Equal(Shell.Page.Legacy, m1.page)
        let m2 = fst (Shell.update (Shell.RootMsg.Shell (Shell.Navigate Shell.Page.Constructor)) m1)
        Assert.Equal(Shell.Page.Constructor, m2.page)

    // =======================================================================
    // ui-smoke — the constructor front door (ribbon + canvas + open modal) renders.
    // =======================================================================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the constructor front door with the value-id modal renders one frame`` () =
        HeadlessSession.run (fun () ->
            let cv = { (model [ sampleAt -0.3 0.0; sampleAt 0.3 0.0 ]) with selection = ConstructorView.ElementSelected 0; valueIdModalOpen = true }
            let m = { (fst Shell.init) with page = Shell.Page.Constructor; constructor = cv }
            let window = Window()
            window.Content <- Component(fun _ -> Shell.view m ignore)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(window.IsVisible)
            window.Close())
