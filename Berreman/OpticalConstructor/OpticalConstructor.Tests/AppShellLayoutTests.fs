namespace OpticalConstructor.Tests

open OpticalConstructor.Ui
open OpticalConstructor.Ui.UserEnvironment
open Xunit

/// §J.8 — the surviving AppShell layout seam (spec 0038 Part B.1). The Elmish shell
/// was retired; what remains of `AppShell` is the theme seam (`themeVariant`, pinned
/// headlessly in Ui.Tests) and the Avalonia-free `setPanelVisible` reducer over the
/// persisted `PanelLayout` exercised here: a pure `panel-id -> visible -> layout ->
/// layout` function the environment round-trip persists (AC-J8), so the saved layout
/// stays editable and testable without a window.
module AppShellLayoutTests =

    let private layout : PanelLayout = { panels = defaultPanels }

    let private stateOf (panel : string) (l : PanelLayout) : PanelState =
        l.panels |> List.find (fun p -> p.panel = panel)

    [<Fact>]
    let ``hiding a panel flips exactly that panel to invisible`` () =
        let hidden = AppShell.setPanelVisible "results" false layout
        Assert.False((stateOf "results" hidden).visible)
        for p in hidden.panels do
            if p.panel <> "results" then Assert.True(p.visible, $"panel '{p.panel}' must stay visible")

    [<Fact>]
    let ``showing a hidden panel restores the original layout exactly`` () =
        let roundTripped =
            layout
            |> AppShell.setPanelVisible "chart" false
            |> AppShell.setPanelVisible "chart" true
        Assert.Equal(layout, roundTripped)

    [<Fact>]
    let ``an unknown panel id is a no-op`` () =
        Assert.Equal(layout, AppShell.setPanelVisible "no-such-panel" false layout)

    [<Fact>]
    let ``the reducer preserves the panel order`` () =
        let hidden = AppShell.setPanelVisible "materials" false layout
        Assert.Equal<string list>(
            layout.panels |> List.map (fun p -> p.panel),
            hidden.panels |> List.map (fun p -> p.panel))

    [<Fact>]
    let ``the target panel keeps its dock edge and size`` () =
        let before = stateOf "sources" layout
        let after = stateOf "sources" (AppShell.setPanelVisible "sources" false layout)
        Assert.Equal(before.dock, after.dock)
        Assert.Equal(before.size, after.size)

    [<Fact>]
    let ``panels other than the target are untouched entirely`` () =
        let hidden = AppShell.setPanelVisible "stack" false layout
        List.iter2
            (fun (a : PanelState) (b : PanelState) ->
                if a.panel <> "stack" then Assert.Equal(a, b))
            layout.panels
            hidden.panels

    [<Fact>]
    let ``an empty layout is a no-op`` () =
        let empty : PanelLayout = { panels = [] }
        Assert.Equal(empty, AppShell.setPanelVisible "stack" false empty)

    [<Fact>]
    let ``the reducer is idempotent`` () =
        let once = AppShell.setPanelVisible "chart" false layout
        Assert.Equal(once, AppShell.setPanelVisible "chart" false once)

    [<Fact>]
    let ``every panel carrying the target id updates, not just the first`` () =
        // The reducer's contract is "the panel with this id" over the WHOLE list —
        // if a stored layout ever carries a duplicated id, no row is silently skipped.
        let duplicated : PanelLayout =
            { panels =
                [
                    { panel = "chart"; dock = Center; size = 600.0; visible = true }
                    { panel = "chart"; dock = Right;  size = 320.0; visible = true }
                ] }
        let hidden = AppShell.setPanelVisible "chart" false duplicated
        Assert.True(hidden.panels |> List.forall (fun p -> not p.visible))

    [<Theory>]
    [<InlineData("stack")>]
    [<InlineData("materials")>]
    [<InlineData("sources")>]
    [<InlineData("chart")>]
    [<InlineData("results")>]
    let ``each built-in default panel can be hidden individually`` (panel : string) =
        let hidden = AppShell.setPanelVisible panel false layout
        Assert.False((stateOf panel hidden).visible)
        Assert.Equal(1, hidden.panels |> List.filter (fun p -> not p.visible) |> List.length)
