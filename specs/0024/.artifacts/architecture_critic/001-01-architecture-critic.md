# Architecture critique -- 001.slice-md cycle 3

## Summary

Clean foundation, and **cycle-2's open spec-fit gap is resolved**: attempt 03
reworked the smoke test so it now drives the *real* `App` lifetime headlessly —
`App.Initialize`'s theme seam runs in the session and is asserted via
`Application.Current.RequestedThemeVariant`, and a fresh `App` carrying a
`ClassicDesktopStyleApplicationLifetime` actually executes
`OnFrameworkInitializationCompleted` (AC-U1.3 / §U1.7). Of the prior cycle's three
flagged items, the only one still standing is documentation drift in the
`ui-tests.gates` descriptor; the dual-project coupling remains a (spec-mandated)
forward hazard for U2. Neither is a blocker.

## Layering

No violations, unchanged from cycle 2. `Shell.fs`/`ConstructionView.fs` (Ui)
depend only downward (`AppShell`, `ConstructionPage`, `Workspace`, `Charts`,
`UserEnvironment`, `Domain`, public FuncUI/Elmish); `Program.fs` (App) depends on
`Shell`, never the reverse. The clone stays unreferenced (§0.2) and the only new
packages are the two mandated public-NuGet `Avalonia.FuncUI.Elmish` / `Elmish`
(§U1.1). The test project references Ui + App in the correct direction and only
to boot the real composition root headlessly.

## Separation of concerns

The dispatcher stays thin: `update` (Shell.fs:139) delegates each case to the
matching sub-`update` and re-wraps, and `updateShell` (Shell.fs:130) routes every
shell edit through an existing pure `AppShell` reducer (`toggleTheme`,
`setPanelVisible`, `dockPanel`). The superseded `AppShell.panelView`/`shellView`
were removed there (R-4's allowed "supersede" branch), leaving `AppShell` holding
only the `toDock` seam + layout reducers. Soft note carried from cycle 2: the
dock-`Border` block in `Shell.panelView` (Shell.fs:192) restates the shape the
removed `AppShell.panelView` had; fine for one call site, but if U3–U6 each grow
their own panel chrome, factor a shared panel-frame helper rather than copying it.

## Consistency

The one live finding is the **`ui-tests` gate descriptor**
(`.gates/OpticalConstructor/ui-tests.gates:2`): it still advertises
"Avalonia.Headless.**XUnit** per-panel/page view tests" that assert "a simulated
interaction dispatches the expected RootMsg." The landed `PanelViewTests` does
neither — `.XUnit` was deliberately dropped (the documented xUnit-v2/v3 binding
conflict) and the test only asserts `>= 2` `"Layer "`-prefixed `TextBlock`s render,
with no interaction or `RootMsg` dispatch. The SoW records the gate files
pre-existed and were "not recreated," so this is inherited stale prose, and the
gate *command* (`--filter Category!=ui-smoke`) is correct and green. Since R-7
nominally makes this slice the owner of `ui-tests.gates`, a later slice should
either tighten the test toward the promise or correct the description. Everything
else is idiomatic: the `init`/`initFrom` split resolving the `mkProgram` `unit -> _`
mismatch, the `WS` module alias for the `Workspace` name clash, and the
xunit.v3 + `HeadlessUnitTestSession` choice (TestApp.fs) consistent with the
sibling `OpticalConstructor.Tests` framework.

## Spec fit

Strong, and now complete on the AC-U1.3 lifetime/theme half that cycle 2 flagged.
R-1..R-5 and R-7 land squarely: `RootMsg` carries exactly the U1-required
`Construction`/`Workspace`/`Shell` cases (Shell.fs:77-80), defers
`Fit`/`Source`/`Chart`/`Io`, and `update` attaches `Cmd.none` everywhere (the
non-requirement). R-4 routes the visible/ordered set through
`AppShell.visiblePanels` and dock edges through `AppShell.toDock`; R-5 renders the
`stack` panel from `StackEditor.layerRowLabels` (which composes `displayThickness`,
StackEditor.fs:218) with a total empty-stack fallback (ConstructionView.fs:31-43).
The attempt-03 smoke test (SmokeTests.fs) now asserts the theme seam ran on the
session `App` and drives `OnFrameworkInitializationCompleted` on a desktop
lifetime that sets + shows `MainWindow`, then renders each page body — so the
"approximation" I noted last cycle is closed. The documented reason a *fresh* App
is used (the session app's `ApplicationLifetime` is immutable post-init) is sound
and empirically verified per the Gotchas.

## Evolvability

The headline forward-risk persists and is unchanged by attempt 03: `RootModel`
embeds **both** `construction : ConstructionPage.Model` and `workspace : WS.Model`,
each holding its own `project`, both seeded from one `Templates.bandpassFilter ()`
result (Shell.fs:112-113). They agree today; the moment U2 wires `EditStack`
through `ConstructionPage.update`, `construction.project` advances while
`workspace.project` silently goes stale unless U2 reconciles them. The duplication
is **spec-mandated** — R-2 lists both as required fields and §0.1 freezes both
modules, so the worker correctly could not collapse them — but it is exactly the
"future slice cornered" hazard the rubric asks me to surface. U2 should designate
one tree canonical and push/re-derive to the other on every mutation;
`Shell.update`'s dispatcher is the natural fan-out point. Otherwise the surface is
a good base: `fit` is an on-demand `option`, and `ShellMsg` already defines
`ToggleTheme`/`SetPanelVisible`/`SetPanelDock` (Shell.fs:44) though the view wires
only `Navigate` — mild but justified forward scaffolding (`updateShell` handles
them totally through existing reducers). One small note: `initialProject`
(Shell.fs:94) is a module-level value evaluated at first `Shell` access, so a
throwing seed would surface as a `TypeInitializationException` rather than at a
call site; if a later slice makes the seed fallible, a `unit ->` function would
localize the failure.

## Risks

- **`[<ReferenceEquality>]` on `RootModel`** (Shell.fs:58): documented rationale
  is correct — FuncUI re-renders/diffs the view tree per dispatch and Elmish does
  not dedupe by model equality, and the embedded non-structural engine values
  leave no alternative. No action.
- **Two `App` instances in one headless session** (the session app plus the fresh
  desktop-lifetime app the smoke test creates) is sound and well-recorded, but is
  fragile against an Avalonia-headless upgrade that tightens single-application
  assumptions. Acceptable; flagging only so a future failure here is recognized
  quickly.

## Bottom line

I would ship this. Attempt 03 closed the cycle-2 spec-fit gap cleanly — the theme
seam and `OnFrameworkInitializationCompleted` are now genuinely exercised — and
the foundation remains disciplined: requirements met, clone unreferenced, reducers
reused not duplicated, model pure (§0.5) and layout public-only (§0.3). The two
residual items — the stale `ui-tests.gates` description and the spec-mandated
dual-project coupling — are follow-ups for later slices, not grounds to re-spawn.
The verdict is the judge's; my read is green.
