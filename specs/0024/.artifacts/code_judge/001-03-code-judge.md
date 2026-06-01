# Code judge -- 001.slice-md cycle 3

## Inputs read

- Slice spec: `C:\GitHub\Berreman\specs\0024\.slices\001.slice-md`
- State-of-the-world: `C:\GitHub\Berreman\specs\0024\.slices\001-state-of-the-world.md`
- Impl-log: `C:\GitHub\Berreman\specs\0024\.slices\001-impl-log.md`
- Gate results: build pass / unit-tests pass / constructor-unit-tests pass / impl-log-structure pass / state-of-world-structure pass / ui-smoke pass / ui-tests pass
- Critic critiques:
  - `C:\GitHub\Berreman\specs\0024\.artifacts\architecture_critic\001-01-architecture-critic.md`
  - `C:\GitHub\Berreman\specs\0024\.artifacts\reuse_critic\001-02-reuse-critic.md`

## Rationale

All seven gates in the slice roster are `pass`, and the diff lands the full
R-1..R-8 surface this CRITICAL gating slice owns. R-1 adds exactly the two
mandated public-NuGet packages (`Avalonia.FuncUI.Elmish` 1.6.0 + `Elmish`
4.3.0) to both the Ui and App fsprojs and nothing else (non-requirement
honoured). R-2 stands up `Shell.fs` in the `OpticalConstructor.Ui` project with
a `Page` DU of exactly `Construction`/`SynthesisFit`, a `RootModel` aggregating
the required sub-models, a `RootMsg` carrying exactly the U1-minimum
`Construction`/`Workspace`/`Shell` cases (deferring `Fit`/`Source`/`Chart`/`Io`),
and a thin dispatcher `update` that attaches `Cmd.none` to every case (the
explicit non-requirement). R-3 replaces the static mount with the Elmish
`Program.mkProgram … |> Program.withHost |> Program.run` seam from `MainWindow`,
seeded from `Startup.settings`. R-4/R-5 route the visible/ordered panel set
through the existing `AppShell.visiblePanels` / `setPanelVisible` / `dockPanel`
reducers and `toDock`, render the `stack` panel read-only from
`StackEditor.layerRowLabels`, and render the other four panels as titled
placeholders (R-8). R-6/R-7 add the headless `OpticalConstructor.Ui.Tests`
project (registered in `Berreman.slnx`), the `ui-smoke` smoke test, and the
`stack`-panel `ui-tests` view test.

The cycle-2 block is genuinely closed. Cycle 2 routed back because AC-U1.3 /
§U1.7 require the smoke test to prove the real `App` lifetime runs, yet the
prior `SmokeTests.fs` constructed `MainWindow()` directly and never drove
`OnFrameworkInitializationCompleted` or the `Initialize`/`AppShell.themeVariant`
theme seam. Attempt 03 reworked exactly that: `TestApp.fs`'s
`BuildAvaloniaApp` now points the shared `HeadlessUnitTestSession` at the real
`OpticalConstructor.App.App`, so `App.Initialize`'s theme seam runs and is
asserted via `Application.Current.RequestedThemeVariant`; the test then drives
`App.OnFrameworkInitializationCompleted` on a fresh `App` carrying a
`ClassicDesktopStyleApplicationLifetime`, asserting its `MainWindow` is set and
shown, and renders each page body one frame. The architecture critic — which
established the cycle-2 gap from the code — now states plainly "I would ship
this" and confirms "the cycle-2 spec-fit gap is closed." The documented reason a
fresh `App` is used (the session app's `ApplicationLifetime` is immutable
post-init) is sound and empirically verified per the Gotchas.

The new public surface is exercised by tests in the diff, satisfying the
done-green coverage criterion: `ui-smoke` drives the real lifetime/theme path
and renders each page; `ui-tests` (`PanelViewTests.fs`) asserts the `stack`
panel renders the selected node's layer rows (AC-U1.2). The SoW and impl-log
line up with the inspected diff — new `Shell.fs`, `ConstructionView.fs`, the
`OpticalConstructor.Ui.Tests` project (`TestApp.fs`/`SmokeTests.fs`/
`PanelViewTests.fs`), the `AppShell.fs` supersede that removed
`shellView`/`panelView` and made `toDock` public, the fsproj/`Program.fs` edits,
and the slnx registration. No layering violation, model pure per §0.5, layout
public-only per §0.3, clone unreferenced per §0.2.

Three residual items remain, none an unmet U1 requirement. (1) The
`ui-tests.gates` descriptor carries stale prose advertising
`Avalonia.Headless.XUnit` and a "simulated interaction dispatches the expected
RootMsg" assertion the landed `PanelViewTests` does not perform — inherited prose
in a pre-existing gate file the worker correctly did not recreate; the gate
*command* is correct and green, and dropping `.XUnit` is the well-documented,
defensible resolution of an unsatisfiable xUnit-v2/v3 binding conflict. R-7's
core obligation (a per-panel view test plus the gate file the `ui-tests` gate
depends on) is met; the prose is documentation drift for a later slice. (2) The
`RootModel` holding both `construction.project` and `workspace.project` from one
`Templates.bandpassFilter ()` seed is spec-mandated (R-2 lists both fields, §0.1
freezes both modules) and a forward hazard U2 owns, not a U1 defect. (3) The
reuse critic's F1 (headless-render scaffold duplicated between `SmokeTests` and
`PanelViewTests`) is a cheap, bounded follow-up cleanup, explicitly non-blocking.
Per the rubric, advisory findings are noted and the cycle moves on.

`cycles_remaining == 0`, so `route-back-to-worker` is not permitted; the choice
is `done-green` or `escalate-to-human`. The slice sits squarely on done-green
ground — every gate green, the cycle-2 binding gap closed, no unmet slice-spec
requirement, SoW/impl-log consistent with the diff, and all new public surface
test-covered — so escalation is unwarranted. Verdict: `done-green`.

## Verdict

done-green

```json
{"verdict": "done-green", "rationale": "All seven slice gates pass and R-1..R-8 land in full: exactly the two mandated public packages, the Shell.fs root MVU surface with the U1-minimum RootMsg cases and a Cmd.none dispatcher, the Elmish mount from MainWindow seeded from Startup.settings, panel-id dispatch through the existing AppShell reducers, the read-only stack panel via StackEditor.layerRowLabels, and the headless OpticalConstructor.Ui.Tests project with the ui-smoke and stack-panel ui-tests. The cycle-2 block is closed: attempt 03 reworked the smoke test to drive the REAL App lifetime headlessly -- TestApp.fs points the headless session at OpticalConstructor.App.App so App.Initialize's theme seam runs (asserted via Application.Current.RequestedThemeVariant), and OnFrameworkInitializationCompleted runs on a ClassicDesktopStyleApplicationLifetime that sets+shows MainWindow, satisfying AC-U1.3/SS-U1.7. New public surface is test-covered and SoW/impl-log match the inspected diff. The architecture critic now reads 'I would ship this'; both critics are green. Residual items -- stale ui-tests.gates descriptor prose, the spec-mandated dual construction/workspace project coupling, and the reuse critic's F1 test-scaffold duplication -- are advisory follow-ups for later slices, not unmet U1 requirements. With cycles_remaining=0 the slice is on done-green ground.", "retry_hint": ""}
```
