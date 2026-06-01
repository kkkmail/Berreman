# Reuse critique -- 007.slice-md cycle 1

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` (focused on `Berreman/OpticalConstructor/OpticalConstructor.Ui` and `OpticalConstructor.Ui.Tests`; the slice touches nothing else).
- Files inspected: ~24/200 (the new `FitView.fs` + `FitPanelTests.fs`, the `Shell.fs` diff, and the slice-002..006 sibling view/test modules they should reuse from).
- Extensions: `.fs` view/test sources (the diff is pure F#; `.md`/`.json` carry no reusable code for this slice).

## Findings

### F1: `Shell.workingSystemOf` re-implements `ResultsView.activeSystem` byte-for-byte

- **Worker added:** `workingSystemOf (model : RootModel)` at `Berreman/OpticalConstructor/OpticalConstructor.Ui/Shell.fs:200`–205.
- **Existing helper:** `activeSystem (ws : WS.Model)` at
  `Berreman/OpticalConstructor/OpticalConstructor.Ui/ResultsView.fs:51`–55, delivered by slice 006.
- **Why it matters:** The two bodies are identical modulo the input wrapper — both are "the active-index system, else the head system, else `None`":

  ```fsharp
  // ResultsView.activeSystem
  let systems = ws.project.systems
  match ws.active with
  | Some i when i >= 0 && i < List.length systems -> Some systems.[i]
  | _ -> match systems with | s :: _ -> Some s | [] -> None

  // Shell.workingSystemOf
  let systems = model.workspace.project.systems
  match model.workspace.active with
  | Some i when i >= 0 && i < List.length systems -> Some systems.[i]
  | _ -> match systems with | s :: _ -> Some s | [] -> None
  ```

  `workingSystemOf` reads only `model.workspace`, so it is `activeSystem model.workspace`
  expanded inline. The active-selection convention (§J.5: "the workspace owns the
  active/visible selection") now has two copies; a future change to the fallback
  rule (e.g. honour a visible-but-inactive system) has to be made in two places, and
  the schematic (slice 006) and the fit (this slice) could silently diverge on which
  system they operate over.
- **Suggested action:** Reuse the workspace selector. `ResultsView.activeSystem` is
  `private`, which is the only reason it was not reachable — promoting it to a public
  `Workspace`-layer helper (e.g. `Workspace.activeSystem : Model -> OpticalSystem option`)
  and calling it from both `ResultsView` and `Shell.workingSystemOf` removes the copy.
  Editing `Workspace.fs` is a frozen-core edit (§0.1), so the defensible alternative is
  to lift the selector into the composition root once and have `ResultsView` consume it;
  either way the duplication is the judge's call, but it is a real near-miss, not noise.

### F2: `FitPanelTests` re-declares the `mount` / `buttonByContent` test scaffolding

- **Worker added:** `mount` (`FitPanelTests.fs:67`–72) and `buttonByContent`
  (`FitPanelTests.fs:74`–77), plus a new `hasButton` (`FitPanelTests.fs:79`–82).
- **Existing helper:** identical `mount` and `buttonByContent` in
  `ResultsPanelTests.fs:33`/`:45`, `SourcePanelTests.fs:33`/`:45`,
  `MaterialsPanelTests.fs:42`, and `ChartPanelTests.fs:23` — verified byte-identical
  against `SourcePanelTests.fs:33`–48.
- **Why it matters:** This is direct duplication of headless-mount + visual-tree
  button-lookup scaffolding that now lives in five panel-test files. A change to how a
  panel is mounted headlessly (e.g. a `RunJobs` ordering fix) must be applied in every
  copy or the suite drifts. Note, however, that the suite **already** shares a real
  helper for the part that needs cross-file consistency — `HeadlessSession.run`, which
  every one of these files calls — so the precedent for a shared test-helper module
  exists; `mount`/`buttonByContent` simply were not put there.
- **Suggested action:** Lower-priority than F1 because the per-file copy is the
  established repo-wide convention (slices 002–006 each carry it), so re-spawning purely
  to fix it would be inconsistent with the rest of the suite. The clean move is to hoist
  `mount`/`buttonByContent`/`textBlocks`/`hasButton` next to `HeadlessSession` and have
  all six files consume them — but that is a suite-wide refactor touching frozen-passing
  test files, not a defect this slice introduced. Reasonable to leave as-is and document
  the convention; flag for a future test-helper consolidation pass.

### F3: the overlay-host shape and x-sample constant are copied from `ResultsView`

- **Worker added:** `overlaySection` at `FitView.fs:133`–143, which builds
  `let x = Analytics.StandardLightVariables.wavelength200to800Range 12` and wraps
  `ChartHosts.scottPlotHost plot` in a `Border` of `height 220.0`.
- **Existing helper:** `ResultsView.overlayHost` (`ResultsView.fs:171`–174) uses the same
  `wavelength200to800Range 12` x-sample, and `ResultsView.workspaceSection`
  (`ResultsView.fs:218`) wraps the host in the same `Border.height 220.0`.
- **Why it matters:** The fit-vs-measured overlay correctly reuses the slice-002
  `ChartHosts.scottPlotHost` and the Part-H `Plot1DView.renderComparison` (good — the
  hosting seam is not reinvented). What is copied is the *plot-window framing*: the
  magic `12`-sample wavelength range and the `220.0` overlay-Border height now appear in
  two view modules. If the project later standardises the comparison-plot resolution or
  the overlay band height, both sites drift independently.
- **Suggested action:** Minor. Reuse a single named constant for the comparison-overlay
  x-sample / Border height (a shared `let comparisonXSamples` / `overlayHeight` in
  `ChartHosts` or a small Ui constants module) rather than the literal in each view. Not
  re-spawn-worthy on its own; fold into F1/F2 cleanup if the judge routes a polish pass.

## Bottom line

The slice reuses the load-bearing seams correctly — the AvaPlot host (`ChartHosts`), the
frozen `SynthesisFitPage` progress/accept/overlay surface, `JobRunner.runIterations`,
and `LocalRefinement.refineWith` are all consumed unchanged, and the marshaling follows
slice 003's discipline without inventing a competing primitive. The only substantive
finding is F1, the byte-identical `activeSystem`/`workingSystemOf` pair, which is real
but blocked from clean reuse by an existing `private` modifier and the §0.1 freeze on
`Workspace.fs`. F2 and F3 are convention-level duplications the slice merely inherits
from the established pattern. My read: not substantive enough to bind a re-spawn — the
judge could fold F1 into a follow-up selector-consolidation rather than reject this diff.
