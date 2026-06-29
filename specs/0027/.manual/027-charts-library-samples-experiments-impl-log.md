# 0027-027 — Charts, Library element-view, more samples, R2/λ experiments (impl log)

Implements `026-task.txt` (four parts). Built via a 6-stage multi-agent **workflow** (design/blueprint, then
Samples → R2/λ Experiments → Charts → Library element-view, then verification — each phase reading the
previous phase's code and building + testing to green). All data stays **mocked** (the in-memory
functional-proxy seams from 024/025).

**Result:** full solution builds clean; `OpticalConstructor.Ui.Tests` **268/268** (was 245, +23),
`OpticalConstructor.Tests` **287/287** (was 272, +15). The Main screen now has **seven** ribbon bays:
Rotation / Move / Add / Render / Library / Experiments / **Details**.

## Part 1 — more samples (the FSX comparison set)

`ElementId.fs` (`module Library`) gains a curated set of DISTINCT useful samples mined from
`Analytics/Examples/` (excluding the wedges, `Work_01.fsx`, and overlapping legacy `.m` — the legacy
`legacy/examples/*.m` are plain text but duplicate the engine presets): a **glass / vacuum interface**, a
**single glass film**, the **41-layer λ/4 glass/vacuum multilayer**, an **EUV Mo/Si 100-pair multilayer**, a
**uniaxial** and a **biaxial** crystal film, an **active / gyrotropic crystal plate**, and a **dispersive
Langasite-on-Silicon** film. Each `Sample` now carries a human `description : string` (materials +
thicknesses, used by the Details view and the Library confirm step), and the seed tree groups them
(Samples → Glass / Multilayers / Crystals / Dispersive). `Propagation.sampleToSystem` maps every sample id
to a real engine `OpticalSystem` from the `OpticalProperties` presets (`transparentGlass*`,
`uniaxialCrystal`/`biaxialCrystal`, `euvMolybdenum`/`euvSilicon`, `planarCrystal`, dispersive
`Langasite`/`Silicon` evaluated at the run λ) — the sample Mueller matrices come from the existing engine,
unchanged.

## Part 2 — R2 and wavelength 1-D experiments

`Experiments.Experiment` grows from one case to three (compiler-guided): `RotateR1FullCircle of ElementId`
(the existing analyzer Malus sweep), **`SweepR2 of ElementId`** (vary the swept element's incidence / R2 over
0…89° — the engine cannot solve exactly 90°, so it is computed to **89** but the chart x-axis is drawn to
**90**), and **`SweepWaveLength of ElementId`** (vary λ over a user-editable range, default **200–800 nm**).
`Propagation.fs` gains the per-point sweep primitives (the sample Mueller re-solved at each incidence /
wavelength; intensity through an optional analyzer; Ψ/Δ of the sample output). The Main scene's
`experimentResult` now returns an **`ExperimentChart`** for each kind, with an `Ellipsometer` detector
yielding **two** series (Ψ and Δ vs the swept variable) and an `Intensity` detector one. The Model gained
`experimentKind` + `lambdaRange`; the Experiments bay (`ExperimentControls`) gained an experiment-**kind**
selector (Rotate R1 / Sweep R2 / Sweep λ) and editable λ min/max — kept generic (the host supplies the kind +
range), with the chart rendering moved out of the bay into the scene.

## Part 3 — charts: axes + the interactive pop-out window

New **`ExperimentChart.fs`** (TestWindows) is the renderer-neutral chart data model:
`ChartSeries { name; points }` and `ExperimentChart { series; xLabel; yLabel; title; description }`.

- **Inline chart** (rendered by the scene from the `ExperimentChart`): now has **axes**, axis **labels**
  (from xLabel/yLabel), **major gridlines**, a small legend for the two-series ellipsometer case, and the
  prose **description** beneath it.
- **Double-click** the inline chart opens a separate, interactive **`ChartWindow.fs`** hosting a
  **`ScottPlot.Avalonia.AvaPlot`** (ScottPlot 5.1.59 — the app's existing interactive engine; added to the
  TestWindows `.fsproj`). It gives axes + labels + major **and** minor gridlines and zoom/pan for free, plus:
  a **crosshair + marker that snaps to the nearest curve point on mouse-move and shows its x & y values**
  (pixel → coordinate → nearest data point), a toolbar with **font-size +/-**, **minor / major gridline
  toggles**, **PNG export** (`Plot.SavePng`) and **CSV export** (x,y rows), and the **description** under the
  plot. The double-click → window is wired through a guarded `openChartWindowHook` (the window is IO, opened
  from the host; the headless test host never opens it).

The description is built from the bound elements + what is swept (e.g. "Intensity vs analyzer R1; source
600 nm, glass sample, ideal LP analyzer").

## Part 4 — Library element-view (confirm + description + the layer stack)

- **Assigning is confirm-gated.** `LibraryControls` now makes a clicked entry **pending**, shows its **full
  description** (e.g. the multilayer's materials + thicknesses), and offers **Confirm** (commits the bind,
  setting the element's `valueId`) / **Cancel** — so you see what a "Quarter-wave multilayer" actually is
  before assigning it.
- **New "Details" bay** (after Experiments, the 7th bay) shows the selected element's bound entry: its title,
  full description, and — for a sample — a **layer-stack band view**. New reusable **`LayerBandsControls.fs`**
  (Controls, domain-free) draws the stack as a vertical column of coloured rectangles sized by each layer's
  thickness and labelled with material + thickness — borrowing V1's `Schematic.fs`/`ResultsView.fs` idea but
  as a reusable Lego component. The scene flattens a sample's engine `OpticalSystem` into the bands and
  **collapses runs of identical layers** (so the 41-layer / 100-pair multilayers read as "glass ×21 / vacuum
  ×20", etc.).

## Tests (+38)

- Domain (`OpticalConstructor.Tests`, +15): every seeded sample resolves to a finite, ≤-input-intensity
  Mueller matrix and a non-empty description (`PropagationTests`); the R2 sweep is monotone with its last x
  near 89 (axis to 90) and the λ sweep spans the chosen range; an ellipsometer sweep yields two series;
  `Library`/`Experiment` proxy tests extended for the new entries / DU cases.
- UI (`OpticalConstructor.Ui.Tests`, +23): the Library **confirm flow** (select-pending → Confirm binds
  `valueId`; Cancel does not; the pending description shows); the Experiments **kind selector** + λ-range;
  a headless smoke that the **`ChartWindow` opens and renders** for a sample `ExperimentChart`; the inline
  chart's axis labels come from the `ExperimentChart`; new **`LayerBandsControlsTests`** (the band view
  renders a stack, collapses identical runs); and the ribbon now offering **seven** bays incl. Details.

## Files

- New: `OpticalConstructor.TestWindows/{ExperimentChart,ChartWindow}.fs`;
  `OpticalConstructor.Controls/LayerBandsControls.fs`;
  `OpticalConstructor.Ui.Tests/LayerBandsControlsTests.fs`.
- Changed: `OpticalConstructor.Domain/{ElementId,Propagation}.fs` (samples + descriptions; the two new
  experiment DU cases + sweep primitives + `sampleToSystem`); `OpticalConstructor.Controls/{ExperimentControls,
  LibraryControls}.fs` (kind selector + λ range; confirm-gated binding); `OpticalConstructor.TestWindows/
  TableAndElementRotationView.fs` (`experimentKind`/`lambdaRange`; `experimentResult` → `ExperimentChart`;
  inline chart axes/gridlines/description; double-click → `ChartWindow`; the Details bay); the Controls /
  TestWindows / Ui.Tests `.fsproj`s; the Experiment / Library / Propagation / ExperimentControls /
  LibraryControls tests.

## Notes

- The interactive engine is **ScottPlot.Avalonia** (the app already used it for 1-D plots); **Plotly/WebView2
  is deferred** (its WebView host is unavailable on net10.0), so the pop-out window is ScottPlot, not Plotly.
- Still mocked / deferred per the spec: real disk-backed Storage proxies, the sample editor (Material →
  Sample), non-ideal LP/CP/detectors, dual R+T analyzer arms, and 2-D experiments — all future, compiler-
  guided additions on top of this.
