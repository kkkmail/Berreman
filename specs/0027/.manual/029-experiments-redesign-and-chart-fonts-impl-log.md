# 0027-029 — Experiments redesign (element → variable → capture → collection) + chart-window fonts (impl log)

Implements `028-comments.txt`. Two independent deliverables:

1. **Experiments tab** — replace the confusing "Rotate R1 / Sweep R2 / Sweep λ" kind picker with a proper
   multi-step, editable experiment builder over the live setup, backed by a persistent collection
   (Add / Edit / Remove), with the varied quantity **constrained by the chosen element** (as data, not
   hard-coded) and an editable **T / R / both** capture that removes the old hard-coded T-only / mirror-R.
2. **View-chart screen fonts** — the pop-out `ChartWindow`'s `Font −/+` now resizes a **selectable** target
   (header / axis labels / tick labels / legend), each with its own size, and the current size is **shown**.

**Result:** full solution builds clean (x64, `--warnaserror+:25`). `OpticalConstructor.Tests` **301/301**
(was 287, +14); `OpticalConstructor.Ui.Tests` **277/277** (was 268, +9). No gate regressed.

---

## Part 1 — the Experiments domain (`OpticalConstructor.Domain/ElementId.fs`, module `Experiments`)

The old `Experiment` DU (`RotateR1FullCircle | SweepR2 | SweepWaveLength of ElementId`) and the
`ExperimentSet` / `SetupStep` "sets" model are gone; the word *sweep* is gone with them. The new domain,
every primitive elevated:

- **`VariableParameter`** = `VaryWaveLength | VaryR1 | VaryR2` (a DU, with `.code` / `.label` / `.unitLabel`).
- **`variablesFor : CatalogueKind -> VariableParameter list`** — the element's varyable quantities declared
  **as data, keyed off the kind** (a light source → wavelength only; a linear/circular polarizer → R1 only;
  a sample → R1 and R2; lens / mirror / detector → nothing yet). This is the "attached to the element, not
  hard-coded" requirement: the Experiments bay *reads* this table rather than baking the constraint into the
  UI. Adding a kind's capability later is a one-line data edit here.
- **`MeasurementMode`** = `CaptureTransmitted | CaptureReflected | CaptureBoth` (with `.capturesTransmitted`
  / `.capturesReflected` / `.label` and **`ofEmission : Placement.Emission -> MeasurementMode`**). This is
  the T/R/both fix: `ofEmission` defaults a varied **mirror** to R (its `EmitReflectedOnly`) and everything
  else to both/T (from `defaultEmission`), so the previously hard-coded behaviour becomes an editable,
  emission-derived **default** the user can override.
- **`VariableRange` = { min; max; points }`** with `forVariable` defaults (R1 0…360°/73pts, R2 0…89°/91pts,
  λ 200…800 nm/91pts).
- **`ExperimentId of int`** — a *deterministic* monotonic id (not a Guid), so add/edit/remove are unit-
  testable without a clock.
- **`Experiment`** = `{ id; elementId; elementLabel; variable; measurement; range }` with a prose
  `.description`. It references its element by the serializable `ElementId` (survives save-load) and captures
  a human `elementLabel` at add-time so the collection row survives the element's removal.
- **`ExperimentDraft`** = the in-progress editor (`elementId`/`variable` optional until chosen; an
  `editingId` cursor) and **`ExperimentCollection` = { experiments; nextId; draft }`** with pure operations:
  `chooseElement` (defaults variable to the first allowed + range + emission-default capture), `chooseVariable`
  (resets the range), `chooseMeasurement`, `setRangeMin/Max/Points`, **`commit`** (from a fresh draft
  APPENDS and then leaves the draft EDITING the new one — so re-confirm updates in place and never
  duplicates; while editing it updates), **`edit`** (loads an experiment into the draft), **`remove`**,
  `newDraft`, `canCommit`.
- The mock **`ExperimentProxy`** seam is repurposed to `listExperiments` / `tryGetExperiment` over seed
  template `Experiment`s (the App composition root still just builds it via `createInMemory` and injects it —
  wiring unchanged).

## Part 2 — the propagation (`OpticalConstructor.Domain/Propagation.fs`)

- New **`Branch = BranchTransmitted | BranchReflected`** + **`sampleMueller branch`** (reads the engine's
  `muellerMatrixT ()` / `muellerMatrixR ()` — the reflected branch is the existing engine, no new physics);
  `sampleMuellerT` now delegates and **`sampleMuellerR`** is its counterpart.
- **`rotatingAnalyzerCurveRange lo hi`** for a user-editable R1 range; `rotatingAnalyzerCurve` is the
  0…360° special case (so the Malus proof is unchanged).
- The incidence / wavelength sweeps (`r2SweepCurve`, `r2SweepPsiDelta`, `waveLengthSweepIntensity`,
  `waveLengthSweepPsiDelta`) take a leading **`branch`** and an explicit **range** so the host can drive T,
  R, or both over the experiment's range.

## Part 3 — the Experiments bay (`OpticalConstructor.Controls/ExperimentControls.fs`)

Rewritten, still **domain-free**. The view is now: a readout, **1. Element to vary** (candidate boxes),
**2. Vary** (variable boxes, constrained to the element's allowed set — a hint when the element has nothing
to vary), **3. Capture** (T / R / both boxes), a **range** row (min / max / points, unit-labelled by the
variable), an **Add experiment / Update experiment** button + **New**, the persistent **Experiments** list
(each row: description → click to Edit, highlighted while editing, + a **Remove** button), and the inline
chart + Ψ/Δ readout + description. Domain-free mirror DUs `VariableChoice` / `MeasurementChoice` (+ code/
label helpers), an `ExperimentRow`, and a `Handlers` record (`chooseElement`, `chooseVariable`,
`chooseMeasurement`, `setRangeMin/Max/Points`, `addOrUpdate`, `newExperiment`, `editExperiment`,
`removeExperiment`, `openChartWindow`). Stable ids centralised in `UiIds`.

## Part 4 — the host wiring (`OpticalConstructor.TestWindows/TableAndElementRotationView.fs`)

- The Model drops `chosenSwept` / `experimentKind` / `lambdaRange` for one **`experimentCollection :
  Experiments.ExperimentCollection`**. The `Msg` gains `ExpChooseElement / ExpChooseVariable /
  ExpChooseMeasurement / ExpSetRangeMin|Max|Points / ExpCommit / ExpNew / ExpEdit / ExpRemove` (all thin
  wrappers over the pure collection ops); `ExpChooseElement` reads the element's kind → `variablesFor` and
  its `emission` → `MeasurementMode.ofEmission` for the defaults.
- **`experimentResult`** is recomputed from the current **draft**: `VaryR1` → the rotating curve over the
  chosen R1 range (or single-point Ψ/Δ for an ellipsometer); `VaryR2` → incidence sweep (clamped below 90°,
  drawn to 90); `VaryWaveLength` → λ sweep. The **capture** selects the branch(es): `CaptureBoth` yields one
  series per branch (`Intensity (T)` / `Intensity (R)`, or Ψ/Δ per branch); an intensity vs ellipsometer
  detector picks intensity vs Ψ/Δ series. `experimentState` projects the draft + collection into the bay
  state (now public for unit tests, like `experimentResult`). The double-click → pop-out `ChartWindow` hook
  is unchanged.

## Part 5 — the chart-window fonts (`ExperimentChart.fs` + `ChartWindow.fs`)

- New pure **`ChartFont`** module: `ChartFontTarget = Title | AxisLabels | TickLabels | Legend`, a
  `ChartFontState` holding a **per-target** size + the selected target, with `sizeOf` / `selectedSize` /
  `withSelected` / `withSize` / `bumpSelected` (clamped to 6…40 pt) / `readout`. Kept pure (`float`, no
  ScottPlot) so the selection / clamp logic is unit-tested without a native window.
- `ChartWindow` gains a **target selector** (`ComboBox`, one item per target) and a **size readout**
  TextBlock; `Font −/+` now `bumpSelected` the *selected* target and `applyFonts` pushes **all four** sizes
  onto ScottPlot — the title (`Axes.Title.Label.FontSize`), both axis labels (`AxisBase.Label.FontSize`),
  both **tick-label** styles (`AxisBase.TickLabelStyle.FontSize`), and the **legend** (`Legend.FontSize`).
  So header and tick labels are now resizable (they weren't), each independently, with the size shown.

## Tests (+23 across the two suites)

- **`OpticalConstructor.Tests`** (+14): `PropagationTests` updated for the branch/range signatures and gains
  `sampleMuellerR` physicality, the `sampleMueller` branch selector, and the `rotatingAnalyzerCurveRange`
  sub-range / 0…360 equivalence. `ExperimentProxyTests` rewritten for the new domain: `variablesFor` per
  kind, `MeasurementMode.ofEmission` (mirror → R), the ranges, and the full collection lifecycle
  (choose → commit appends → re-commit no-dup → edit updates in place → newDraft appends distinct →
  remove clears the editing draft), plus the proxy stub seam.
- **`OpticalConstructor.Ui.Tests`** (+9 net): `ExperimentControlsTests` rewritten for the new bay/flow (the
  element→variable constraint, capture default from a mirror, range round-trip, Add/Edit/Remove, the
  measurement-aware `experimentResult` branches incl. `CaptureBoth` → two series, and headless proofs that
  the bay draws the intensity polyline, lists candidates + a click picks the element, and clicking **Add**
  grows the collection). New **`ChartFontTests`** covers the per-target select / bump / clamp / readout, and
  the `ChartWindow` smoke test asserts the font-target selector + size readout are present.

## Files

- Changed: `OpticalConstructor.Domain/{ElementId,Propagation}.fs`;
  `OpticalConstructor.Controls/ExperimentControls.fs`;
  `OpticalConstructor.TestWindows/{ExperimentChart,ChartWindow,TableAndElementRotationView}.fs`;
  `OpticalConstructor.Tests/{ExperimentProxyTests,PropagationTests}.fs`;
  `OpticalConstructor.Ui.Tests/{ExperimentControlsTests}.fs` + `.fsproj`.
- New: `OpticalConstructor.Ui.Tests/ChartFontTests.fs`.

## Decisions / interpretations (recorded per the arc convention)

- **"Attached to the element, not hard-coded"** — modelled as `Experiments.variablesFor` (a single central
  data table keyed by `CatalogueKind`), which the bay *reads*. The capability lives with the domain element
  model, not scattered in the experiment UI; adding a kind's capability is a one-line data change.
- **T / R / both default** — taken from the *varied element's* `Emission` (mirror → R, others → both/T),
  editable via the Capture selector. This directly repurposes the existing `Placement.Emission` DU the spec
  refers to.
- **Experiment identity** — a deterministic `ExperimentId of int` (monotonic counter in the collection),
  chosen over a Guid so the add/edit/remove semantics are unit-testable and reproducible.
- **"View chart screen"** — interpreted as the pop-out interactive `ChartWindow` (the only screen with the
  `Font −/+` controls in this arc). The V1 `Ui/ChartView.fs` has no such controls and was left untouched.
- **Deferred (unchanged from before):** real disk-backed Storage proxies, the sample editor (Material →
  Sample), non-ideal LP/CP/detectors, dual R+T analyzer *arms* in the scene, and 2-D experiments — all
  future, compiler-guided additions on top of this.
