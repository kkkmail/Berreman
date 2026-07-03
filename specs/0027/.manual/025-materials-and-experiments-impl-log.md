# 0027-025 — Materials & Experiments (spec 024) phases 1–4 (ellipsometer only) — implementation log

Implements `024-materials-and-experiments-spec.md`, phases 1–4 with phase 4 = **ellipsometer only**. All
data is **mocked** (in-memory functional-proxy seams; no disk persistence — that stays out of scope). Built
via a 6-stage multi-agent **workflow** (a concrete design/blueprint stage, then the four phases sequentially,
then verification — each phase reading the previous phase's code and building + testing to green).

**Result:** full solution builds clean; `OpticalConstructor.Ui.Tests` **245/245** (was 224, +21),
`OpticalConstructor.Tests` **272/272** (was 245, +27). Six ribbon bays now: Rotation / Move / Add / Render /
**Library** / **Experiments**.

## Phase 1 — the Library bay

`OpticalConstructor.Domain/ElementId.fs` → `module Library`:

- **`ElementId`** — an elevated, serializable single-case DU (`ElementId of string`, `.value`); every table
  element now carries its own `id` (distinct from `placement.valueId`), so experiments reference elements by
  id, never by an in-memory ref (spec §1/§5).
- The choosable, kind-constrained things: **`Material`** (keeps the engine dispersive tensor), **`Sample`**
  (a cut-out plate: materialId + thickness + `SubstrateKind = ThinFilm | Plate | Wedge`), **`SourcePreset`**
  (λ), **`DetectorPreset`** (`DetectorKind = Intensity | Ellipsometer`), **`PolarizerPreset`**
  (`PolarizerKind = IdealLinear | IdealCircularLeft | IdealCircularRight`), unified in **`LibraryEntry`** with
  `entryId` / `displayName` / `forKinds` (a polarizer entry serves the LinearPolarizer or CircularPolarizer
  role per its kind). **`LibraryTree`** is a recursive `Group`/`Leaf` grouping — *one of possibly several*
  representations (spec R3); the proxy returns a `LibraryTree list`.
- **`LibraryProxy`** — the functional-proxy seam: a record of `Result`-returning camelCase functions
  (`entriesForKind`, `libraryTrees`, `tryGetEntry`) built by **`createInMemory`** over the seeded collection:
  glass(1.52)/glass(1.75) materials; samples **glass → single film → quarter-wave multilayer**; **Intensity**
  + **Ellipsometer** detectors; **1 ideal LP + 2 ideal CP (left/right)**; a 600 nm source.

`OpticalConstructor.Controls/LibraryControls.fs` — the bay (`State` / `Handlers` / `UiIds` / `view`), kind-
constrained rows for the selected element, generic (the host flattens entries to display rows, like
`ElementPaletteControls`). Selecting a row → the host sets the **selected element's `valueId`**.

Main scene (`TableAndElementRotationView.fs`): `BayNames.library`, the bay in `mainBays`, a `library :
Library.LibraryProxy` model field (injected), `BindValueId` wiring, element ids minted on add/seed. App
(`Program.fs`): the mock `LibraryProxy` is built at the composition root and passed via `initMainWith`.

## Phase 2 — the Experiments bay

`ElementId.fs` → `module Experiments`:

- **`Experiment = RotateR1FullCircle of ElementId`** — a **single-case DU** (spec Q4/Q8: "which element's R1
  makes a full circle"), so further experiment kinds are compiler-guided additions. **`ExperimentSet`** is an
  ordered list of `(SetupStep list * Experiment)` (setup steps reference elements by id, e.g.
  `SetElementR1 (ElementId, deg)`). **`ExperimentProxy`** (`listExperimentSets` / `tryGetExperimentSet`) +
  `createInMemory` mock with a couple of stored sets.
- `OpticalConstructor.Controls/ExperimentControls.fs` — the bay: pick the swept element **by id** from the
  elements currently present (with a readout). Main scene gains `experiments : ExperimentProxy` and
  `chosenSwept : ElementId option`; App injects the mock.

## Phase 3 — one experiment end-to-end (intensity / Malus)

`OpticalConstructor.Domain/Propagation.fs` — the pure **MM/SV propagation pipeline** (spec §1 Q3), reusing the
**existing engine** for the sample's Mueller matrix (`OpticalSystemSolver(info, system).muellerMatrixT()`),
never reinventing the physics:

- ideal-polarizer **input Stokes** (`inputStokes`: LP `[1; cos2θ; sin2θ; 0]`, CP `[1;0;0;±1]`) and ideal
  **analyzer Mueller** matrices (standard ½ forms, rotated by R1); `identityMueller` for an **absent** sample
  (spec R1 — nothing is synthesized); `sampleToSystem` maps a `Sample` to an engine `OpticalSystem` (the
  multilayer id keys a real three-film glass stack);
- `propagate svIn mmSample mmAnalyzer = MM_analyzer · (MM_sample · SV_in)`; intensity = the detector Stokes
  `S0`; `rotatingAnalyzerCurve` sweeps the analyzer R1 over 0…360° → an `(angleDeg, intensity)` curve. With an
  identity sample + ideal LP in/analyzer this is exactly Malus `I = I₀cos²θ`.

The Main scene runs it end-to-end (input polarizer entry → SV_in; sample entry → engine MM; detector kind →
intensity curve) and shows an **inline result** in the Experiments bay (the curve as a small polyline, 73
points / 5° — no Plotly/WebView).

## Phase 4 — ellipsometer (Ψ/Δ) only

`Propagation.fs`: **`PsiDelta`** (a record of `Angle`s) and `psiDeltaOfStokes` — the inverse of
`S1 = −cos2Ψ, S2 = sin2Ψ cosΔ, S3 = −sin2Ψ sinΔ` (`2Ψ = atan2(√(S2²+S3²), −S1)`, `Δ = atan2(−S3, S2)`);
`ellipsometerReadout`. When the bound detector is an **Ellipsometer**, the Main scene shows Ψ/Δ instead of an
intensity curve. Nothing else from the spec's "later" list (non-ideal LP/CP/detectors, dual R+T analyzers,
sample editor, real storage, 2-D) was implemented.

## Tests (+48)

- Domain (`OpticalConstructor.Tests`, +27): `LibraryProxyTests` (ElementId; `entriesForKind` is
  kind-constrained — LP role gets only the LP, CP role exactly the two CPs; `tryGetEntry`; every tree leaf
  resolves; `forKinds`; a **stub proxy of the same shape**), `ExperimentProxyTests` (the experiment names its
  swept element by id; seeded sets + steps; stub), `PropagationTests` (Stokes read-back; ideal LP/CP input;
  aligned passes full S0 / crossed ~0; the **rotating-analyzer Malus `cos²` curve**; `propagate`
  composition; the engine sample MM transmits ≤ the input; **Ψ/Δ round-trips** known pairs; ellipsometer
  readout finite/in-range; the multilayer maps to a real three-film stack).
- UI (`OpticalConstructor.Ui.Tests`, +21 across `LibraryControlsTests` / `ExperimentControlsTests`): bay
  state/UiIds, kind-constrained rows, the `BindValueId` binding (and rebind), the ribbon offering all **six**
  bays, and headless render/click proofs that a Library click binds the element's `valueId`.

## Files

- New: `OpticalConstructor.Domain/{ElementId,Propagation}.fs`;
  `OpticalConstructor.Controls/{LibraryControls,ExperimentControls}.fs`;
  `OpticalConstructor.Tests/{LibraryProxyTests,ExperimentProxyTests,PropagationTests}.fs`;
  `OpticalConstructor.Ui.Tests/{LibraryControlsTests,ExperimentControlsTests}.fs`.
- Changed: `OpticalConstructor.TestWindows/TableAndElementRotationView.fs` (ElementId on elements; Library +
  Experiments bays; `valueId` binding; `chosenSwept`; the end-to-end run + inline result);
  `OpticalConstructor.App/Program.fs` (mock proxies built at the composition root, `initMainWith`); the
  Domain / Controls / Ui.Tests `.fsproj`s.

## Notes / deferred (per spec §6 "later")

- Real disk-backed proxies (`OpticalConstructor.Storage`) replace the mocks later — the bay/scene logic is
  unchanged by that swap (the whole point of the proxy seam).
- The Library renders one tree representation now (the model already allows more); the **sample editor**
  (Material → Sample), non-ideal LP/CP/detectors, dual R+T analyzer arms, experiment-set setup-step
  execution in the UI, and 2-D experiments are future `Experiment`/case additions (compiler-guided).
