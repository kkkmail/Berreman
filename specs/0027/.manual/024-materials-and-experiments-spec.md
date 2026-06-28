# 0027-024 — Materials & Experiments: marrying the table with the Berreman solver (spec v2)

**Status: agreed direction** — incorporates the answers in `023-answers.txt`. Still a feature list / mini-spec
(no code yet); §7 records the decisions and the few residual questions. (Supersedes `022`.) No other files
changed.

---

## 1. Goal — the "marriage" and the compute model

The table (Lego scene: source → … → detector) is the UI; the Berreman code (BC) is the engine. A table +
an experiment drives a BC run and produces chart(s) + data.

**Compute model — start simple: Mueller-matrix (MM) / Stokes-vector (SV) propagation + wavelength** (per
Q3). The pipeline, in table order:

```
Source            ── defines the WAVELENGTH λ
Input LP / CP     ── defines the input STOKES VECTOR  SV_in
Sample            ── at (λ, incidence angle, sample rotation) Berreman computes the sample's MUELLER MATRIX
                       MM_sample  (for the T branch, the R branch, or both — per the sample's Emission)
SV propagation    ── SV_out = MM_sample · SV_in        (one SV per emitted branch: T, R, or both)
Analyzer (LP/CP)  ── placed AFTER the sample; SV_det = MM_analyzer · SV_out   (its MM rotates with its R1)
Detector          ── records what its TYPE permits (intensity = S0; ellipsometer = Ψ/Δ)  →  chart(s) + data
```

- The **sample's** MM comes from the existing engine (`Solution` → `EmField.muellerMatrix`,
  `FieldFunctions.fs`); the **LP / CP / analyzer** are MM elements (ideal to start — §2a/Q2). This composes
  ideal-polarizer MMs around the Berreman-computed sample MM — no new physics, just plumbing.
- **T / R / both** reuse the existing `Emission` DU (`EmitReflectedOnly | EmitTransmittedOnly | EmitBoth`,
  `Placement.fs`) and the snap-chain branch work: a sample that emits **both** drives **two** analyzer +
  detector arms (Q6) — one on the transmitted branch, one on the reflected.
- The element's `valueId : string option` (`Placement.fs:188`, already present, `None` by default) is the
  binding key from a table element to its stored spec.

**For reference, the underlying engine path** (unchanged): `IncidentLightInfo` + `OpticalSystem`
(`films`/`substrate`) + `RangedVariable` → `Analytics.Variables.calculate` → `Solution.emSys` →
`OpticalFunction` / Stokes / Mueller → `Charting.plot`.

---

## 2. Two new ribbon Bays

The Main screen is a Ribbon of Bays (`Ribbon.fs`; existing: Rotation / Move / Add / Render). We add two.

### 2a. The **Library** bay  *(name agreed: `Library`)*

**Library vs Materials vs Sample (Q1).** Three distinct ideas, deliberately separated:

- **Material** — an *infinite / bulk* material: glass, quartz, … (a refractive index / dispersive tensor).
  This is the existing `MaterialEntry` (`MaterialLibrary.fs`: id, name, category, `OpticalPropertiesWithDisp`).
- **Sample** — a **cut-out plate**: a material (or a stack) cut to a *thickness / plate geometry* → a real,
  finite sample that maps to `Layer`(s) / an `OpticalSystem`. E.g. *one glass → five real samples of that
  glass at different thicknesses.*
- **Library** — the **store the user picks from**, and the ribbon's job. It holds the choosable
  table-element specs (Samples, Sources, Detectors, Polarizers). A Sample **must be in the Library to be
  chosen**; Materials are a separate collection that Samples are cut from (the **sample editor** that turns a
  Material into a Sample — and edits/derives samples — is **later**; the mock is enough now).

**What the bay does.** The user selects a table element; the Library **constrains by element kind** and shows
only entries valid for that kind; the user picks one, which sets the element's `valueId` (readout shows the
bound name). A `Sample` element shows samples; a `Detector` shows detector types; a polarizer shows LP/CP
specs; the source shows source presets.

**Tree-like search window.** The Library is a **tree**, not a flat list — essential for samples, e.g.
`Samples → Glass → (a kind of glass) → {0.5 mm, 1 mm, 2 mm, …}`. The bay presents a searchable tree; the mock
proxy returns that tree.

**Changeability (Q2).** Real LPs/CPs and detectors are often **non-ideal**, so these specs are Library
entries that can be **swapped**. We **start** with one **ideal LP** and two **ideal CP** (left / right) and
the ideal detectors, with the model leaving room for non-ideal variants later.

**Stored model (proposed).**

```fsharp
type Material =                       // infinite/bulk — the existing MaterialEntry
    { id : string; name : string; category : MaterialCategory; properties : OpticalPropertiesWithDisp }

type Sample =                         // a cut-out plate: material(s) + thickness/plate → Layer(s)/OpticalSystem
    { id : string; name : string; materialId : string; thickness : Thickness; substrate : SubstrateKind }
    // SubstrateKind = ThinFilm | Plate | Wedge   (thin vs thick, reusing engine Substrate)

type SourcePreset    = { id; name; waveLength : WaveLength }                        // λ (Q3: source = λ)
type DetectorPreset  = { id; name; kind : DetectorKind }                            // Intensity | Ellipsometer (§4)
type PolarizerPreset = { id; name; kind : PolarizerKind }                           // ideal LP / ideal CP L|R (→ non-ideal later)

type LibraryEntry =                   // the choosable, kind-constrained, tree-organized things
    | SampleItem    of Sample
    | SourceItem    of SourcePreset
    | DetectorItem  of DetectorPreset
    | PolarizerItem of PolarizerPreset
    // each carries id (= valueId), display name, forKind : CatalogueKind, and a tree path
```

**Seeded entries (mock; map to existing engine presets).**

- **Materials (infinite):** glass (n = 1.52, 1.75), quartz, … (`OpticalProperties.Standard` / `.Dispersive`).
- **Samples (cut-out plates, in the Library — simplest → multilayer):**
  1. **Glass** plate (single transparent-glass film).
  2. **Glass** plate, a second thickness (showing the "same glass, different thickness" tree).
  3. **Quarter-wave multilayer** (the glass/vacuum stack, `MultilayerThinFilm.fsx`).
  4. *(optional)* **EUV Mo/Si multilayer** or **Langasite-on-Silicon** (dispersion / complex n).
- **Detectors (2):** **Intensity**, **Ellipsometer** (§4).
- **Polarizers:** **1 ideal LP**, **2 ideal CP** (left, right) — changeable later (Q2).
- **Source:** a monochromatic source (e.g. 600 nm).

### 2b. The **Experiments** bay

**An experiment (Q4): a choice of WHICH element's R1 makes a full circle.** Model it as a **single-case DU
now**, so adding experiment kinds later is compiler-guided:

```fsharp
type Experiment =
    | RotateR1FullCircle of ElementRef        // sweep this element's R1 over 0 … 360°  (1-D)
    // future cases (compiler will flag the match sites): RotateR1 of ElementRef * Range<Angle>,
    //   SweepWaveLength of …, SweepIncidenceAngle of …, a 2-D pair, …
```

- The swept R1 is the element's spin about its own beam axis — for an **analyzer** (an LP/CP after the
  sample, Q6) this is the **rotating-analyzer** measurement; for each angle θ the analyzer's MM rotates,
  the SV is re-propagated (§1), and the detector records → a curve over θ.
- **1-D to start** (Q8); 2-D is a future DU case (→ `plot3D`). Experiments group into **ordered sets** with
  setup steps between them (e.g. *set polarizer 45° → swap LP→CP → rotate analyzer full circle*) — modelled
  as `ExperimentSet = { name; steps : (SetupStep list * Experiment) list }`, also extended case-by-case.

---

## 3. IO proxies — the functional-proxy seam (mocks now)

No `*Proxy` records exist yet (only `unit`-returning UI `Handlers`). Per the convention (record of camelCase
`Result`-returning functions, built by `create`, test stub of the same shape), introduce — **mocked** now,
since nothing is stored (Q7: real persistence out of scope):

```fsharp
type LibraryProxy =
    { entriesForKind : CatalogueKind -> Result<LibraryEntry list, LibraryError>     // kind-constrained
      libraryTree    : unit -> Result<LibraryNode, LibraryError>                    // the searchable tree
      tryGetEntry    : string -> Result<LibraryEntry option, LibraryError> }        // by valueId

type MaterialProxy =                                                                // the infinite materials
    { listMaterials  : unit -> Result<Material list, MaterialError>
      tryGetMaterial : string -> Result<Material option, MaterialError> }

type ExperimentProxy =
    { listExperimentSets  : unit   -> Result<ExperimentSet list, ExperimentError>
      tryGetExperimentSet : string -> Result<ExperimentSet option, ExperimentError> }
```

- **Mock**: `createInMemory` closes over the seeded collection/tree and returns it — no IO, deterministic for
  tests (the headless tests swap in a stub of the same shape).
- **Later** (`OpticalConstructor.Storage`): a real `create` reading from disk; Bays and logic are unchanged.
- A `Context` bundles a proxy + the current selection so the views stay pure.

---

## 4. Detectors & measured quantities — detector type fixes the measurement (Q5)

- **Intensity detector** — records intensity = **S₀** of the Stokes vector at the detector. Under a rotating
  analyzer this traces `I(θ) = I₀·cos²(θ − θ₀)` (Malus). (`OpticalFunction` `I`/`R`/`T`.)
- **Ellipsometer** — records **Ψ, Δ** (`tan Ψ = |r_p/r_s|`, `Δ = arg r_p − arg r_s`), related to the SV
  (`S₁ = −cos 2Ψ`, `S₂ = sin 2Ψ cos Δ`, `S₃ = −sin 2Ψ sin Δ`) and the Mueller matrix — both already computed
  by the engine (`EmField.stokesVector` / `muellerMatrix`, plus `EllipticityR/T`, `AzimuthR/T`). A
  rotating-analyzer ellipsometer recovers the upper-left 3×3 of the Mueller matrix; full Stokes needs a
  rotating compensator (a future detector/analyzer type).
- **Analyzer (Q6)** is **not** a distinct element type: it is an **LP / CP placed after the sample** (the
  table already supports a polarizer there). If the sample emits **both** R and T, there are **two**
  analyzers (one per branch), each feeding its own detector. (An "LP+CP / CP+LP" compound analyzer is a
  future composition.)
- **Polarizers** (input and analyzer): ideal **LP** and ideal **CP** (left/right) MM elements now; non-ideal
  later (Q2).

---

## 5. What this touches (when we build it — NOT in this task)

- `OpticalConstructor.Domain`: `Material` (keep `MaterialEntry`), `Sample` (cut-out plate), `LibraryEntry` +
  tree, `Experiment` / `ExperimentSet` (single-case DUs), the `*Proxy` + `*Error` types, and the MM/SV
  propagation pipeline (compose ideal-polarizer MMs with the engine sample MM, read out per detector type).
- `OpticalConstructor.Controls`: two Bays (`LibraryControls` with a tree-search view; `ExperimentControls`)
  in the `State` + `Handlers` shape.
- `OpticalConstructor.TestWindows` (Main scene): two more `Ribbon.Bay`s; bind `valueId` on select.
- `OpticalConstructor.App`: build the **mock** proxies at the composition root and inject them.
- Tests: stub proxies; pure binding/selection + MM/SV-propagation tests; headless "the Library tree lists
  kind-constrained entries / sets valueId", "rotating the analyzer's R1 traces a Malus curve".

---

## 6. Proposed phasing (small, each independently green)

1. **Library bay** — mock `LibraryProxy` (tree, kind-constrained) + `valueId` binding + readout. Seed §2a.
2. **Experiments bay** — `Experiment = RotateR1FullCircle` (single-case DU) + `ExperimentSet` + mock
   `ExperimentProxy` (build/edit; no solve yet).
3. **One experiment end-to-end** — table → λ + SV_in (input LP/CP) → sample MM (Berreman) → SV propagate →
   analyzer MM → detector readout (intensity) → chart ONE curve (the rotating-analyzer Malus proof).
4. **Later:** ellipsometer Ψ/Δ; non-ideal LP/CP/detectors; both-branch (R+T) dual analyzers; the sample
   editor (Material → Sample); real Storage proxies (Q7); 2-D experiments; experiment-set setup steps.

---

## 7. Decisions (from `023`) and residual questions

**Agreed:**
- **Q1 — name:** ribbon = **Library**; **Materials** = infinite bulk materials, separate from **Samples**
  (cut-out plates). A sample must be in the Library to be chosen; tree-like search; sample editor later.
- **Q2 — changeability:** LP/CP/detectors are swappable Library specs; **start ideal** (1 LP, 2 CP L/R).
- **Q3 — compute model:** **MM/SV propagation + λ**: source→λ, input LP/CP→SV_in, sample→MM (from λ,
  incidence angle, sample rotation), SV propagates (T/R/both), analyzer×MM, detector records → chart + data.
- **Q4 / Q8 — experiment:** a **single-case DU** "rotate which element's R1 full circle", **1-D**; add DU
  cases later (compiler-guided).
- **Q5 — detector:** the detector **type fixes** what it measures (Intensity → S₀; Ellipsometer → Ψ/Δ).
- **Q6 — analyzer:** an **LP/CP after the sample** (not a distinct element); **two** analyzers if the sample
  emits both R and T.
- **Q7 — persistence:** **out of scope** now (mocks only).

**Residual (small, can decide as we build):**
- **R1.** Input polarizer / analyzer placement: do we *require* an LP/CP table element before the sample
  (input) and after it (analyzer), or synthesize ideal ones when absent?
- **R2.** Sample ↔ table: does one `Sample` table element map to one `OpticalSystem` (its samples may already
  be multilayer), and how is the swept-element list (for "which element's R1") presented (only
  analyzers/polarizers, or any element)?
- **R3.** Tree shape for the mock Library (categories/levels) — fine to fix pragmatically in Phase 1.

---

## 8. References (web — standards)

- Ψ/Δ ↔ Stokes/Mueller conventions and which Mueller elements each configuration recovers — J.A. Woollam
  Ellipsometry FAQ: <https://www.jawoollam.com/resources/ellipsometry-faq>; Mueller–Stokes calculus in
  ellipsometry (OSTI): <https://www.osti.gov/biblio/5467181>; Mueller-matrix spectroscopic ellipsometry
  (arXiv): <https://arxiv.org/pdf/1210.1076>.
- Rotating-analyzer **intensity** / Malus `I = I₀cos²θ`:
  <https://iitr.ac.in/Academics/static/Department/Physics/Optics%20Laboratory/6._Malus_law.pdf>.
