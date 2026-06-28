# 0027-022 — Materials & Experiments: marrying the table with the Berreman solver (preliminary spec)

**Status: DISCUSSION ONLY — for agreement.** This is a feature list / mini-spec, not an implementation. No
code was changed (per `021-…txt` "Make no other changes"). Open questions are collected in §7 — those are
the points to agree before any code.

---

## 1. Goal — the "marriage"

The Optical Constructor **table** (the Lego scene: source → … → detector) is the UI. The **Berreman code**
(BC, `Softellect.Berreman.Core` + `Analytics`) is the engine. We want the table to *drive a BC run* and chart
the result.

**How BC runs today** (verified — these types already exist):

```
IncidentLightInfo            -- λ, incidence angle, polarization, ellipticity   (Fields.fs)
  +  OpticalSystem           -- { upper; films : Layer list; substrate : Substrate option; lower }  (Media.fs)
  +  RangedVariable          -- a 1-D (or 2-D for surfaces) sweep                (Analytics/Variables.fs)
  ──► Analytics.Variables.calculate : array<float * Solution>                    (Variables.fs)
        Solution.emSys : EmFieldSystem = { incident; reflected; transmitted }    (Fields.fs)
  ──► OpticalFunction        -- R/T, Rs/Rp/Ts/Tp, Is/Ip, EllipticityR/T, AzimuthR/T;
                                 Stokes & Mueller available on EmField           (FieldFunctions.fs)
  ──► Analytics.Charting.plot / plot3D  (Plotly.NET)                             (Charting.fs)
```

- **Substrate variants** decide thin vs thick: `None` = thin film (one R, one T); `Some (Plate layer)` =
  thick plate (multiple internal reflections → more than one R/T); `Some (Wedge …)` swept by a
  `WedgeAngleRange`.
- **Sweep kinds** (`RangedVariable`): `IncidenceAngleRange`, `WaveLengthRange`, `PolarizationRange`,
  `EllipticityRange`, `WedgeAngleRange`, and `ArbitraryVariableRange` (a custom 1-D sweep carrying
  `getSys : OpticalSystem -> double -> OpticalSystem` — the general "vary anything" hook).

**The crux to agree (the mapping, §7):** how the table chain becomes `IncidentLightInfo` + `OpticalSystem` +
the experiment. Sketch:

| Table element | BC role |
|---|---|
| Light source (S) | `IncidentLightInfo` (λ, incidence angle, polarization) — bound from a stored source preset |
| Sample (Sa) | one or more `Layer`s in the `OpticalSystem` (thin film, multilayer, or thick `Plate`) — bound from a stored **material** |
| Linear / circular polarizer (LP/CP) | an ideal Jones/Mueller polarizer applied to the field on the beam |
| Detector (D) | which `OpticalFunction`(s) to read out (intensity vs ellipsometric Ψ/Δ) |
| Lens / flat & curved mirror | beam *geometry* only (already handled by the snap chain); not optical layers (for now) |

The element's existing **`valueId : string option`** slot (`Placement.fs:188`, already in `ElementPlacement`,
`None` by default) is the binding key: it points at the stored thing the element *is*.

---

## 2. Two new ribbon Bays

The Main screen is already a **Ribbon of Bays** (`Ribbon.fs`; current bays: Rotation / Move / Add / Render).
We add two more bays, each a reusable control with the established `State` + `Handlers` shape.

### 2a. The **Library** bay  *(proposed one-word name)*

> **Name proposal: `Library`.** It is the *stored collection of concrete optical things* you pick from to
> "fill in" a table element — distinct from the abstract `CatalogueKind` (the element *kind*) and from the
> existing per-kind **Add** palette. The domain already has a `MaterialLibrary` (`MaterialLibrary.fs`) — this
> bay generalizes that to also hold sources / detectors / polarizers.
> Alternatives if `Library` is vetoed: `Catalog`, `Stock`, `Presets`, `Components`, `Materials` (the last
> undersells sources/detectors). **Pick one — §7.Q1.**

**What it does.** With a table element selected, the Library bay lists the stored entries whose kind matches
that element, and selecting one sets the element's `valueId` (and shows a readout of the bound name). E.g.
select a `Sample` → choose "Glass (n = 1.52)" or "Quarter-wave multilayer"; select a `Detector` → choose
"Intensity" or "Ellipsometer"; select a polarizer → choose "Ideal LP" / "Ideal CP (left)".

**Stored model (proposed).** Generalize `MaterialEntry` to a `LibraryEntry` discriminated by what it
configures (re-using the existing `MaterialEntry` for the material case):

```fsharp
type LibraryEntry =
    | MaterialItem  of MaterialEntry            // existing: id, name, category, OpticalPropertiesWithDisp
    | SourceItem    of SourcePreset             // λ, incidence angle, polarization, ellipticity
    | DetectorItem  of DetectorPreset           // Intensity | Ellipsometer (see §4)
    | PolarizerItem of PolarizerPreset          // IdealLinear | IdealCircular of Handedness
// each carries a stable `id` (the valueId) + a display `name` + a `forKind : CatalogueKind`
```

**Mock proxy now (the IO seam).** We have nothing stored, so wire a **mock functional proxy** that returns
an in-memory collection — see §3.

**Seeded entries (per the brief; map to existing engine presets):**

- **Samples / materials — 3–4, simplest → multilayer** (from `Analytics/Examples/`, re-using
  `OpticalProperties.Standard` / `.Dispersive`):
  1. **Glass** — single transparent glass interface/film (`transparentGlass`, n = 1.52) — the simplest case.
  2. **Single glass film** — one finite-thickness film (e.g. `transparentGlass175`).
  3. **Quarter-wave multilayer** — the glass/vacuum stack from `MultilayerThinFilm.fsx`.
  4. *(optional 4th)* **EUV Mo/Si multilayer** (`MultilayerThinFilm_EUV.fsx`) **or** a dispersive
     **Langasite-on-Silicon** (`LangasiteOnSilicon.fsx`) — to exercise dispersion / complex n.
- **Detectors — 2:** **Intensity** (records I / R / T) and **Ellipsometer** (records Ψ/Δ) — §4.
- **Polarizers:** **1 ideal LP**, **2 ideal CP** — **left** and **right** handed.
- **Light source — ≥1:** a monochromatic source (e.g. 600 nm, S- or P-polarized) — bound to the source element.

### 2b. The **Experiments** bay

**What it is.** An *experiment* specifies **what is measured**: usually **one element is rotated** (e.g. an
analyzer swept a full circle) while a **detector records** something. The detector's *type* sets what is
recorded — an **intensity** detector records I(θ) (Malus, `I = I₀cos²θ`); an **ellipsometer** records Ψ/Δ
(and/or Stokes / Mueller).

**Experiments come in groups/sets.** A set is an *ordered* list of setup-steps + experiments, e.g.:
*"set the polarizer to 45° → swap LP for CP → sweep the analyzer 0…360° recording intensity."* **2-D
experiments are rare** (two swept variables → a surface via `plot3D`).

**Model (proposed).**

```fsharp
type SweptVariable =                 // maps onto the engine's RangedVariable
    | RotateElement of ElementRef * Range<Angle>     // "rotate THIS element" → ArbitraryVariableRange(getSys)
    | IncidenceAngle of Range<IncidenceAngle>
    | WaveLength     of Range<WaveLength>
    | Polarization   of Range<Polarization>

type Experiment =
    { name : string
      sweep : SweptVariable                 // 1-D (the common case)
      sweep2 : SweptVariable option         // a 2-D experiment (rare)
      measured : OpticalFunction list }     // chosen by / defaulted from the detector type

type SetupStep =                            // applied to the table state before the next experiment
    | SetAngle of ElementRef * Angle
    | SwapKind of ElementRef * CatalogueKind   // e.g. LP → CP
    | Bind     of ElementRef * (* valueId *) string

type ExperimentSet = { name : string; steps : (SetupStep list * Experiment) list }
```

- The **"rotate this element"** sweep is the natural fit for `ArbitraryVariableRange` (its
  `getSys : OpticalSystem -> double -> OpticalSystem` rotates the chosen element's Jones/Mueller transform);
  a pure source-polarization sweep can use `PolarizationRange` directly.
- The bay shows the current set's experiments, lets you add/remove/reorder, and (later) **Run** → `calculate`
  → `plot`. Stored sets come from a **mock `ExperimentProxy`** (§3).

---

## 3. IO proxies — the functional-proxy seam (mocks first)

The codebase has **no `*Proxy` records yet** (only the UI `Handlers` records, which are `unit`-returning
view seams). Per the convention (Softellect; CLAUDE.md "Model IO as functional proxies"), an IO proxy is a
**record of camelCase, `Result`-returning functions, built by a `create`, with a test stub of the same
shape**. We introduce the first two — both **mocked** now, since nothing is stored:

```fsharp
type LibraryProxy =
    { listEntries     : CatalogueKind -> Result<LibraryEntry list, LibraryError>   // entries valid for a kind
      tryGetEntry     : string -> Result<LibraryEntry option, LibraryError> }       // by valueId

type ExperimentProxy =
    { listExperimentSets : unit   -> Result<ExperimentSet list, ExperimentError>
      tryGetExperimentSet : string -> Result<ExperimentSet option, ExperimentError> }
```

- **Mock** (`...Proxy.createInMemory entries`) closes over the seeded collection and returns it — no IO,
  fully deterministic for tests (a stub proxy is swapped in by the headless tests, exactly as the brief asks).
- **Later** (`OpticalConstructor.Storage`): a real `create` that reads JSON/`.binz` from disk; the logic and
  the Bays are unchanged (they hold the proxy, never a file handle).
- Bays receive the proxy's data through a `Context` (proxy + current selection), keeping the views pure.

---

## 4. Detectors & measured quantities (standards check)

- **Intensity detector** — records total intensity. Under a **rotating analyzer**, `I(θ) = I₀·cos²(θ − θ₀)`
  (Malus's law). Maps to `OpticalFunction` `I` / `R` / `T` (and `Is/Ip`, `Rs/Rp`, `Ts/Tp`).
- **Ellipsometer** — records the ellipsometric angles **Ψ, Δ**: `tan Ψ = |r_p / r_s|`, `Δ = arg(r_p) − arg(r_s)`.
  They relate to the **Stokes** parameters (`S₁ = −cos 2Ψ`, `S₂ = sin 2Ψ cos Δ`, `S₃ = −sin 2Ψ sin Δ`) and to
  the **Mueller** matrix — both already computed by the engine (`EmField.stokesVector`,
  `EmField.muellerMatrix`, plus `EllipticityR/T`, `AzimuthR/T` in `FieldFunctions.fs`). A **rotating-analyzer**
  ellipsometer recovers the upper-left 3×3 of the Mueller matrix; full Stokes needs a **rotating
  compensator** (good to note for future detector types).
- **Polarizers** — ideal **linear** (LP) and ideal **circular** (CP, left/right) modelled as fixed
  Jones/Mueller elements applied to the field on the beam before the detector. (The domain currently has an
  `Analyzer` placeholder, `BeamTree.fs`; agree whether "analyzer" is a distinct element or just
  "a polarizer immediately before a detector" — §7.Q6.)

---

## 5. What this touches (when we build it — NOT now)

- `OpticalConstructor.Domain`: extend `MaterialLibrary` → a `Library`/`LibraryEntry` (materials + sources +
  detectors + polarizers); add `Experiment` / `ExperimentSet`; add the `*Proxy` + `*Error` types.
- `OpticalConstructor.Controls`: two new Bays (`LibraryControls`, `ExperimentControls`) with `State`/`Handlers`.
- `OpticalConstructor.TestWindows` (Main scene): two more `Ribbon.Bay`s wired through; bind `valueId` on select.
- `OpticalConstructor.App`: construct the **mock** proxies at the composition root and inject them.
- Tests: stub proxies + pure binding/selection tests; headless "the bay lists entries / sets valueId".

---

## 6. Proposed phasing (small, each independently green)

1. **Library bay + mock `LibraryProxy` + `valueId` binding + readout** (no solve). Seed the entries from §2a.
2. **Experiments bay + `Experiment`/`ExperimentSet` model + mock `ExperimentProxy`** (build/edit a set; no solve).
3. **Run one experiment**: table → `IncidentLightInfo` + `OpticalSystem` + `RangedVariable` → `calculate` →
   chart ONE `OpticalFunction` (a single end-to-end proof; the rotating-analyzer-intensity case).
4. **Later:** real storage proxies; ellipsometer Ψ/Δ readout; 2-D experiments; more materials/detectors.

---

## 7. Open questions — to AGREE before coding

- **Q1. Ribbon name** — `Library` (proposed)? or `Catalog` / `Stock` / `Presets` / `Components` / `Materials`?
- **Q2. Table → OpticalSystem mapping** — which elements form the layer stack (only `Sample`s? in beam order?),
  and how is a **thick plate** vs **thin film** chosen (a per-sample flag → `Substrate Plate` vs `None`)? Do
  multiple samples concatenate into `films`? Where do the gaps go (geometry only, or layer thicknesses)?
- **Q3. Source binding** — does the light-source element fully define `IncidentLightInfo`, with the swept λ /
  incidence angle overriding it during an experiment?
- **Q4. "Rotate this element" sweep** — confirm `ArbitraryVariableRange` (with a `getSys` that rotates the
  chosen element's Jones/Mueller) is the mechanism, vs special-casing `PolarizationRange`.
- **Q5. Detector → measured quantity** — does the detector *type* alone fix the `OpticalFunction`s (Intensity →
  `I`/`R`/`T`; Ellipsometer → Ψ/Δ), or can the experiment override?
- **Q6. Analyzer** — keep the domain `Analyzer` placeholder as a distinct element, or treat "analyzer" as
  "a polarizer immediately upstream of a detector"?
- **Q7. Persistence** — when do the mock proxies become real (`Storage`), and in what format (JSON vs `.binz`)?
- **Q8. 1-D default, 2-D rare** — confirm the Experiments bay defaults to one swept variable, with 2-D as an
  explicit opt-in (→ `plot3D`).

---

## 8. References (web — standards & ideas)

- Rotating-analyzer ellipsometry, Ψ/Δ ↔ Stokes/Mueller conventions, and which Mueller elements each
  configuration recovers — J.A. Woollam Ellipsometry FAQ: <https://www.jawoollam.com/resources/ellipsometry-faq>;
  Mueller–Stokes calculus conventions in ellipsometry (OSTI): <https://www.osti.gov/biblio/5467181>;
  Mueller-matrix spectroscopic ellipsometry overview (arXiv): <https://arxiv.org/pdf/1210.1076>.
- Rotating-analyzer **intensity** measurement / Malus's law `I = I₀cos²θ` — IIT-R optics lab note:
  <https://iitr.ac.in/Academics/static/Department/Physics/Optics%20Laboratory/6._Malus_law.pdf>.
