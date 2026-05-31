# Optical Constructor App — Authoritative Specification & Tech Stack

This is the single, self-consistent specification for turning the Berreman repository
into a full-scale, GUI-driven **optical-constructor application**. It defines **what we
will build** (the agreed feature set) and **how we will build it** (the agreed platform,
storage, UI, and numerical-library decisions).

Two feature tiers are used, both in scope:

- **[Core]** — table-stakes; the app is not usable without it.
- **[Standard]** — expected of a serious tool; part of the committed scope.

Each feature notes whether the capability **already exists in the engine** (and so needs
mostly UI/plumbing) or is **net-new**.

The solution **targets .NET 10** and is **F#-first** (one language end to end), with the
domain model expressed as F# records and discriminated unions (`OpticalSystem`, `Layer`,
`OpticalProperties`, …).

---

## Part I — Context: what the repository already is

The codebase is a type-safe **F# implementation of the Berreman 4×4 transfer-matrix
method** for wave propagation through stratified, anisotropic, optionally
magneto-optic/active media. It already supports:

- Isotropic, uniaxial, biaxial, cubic, planar and chiral/active crystals via full
  3×3 `Eps`/`Mu`/`Rho` tensors (`MaterialProperties.fs`, `OpticalProperties/*`).
- Multilayer stacks, thick plates and **wedge** substrates with multiple internal
  reflections (`Media.fs`, `Solvers.fs`).
- Dispersive materials (e.g. Silicon, Langasite) and EUV materials (Mo/Si).
- Reflectance/transmittance (R/T, s/p), Stokes vectors, Jones/Mueller matrices,
  ellipticity, azimuth, and in-layer E/H/D/B/Poynting fields (`FieldFunctions.fs`).
- Parametric sweeps over wavelength, incidence angle, polarization, ellipticity,
  wedge angle, or arbitrary variables (`Variables.fs`), with 1D/3D charts
  (`Charting.fs`) and ready-made example scripts (`Analytics/Examples/*.fsx`).

The **computational core is strong**; the gap to an "optical constructor app" is almost
entirely **UI, data management, materials/source libraries, design synthesis
(optimization/fitting), and persistence**.

**Engine note for curved elements.** The Berreman core is a 1D plane-wave formalism.
Lenses and non-flat mirrors require treating each surface as a *local* interface seen by
a *local* plane wave (a ray/beam carrying its own direction and polarization), with the
existing 4×4 solver evaluated at the local angle of incidence — and, for focusing,
ray/beam propagation between elements. This is the one place where a modest extension
beyond the pure 1D model is unavoidable (see §1).

Design is benchmarked against established tools: thin-film design (Essential Macleod,
OptiLayer, TFCalc, OpenFilters), ellipsometry analysis (WVASE, CompleteEASE, DeltaPsi2),
anisotropic 4×4 toolkits (Berreman4x4, PyLlama, pyElli), and illumination/optical-design
suites (LightTools, TracePro, Speos).

---

## Part II — Agreed feature set

### 1. Optical-system construction & UI

- **[Core]** **Compose an end-to-end optical system from elements** — chain optical
  elements in sequence, e.g. **light source → polarizer → sample → analyzer →
  detector**. This is the central act of an optical constructor: pick elements from a
  palette, order them, connect them, and run the whole train. (Net-new orchestration
  layer above the existing single-system solver.)
- **[Core]** **Branching beam paths (reflected & transmitted) — a system tree, not just
  a chain.** A real system can be large and is generally **not a single linear chain**:
  almost every element produces **both a reflected and a transmitted beam**, and the user
  can place further elements in front of **either** output. The system is therefore a
  **tree of beams** — each non-mirror element is a node that splits incident light into a
  transmitted branch and a reflected branch, and downstream sub-systems can be attached to
  each branch independently (the standard "ray-splitting" / branching model used by
  non-sequential optical design). The Berreman solver already yields the reflected and
  transmitted fields at each element, so this is an **orchestration/topology layer** that
  routes those two outputs onward and propagates energy and polarization (Jones/Stokes)
  along every branch. Specifics:
  - **Mirrors are a deliberate special case:** an element classified as a *mirror*
    supports the **reflected branch only** — by design we ignore its transmitted light
    even if some physically leaks through. Only the reflected output can carry downstream
    elements.
  - Every **non-mirror** element exposes **two attachment points** (reflected,
    transmitted); a branch may be left open (terminated by a detector or by nothing) or
    continued with another element or sub-assembly.
  - Each branch carries its own beam state (direction, energy, polarization), so detectors
    on different branches report independent results.
- **[Core]** **Lenses and non-flat (curved) mirrors as elements** — without focusing
  elements a plane wave can never be converted into a converging/diverging beam or an
  image. Support spherical/aspheric lenses and concave/convex mirrors, each with
  radius/curvature, aperture, substrate material, and **coatings on their surfaces**.
  **Critical for EUV**, where multilayer-mirror reflectance is strongly angle-of-
  incidence dependent and curved mirrors typically need **depth-graded multilayer
  spacing** (~5% center-to-edge) to keep reflectance uniform across the changing local
  angle. (Net-new; uses the local-interface treatment from Part I.)
- **[Core]** **Visual stack/layer editor** — add, delete, reorder, duplicate, and group
  layers in a list/table; edit thickness and material per row. (Engine has
  `Layer`/`BaseOpticalSystem`/`OpticalSystem`; needs a UI surface.)
- **[Core]** **Incident & exit medium selectors** (upper/lower half-spaces) with
  refractive index or material reference.
- **[Core]** **Schematic cross-section view** of the stack drawn to scale, color-coded
  by material, with thickness labels and the incident-light ray/angle drawn.
- **[Core]** **Substrate type switch**: thin film vs. thick **plate** vs. **wedge** (with
  wedge angle), exposing the existing multi-reflection handling.
- **[Core]** **Per-element default unit of measure** — each layer/element/source
  remembers and displays the unit natural to it (a plate in mm, a film in nm, a source in
  nm/µm/eV) while the system mixes units freely; full conversion behaviour in §2.
- **[Core]** **Persistent UI customization** — remember the user's working environment
  between sessions: a **board / palette of commonly used elements** (pin/favorite
  frequently used layers, materials, sources, and whole sub-assemblies, grouped and named
  for quick reuse), the **last working folder(s)** and **recent files/projects**,
  window/panel layout, and toolbar contents. Customizations persist as defaults for new
  projects. (Net-new.)
- **[Standard]** **Per-layer anisotropy/orientation controls**: pick material class
  (isotropic / uniaxial / biaxial / active) and set **crystal-axis rotation**
  (Euler/rotateX/Y/Z) — already supported by `Layer.rotate`/`OpticalSystem.rotate`.
- **[Standard]** **Repeat/period builder** — define a unit cell (e.g. `[H L]`) and a
  repeat count to generate Bragg mirrors / DBRs / EUV Mo–Si stacks without manual
  duplication.
- **[Standard]** **Live recalculation** as parameters change, with a busy/cancel
  indicator for heavy sweeps.
- **[Standard]** **Drag-and-drop** of materials from the library onto layers.
- **[Standard]** **Multi-system workspace** — hold several systems side by side for
  comparison (engine already has `plotComparison`).
- **[Standard]** **Gradient-index / inhomogeneous layers** auto-discretized into
  sub-layers.
- **[Standard]** **Stack templates/wizards** (AR coating, bandpass filter, beam splitter,
  polarizer, waveplate, DBR) that pre-populate a starting design.
- **[Standard]** **2D/3D rendered view** of the element/system with the beam path.

### 2. Optical substances / materials management

- **[Core]** **Materials library** with built-in common materials and user-defined
  entries; searchable, categorized (glasses, metals, semiconductors, crystals).
- **[Core]** **Dispersion model editor** — define n,k vs. wavelength via tabulated data
  or analytic models: **Sellmeier, Cauchy, Lorentz/Drude-Lorentz, Tauc-Lorentz,
  Gaussian/harmonic oscillators, Drude (metals), constant n+ik**. (Engine has a
  `Dispersion` abstraction; needs a model library + UI.)
- **[Core]** **Import from refractiveindex.info** (its open YAML format) and from CSV/tab
  data; export the same.
- **[Core]** **Human-convenient units of measure with automatic conversion.** Every
  quantity may be entered and displayed in whatever unit is natural for it, and
  **different parts of the same system may use different units**:
  - **Length / thickness**: m, mm, **mkm (µm)**, nm, **Å** — e.g. a thick plate in
    **mm**, a thin film in **nm**.
  - **Wavelength / photon energy**: nm (e.g. a **green laser** ≈ 532 nm), **mkm** (an
    **IR laser**, e.g. 10.6 µm), **Å**, and **eV** (e.g. a **free-electron laser**,
    EUV/X-ray) — with the energy↔wavelength conversion **E[eV] = 1239.84 / λ[nm]** (hc)
    handled transparently, plus the spectroscopy wavenumber cm⁻¹.
  - **Each element/parameter carries its own *default* unit of measure** (the unit it was
    created in / is most natural for it), so a project mixes mm plates, nm films, and eV
    sources without the user converting anything by hand.
  - **Conversions are automatic and lossless internally** — store one canonical SI base
    (meters) and convert only at the UI/IO boundary, so charts, sweeps, and fits can be
    requested in any unit. This extends the existing units-of-measure support
    (`Constants.fs` already has nm/mkm/mm/m); the net-new work is the **eV (and cm⁻¹)
    photon-energy conversions** and the **per-element default-unit** plumbing.
- **[Standard]** **Anisotropic material definition** — enter principal n_o/n_e (uniaxial)
  or n_x/n_y/n_z (biaxial), full ε tensor, μ, and gyration/ρ (chiral/magneto-optic),
  matching existing `Eps`/`Mu`/`Rho` support.
- **[Standard]** **Material preview plot** of n(λ) and k(λ) (and ρ, ε components) over a
  chosen range — already prototyped as `plotN11`, `plotXi11`, `plotRho11`, etc.
- **[Standard]** **Unit presets & display formatting** — quick unit switching on any
  axis/field, sensible significant figures, and "show in eV / nm / µm" toggles on
  spectral plots.
- **[Standard]** **Temperature/composition dependence** (thermo-optic dn/dT) as optional
  model parameters.

### 3. Light sources & illumination

- **[Core]** **Source/incident-light editor**: wavelength (single or range), incidence
  angle, **polarization state** (linear angle, s/p, circular, elliptical via
  ellipticity), and intensity. (Present in `Fields.fs` / `StandardLightVariables.fs`.)
- **[Standard]** **Polarization presets** (s, p, 45°, RCP/LCP, unpolarized) and a
  **Poincaré-sphere / polarization-ellipse** picker.
- **[Standard]** **Spectral source profiles** — flat, blackbody, standard illuminants
  (D65, A), laser line, or imported spectrum — for integrated/photometric results.
- **[Standard]** **Angle/cone definition** — collimated vs. finite cone angle and
  detector acceptance (averages over cone/bandwidth).
- **[Standard]** **Gaussian-beam source** — the engine already has FFT-based
  Gaussian-beam scaffolding (`FourierTransform.fs`); expose beam waist/divergence and
  propagate.
- **[Standard]** **Coherent vs. incoherent layer flags** (thick substrate treated
  incoherently while films stay coherent).
- **[Standard]** **Multiple simultaneous sources** and unpolarized averaging.

### 4. Calculations / analysis

- **[Core]** **Spectral & angular scans** of R, T (and absorption A = 1−R−T), s/p split,
  over wavelength or angle — the engine's primary output today.
- **[Core]** **Parameter sweeps** over any model variable (thickness, angle,
  polarization, ellipticity, wedge angle, material parameter) — existing
  `RangedVariable`/`ArbitraryVariableRange`/`calculate`/`calculate3D`.
- **[Standard]** **Polarimetric outputs**: Stokes vectors, **Jones & Mueller matrices**,
  ellipticity, azimuth, degree of polarization (mostly present; finish the commented-out
  `muellerMatrix` path in `FieldFunctions.fs`).
- **[Standard]** **Ellipsometric quantities** Ψ, Δ (and N, C, S) vs. λ/angle — the key
  deliverable for the ellipsometry user base; derivable from existing field amplitudes.
- **[Standard]** **In-layer field / EFI profile** — |E|², Poynting, absorption per layer
  vs. depth (engine computes E/H/D/B/Poynting); critical for laser-damage and absorber
  placement.
- **[Standard]** **Absorptance per layer** and total absorbed-power breakdown.
- **[Standard]** **Color calculation** — convert reflectance/transmittance spectra to CIE
  XYZ/Lab/sRGB swatches.
- **[Standard]** **Convergence controls** exposed (number of internal reflections,
  numerical thresholds in `SolverParameters`).

### 5. Design synthesis, optimization & fitting

This is the target spec for porting the existing partial Wolfram/Mathematica
optimization code to F#. It provides both **refinement** (improve a given starting design
by small adjustments of layer thicknesses) and **synthesis** (build the design up, e.g.
needle insertion, changing the number of layers). The optimization library and
abstraction are specified in Part III §4.

- **[Core]** **Target / merit-function editor** — specify desired R/T/Ψ/Δ vs. λ/angle as
  a weighted least-squares merit function, with per-target **weights, tolerances, and
  one-sided/inequality targets** (e.g. "R ≤ 0.5%").
- **[Core]** **Local refinement optimization** of layer thicknesses (and optionally
  indices/geometry) — **Levenberg–Marquardt** as the workhorse, plus gradient/quasi-
  Newton and Nelder–Mead simplex. Support **bounded parameters and inequality targets**
  (forbid negative thickness, bound n to physical ranges); robust normal-equation solves
  (QR / Gauss–Jordan). This mirrors the Wolfram `FindMinimum` / `NonlinearModelFit`
  workflow being ported.
- **[Standard]** **Inverse fitting / characterization from measured data** — load
  measured **photometric (R/T)** and/or **ellipsometric (Ψ, Δ)** spectra and regress
  model parameters (thicknesses, optical constants, nonuniformity, roughness). This is the
  core ellipsometry "reverse engineering" workflow, a formal **inverse problem** stated as
  constrained optimization.
- **[Standard]** **Fit-quality reporting** — χ²/MSE, parameter confidence intervals,
  correlation matrix, residual plots, and overlay of fit vs. measured data.
- **[Standard]** **Synthesis methods (start-point-insensitive)** — **needle
  optimization** (automatically insert a thin "needle" layer at the position that most
  decreases the merit function, growing the layer count without bounding the parameter
  space) combined with **tunneling / gradual evolution** to escape local minima.
- **[Standard]** **Global optimization** — simulated annealing and genetic/evolutionary
  algorithms for difficult, multimodal merit landscapes where refinement stalls.

### 6. Charts & visualization

- **[Core]** **Interactive 1D plots** (R/T/Ψ/Δ vs. λ or angle) with zoom/pan, multiple
  traces, legends.
- **[Core]** **Overlay & comparison** of multiple systems/curves (existing
  `plotComparison`).
- **[Standard]** **2D/3D surface & contour plots** (e.g. R vs. λ × angle) — existing
  `plot3D`.
- **[Standard]** **Chart customization** — axis ranges/scales (log), titles, colors, line
  styles, units, gridlines, annotations; per-trace show/hide.
- **[Standard]** **Cursor/readout & markers** — hover values, peak/min finders, FWHM,
  delta between cursors.
- **[Standard]** **Specialized plots** — polarization ellipse, Poincaré sphere,
  field-depth profile, CIE chromaticity diagram, dispersion (n,k) plots.

### 7. Project management, persistence & I/O

- **[Core]** **Save/Open project files** — full state (systems, materials, sources,
  sweeps, chart settings) in the canonical JSON format defined in Part III §2.
- **[Core]** **Export chart data** to CSV/Excel and **chart images** to PNG/SVG/PDF.
- **[Standard]** **Recent files, autosave, and undo/redo** of model edits.
- **[Standard]** **Report generator** — one-click PDF/HTML report with schematic,
  parameters, plots, and fit results.
- **[Standard]** **Material library import/export** as a shareable file.
- **[Standard]** **Version control / design history & diff** between design revisions.

### 8. Environment customization & UX

- **[Standard]** **Preferences**: default units, wavelength range, number of sweep
  points, decimal precision, solver defaults.
- **[Standard]** **Light/dark theme** and configurable chart color palettes.
- **[Standard]** **Dockable/resizable panels** (stack, materials, sources, chart,
  results) with saved layouts/workspaces. (Works together with the §1 persistent UI
  customization / favorites board.)
- **[Standard]** **Input validation & physical sanity warnings** (e.g. n<1 absorbing
  region, non-causal data, thickness ≤ 0).
- **[Standard]** **Progress, cancellation, and timing** for long sweeps/optimizations.

### 9. Documentation, help & onboarding

- **[Standard]** **In-app material/units glossary and tooltips**, plus a **gallery of
  worked examples** built from the existing `Analytics/Examples/*.fsx` (AR coating, DBR,
  EUV Mo/Si, active crystal, wedge, dispersive glass).
- **[Standard]** **Getting-started tutorial / sample projects** shipped with the app.

---

## Part III — Implementation decisions (how we will build it)

### 1. Platform & language

**.NET 10**, **F#-first** end to end. The domain model stays F# records + discriminated
unions. One language across engine, UI, and tooling maximizes LLM reliability and keeps
the whole codebase under one set of standards (see §5, constitution alignment).

### 2. Storage / data formats

- **Canonical project/document files → JSON, with a published JSON Schema, validated on
  load.** "Comments" are carried as a **structured convention inside the schema** — a
  `"description"`/`"_comment"` field (the domain model already has `description` fields).
  JSONC is used for files meant to be hand/LLM-edited. Shipping a schema and validating
  on load is the single biggest lever for safe automated edits.
- **Large / bulk numeric data → zipped FsPickler binary with the `.binz` extension.**
  Reuse the existing `Softellect.Sys.Serialization` `BinaryZippedFormat` (built on
  MBrace.FsPickler) verbatim — it gives ~100× size reduction vs. pretty JSON/XML and
  serializes F# records/DUs natively. Binary pickles are opaque and version-coupled to the
  types, so they are used **only for derived/bulk results and caches** (sweeps, field maps,
  fit histories), **never** for the canonical, hand/LLM-editable project definition. The
  `.binz` sidecar is referenced from the JSON.
- **Simple flat rules / declarations → a small YAML subset** (the shape of the existing
  `Softellect/.gates/TheBird/*.gates` files: shallow, flat, a few named keys plus one short
  list). Deep nesting stays out of YAML (that is JSON's job). Each YAML file is
  schema-validated.
- **Preferences/config → the canonical JSON + Schema** (one parser for everything).

**Libraries (all open-source).** `System.Text.Json` + **FSharp.SystemTextJson** for
idiomatic DU/record/option serialization; **FSharp.Data** for importing external
measurement/material data; **JsonSchema.Net** for schema validation. MBrace.FsPickler is
already in-house via `Softellect.Sys`.

### 3. UI tech stack (Windows-only desktop)

**Pure F# — Avalonia + Avalonia.FuncUI (MVU) + ScottPlot, with WebView2 hosting
Plotly.NET output.**

- **Avalonia + Avalonia.FuncUI** gives idiomatic **F# MVU/Elmish** (model holds state,
  pure `update`, view as a function of state). This matches the functional codebase and the
  governing constitution. Avalonia is modern, GPU-accelerated (Skia), and actively
  developed.
- **Charts:** **ScottPlot** for fast, native, interactive 2D scientific plots; keep
  **Plotly.NET** (already in use) for publication-quality/exportable charts, hosted in a
  **WebView2** control. The **3D viewport** uses a custom OpenTK renderer.
- **LLM generation** for Avalonia is good: its XAML is ~90% WPF-compatible (WPF is heavily
  represented in training data), and Avalonia publishes Copilot guidance. FuncUI's surface
  is a small, regular DSL; supplying its docs plus a couple of canonical component examples
  as in-context anchors makes generation reliable.

**FuncUI is consumed from an audited private clone.** Avalonia.FuncUI and Avalonia core are
**MIT-licensed**, so cloning, vendoring, and commercial use (including a privately modified
fork) are permitted.

> **Mandatory audit of the FuncUI clone (`C:\GitHub\Avalonia.FuncUI.Clone\`).** Before use,
> the clone **must be audited** for:
> - **Any political statements** — in code, comments, README/docs, license headers,
>   resources (strings, images, icons, sample data), commit/PR/issue templates, CI config,
>   and embedded URLs.
> - **Back doors or behavioral restrictions made in support of those statements** — code
>   that disables, degrades, nags, geofences, date-gates, or otherwise alters behavior
>   based on locale/region/IP/name/political criteria, "protestware"-style payloads, or
>   telephone-home/telemetry tied to any of the above.
>
> **Everything related to that must be cleared out** of the clone — removed entirely, not
> merely disabled — and the result re-reviewed to confirm nothing reintroduces it on
> update. This is a gating step: the clone is not approved for linking until the audit
> passes.
>
> **Linking mechanism is deliberately undecided for now.** The audited clone will be
> consumed **either as a local NuGet package OR as a project reference** — that choice is
> made later, not now. The audit and clear-out apply identically either way. Avalonia core
> is consumed as the normal MIT NuGet.

### 4. Numerical libraries

Linear algebra (the engine kernel) and optimization (fitting/synthesis) are **two separate
concerns**, each kept behind its own adapter on the existing math-abstraction seam
(`MathNetNumericsMath.fs` / `ExtremeNumericsMath.fs`, whose purpose is to "abstract away
differences … switching between them is VERY painful").

**Linear algebra.** The repo ships a **clone of Math.NET Numerics** (`MathNetNumerics/…`)
plus two gap-filling modules:

- **Matrix exponential (`MatrixExp.fs`)** is written in this repo, self-contained, and
  settled. No external library is involved.
- **General complex non-Hermitian EVD** (eigenvalues and right eigenvectors — the core of
  the Berreman method) uses the **cloned Math.NET `m.Evd()`** via `MatrixEvd.fs`, kept
  behind the math seam. (Math.NET was cloned because its complex EVD was buggy.)

ALGLIB is used for **optimization only**, not for linear algebra.

**Optimization.** Used for inverse fitting and design synthesis (Part II §5):

- **Use ALGLIB, behind our own optimization abstraction.** ALGLIB is a genuine,
  professionally maintained native implementation (consistent C++/C#/Java), already
  referenced in this codebase, and covers the need in pure managed code: **`minlm`**
  (Levenberg–Marquardt nonlinear least squares), **`minbleic`** (bound/linear-constrained),
  **`minnlc`** (nonlinearly constrained), `minlbfgs`, `mincg`, and `lsfit` curve fitting
  with error estimates.
- **Define a library-agnostic optimization interface** in our own terms — an
  objective/residual function `(float[] -> float[])` (+ optional Jacobian), parameter
  bounds, constraints, and a result record — with ALGLIB behind it, so the physics/fitting
  code does not depend on the library directly.
- **Validate ALGLIB's `minlm` against the Wolfram reference fits.**
- **Drop Extreme Optimization** from the numerics path; it may remain only as a legacy
  reference in `ExtremeNumericsMath.fs` until the math seam is fully generalized.
- **License:** ALGLIB's free edition is **GPL** (fine for in-house/research); a
  **commercial license** is required for closed redistribution.

### 5. Alignment with the spec-driven-development constitution

- **P1 — LLM-friendliness is the meta-principle:** every decision above favors the option
  that makes the LLM more reliable (single-paradigm F#, schema-validated JSON, one math
  seam).
- **P2 — translatability over stack-binding:** MVU commits to a *shape* (owned state + pure
  update + projected view) rather than a framework; the math/optimization seams keep
  backends swappable.
- **P3 — state separable from UI:** FuncUI MVU makes the model the single state holder and
  the view a pure projection, so behavior is testable without a UI.
- **P4 — locality of reasoning ("LLMs follow imports; they do not follow magic"):** FuncUI
  views are plain F# function calls the model can follow.

---

## Sources (external references)

**Thin-film design**
- [TFCalc](https://www.hulinks.co.jp/en/tfcalc/) ·
  [Essential Macleod](https://jp.optosigma.com/en_jp/ems-1.html) ·
  [OptiLayer booklet](https://www.optilayer.com/images/OptiLayerBooklet2022.pdf) ·
  [Optical thin-film design overview](https://www.numberanalytics.com/blog/optical-thin-film-design-essentials) ·
  [OpenFilters & other programs (pyLuminous links)](https://sourceforge.net/p/pyluminous/wiki/links:%20thin%20film%20programs/)

**Optimization & inverse fitting**
- [OpenFilters: design, optimization & synthesis](https://www.researchgate.net/publication/5403710_OpenFilters_open-source_software_for_the_design_optimization_and_synthesis_of_optical_filters) ·
  [Needle optimization technique (Appl. Opt.)](https://pubmed.ncbi.nlm.nih.gov/17279158/) ·
  [Inverse problems of synthesis & characterization of multilayer coatings](https://link.springer.com/article/10.3103/S1063457609050074) ·
  [Three-step method for thin-film optimization](https://www.sciencedirect.com/science/article/abs/pii/S0030402612004342)

**Ellipsometry**
- [WVASE](https://www.jawoollam.com/ellipsometry-software/wvase) ·
  [CompleteEASE](https://www.jawoollam.com/ellipsometry-software/completeease) ·
  [DeltaPsi2 (HORIBA)](https://www.horiba.com/usa/scientific/products/detail/action/show/Product/deltapsi2-software-1648/) ·
  [Optical constants tutorial](https://www.jawoollam.com/resources/ellipsometry-tutorial/optical-constants)

**EUV lenses / curved mirrors (angle sensitivity & graded multilayers)**
- [Multilayer mirrors for EUV lithography (Laser Focus World)](https://www.laserfocusworld.com/optics/article/16566714/optics-for-scanning-multilayer-mirrors-enable-next-generation-euv-lithography) ·
  [EUV multilayer coatings for SDO/AIA (graded spacing on curved mirrors)](https://www.researchgate.net/publication/228721003_Development_and_testing_of_EUV_multilayer_coatings_for_the_Atmospheric_Imaging_Assembly_instrument_aboard_the_Solar_Dynamics_Observatory) ·
  [Interface-engineered EUV multilayer mirrors](https://www.sciencedirect.com/science/article/abs/pii/S0167931706001808)

**4×4 / Berreman toolkits**
- [Berreman4x4](https://github.com/Berreman4x4/Berreman4x4) ·
  [PyLlama (arXiv)](https://arxiv.org/pdf/2012.05945) ·
  [pyElli dispersions](https://pyelli.readthedocs.io/en/latest/dispersions.html)

**Materials data**
- [refractiveindex.info](https://refractiveindex.info/) ·
  [refractiveindex.info database paper (Nature Sci. Data)](https://www.nature.com/articles/s41597-023-02898-2) ·
  [Sellmeier formula (RP Photonics)](https://www.rp-photonics.com/sellmeier_formula.html)

**Illumination / optical-design suites**
- [Keysight LightTools](https://www.keysight.com/us/en/products/software/optical-solutions-software/optical-design-solutions/lighttools.html) ·
  [TracePro](https://lambdares.com/tracepro) ·
  [Ansys Speos](https://www.ansys.com/products/optics/ansys-speos) ·
  [Photopia](https://www.ltioptics.com/en/)

**Branching beam paths (reflected & transmitted / ray splitting)**
- [Sequential vs. non-sequential modes (Edmund Optics)](https://www.edmundoptics.com/knowledge-center/application-notes/optics/sequential-and-non-sequential-modes/) ·
  [Sequential vs non-sequential ray tracing (Zemax/Ansys)](https://support.zemax.com/hc/en-us/articles/1500005489061-What-is-the-difference-between-sequential-and-non-sequential-ray-tracing) ·
  [Splitting rays into many directions (OpticStudio non-sequential)](https://support.zemax.com/hc/en-us/articles/30802376332435-Splitting-Rays-into-Many-Directions-in-OpticStudio-Non-Sequential-Mode) ·
  [Tracing rays sequentially and non-sequentially (Synopsys)](https://www.synopsys.com/optical-solutions/learn/sequential-vs-nonsequential.html)

**Storage formats & libraries**
- [FSharp.SystemTextJson](https://github.com/Tarmil/FSharp.SystemTextJson) ·
  [FSharp.Data](https://fsprojects.github.io/FSharp.Data/) ·
  [MBrace.FsPickler](https://mbraceproject.github.io/FsPickler/)

**UI & charts**
- [Avalonia.FuncUI](https://github.com/fsprojects/Avalonia.FuncUI) ·
  [FuncUI docs](https://funcui.avaloniaui.net/) ·
  [Avalonia.FuncUI LICENSE (MIT)](https://github.com/fsprojects/Avalonia.FuncUI/blob/master/LICENSE) ·
  [MIT License terms (OSI)](https://opensource.org/license/mit) ·
  [F# desktop apps (fsharp.org)](https://fsharp.org/use/desktop-apps/) ·
  [Best practices for GitHub Copilot with Avalonia](https://avaloniaui.net/blog/best-practices-for-using-github-copilot-with-avalonia) ·
  [ScottPlot](https://scottplot.net/) ·
  [Plotly.NET](https://plotly.net/)

**Numerical libraries**
- [ALGLIB](https://www.alglib.net/) ·
  [ALGLIB optimization suite](https://www.alglib.net/optimization/) ·
  [ALGLIB Levenberg–Marquardt (minlm)](https://www.alglib.net/optimization/levenbergmarquardt.php) ·
  [ALGLIB C# manual](https://www.alglib.net/translator/man/manual.csharp.html)
