# Optical Constructor App — Recommended Feature List (rev. 2)

A bullet-point feature roadmap for turning this repo into a full-scale, GUI-driven
optical-constructor application. Each feature is tagged with a priority tier:

- **[Core]** — table-stakes; comparable apps are unusable without it. **(We will do this.)**
- **[Standard]** — expected by most users; defines a "serious" tool. **(We will do this.)**
- **[Niche]** — not planned now, kept as nice-to-have for later.

Where useful, items note whether the capability **already exists in the engine**
(and "just" needs UI/plumbing) or is **net-new**.

> **Revision note.** This rev. 2 incorporates review comments: explicit support for
> **lenses / non-flat mirrors** (needed to turn a plane wave into anything else, and
> critical for EUV, which is extremely angle-sensitive); making **combining elements
> into an end-to-end optical system** a first-class Core feature; adding **persistent
> UI customization** to Core; expanding **optimization & inverse fitting** with web
> research (to port the user's existing partial Wolfram implementation to F#);
> elevating **human-convenient units of measure with automatic conversion** (mm, mkm,
> nm, Å, eV, …; mixed units across a system; a default unit per element) to a Core
> feature (see §1, §2); and several tier moves (see §1, §3, §4, §7).

---

## 0. Context — what this repo already is

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
  wedge angle, or arbitrary variables (`Variables.fs`), with 1D/3D Plotly.NET
  charts (`Charting.fs`) and ready-made example scripts (`Analytics/Examples/*.fsx`).

So the **computational core is strong**; the gap to an "optical constructor app" is
almost entirely **UI, data management, materials/sources libraries, design
synthesis (optimization/fitting), and persistence**.

> **Engine note for curved elements (see §1).** The Berreman core is a 1D plane-wave
> formalism. Lenses and non-flat mirrors require treating each surface as a *local*
> interface seen by a *local* plane wave (a ray/beam carrying its own direction and
> polarization), with the existing 4×4 solver evaluated at the local angle of
> incidence — and, for focusing, ray/beam propagation between elements. This is the
> one place where a modest extension beyond the pure 1D model is unavoidable.

Benchmarked against established tools: thin-film design (Essential Macleod, OptiLayer,
TFCalc, OpenFilters), ellipsometry analysis (WVASE, CompleteEASE, DeltaPsi2),
anisotropic 4×4 toolkits (Berreman4x4, PyLlama, pyElli), and illumination/optical-design
suites (LightTools, TracePro, Speos).

---

## 1. Optical-system construction & UI

- **[Core]** **Compose an end-to-end optical system from elements** — chain optical
  elements in sequence, e.g. **light source → polarizer → sample → analyzer →
  detector**. This is the central act of an "optical constructor": pick elements from
  a palette, order them, connect them, and run the whole train. (Net-new
  orchestration layer above the existing single-system solver.)
- **[Core]** **Lenses and non-flat (curved) mirrors as elements** — without focusing
  elements a plane wave can never be converted into a converging/diverging beam or an
  image. Support spherical/aspheric lenses and concave/convex mirrors, each with
  radius/curvature, aperture, substrate material, and **coatings on their surfaces**.
  **Critical for EUV**, where multilayer-mirror reflectance is strongly angle-of-
  incidence dependent and curved mirrors typically need **depth-graded multilayer
  spacing** (~5% center-to-edge) to keep reflectance uniform across the changing local
  angle. (Net-new; uses the local-interface treatment from §0.)
- **[Core]** Visual **stack/layer editor** — add, delete, reorder, duplicate, and
  group layers in a list/table; edit thickness and material per row. (Engine has
  `Layer`/`BaseOpticalSystem`/`OpticalSystem`; needs a UI surface.)
- **[Core]** **Incident & exit medium selectors** (upper/lower half-spaces) with
  refractive index or material reference.
- **[Core]** **Schematic cross-section view** of the stack drawn to scale, color-
  coded by material, with thickness labels and the incident-light ray/angle drawn.
- **[Core]** **Substrate type switch**: thin film vs. thick **plate** vs. **wedge**
  (with wedge angle), exposing the existing multi-reflection handling.
- **[Core]** **Per-element default unit of measure** — each layer/element/source
  remembers and displays the unit natural to it (a plate in mm, a film in nm, a source
  in nm/µm/eV) while the system mixes units freely; see the units-of-measure item in
  §2 for the full conversion behaviour.
- **[Core]** **Persistent UI customization** — remember the user's working
  environment between sessions: a **board / palette of commonly used elements**
  (pin/favorite frequently used layers, materials, sources, and whole sub-assemblies,
  grouped and named for quick reuse), the **last working folder(s)** and **recent
  files/projects**, window/panel layout, and toolbar contents. Customizations persist
  as defaults for new projects. (Net-new; aligns with how CAD/Photoshop/Vectorworks
  let users pin favorites and save workspaces.)
- **[Standard]** **Per-layer anisotropy/orientation controls**: pick material class
  (isotropic / uniaxial / biaxial / active) and set **crystal-axis rotation**
  (Euler/rotateX/Y/Z) — already supported by `Layer.rotate`/`OpticalSystem.rotate`.
- **[Standard]** **Repeat/period builder** — define a unit cell (e.g. `[H L]`) and a
  repeat count to generate Bragg mirrors / DBRs / EUV Mo–Si stacks without manual
  duplication (examples currently hand-write 40–100 layers).
- **[Standard]** **Live recalculation** as parameters change, with a busy/cancel
  indicator for heavy sweeps.
- **[Standard]** **Drag-and-drop** of materials from the library onto layers.
- **[Standard]** **Multi-system workspace** — hold several systems side by side for
  comparison (engine already has `plotComparison`).
- **[Standard]** **Gradient-index / inhomogeneous layers** auto-discretized into
  sub-layers. *(moved up from Niche)*
- **[Standard]** **Stack templates/wizards** (AR coating, bandpass filter, beam
  splitter, polarizer, waveplate, DBR) that pre-populate a starting design.
  *(moved up from Niche)*
- **[Standard]** **2D/3D rendered view** of the element/system with the beam path.
  *(moved up from Niche)*
- **[Niche]** **Roughness/interface layers** (EMA/effective-medium intermixing).

## 2. Optical substances / materials management

- **[Core]** **Materials library** with built-in common materials and user-defined
  entries; searchable, with categories (glasses, metals, semiconductors, crystals).
- **[Core]** **Dispersion model editor** — define n,k vs. wavelength via tabulated
  data or analytic models: **Sellmeier, Cauchy, Lorentz/Drude-Lorentz, Tauc-Lorentz,
  Gaussian/harmonic oscillators, Drude (metals), constant n+ik**. (Engine has a
  `Dispersion` abstraction; needs a model library + UI.)
- **[Core]** **Import from refractiveindex.info** (its open YAML format) and from
  CSV/tab data; export the same.
- **[Standard]** **Anisotropic material definition** — enter principal n_o/n_e (uniaxial)
  or n_x/n_y/n_z (biaxial), full ε tensor, μ, and gyration/ρ (chiral/magneto-optic),
  matching existing `Eps`/`Mu`/`Rho` support.
- **[Standard]** **Material preview plot** of n(λ) and k(λ) (and ρ, ε components) over
  a chosen range — already prototyped as `plotN11`, `plotXi11`, `plotRho11`, etc.
- **[Core]** **Human-convenient units of measure with automatic conversion.** The
  system must let every quantity be entered and displayed in whatever unit is natural
  for it, and **different parts of the same system may use different units**:
  - **Length / thickness**: m, mm, **mkm (µm)**, nm, **Å** — e.g. a thick plate in
    **mm**, a thin film in **nm**.
  - **Wavelength / photon energy**: nm (e.g. a **green laser** ≈ 532 nm), **mkm** (an
    **IR laser**, e.g. 10.6 µm), **Å**, and **eV** (e.g. a **free-electron laser**,
    EUV/X-ray) — with the energy↔wavelength conversion **E[eV] = 1239.84 / λ[nm]**
    (hc) handled transparently, plus the spectroscopy wavenumber cm⁻¹ as a bonus.
  - **Each element/parameter carries its own *default* unit of measure** (the unit it
    was created in / is most natural for it), so a project naturally mixes mm plates,
    nm films, and eV sources without the user converting anything by hand.
  - **Conversions are automatic and lossless internally** (store one canonical SI base
    — e.g. meters — and convert only at the UI/IO boundary), so charts, sweeps, and
    fits can be requested in any unit. This **extends the existing units-of-measure**
    support (`Constants.fs` already has nm/mkm/mm/m); the net-new work is the **eV (and
    cm⁻¹) photon-energy conversions** and the **per-element default-unit** plumbing.
- **[Standard]** **Unit presets & display formatting** — quick unit switching on any
  axis/field, sensible significant figures, and "show in eV / nm / µm" toggles on
  spectral plots.
- **[Standard]** **Temperature/composition dependence** (thermo-optic dn/dT) as
  optional model parameters.
- **[Niche]** **Kramers–Kronig consistency check / enforcement** for entered data.
- **[Niche]** **Mixing models** (Bruggeman/Maxwell-Garnett EMA) to synthesize a
  material from constituents + volume fraction.
- **[Niche]** **Nonlinear / electro-optic coefficients** stored for future modules.

## 3. Light sources & illumination

- **[Core]** **Source/incident-light editor**: wavelength (single or range),
  incidence angle, **polarization state** (linear angle, s/p, circular, elliptical
  via ellipticity), and intensity. (All present in `Fields.fs` / `StandardLightVariables.fs`.)
- **[Standard]** **Polarization presets** (s, p, 45°, RCP/LCP, unpolarized) and a
  **Poincaré-sphere / polarization-ellipse** picker.
- **[Standard]** **Spectral source profiles** — flat, blackbody, standard illuminants
  (D65, A), laser line, or imported spectrum — for integrated/photometric results.
- **[Standard]** **Angle/cone definition** — collimated vs. finite cone angle and
  detector acceptance, as in OptiLayer (averages over cone/bandwidth).
- **[Standard]** **Gaussian-beam source** — the engine already has FFT-based
  Gaussian-beam scaffolding (`FourierTransform.fs`); expose beam waist/divergence and
  propagate. *(moved up from Niche)*
- **[Standard]** **Coherent vs. incoherent layer flags** (thick substrate treated
  incoherently while films stay coherent). *(moved up from Niche)*
- **[Standard]** **Multiple simultaneous sources** and unpolarized averaging.
  *(moved up from Niche)*

## 4. Calculations / analysis

- **[Core]** **Spectral & angular scans** of R, T (and absorption A = 1−R−T), s/p
  split, over wavelength or angle — the engine's primary output today.
- **[Core]** **Parameter sweeps** over any model variable (thickness, angle,
  polarization, ellipticity, wedge angle, material parameter) — existing
  `RangedVariable`/`ArbitraryVariableRange`/`calculate`/`calculate3D`.
- **[Standard]** **Polarimetric outputs**: Stokes vectors, **Jones & Mueller
  matrices**, ellipticity, azimuth, degree of polarization (mostly present; finish
  the commented-out `muellerMatrix` path in `FieldFunctions.fs`).
- **[Standard]** **Ellipsometric quantities** Ψ, Δ (and N, C, S) vs. λ/angle — the
  key deliverable for the ellipsometry user base; derivable from existing field
  amplitudes.
- **[Standard]** **In-layer field / EFI profile** — |E|², Poynting, absorption per
  layer vs. depth (engine computes E/H/D/B/Poynting); critical for laser-damage and
  absorber placement (TFCalc's "EFI").
- **[Standard]** **Absorptance per layer** and total absorbed-power breakdown.
- **[Standard]** **Color calculation** — convert reflectance/transmittance spectra to
  CIE XYZ/Lab/sRGB swatches (standard in coating tools).
- **[Standard]** **Convergence controls** exposed (number of internal reflections,
  numerical thresholds in `SolverParameters`). *(moved up from Niche)*
- **[Niche]** **Group delay / GDD / phase** of reflection/transmission (for chirped
  mirrors / ultrafast optics).
- **[Niche]** **Critical-angle / TIR analysis** and evanescent-field handling
  (example `TotalRefl_Glass.fsx` exists; harden grazing-incidence numerics).
- **[Niche]** **Photometric/radiometric integrals** (luminous transmittance, solar
  rejection, integrated reflectance over a band).
- **[Niche]** **Monte-Carlo / tolerance & sensitivity analysis** over thickness/index
  errors with statistical yield reporting.

## 5. Design synthesis, optimization & fitting

> The single biggest functional gap vs. commercial tools and the highest-value
> addition. The user already has **partial Wolfram/Mathematica code** for this that was
> never ported to F#; this section is the target spec for that port. The repo carries
> minimization/ODE scaffolding under `!Temp/Minimization`, and MathNet.Numerics
> provides nonlinear least-squares/optimization primitives to build on.
>
> Web research note — coating/ellipsometry design splits into two families:
> **refinement** (improve a *given* starting design by small adjustments of layer
> thicknesses; fast but sensitive to the starting point) and **synthesis** (build the
> design up, e.g. needle insertion; far less sensitive to the starting point and can
> change the number of layers). A good app offers both.

- **[Core]** **Target / merit-function editor** — specify desired R/T/Ψ/Δ vs. λ/angle
  as a weighted least-squares merit function, with per-target **weights, tolerances,
  and one-sided/inequality targets** (e.g. "R ≤ 0.5%").
- **[Core]** **Local refinement optimization** of layer thicknesses (and optionally
  indices/geometry) — **Levenberg–Marquardt** as the workhorse, plus gradient/quasi-
  Newton and Nelder–Mead simplex. Support **bounded parameters and inequality
  targets** (forbid negative thickness, bound n to physical ranges); robust normal-
  equation solves (QR / Gauss–Jordan). This mirrors the Wolfram `FindMinimum` /
  `NonlinearModelFit` workflow being ported.
- **[Standard]** **Inverse fitting / characterization from measured data** — load
  measured **photometric (R/T)** and/or **ellipsometric (Ψ, Δ)** spectra and regress
  model parameters (thicknesses, optical constants, nonuniformity, roughness). This
  is the core ellipsometry "reverse engineering" workflow (WVASE/CompleteEASE) and a
  formal **inverse problem** stated as constrained optimization.
- **[Standard]** **Fit-quality reporting** — χ²/MSE, parameter confidence intervals,
  correlation matrix, residual plots, and overlay of fit vs. measured data.
- **[Standard]** **Synthesis methods (start-point-insensitive)** — **needle
  optimization** (automatically insert a thin "needle" layer at the position that most
  decreases the merit function, growing the layer count without bounding the parameter
  space) combined with **tunneling / gradual evolution** to escape local minima; this
  is the technique behind TFCalc / OptiLayer / OpenFilters.
- **[Standard]** **Global optimization** — simulated annealing and genetic/evolutionary
  algorithms for difficult, multimodal merit landscapes where refinement stalls.
- **[Niche]** **Automatic layer-count reduction/synthesis** (remove negligible layers;
  "design simplification" after needle growth).
- **[Niche]** **Multi-sample / multi-angle simultaneous fitting** (shared parameters
  across data sets).
- **[Niche]** **Manufacturability constraints & error self-compensation** (min
  thickness, available materials, quarter-wave snapping; reoptimization that exploits
  the error self-compensation mechanism for monitored deposition).

## 6. Charts & visualization

- **[Core]** **Interactive 1D plots** (R/T/Ψ/Δ vs. λ or angle) with zoom/pan, multiple
  traces, legends — already via Plotly.NET.
- **[Core]** **Overlay & comparison** of multiple systems/curves (existing
  `plotComparison`).
- **[Standard]** **2D/3D surface & contour plots** (e.g. R vs. λ × angle) — existing
  `plot3D`.
- **[Standard]** **Chart customization** — axis ranges/scales (log), titles, colors,
  line styles, units, gridlines, annotations; per-trace show/hide.
- **[Standard]** **Cursor/readout & markers** — hover values, peak/min finders, FWHM,
  delta between cursors.
- **[Standard]** **Specialized plots** — polarization ellipse, Poincaré sphere, field-
  depth profile, CIE chromaticity diagram, dispersion (n,k) plots.
- **[Niche]** **Linked/synchronized axes** across plots; small-multiples dashboards.
- **[Niche]** **Animation** of a sweep parameter (e.g. thickness morphing).
- **[Niche]** **Live cross-probe** between schematic, table, and chart (click a layer →
  highlight its field contribution).

## 7. Project management, persistence & I/O

- **[Core]** **Save/Open project files** — full state (systems, materials, sources,
  sweeps, chart settings) in a human-readable, versioned format (JSON recommended;
  F# records serialize cleanly).
- **[Core]** **Export chart data** to CSV/Excel and **chart images** to PNG/SVG/PDF.
- **[Standard]** **Recent files, autosave, and undo/redo** of model edits.
- **[Standard]** **Report generator** — one-click PDF/HTML report with schematic,
  parameters, plots, and fit results (DeltaPsi2/CompleteEASE-style).
- **[Standard]** **Material library import/export** as a shareable file.
- **[Standard]** **Version control / design history & diff** between design revisions.
  *(moved up from Niche)*
- **[Niche]** **Scripting / automation API** — keep the `.fsx` scripting path as a
  first-class "advanced" interface; let the GUI generate the equivalent script.
- **[Niche]** **Batch processing** of many files/designs (engine already uses `PSeq`
  parallelism).
- **[Niche]** **Interop import/export** with other tools' formats (e.g. Macleod/
  OptiLayer/Filmetrics data, Zemax coating tables).

## 8. Environment customization & UX

- **[Standard]** **Preferences**: default units, wavelength range, number of sweep
  points, decimal precision, solver defaults.
- **[Standard]** **Light/dark theme** and configurable chart color palettes.
- **[Standard]** **Dockable/resizable panels** (stack, materials, sources, chart,
  results) with saved layouts/workspaces. (Works together with the §1 persistent UI
  customization / favorites board.)
- **[Standard]** **Input validation & physical sanity warnings** (e.g. n<1 absorbing
  region, non-causal data, thickness ≤ 0).
- **[Standard]** **Progress, cancellation, and timing** for long sweeps/optimizations
  (examples currently use `#time`).
- **[Niche]** **Localization / units locale**, keyboard shortcuts, and a command palette.
- **[Niche]** **Plugin/extension points** for custom dispersion models, merit
  functions, or chart types.
- **[Niche]** **Cloud/remote compute** for heavy 3D sweeps or batch fits.

## 9. Documentation, help & onboarding

- **[Standard]** **In-app material/units glossary and tooltips**, plus a **gallery of
  worked examples** built from the existing `Analytics/Examples/*.fsx`
  (AR coating, DBR, EUV Mo/Si, active crystal, wedge, dispersive glass).
- **[Standard]** **Getting-started tutorial / sample projects** shipped with the app.
- **[Niche]** **Theory/reference panel** linking results to the underlying Berreman
  formalism and citing sources.

---

## Suggested build order (Core/Standard first; Niche = later)

1. **MVP (Core)**: element-composition workspace (source → polarizer → sample →
   analyzer → detector), stack/layer editor + schematic, **lens / curved-mirror
   elements**, source editor, materials library (Sellmeier/Cauchy/tabulated +
   refractiveindex.info import), spectral/angular R/T/A scan, interactive plot,
   save/open project, CSV/PNG export, target/merit editor + local (LM) optimization,
   **human-convenient units with automatic conversion** (mm/mkm/nm/Å/eV, mixed units,
   per-element default unit), and **persistent UI customization** (favorites board,
   recent folders/files).
2. **v2 (Standard)**: anisotropy/orientation UI, GRIN layers, templates/wizards,
   2D/3D rendered view, ellipsometric Ψ/Δ + Mueller/Stokes, field/EFI profiles, color
   calc, comparison & 3D plots, **inverse fitting to measured data + fit-quality
   reporting**, **needle/global synthesis**, Gaussian-beam & multi-source, report
   generator, version history/diff, preferences/themes.
3. **Later (Niche)**: roughness/EMA, tolerance/Monte-Carlo, GDD/phase, KK
   enforcement, manufacturability/error-self-compensation, scripting/automation
   surface, plugins, batch/cloud, third-party interop.

---

### Sources (web research)

- Thin-film design: [TFCalc](https://www.hulinks.co.jp/en/tfcalc/),
  [Essential Macleod](https://jp.optosigma.com/en_jp/ems-1.html),
  [OptiLayer booklet](https://www.optilayer.com/images/OptiLayerBooklet2022.pdf),
  [thin-film design overview](https://www.numberanalytics.com/blog/optical-thin-film-design-essentials),
  [OpenFilters & others (pyLuminous links)](https://sourceforge.net/p/pyluminous/wiki/links:%20thin%20film%20programs/)
- Optimization & inverse fitting: [OpenFilters: design, optimization & synthesis](https://www.researchgate.net/publication/5403710_OpenFilters_open-source_software_for_the_design_optimization_and_synthesis_of_optical_filters),
  [Needle optimization technique (Appl. Opt.)](https://pubmed.ncbi.nlm.nih.gov/17279158/),
  [Inverse problems of synthesis & characterization of multilayer coatings](https://link.springer.com/article/10.3103/S1063457609050074),
  [Error self-compensation in optical coating technology](https://www.tandfonline.com/doi/full/10.1080/17415977.2017.1395424),
  [Three-step method for thin-film optimization](https://www.sciencedirect.com/science/article/abs/pii/S0030402612004342)
- Ellipsometry: [WVASE](https://www.jawoollam.com/ellipsometry-software/wvase),
  [CompleteEASE](https://www.jawoollam.com/ellipsometry-software/completeease),
  [DeltaPsi2 (HORIBA)](https://www.horiba.com/usa/scientific/products/detail/action/show/Product/deltapsi2-software-1648/),
  [optical constants tutorial](https://www.jawoollam.com/resources/ellipsometry-tutorial/optical-constants)
- EUV lenses / curved mirrors (angle sensitivity & graded multilayers):
  [Multilayer mirrors for EUV lithography (Laser Focus World)](https://www.laserfocusworld.com/optics/article/16566714/optics-for-scanning-multilayer-mirrors-enable-next-generation-euv-lithography),
  [EUV multilayer coatings for SDO/AIA (graded spacing on curved mirrors)](https://www.researchgate.net/publication/228721003_Development_and_testing_of_EUV_multilayer_coatings_for_the_Atmospheric_Imaging_Assembly_instrument_aboard_the_Solar_Dynamics_Observatory),
  [Interface-engineered EUV multilayer mirrors](https://www.sciencedirect.com/science/article/abs/pii/S0167931706001808)
- 4×4 / Berreman toolkits: [Berreman4x4](https://github.com/Berreman4x4/Berreman4x4),
  [PyLlama (arXiv)](https://arxiv.org/pdf/2012.05945),
  [pyElli dispersions](https://pyelli.readthedocs.io/en/latest/dispersions.html)
- Materials data: [refractiveindex.info](https://refractiveindex.info/),
  [refractiveindex.info database paper (Nature Sci. Data)](https://www.nature.com/articles/s41597-023-02898-2),
  [Sellmeier formula (RP Photonics)](https://www.rp-photonics.com/sellmeier_formula.html)
- Persistent UI customization / workspaces & favorites:
  [Vectorworks workspace customization](https://www.vectorworks.net/en-US/newsroom/workspace-customization),
  [Personalize the user experience (Microsoft Dynamics 365)](https://learn.microsoft.com/en-us/dynamics365/fin-ops-core/dev-itpro/get-started/personalize-user-experience),
  [Customizing the Photoshop interface (workspaces, panels, shortcuts)](https://jkost.com/blog/2021/05/30-ways-to-customize-the-photoshop-interface.html)
- Illumination/optical-design suites: [Keysight LightTools](https://www.keysight.com/us/en/products/software/optical-solutions-software/optical-design-solutions/lighttools.html),
  [TracePro](https://lambdares.com/tracepro),
  [Ansys Speos](https://www.ansys.com/products/optics/ansys-speos),
  [Photopia](https://www.ltioptics.com/en/)
