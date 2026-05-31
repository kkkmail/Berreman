# Optical Constructor App — Recommended Feature List

A bullet-point feature roadmap for turning this repo into a full-scale, GUI-driven
optical-constructor application. Each feature is tagged with a priority tier:

- **[Core]** — table-stakes; comparable apps are unusable without it.
- **[Standard]** — expected by most users; defines a "serious" tool.
- **[Niche]** — valued by specialists/power users; rarely used but differentiating.

Where useful, items note whether the capability **already exists in the engine**
(and "just" needs UI/plumbing) or is **net-new**.

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
synthesis (optimization/fitting), and persistence**. The list below is framed
accordingly, benchmarked against established tools: thin-film design
(Essential Macleod, OptiLayer, TFCalc, OpenFilters), ellipsometry analysis
(WVASE, CompleteEASE, DeltaPsi2), anisotropic 4×4 toolkits (Berreman4x4, PyLlama,
pyElli), and illumination/optical-design suites (LightTools, TracePro, Speos).

---

## 1. Optical-system construction & UI

- **[Core]** Visual **stack/layer editor** — add, delete, reorder, duplicate, and
  group layers in a list/table; edit thickness and material per row. (Engine has
  `Layer`/`BaseOpticalSystem`/`OpticalSystem`; needs a UI surface.)
- **[Core]** **Incident & exit medium selectors** (upper/lower half-spaces) with
  refractive index or material reference.
- **[Core]** **Schematic cross-section view** of the stack drawn to scale, color-
  coded by material, with thickness labels and the incident-light ray/angle drawn.
- **[Core]** **Substrate type switch**: thin film vs. thick **plate** vs. **wedge**
  (with wedge angle), exposing the existing multi-reflection handling.
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
- **[Niche]** **Gradient-index / inhomogeneous layers** auto-discretized into sub-layers.
- **[Niche]** **Stack templates/wizards** (AR coating, bandpass filter, beam splitter,
  polarizer, waveplate, DBR) that pre-populate a starting design.
- **[Niche]** **Roughness/interface layers** (EMA/effective-medium intermixing).
- **[Niche]** **2D/3D rendered view** of the element with the beam path.

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
- **[Standard]** **Units management** with the existing units-of-measure (nm/µm/mm/m,
  eV↔nm conversion for the spectroscopy crowd).
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
- **[Niche]** **Gaussian-beam source** — the engine already has FFT-based Gaussian-beam
  scaffolding (`FourierTransform.fs`); expose beam waist/divergence and propagate.
- **[Niche]** **Coherent vs. incoherent layer flags** (thick substrate treated
  incoherently while films stay coherent).
- **[Niche]** **Multiple simultaneous sources** and unpolarized averaging.

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
- **[Niche]** **Group delay / GDD / phase** of reflection/transmission (for chirped
  mirrors / ultrafast optics).
- **[Niche]** **Critical-angle / TIR analysis** and evanescent-field handling
  (example `TotalRefl_Glass.fsx` exists; harden grazing-incidence numerics).
- **[Niche]** **Photometric/radiometric integrals** (luminous transmittance, solar
  rejection, integrated reflectance over a band).
- **[Niche]** **Monte-Carlo / tolerance & sensitivity analysis** over thickness/index
  errors with statistical yield reporting.
- **[Niche]** **Convergence controls** exposed (number of internal reflections,
  numerical thresholds in `SolverParameters`).

## 5. Design synthesis, optimization & fitting

> This is the single biggest functional gap vs. commercial tools and the highest-value
> addition. The repo carries minimization/ODE scaffolding under `!Temp/Minimization`.

- **[Core]** **Target/merit-function editor** — specify desired R/T/Ψ/Δ vs. λ/angle
  with weights and tolerances.
- **[Core]** **Local optimization** of layer thicknesses (and indices) to a target
  (e.g. Levenberg–Marquardt / gradient / simplex).
- **[Standard]** **Inverse fitting to measured data** (load measured R/T or ellipsometric
  Ψ/Δ and regress model parameters) — the core ellipsometry workflow (WVASE/CompleteEASE).
- **[Standard]** **Fit-quality reporting** — χ²/MSE, parameter confidence intervals,
  correlation matrix, residual plots.
- **[Standard]** **Global/refinement strategies** — needle optimization & tunneling
  (the technique that made TFCalc/OptiLayer powerful), simulated annealing, or
  evolutionary search.
- **[Niche]** **Automatic layer-count synthesis** (add/remove layers automatically).
- **[Niche]** **Multi-sample / multi-angle simultaneous fitting** (shared parameters).
- **[Niche]** **Manufacturability constraints** (min thickness, available materials,
  quarter-wave snapping) and **error-compensation/reoptimization** for monitoring.

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
- **[Niche]** **Scripting / automation API** — keep the `.fsx` scripting path as a
  first-class "advanced" interface; let the GUI generate the equivalent script.
- **[Niche]** **Batch processing** of many files/designs (engine already uses `PSeq`
  parallelism).
- **[Niche]** **Interop import/export** with other tools' formats (e.g. Macleod/
  OptiLayer/Filmetrics data, Zemax coating tables).
- **[Niche]** **Version control / design history & diff** between design revisions.

## 8. Environment customization & UX

- **[Standard]** **Preferences**: default units, wavelength range, number of sweep
  points, decimal precision, solver defaults.
- **[Standard]** **Light/dark theme** and configurable chart color palettes.
- **[Standard]** **Dockable/resizable panels** (stack, materials, sources, chart,
  results) with saved layouts/workspaces.
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

## Suggested build order (MVP → full)

1. **MVP (Core)**: stack/layer editor + schematic, source editor, materials library
   with Sellmeier/Cauchy/tabulated + refractiveindex.info import, spectral/angular
   R/T/A scan, interactive plot, save/open project, CSV/PNG export.
2. **v2 (Standard)**: anisotropy/orientation UI, ellipsometric Ψ/Δ + Mueller/Stokes,
   field/EFI profiles, color calc, comparison & 3D plots, optimization to targets,
   fitting to measured data with quality metrics, report generator, preferences/themes.
3. **v3 (Niche)**: needle/global optimization, tolerance/Monte-Carlo, Gaussian beam,
   GDD/phase, EMA/roughness, scripting/automation surface, plugins, batch/cloud.

---

### Sources (web research)

- Thin-film design: [TFCalc](https://www.hulinks.co.jp/en/tfcalc/),
  [Essential Macleod](https://jp.optosigma.com/en_jp/ems-1.html),
  [OptiLayer booklet](https://www.optilayer.com/images/OptiLayerBooklet2022.pdf),
  [thin-film design overview](https://www.numberanalytics.com/blog/optical-thin-film-design-essentials),
  [OpenFilters & others (pyLuminous links)](https://sourceforge.net/p/pyluminous/wiki/links:%20thin%20film%20programs/)
- Ellipsometry: [WVASE](https://www.jawoollam.com/ellipsometry-software/wvase),
  [CompleteEASE](https://www.jawoollam.com/ellipsometry-software/completeease),
  [DeltaPsi2 (HORIBA)](https://www.horiba.com/usa/scientific/products/detail/action/show/Product/deltapsi2-software-1648/),
  [optical constants tutorial](https://www.jawoollam.com/resources/ellipsometry-tutorial/optical-constants)
- 4×4 / Berreman toolkits: [Berreman4x4](https://github.com/Berreman4x4/Berreman4x4),
  [PyLlama (arXiv)](https://arxiv.org/pdf/2012.05945),
  [pyElli dispersions](https://pyelli.readthedocs.io/en/latest/dispersions.html)
- Materials data: [refractiveindex.info](https://refractiveindex.info/),
  [refractiveindex.info database paper (Nature Sci. Data)](https://www.nature.com/articles/s41597-023-02898-2),
  [Sellmeier formula (RP Photonics)](https://www.rp-photonics.com/sellmeier_formula.html)
- Illumination/optical-design suites: [Keysight LightTools](https://www.keysight.com/us/en/products/software/optical-solutions-software/optical-design-solutions/lighttools.html),
  [TracePro](https://lambdares.com/tracepro),
  [Ansys Speos](https://www.ansys.com/products/optics/ansys-speos),
  [Photopia](https://www.ltioptics.com/en/)
