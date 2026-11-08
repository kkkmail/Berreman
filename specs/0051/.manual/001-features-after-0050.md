# 001 — Proposed features after 0050

This is a proposal for discussion. It assumes spec 0050 is done in full:
- kit folders;
- the SQLite library and the catalogue files;
- the online material sources (refractiveindex.info, PTB OCDB, Henke, AGF);
- item models, setup slots with N objects in series;
- experiments v2 with the import wizard;
- studies with staged and joint fits, multi-start, and publish / promote;
- the gyrotropic Measurement Planner.

Nothing below repeats 0050. Items that 0050 placed out of scope (`specs/0050/.manual/001` §W, `.spec-md` §8)
are included where they still make sense.

**Legend.**
- I = impact, E = effort, each High / Medium / Low.
- Effort scale: L ≈ one spec section, mostly wiring existing code; M ≈ several sections; H ≈ a spec of its own.
- "Exists, unwired" means the code is in the tree, compiled and tested, but no window reaches it.
- Within each group, items are sorted by impact (H → L), then by effort (L → H).

**Observation (code survey, 2026-11-08).** Much of spec 0022's engine work was never wired into a window. This
includes the synthesis page, the chart tools, the polarization plots, the source spectra / cone / Gaussian beam,
graded layers, in-layer fields, colour, validation, the 3-D view and localization. These items are the cheap wins
below.

## 1. Inverse problem and fitting

- **Spectroscopic anisotropic fits** · I H · E M
  - The dispersion coefficients of every tensor component, gyration included, become study unknowns. Multi-colour
    data then resolves the retardance order.
  - 0044 `020` calls this "the single largest capability gap" (candidate 2). 0050's acceptance fit (AT-2) is
    single-λ.
- **Depolarizing measurement model** · I H · E M
  - Thickness non-uniformity, spectral bandwidth, angular spread and incoherent backside reflections become fittable
    parameters. Real data is never Mueller–Jones, so without them the fit absorbs artefacts.
  - The cone, bandwidth and coherence types exist, unwired, in `SourceSpec.fs`.
  - Sources: 0044 `020` candidate 4; 0022 `010` §3; CompleteEASE practice.
- **Global search beyond multi-start** · I M · E L
  - Grid-scan "global fit" seeding, plus the existing simulated annealing and genetic algorithm (`Synthesis.fs`) as
    study engines, for multimodal retardance-order landscapes.
  - Sources: 0044 `002` §2.5; 0022 `010` §5; CompleteEASE global fit; DeltaPsi2 multi-guess.
- **Mueller-matrix post-processing** · I M · E L
  - Cloude nearest-physical filtering, plus Lu–Chipman and differential decompositions (retardance, diattenuation,
    depolarization, rotation) of measured and fitted matrices.
  - Sources: 0042 `004` §8; 0044 `020` candidate 4; pySCATMECH; pymueller.
- **Virtual-experiment ensemble** · I M · E L
  - "Simulate N noisy repeats and refit" in the app, using the bench noise model 0050 promotes, with the scatter
    shown against the predicted σ.
  - First use: the missing noisy run of the feasible biaxial design (0044 `020` candidate 1).
- **Chiroptical artefact guard** · I M · E L
  - Flag apparent circular dichroism that is really LD′·LB − LB′·LD cross-talk (0044 `020` candidate 6).
- **Vendor tolerances as priors** · I M · E L
  - Item models carry thickness (±20 µm) and cut-misalignment tolerances, which become default priors on nuisance
    unknowns (0044 `020` candidate 5; `017`).
- **Order-independent CB/LB observable** · I M · E M
  - The two-modulator ratio of Arteaga et al.: it needs no retardance order and works with mm-thick plates
    (0044 `020` candidate 3).
- **Model-free optical constants** · I M · E M
  - λ-by-λ n, k inversion, then a fit of a Kramers–Kronig-consistent B-spline (§6). This is standard ellipsometry
    practice (Johs & Hale).
- **Bayesian posteriors** · I M · E M
  - MCMC over a study's unknowns with priors (PTB σ, tolerances), shown as corner plots beside the Jacobian σ and
    the profile likelihood (0046 `018` §3; refnx).
- **Batch / recipe fitting** · I M · E M
  - Run one study template over many samples or files, and tabulate the results (DeltaPsi2 recipes and mapping).

## 2. Measurement Planner

- **Targets beyond gyrotropic crystals** · I H · E M
  - Isotropic and absorbing films (thickness ↔ n), non-active uniaxial and biaxial crystals, and multilayer stacks.
  - 0046 `018` asked which target comes first; 0050 did gyrotropic.
- **Tolerance-aware plans** · I M · E L
  - Rank candidates with nuisance parameters (thickness, misalignment, dial zeros) marginalised, so a plan survives
    vendor tolerances (0044 `017`: 0.11 wave per degree).
- **Sample-procurement planning** · I M · E M
  - Candidates include cuts and thicknesses to order, not only experiments on the samples already owned.
  - Example: the thick near-optic-axis plate that buys back the ~250× gyration penalty (0044 `015`).
- **Shipped playbooks** · I M · E M
  - Precomputed layer-1 and coarse layer-2 answers per crystal class × common kit (0046 `018` §3; 0050 §W).
- **Sequential Bayesian design** · I M · E H
  - Expected information gain and an adaptive "measure → update → next" loop (0046 `018` §3; 0050 §W).

## 3. Forward physics and experiment kinds

- **Graded, rough and mixed layers** · I H · E M
  - Graded-index layers (`GradientLayer.discretize` exists), effective-medium layers (Bruggeman, Maxwell-Garnett)
    and interface roughness.
  - Sources: 0022 `010` §1; 0027 `033` scope note; DeltaPsi2.
- **Realistic sources in the run law** · I M · E L
  - Spectral profiles, a finite cone, coherence and the Gaussian beam from `SourceSpec.fs` (exists, unwired), in
    the source editor and the run law (0022 `010` §3).
- **Temperature and field as experiment variables** · I M · E L
  - dn/dT: `ThermoOptic` exists and is always `None`.
  - Faraday and Kerr presets on the existing Polder μ (0022 `010` §2).
- **Twisted and helical media** · I M · E M
  - Cholesteric, twisted-nematic and sculptured films, built by automatic sub-layering of a rotating director
    (PyLlama; TechWiz and LCD Master usage).
- **Focusing elements with real physics** · I M · E M
  - The local-interface model for lenses and curved mirrors, and EUV depth-graded multilayers. `CurvedElements.fs`
    exists, unwired; the table only routes the rays (0022 `010` §1).
- **Conoscopy (divergent-beam) experiments** · I M · E H
  - One shot samples the whole internal-angle range. It is also a new Planner candidate kind (0044 `002` §2.3,
    family 4).

## 4. Thin-film design (synthesis)

- **Design workbench** · I H · E L
  - Wire `SynthesisFitPage`, whose engine and page model exist with tests but have no view:
    - the target / merit editor (weights, tolerances, one-sided targets);
    - LM refinement, needle + tunnelling, annealing and GA;
    - the fit report.
  - Source: 0022 `010` §5.
- **Stack templates** · I M · E L
  - AR, DBR / Bragg, band-pass, beam splitter, wave plate, and A / C / biaxial compensator starters. Today there is
    only a quarter-wave starter period (0022 `010` §1).
- **Tolerance and yield** · I M · E M
  - Monte-Carlo thickness and index errors, per-layer sensitivity, and robust synthesis (OptiLayer; Essential
    Macleod).

## 5. Analysis outputs and charts

- **Proper export** · I H · E L
  - A save dialog, PNG / SVG / PDF images, and CSV / Excel for every chart and for study results. Today PNG and CSV
    go to a fixed temp file (0022 `010` §7).
- **Mueller 4×4 grid chart** · I H · E L
  - All 16 normalised elements vs. λ or angle, measured over model, with per-element residuals. This is the usual
    display for Mueller data.
- **Chart tools already written** · I M · E L
  - Two cursors, peak finder, FWHM and log axes (`Charts/Readout.fs`, `ChartSettings.fs`).
  - Poincaré sphere and polarization ellipse (`PolarizationPlots.fs`). Source: 0022 `010` §6.
- **Overlay and comparison** · I M · E L
  - Several experiments, setups or fit runs on one chart; `plotComparison` exists (0022 `010` §1, §6).
- **2-D maps** · I M · E L
  - Heat-map, contour and polar (conoscopic / iso-contrast) views of two-variable scans (0027 `024` "2-D
    experiments"; LCD tools).
- **In-layer fields and absorption** · I M · E L
  - |E|², Poynting and per-layer absorptance vs. depth; `Analytics/FieldProfile.fs` exists.
  - Sources: 0022 `010` §4; Essential Macleod; Lumerical STACK; pyGTM.
- **Colour** · I L · E L
  - CIE XYZ / Lab / sRGB swatches of R / T spectra; `Analytics/Colorimetry.fs` exists (0022 `010` §4).

## 6. Materials and optical constants

- **Curated crystal catalogue** · I H · E M
  - Quartz, langasite / LGT, KTP, BBO, calcite, LiNbO₃, sapphire and others, with dispersive ε, gyration and
    citations. No free library carries gyration or magneto-optic data (0046 `018` §2 "Gaps"; the LGT:Nd tables in
    0044 `.manual`).
- **Kramers–Kronig check** · I M · E L
  - Flag non-causal tabulated or pulled n, k (0022 `010` §8).
- **More dispersion models** · I M · E M
  - KK-consistent B-spline, Cody–Lorentz and PSEMI-type oscillators (Johs & Hale; ellipsometry practice).
- **Further sources** · I L · E M
  - Materials Project tensors, as priors only; NIST FFAST as an X-ray cross-check (0046 `018` §2; 0050 §8).

## 7. Workflow, automation and reporting

- **Undo / redo** · I H · E M
  - Multi-level, over the table, the slots, kit edits and the editors.
  - 0026 §14 marked it "Must", 0050 §8 excluded it, and it is absent today.
- **Report generator** · I H · E M
  - One-click HTML / PDF of a study: setup schematic, data, fit overlays, values ± σ, correlations, the Planner
    report, provenance and citations (0022 `010` §7; DeltaPsi2 automatic reporting).
- **Hot folder** · I M · E L
  - New lab files dropped into a kit's `data/` are attached automatically through the kit's import profile and
    naming template.
- **Fit-run comparison** · I M · E L
  - A side-by-side table and diff of fit runs and item revisions (0022 `010` §7 "design history & diff").
- **Script API and headless runner** · I M · E M
  - An F# script and command-line surface over kits, studies and the Planner, driven by the UI's semantic messages
    (pyElli, refnx; DeltaPsi2 recipes).

## 8. Usability and onboarding

- **Example kits and a tutorial** · I H · E L
  - Worked kits under the start screen: AR coating, DBR, EUV Mo/Si, quartz gyration and the two 0050 reference
    kits.
  - A short getting-started tour (0022 `010` §9).
- **Russian UI** · I H · E M
  - Every caption goes through `Localization.fs` / `strings.json`, with the startup completeness check.
  - 0026 §3 marked it "Must". The loader exists, but the captions are hard-coded English.
- **Context help** · I H · E M
  - F1 / right-click help on the selected object, tooltips, a hotkey list and a glossary (0026 §13; 0022 `010` §9).
- **Drag and drop** · I M · E L
  - Library entries onto the table, slots and layers. The pure drop function exists in `StackEditor.fs`
    (0022 `010` §1).
- **Preferences window** · I M · E L
  - Units, default ranges, sweep points, precision, solver defaults and theme. The data exists in
    `UserEnvironment.fs` (0022 `010` §8).
- **Physical sanity warnings** · I M · E L
  - Wire `Validation.fs`: thickness ≤ 0, n < 1 regions, out-of-model input (0022 `010` §8).
- **3-D system view** · I L · E M
  - `SystemView3D.fs` exists, unwired (0022 `010` §1).
- **Dockable panels and saved layouts** · I L · E M
  - From 0022 `010` §8. Low while Avalonia's future is open.

## 9. Collaboration

- **Review mode** · I M · E M
  - Comment threads on experiments, studies and fit runs, a review status, and a per-file diff of a returned kit
    against the one sent (0046 `018` §1).
- **Merge of divergent library entries** · I L · E M
  - A field-level merge with preview, instead of a flagged fork (0050 §8).
- **Shared or cloud library** · I L · E H
  - Only on a concrete multi-site need (0046 `018` §1; 0050 §W).

## 10. Performance and platform

- **Parallel solves** · I H · E L
  - Jacobian columns, multi-start members, scan points and Planner candidates computed in parallel.
  - Only `Analytics/Variables.fs` is parallel today, and the AT-2 fit alone takes about 16 minutes (0050 `003`).
- **WinForms port** · I L · E H
  - Only if Avalonia is dropped (`AGENTS.md`; 0050 §W).

## Sources

**Internal:**
- `specs/0022/.manual/010`;
- `specs/0026/.feature-list-md`;
- `specs/0027/.manual/024`, `033`;
- `specs/0033/.manual/010`;
- `specs/0042/.manual/004`;
- `specs/0044/.manual/002`, `015`, `017`, `020`;
- `specs/0046/.manual/018`;
- `specs/0050/.manual/001`, `003`, `specs/0050/.spec-md`.

**Web** (this round, in addition to those already cited in 0022 `010` and 0046 `018`):
- Woollam global fit (grid seeding before regression): https://image-ppubs.uspto.gov/dirsearch-public/print/downloadPdf/7239391
- DeltaPsi2 (EMA, graded constants, multi-guess, recipes, automatic reporting): https://www.horiba.com/scientific/products/ellipsometers/software/deltapsi-2/?L=118
- Essential Macleod (field distribution, absorptance, colour, tolerancing): https://thinfilmcenter.com/core.php
- OptiLayer robust synthesis: https://www.laserfocusworld.com/optics/article/16551524/optical-design-advanced-thin-film-software-techniques-improve-design-to-fabrication-workflow
- Optiland needle synthesis: https://optiland.readthedocs.io/en/latest/examples/Tutorial_5c_Thin_Film_Optimization_and_Needle_Synthesis.html
- pyElli (2×2 / 4×4 solvers, EMAs, refractiveindex.info materials): https://pypi.org/project/pyElli/0.15.1 · https://joss.theoj.org/papers/10.21105/joss.08503.pdf
- PyLlama (cholesteric media): https://pyllama.readthedocs.io · pyGTM (layer-resolved absorption): https://pygtm.readthedocs.io
- Lumerical STACK (anisotropic tensors, field output): https://optics.ansys.com/hc/en-us/articles/5218682727827-2022-R1-2-Release-Notes
- refnx (MCMC, co-refinement): https://refnx.readthedocs.io/ · FIM vs. MCMC: https://arxiv.org/pdf/2103.08973
- KK-consistent B-spline and knot choice: https://proceedings.spiedigitallibrary.org/conference-proceedings-of-spie/10330/103300B/B-spline-parameterization-of-the-dielectric-function-and-information-criteria/10.1117/12.2270249.full
- Lu–Chipman decomposition tools: https://pages.nist.gov/pySCATMECH/code.html · https://pypi.org/project/pymueller/
- Polarimeter condition number and eigenvalue calibration: https://pubmed.ncbi.nlm.nih.gov/29877418/
- Iso-contrast and compensation films (Berreman 4×4 in LCD design): https://lcd.creol.ucf.edu/Publications/2009/JDT%20Jiao%20Biaxial.pdf
