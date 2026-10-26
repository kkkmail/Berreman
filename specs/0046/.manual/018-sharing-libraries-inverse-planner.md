# 018 — Sharing problems, online material libraries, and the inverse-problem planner (research report)

## Task setup

Three questions came out of the discussion with users.

1. **Sharing.** User A builds a direct problem, or an inverse problem with measured data, and wants
   user B to review, comment on, or continue it. A must export the experiment collection and B must
   import it. Open points:
   - The model depends on A's materials and samples. If A does not include them, B cannot load the
     model. If A does include them, B may already have materials or samples with the same names.
   - A cloud store would avoid the problem, but it brings accounts, internet access, admins and
     permissions. That is overkill for a small app with a few users.
2. **Online material libraries.** Are there FREE online libraries of optical constants? If yes, the
   app should search them, pull data, and add or update materials. If a library is paid, the user logs
   in there directly and pastes the data into the app (already possible).
3. **The "oracle" for the inverse problem.** The existing tests prove recovery for chosen cuts and
   instruments under zero error. A real user has only some sample cuts, some prior knowledge, and some
   instruments and supporting elements (LP, CP, LP+CP, detectors). The system should say:
   - the best that can be recovered with that kit;
   - which experiments to run.

   The user then measures and the inverse problem runs. Each combination of cuts, knowledge,
   detectors and experiments is a different inverse problem. Combinations should be precomputed where
   possible, because equipment rarely changes, so nothing heavy (the "LLM cycle") should run every
   time. The name "oracle" does not fit and needs a better term.

**What the app already has** (relevant facts from the code):
- **Stable identity.** Materials and samples have a GUID plus a version number (`MaterialVersionId`,
  `SampleVersionId`). Editing an entry that an experiment uses creates a new version (`VersionsInUse`).
  Seeded entries are protected built-ins.
- **Experiments.** An experiment freezes its setup: the element kinds and their Library bindings by
  id. Its measured-data file is only a path (`DataFilePath`).
- **Project file.** `.ocproj` is one self-contained JSON file.
- **Import.** `Storage/MaterialImport.fs` already imports refractiveindex.info YAML and CSV.

## 1. Sharing a problem without a cloud

**The key point: identity is not the name.** Every material and sample already has a GUID and a
version. Two entries called "Si" are only a display collision unless they share an id. Import should
decide by identity and content, never by name.

**Recommendation: a self-contained exchange file** (working name `.ocbundle`, a zip). It contains:
- **Manifest:** schema version, app version, author, date, and a SHA-256 hash of every embedded
  entry, computed over canonical JSON.
- **The experiment collection:** its scene and experiments.
- **The dependency closure:** every sample version referenced, plus every material version those
  samples reference. Built-ins are embedded too, because B may run a different app version whose
  built-in data differs.
- **The measured-data files themselves:** today an experiment stores only a path, which breaks on
  another machine.
- **Provenance per entry:** built-in, user-made, imported from a bundle, or pulled from a library
  (source, record, source version, licence, citation).
- **Review notes:** B's comments attached to experiments, so a review round-trips as a new bundle.

**What happens to each incoming entry on import:**

| Incoming vs B's library | Meaning | Default action |
|---|---|---|
| Same id, same version, same hash | Identical | Reuse silently |
| Same id, version B does not have | B has an older or newer history of the same entry | Add it as that entry's version. Do not move B's "latest" unless B asks |
| Same id and version, different hash | Divergence (cannot happen if versions are immutable) | Import as a fork with a new id, flagged |
| Different id, same name | Name collision only | Import side by side, labelled "Si — from A". Offer an explicit "use my Si instead" re-binding, with a diff preview |
| Not present | New | Add |

Show this as an import dialog with one row per entry, its status and the default action. Never
overwrite silently. Versions must be immutable once saved or exported. That rule is what makes the
table decidable, and it lets A see exactly what B changed.

**Precedents.**
- **Zemax archive (`.zar`):** a "pack-and-go" single file holding the lens file plus the material
  and coating catalogues it needs, meant for sending to another user. It shows the pitfall: catalogues
  are matched by file name, and there is no way to merge or rename on a name collision while loading.
- **KiCad 9:** moved to embedding referenced files (fonts, 3-D models, datasheets) in the design so
  that projects move between machines intact.

Both confirm the approach: always embed, and resolve by identity, not by name.

**A middle ground instead of a cloud:** a *shared library folder*. This is an extra library source on
a network drive, OneDrive, Dropbox or a Git repository.
- **Permissions:** the folder's own file permissions; no server, accounts or admin.
- **Conflicts:** versions are immutable files that are never edited in place, so the only possible
  conflict is two people adding the same name, which is harmless because the ids differ.

A real cloud store with accounts and permissions should wait until there is a concrete multi-site
need.

## 2. Free online optical-constant libraries — they exist

| Source | Content | Licence | Access | Use in the app |
|---|---|---|---|---|
| **refractiveindex.info** (M. Polyanskiy) | Large compilation of n, k (dispersion formulas or tables) from papers and datasheets, many thousands of records | **CC0** (public domain) | GitHub repo `polyanskiy/refractiveindex.info-database` plus dated zip snapshots; YAML; cite *Sci. Data* 11, 94 (2024) | **Primary source.** Our importer already reads its YAML |
| **PTB OCDB** (German metrology institute) | Measured X-ray, EUV and VUV optical constants (about 1–220 nm) **with uncertainties** | Data **CC BY 4.0** (the Python package is GPLv3, code only) | www.ocdb.ptb.de; `pip install ocdb` bundles the data | EUV lithography materials. The uncertainties feed the planner (§3) |
| **CXRO / LBNL Henke tables** | Atomic scattering factors f₁, f₂ for Z = 1–92, 10–30 000 eV (f₁ not given below 29 eV); compound n from composition and density | No licence stated, only an LBNL "notice to users"; check before redistributing | Per-element text files (`sf/<el>.nff`) and one tar.gz | Compute EUV n, k for any compound on the fly; 13.5 nm = 92 eV lies inside the range. Semi-empirical and poor near absorption edges |
| **NIST FFAST** (SRD 66) | f₁, f₂ for Z ≤ 92, X-ray range | Public access; see NIST's licence page | Web tables | X-ray cross-check of Henke |
| **Glass makers' catalogues** (Schott, Ohara, Hoya, CDGM) | Sellmeier and other formulas per glass, in AGF text | Free download from the vendor; redistribution terms vary; coefficients change between revisions | Vendor sites | Let the user import a downloaded AGF; do not bundle it |
| **Materials Project** | **Computed** (DFT) dielectric tensors (about 1 000 compounds, static) and computed absorption spectra | CC BY 4.0; free account and API key; large downloads by request | `mp-api` REST client | Only a *starting guess* (prior) for unknown anisotropic crystals; not reference data |
| Filmetrics / KLA database | n, k for thin-film materials | Some files open, some proprietary and encrypted; "no guarantee" | Web lookup | Manual copy and paste only |
| NASA Ames OCdb | Ices and organics (astrophysics) | — | Web | Not relevant |

**Paywalled sources** (Palik handbook, Landolt–Börnstein, vendor ellipsometry libraries) stay on the
manual path described in the task.

**Gaps:**
- No free library carries **gyration (optical activity)** or **magneto-optic** tensors. Those stay
  user-entered from papers.
- Anisotropic coverage is limited. The documentation I read does not say how refractiveindex.info
  encodes uniaxial and biaxial crystals (probably one record per axis), so mapping its records to our
  tensors must be checked on the first real import.

**Recommendation: a `MaterialSourceProxy`** (search, fetch).
- **Adapters, in order:**
  1. refractiveindex.info, with an offline snapshot and an optional "update snapshot";
  2. PTB OCDB, for EUV;
  3. a Henke-based compound calculator, for EUV and X-ray from a formula plus density.
- **Pulling:** each pulled record becomes a user material version carrying provenance (source,
  record path, snapshot date or commit, licence, citation).
- **Updating:** "update" makes a NEW version when the source changes; it never mutates a version in
  place, which is the same rule as §1.
- **Licences:** CC0 needs nothing; CC BY needs the attribution shown in the app and carried inside
  bundles.

## 3. The "oracle" → a **Measurement Planner**

**Established terms.** The field calls this *optimal experimental design* (OED) and *identifiability
analysis*. I suggest two names:
- **Measurement Planner** for the tool;
- **Recoverability report** plus **Measurement plan** for what it produces.

**Inputs:**
- **Lab profile** (entered once): sources and λ ranges; polarizers, compensators and LP+CP
  combinations; detectors (intensity, ellipsometer Ψ/Δ, which Mueller rows); angle ranges; noise
  levels.
- **Sample inventory:** cuts (crystal axes relative to the surface), thicknesses, substrates.
- **Prior knowledge:** crystal class or point group, known components, dispersion-model choices.
- **Targets:** the parameters the user cares about.

**Layer 1 — structural (symmetry and physics).** Exact, cheap, and fully tabulatable:
- **Unknowns.** The point group fixes which tensor components exist: isotropic 1, uniaxial 2,
  biaxial 3 plus orientation for monoclinic and triclinic. Optical activity exists in 18 of the 21
  non-centrosymmetric groups (quartz: g₁₁, g₃₃).
- **What each measurement sees.** A c-cut at normal incidence sees only n_o (and g₃₃); n_e needs
  oblique incidence or an a-cut.
- **Activity in birefringent directions.** Optical activity there is 10⁻²–10⁻⁵ of the birefringence.
  It needs full-Mueller (two-modulator) or HAUP-class measurements: Arteaga et al. measured quartz
  g₁₁ and g₃₃ this way.
- **Low-symmetry crystals** (β-Ga₂O₃, CdWO₄, Lu₂SiO₅) need several cuts × azimuths × angles, all
  fitted together.
- **Detectors.** Intensity-only detectors lose phase.

This layer answers outright "this kit can never recover X" and "you need an off-axis cut".

**Layer 2 — practical (numerical).** At a nominal parameter point (from the prior), with the
instruments' noise model:
- **Fisher information.** Compute the Jacobian of every candidate experiment with respect to the
  unknowns, using the Berreman solver we already have, and form the Fisher information matrix (FIM).
- **Rank** shows how many parameter combinations are identifiable.
- **Smallest singular vectors** show which combinations are confounded (classically thickness ↔ n).
- **Cramér–Rao bounds** give the best achievable uncertainty per parameter: "the best we can
  recover".
- **Plan selection.** Greedily add the candidate experiment (cut × angle range × λ range × polarizer
  and analyzer settings × detector) that most improves the D-criterion (det FIM), or the A-criterion
  restricted to the targets. Polarimeter literature adds the instrument-matrix condition number for
  choosing polarizer and compensator states.
- **Caveat.** The FIM is a local, linearised measure. After the real measurement, check the fit with
  profile likelihood (Raue et al. 2009). Ellipsometry software does this retrospectively (correlation
  matrices, "uniqueness" fits); none I found plans prospectively over a lab's inventory. That is the
  differentiator.
- **Later upgrade:** Bayesian expected information gain with sequential design. "Deep Adaptive
  Design" is precisely the precompute-then-deploy idea.
- **Precedents:** FIM-optimised λ and angle in EUV scatterometry (arXiv 2504.12133) and in neutron
  reflectometry (arXiv 2103.08973, 2108.05605).

**No LLM is needed.** This is deterministic linear algebra over the solver.
- **Cost.** One Jacobian is (#unknowns + 1) forward solves per point. Hundreds to a thousand
  candidates means seconds to minutes.
- **Cache key:** hash of (lab profile, inventory, prior and material model, targets, nominal point).
  Candidate Jacobians are stored per (cut, experiment), so adding one cut or one instrument computes
  only the new candidates. A recompute happens only when the inventory changes.
- **Shipped playbooks.** Layer 1 and coarse layer-2 results for crystal class × common kits (for
  example "uniaxial + LP + rotating analyzer + intensity → recoverable: n_o, n_e only with an
  oblique a-cut") can be precomputed offline and shipped.

**Workflow:**
1. Enter the lab profile (once).
2. Enter the inventory and prior.
3. The planner produces the recoverability report and the ranked plan.
4. The user measures.
5. The existing inverse fit runs.
6. A post-fit identifiability check runs.
7. If needed, the planner proposes the next experiment.

The planner's candidates are exactly the app's `Experiment` values (what varies, polarizers on the
path, detector-decided quantities). A plan can therefore be emitted as an experiment collection, the
same object §1 shares.

## Suggested next steps

1. Bundle format plus import dialog: identity by GUID + version + hash, embedding the closure and
   the data files. Make versions immutable once exported.
2. `MaterialSourceProxy` with the refractiveindex.info adapter (offline snapshot, provenance), then
   PTB OCDB and the Henke compound calculator.
3. Measurement Planner, layer 1 (symmetry table) and the layer-2 recoverability report for the current
   experiment kinds; then plan generation and caching.

**Open questions for the operator** (short):
- the bundle extension;
- whether a shared-folder library is wanted;
- which planner target comes first: isotropic films, uniaxial crystals, or gyrotropic crystals.

## Sources

- refractiveindex.info about / licence (CC0): https://refractiveindex.info/about
- refractiveindex.info database repo: https://github.com/polyanskiy/refractiveindex.info-database
- Polyanskiy, *Refractiveindex.info database of optical constants*, Sci. Data 11, 94 (2024): https://doi.org/10.1038/s41597-023-02898-2
- indicio (offline refractiveindex.info package): https://pypi.org/project/indicio/
- PTB OCDB docs: https://ocdb-data.readthedocs.io/latest/ · site: https://www.ocdb.ptb.de/
- CXRO X-ray interactions with matter: https://henke.lbl.gov/optical_constants/ · scattering-factor files: https://henke.lbl.gov/optical_constants/asf.html
- NIST FFAST (SRD 66): https://physics.nist.gov/PhysRefData/FFast/Text2000/sec02.html · https://catalog.data.gov/dataset/nist-x-ray-form-factor-attenuation-and-scattering-tables-srd-66-c5f2b
- Materials Project dielectric methodology: https://docs.materialsproject.org/methodology/materials-methodology/dielectricity · terms: https://legacy.materialsproject.org/terms · API: https://docs.materialsproject.org/downloading-data/using-the-api/getting-started
- High-throughput dielectric screening (≈1 000 compounds): https://appliedenergyscience.lbl.gov/publications/high-throughput-screening-inorganic
- Glass catalogues / AGF: https://microsoft.github.io/OpticSim.jl/stable/glasscat/ · https://github.com/davidbrowns/GlassPlotter · https://www.hoya-opticalworld.com/english
- Filmetrics / KLA refractive-index database: https://www.kla.com/products/instruments/refractive-index-database
- NASA Ames OCdb: https://ocdb.smce.nasa.gov/
- Zemax archive (.zar): https://community.zemax.com/got-a-question-7/how-do-i-create-a-zemax-archive-zar-file-956 · collisions: https://community.zemax.com/got-a-question-7/what-settings-should-i-choose-when-opening-a-zar-file-639
- KiCad 9 embedded files: https://www.cnx-software.com/2025/02/21/kicad-9-release/
- Multi-cut generalized ellipsometry, monoclinic β-Ga₂O₃: https://arxiv.org/pdf/1512.08590 · CdWO₄: https://arxiv.org/pdf/1701.00813 · Lu₂SiO₅: https://arxiv.org/pdf/1908.09777
- Arteaga, Canillas, Jellison — quartz gyration by two-modulator generalized ellipsometry: https://diposit.ub.edu/dspace/handle/2445/24297 · thesis: https://www.mmpolarimetry.com/Thesis_OriolArteaga.pdf
- HAUP and optical activity in crystals (review / langasite / KTP): https://www.intechopen.com/chapters/84981 · https://czasopisma.pan.pl//Content/133153/PDF/OPELRE_2024_32_4_M_Shopa.pdf · https://pub.pg.edu.pl/pl/download/142545-dual-wavelength-polarimeter-application-in-investigations-of-the-optical-activity-of-a-langasite-cry
- Gyrotropic point groups (18 of 21): https://arxiv.org/pdf/2003.10872
- OED criteria (A / D / E): https://arxiv.org/pdf/0802.4381
- FIM design in neutron reflectometry: https://arxiv.org/pdf/2103.08973 · https://arxiv.org/pdf/2108.05605
- FIM-optimised EUV scatterometry: https://arxiv.org/pdf/2504.12133
- Profile likelihood identifiability (Raue et al. 2009): https://pubmed.ncbi.nlm.nih.gov/19505944 · https://www.arxiv.org/pdf/1202.4605
- Polarimeter condition number: https://human-competitive.org/sites/default/files/letnes-paper-b.pdf · https://spiedigitallibrary.org/journals/Optical-Engineering/volume-58/issue-8/082410/On-the-equivalence-of-optimization-metrics-in-Stokes-polarimetry/10.1117/1.OE.58.8.082410.pdf
- Ellipsometry correlation / uniqueness practice: https://www.horiba.com/id/scientific/products/ellipsometers/ellipsometry-academy/faqs/data-analysis · https://arxiv.org/pdf/2203.13793
- Bayesian / sequential experimental design: https://arxiv.org/pdf/2202.07472 · https://ar5iv.arxiv.org/html/2003.07315
