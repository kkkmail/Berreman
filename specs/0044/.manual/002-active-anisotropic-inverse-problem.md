# Spec 0044 (preliminary) — Inverse determination of the optical properties of a transparent, optically active, anisotropic, homogeneous material (quartz), and relocating the 0042 Mueller data into the repo

Status: **preliminary, prose only.** Nothing in the repository has been changed by this document.
It is written to be reviewed and answered the way `specs/0042/.manual/002-…md` was answered by
`003-comments.txt`; the open review questions are collected in §9.

Task source: `specs/0044/.manual/001-task.txt`. Its four items map onto the parts below.

| Task item | Part |
|---|---|
| 1. Web research — what experiments, what inverse method | §2 |
| 2. Use the library's quartz at a single wavelength for the forward data | §3 |
| 3. Work out the inverse setup + write the test(s), 0042-style | §4–§6 |
| 4. Repoint `MuellerReconstructionTests` at `Data/MuellerMatrix/data.zip`; advise on gaps | §7 |

Repository facts this spec was built from (all verified by reading the code, not assumed):

- Solver forward surface: `OpticalSystemSolver.muellerMatrixT() / .muellerMatrixR()`
  (`Berreman/Solvers.fs:315-316`), `SolverParameters.numberOfReflections` (`Solvers.fs:15`).
- Material construction: `OpticalProperties.type_3_4_6_Crystal` (`OpticalProperties/Active.fs:113`),
  `OpticalProperties.rotateX/rotateY/rotateZ` (`Berreman/MaterialProperties.fs:204-207`),
  `OpticalSystem.plateSystem` (`OpticalProperties/Standard.fs:188`),
  `IncidentLightInfo.createInclined` (`Standard.fs:42`).
- Optimizer: `AlglibAdapter.optimize` behind `OptimizationInterface.OptimizationRequest`
  (`Optimization/OptimizationInterface.fs:59`), `LevenbergMarquardt → minlm`.
- Fit diagnostics: `FitQuality.FitReport` — χ², reduced χ², standard errors, 95 % CI, covariance,
  correlation, per-sample residuals (`Optimization/FitQuality.fs:29`).
- 0042 output that this spec reuses wholesale: `OpticalConstructor.Domain/MuellerReconstruction.fs`
  (`Retardance`, `retarderMueller`, `kron4`, `vecColumnMajor`, `frobeniusDiff`, `MuellerSolverProxy`,
  `MuellerDataProxy`) and `OpticalConstructor.Storage/MuellerDataStore.fs`
  (`parseMuellerCsv`, `createFileBacked`).

---

## 1. Purpose and scope

Two deliverables that share one test project:

1. **A synthetic inverse-problem exercise.** Take quartz — a transparent, optically active,
   anisotropic, homogeneous, uniaxial class-32 crystal — at **one** wavelength; generate the forward
   "measured" data with this repository's own Berreman solver; then recover the material's optical
   constants from that data and assert the round trip. Because the ground truth is known exactly, the
   test measures the *inverse machinery*, not the physics.
2. **Repointing the 0042 measured-data tests** at the committed `Berreman/Data/MuellerMatrix/data.zip`
   so they stop depending on a sibling `optics-mueller` checkout and stop skipping.

Explicitly **out of scope**: fitting real measured quartz data; dispersion (a single wavelength is
mandated by the task); absorbing or depolarizing samples; any UI.

The 0042 discipline carries over unchanged and is the reason the design below looks the way it does:
**many tiny pure functions**, **IO and the optimizer behind `*Proxy` records**, **elevated primitives
everywhere**, **`Result` at every boundary**, and **the numeric core stays pure**.

---

## 2. Part 1 — What experiments are needed, and what method inverts them (web research)

### 2.1 The unknowns

For a transparent, homogeneous, optically active, **uniaxial** crystal of class 32 (quartz's class)
the complete optical description at one wavelength is:

| Symbol | Meaning | Quartz @ 632.8 nm |
|---|---|---|
| `n_o` | ordinary index (ε₁₁ = ε₂₂ = n_o²) | ≈ 1.5426 |
| `n_e` | extraordinary index (ε₃₃ = n_e²) | ≈ 1.5517 |
| `g₁₁` | gyration tensor component ⊥ optic axis | ≈ +5.9 × 10⁻⁵ (repo) / 4.8 ± 0.5 × 10⁻⁵ (Arteaga 2009) |
| `g₃₃` | gyration tensor component ∥ optic axis | ≈ −10.1 × 10⁻⁵ (repo) / 10.06 ± 0.07 × 10⁻⁵ (Arteaga 2009) |
| `d` | sample thickness | a lab-measured quantity, see §2.4 |
| orientation | optic-axis direction relative to the surface normal, plus the azimuth about the normal | fixed by the cut, refined as nuisance parameters |

Class 32 forces the gyration tensor to `diag(g₁₁, g₁₁, g₃₃)`; `Rho.type_3_4_6_Crystal`
(`Active.fs:47`) is exactly that assembly, and the engine stores it as an **imaginary** matrix
(`Rho.fromIm`, `MaterialProperties.fs:139`). Absorption is zero (transparent), so there is no
dichroism of any kind — a fact the inverse method exploits (§2.5).

### 2.2 Why a single experiment cannot work — the identifiability physics

For propagation at an internal angle `θ` to the optic axis, the effective gyration seen by the wave is
the projection of the tensor on the propagation direction,

```
G(θ) = g₁₁ · sin²θ + g₃₃ · cos²θ
```

while the linear birefringence available to that same wave grows from zero at `θ = 0` to its full
`n_e − n_o` at `θ = 90°`. This single formula dictates the whole experiment design:

- **`θ = 0` (light along the optic axis, i.e. a z-cut plate at normal incidence).** No linear
  birefringence at all; the sample is a **pure circular retarder** and the transmitted Mueller matrix
  is a pure rotator. `G = g₃₃` is read off directly and with excellent conditioning. This is the
  classical quartz optical-rotation measurement.
- **`θ = 90°` (light perpendicular to the optic axis, i.e. an x- or y-cut plate at normal
  incidence).** `G = g₁₁`, but now the linear birefringence is maximal and **buries** the optical
  activity — the hard, historically contentious measurement. This is precisely the regime the
  literature calls out (HAUP, and Arteaga/Canillas/Jellison's transmission 2-MGE work).
- **Intermediate `θ`** mixes the two and is what actually separates `g₁₁` from `g₃₃` in a joint fit.

**A consequence that constrains the design and is easy to miss:** you cannot reach large `θ` merely by
tilting a z-cut plate. Snell's law caps the internal angle at `arcsin(1/n_o) ≈ 40.4°` for quartz, so
even at grazing incidence a z-cut sample gives `sin²θ ≤ 0.42` — `g₁₁` never carries more than 42 % of
the projection weight. That is exactly why Arteaga et al. report `g₃₃` to 0.7 % but `g₁₁` only to
~10 % from a single z-cut crystal. **The experiment set must therefore include at least one cut whose
optic axis is not along the normal** (an x-cut, or a 45°-cut); tilting alone is not enough.

The remaining unknowns separate as follows:

- `n_e − n_o` and `d` enter the transmitted linear retardance **only as the product** `(n_e − n_o)·d`.
  They are separated either by (a) knowing `d` independently, which is what a lab does, or (b) adding
  oblique-incidence data, where the internal path length and the effective index vary differently with
  angle, or (c) adding **reflection** Mueller data, which depends on the indices but not on `d`.
- The absolute level of `n_o` comes from the Fresnel amplitudes — i.e. from the angle-of-incidence
  dependence of the reflected/transmitted intensity, not from the retardance.
- With a thick plate the linear retardance wraps many times (a 1 mm x-cut quartz plate is ≈ 14.3
  waves at 632.8 nm), which makes the `(n_e − n_o)·d` inversion **order-ambiguous**. Either keep the
  linear-retardance-bearing sample thin enough that the retardance stays below one wave (≈ 20 µm gives
  ≈ 0.29 wave ≈ 103°), or treat `d` as known. §2.4 does both.

### 2.3 What the literature actually measures with

Four families of instrument recur; all four produce data this repository can synthesize:

1. **HAUP — High-Accuracy Universal Polarimeter** (Kobayashi & Uesu, 1983; Kobayashi et al., 1986).
   Crossed-polarizer transmission with fine, independent scanning of the polarizer and analyzer
   azimuths, fitting the transmitted intensity surface `I(θ_P, θ_A)`. Simultaneously yields linear
   birefringence and the gyration component along the propagation direction. Its published Achilles
   heel is the **parasitic ellipticity of the prisms**, which must be calibrated away — structurally
   the same problem as spec 0042's Stage-1 AIR calibration, and the reason 0042's design puts the
   instrument model in front of the sample model.
2. **Generalized / Mueller-matrix ellipsometry** — in particular the **two-modulator generalized
   ellipsometer (2-MGE)** in *transmission at oblique incidence*, which is the technique that produced
   the modern quartz `g₁₁`, `g₃₃` values (Arteaga, Canillas & Jellison, *Appl. Opt.* **48**, 5307
   (2009)). It measures the full normalized Mueller matrix as a function of incidence angle and sample
   azimuth. Standard practice is to combine **multiple angles of incidence and multiple sample
   azimuths** and regress them all simultaneously against a 4×4-matrix (Berreman/Schubert) forward
   model with the crystal orientation carried as Euler angles.
3. **Rotating-analyzer / rotating-element polarimetry** — the cheap version, and exactly the
   experiment spec 0042 already reproduces (`I = a_effᵀ · M · s_eff`). It reaches the full 16-element
   Mueller matrix only if the source and analyzer between them span S₃, i.e. at least one elliptical
   element is present. 0042's rank-16 assertion is the observability check for this.
4. **Conoscopy / divergent-beam imaging**, which sweeps the internal angle `θ` in one shot. It is the
   natural way to sample `G(θ)` densely, and it is a good future candidate here, but it needs a beam
   model this test does not require.

**Recommendation for this spec: model family 2** (transmission and reflection Mueller matrices versus
incidence angle and sample azimuth, at several cuts). It is the mainstream method for exactly this
material class, it maps one-to-one onto `muellerMatrixT()` / `muellerMatrixR()`, and it keeps the
instrument model out of the picture so the test measures the inverse problem rather than a simulated
polarimeter. Family 3 is a documented follow-up (§6, "future candidates") that would let the 0042
intensity-level machinery be reused verbatim.

### 2.4 The recommended synthetic experiment set

All at **λ = 632.8 nm** (He-Ne; the wavelength the quartz literature is densest at). "Azimuth" means
rotation of the sample about the surface normal; "cut" means the orientation of the optic axis
relative to that normal.

| # | Cut (optic axis) | Thickness | Incidence angles | Azimuths | Observable | Primarily constrains |
|---|---|---|---|---|---|---|
| C1 | z-cut (axis ∥ normal) | 1.0 mm | 0° | 0° | `M_T` | **`g₃₃`** — pure rotator, ≈ 18.6° of rotation |
| C2 | z-cut | 1.0 mm | 10…70° step 10° | 0°, 45° | `M_T`, `M_R` | `n_o`, `n_e`; `g₁₁` weakly (`sin²θ ≤ 0.42`) |
| C3 | x-cut (axis ⊥ normal, in-plane) | 20 µm | 0° | 0°, 22.5°, 45°, 67.5° | `M_T` | `(n_e − n_o)·d`, axis azimuth, **`g₁₁`** |
| C4 | x-cut | 20 µm | 0…60° step 15° | 0°, 45° | `M_T`, `M_R` | separates `n_o`, `n_e`, `d`; refines `g₁₁` |

Why each row is present:

- **C1 is the anchor.** It is the only configuration in which optical activity is not competing with
  linear birefringence, and it fixes `g₃₃` almost independently of everything else.
- **C2 supplies the Fresnel/angle information** that pins the absolute indices, and is the
  configuration the 2-MGE literature uses.
- **C3 is the row that makes `g₁₁` identifiable at all** (§2.2: tilting a z-cut cannot get there).
  Its thickness is deliberately sub-wave in retardance so `(n_e − n_o)·d` has no order ambiguity.
- **C4 breaks the `Δn·d` product** and gives `g₁₁` a second, independent look.
- The **reflection** matrices in C2/C4 are what make `d` and the indices separable without assuming a
  known thickness; if review prefers to declare `d` known (the honest lab situation), C2/C4 can drop
  to transmission only. **Recommendation: keep reflection in, and fit `d` — it makes the test a
  stronger statement about identifiability.**

Ablation (this is an assertion, not a remark — see §6): **fitting from C1 alone must fail to
determine `g₁₁`.** The Jacobian column for `g₁₁` collapses and its reported standard error explodes.
That negative result is the executable form of the answer to task item 1.

### 2.5 The inverse method

**Two stages, mirroring the shape spec 0042 settled on** (a closed-form/linear stage that produces
trustworthy starting values, then the authoritative stage):

**Stage A — analytic inversion of each measured Mueller matrix (closed form, no optimizer).**
For a homogeneous non-depolarizing sample, the normalized Mueller–Jones matrix inverts in closed form
to the six elementary anisotropy coefficients — linear birefringence `LB` and `LB′` (the 0°/45° pair),
circular birefringence `CB`, and the three dichroisms `LD`, `LD′`, `CD` (Arteaga & Canillas,
*Opt. Lett.* **35**, 559 (2010), with the erratum *Opt. Lett.* **35**, 3525). The equivalent route is
the **differential/logarithmic decomposition** (Azzam 1978; Ortega-Quijano & Arce-Diego 2011;
Ossikovski 2011), where `L = ln M` is resolved into the elementary generators.

For a transparent sample the three dichroisms are structurally zero, which gives Stage A a **free
self-consistency check**: if the recovered `LD`, `LD′`, `CD` are not ≈ 0, either the forward model or
the inversion is wrong. What survives is:

- `CB` → the optical rotation → `G(θ)` and hence, per configuration, a direct estimate of `g₃₃` (C1)
  or of `g₁₁` (C3);
- `LB`, `LB′` → `(n_e − n_o)·d` and the in-plane axis azimuth.

Stage A is used for **starting values, sanity checks and diagnostics — not as the answer.** One
caveat that must be honored: a `Plate` substrate in this solver is summed **incoherently** over
multiple internal reflections (`getMuellerMatrix`, `Solvers.fs:286`, sums the Mueller matrices of the
emerging beams). A sum of Mueller–Jones matrices is in general *weakly depolarizing*, so the
Arteaga inversion — which assumes a Mueller–Jones matrix — is only approximate there. Either evaluate
Stage A on a single-pass configuration, or accept it as a starting-value generator. **Recommendation:
pin `SolverParameters` explicitly in the test rather than inheriting the default `numberOfReflections
= 3`, and record the chosen value in the test's doc comment.**

**Stage B — nonlinear least squares against the full Berreman forward model (authoritative).**
Fit the parameter vector against **all configurations simultaneously**, with the residual built from
the `m00`-normalized Mueller elements:

```
r_k = w · ( M_model[i,j] / M_model[0,0]  −  M_data[i,j] / M_data[0,0] ) / σ
```

Levenberg–Marquardt (`AlglibAdapter.optimize`, `LevenbergMarquardt → minlm`) is the correct and
already-available workhorse; this is the standard choice throughout the generalized-ellipsometry
literature. Nothing external is needed — the same conclusion 0042 reached for its linear stage.

**Parameter scaling is mandatory, not cosmetic.** The physical parameters span four orders of
magnitude (`n ≈ 1.5`, `g ≈ 1e-4`, `d ≈ 2e-5 m`), and `AlglibAdapter.runLm` calls
`alglib.minlmcreatev(n, m, x0, 1.0e-6, &state)` — a **fixed absolute** numerical-differentiation step
of `1e-6`. Applied to a 20 µm thickness that is a 5 % perturbation, which will produce a meaningless
Jacobian. The fit vector must therefore be **dimensionless and O(1)** (each parameter carried as a
scaled offset from its start value, or in units chosen so it lands near 1), with the mapping back to
physical units done inside the residual closure. This is the single most likely cause of a
mysteriously non-converging first implementation and should be built in from the start, not debugged
into existence.

**Identifiability reporting is part of the deliverable, not a nicety.** `FitQuality.reportFrom`
already returns χ², reduced χ², per-parameter standard errors, 95 % confidence intervals, the
covariance matrix and the **parameter correlation matrix**; the residual Jacobian it computes also
yields the condition number. Report all of them, and assert on them (§6). This is the standard
ellipsometric answer to "is the inverse problem well posed" — the ill-posedness of ellipsometric
inversion, severe parameter correlation and convergence to non-physical local minima are the three
failure modes the literature warns about, and each has a direct diagnostic here.

**Global search is deliberately out of scope.** With synthetic noiseless data and a physically
sensible start point, LM converges; a multistart/differential-evolution wrapper is a documented future
candidate (§6) for the day real data with an unknown retardance order appears.

### 2.6 Key references

- O. Arteaga, A. Canillas, G. E. Jellison Jr., "Determination of the components of the gyration tensor
  of quartz by oblique incidence transmission two-modulator generalized ellipsometry," *Appl. Opt.*
  **48**(28), 5307 (2009). <https://opg.optica.org/ao/abstract.cfm?uri=ao-48-28-5307>
- O. Arteaga, A. Canillas, "Analytic inversion of the Mueller–Jones polarization matrices for
  homogeneous media," *Opt. Lett.* **35**(4), 559 (2010);
  erratum *Opt. Lett.* **35**(20), 3525 (2010).
  <https://opg.optica.org/ol/abstract.cfm?uri=ol-35-4-559>
- O. Arteaga, "Mueller matrix polarimetry of anisotropic chiral media" (PhD thesis, U. Barcelona) —
  the long-form treatment of separating optical activity from linear birefringence.
  <https://www.tdx.cat/bitstream/handle/10803/687323/OAB_PhD-THESIS.pdf>
- C. Sanchez, J. Etxebarria et al. / Kobayashi & Uesu, HAUP: "A new optical method and apparatus HAUP
  for measuring simultaneously optical activity and birefringence of crystals," *J. Appl. Cryst.*
  (1983) and the accuracy-improvement papers; the α-quartz application with multiple reflections is
  *JOSA B* **15**(3), 1147 (1998). <https://opg.optica.org/josab/abstract.cfm?uri=josab-15-3-1147>
- R. Ossikovski, "Differential matrix formalism for depolarizing anisotropic media," *Opt. Lett.*
  **36**(12), 2330 (2011). <https://opg.optica.org/ol/abstract.cfm?uri=ol-36-12-2330>
- N. Ortega-Quijano, J. L. Arce-Diego, "Mueller matrix differential decomposition," *Opt. Lett.*
  **36**, 1942 (2011).
- M. Schubert, "Polarization-dependent optical parameters of arbitrarily anisotropic homogeneous
  layered systems," *Phys. Rev. B* **53**, 4265 (1996) — generalized ellipsometry on the 4×4 formalism.
- G. Ghosh, "Dispersion-equation coefficients for the refractive index and birefringence of calcite
  and quartz crystals," *Opt. Commun.* **163**, 95 (1999) — the quartz Sellmeier used in §3.
- Carried over from spec 0042 §8: Chipman, *Polarized Light and Optical Systems* Ch. 7; Goldstein,
  *Appl. Opt.* **31**, 6676 (1992); Gil, *Appl. Opt.* **55**, 5449 (2016) (Cloude realizability);
  Lu & Chipman, *JOSA A* **13**, 1106 (1996).

---

## 3. Part 2 — Quartz in this repository: audit, and what has to be added

### 3.1 Finding: **there is no quartz material in the library.** This must be built.

The task says "use existing in the library quartz". A repository-wide search finds quartz in exactly
three places, and **none of them is a usable material**:

1. `BerremanTests/RhoWithDispValueTests.fs:45-52` — a **test fixture** holding
   `quartzG11 = 5.9e-5`, `quartzG33 = -10.1e-5` as a `UniaxialGyration<RhoValue>`. It lives in the
   test project, not in `OpticalProperties`, and it carries **only the gyration** — no permittivity.
2. `OpticalConstructor.Domain/MaterialComplexityEditor.fs:343` — the same two numbers in a **doc
   comment**, as the worked example for the class-32 gyration form.
3. `OpticalProperties/Active.fs:113` — `OpticalProperties.type_3_4_6_Crystal e11 e33 g11 g33`, the
   **builder** that would assemble quartz once someone supplies the four constants.

`OpticalProperties/Standard.fs` has only generic stand-ins (`uniaxialCrystal` = n 1.5/1.65/1.65);
`OpticalProperties/Dispersive.fs` has `Langasite` (also class 32, also optically active — the nearest
existing thing) and `Silicon`. **There is no quartz permittivity anywhere in the repository.**

**Recommendation:** add a first-class `Quartz` definition to the `OpticalProperties` project, as the
single source of truth, and have both the new inverse test and the existing
`RhoWithDispValueTests` fixture consume it instead of re-declaring the constants. This is a small,
well-scoped addition and it is the honest reading of "use existing in the library quartz" — the
constants exist, the material does not.

Placement: `Standard.fs` is fenced with "**!!! DO NOT CHANGE ANY VALUES HERE !!!**" and an
"add any custom values after this line" convention, and `Active.fs` is where class-32 assembly already
lives. **Recommendation: a new `OpticalProperties/Quartz.fs` compiled after `Active.fs`**, so nothing
existing is disturbed and the constants sit with their provenance. (Adding to the end of `Active.fs`
is an acceptable alternative — review call, §9 Q3.)

### 3.2 The proposed constants (λ = 632.8 nm, 24 °C, right-handed enantiomorph)

| Constant | Value | Provenance |
|---|---|---|
| `n_o` | 1.542606 | Ghosh 1999 Sellmeier for the ordinary ray, evaluated at 0.6328 µm |
| `n_e` | 1.551651 | Ghosh 1999 Sellmeier for the extraordinary ray, evaluated at 0.6328 µm |
| `Δn` | 0.009045 | derived |
| `g₁₁` | +5.9 × 10⁻⁵ | the repository's existing value (`RhoWithDispValueTests.fs:46`) |
| `g₃₃` | −10.1 × 10⁻⁵ | the repository's existing value (`RhoWithDispValueTests.fs:47`) |

Every one of these is elevated on the way in — `RefractionIndex`, `EpsValue.fromRefractionIndex`,
`RhoValue`, `WaveLength.nm` — and assembled through the existing
`OpticalProperties.type_3_4_6_Crystal`. No new 3×3 algebra is introduced.

Two notes for the reviewer, neither of which blocks anything:

- **The repository's `g₁₁` is at the high end of the literature.** Arteaga et al. (2009) report
  `g₃₃ = 10.06 ± 0.07 × 10⁻⁵` (which the repository's 10.1 matches almost exactly) but
  `g₁₁ = 4.8 ± 0.5 × 10⁻⁵` against the repository's 5.9, and a ratio `g₁₁/g₃₃ ≈ −0.486` against the
  repository's −0.584. Since this test is a **synthetic round trip**, the repository's values *are*
  the ground truth by definition and the discrepancy is irrelevant to pass/fail — but the new
  `Quartz` definition should carry a doc comment recording both, so nobody later mistakes the test for
  a literature validation.
- **Handedness is one overall sign.** `assembleRho` (`Active.fs:133`) applies `hand.sign` to every
  component, so the left-handed enantiomorph is the negation. The definition should expose the
  handedness explicitly rather than baking a sign into the constants.

### 3.3 A forward-model self-check that is worth asserting

Along the optic axis the specific rotation is `ρ = π·|g₃₃| / (λ·n̄)`. With the constants above:

```
ρ = π · 1.01e-4 / (632.8e-9 · 1.542606) = 325.0 rad/m = 18.62 °/mm
```

The measured optical rotatory power of α-quartz at 632.8 nm is **18.8 °/mm** (188 °/cm). The
repository's own constants reproduce the literature to **within 1 %**, which means the C1
configuration doubles as an end-to-end validation of the whole forward chain — material assembly,
`Rho.fromIm` sign convention, Berreman propagation, and `muellerMatrixT()` — against an external
number. **Assert it** (§6, T4). If it fails, the bug is in the forward model, and every inverse
result downstream is meaningless.

Companion magnitudes, useful when picking tolerances:

| Quantity | Value |
|---|---|
| C1 rotation (z-cut, 1 mm) | 18.62° |
| C3 linear retardance (x-cut, 20 µm) | 0.286 wave = 102.9° |
| Linear retardance of a 1 mm x-cut plate | 14.29 waves — the order ambiguity of §2.2 |
| Optical rotation over 20 µm along the axis | 0.372° |
| Max internal angle in quartz from air | 40.41° |

---

## 4. Part 3 — Where the code goes, and what already exists

### 4.1 Home

**`Berreman/BerremanTests`**, exactly as 0042 chose. It is the UI-less test project, and it already
carries explicit `ProjectReference`s to `OpticalProperties`, `Analytics`,
`OpticalConstructor.Domain`, `OpticalConstructor.Optimization` and `OpticalConstructor.Storage`
(`BerremanTests.fsproj:66-83`) — **every project this spec needs is already referenced; no new project
reference is required.**

The reusable pure functions go into a **new `OpticalConstructor.Domain/MuellerInverse.fs`**, next to
`MuellerReconstruction.fs`. One constraint drove this: `OpticalConstructor.Domain` references
`Berreman` and `Analytics` but **not** `OpticalProperties` directly (`Domain.fsproj:204-205`).
Therefore **`MuellerInverse` must stay material-agnostic** — it consumes
`Berreman.MaterialProperties.OpticalProperties` values and a caller-supplied system builder, never
`OpticalProperties.Active`. Quartz-specific construction lives in the `OpticalProperties` project
(§3.1) and the test wires the two together. This keeps the module reusable for the next material and
avoids adding a reference that only exists to reach one constant.

### 4.2 Reuse map (call these — do not reinvent)

| Need | Reuse | Location |
|---|---|---|
| Transmitted / reflected Mueller matrix | `OpticalSystemSolver.muellerMatrixT() / .muellerMatrixR()` | `Solvers.fs:315-316` |
| Multiple-reflection control | `SolverParameters.numberOfReflections` | `Solvers.fs:15` |
| Class-32 material assembly | `OpticalProperties.type_3_4_6_Crystal` | `Active.fs:113` |
| Crystal orientation | `OpticalProperties.rotateX / rotateY / rotateZ` | `MaterialProperties.fs:204-207` |
| Plate system | `OpticalSystem.plateSystem properties description thickness` | `Standard.fs:188` |
| Inclined incident light | `IncidentLightInfo.createInclined` | `Standard.fs:42` |
| Mueller type + `M*M`, `M*S` | `MuellerMatrix` | `Fields.fs:598` |
| Build/read a Mueller by rows / element | `Propagation.muellerOfRows` / `muellerElement` | `Propagation.fs:35 / 40` |
| Frame rotation `R(φ)`, `R(−φ)·M·R(φ)` | `Propagation.rotationMueller` / `rotateMueller` | `Propagation.fs:117 / 128` |
| Retarder / rotator reference forms | `MuellerReconstruction.retarderMueller` | `MuellerReconstruction.fs:39` |
| 4×4 difference metric (Frobenius, mean, max, argmax) | `MuellerReconstruction.frobeniusDiff` | `MuellerReconstruction.fs:96` |
| Nonlinear LSQ (LM) | `AlglibAdapter.optimize` + `OptimizationRequest` | `OptimizationInterface.fs:59` |
| χ², σ, 95 % CI, covariance, **correlation**, residuals | `FitQuality.reportFrom` → `FitReport` | `FitQuality.fs:29` |
| Bounded parameters | `OptimizationInterface.ParameterBounds` | `OptimizationInterface.fs:34` |
| Element-wise 4×4 tolerance assert | the `assertMuellerEqual` loop + `allowedDiff` | `MuellerMatrixTests.fs:20`, `MatrixComparison.fs:13` |
| Proxy record + factory + mock shape | `MuellerSolverProxy` / `createMathNetSvd` / its test mock | `MuellerReconstruction.fs:164, :204` |
| Elevated primitives | `Angle`, `WaveLength`, `Thickness`, `RefractionIndex`, `RhoValue`, `EpsValue`, `IncidenceAngle` | `Geometry.fs`, `Fields.fs`, `MaterialProperties.fs` |

### 4.3 Gaps to build (small, well-scoped)

1. **The `Quartz` material** (§3.1–3.2), in `OpticalProperties`.
2. **`OpticalConstructor.Domain/MuellerInverse.fs`** — the pure inverse module (§5).
3. **`ForwardModelProxy`** and **`NonlinearSolverProxy`** (§4.4).
4. **`ParameterScaling`** — the dimensionless ⇄ physical mapping that the fixed `1e-6` LM
   differentiation step makes mandatory (§2.5). This is a *type*, not a convention: the scaled vector
   and the physical parameters must not be interchangeable by accident.
5. **`analyticInversion`** — the Arteaga/Canillas closed form (or the matrix-logarithm route). It is
   the one genuinely new piece of physics-math; everything else is assembly.
6. **`normalizeMueller`** (divide by `m00`) and a **non-depolarization / Cloude-eigenvalue check**,
   which Stage A needs as a precondition and which 0042 already listed as a future candidate.

### 4.4 Proxies — the forward model *and* the optimizer

0042's review answer Q7 was "the numeric solve is an external routine ⇒ put it behind a proxy so
backends are swappable," and its §5.5 anticipated a `refineNonlinear` field. This spec takes that up
with **two** proxies, both following the repo's real shape (module factory + private `try/with`
boundary, per `ExperimentDataStore.fs:34` / `MuellerDataStore.fs`):

**`ForwardModelProxy`** — one function field mapping a configuration and a parameter set to a Mueller
matrix. The real factory closes over `OpticalSystemSolver`. It exists for three reasons, in order of
importance: (a) `OpticalSystemSolver` **throws** on degenerate inputs (`getMuellerMatrix` ends in
`failwith "Invalid combination of parameters…"`), and per the repo's error rules that exception must
be caught at this boundary and mapped to a typed `ForwardModelError` — F# above the boundary never
handles exceptions; (b) the fit is the solver's hot loop, so a memoizing or coarsened backend is a
plausible later swap; (c) a mock backend lets the residual/scaling/reporting logic be tested with zero
solver calls, which is the same trick `ExperimentDataProxyTests` already uses.

**`NonlinearSolverProxy`** — one function field taking a residual closure, a start vector and bounds,
returning a solution or a typed error. The real factory wraps `AlglibAdapter.optimize` with
`LevenbergMarquardt`; a mock returns a canned solution. This is the exact analogue of 0042's
`MuellerSolverProxy`, and it keeps ALGLIB confined to the one file that is allowed to see it.

Both records take `[<ReferenceEquality>]`, matching `MuellerDataProxy` / `MuellerSolverProxy`, and
both sides of both proxies are elevated types.

---

## 5. The intended decomposition (many tiny pure functions)

Illustrative signatures only — the point is the granularity and the elevated types, not the exact
names. Every one of these is pure and concretely typed.

```
// --- elevated inputs ---
type OpticAxisCut       = ZCut | XCut | TiltedCut of Angle
type SampleAzimuth      = SampleAzimuth of Angle
type Observable         = TransmittedMueller | ReflectedMueller
type MeasurementConfiguration =
    { cut : OpticAxisCut; thickness : Thickness; incidenceAngle : IncidenceAngle
      azimuth : SampleAzimuth; observable : Observable; waveLength : WaveLength }

type MaterialParameters =
    { ordinaryIndex : RefractionIndex; extraordinaryIndex : RefractionIndex
      g11 : RhoValue; g33 : RhoValue }

type MuellerObservation = { configuration : MeasurementConfiguration; measured : MuellerMatrix }

// --- forward-side helpers (pure) ---
gyrationProjection   : Angle -> RhoValue -> RhoValue -> RhoValue      // g11 sin^2 t + g33 cos^2 t
internalAngle        : RefractionIndex -> IncidenceAngle -> Angle     // Snell
orientForCut         : OpticAxisCut -> SampleAzimuth -> OpticalProperties -> OpticalProperties

// --- data reduction (pure) ---
normalizeMueller     : MuellerMatrix -> MuellerMatrix                 // divide by m00
muellerElements      : MuellerMatrix -> float[]                       // the 15 normalized elements
elementResidual      : float -> float -> float -> float               // (model, data, sigma)
residualVector       : MuellerObservation list -> MuellerMatrix list -> float[]

// --- Stage A: analytic inversion (pure, closed form) ---
type ElementaryAnisotropy = { lb : Retardance; lbPrime : Retardance; cb : Retardance
                              ld : float; ldPrime : float; cd : float }
analyticInversion    : MuellerMatrix -> Result<ElementaryAnisotropy, InversionError>
transparencyResidual : ElementaryAnisotropy -> float                  // |LD|+|LD'|+|CD|, must be ~0
startingParameters   : ElementaryAnisotropy list -> MaterialParameters

// --- Stage B: scaling + fit ---
type ParameterScaling = { centre : MaterialParameters; scale : MaterialParameters }
toScaled             : ParameterScaling -> MaterialParameters -> float[]
ofScaled             : ParameterScaling -> float[] -> MaterialParameters
buildResidual        : ForwardModelProxy -> ParameterScaling -> MuellerObservation list -> (float[] -> float[])
solveInverse         : NonlinearSolverProxy -> ... -> Result<InverseSolution, InverseError>

// --- diagnostics ---
type InverseSolution = { parameters : MaterialParameters; report : FitQuality.FitReport
                         conditionNumber : float; iterations : int }
```

---

## 6. The test(s) and their acceptance criteria

New file `BerremanTests/MuellerInverseTests.fs`, appended to the `Compile` list after
`MuellerReconstructionTests.fs`. Shape mirrors `MuellerReconstructionTests` (xUnit v3 `[<Fact>]`,
back-tick names naming the acceptance criterion, a top-of-file `///` block listing the deliberately
un-asserted future candidates). **Every test is self-contained: the forward data is generated in-test
from the library, so there is no external data, no skip path, and no committed fixture.**

**Always-run pure unit tests (no solver, no optimizer):**

- **T1** `gyrationProjection` returns `g₃₃` at θ = 0 and `g₁₁` at θ = 90°, and the class-32 tensor
  assembled through `Rho.type_3_4_6_Crystal` is `diag(g₁₁, g₁₁, g₃₃)` imaginary.
- **T2** `orientForCut` on a z-cut/x-cut produces the expected rotated `Eps`/`Rho` (compare against
  literals built independently, the way `RhoWithDispValueTests` already does).
- **T3** `analyticInversion` of a synthetic **pure rotator** returns `CB = 2ρd`, `LB = LB′ = 0` and
  all three dichroisms ≈ 0; of a synthetic **pure linear retarder** it returns the retardance and
  azimuth and `CB ≈ 0`. Round-trip: `analyticInversion ∘ (build from coefficients) = id`.
- **T4** `toScaled` / `ofScaled` round-trip exactly, and a 1-unit step in the scaled vector moves each
  physical parameter by its intended physical amount (this is the guard on the §2.5 step-size trap).

**Forward-model tests (solver, no optimizer):**

- **T5** *(the external cross-check, §3.3)* the C1 transmitted Mueller matrix is a pure rotator to
  within tolerance, and its rotation angle is **18.6° ± 0.2°** for a 1 mm z-cut plate — i.e. within
  1 % of the literature 18.8 °/mm.
- **T6** every configuration's Mueller matrix is non-depolarizing to within tolerance (Cloude
  coherency eigenvalues non-negative, three of them ≈ 0), and `LD = LD′ = CD ≈ 0` — the transparency
  invariant. This also validates the `numberOfReflections` choice.

**Inverse tests (the deliverable):**

- **T7 — the round trip.** From the noiseless C1–C4 dataset, starting from a perturbed guess
  (`n` off by +2 %, `g` off by +30 %, `d` off by +10 %), the fit recovers all parameters. Suggested
  acceptance: `n_o`, `n_e` to **1e-6 relative**; `g₁₁`, `g₃₃` to **1e-4 relative**; `d` to **1e-5
  relative**; final χ² below **1e-16** in the scaled residual space. The exact bands are tuned during
  implementation and pinned once observed — the same protocol 0042 used for its 2e-3 band.
- **T8 — identifiability is reported and sane.** `FitQuality`'s correlation matrix has no
  off-diagonal magnitude above a pinned bound (proposed 0.99), every parameter's 95 % CI brackets the
  truth, and the Jacobian condition number is finite and below a pinned bound.
- **T9 — the ablation (this is the executable answer to task item 1).** Refitting from **C1 alone**,
  `g₁₁` is **not** recovered: its Jacobian column is ≈ 0 and its reported standard error exceeds the
  parameter itself by orders of magnitude. Adding C3 restores it. Asserting the failure is what makes
  the multi-configuration experiment design a *result* rather than an assumption.
- **T10 — mock-driven, no solver.** The whole Stage-B pipeline runs against a `ForwardModelProxy`
  mock returning canned matrices and a `NonlinearSolverProxy` mock returning a canned solution,
  proving the residual/scaling/reporting logic is testable without touching the solver — the same
  guarantee 0042 established for its data path.
- **T11 — typed errors, never a throw.** A degenerate configuration drives `ForwardModelProxy` into
  its `try/with` boundary and yields a typed `ForwardModelError`; a non-converging request yields a
  typed `InverseError`. No exception crosses either proxy.

**Optional / documented in the top-of-file comment, not implemented in the first pass:**
noise robustness (σ = 1e-3 on normalized elements → `g₃₃` recovered inside its reported 1σ, `g₁₁`
markedly worse, mirroring the literature's ±10 %); multistart/global search; the rotating-analyzer
intensity-level variant that would reuse 0042's `kron4`/design-matrix machinery end to end;
Lu–Chipman polar decomposition; dispersion (multi-wavelength); a biaxial or absorbing material.

**Runtime is a real design constraint.** Each residual evaluation runs one `OpticalSystemSolver` per
configuration, and LM with numerical differentiation costs `(n_params + 1)` residual evaluations per
iteration. The C1–C4 set as tabulated is ≈ 40 configurations; with 5 parameters and ~30 iterations
that is ~7 000 solver calls. That should be seconds, not minutes, but it must be **measured in the
first slice** — if it is slow, trim the angle grids (they are oversampled for a noiseless fit) rather
than loosening tolerances.

---

## 7. Part 4 — Repoint `MuellerReconstructionTests` at `Data/MuellerMatrix/data.zip`

### 7.1 What the tests need today

Two facts in `MuellerReconstructionTests.fs` are data-dependent, and both currently locate their data
by `tryFindOpmFinalDir()` (`:136`), which walks up from the test output directory looking for a
sibling `optics-mueller\data\raw\final\lp_lp.csv` checkout and returns `None` when it is absent — at
which point the facts call `Assert.Skip` (`:480`, `:677`) instead of running:

- ``Stage-1 re-derived calibration constants cross-check the section 2.2 rounded values`` (`:472`)
- ``section 7.2 end-to-end: … reconstructs M_QZ, M_LR and M_{QZ+LR} …`` (`:664`)

Between them they load exactly **seven** files: `lp_lp.csv`, `lp_cpl.csv`, `cpl_lp.csv`,
`cpl_cpl_day1_main.csv`, `cpl_cpl_day2_corrections.csv`, `darkness_checks.csv`, `bullshit_checks.csv`.

### 7.2 Verified inventory of `data.zip` — **nothing the tests need is missing**

`Berreman/Data/MuellerMatrix/data.zip` (62 777 bytes, tracked in git since `904d24a`) was opened and
every entry inspected. It contains 18 files:

| Entry | Data rows | Needed by the tests? |
|---|---|---|
| `data/raw/final/lp_lp.csv` | 209 | **yes** |
| `data/raw/final/lp_cpl.csv` | 209 | **yes** |
| `data/raw/final/cpl_lp.csv` | 209 | **yes** |
| `data/raw/final/cpl_cpl_day1_main.csv` | 170 | **yes** |
| `data/raw/final/cpl_cpl_day2_corrections.csv` | 57 | **yes** |
| `data/raw/final/darkness_checks.csv` | 6 | **yes** |
| `data/raw/final/bullshit_checks.csv` | 2 | **yes** |
| `data/raw/*.csv` (8 files: `cpl_cpl*.csv`, `cpl_lr45_*.csv`) | — | no — earlier exploratory sweeps |
| `data/shapes/*.json` (3 files) | — | no — ellipse-shape metadata |

The seven required row counts match spec 0042 §6's documented inventory **exactly** (209/209/209/
170/57/6/2). Every file carries the full 16-column header with a UTF-8 BOM and ISO-8601 `captured_at`
values — i.e. precisely the shape `MuellerDataStore.parseMuellerCsv` is already written to consume,
BOM-stripping and all. **No parser change is required.**

**Advice on what is genuinely absent (the "double check and advise" the task asks for).** Three things
are not in the archive. None blocks the change; the last one is worth a decision:

1. **The reference Python scripts** (`matrix_step1_air_fit.py`, `matrix_glue.py`,
   `matrix_fit_linear.py`) and the OPM `notes/`. Spec 0042 calls these "authoritative, exact" — the
   arbiter when the report and the code disagree. The **test** does not need them (it is pure F# and
   re-derives everything), but the next person to touch this pipeline will want them and they will no
   longer be a `..\` away. **Recommendation: add them to the archive under a `reference/` prefix, or
   copy them into `specs/0042/.manual/`.** Cheap insurance; not a blocker.
2. **The report PDF** — already in the repository at `specs/0042/.manual/MuellerMatrix_final.pdf`.
   Nothing to do.
3. **The full-precision golden file.** Spec 0042 §5.6 (review Q8) proposed committing a
   `reference_summary.json` emitted once, offline, by the OPM Python, to enable a ~1e-9 regression
   anchor. **It was never produced.** The current test instead compares against 6-decimal literals
   inlined at `MuellerReconstructionTests.fs:840-857` with a 2e-3 band. That is sufficient for the
   present assertions, so this is not "missing data" in the blocking sense — but if the tight anchor
   is still wanted, **now is the moment**: once the sibling checkout is no longer wired up, generating
   it gets harder. **Recommendation: generate it while `C:\GitHub\optics-mueller` is still present**
   (it is), and commit it either into the archive or as a `BerremanTests/fixtures/` file.

### 7.3 The change

**The proxy contract does not move.** `MuellerDataProxy.tryLoadFamily : DataFilePath -> Result<…>`
(`MuellerReconstruction.fs:298`) stays byte-identical, and so does every consumer. Only the
**factory** — the composition root — changes. This is exactly the substitution the functional-proxy
discipline exists to make cheap, and it is the strongest argument for the design.

1. **`MuellerDataStore` gains `createArchiveBacked`.** A new factory alongside `createFileBacked`
   (`MuellerDataStore.fs:157`), closing over the archive's path; the `DataFilePath` handed to
   `tryLoadFamily` is interpreted as the **entry name inside the archive**
   (`data/raw/final/lp_lp.csv`). It opens the archive with `System.IO.Compression.ZipFile.OpenRead`,
   reads the entry's text, and delegates it **unchanged** to the existing pure `parseMuellerCsv` —
   the same `loadThrough` shape, with the zip read replacing `File.ReadAllText` as the boundary. All
   .NET IO and compression exceptions are caught **there** and mapped to a typed error, never thrown
   across into the pure Domain. `System.IO.Compression` is in the .NET shared framework — **verify at
   implementation that no `PackageReference` is needed** (it should not be on `net10.0`).
   Opening the archive per call is fine at 62 KB; a `tryLoadAll` variant is an optimization, not a
   requirement.
2. **The archive path is elevated.** Introduce a single-case `MuellerArchivePath` (or reuse
   `DataFilePath` for it, §9 Q5) rather than passing a bare `string` into the factory.
3. **`MuellerDataError` gains a case.** A missing archive entry is a distinct, actionable failure
   that deserves its own case carrying both the archive and the entry name, rather than being folded
   into `MalformedRow`'s reason string — the "error DUs carry diagnostic payload" rule points that
   way. The cost is updating the match sites, all of which are inside Storage and the tests.
   (Reusing `MalformedRow` is defensible — the existing doc comment already sanctions mapping IO
   failures onto it — so this is a review call, §9 Q6. **Recommendation: add the case.**)
4. **The archive is copied to the test output.** In `BerremanTests.fsproj`, alongside the existing
   item groups:
   `<Content Include="..\Data\MuellerMatrix\data.zip" Link="Data\MuellerMatrix\data.zip">`
   `<CopyToOutputDirectory>PreserveNewest</CopyToOutputDirectory></Content>`.
   Precedent: `OpticalConstructor.Tests.fsproj:233ff` copies its `fixtures\*` the same way. The test
   then resolves the archive as `Path.Combine(AppContext.BaseDirectory, "Data", "MuellerMatrix",
   "data.zip")`.
5. **`tryFindOpmFinalDir()` and both `Assert.Skip` sites are deleted.** The two facts become
   unconditional, and their doc comments are updated to say the data is committed rather than
   walked-up-to. The `load "<name>.csv"` helpers change only in that the name becomes the archive
   entry path.
6. **`.gitattributes` gains `*.zip binary`** in the existing "Binary (never normalized)" block. The
   file is currently protected only by git's `text=auto` binary auto-detection; it reads back
   correctly today, but the repository's stated policy is an explicit rule per type, and an explicit
   rule removes any chance that a future `git add --renormalize` corrupts the archive. **Do this in
   the same slice.**

### 7.4 Consequences

- **The `unit-tests` gate gets stronger, not weaker.** Its `count_at_least` capture is
  `"Passed:\s*(\d+)"`, and a *skipped* xUnit test is not counted as passed. Today the passed count
  depends on whether the machine happens to have a sibling `optics-mueller` checkout — the two facts
  pass here and would skip on a clean CI box. After the change the count is **deterministic and two
  higher on any machine**. The baseline must be re-captured once after the change; that is a
  baseline bump, not a regression.
- The `Xunit.SkippableFact` package reference stays — `SolverTests.fs:942-948` still uses it.
- Nothing outside `MuellerReconstructionTests.fs`, `MuellerDataStore.fs`,
  `MuellerReconstruction.fs` (the error DU), `BerremanTests.fsproj` and `.gitattributes` is touched.

---

## 8. Proposed phasing (slices)

| # | Slice | Contents | Gate |
|---|---|---|---|
| 1 | **Archive-backed data** | §7.3 in full — `createArchiveBacked`, elevated archive path, error case, `fsproj` content copy, `.gitattributes`, delete the skips. Independent of everything else; land it first. | build + unit-tests (re-baseline) |
| 2 | **Quartz material** | §3 — the `Quartz` definition in `OpticalProperties`, `RhoWithDispValueTests` repointed at it, T1/T2 unit tests, and the §3.3 rotation cross-check T5. | build + unit-tests |
| 3 | **Inverse primitives** | `MuellerInverse.fs` — configurations, orientation, normalization, `gyrationProjection`, `ParameterScaling`, `residualVector`; T4 and the scaling guard. Pure, no solver. | build + unit-tests |
| 4 | **Analytic inversion (Stage A)** | `analyticInversion`, `transparencyResidual`, `startingParameters`; T3, T6. | build + unit-tests |
| 5 | **Proxies** | `ForwardModelProxy` + real/mock factories; `NonlinearSolverProxy` + ALGLIB/mock factories; T10, T11. | build + unit-tests |
| 6 | **Stage B + acceptance** | Wire the composition root, fit the C1–C4 dataset, tune and pin the tolerance bands; T7, T8, T9. | build + unit-tests |
| 7 | *(later)* | The documented future candidates from §6. | — |

Slice 1 is deliberately first and self-contained: it is the item with a concrete, already-verified
target, and it de-risks the measured-data tests before any new physics lands.

---

## 9. Open questions for review

- **Q1 — Experiment set.** Is the four-configuration C1–C4 set (§2.4) the right scope, or should the
  first pass be the two-cut minimum (C1 + C3) with C2/C4 deferred? The four-row set is what makes the
  identifiability claims assertable; the two-row set is faster.
- **Q2 — Fit `d`, or declare it known?** §2.4 recommends fitting it (with reflection data present) to
  make the test a stronger identifiability statement. Declaring it known is the realistic lab case and
  removes the hardest correlation.
- **Q3 — Where does `Quartz` live?** A new `OpticalProperties/Quartz.fs` (recommended) or appended to
  `Active.fs`? And should a dispersive `Quartz()` with the Ghosh Sellmeier be added alongside
  `Langasite` at the same time, or deferred (the task says single-wavelength)?
- **Q4 — `numberOfReflections`.** Pin it to what? A single-pass value keeps the Mueller matrices
  strictly Mueller–Jones and makes Stage A exact; the default 3 is more physical but weakly
  depolarizing. Recommendation: pin explicitly, document the choice, and let T6 police it.
- **Q5 — Archive path type.** A new elevated `MuellerArchivePath`, or reuse `DataFilePath` for the
  `.zip` and let the second `DataFilePath` mean the entry?
- **Q6 — `MuellerDataError`.** Add `ArchiveEntryMissing of archive : string * entry : string`
  (recommended), or fold archive failures into the existing `MalformedRow` reason string as the
  file-backed adapter already does for IO failures?
- **Q7 — The 0042 golden file.** Generate the full-precision `reference_summary.json` now, while
  `C:\GitHub\optics-mueller` is still present, and commit it (recommended) — or accept the current
  2e-3 band against inlined literals as final?
- **Q8 — Reference scripts.** Copy the three OPM Python scripts into the repository (archive
  `reference/` prefix, or `specs/0042/.manual/`) so the authoritative algorithm survives the
  decoupling — or accept that they stay external?
- **Q9 — Tolerances.** The bands in §6 T7/T8 are proposals. Confirm the protocol: pin them once
  observed, as 0042 did for its 2e-3 band, rather than choosing them up front.
