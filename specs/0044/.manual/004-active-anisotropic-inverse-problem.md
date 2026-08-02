# Spec 0044 — Inverse determination of the optical properties of a transparent, optically active, anisotropic, homogeneous material, and retirement of the external Mueller data dependency

Status: **authoritative.** This supersedes `002-active-anisotropic-inverse-problem.md` and folds in
every decision from `003-answers.txt`. There are no options in this document: where `002` offered a
choice, the choice has been made and is stated as a requirement. §11 records the decision register so
the reasoning behind each resolved point stays visible. §12 lists the small number of items that are
settled by measurement during implementation rather than by fiat; none of them blocks a slice, and
none of them is a question for the operator.

Sources this spec was built from:

- Task: `specs/0044/.manual/001-task.txt`. Answers: `specs/0044/.manual/003-answers.txt`.
- Spec 0042 (`specs/0042/.manual/004-mueller-inverse-reconstruction-test.md`) — the design this one
  follows for structure, proxy discipline, and tolerance protocol.
- The repository itself. Every file:line citation below was read, not inferred.
- Web research on the measurement and inversion of gyrotropic anisotropic crystals (§3.6).

---

## 1. Scope

Two deliverables, one test project.

**Deliverable A — a synthetic inverse-problem exercise.** Quartz is a transparent, optically active,
anisotropic, homogeneous, uniaxial class-32 crystal. At one wavelength, generate "measured" Mueller
matrices with this repository's own Berreman solver, then recover the material's optical constants
from those matrices and assert the round trip. The ground truth is known exactly, so the test measures
the inverse machinery, not the physics.

**Deliverable B — retirement of the external data dependency.** `MuellerReconstructionTests` stops
looking for a sibling `optics-mueller` checkout and reads the committed
`Berreman/Data/MuellerMatrix/data.zip` instead, entirely behind a proxy. The reference Python scripts
and the reference numerical results are copied into the repository so that nothing of value is lost
when the external checkout disappears.

Out of scope: fitting real measured quartz data; dispersion (the task fixes one wavelength);
absorbing, depolarizing or biaxial materials; any UI; executing Python at build or test time.

The 0042 disciplines carry over unchanged and are the reason the design looks the way it does: **many
tiny pure functions**, **IO and the optimizer behind `*Proxy` records**, **every primitive elevated**,
**`Result` at every boundary, never a throw across one**, and **a numeric core that stays pure**.

---

## 2. Requirements summary

| # | Requirement |
|---|---|
| R1 | The forward data set is the full four-configuration set C1–C4 of §3.4. |
| R2 | Sample thickness is **known**, not fitted. The fit vector is exactly `(n_o, n_e, g₁₁, g₃₃)`. |
| R3 | Quartz is **test data**, defined in the test project. `OpticalProperties` is not modified. |
| R4 | `SolverParameters.numberOfReflections = 3`, stated explicitly at every call site. |
| R5 | The measured-data test knows nothing about files, archives, entry names or locations. All of it is behind the data proxy. |
| R6 | Absent or unreadable measured data is a **test failure**, never a skip. |
| R7 | No code path may reference, probe for, or walk up to `optics-mueller`. |
| R8 | The four reference Python scripts and the two reference result files are copied into `Berreman/Data/MuellerMatrix/`, with a `README.md` recording their provenance. |
| R9 | Numeric tolerances are pinned once observed, per the 0042 protocol (§9). |

---

## 3. Part A — The experiment set and the inversion method

### 3.1 The unknowns

For a transparent, homogeneous, uniaxial class-32 crystal at a single wavelength the complete optical
description is four numbers plus the geometry:

| Symbol | Meaning | Quartz at 632.8 nm |
|---|---|---|
| `n_o` | ordinary index, ε₁₁ = ε₂₂ = n_o² | 1.542606 |
| `n_e` | extraordinary index, ε₃₃ = n_e² | 1.551651 |
| `g₁₁` | gyration component perpendicular to the optic axis | +5.9 × 10⁻⁵ |
| `g₃₃` | gyration component along the optic axis | −10.1 × 10⁻⁵ |
| `d` | thickness | **known** (R2) |
| cut, azimuth | crystal orientation | **known**, fixed per configuration |

Class 32 forces the gyration tensor to `diag(g₁₁, g₁₁, g₃₃)`; `Rho.type_3_4_6_Crystal`
(`OpticalProperties/Active.fs:47`) is exactly that assembly and the engine stores it as an imaginary
matrix (`Rho.fromIm`, `MaterialProperties.fs:139`). Transparency means all three dichroisms are
structurally zero — an invariant the method exploits twice, once as a starting-value simplification
and once as a test assertion.

**The fit vector is four-dimensional.** Thickness is known (R2), the cut and azimuth are fixed by the
experiment, and there is no dispersion. This is what makes the problem tractable enough to assert
tight round-trip tolerances.

### 3.2 The identifiability physics

For propagation at internal angle `θ` to the optic axis, the effective gyration seen by the wave is
the projection of the tensor onto the propagation direction:

```
G(θ) = g₁₁ · sin²θ + g₃₃ · cos²θ
```

while the available linear birefringence runs from zero at `θ = 0` to its full `n_e − n_o` at
`θ = 90°`. That single expression dictates the entire experiment design:

- **`θ = 0`** — light along the optic axis, i.e. a z-cut plate at normal incidence. There is no linear
  birefringence at all; the sample is a pure circular retarder and the transmitted Mueller matrix is a
  pure rotator. `G = g₃₃` is read off directly, with excellent conditioning.
- **`θ = 90°`** — light perpendicular to the optic axis, i.e. an x-cut plate at normal incidence.
  `G = g₁₁`, but the linear birefringence is now maximal and buries the optical activity. This is the
  historically contentious measurement that HAUP and two-modulator generalized ellipsometry exist to
  do well.
- **Intermediate `θ`** mixes the two, and is what separates `g₁₁` from `g₃₃` in a joint fit.

**A constraint that is easy to miss and that fixes the design: you cannot reach large `θ` by tilting a
z-cut plate.** Snell's law caps the internal angle at `arcsin(1/n_o) = 40.41°` for quartz, so even at
grazing incidence a z-cut sample gives `sin²θ ≤ 0.42` — `g₁₁` never carries more than 42 % of the
projection weight. This is precisely why Arteaga, Canillas and Jellison report `g₃₃` to 0.7 % but
`g₁₁` only to about 10 % from a single z-cut crystal. **The experiment set therefore must include a
cut whose optic axis is not along the normal.** Tilting alone is not sufficient, and C3/C4 exist for
this reason and no other.

With `d` known (R2), the remaining separations are straightforward: the transmitted linear retardance
fixes `n_e − n_o` directly, the angle-of-incidence dependence of the Fresnel amplitudes fixes the
absolute level of `n_o`, and the reflection matrices supply index information that does not pass
through the sample at all.

### 3.3 Sample thickness and the retardance order

Although `d` is known, the thickness of the birefringent samples is still a design variable, because
it controls the shape of the χ² surface. A 1 mm x-cut quartz plate is 14.29 waves of linear
retardance at 632.8 nm; the residual then oscillates violently with `n_e − n_o` and the fit acquires a
dense forest of local minima roughly half a fringe apart. **The linear-birefringence-bearing samples
(C3, C4) are therefore 20 µm thick**, which is 0.286 wave — sub-wave, so the residual is smooth and
unimodal in `n_e − n_o` and a local optimizer is the correct tool. The z-cut samples (C1, C2) carry no
retardance at normal incidence and are 1.0 mm, which makes their optical rotation (18.62°) large and
easy to assert.

### 3.4 The experiment set (R1)

All at **λ = 632.8 nm**. "Azimuth" is rotation of the sample about the surface normal; "cut" is the
orientation of the optic axis relative to that normal.

| # | Cut | Thickness | Incidence angles | Azimuths | Observable | Role |
|---|---|---|---|---|---|---|
| C1 | z-cut (axis ∥ normal) | 1.0 mm | 0° | 0° | `M_T` | anchors `g₃₃`; pure rotator, 18.62° |
| C2 | z-cut | 10°…70° step 10° | | 0°, 45° | `M_T`, `M_R` | anchors `n_o`, `n_e`; `g₁₁` at ≤ 42 % weight |
| C3 | x-cut (axis in the surface plane) | 20 µm | 0° | 0°, 22.5°, 45°, 67.5° | `M_T` | anchors `n_e − n_o` and **`g₁₁`** |
| C4 | x-cut | 20 µm | 0°…60° step 15° | 0°, 45° | `M_T`, `M_R` | second independent look at `g₁₁`; index/angle leverage |

(C2's thickness is 1.0 mm; the cell is merged above for width.) Every configuration contributes the
15 `m00`-normalized Mueller elements of each requested observable, so the data vector is roughly 900
numbers against four unknowns — massively over-determined, which is what makes the identifiability
diagnostics meaningful rather than decorative.

### 3.5 The inversion method

**Two stages, mirroring the shape spec 0042 settled on: a closed-form stage that produces trustworthy
starting values and diagnostics, then the authoritative nonlinear stage.**

**Stage A — analytic inversion of each measured Mueller matrix, closed form, no optimizer.** For a
homogeneous non-depolarizing sample the normalized Mueller–Jones matrix inverts in closed form to the
six elementary anisotropy coefficients: linear birefringence `LB` and `LB′` (the 0°/45° pair),
circular birefringence `CB`, and the three dichroisms `LD`, `LD′`, `CD` (Arteaga & Canillas,
*Opt. Lett.* **35**, 559 (2010), with the erratum *Opt. Lett.* **35**, 3525 (2010)). Because the
sample is transparent, all three dichroisms must come back ≈ 0, which gives Stage A a free
self-consistency check. What survives is `CB`, which yields `G(θ)` and hence `g₃₃` from C1 and `g₁₁`
from C3, and `LB`/`LB′`, which yield `(n_e − n_o)·d` and the in-plane axis azimuth.

**Stage A is a starting-value generator and a diagnostic. It is never the answer.** One reason is
structural and follows from R4: with `numberOfReflections = 3` the `Plate` branch of the solver
accumulates five emerging beams and `getMuellerMatrix` (`Solvers.fs:286`) **sums their Mueller
matrices**, so `muellerMatrixT()` is the direct beam plus the beam that has bounced twice inside the
plate. A sum of Mueller–Jones matrices is in general weakly depolarizing, and the analytic inversion
assumes a Mueller–Jones matrix. The contamination is small — the second transmitted beam carries about
`R² ≈ 0.2 %` of the first at quartz's `R ≈ 4.6 %` — so Stage A remains accurate enough for starting
values and for the transparency check, but not accurate enough to be the reported result. Stage B
carries the full multiple-reflection model and therefore has no such error.

**Stage B — nonlinear least squares against the full Berreman forward model, authoritative.** Fit the
four parameters against **all four configurations simultaneously**, with the residual built from the
`m00`-normalized Mueller elements:

```
r_k = ( M_model[i,j] / M_model[0,0]  −  M_data[i,j] / M_data[0,0] ) / σ
```

Levenberg–Marquardt through `AlglibAdapter.optimize` (`LevenbergMarquardt → minlm`) is the method.
Nothing external is required, which is the same conclusion 0042 reached for its linear stage. Global
search is not used: the data is noiseless, the start point is physically sensible, and §3.3 has
removed the multimodality that would motivate it.

**Parameter scaling is mandatory and is a type, not a convention.** The physical parameters span four
orders of magnitude (`n ≈ 1.5`, `g ≈ 1e-4`), and `AlglibAdapter.runLm` calls
`alglib.minlmcreatev(n, m, x0, 1.0e-6, &state)` — a **fixed absolute** numerical-differentiation step
of `1e-6`. Applied to a gyration component of `1e-4` that is a 1 % perturbation, which is far outside
the linear regime the Jacobian assumes. The fit vector is therefore **dimensionless and O(1)**: each
parameter is carried as a scaled offset, and the mapping back to physical units happens inside the
residual closure. `ParameterScaling`, `toScaled` and `ofScaled` (§6) exist for this and are covered by
their own unit test, because a silent scaling error produces a fit that converges to the wrong answer
rather than one that fails loudly.

**Identifiability reporting is part of the deliverable.** `FitQuality.reportFrom`
(`Optimization/FitQuality.fs:114`) returns χ², reduced χ², standard errors, 95 % confidence intervals,
the covariance matrix and the parameter correlation matrix, and `FitQuality.residualJacobian`
(`FitQuality.fs:85`) is public. Its per-parameter step is `√eps · max(|x_p|, 1e-12)` — **relative**,
so it behaves correctly on the scaled O(1) vector.

One property of that machinery drives the test design and must not be got wrong. `Cov = reducedχ² ·
(JᵀJ)⁻¹`, and on noiseless synthetic data `reducedχ²` is ~1e-30. Therefore:

- **standard errors and confidence intervals are numerically meaningless** on noiseless data — they
  collapse toward zero and must not be asserted against the truth;
- **the correlation matrix is still valid**, because `corr_ij = cov_ij / (σ_i σ_j)` and the
  `reducedχ²` factor cancels exactly;
- **the Jacobian condition number is still valid**, being a property of `J` alone.

So identifiability is asserted through the **correlation matrix** and the **Jacobian condition
number**, and through the **column norms of `J`**, never through σ or CI. Confidence-interval coverage
belongs to the noisy variant, which is documented as a future candidate and not implemented here.

The condition number is computed locally from `FitQuality.residualJacobian` through the existing
`Berreman.MathNetNumericsMath` `RealMatrix` seam — the same seam `FitQuality.covarianceMatrix` uses.
`LeastSquaresSolution` (`MuellerReconstruction.fs:146`) is **not** extended: it is a spec-0042
contract, it carries `solution`, `rank` and `rmse`, and this spec does not need it to carry more.

### 3.6 References

- O. Arteaga, A. Canillas, G. E. Jellison Jr., "Determination of the components of the gyration tensor
  of quartz by oblique incidence transmission two-modulator generalized ellipsometry," *Appl. Opt.*
  **48**(28), 5307 (2009). <https://opg.optica.org/ao/abstract.cfm?uri=ao-48-28-5307>
  Reports `g₃₃ = 10.06 ± 0.07 × 10⁻⁵` and `g₁₁ = 4.8 ± 0.5 × 10⁻⁵` at 632.8 nm, ratio ≈ −0.486, and
  states that accurate `g₁₁` needs large angles of incidence — the source of §3.2's constraint.
- O. Arteaga, A. Canillas, "Analytic inversion of the Mueller–Jones polarization matrices for
  homogeneous media," *Opt. Lett.* **35**(4), 559 (2010); erratum *Opt. Lett.* **35**(20), 3525 (2010).
  <https://opg.optica.org/ol/abstract.cfm?uri=ol-35-4-559> — Stage A.
- O. Arteaga, "Mueller matrix polarimetry of anisotropic chiral media" (PhD thesis, U. Barcelona).
  <https://www.tdx.cat/bitstream/handle/10803/687323/OAB_PhD-THESIS.pdf> — the long-form treatment of
  separating optical activity from linear birefringence.
- K. Kobayashi, Y. Uesu, HAUP: "A new optical method and apparatus for measuring simultaneously
  optical activity and birefringence of crystals," *J. Appl. Cryst.* (1983), and the accuracy
  improvements in *J. Appl. Cryst.* **19** (1986); α-quartz with multiple reflections, *JOSA B*
  **15**(3), 1147 (1998). <https://opg.optica.org/josab/abstract.cfm?uri=josab-15-3-1147>
- R. Ossikovski, "Differential matrix formalism for depolarizing anisotropic media," *Opt. Lett.*
  **36**(12), 2330 (2011). <https://opg.optica.org/ol/abstract.cfm?uri=ol-36-12-2330> — the
  matrix-logarithm route, equivalent to Stage A for non-depolarizing media.
- N. Ortega-Quijano, J. L. Arce-Diego, "Mueller matrix differential decomposition," *Opt. Lett.*
  **36**, 1942 (2011).
- M. Schubert, "Polarization-dependent optical parameters of arbitrarily anisotropic homogeneous
  layered systems," *Phys. Rev. B* **53**, 4265 (1996) — generalized ellipsometry on the 4×4
  formalism; the standard practice of regressing multiple angles of incidence and multiple sample
  azimuths simultaneously.
- G. Ghosh, "Dispersion-equation coefficients for the refractive index and birefringence of calcite
  and quartz crystals," *Opt. Commun.* **163**, 95 (1999) — the Sellmeier evaluated in §4.
- Carried over from spec 0042 §8: Chipman, *Polarized Light and Optical Systems* Ch. 7; Goldstein,
  *Appl. Opt.* **31**, 6676 (1992); Gil, *Appl. Opt.* **55**, 5449 (2016); Lu & Chipman,
  *JOSA A* **13**, 1106 (1996).

---

## 4. Part B — Quartz as test data

**Quartz is defined in the test, not in the library (R3).** `OpticalProperties` is not modified by this
spec, and neither is `RhoWithDispValueTests`, which keeps its own class-32 gyration fixture — it tests
the gyration assembly, which is a different concern, and repointing it is out of scope.

The quartz constants are `let`-bound at the top of the new test file, elevated on the way in
(`RefractionIndex`, `EpsValue.fromRefractionIndex`, `RhoValue`, `WaveLength.nm`, `Thickness`) and
assembled through the existing `OpticalProperties.type_3_4_6_Crystal` (`Active.fs:113`). No new 3×3
algebra is introduced anywhere.

| Constant | Value | Provenance |
|---|---|---|
| λ | 632.8 nm | He-Ne; the wavelength the quartz literature is densest at |
| `n_o` | 1.542606 | Ghosh 1999 ordinary Sellmeier evaluated at 0.6328 µm |
| `n_e` | 1.551651 | Ghosh 1999 extraordinary Sellmeier evaluated at 0.6328 µm |
| `Δn` | 0.009045 | derived |
| `g₁₁` | +5.9 × 10⁻⁵ | the repository's existing value (`RhoWithDispValueTests.fs:46`) |
| `g₃₃` | −10.1 × 10⁻⁵ | the repository's existing value (`RhoWithDispValueTests.fs:47`) |

The test file's doc comment must record two things about these numbers, so that nobody later mistakes
a synthetic round trip for a literature validation:

- **`g₃₃` agrees with the literature almost exactly** (10.1 versus Arteaga's 10.06 ± 0.07), but
  **`g₁₁` is at the high end** (5.9 versus 4.8 ± 0.5; ratio −0.584 versus −0.486). Since this is a
  synthetic round trip, the repository's values *are* the ground truth by definition and the
  discrepancy cannot affect pass or fail.
- **Handedness is one overall sign.** `assembleRho` (`Active.fs:133`) applies `hand.sign` to every
  component, so the left-handed enantiomorph is the negation of the right-handed one. The test data
  states its handedness explicitly rather than baking a sign into the constants.

### 4.1 The forward-model cross-check

Along the optic axis the specific rotation is `ρ = π·|g₃₃| / (λ·n̄)`. With the constants above:

```
ρ = π · 1.01e-4 / (632.8e-9 · 1.542606) = 325.0 rad/m = 18.62 °/mm
```

The measured optical rotatory power of α-quartz at 632.8 nm is **18.8 °/mm** (188 °/cm). The
repository's own constants reproduce the literature to **within 1 %**. C1 therefore doubles as an
end-to-end validation of the whole forward chain — material assembly, the `Rho.fromIm` sign
convention, Berreman propagation, plate multiple reflections and `muellerMatrixT()` — against an
external number, and it is asserted as such (§7, T5). If T5 fails, the forward model is wrong and
every inverse result downstream is meaningless.

Companion magnitudes, for tolerance selection:

| Quantity | Value |
|---|---|
| C1 rotation (z-cut, 1.0 mm) | 18.62° |
| C3 linear retardance (x-cut, 20 µm) | 0.286 wave = 102.9° |
| Linear retardance of a 1 mm x-cut plate | 14.29 waves — the multimodality §3.3 avoids |
| Optical rotation over 20 µm along the axis | 0.372° |
| Maximum internal angle in quartz from air | 40.41° |
| Normal-incidence surface reflectance | 4.55 %; second transmitted beam ≈ 0.21 % of the first |

---

## 5. Part C — Placement and reuse

### 5.1 Where the code lives

**Tests: `Berreman/BerremanTests`,** in a new file `MuellerInverseTests.fs`, appended to the `Compile`
list after `MuellerReconstructionTests.fs`. The project already carries explicit `ProjectReference`s
to `OpticalProperties`, `Analytics`, `OpticalConstructor.Domain`, `OpticalConstructor.Optimization`
and `OpticalConstructor.Storage` (`BerremanTests.fsproj:66-83`). **No new project reference is
required for Part C.**

**Reusable pure functions: a new `OpticalConstructor.Domain/MuellerInverse.fs`,** next to
`MuellerReconstruction.fs`. `OpticalConstructor.Domain` references `Berreman` and `Analytics` but not
`OpticalProperties` (`OpticalConstructor.Domain.fsproj:204-205`). **`MuellerInverse` is therefore
material-agnostic**: it consumes `Berreman.MaterialProperties.OpticalProperties` values and a
caller-supplied system builder, and never names `OpticalProperties.Active`. Quartz-specific
construction stays in the test (R3). This keeps the module reusable for the next material and avoids
adding a project reference that would exist only to reach two constants.

### 5.2 Reuse map — call these, do not reinvent

| Need | Reuse | Location |
|---|---|---|
| Transmitted / reflected Mueller matrix | `OpticalSystemSolver.muellerMatrixT() / .muellerMatrixR()` | `Solvers.fs:315-316` |
| Multiple-reflection count | `SolverParameters` (R4: `numberOfReflections = 3`, explicit) | `Solvers.fs:15` |
| Class-32 material assembly | `OpticalProperties.type_3_4_6_Crystal` | `Active.fs:113` |
| Crystal orientation | `OpticalProperties.rotateX / rotateY / rotateZ` | `MaterialProperties.fs:204-207` |
| Plate system | `OpticalSystem.plateSystem properties description thickness` | `Standard.fs:188` |
| Inclined incident light | `IncidentLightInfo.createInclined` | `Standard.fs:42` |
| Mueller type, `M*M`, `M*S` | `MuellerMatrix` | `Fields.fs:598` |
| Build / read a Mueller by rows and by element | `Propagation.muellerOfRows` / `muellerElement` | `Propagation.fs:35 / 40` |
| Frame rotation `R(φ)` and `R(−φ)·M·R(φ)` | `Propagation.rotationMueller` / `rotateMueller` | `Propagation.fs:117 / 128` |
| Retarder / rotator reference forms | `MuellerReconstruction.retarderMueller`, `Retardance` | `MuellerReconstruction.fs:39, :21` |
| 4×4 difference metric | `MuellerReconstruction.frobeniusDiff` | `MuellerReconstruction.fs:96` |
| Nonlinear LSQ (Levenberg–Marquardt) | `AlglibAdapter.optimize` + `OptimizationRequest` | `OptimizationInterface.fs:59` |
| Bounds | `OptimizationInterface.ParameterBounds` | `OptimizationInterface.fs:34` |
| χ², σ, CI, covariance, **correlation**, residuals | `FitQuality.reportFrom` → `FitReport` | `FitQuality.fs:114, :29` |
| Residual Jacobian (relative steps) | `FitQuality.residualJacobian` | `FitQuality.fs:85` |
| Real matrix algebra for the condition number | `MathNetNumericsMath` `RealMatrix` seam | `MathNetNumericsMath.fs` |
| Element-wise 4×4 tolerance assert | the `assertMuellerEqual` loop + `allowedDiff` | `MuellerMatrixTests.fs:20`, `MatrixComparison.fs:13` |
| Proxy record + factory + mock shape | `MuellerSolverProxy` / `createMathNetSvd` and its test mock | `MuellerReconstruction.fs:164, :204` |
| Elevated primitives | `Angle`, `WaveLength`, `Thickness`, `RefractionIndex`, `RhoValue`, `EpsValue`, `IncidenceAngle` | `Geometry.fs`, `Fields.fs`, `MaterialProperties.fs` |

### 5.3 The two new proxies

**`ForwardModelProxy`** — one function field mapping a measurement configuration and a parameter set to
a Mueller matrix. The real factory closes over `OpticalSystemSolver` with
`{ numberOfReflections = 3 }`. It exists for three reasons, in order of weight:

1. `OpticalSystemSolver` **throws** on degenerate input — `getMuellerMatrix` ends in
   `failwith "Invalid combination of parameters in getMuellerMatrix!"` (`Solvers.fs:310`). Per the
   repository's error rules that exception is caught **at this boundary** and mapped to a typed
   `ForwardModelError`; F# above the boundary never handles exceptions.
2. The fit's hot loop is entirely solver calls, so a memoizing or coarsened backend is a plausible
   later swap that must not touch the inverse logic.
3. A mock backend lets the residual, scaling and reporting logic be exercised with zero solver calls —
   the same trick `ExperimentDataProxyTests` already uses.

**`NonlinearSolverProxy`** — one function field taking a residual closure, a start vector and bounds,
returning a solution or a typed error. The real factory wraps `AlglibAdapter.optimize` with
`LevenbergMarquardt`; the mock returns a canned solution. This is the direct analogue of 0042's
`MuellerSolverProxy` and it keeps ALGLIB confined to the one file allowed to see it
(`Optimization/AlglibAdapter.fs`).

Both records are `[<ReferenceEquality>]`, matching `MuellerDataProxy` and `MuellerSolverProxy`, and
both sides of both proxies carry elevated types only.

---

## 6. The decomposition

Illustrative signatures; the requirement is the granularity, the purity and the elevated types.

```
// --- elevated inputs (Domain) ---
type OpticAxisCut     = ZCut | XCut | TiltedCut of Angle
type SampleAzimuth    = SampleAzimuth of Angle
type Observable       = TransmittedMueller | ReflectedMueller
type MeasurementConfiguration =
    { cut : OpticAxisCut; thickness : Thickness; incidenceAngle : IncidenceAngle
      azimuth : SampleAzimuth; observable : Observable; waveLength : WaveLength }

type MaterialParameters =
    { ordinaryIndex : RefractionIndex; extraordinaryIndex : RefractionIndex
      g11 : RhoValue; g33 : RhoValue }

type MuellerObservation = { configuration : MeasurementConfiguration; measured : MuellerMatrix }

// --- forward-side helpers (pure) ---
gyrationProjection    : Angle -> RhoValue -> RhoValue -> RhoValue     // g11 sin^2 t + g33 cos^2 t
internalAngle         : RefractionIndex -> IncidenceAngle -> Angle    // Snell
orientForCut          : OpticAxisCut -> SampleAzimuth -> OpticalProperties -> OpticalProperties

// --- data reduction (pure) ---
normalizeMueller      : MuellerMatrix -> MuellerMatrix                // divide by m00
normalizedElements    : MuellerMatrix -> float[]                      // the 15 off-m00 elements
residualVector        : MuellerObservation list -> MuellerMatrix list -> float[]

// --- Stage A: analytic inversion (pure, closed form) ---
type ElementaryAnisotropy =
    { lb : Retardance; lbPrime : Retardance; cb : Retardance
      ld : float; ldPrime : float; cd : float }
analyticInversion     : MuellerMatrix -> Result<ElementaryAnisotropy, InversionError>
transparencyResidual  : ElementaryAnisotropy -> float                 // |LD| + |LD'| + |CD|, must be ~0
startingParameters    : ElementaryAnisotropy list -> MaterialParameters

// --- Stage B: scaling + fit ---
type ParameterScaling = { centre : MaterialParameters; scale : MaterialParameters }
toScaled              : ParameterScaling -> MaterialParameters -> float[]
ofScaled              : ParameterScaling -> float[] -> MaterialParameters
buildResidual         : ForwardModelProxy -> ParameterScaling -> MuellerObservation list -> Residual
solveInverse          : NonlinearSolverProxy -> ... -> Result<InverseSolution, InverseError>

// --- diagnostics (pure over the Jacobian) ---
jacobianColumnNorms   : float[][] -> float[]
jacobianCondition     : float[][] -> float
type InverseSolution  = { parameters : MaterialParameters; report : FitQuality.FitReport
                          conditionNumber : float; columnNorms : float[]; iterations : int }
```

---

## 7. Part C — the tests

New file `BerremanTests/MuellerInverseTests.fs`. Shape mirrors `MuellerReconstructionTests`: xUnit v3
`[<Fact>]`, back-tick names naming the acceptance criterion, and a top-of-file `///` block listing the
deliberately un-asserted future candidates. **Every test is self-contained — the forward data is
generated in-test from the library, so there is no external data, no fixture, and no skip path
anywhere in this file.**

**Pure unit tests — no solver, no optimizer:**

- **T1** `gyrationProjection` returns `g₃₃` at θ = 0 and `g₁₁` at θ = 90°, and the class-32 tensor
  assembled through `Rho.type_3_4_6_Crystal` is imaginary `diag(g₁₁, g₁₁, g₃₃)`.
- **T2** `orientForCut` on a z-cut and an x-cut produces the expected rotated `Eps` and `Rho`,
  compared against literals built independently, exactly as `RhoWithDispValueTests` already does.
- **T3** `analyticInversion` of a synthetic **pure rotator** returns `CB = 2ρd`, `LB = LB′ = 0` and all
  three dichroisms ≈ 0; of a synthetic **pure linear retarder** it returns the retardance and azimuth
  with `CB ≈ 0`. Round trip: `analyticInversion` of a matrix built from given coefficients returns
  those coefficients.
- **T4** `toScaled` and `ofScaled` round-trip exactly, and a unit step in the scaled vector moves each
  physical parameter by its intended physical amount. **This is the guard on the fixed-`1e-6`
  differentiation-step trap of §3.5 and it is not optional.**

**Forward-model tests — solver, no optimizer:**

- **T5** The C1 transmitted Mueller matrix is a pure rotator to within tolerance, and its rotation
  angle is **18.62° ± 0.2°** for a 1.0 mm z-cut plate — i.e. within 1 % of the literature 18.8 °/mm
  (§4.1).
- **T6** Every configuration's Mueller matrix is **physically realizable** — the Cloude coherency
  matrix has no negative eigenvalue — and its **depolarization index is below a pinned bound**. Note
  the wording: with `numberOfReflections = 3` (R4) the matrices are weakly depolarizing *by
  construction* (§3.5), so strict non-depolarization is the wrong assertion and would fail. The bound
  is expected to sit near the 0.2 % second-beam contamination of §4.1 and is pinned per §9. The same
  test asserts `transparencyResidual ≈ 0` at the same order.

**Inverse tests — the deliverable:**

- **T7 — the round trip.** From the noiseless C1–C4 data, starting from a perturbed guess (`n` off by
  +2 %, `g` off by +30 %), the fit recovers all four parameters. Target bands, to be pinned per §9:
  `n_o` and `n_e` to 1e-6 relative, `g₁₁` and `g₃₃` to 1e-4 relative, final χ² below 1e-16 in the
  scaled residual space.
- **T8 — identifiability is reported and sane.** The parameter **correlation** matrix has no
  off-diagonal magnitude above a pinned bound (target 0.99), and the **Jacobian condition number** is
  finite and below a pinned bound. Standard errors and confidence intervals are **not** asserted
  against the truth — on noiseless data `reducedχ²` is ~1e-30 and they are numerically meaningless
  (§3.5). The test asserts only that σ is negligible, which is the correct statement about a perfect
  fit.
- **T9 — the ablation, which is the executable answer to task item 1.** Refitting from **C1 alone**,
  `g₁₁` is not recovered: its Jacobian column norm is below a pinned floor and the condition number
  exceeds a pinned ceiling, i.e. the parameter is unobservable. Adding C3 restores both. **Asserting
  the failure is what turns the multi-configuration experiment design from an assumption into a
  result.**
- **T10 — mock-driven, no solver.** The entire Stage-B pipeline runs against a `ForwardModelProxy`
  mock returning canned matrices and a `NonlinearSolverProxy` mock returning a canned solution,
  proving the residual, scaling and reporting logic is testable without touching the solver — the same
  guarantee 0042 established for its data path.
- **T11 — typed errors, never a throw.** A degenerate configuration drives `ForwardModelProxy` into
  its `try/with` boundary and yields a typed `ForwardModelError`; a non-converging request yields a
  typed `InverseError`. No exception crosses either proxy.

**Documented in the top-of-file comment, not implemented:** noise robustness (σ = 1e-3 on the
normalized elements, at which point confidence-interval coverage becomes the right assertion, `g₃₃`
should land inside its reported 1σ and `g₁₁` should be markedly worse, mirroring the literature's
±10 %); multistart or global search; the rotating-analyzer intensity-level variant that would reuse
0042's `kron4` design-matrix machinery end to end; Lu–Chipman polar decomposition; dispersion;
biaxial or absorbing materials; fitting thickness and orientation as nuisance parameters.

**Runtime is a design constraint.** Each residual evaluation runs one `OpticalSystemSolver` per
configuration, and Levenberg–Marquardt with numerical differentiation costs `(4 + 1)` residual
evaluations per iteration. The C1–C4 set is about 40 configurations; at ~30 iterations that is roughly
6 000 solver calls. This must be **measured in the slice that lands T7**. If it is slow, the fix is to
thin the angle grids, which are oversampled for a noiseless fit — never to loosen a tolerance.

---

## 8. Part D — Retiring the external Mueller data dependency

### 8.1 What is being replaced

Two facts in `MuellerReconstructionTests.fs` are data-dependent. Both locate their data through
`tryFindOpmFinalDir()` (`:136`), which walks up from the test output directory looking for a sibling
`optics-mueller\data\raw\final\lp_lp.csv`, and both call `Assert.Skip` when it is absent (`:480`,
`:677`):

- ``Stage-1 re-derived calibration constants cross-check the section 2.2 rounded values`` (`:472`)
- ``section 7.2 end-to-end: … reconstructs M_QZ, M_LR and M_{QZ+LR} …`` (`:664`)

Between them they load seven data sets: `lp_lp`, `lp_cpl`, `cpl_lp`, `cpl_cpl_day1_main`,
`cpl_cpl_day2_corrections`, `darkness_checks`, `bullshit_checks`.

### 8.2 The committed archive is complete

`Berreman/Data/MuellerMatrix/data.zip` (62 777 bytes, tracked since commit `904d24a`) was opened and
every entry inspected. It holds 18 files:

| Entry | Data rows | Needed |
|---|---|---|
| `data/raw/final/lp_lp.csv` | 209 | yes |
| `data/raw/final/lp_cpl.csv` | 209 | yes |
| `data/raw/final/cpl_lp.csv` | 209 | yes |
| `data/raw/final/cpl_cpl_day1_main.csv` | 170 | yes |
| `data/raw/final/cpl_cpl_day2_corrections.csv` | 57 | yes |
| `data/raw/final/darkness_checks.csv` | 6 | yes |
| `data/raw/final/bullshit_checks.csv` | 2 | yes |
| `data/raw/*.csv` — 8 earlier exploratory sweeps | — | no |
| `data/shapes/*.json` — 3 beam-shape files | — | no |

The seven required row counts match spec 0042 §6 exactly (209/209/209/170/57/6/2). Every file carries
the full 16-column header with a UTF-8 BOM and ISO-8601 `captured_at` values — precisely what
`MuellerDataStore.parseMuellerCsv` already consumes, BOM stripping included. **No parser change is
required, and nothing the tests need is missing.**

### 8.3 The data seam is re-keyed on meaning, not location (R5)

The test must not know that an archive exists, where it is, or what its entries are called. The
`MuellerDataProxy` seam is therefore re-keyed from a location to an identity:

- **`MuellerDataSet`**, a new DU in `OpticalConstructor.Domain.MuellerReconstruction`, with one case
  per data set: `LpLpFamily`, `LpCplFamily`, `CplLpFamily`, `CplCplDay1`, `CplCplDay2`,
  `DarknessChecks`, `BullshitChecks`. It is pure identity and carries **no** entry name, no path and
  no archive knowledge. (It is distinct from the existing `Family` DU, which names a polarizer pair,
  not a file.)
- **`MuellerDataProxy` becomes** `{ tryLoadDataSet : MuellerDataSet -> Result<MuellerRawRow list,
  MuellerDataError> }`. The record keeps `[<ReferenceEquality>]`.
- **`MuellerDataStore.createArchiveBacked () : MuellerDataProxy`** takes **no argument**. It resolves
  the archive internally and owns the `MuellerDataSet → entry name` mapping. Location knowledge lives
  in Storage, which is where the IO boundary belongs, and never in Domain and never in the test.
- **`MuellerDataStore.createFileBacked` is deleted**, together with its private `loadThrough` and its
  `File.ReadAllText` boundary. It has exactly one consumer, the test being changed, and a path-keyed
  factory can no longer satisfy the seam. `parseMuellerCsv` is untouched — it is pure, it is the
  valuable part, and it keeps its own direct tests.
- **`MuellerDataStore.tryLoadFromArchive (archive : MuellerArchivePath) (dataSet : MuellerDataSet)`**
  is the public worker that `createArchiveBacked ()` partially applies to the resolved default. It
  exists so the archive-failure paths remain testable by pointing it at a deliberately bogus archive;
  the proxy the production tests consume still takes no location.
- **`MuellerArchivePath`** is a single-case DU with a `.value` accessor. No bare string crosses into
  a factory.

Reading is `System.IO.Compression.ZipFile.OpenRead`, then the entry's text, then the text is handed
**unchanged** to `parseMuellerCsv`. The whole IO block is under `try/with` at that boundary and every
failure becomes a typed error; no exception reaches the Domain. Opening the archive once per data set
is correct at 62 KB and no caching is introduced. `System.IO.Compression` is part of the .NET shared
framework; confirm during implementation that no `PackageReference` is needed on `net10.0`.

`MuellerDataError` gains two cases carrying real diagnostic payload, alongside the existing
`MalformedRow` and `EmptyFile`:

- `ArchiveUnreadable of archive : string * reason : string`
- `DataSetMissing of dataSet : MuellerDataSet * archive : string`

### 8.4 Failure, not skip (R6, R7)

`tryFindOpmFinalDir()` is **deleted**, both `Assert.Skip` call sites are **deleted**, and both facts
become unconditional. Any load failure fails the test, loudly, with the typed error in the message.
The data is in the repository; its absence is a defect, not an environmental condition.

**No code path anywhere may reference `optics-mueller`, probe for it, or walk up to it.** The
comments in `MuellerReconstructionTests.fs` that explain the walk-up and the skip are rewritten to
describe the committed archive. After this slice the string `optics-mueller` appears in the repository
only in spec documents and in the provenance README of §8.6.

The archive is copied to the test output so `createArchiveBacked ()` can resolve it. In
`BerremanTests.fsproj`, following the `OpticalConstructor.Tests.fsproj:233` fixtures precedent:

```xml
<Content Include="..\Data\MuellerMatrix\data.zip" Link="Data\MuellerMatrix\data.zip">
    <CopyToOutputDirectory>PreserveNewest</CopyToOutputDirectory>
</Content>
```

### 8.5 The reference results — decision on the 0042 golden file (Q7)

Spec 0042 §5.6 (review Q8) proposed running the OPM Python once, offline, to emit a full-precision
reference summary. **It does not need to be generated: it already exists.** The OPM checkout carries
the reference pipeline's own output at full precision:

- `analysis/matrix_solution/step1/summary.json` — the Stage-1 AIR calibration constants:
  `z_LP = 155.44011830109392°`, `θ_src = 39.31100224361002°`, `δ_src = 82.55344158462403°`,
  `z_CPL = 96.99613336321012°`, `δ_an = 83.66804489441891°`, plus every fit RMSE and repeat spread.
- `analysis/matrix_solution/step2_linear/summary.json` — the per-family gains, `dark_mean = 869.666`,
  the split time, all four matrices at ten decimals, the cascade metrics
  (`frobenius_norm = 0.5714178795538456`, `mean_abs_entry_diff = 0.11328628636347338`,
  `max_abs_entry_diff = 0.26806464658062423`), and per-fit `rank = 16`, `condition_number ≈ 10.05`,
  `rmse = 0.05642497 / 0.09144846 / 0.10162660` for QZ / LR / combined.

**Decision.** Both files are copied into the repository (§8.6) as reference documentation, and the
golden values are **transcribed into the test as full-precision F# literals**, replacing the
six-decimal report values currently inlined at `MuellerReconstructionTests.fs:840-857`. The test does
**not** parse JSON at run time. This is deliberate:

- the test already works exactly this way, so the change is a precision upgrade, not a new mechanism;
- it introduces no JSON schema, no parser dependency and no second fixture path, which keeps R5 true
  by construction — the test reads no file at all for its expectations;
- the JSON files remain in the repository as the auditable provenance of those literals.

The tolerance moves from the present 2e-3 band to the tightest band the observed agreement supports,
pinned per §9. Two assertions are kept, not one: the tight band against the transcribed full-precision
values, which catches numerical drift, and the existing physics-level agreement statement, which is
what the report claims. `rank = 16` continues to be asserted. `condition_number` is recorded in the
reference file but **not** asserted, because asserting it would require extending the spec-0042
`LeastSquaresSolution` contract for no benefit.

The reference files also corroborate a correction the 0042 implementation already made from the data:
the two largest cascade element differences are at 0-indexed `(3,3)` and `(3,1)` — `delta[3][3] =
−0.2680646466` and `delta[3][1] = −0.2678257477` — not the `(4,2)` and `(2,2)` the 0042 slice text
named. The existing assertion stands.

### 8.6 The provenance folder (R8)

The following files are copied into `Berreman/Data/MuellerMatrix/`, flat, keeping their original
names except where two would collide:

| Destination | Source in the OPM checkout | What it is |
|---|---|---|
| `matrix_step1_air_fit.py` | `analysis/scripts/matrix_step1_air_fit.py` | Stage 1 — AIR calibration |
| `matrix_glue.py` | `analysis/scripts/matrix_glue.py` | Stage 2 — per-row effective states and design rows |
| `matrix_fit_linear.py` | `analysis/scripts/matrix_fit_linear.py` | Stage 3 — gains, linear fit, cascade comparison |
| `cpl_cpl_analyzer.py` | `analysis/scripts/cpl_cpl_analyzer.py` | the experiment-label grammar (`parse_experiment_name`) |
| `reference_step1_summary.json` | `analysis/matrix_solution/step1/summary.json` | Stage-1 constants at full precision |
| `reference_step2_linear_summary.json` | `analysis/matrix_solution/step2_linear/summary.json` | matrices, gains and metrics at full precision |
| `README.md` | — | the provenance record below |

`README.md` in that folder must record, at minimum:

- **Source repository** `https://github.com/NihilismVoid/optics-mueller.git`, **branch** `main`,
  **commit** `2f1a35b70f877e41fa9b576fadd138d7dc80b858`, copied **2026-08-02**.
- A one-line description of each copied file, as in the table above.
- The **inventory of `data.zip`** from §8.2, including which entries the tests consume and which are
  earlier exploratory sweeps kept for completeness.
- A statement that **no project builds, references or executes the Python** — the scripts are the
  authoritative algorithm reference that spec 0042 §3 ports, retained so the port can be audited after
  the external checkout is gone, and BNM never invokes Python at build or test time.
- Pointers to `specs/0042/.manual/MuellerMatrix_final.pdf` (the report) and to spec 0042 §3, which
  maps each script onto the ported F# stage.

### 8.7 Line-ending policy

`.gitattributes` gains two rules, matching the file's existing single-source-of-truth structure:

- `*.py text eol=lf` in the scripts block, next to `*.sh`.
- `*.zip binary` in the "Binary (never normalized)" block. `data.zip` is currently protected only by
  git's `text=auto` binary auto-detection; it reads back correctly today, but the repository's stated
  policy is an explicit rule per type, and an explicit rule removes any possibility that a future
  `git add --renormalize` corrupts the archive.

The copied `.json` files are already covered by the existing `*.json text eol=lf` rule.

### 8.8 Consequences

- **The `unit-tests` gate gets stronger.** Its `count_at_least` capture is `"Passed:\s*(\d+)"`, and a
  skipped xUnit test is not counted as passed. Today the passed count depends on whether the machine
  happens to have a sibling checkout. Afterwards it is deterministic and two higher on every machine.
  The baseline is re-captured once; that is a baseline bump, not a regression.
- The `Xunit.SkippableFact` package reference stays — `SolverTests.fs:942-948` still uses it.
- Files touched by Part D: `MuellerReconstruction.fs` (the `MuellerDataSet` DU, the re-keyed proxy,
  two error cases), `MuellerDataStore.fs` (the archive factory, deletion of the file-backed one),
  `MuellerReconstructionTests.fs` (the mock re-key, the deleted walk-up and skips, the transcribed
  golden literals), `BerremanTests.fsproj`, `.gitattributes`, and the seven new files in
  `Berreman/Data/MuellerMatrix/`. Nothing else.

---

## 9. Tolerance protocol (R9)

The 0042 approach, applied without exception. Every numeric band in §7 and §8.5 is a **target**, not a
literal. For each one:

1. Implement, run, and **record the observed agreement** in the slice's implementation log.
2. Pin the assertion **one order of magnitude looser than observed**, and write the observed value
   into the test's comment next to the assertion so the next reader knows the real margin.
3. A band is never widened to make a failing test pass. If the observed agreement is worse than the
   target by more than an order of magnitude, that is a defect to investigate, not a tolerance to
   relax. For the §8.5 golden comparison specifically: the F# re-derives every constant independently
   through a different SVD implementation, so agreement at 1e-12 is expected and agreement worse than
   **1e-6** indicates a procedural difference from the reference pipeline that must be found and
   fixed.

---

## 10. Phasing

| # | Slice | Contents | Gate |
|---|---|---|---|
| 1 | **Provenance** | §8.6 — copy the four scripts and two reference result files into `Berreman/Data/MuellerMatrix/`, write `README.md`, add the two `.gitattributes` rules. No code. | build |
| 2 | **Archive-backed data** | §8.3, §8.4 — `MuellerDataSet`, the re-keyed proxy, the two error cases, `MuellerArchivePath`, `createArchiveBacked` / `tryLoadFromArchive`, deletion of `createFileBacked`, the `fsproj` content copy, deletion of the walk-up and both skips, mock re-key. | build + unit-tests (re-baseline) |
| 3 | **Golden precision** | §8.5 — transcribe the full-precision literals, tighten the band per §9, keep `rank = 16`. | build + unit-tests |
| 4 | **Quartz + forward** | §4 — quartz test data, the configuration types, `orientForCut`, `gyrationProjection`, `internalAngle`; T1, T2, T5. | build + unit-tests |
| 5 | **Inverse primitives** | `MuellerInverse.fs` — normalization, `residualVector`, `ParameterScaling`; T4. Pure, no solver. | build + unit-tests |
| 6 | **Stage A** | `analyticInversion`, `transparencyResidual`, `startingParameters`, realizability and depolarization-index helpers; T3, T6. | build + unit-tests |
| 7 | **Proxies** | `ForwardModelProxy` and `NonlinearSolverProxy` with real and mock factories; T10, T11. | build + unit-tests |
| 8 | **Stage B and acceptance** | Wire the composition root, fit C1–C4, measure runtime, pin the bands; T7, T8, T9. | build + unit-tests |

Slices 1–3 are Part D and are independent of Part A/B/C. They land first: they have concrete,
already-verified targets, and they remove the external dependency before any new physics arrives.

---

## 11. Decision register

| Ref | Decision |
|---|---|
| Q1 | The full C1–C4 set (§3.4). C2 and C4 keep both transmission and reflection. |
| Q2 | Thickness is **known**. The fit vector is exactly `(n_o, n_e, g₁₁, g₃₃)`. Sample thickness remains a *design* variable: the birefringent samples are 20 µm so the χ² surface stays unimodal (§3.3). |
| Q3 | Quartz is **test data**, defined in `MuellerInverseTests.fs`. `OpticalProperties` is not modified; `RhoWithDispValueTests` keeps its own fixture and is not repointed. |
| Q4 | `numberOfReflections = 3`, stated explicitly. Consequence, carried through §3.5 and T6: the matrices are weakly depolarizing by construction, so Stage A is a starting-value generator and T6 asserts realizability plus a bounded depolarization index rather than strict non-depolarization. |
| Q5 | The seam is re-keyed from `DataFilePath` to the semantic `MuellerDataSet`; `createArchiveBacked ()` takes no argument and owns archive, location and entry naming. The test knows none of it. |
| Q6 | Missing or unreadable data is a **failure**. All skip paths deleted. Two new typed error cases carry the diagnostic payload. |
| Q7 | No Python is run and no new artifact is generated: the reference results already exist in the OPM checkout and are copied in (§8.6). The golden values are **transcribed into the test as full-precision literals**; the JSON files are committed as provenance. `optics-mueller` is never referenced by code again. |
| Q8 | Four scripts and two reference result files copied into `Berreman/Data/MuellerMatrix/`, plus a `README.md` recording repository, branch `main`, commit `2f1a35b7`, per-file descriptions, the `data.zip` inventory, and the statement that nothing builds or executes the Python. |
| Q9 | The 0042 protocol: measure, pin one order looser than observed, record the observed value, never widen to pass (§9). |

---

## 12. Residual items, settled by measurement during implementation

None of these is a question for the operator; each is resolved by running the code and recording the
result under the §9 protocol.

- The exact pinned values for every band in §7 and §8.5.
- The depolarization-index bound in T6, expected near the 0.2 % second-beam contamination of §4.1.
- The Jacobian condition-number ceiling and column-norm floor used by T8 and T9.
- Wall-clock cost of the T7 fit, measured in slice 8; if it is slow the angle grids are thinned, never
  the tolerances.
- Confirmation that `System.IO.Compression.ZipFile` needs no `PackageReference` on `net10.0`.

## 13. Open questions

None. Every point raised in `002-active-anisotropic-inverse-problem.md` §9 has been answered in
`003-answers.txt` or decided above, and the spec is implementable as written.
