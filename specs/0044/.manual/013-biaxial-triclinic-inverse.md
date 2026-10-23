# Spec 0044, manual task 012 — the biaxial (triclinic) inverse problem

Record for `012-biaxial-inverse.txt`. New test class
`Berreman/BerremanTests/BiaxialInverseTests.fs`, new shared module
`Berreman/BerremanTests/InverseFitHarness.fs`, and a generalization of
`OpticalConstructor.Domain/MuellerInverse.fs` that both suites now run on.

---

## Why TRICLINIC (class 1)

The task asked for "the most complex possible variant with as many optical parameters as possible". That
is class 1, and the count is forced by symmetry:

| Crystal class | Optical character | Independent indices | Independent gyration components | Unknowns |
|---|---|---|---|---|
| 3 / 32 / 4 / 422 / 6 / 622 | uniaxial | 2 | 2 (`g₁₁`, `g₃₃`) | **4** — `MuellerInverseTests` |
| 222 (orthorhombic) | biaxial | 3 | 3 (diagonal) | 6 |
| mm2 | biaxial | 3 | 1 (`g₁₂`) | 4 |
| 2 (monoclinic) | biaxial | 3 | 4 | 7 |
| **1 (triclinic)** | **biaxial** | **3** | **6 (full symmetric)** | **9** — this suite |

Class 1 has no symmetry element beyond the identity, so nothing constrains either tensor: ε gives three
distinct principal refractive indices (every triclinic crystal is biaxial) and the gyration tensor, being
a symmetric second-rank tensor, gives all six of its independent components. Every other optically active
class is a constrained special case — monoclinic 2 zeroes `g₁₂` and `g₂₃`, orthorhombic 222 zeroes all
three off-diagonals, and the uniaxial classes additionally force `g₁₁ = g₂₂` and `n₁ = n₂`.

The engine already had the builder: `OpticalProperties/Active.fs` ships
`Rho.type_1_Crystal g₁₁ g₂₂ g₃₃ g₂₃ g₁₃ g₁₂` alongside the 222, 2 and m forms, and
`Eps.fromRefractionIndex (n₁, n₂, n₃)` supplies the biaxial permittivity. Nothing in
`OpticalProperties` had to change.

---

## The ground truth, and exactly how much of it is real

**The three principal refractive indices are lithium triborate's (LiB₃O₅, LBO) at 632.8 nm**, evaluated
from the standard Sellmeier set

```
n_x² = 2.4542 + 0.0113/(λ² − 0.0114) − 0.0139 λ²
n_y² = 2.5390 + 0.0128/(λ² − 0.0119) − 0.0185 λ²      (λ in µm)
n_z² = 2.5865 + 0.0131/(λ² − 0.0122) − 0.0186 λ²
```

giving `n₁ = 1.574064836`, `n₂ = 1.601416913`, `n₃ = 1.616414016`. The three birefringences are therefore
`n₂ − n₁ = 0.027352`, `n₃ − n₂ = 0.014997`, `n₃ − n₁ = 0.042349` — three different numbers, which is what
"biaxial" means and what a uniaxial crystal cannot present.

**The gyration magnitude is anchored on a real measurement of that same crystal**: Shopa, Ftomyn & Shopa
measured `g₁₂ = 4.31 × 10⁻⁵` and an optical rotatory power of `7.06 ° mm⁻¹` along an optic axis, by
dual-wavelength polarimetry at 633 and 661 nm (*J. Appl. Cryst.* **56**, 2023). `g₁₁ = 4.31e-5` here is
that number.

**The other five components are synthetic, and this is stated in the test itself, loudly.** LBO is class
mm2 and non-enantiomorphous: its symmetry allows exactly the one component that was measured. No real
crystal has a published complete six-component gyration tensor — the most complete measurements in the
literature are the four-component monoclinic ones from Glazer's group's `tilter` method (tartaric acid:
ρ°₁₁ = 79(7), ρ°₂₂ = 90(13), ρ°₃₃ = −70(4), ρ°₂₃ = −18(8) ° mm⁻¹) — so the remaining five are chosen of
the same order as the measured one, with distinct magnitudes and mixed signs so that no two of the nine
unknowns are degenerate:

| | `g₁₁` | `g₂₂` | `g₃₃` | `g₂₃` | `g₁₃` | `g₁₂` |
|---|---|---|---|---|---|---|
| value (×10⁻⁵) | **4.31** | 6.90 | −2.85 | 1.72 | −3.64 | 2.46 |
| provenance | measured (LBO) | synthetic | synthetic | synthetic | synthetic | synthetic |

This changes nothing about what the facts establish. The forward data is generated FROM these values, so
they are the ground truth by definition and these facts measure the INVERSE MACHINERY, not the physics —
exactly as the quartz facts in `MuellerInverseTests` do, and for the same stated reason.

Sources:

- [Refractive index database — LiB₃O₅ (LBO), Sellmeier data](https://refractiveindex.info/?shelf=main&book=LiB3O5&page=Kato-alpha)
- [Shopa, Ftomyn & Shopa, *Optical rotation in the lithium triborate nonlinear crystal*, J. Appl. Cryst. 56 (2023)](https://journals.iucr.org/j/issues/2023/02/00/gj5295/)
- [Glazer group, *Determination of optical activity in monoclinic crystals of tartaric acid using the 'tilter' method*, J. Phys.: Condens. Matter 9 (1997)](https://iopscience.iop.org/article/10.1088/0953-8984/9/49/004)
- [Manifestation of optical activity in crystals of different symmetry classes](https://www.researchgate.net/publication/226176872_Manifestation_of_optical_activity_in_crystals_of_different_symmetry_classes) — the class-by-class gyration-tensor component count
- [Kato, *Temperature dispersion of refractive indices in β-BaB₂O₄ and LiB₃O₅*](https://www.researchgate.net/publication/224484675_Temperature_dispersion_of_refractive_indices_in_b-BaB2O4_and_LiB3O5_crystals_for_nonlinear_optical_devices) — the Sellmeier coefficients above

---

## What was generalized, and what it cost

The task said to extract or generalize rather than copy. Three things moved.

### 1. `MuellerInverse` is now generic in its parameter set

The Stage-B seam used to be hard-wired to a four-field uniaxial record. It now runs on a list of axes the
material supplies:

```fsharp
type ParameterAxis<'Parameters> =
    {
        name : ParameterName
        read : 'Parameters -> double
        write : double -> 'Parameters -> 'Parameters
    }
```

`ParameterScaling<'Parameters>` carries that list, and `toScaled` / `ofScaled` became a map and a fold
over it instead of four hand-written lines. `ForwardModelProxy<'Parameters>`, `createBerremanForward` and
`forwardModels` became generic the same way. `MaterialParameters` was renamed `UniaxialParameters` (it was
never general) and gained a `static member axes`; `TriclinicParameters` was added beside it with nine.

`read` and `write` speak plain `double` deliberately, and that is the one place in the module where a
primitive is allowed: the parameters of a single material carry DIFFERENT elevated types
(`RefractionIndex` alongside `RhoValue`), so a double is the only thing they have in common, and this
record is precisely the boundary at which the optimizer's dimensionless vector is entitled to see them.

Blast radius outside the two test files: **none**. `MaterialParameters` and friends were named only by
`BerremanTests`; `MuellerInverseSolver.fs` mentions `ParameterScaling` in a doc comment and nothing else.

### 2. `SampleCut`, and the new `YCut`

`OpticAxisCut` was uniaxial vocabulary — a biaxial crystal has no single optic axis — and it was missing a
case. It is now `SampleCut` with `ZCut | XCut | YCut | TiltedCut`. `YCut` rotates the crystal 90° about x,
so a third crystal axis ends up along the surface normal. For a uniaxial crystal it would duplicate
`XCut`; for a biaxial one it does not, and the ablation below shows what it buys.

### 3. `InverseFitHarness` — the driver both suites share

`observeWith`, `residualFor`, `fitObservations`, `scalingAround`, `recoveryErrors`, `describeErrors`,
`describeColumnNorms`, `configurationOf`, `assertMuellerEqual`, plus the `RelativeError`, `SearchBox` and
`InverseFit` types. `MuellerInverseTests` and `BiaxialInverseTests` now differ in their MATERIAL and their
MEASUREMENT SET and in nothing else — same forward evaluation, same residual, same Levenberg–Marquardt
call, same acceptance arithmetic.

---

## The measurement set

**Thickness first, because it decides whether the fit is possible at all.** Linear retardance enters
through cos and sin of `2π Δn d / λ`, so a plate carrying many waves gives a residual surface that
oscillates into local minima roughly half a fringe apart. The largest birefringence here is 0.042349 —
five times quartz's — so the plate has to be five times thinner than the uniaxial suite's 20 µm to stay in
the same sub-wave regime. **4 µm is 0.268 wave**, against that suite's 0.286.

Unlike the uniaxial case there is no retardance-free principal cut to spend thickness on: a biaxial
crystal has linear birefringence along every principal axis, and the only directions free of it are the
two optic axes, which are oblique.

| Group | Configurations | What it is for |
|---|---|---|
| **B1** | 3 principal cuts × 4 azimuths, normal incidence, transmission = **12** | each cut contributes its own transverse pair of indices and its own gyration diagonal components |
| **B2** | 3 cuts × {25°, 55°} × {0°, 90°} × {T, R} = **24** | absolute index leverage, and the longitudinal-field coupling that makes the gyration OFF-diagonals observable |
| **B3** | x-cut at 72° × {0°, 180°} × {T, R} = **4** | the optic-axis approach |
| | **40 total → 600 residual entries against 9 unknowns** | |

**The optic axes are reachable, and only from one cut.** A biaxial crystal has two directions along which
the linear birefringence vanishes identically, so a wave down one of them is a pure circular retarder —
the biaxial analogue of the uniaxial C1 configuration. They lie in the plane of the largest and smallest
index at `tan²V = (n₂²−n₁²)/(n₃²−n₂²)` from the largest-index axis:

- **53.299°** from the n₃ axis, i.e. **36.701°** from the n₁ axis;
- Snell caps a plate's internal angle at `arcsin(1/n₂)` = **38.642°**.

So an **x-cut plate can just reach an optic axis, with 1.9° to spare, and a z-cut plate never can**. That
is asserted as a fact, and it is why B3 sits on the x-cut at 72° external incidence.

---

## Results

All nine facts pass. `BiaxialInverseTests` runs in **68 s**.

### The deliverable — all nine constants recovered from a perturbed start

Start guess: every gyration component 25–35 % wrong in **alternating** directions (so the start is not a
uniform rescaling of the truth, which a fit could exploit), the three indices +0.20 %, +0.10 %, +0.15 %.
Those index perturbations look small and are not: they move `n₂ − n₁` by −5.7 % and `n₃ − n₂` by +5.5 %,
and birefringence, not absolute index, is what the retardance depends on.

| Quantity | Observed | Pinned band |
|---|---|---|
| relative error, `n₁` | 2.12e-15 | < 1e-10 |
| relative error, `n₂` | 2.08e-15 | < 1e-10 |
| relative error, `n₃` | 2.20e-15 | < 1e-10 |
| relative error, `g₁₁` | 4.09e-15 | < 1e-10 |
| relative error, `g₂₂` | 8.48e-13 | < 1e-10 |
| relative error, `g₃₃` | 1.64e-13 | < 1e-10 |
| relative error, `g₂₃` | 9.84e-13 | < 1e-10 |
| relative error, `g₁₃` | 4.63e-12 | < 1e-10 |
| relative error, `g₁₂` | 2.44e-12 | < 1e-10 |
| final χ² | 1.25e-25 | < 1e-21 |
| worst normalized element residual at the solution | 1.01e-13 | < 1e-11 |
| iterations | **12** | > 0 |

**B1 — nine unknowns are not harder than four.** Twelve iterations, against the thirteen the
four-parameter uniaxial fit takes from its own perturbed start, and a Jacobian condition number of
**382.5** against that suite's 436. What costs a fit its convergence is the CONDITIONING of the
measurement set, not the count of unknowns. The spread in the recovered precision (indices at ~2e-15,
gyration components at 4e-15 … 5e-12) is the price of the gyration columns being ~100× weaker than the
index columns, which is exactly what the identifiability fact measures.

### Identifiability — Jacobian column norms at the truth

| | `n₁` | `n₂` | `n₃` | `g₁₁` | `g₂₂` | `g₃₃` | `g₂₃` | `g₁₃` | `g₁₂` |
|---|---|---|---|---|---|---|---|---|---|
| full set | 0.2511 | 0.2701 | 0.2568 | 2.36e-3 | 2.64e-3 | 2.39e-3 | 9.78e-4 | 1.48e-3 | 1.03e-3 |

Condition number **382.5** (pinned < 1e3). The six gyration components are within a factor of **2.70** of
one another (pinned < 3), which is what makes the nine-parameter fit well posed rather than merely
soluble.

### The ablation — two independent design levers

This is the result the task's "what experiments are needed" question actually has, and it was **measured,
not predicted**: the first draft of this fact guessed wrongly and the guess is now recorded in the test as
the reason the fact exists.

| Measurement set | `n₁` | `n₂` | `n₃` | `g₁₁` | `g₂₂` | `g₃₃` | `g₂₃` | `g₁₃` | `g₁₂` |
|---|---|---|---|---|---|---|---|---|---|
| z-cut, normal only | 0.1115 | 0.1116 | **1.7e-6** | 1.08e-3 | 1.06e-3 | **1.3e-7** | **3.9e-6** | **2.4e-7** | **5.1e-6** |
| + x-cut, normal | 0.1115 | 0.1583 | 0.1124 | 1.08e-3 | 1.53e-3 | 1.10e-3 | **1.5e-5** | **1.3e-6** | **5.3e-6** |
| + y-cut, normal (all three cuts) | 0.1574 | 0.1583 | 0.1580 | 1.48e-3 | 1.53e-3 | 1.47e-3 | **1.5e-5** | **5.4e-6** | **5.9e-6** |
| full set (adds oblique + reflection) | 0.2511 | 0.2701 | 0.2568 | 2.36e-3 | 2.64e-3 | 2.39e-3 | 9.78e-4 | 1.48e-3 | 1.03e-3 |

Bold = at the differencing floor, i.e. structurally unobservable. The separation between dead columns
(1.3e-7 … 5.9e-6) and live ones (≥ 9.8e-4) is three orders wide, so the fact's threshold sits at 1e-4, an
order clear of both.

**B2 — THE CUTS BUY THE DIAGONAL.** A wave sees only the components transverse to its own propagation
direction, so a z-cut plate at normal incidence is blind to `n₃` and `g₃₃` entirely — five of the nine
unknowns have nothing to act on. Each further principal cut brings one more principal index and one more
gyration diagonal component into view. Three cuts, three indices, three diagonals.

**B3 — AND ONLY THE TILT BUYS THE OFF-DIAGONAL.** This is the part that is not obvious. All three cuts at
normal incidence leave every off-diagonal gyration component two orders of magnitude below the diagonal
ones — *including `g₁₂`, which IS transverse on a z-cut and might therefore be expected to show*. It is
oblique incidence that makes them observable, lifting them by 63× (`g₂₃`), 274× (`g₁₃`) and 177× (`g₁₂`)
to within a factor of three of the diagonals. The reason is the engine's constitutive convention rather
than geometry: its ρ couples through the wave's LONGITUDINAL field component, which only exists
off-normal.

The practical consequence for anyone designing this experiment: **cutting more plates does not substitute
for tilting them, and tilting does not substitute for cutting.** Both are required.

### Supporting facts

- **The triclinic parameter set is a strict generalization of the uniaxial one.** Setting `n₁ = n₂`,
  `g₁₁ = g₂₂` and every off-diagonal to zero turns the class-1 material into exactly the uniaxial
  gyrotropic material `MuellerInverseTests` fits, and the two builders then produce the SAME Mueller
  matrix, element for element, across five probe configurations spanning all three cuts, normal and
  oblique, transmission and reflection. This is the bridge to the suite that is already trusted: if
  `type_1_Crystal` laid its components out differently from `type_3_4_6_Crystal`, or the generalization
  had an index transposed, this fails and every other fact in the file becomes untrustworthy at the same
  moment.

- **`orientForCut` carries each principal axis onto the normal, gyration tensor and all.** The
  permittivity diagonal permutes as `(e₁,e₂,e₃) → (e₁,e₂,e₃) / (e₃,e₂,e₁) / (e₁,e₃,e₂)` for Z/X/Y, so each
  cut puts a different index in the longitudinal slot. The gyration tensor is checked as a WHOLE — it
  stays symmetric, and its trace and Frobenius norm are invariant under all three rotations. A
  diagonal-only check cannot see a transposed or half-applied rotation; a full symmetric tensor cannot
  hide one.

- **The nine-coordinate scaled space round-trips, and a unit step moves exactly one parameter.** This
  matters more than its uniaxial counterpart did: the uniaxial `toScaled`/`ofScaled` were four hand-written
  lines where an error would be visible on the page, and these are a fold over an axis list, where an
  off-by-one, a `read` pointed at the wrong field, or two axes sharing a `write` would all produce a
  mapping that is smooth, total and wrong — and a fit over a wrong mapping converges confidently to a
  wrong answer instead of failing. Asserted for all 81 (axis, parameter) pairs.

- **Realizability and depolarization.** Worst transmission depolarization 0.0273 (pinned < 0.05), worst
  overall 0.3919 (pinned < 0.5), no negative Cloude eigenvalue anywhere. Both are within a few per cent of
  what the uniaxial suite measures for quartz (0.0295 and 0.416) — the expected answer, since the
  depolarization comes from summing the plate's emerging beams and that is governed by the surface
  reflectance, which the two materials share to within a few per cent of index.

---

## Runtime

| | Before | After |
|---|---|---|
| `BiaxialInverseTests` | — | **68 s**, 9 tests |
| `MuellerInverseTests` | ~600 s, 20 tests | unchanged |
| whole `BerremanTests` assembly | 508 s, 179 tests | 559 s, **188 total / 0 failed / 5 skipped** |

The nine-parameter suite is cheap — one fit at 40 configurations and 12 iterations, plus five Jacobians
for the identifiability and ablation facts. It is an eighth of what the uniaxial suite's noisy ensemble
costs, because there is no ensemble here: manual task 012 asked for the non-noise test, and noise on a
nine-parameter biaxial fit is the obvious next slice.
