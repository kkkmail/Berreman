# Spec 0044, manual task 016 part 2 — the absorbing biaxial (triclinic) inverse problem

Record for the second of the three tasks in `016-biaxial-absorbing-inverse.txt`: repeat the task-012
exercise for a **biaxial, gyrotropic, ABSORBING** crystal.

New test class `Berreman/BerremanTests/AbsorbingBiaxialInverseTests.fs` (7 facts, 177 s, all green).
New `AbsorbingTriclinicParameters` in `OpticalConstructor.Domain/MuellerInverse.fs`; two new residual
builders in `InverseFitHarness`.

---

## Fifteen parameters, not twenty-one — and the reason is a measurement

The design started at **21**: three principal indices, six components of ε″, six of the gyration real
part g′ and six of its imaginary part g″. Textbook optics says an absorbing gyrotropic crystal has a
**complex** gyration tensor, with g′ driving circular birefringence (optical rotation) and g″ driving
circular dichroism.

**The engine cannot express g″, and the suite measures why.** The engine's ρ is the bi-anisotropic
magnetoelectric tensor and its transparent gyration is purely imaginary (every crystal-class builder in
`Active.fs` goes through `Rho.fromIm`). Mapping g″ onto the *real* part of ρ is the natural reading. It
does not work:

| probe (optic-axis cut, 100 µm) | `cb` (circular birefringence) | `cd` (circular dichroism) |
|---|---|---|
| **imaginary** ρ, magnitude 1× | **5.744°** | 1.8e-15 |
| **real** ρ, magnitude 1× | 2.4e-15 | 1.3e-12 |
| **real** ρ, magnitude 1000× | 3.1e-14 | **−2.24e-5** |

A thousandfold increase multiplies the circular dichroism by **1.7 × 10⁷**, not by 10³. That is not a
first-order coupling. At realistic gyration magnitudes (~1e-5) a real ρ produces ~1e-13 of circular
dichroism — nothing — and six numerically dead columns would make the fit singular.

So the parameter set is **3 + 6 + 6 = 15**, and the finding is asserted as the first inverse fact in the
file rather than buried in a comment. This is the same class of result as finding F1 in the original
implementation log (the engine's ρ is not the crystallographic gyration tensor): a property of *this*
constitutive model, worth pinning so nobody re-derives it from theory and gets it wrong.

Absorption itself is unaffected — it enters through ε″, is first-order, and is recovered well.

---

## The physics that had to be right: ε′ and ε″ do not share principal axes

The task flagged this explicitly, and it is the reason ε″ carries **six** components rather than three.
Neumann's principle constrains ε′ and ε″ independently and identically:

| system | ε′ and ε″ principal frames |
|---|---|
| orthorhombic and higher | both locked to the crystallographic axes — **coincident** |
| monoclinic | one axis shared, the other two free to rotate (with wavelength) |
| **triclinic** | **nothing shared — no frame diagonalizes both** |

Parameterizing ε″ by its six components **in the ε′ frame** captures that exactly, and avoids the gauge
degeneracy, periodicity and ordering ambiguity that three Euler angles for a second principal frame would
drag into a nonlinear fit. It also makes the property impossible to lose by accident: setting the ε″
off-diagonals to zero would put the frames back together and quietly reduce the problem to an
orthorhombic-like one while still looking general.

Measured: the three principal absorption axes sit **42.20°, 42.77° and 26.51°** from the crystal axes,
and ε″ is positive definite (a passive absorber, not an amplifier in some polarization). Both asserted.

---

## The measurement set had to change — absolute throughput

The decisive fact is an information count, not a geometric one:

| | independent real parameters |
|---|---|
| a general Jones matrix | 8 |
| → Mueller matrix (absolute phase lost) | 7 |
| → **normalized** Mueller matrix (absolute intensity lost) | **6** |

The one quantity `m₀₀` normalization discards is the **isotropic absorption** — the attenuation common to
every polarization state. For a transparent sample that costs nothing (it removes source brightness and
detector gain). For an absorbing sample it removes a real material parameter, and no number of extra
cuts, azimuths or angles puts it back.

So this suite fits **absolute** Mueller data: the fifteen normalized elements per observation *plus* one
log-throughput term `ln(m₀₀ model) − ln(m₀₀ measured)`. Log because transmittance is multiplicative,
spans decades, and the log form is dimensionless like the normalized-element residuals beside it.
(`InverseFitHarness.throughputResidualFor` / `fitAbsorbingObservations`.)

### And the measured answer is softer than the argument

| direction | normalized residual | absolute residual |
|---|---|---|
| **isotropic** absorption (raise ε″₁₁, ε″₂₂, ε″₃₃ together) | 0.0677 | **0.2086** |
| **anisotropic** absorption (raise ε″₁₁, lower ε″₂₂) | 0.2051 | — |

Taken literally the information count predicts a **hard null** under normalization. Measured, normalized
data is **3.0× less sensitive** to the isotropic direction than to an anisotropic one of the same size,
and adding absolute throughput restores a factor of **3.1**. Weak, not dead.

The reason is physical and is the same second-order channel the uniaxial suite records for its ordinary
index: **a plate is not a single pass.** Its Fresnel coefficients and its internally-reflected beams both
depend on the absorption level, and their relative weights survive normalization. A single-pass sample
would show the hard null; a plate leaks the isotropic level back in through its own geometry.

Note this had to be measured as a **directional** sensitivity (`‖J·v‖`), not a Jacobian column norm: the
suppressed direction is a *combination* of three parameters, and every one of the three is individually
alive — `e11` alone changes the linear dichroism and normalized data sees that perfectly well. Only the
sum is suppressed. `InverseFitHarness.directionalSensitivity` and `sumWeights` were added for this.

---

## The measurement set

Inherited from task 016 part 1: **100 µm plates** (the LBO vendor minimum), **single wavelength**
632.8 nm, 44 configurations.

| Group | Configurations | Purpose |
|---|---|---|
| **A1** | 2 optic-axis cuts × 4 azimuths, normal, T = **8** | the gyration anchor, near-zero linear retardance |
| **A2** | optic-axis cut × {20°, 40°, 60°} × {0°, 90°} × {T, R} = **12** | tilter geometry — retardance grows continuously from zero |
| **A3** | 3 principal cuts × {0°, 45°} × {0°, 90°} × {T, R} = **24** | the gyration off-diagonals, and the **linear dichroism** of each transverse plane, which is what makes ε″ identifiable component by component |

A caveat the transparent suite does not have: in an absorbing crystal the two optic axes **split into
four singular optical axes**, so the transparent twin's binormal is only approximately retardance-free
here. At ε″/ε′ ~ 1e-3 the splitting is far below the alignment tolerance the plate already carries, but
the cut is no longer exactly an optic-axis cut.

Measured throughput across the set: **24 % to 47 %** — squarely the "near the absorption edge but still
comfortably measurable in transmission" regime (ε″ ~ 1e-3 ⇒ k ≈ 3e-4 ⇒ α ≈ 80 cm⁻¹).

---

## Results

### All fifteen constants recovered

| Quantity | Observed | Pinned band |
|---|---|---|
| `n₁` / `n₂` / `n₃` | 6.49e-15 / 6.24e-15 / 6.18e-15 | < 1e-8 |
| `e11` / `e22` / `e33` | 5.81e-13 / 2.58e-13 / 7.71e-15 | < 1e-8 |
| `e23` / `e13` / `e12` | 7.22e-13 / 7.21e-13 / 1.02e-14 | < 1e-8 |
| `g11` / `g22` / `g33` | 1.12e-10 / 9.29e-11 / 2.57e-10 | < 1e-8 |
| `g23` / `g13` / `g12` | 1.39e-10 / 9.94e-13 / 3.47e-10 | < 1e-8 |
| final χ² | 6.36e-23 | < 1e-18 |
| iterations | **22** | > 0 |

**Adding absorption made the problem no harder.** 22 iterations against the transparent nine-parameter
fit's 28 on the same plate and cuts, and a Jacobian condition number of **7677** against its **7954** —
statistically indistinguishable. Six extra unknowns cost essentially nothing, because the linear
dichroism they carry is a channel the transparent problem simply was not using.

### Identifiability — the absorption is the best-determined part

Jacobian column norms at the truth:

| | `n₁` | `n₂` | `n₃` | ε″ (six) | `g11` | `g22` | `g33` | `g23` | `g13` | `g12` |
|---|---|---|---|---|---|---|---|---|---|---|
| \|J\| | 4.45 | 6.05 | 5.38 | **0.186 – 0.241** | 0.0141 | 0.0392 | 0.0254 | 0.00145 | 0.0374 | 0.00185 |

All six absorption columns land within 30 % of each other at 0.19–0.24 — an **order of magnitude
stronger than any gyration column**. Linear dichroism is a large, direct, well-conditioned signal;
optical activity remains the hard part, exactly as in the transparent case.

### Supporting facts

- **The 15-coordinate scaled space round-trips, and a unit step moves exactly one parameter** — all 225
  (axis, parameter) pairs. Fifteen hand-written axes, each with a `read` and a `write`; a `read` pointed
  at the wrong field or two axes sharing a `write` would give a mapping that is smooth, total and wrong,
  and a fit over a wrong mapping converges confidently to a wrong answer instead of failing.
- **The sample attenuates and stays physically realizable** — throughput 24–47 %, no negative Cloude
  eigenvalue anywhere. For an absorbing medium this is sharper than for a transparent one: a sign error
  in ε″ produces *gain*, and gain is exactly what a Cloude eigenvalue catches.

---

## Extraction

Added to `InverseFitHarness` (no copy/paste into the new suite):

| Addition | Purpose |
|---|---|
| `throughputResidualFor` | the 15 normalized elements + one log-throughput term per observation |
| `fitWithResidual` | the runner, now taking a pre-built residual; `fitObservations` and `fitAbsorbingObservations` are two-line wrappers over it |
| `fitAbsorbingObservations` | the absolute-data runner |
| `directionalSensitivity` | `‖J·v‖` — "is this *combination* observable?", which a column norm cannot answer |
| `sumWeights` | build a direction from axis NAMES rather than indices typed at a call site |

`AbsorbingTriclinicParameters` and its 15 axes live in the Domain beside `TriclinicParameters`, so the
generic `ParameterAxis` machinery drives this suite exactly as it drives the others.

Per the task's instruction, only the new class was run; the existing suites were compiled, not executed.
