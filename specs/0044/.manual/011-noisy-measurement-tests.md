# Spec 0044, manual task 010 — noisy-measurement tests

Record for `010-extend-mueller-inverse-tests.txt`. Everything below is in
`Berreman/BerremanTests/MuellerInverseTests.fs`; nothing outside that file changed.

---

## What "noise" means here

Two error sources are modelled, because they are the two an angle-resolved Mueller measurement
actually has, and they enter the inverse problem at completely different places.

| # | Source | Where it enters | Modelled as |
|---|---|---|---|
| 1 | **Rotation-angle error** | the INDEPENDENT variable | the forward model is evaluated at the TRUE (perturbed) incidence and azimuth, while the observation is filed against the NOMINAL ones — because the nominal ones are all the experimenter has |
| 2 | **Detector error** | the DEPENDENT variable | each of the 15 normalized Mueller elements other than `m₀₀` is read a little high or a little low |

Both errors are drawn from a uniform distribution on `[−u, +u]`, so `u` is a MAX error and the
corresponding standard deviation is `u/√3`. Uniform rather than Gaussian is the honest choice for
both: a rotation stage's setting error is bounded by its mechanics and a detector's
quantization/linearity error is bounded by its specification, and neither has a tail.

**Repeatability** comes from `UniformDeviateProvider.fromSeed`, a provider record wrapping
`System.Random(seed)` — the same seeded generator `Optimization/Synthesis.fs` already uses to make
its annealing and GA runs reproducible. Randomness is injected as a provider rather than reached for
directly, per the repository rule for ambient effects, so one noisy experiment is a pure function of
its `NoiseSeed`.

**Not modelled, deliberately:** plate thickness and wavelength are taken as known exactly (spec R2
fixes thickness as a known per-configuration quantity, and a stabilized HeNe line is known far better
than anything else in the experiment), and ABSOLUTE throughput is irrelevant by construction, because
every comparison in `MuellerInverse` is between NORMALIZED matrices — source brightness, detector gain
and exposure divide out exactly. Modelling a gain error would be modelling nothing.

---

## The magnitudes, and where they come from

The task asked for "a decent but simple optical lab", explicitly NOT metrology-grade. Both figures were
taken from what the equipment actually delivers.

### Rotation: ±0.2° max error

| Evidence | Figure |
|---|---|
| Plain manual SM1 rotation mount (Thorlabs RSP1 family): laser-engraved scale | graduated in **2°** steps |
| High-precision manual rotation mount: side vernier | **5 arcmin = 0.083°** |
| Motorized DC-servo rotation stage (Thorlabs PRM1Z8) | **±0.3° backlash**, 0.1 % on-axis accuracy, 1 arcsec resolution |
| Published polarimeter builds: rotation-stage positioning / repeatability | **0.1–0.2°**; wave-plate mount friction can spoil a setting "by a degree or more" |
| Research ellipsometer angle-of-incidence and sample azimuth | **0.001–0.016°** — the high-precision regime the task excludes |

0.2° sits where a careful worker with ordinary mounts actually lands: better than reading a 2° scale by
eye, worse than an encoder or a research ellipsometer.

Sources:

- [Thorlabs — SM1-compatible manual rotation mounts](https://www.thorlabs.com/newgrouppage9.cfm?objectgroup_id=4113)
- [Thorlabs — high-precision manual rotation mounts (5 arcmin vernier)](https://www.thorlabs.com/newgrouppage9.cfm?objectgroup_ID=990)
- [Thorlabs — PRM1Z8 motorized precision rotation stage](https://www.thorlabs.com/thorproduct.cfm?partnumber=PRM1Z8)
- [Meadowlark — An economical means for accurate azimuthal alignment of polarization optics](https://www.meadowlark.com/wp-content/uploads/2022/04/Azimuthal-Alignment-of-Polarization-Optics.pdf)
- [RP Photonics — Polarimeters (operation principle, accuracy, refinements)](https://www.rp-photonics.com/polarimeters.html)
- [Method for analyzing the measurement error with respect to azimuth and incident angle for the rotating polarizer–analyzer ellipsometer (MDPI Crystals 11, 349)](https://www.mdpi.com/2073-4352/11/4/349)

### Detector: ±0.005 of full scale on each normalized element

Normalized Mueller elements are bounded by 1, so this is directly a fraction of full scale.

| Evidence | Figure |
|---|---|
| Mueller-matrix imaging polarimeter, single-shot CCD pixel | std ≈ **0.01** of maximum intensity |
| Averaged Mueller matrix from the same class of instrument | mean error in the **third decimal**, per-pixel spread in the **second** |
| Silicon photodiode power meter, ABSOLUTE calibration | ±3–5 % (irrelevant here — normalization removes it) |
| Silicon photodiode linearity over a decade of irradiance | ~**1 %** |
| Self-calibrated / multi-detector laboratory standards | ±0.1–0.2 % |

0.005 sits between the averaged research figure (~1e-3, which the class header of
`MuellerInverseTests` already named as the eventual target) and the single-shot figure (~1e-2).

Sources:

- [Mueller matrix imaging polarimeter with polarization camera (J. Eur. Opt. Soc.)](https://jeos.edpsciences.org/articles/jeos/pdf/2024/01/jeos20230037.pdf)
- [Some aspects on the uncertainty calculation in Mueller ellipsometry (Opt. Express 28, 8108)](https://opg.optica.org/oe/fulltext.cfm?uri=oe-28-6-8108&id=428794)
- [Near-infrared Mueller matrix imaging system and application to strain imaging (arXiv:1009.5549)](https://arxiv.org/pdf/1009.5549)
- [RP Photonics — Optical power meters](https://www.rp-photonics.com/optical_power_meters.html)
- [Measurement of the linear range of a photodiode detector (Stony Brook)](https://www.stonybrook.edu/laser/_hmiao/report2/)

---

## The parameterized runner

The task's "extract the core runner rather than copy/paste" is `fitObservations`:

```fsharp
fitObservations : SearchBox -> MuellerObservation list -> MaterialParameters
                    -> ParameterScaling * (float[] -> float[]) * NonlinearSolution
```

Every fit in the file — the original perturbed-start round trip, the blind-start control, and all eight
noisy experiments — goes through it. It takes OBSERVATIONS rather than configurations, because that is
exactly the seam noise enters through: a noisy experiment is the same fit run against data generated at
slightly wrong angles and read back by a slightly wrong detector, and nothing about the fit changes.
`runFit` is now a two-line wrapper that generates noiseless data and calls it.

`SearchBox` is a parameter and not a constant because the two start guesses sit at very different
distances from the truth. Quartz's `n_e` is **51.65 scaled units** from `n = 1.5`, so the ±50 box that
comfortably brackets `perturbedStart` would EXCLUDE the answer from the blind start and the fit would
converge against a bound. The blind fits use ±200.

---

## The blind start, and the control

Per the task, the noisy fits start from `n_o = n_e = 1.5`, `g₁₁ = g₃₃ = 0`. Relative to quartz that is
2.8 % and 3.3 % low on the indices, with the birefringence and both gyration components starting at
exactly zero.

That start is legitimate rather than lucky, for the structural reason the existing `perturbedStart`
commentary gives: linear retardance enters through `cos`/`sin` of `2π(n_e−n_o)d/λ`, so what matters is
the distance in FRINGES. The truth is 0.286 wave for the 20 µm plates, so zero birefringence is under a
third of a fringe away — inside the same basin of attraction.

The control fact asserts this directly by running the identical fit on NOISELESS data:

| Quantity | Observed | Pinned band |
|---|---|---|
| relative error, `n_o` | 5.04e-15 | < 1e-11 |
| relative error, `n_e` | 5.01e-15 | < 1e-11 |
| relative error, `g₁₁` | 1.15e-15 | < 1e-11 |
| relative error, `g₃₃` | 3.31e-13 | < 1e-11 |
| final χ² | 1.40e-22 | < 1e-18 |
| iterations | 19 (against 13 from the near start) | > 0 |

Whatever scatter the noisy ensemble shows is therefore the MEASUREMENT ERROR and not the start guess —
which is the whole reason this control exists.

---

## The ensemble: eight fixed seeds

Seeds `1 … 8`, one complete 29-configuration experiment each, all measured at 0.2°/0.005 and all fitted
from the blind start against the RECORDED angles.

| Quantity | mean | bias | scatter (relative) | scatter (absolute) | worst single run |
|---|---|---|---|---|---|
| `n_o` | 1.5426224809 | 1.068e-5 | 4.870e-4 | 7.512e-4 | 8.581e-4 |
| `n_e` | 1.5516673605 | 1.054e-5 | 4.837e-4 | 7.506e-4 | 8.538e-4 |
| `g₁₁` | 5.9042e-5 | 7.151e-4 | 1.319e-3 | 7.781e-8 | 3.223e-3 |
| `g₃₃` | −1.00840e-4 | 1.582e-3 | 4.604e-2 | 4.650e-6 | 7.821e-2 |
| `n_e − n_o` | 0.0090448796 | 1.331e-5 | 3.992e-4 | 3.610e-6 | 6.799e-4 |

Per-seed final χ² 4.23e-3 … 5.30e-3 (rms residual 3.12e-3 … 3.49e-3) in 18–27 iterations, on 431
degrees of freedom.

That rms IS the noise, and it decomposes as expected. The detector alone contributes `0.005/√3` =
2.89e-3 per element; subtracting it in quadrature leaves 1.2e-3 … 2.0e-3 for the rotation error. So the
detector dominates the RESIDUAL while the rotation error is a minority contributor to it — which is
worth holding next to N1 below, because the two nevertheless behave completely differently in the
recovered PARAMETERS.

### Findings

**N1 — the errors are RANDOM, not systematic.** The rotation error is systematic WITHIN one
experiment: each configuration's angle error is fixed for the whole of it, so the fit minimizes a model
that is wrong in a fixed way and its answer is displaced. Across the eight experiments those
displacements average: every bias is below the standard error of the ensemble mean (`scatter/√8`), the
largest being `g₁₁` at 1.53 SEM. This is a result rather than an assumption, and it is why the
acceptance band is on the SCATTER. Asserted at 3 SEM.

**N2 — `g₃₃` is the poorly-determined constant, at ~4.6 %**, an order of magnitude noisier than
anything else here (35× `g₁₁`, 95× either index). That is the same result the noiseless identifiability
fact reports as a Jacobian column 36× weaker than `g₁₁`'s, seen from the other side. It is also the
mirror image of the published situation — Arteaga/Canillas/Jellison quote their axial component to
0.7 % and their transverse one to ~10 % — with the two roles swapped for the reason finding F1 records.
The ~4.6 % here is the same order as the literature's ~10 % on ITS poorly-determined component.

**N3 — the BIREFRINGENCE is determined 208× better than either index, in absolute terms.** This is the
physical content of the 0.99999 index correlation the noiseless identifiability fact reports: the data
constrains `n_e − n_o` far harder than the common level, so the two indices wander TOGETHER from
experiment to experiment. Their scatters are near-identical (4.870e-4 and 4.837e-4) while the
difference's absolute scatter is 3.61e-6 against 7.51e-4. The comparison must be in ABSOLUTE units:
relatively the difference looks no better than the indices (4.0e-4 against 4.9e-4), because it is being
divided by a value 170× smaller — which is exactly the arithmetic that hides the effect. Asserted at
50×.

**N4 — finding F3 is closed.** F3 recorded that `FitQuality` cannot report an uncertainty on noiseless
synthetic data: it forms `Cov = reducedχ²·(JᵀJ)⁻¹`, the reduced χ² there is ~1e-30, the covariance
diagonal underflows, and every standard error comes back a flat zero. With real noise present the
reduced χ² is 9.82e-6 and the covariance is an ordinary number — and this is the one place in the
repository where the claim can be TESTED rather than asserted, because a covariance estimate is a
prediction about repeat experiments and the ensemble is exactly a set of repeat experiments.

Seed 1's own reported standard error, converted out of the scaled space, against the spread the other
seven actually produced:

| Quantity | σ reported (scaled) | predicted relative | observed ensemble scatter | ratio |
|---|---|---|---|---|
| `n_o` | 0.73545 | 4.7676e-4 | 4.8702e-4 | **0.98** |
| `n_e` | 0.73594 | 4.7430e-4 | 4.8370e-4 | **0.98** |
| `g₁₁` | 0.011143 | 1.8886e-3 | 1.3187e-3 | **1.43** |
| `g₃₃` | 0.39569 | 3.9177e-2 | 4.6043e-2 | **0.85** |

Asserted at a factor of two either way, which is the honest tolerance: an eight-sample standard
deviation is itself only known to `1/√(2·(8−1))` ≈ 27 %. All four 95 % confidence intervals from that
single fit contain the truth. Reduced χ² at the solution: 9.82e-6 on 431 degrees of freedom.

**N5 — ~~but the agreement stops at the DERIVED quantity~~ — WITHDRAWN, see the correction below.**

> **CORRECTION (manual task 014).** This section originally reported that the pair-covariance route for
> the birefringence was numerically dead: that `var(n_e − n_o) = C₁₁ + C₀₀ − 2C₀₁` came out negative
> because of a five-digit cancellation, and that the covariance therefore could not give an error bar on
> a derived quantity. **That was wrong, and the cause was a bug in this repository's test rather than a
> property of the covariance.**
>
> The prediction did come out as zero, but not for the stated reason. The conversion out of the scaled
> space multiplied `sqrt(variance)` by `scale.n_e − scale.n_o` — and both indices carry the SAME scale of
> 1e-3, so that factor was identically zero and the prediction was zero whatever the covariance
> contained. The scale of a DIFFERENCE of two coordinates that share a scale is that scale itself, not
> the difference of the scales. The reported "negative combination" was inferred from the clamped zero,
> never observed.
>
> With the conversion corrected and the quadratic form written properly as `wᵀCw` with `w = e_ne − e_no`
> (i.e. `C₀₀ − C₀₁ − C₁₀ + C₁₁`, which does not assume the numerically-inverted matrix is exactly
> symmetric), the route **works**:
>
> | | predicted | observed ensemble scatter | ratio |
> |---|---|---|---|
> | `n_e − n_o` | 3.75e-4 | 3.99e-4 | **0.94** |
>
> — as good as any of the four diagonal predictions in N4, and this despite the cancellation being every
> bit as severe as described. The fact now asserts the agreement at the same [0.5, 2.0] band the fitted
> parameters use.
>
> The regression was caught by re-running `MuellerInverseTests` after task 014 moved the noise machinery
> into the shared harness, which is precisely what that targeted run existed to check.

---

## Bands as pinned

| Quantity | scatter band | worst-case band |
|---|---|---|
| `n_o`, `n_e` | < 1.5e-3 | < 3.0e-3 |
| `g₁₁` | < 4.0e-3 | < 1.0e-2 |
| `g₃₃` | < 1.5e-1 | < 2.5e-1 |
| `n_e − n_o` | < 1.2e-3 | < 2.0e-3 |

Roughly 2–3× the observed values, per the spec §9 protocol. The ensemble is DETERMINISTIC — fixed
seeds, no wall-clock, no parallelism — so the slack is there to survive a different `System.Random`
stream or a change in the solver's stopping point, not to absorb run-to-run flakiness.

---

## Runtime

A full inverse fit from the blind start is ~45 s. Measured on this machine (which has other load, so
these vary by ~20 %):

| | Before | After |
|---|---|---|
| `MuellerInverseTests` alone | 44.5 s, 17 tests | ~600 s, 20 tests |
| whole `BerremanTests` assembly | — | 508 s, **179 total / 0 failed / 5 skipped** |

The ensemble fact is ~350–365 s of that, the blind-start control ~63 s, the noise-model fact ~1.6 s.
The 5 skips are the pre-existing `SolverTests` wedge/Mueller facts.

**The ensemble is the whole of the increase, and it cannot be made much cheaper.** Running the eight
seeds CONCURRENTLY was measured and is SLOWER, not faster — two seeds in parallel took 184 s against
~100 s serially, and eight took 504 s against 365 s. The forward solves allocate heavily, so extra
threads buy contention rather than throughput. The seeds are therefore run serially, deliberately, and
the test records the measurement so nobody re-tries it.

If the cost ever has to come down, the levers in order of damage done are: fewer seeds (a 5-seed
ensemble is ~230 s, and the sample standard deviation degrades from 27 % to 35 % uncertainty); a looser
`epsX` for the noisy fits only (polishing to 1e-12 scaled units is 15 orders below the uncertainty the
data supports, so 3–5 of the 18–27 iterations are pure waste); or a reduced configuration set, which
changes the experiment and should be the last resort.
