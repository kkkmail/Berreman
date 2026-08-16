# Spec 0044, manual task 014 — noisy measurement on a biaxial (triclinic) crystal

Record for `014-biaxial-inverse-noisy-tests.txt`, which asked for the biaxial counterpart of the noisy
tests described in `010-extend-mueller-inverse-tests.txt` and recommended at the end of
`013-biaxial-triclinic-inverse.md`.

Two new facts in `Berreman/BerremanTests/BiaxialInverseTests.fs`; the measurement-error model that was
local to `MuellerInverseTests` moved into the shared `InverseFitHarness` and is now generic.

---

## The short version

**The three principal refractive indices and the three birefringences survive an ordinary optical bench.
The six gyration components do not.**

| | recovered to | comment |
|---|---|---|
| `n₁`, `n₂`, `n₃` | **0.088 – 0.091 %** | 1.4e-3 absolute |
| `n₂−n₁`, `n₃−n₂`, `n₃−n₁` | **0.038 – 0.097 %** | 1.1e-5 … 1.6e-5 absolute — 91 to 135× better than any single index |
| `g₁₁` … `g₁₂` | **21 % – 187 %** | every one of them ~1.3e-5 … 3.2e-5 ABSOLUTE, whatever its own value |

That negative result is the interesting one, it has a structural cause, and it is asserted rather than
left implicit — see **N3** below.

---

## What was generalized first

Task 012 moved the fit driver into `InverseFitHarness` so two suites could share it. This task did the
same for the measurement-error model, for the same reason: it describes the **apparatus**, not the
sample, and both crystals are now measured on the same bench with the same eight seeds — which is the
only thing that makes their ensembles comparable at all.

Moved out of `MuellerInverseTests` and made generic in `'Parameters`:

| Moved | Note |
|---|---|
| `NoiseSeed`, `AngleUncertainty`, `ElementUncertainty`, `NoiseParam`, `UniformDeviateProvider` | verbatim; `NoiseParam.decentLab` is now the one definition of the 0.2° / 0.005 bench |
| `disturb`, `misread`, `noisyObserveWith` | generic via `ForwardModelProxy<'Parameters>` |
| `RecoveredQuantity<'Parameters>` | was a uniaxial DU; now a record of `name` + `valueIn`, built either `ofAxis` (a fitted parameter) or `derived` (a birefringence) |
| `RecoveryStatistics<'Parameters>`, `statisticsFor`, `standardErrorOfMean`, `rowFor` | generic |
| `predictedRelativeErrors`, `combinationVariance`, `differenceWeights` | new — the covariance cross-check, which both suites now run |
| `ensembleSeeds` | the same eight for both suites |

`FittedParameter`, the uniaxial-only identity DU, was **deleted**: `UniaxialParameters.axes` from the
domain already carries the names, and `recoveryErrors` / `describeErrors` from the harness already
carry the reporting. `MuellerInverseTests.fs` is a net **332 lines shorter** (+90 / −422) and every one
of its pinned numbers is unchanged.

The draws are consumed in exactly the order they were before — two per configuration for the angles,
then fifteen for the Mueller elements — so every number the uniaxial ensemble had pinned reproduces
bit for bit. That was the one real risk in the move, and the uniaxial suite's own bands are what check
it.

---

## The biaxial noisy design

Same bench as the uniaxial suite (**0.2° max rotation error, 0.005 of full scale per normalized Mueller
element, both uniform**; provenance in `011-noisy-measurement-tests.md`), same eight fixed seeds, same
40-configuration measurement set as the noiseless biaxial facts.

**The start is blind and ISOTROPIC**: `n₁ = n₂ = n₃ = 1.5`, all six gyration components zero. That is
harder than the uniaxial suite's blind start — all three indices are 4.7 %, 6.3 % and 7.2 % low, all
three birefringences start at exactly zero, and at `n₁ = n₂ = n₃` the crystal has no preferred axes at
all, so the three index directions are as nearly degenerate at the first step as they can be. It is
legitimate because what matters is distance in FRINGES: the true birefringences are 0.173, 0.095 and
0.268 wave on the 4 µm plate, so zero is under a third of a fringe from the farthest of them.

### The control — the blind start is not the limitation

| Quantity | Observed | Pinned band |
|---|---|---|
| relative error, `n₁` / `n₂` / `n₃` | 3.10e-15 / 2.91e-15 / 2.88e-15 | < 1e-10 |
| relative error, `g₁₁` … `g₁₂` | 3.24e-13 … 1.17e-12 | < 1e-10 |
| final χ² | 8.79e-26 | < 1e-21 |
| iterations | **15** (against 12 from the perturbed start) | > 0 |

Three extra iterations is the entire price of knowing nothing about the material. Whatever the ensemble
below scatters by, it is therefore measurement error and not the start guess.

---

## Results — eight noisy experiments

Per-seed final χ² 6.07e-3 … 6.82e-3 (rms residual 3.18e-3 … 3.37e-3) in **23–43 iterations**. The rms is
the noise: `0.005/√3` = 2.89e-3 from the detector alone, leaving ~1.3e-3 … 1.7e-3 in quadrature for the
rotation error.

| Quantity | mean | bias | scatter (rel) | scatter (abs) | worst |
|---|---|---|---|---|---|
| `n₁` | 1.5741892 | 7.90e-5 | 9.06e-4 | 1.426e-3 | 1.53e-3 |
| `n₂` | 1.6015453 | 8.01e-5 | 8.92e-4 | 1.428e-3 | 1.50e-3 |
| `n₃` | 1.6165498 | 8.40e-5 | 8.80e-4 | 1.422e-3 | 1.47e-3 |
| `g₁₁` | 4.239e-5 | 1.65e-2 | 0.2995 | 1.291e-5 | 0.569 |
| `g₂₂` | 7.876e-5 | 0.1414 | 0.2076 | 1.432e-5 | 0.433 |
| `g₃₃` | −4.200e-5 | 0.4738 | 0.7921 | 2.258e-5 | 1.876 |
| `g₂₃` | 2.141e-5 | 0.2446 | **1.8701** | 3.217e-5 | **4.130** |
| `g₁₃` | −4.789e-5 | 0.3157 | 0.3999 | 1.456e-5 | 1.073 |
| `g₁₂` | 2.511e-5 | 2.07e-2 | 0.5903 | 1.452e-5 | 0.988 |
| `n₂ − n₁` | 0.0273560 | 1.44e-4 | 3.84e-4 | 1.051e-5 | 7.05e-4 |
| `n₃ − n₂` | 0.0150046 | 4.98e-4 | 9.73e-4 | 1.459e-5 | 2.35e-3 |
| `n₃ − n₁` | 0.0423606 | 2.70e-4 | 3.70e-4 | 1.565e-5 | 9.19e-4 |

### N1 — the errors are RANDOM, not systematic

Every bias is inside three standard errors of the ensemble mean (`scatter/√8`), the largest being `g₁₃`
at 2.23 SEM and `n₃−n₁` at 2.07. With twelve quantities, a maximum |z| of ~2.2 is exactly what a set of
unbiased estimates should show. So the rotation-angle error — which is genuinely systematic *within* one
experiment, because the fit assumes angles the sample was never at — has no preferred direction *across*
experiments. Same conclusion as the uniaxial ensemble, now on nine unknowns instead of four.

### N2 — the birefringences survive 91–135× better than the indices

The three indices all scatter by ~1.42e-3 absolute, and by almost exactly the same amount as each other:
they wander **together** from experiment to experiment. Their differences scatter by 1.05e-5, 1.46e-5 and
1.57e-5. Linear retardance depends on `Δn`, not on any index alone, so the data pins the differences far
harder than the common level, and the common level is the near-null direction.

The comparison must be in ABSOLUTE units. Relatively the differences look no better than the indices
(3.8e-4 … 9.7e-4 against 8.8e-4 … 9.1e-4), because they are being divided by values 40–100× smaller —
which is exactly the arithmetic that hides the effect. Asserted at 50×, the same figure the uniaxial
suite uses for its single birefringence.

### N3 — the gyration tensor is at the edge of measurability, and the cause is the plate thickness

This is the headline, and it is asserted rather than left implicit, so that a design change which fixed
it would announce itself.

Relative scatters run from 21 % (`g₂₂`) to 187 % (`g₂₃`), and the worst single experiment misses `g₂₃` by
four times its own value. The three indices, measured in the very same experiments, come out at 0.09 %.

**What is actually going on is visible only in absolute units.** Every one of the six components is
determined to 1.29e-5 … 3.22e-5 — a spread of 2.5× against component VALUES that span a factor of four.
The uncertainty is a property of the BENCH and the sample geometry, not of the component; the relative
figures are just that absolute uncertainty divided by six different numbers, and `g₂₃` looks worst only
because at 1.72e-5 it is the smallest of the six. Asserted both ways: an absolute band of 6e-5 on each,
and `max < 4 × min` across the six.

**And the cause is the plate thickness, which is forced by the crystal being biaxial.** Circular
retardance grows as `g·d`, so gyration sensitivity is proportional to thickness:

| | plate | gyration absolute uncertainty |
|---|---|---|
| uniaxial suite (quartz), C1 | **1 mm** | `g₁₁` to **7.8e-8** |
| this suite (triclinic) | **4 µm** | every component to **~1.5e-5** |
| ratio | 250× | **190×** |

The 190-fold penalty is almost exactly the 250-fold thickness ratio. The uniaxial suite can use a 1 mm
plate because its C1 configuration propagates along the optic axis and carries **no linear retardance at
all**. A biaxial crystal has linear birefringence along every principal axis, so no such plate exists:
every configuration here has to be 4 µm to stay sub-wave. The optical activity of a biaxial crystal is
hard to measure for a structural reason, not an incidental one.

**The obvious fix, for a future slice.** A biaxial crystal *does* have two retardance-free directions —
its optic axes — and `013` established that this material's are reachable from an x-cut plate (36.70°
internal, against Snell's 38.64° cap). A THICK plate used only in that near-optic-axis configuration
would buy back most of the 250× without reintroducing fringe aliasing, because there is almost no linear
retardance there to alias. That changes the measurement set and would require re-harvesting every
noiseless band, so it was not done here.

### N4 — the fit's own covariance predicts the ensemble to within a factor of ~2, across four decades

A covariance estimate is a prediction about repeat experiments, and an ensemble is exactly a set of
repeat experiments, so the two can be compared. Seed 1's own reported standard error, converted out of
the scaled space, against the spread the other seven produced:

| | `n₁` | `n₂` | `n₃` | `g₁₁` | `g₂₂` | `g₃₃` | `g₂₃` | `g₁₃` | `g₁₂` |
|---|---|---|---|---|---|---|---|---|---|
| predicted | 5.60e-4 | 5.53e-4 | 5.46e-4 | 0.439 | 0.268 | 0.762 | 2.047 | 0.630 | 1.420 |
| observed | 9.06e-4 | 8.92e-4 | 8.80e-4 | 0.300 | 0.208 | 0.792 | 1.870 | 0.400 | 0.590 |
| **ratio** | 0.618 | 0.620 | 0.621 | 1.466 | 1.289 | 0.962 | 1.094 | 1.574 | **2.405** |

The predictions span from "one part in two thousand" to "twice the value itself" and every one lands
between 0.62× and 2.40× of the truth. Banded at [0.3, 4.0] — looser than the uniaxial suite's
[0.5, 2.0], and the reason is recorded rather than hidden: a covariance estimate is a LINEARIZATION
about the solution, exact only where the uncertainty is small enough for the linearization to hold. It
holds beautifully for the indices (0.09 % errors) and progressively less well for the gyration
components, whose error bars are the same size as their own values. Ratios drifting with the size of the
error is what a linearized estimate of a nonlinear problem should do.

**A sharper statement the three indices support and no single one could.** Their ratios are 0.6182,
0.6197 and 0.6208 — identical to four parts in a thousand. The covariance is not merely approximately
right for them, it is wrong by the SAME factor for all three, which is the signature of a discrepancy in
the ERROR MODEL rather than in any one parameter: the covariance treats the whole residual as random
noise, and part of it is the rotation-angle error, which is fixed within an experiment and therefore
inflates the real experiment-to-experiment scatter above what the residual alone predicts. Asserted at
1.05×.

All nine 95 % confidence intervals from that single fit contain the truth.

### N5 — a correction to task 010, caught by the targeted regression run

Moving the noise machinery into the shared harness rewired ~15 call sites in the already-delivered
uniaxial ensemble fact, so `MuellerInverseTests` was re-run to confirm its pinned numbers reproduced.
One assertion failed — and the failure was correct: the **old** assertion had been wrong.

`011-noisy-measurement-tests.md` finding N5 reported that the pair-covariance route for the
birefringence was numerically dead — that `var(n_e − n_o) = C₁₁ + C₀₀ − 2C₀₁` came out negative under a
five-digit cancellation, so a covariance could not give an error bar on a derived quantity. It did
return zero, but not for that reason: the conversion out of the scaled space multiplied by
`scale.n_e − scale.n_o`, and both indices carry the **same** scale of 1e-3, so that factor was
identically zero and the prediction was zero whatever the covariance contained. The scale of a
DIFFERENCE of two coordinates sharing a scale is that scale itself, not the difference of the scales.
The "negative combination" was inferred from the clamped zero and never actually observed.

Rewriting it as the proper quadratic form `wᵀCw` with `w = e_ne − e_no` — which also avoids assuming
the numerically-inverted matrix is exactly symmetric — gives **3.75e-4 predicted against 3.99e-4
observed, a ratio of 0.94**. The route works, and works as well as the diagonal predictions do. The
uniaxial fact now asserts that agreement at the same [0.5, 2.0] band, and `011`'s N5 has been struck
through with the correction inline.

The biaxial results above are unaffected: this suite predicts uncertainties only for the nine FITTED
parameters, each of which reads its own non-zero scale.

---

## Bands as pinned

| Quantity | scatter band | worst-case band |
|---|---|---|
| `n₁`, `n₂`, `n₃` (relative) | < 2.5e-3 | < 4.0e-3 |
| `n₂−n₁`, `n₃−n₂`, `n₃−n₁` (relative) | < 2.5e-3 | < 6.0e-3 |
| six gyration components (**absolute**) | < 6.0e-5 | — |
| gyration spread, max/min absolute | < 4.0 | — |
| birefringence vs worst index, absolute | < 1/50 | — |
| bias, every quantity | < 3 standard errors of the mean | — |
| covariance ratio, every parameter | 0.3 … 4.0 | — |
| index covariance ratios, max/min | < 1.05 | — |

Roughly 2–2.5× the observed values, per the spec §9 protocol. The ensemble is DETERMINISTIC — fixed
seeds, no wall-clock, no parallelism — so the slack exists to survive a different `System.Random` stream
or a change in the solver's stopping point, not to absorb run-to-run flakiness.

---

## Runtime — and this is the real cost

| | Before | After |
|---|---|---|
| `BiaxialInverseTests` | 68 s, 9 tests | **997 s (16.6 min)**, 11 tests — measured |
| `MuellerInverseTests` | ~600 s, 20 tests | unchanged — measured |
| whole `BerremanTests` assembly | 559 s, 188 tests | **~25 min**, 190 total / 0 failed / 5 skipped — inferred, not measured end to end |

The assembly figure is `559 + (997 − 68)`. Only the two suites this task touched were re-run in full;
nothing else in the assembly was modified, so a whole-assembly run would have re-executed the
16.6-minute biaxial suite for no new information.

The biaxial ensemble alone is **~15.5 min**: eight nine-parameter fits at 23–43 iterations each on 40
configurations, i.e. ~10 residual evaluations per iteration and 40 forward solves per evaluation. Noise
roughly triples the iteration count over the noiseless fit's 15.

The seeds are run SERIALLY, deliberately. The uniaxial suite measured concurrency on the same forward
model and found it **slower**: two seeds in parallel took 184 s against ~100 s serially, and eight took
504 s against 365 s, because the solves allocate heavily and extra threads buy contention rather than
throughput.

If the cost has to come down, the levers in order of damage done are: **fewer seeds** (a 5-seed ensemble
is ~9.7 min, and the sample standard deviation degrades from 27 % to 35 % uncertainty); **a looser
`epsX` for the noisy fits only** (polishing to 1e-12 scaled units is fifteen orders below the
uncertainty the data supports, and the noisy fits are spending 23–43 iterations where the noiseless one
spends 15); or **a reduced configuration set**, which changes the experiment and should be the last
resort.
