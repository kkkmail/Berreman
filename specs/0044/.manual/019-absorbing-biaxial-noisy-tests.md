# Spec 0044, manual task 016 part 3 — noisy measurement on an absorbing biaxial crystal

Record for the third of the three tasks in `016-biaxial-absorbing-inverse.txt`: repeat the task-014
noisy exercise for the absorbing crystal.

New test class `Berreman/BerremanTests/AbsorbingBiaxialNoisyTests.fs`; new shared module
`AbsorbingBiaxialSample.fs` extracted from the noiseless suite; a third uncertainty budget added to
`InverseFitHarness`.

---

## The short version

| | recovered to |
|---|---|
| three principal indices | **0.033 %** |
| three birefringences | **0.014 – 0.045 %** (64–91× better than any index, in absolute terms) |
| **isotropic absorption** | **0.063 %** |
| six ε″ components | **0.051 – 0.51 %** |
| six gyration components | **2.4 % – 312 %** — five of the six worse than 10 % |

**The absorption is measurable on an ordinary bench; the optical activity is not.** That is the same
verdict `015` reached for the transparent crystal, now with the absorption added as a large, well-behaved
signal that the transparent problem simply was not using.

---

## What was new in the error model

A transparent suite normalizes `m₀₀` away, so a gain error costs nothing and is not modelled. An
absorbing suite cannot: `m₀₀` is a datum carrying the isotropic absorption. So the bench grew a **third
budget** — `ThroughputUncertainty`, a **1 % relative error on the absolute throughput** — and it is
deliberately looser than the 0.005 on the normalized elements:

> The normalized elements are **ratios taken within one acquisition**, so source drift and detector gain
> cancel out of them. The absolute throughput is a ratio between **two** acquisitions — sample in, sample
> out — and carries the full stability of the source, the detector gain and the reference path.
> Photodiode power meters are ±3–5 % on absolute calibration and ~1 % on linearity; a carefully
> referenced transmittance lands near the 1 % end.

`misreadAbsolute` and `noisyObserveAbsoluteWith` were added beside the existing `misread` /
`noisyObserveWith` rather than replacing them. That is not fastidiousness: the transparent suites' pinned
numbers depend on the exact order in which random draws are consumed, and this task was forbidden from
re-running them. The new path draws the throughput error *first*, then the same fifteen element errors in
the same order, so the transparent stream is untouched.

---

## Two measured constraints that shaped the design

### The blind start does not converge at fifteen unknowns

`014` fits the transparent nine-parameter problem from a **blind** start — isotropic `n = 1.5`, no
optical activity — and that is what makes its ensemble scatter attributable to the bench rather than to
prior knowledge. The same blind start was tried here, with the absorption started at the order of
magnitude a single transmittance reading would give:

| | |
|---|---|
| iterations | **240** (the cap) |
| final χ² | **36.8** (a converged fit reaches 1e-23) |
| worst parameter | `g23` out by a factor of **6** |

So the ensemble uses the **perturbed start** — every ε″ component 20–30 % wrong, every gyration
component 25–35 % wrong, the indices 0.1–0.2 % out. **Nine unknowns tolerate ignorance; fifteen need a
starting point.** That single 15-minute measurement is recorded here rather than kept as a test, because
it asserts a negative and costs a quarter of an hour.

The control role is filled by the noiseless suite's own main fact, which recovers all fifteen constants
to better than 1e-8 from this very start on this very measurement set. Re-running it here would prove
something already proven.

### Five seeds, not eight

A fifteen-parameter fit on noisy data takes 28–62 iterations against the noiseless 22, and eight of them
would put this single fact past forty minutes. Five gives a sample standard deviation at 35 % relative
uncertainty against the eight-seed 27 %, and the seeds are the first five of the shared list so the
ensembles stay comparable as far as they overlap.

The bias threshold moved from 3 standard errors to **5** as a consequence, and that is a correction
rather than a loosening: with five seeds the sampling distribution of the mean is Student-t on four
degrees of freedom, and t(4) puts **4 %** of its mass beyond 3σ against a normal's 0.3 %. Across nineteen
reported quantities a 3σ rule would be expected to flag one by chance. Measured, two sit between 3 and
3.6 (`e22` at 3.5, the isotropic absorption at 3.2) and every other is below 2.7 — which is what an
unbiased set of nineteen t(4) estimates looks like.

---

## Results

Five seeds, 44 configurations, 100 µm plates, 632.8 nm. Per-seed final χ² 0.0498–0.0810 (rms residual
0.0084–0.0107) in 28–62 iterations.

| Quantity | mean | bias | scatter | absolute scatter |
|---|---|---|---|---|
| `n₁` | 1.5744770 | 2.62e-4 | 3.30e-4 | 5.19e-4 |
| `n₂` | 1.6018281 | 2.57e-4 | 3.26e-4 | 5.22e-4 |
| `n₃` | 1.6168239 | 2.54e-4 | 3.23e-4 | 5.22e-4 |
| `e11` | 1.2032e-3 | 2.71e-3 | 2.26e-3 | 2.72e-6 |
| `e22` | 1.8015e-3 | 8.06e-4 | 5.09e-4 | 9.17e-7 |
| `e33` | 8.988e-4 | 1.29e-3 | 1.52e-3 | 1.37e-6 |
| `e23` | 3.512e-4 | 3.36e-3 | 3.56e-3 | 1.24e-6 |
| `e13` | −2.504e-4 | 1.67e-3 | **5.14e-3** | 1.29e-6 |
| `e12` | 4.503e-4 | 5.84e-4 | 1.73e-3 | 7.77e-7 |
| `g11` | 4.633e-5 | 7.49e-2 | 0.408 | 1.76e-5 |
| `g22` | 5.497e-5 | 0.203 | 0.193 | 1.33e-5 |
| `g33` | −8.534e-6 | 0.701 | 0.727 | 2.07e-5 |
| `g23` | 3.682e-5 | 1.141 | **3.122** | 5.37e-5 |
| `g13` | −3.626e-5 | 3.71e-3 | 2.38e-2 | 8.66e-7 |
| `g12` | 2.304e-6 | 0.906 | 1.003 | 2.47e-5 |
| `n₂−n₁` | 0.02735111 | 3.55e-5 | 2.97e-4 | **8.13e-6** |
| `n₃−n₂` | 0.01499585 | 8.35e-5 | 4.48e-4 | **6.72e-6** |
| `n₃−n₁` | 0.04234696 | 5.25e-5 | 1.36e-4 | **5.74e-6** |
| **isotropic absorption** | 1.30118e-3 | 9.08e-4 | **6.32e-4** | 8.21e-7 |

### C1 — the absorption is measurable, the optical activity is not

Every one of the six ε″ components lands inside **0.51 %**. Five of the six gyration components are worse
than **10 %**, and `g23` is worse than **300 %** — the bench does not measure it in any useful sense.

Stated as two separate bounds rather than as a ratio, deliberately: a ratio would be dominated by `g13`,
the one gyration component this design happens to constrain well (2.4 %), and would understate how
cleanly the two groups separate.

The cause is the one `018` measured in the noiseless case: the six absorption columns come in an order of
magnitude stronger than any gyration column. **Linear dichroism is a large, direct signal; optical
activity is a small one riding on top of a much larger linear retardance.**

### C2 — a prediction that turned out wrong

`018` found normalized data 3.0× less sensitive to the isotropic absorption than to an anisotropic one,
with absolute throughput restoring a factor of 3.1 — and the channel that restores it carries the
**loosest** budget on the bench, 1 % against 0.5 %. The obvious inference, written into this suite's
class header before the ensemble ran, was that the isotropic absorption would come back as the
**worst**-determined of the six absorption components: formally identifiable, practically marginal.

**It came back as one of the best**: 0.063 %, against 0.17–0.51 % for the three off-diagonal components.

The inference missed that a throughput reading is a **direct and redundant** measurement of the isotropic
level, repeated across all 44 configurations, so its 1 % error averages down by √44 to about 0.15 %.
The anisotropic components are inferred from small *differences* between normalized elements and inherit
both the 0.5 % element error and a much weaker geometric lever. **A loose budget on a direct, redundant
measurement beats a tight budget on an indirect one.** The header now records the prediction and its
refutation, and the fact asserts the measured ordering.

### C3 — the birefringences again survive far better than the indices

Absolute scatters of 5.74e-6, 6.72e-6 and 8.13e-6 against ~5.2e-4 for every index: **64× to 91× better**.
Same effect and same cause as in the transparent ensembles — linear retardance depends on the
*differences*, so the data pins them far harder than the common index level and the three indices wander
together. Asserted at 50×, the transparent suites' figure.

### C4 — the errors are random, not systematic

Every bias inside 5 standard errors of the mean (see the Student-t discussion above). The rotation error
is systematic *within* one experiment and averages *across* them, exactly as in the transparent case.

---

## Extraction

| Added to | What |
|---|---|
| `InverseFitHarness` | `ThroughputUncertainty`; `NoiseParam.throughput`; `misreadAbsolute`; `noisyObserveAbsoluteWith` |
| `AbsorbingBiaxialSample` (new) | the fifteen-parameter ground truth, `buildAbsorbing`, `absorbingScalingAround`, the 44-configuration measurement set, `perturbedStart`, `searchBox` |

`AbsorbingBiaxialInverseTests` was repointed at the shared sample and re-run (it is one of this task's
own new classes); the pre-existing suites were compiled but not executed, per the task's instruction.

---

## Runtime

| | |
|---|---|
| `AbsorbingBiaxialNoisyTests` | **~17 min**, 1 fact (five 15-parameter fits at 28–62 iterations each) |
| `AbsorbingBiaxialInverseTests` | 177 s, 7 facts |
| `FeasibleBiaxialInverseTests` | 104 s, 5 facts |

The noisy fact is the single most expensive test in the assembly. If it has to come down, the levers in
order of damage done are: **fewer seeds** (three would be ~10 min and leave the standard deviation known
only to 50 %); **a looser `epsX` for the noisy fits only** (polishing to 1e-12 scaled units is far below
the uncertainty the data supports, and the noisy fits spend 28–62 iterations where the noiseless one
spends 22); or **a reduced configuration set**, which changes the experiment and should be last.
