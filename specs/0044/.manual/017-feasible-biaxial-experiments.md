# Spec 0044, manual task 016 part 1 — an experimentally feasible biaxial experiment

Record for the first of the three tasks in `016-biaxial-absorbing-inverse.txt`: *"determine if there are
'better' but realistic experiments that we should use for biaxial, gyrotropic, transparent crystal."*

New test class `Berreman/BerremanTests/FeasibleBiaxialInverseTests.fs` (5 facts, 104 s, all green).
New shared module `Berreman/BerremanTests/BiaxialSample.fs`. `BiaxialInverseTests` was repointed at that
module and otherwise left alone.

---

## The verdict: the existing 4 µm plate is fiction

`BiaxialInverseTests` recovers all nine constants from a measurement set built on a **4 µm free-standing
LBO plate**. Web research on what vendors and laboratories actually supply says that plate does not
exist and cannot be made.

| Evidence | Figure |
|---|---|
| Castech, Newlight, Eksma — LBO **minimum thickness**, all three independently | **100 µm (0.1 mm)**, free-standing |
| LBO thickness **tolerance** at that minimum | ±20 µm — *five times the entire 4 µm plate* |
| Parallelism at that minimum | 20 arcsec |
| Newlight, free-standing boundary | < 50 µm ⇒ must be optically contacted onto ~2 mm fused silica; ≥ 100 µm ⇒ always free-standing |
| Altechna, on BBO (far more thinnable than LBO) | "due to mechanical properties … thinner than 50 µm free-standing pieces are not available" — a material limit, not a process one |
| Thinnest polished free-standing plate in the optical-activity literature | ~13 µm, and that is soft molecular L-alanine over a pinhole; hard inorganics bottom out near 58 µm |
| Petrographic thin section | 30 µm — but **permanently epoxy-bonded to a glass slide**, never free-standing |

LBO is conspicuously absent from the "ultrathin" catalogues BBO appears in: no vendor offers it below
100 µm in any form. **A 4 µm LBO plate is 25× thinner than the thinnest one anybody sells.**

So the 4 µm suite answers "what would the inverse problem do with ideal data?", which is a real question
and the reason it stays exactly as it is. This suite answers a different one.

---

## The fix is not a thinner plate — it is a different cut

The 4 µm figure was forced by keeping linear retardance sub-wave. But a biaxial crystal has two
directions in which linear birefringence vanishes **identically** — its optic axes — and a plate cut
perpendicular to one carries no linear retardance at normal incidence *whatever its thickness*. That is
not a trick invented here; it is how biaxial optical activity has been measured for a century. Real
optic-axis plates in the literature are 0.42–1.32 mm (NaNO₂) and quartz basal sections run 3–14.75 mm,
with rotatory power quoted in deg/mm precisely because thickness is free in that geometry.

### Which optic axis — a 0.73° error that would have destroyed the design

The literature carries **two** formulas under the name "optic axis", and they are not the same:

| | formula | angle from n₃ | linear retardance on a 100 µm plate |
|---|---|---|---|
| **biradial** (ray axes) | `tan²V = (n₂²−n₁²)/(n₃²−n₂²)` | 53.299° | **29.59°** |
| **binormal** (wave-normal axes) | `tan²V = (n₁⁻²−n₂⁻²)/(n₂⁻²−n₃⁻²)` | 54.025° | **1.5e-5°** |

At normal incidence there is no refraction, so the internal wave normal *is* the surface normal and the
**binormal** is the one that matters. The first draft of this suite used the biradial — the formula
already in `BiaxialSample` from task 012, where it is correct for the purpose it serves there — and the
"optic-axis" plate carried 29.6° of stray retardance, i.e. was not an optic-axis plate at all.

The suite does not assert this from theory. It **scans** the cut angle in 0.1° steps and asserts the
measured linear-retardance minimum lands on the binormal prediction (measured minimum 53.999°, binormal
54.025° — inside one scan step) and that the biradial angle is visibly *not* an optic axis. At the exact
binormal the plate inverts to `lb = −1.5e-5°`, `lb′ = 1.5e-6°`, `cb = 5.744°`: a pure circular retarder
to five decimal places, carrying **2.87° of optical rotation** on a plate you can hold with tweezers.

`BiaxialSample.opticAxisWaveNormal` was added alongside the existing `opticAxisFromLargest`; both are
kept, documented as the two different things they are.

### What the thickness buys, and what it costs

Circular retardance grows as `g·d`, so gyration sensitivity is proportional to thickness: **25× more
signal at 100 µm than at 4 µm**, asserted directly by inverting the same cut at both thicknesses.

The cost is alignment sensitivity. Off the optic axis the birefringence returns *linearly*:
`Δn ≈ (n₃−n₁)·sin(2V)·sin θ = 0.0406 sin θ`, which on a 100 µm plate is **0.11 wave per degree**. And the
optic-axis direction depends on the very indices being fitted — it moves **1.54°** between the truth and
this suite's start guess, which is 0.17 wave at 100 µm but would be 0.86 wave at 500 µm, past the
half-fringe basin. The mechanically feasible plate and the numerically safe plate turn out to be the same
plate.

---

## One wavelength, not three — an operator correction

The first draft of this suite used **three wavelengths** (450 / 532 / 632.8 nm), on the standard
laboratory argument that a retardance which aliases at one wavelength does not alias at another, since
`δ = 2πΔn d/λ` scales as 1/λ while the aliasing step is a fixed 2π. That is genuinely how real benches
resolve the order, and the three-colour version measured a decoy 1696× more visible across three lines
than at one.

**It is nevertheless wrong here, and was removed.** The nine constants are wavelength-dependent. Fitting
one set of them to three colours asserts a non-dispersive crystal, which is false; a real multi-colour
experiment would have to fit a **dispersion model**, which is a larger inverse problem of a different
kind. Documenting the non-dispersive assumption as a "scope boundary" was not good enough — it would have
made every number in this suite conditional on a premise no real material satisfies.

Every configuration is now at 632.8 nm.

### The order hazard, measured rather than dodged

Losing wavelength diversity leaves a real hazard, and the suite asserts both halves of it.

A **clean order decoy** is constructed — `n₂` and `n₃` both shifted by exactly one wave (`λ/d` = 6.328e-3),
which sends `n₂−n₁` up one order, leaves `n₃−n₂` untouched, and therefore carries `n₃−n₁` up one order
too: all three birefringences integral at 632.8 nm. Then:

| Configuration group | worst normalized Mueller element gap to the decoy |
|---|---|
| principal cuts, normal incidence | **1.1e-3** — invisible on any real bench |
| optic-axis cuts, normal incidence | **1.65** — could not be missed by eye |
| ratio | **1497×** |

**This was the surprise of the task.** The optic-axis plate was added to amplify the gyration signal. It
turns out to break the order ambiguity as well — and it *can*, precisely because it has no linear
retardance: an index shift that is a pure 2π alias where retardance dominates is an ordinary first-order
change where there is none. A single-wavelength design is viable because of it.

What remains is a precondition on the **start guess**, not on the data: the order is absent from the
principal-cut data, so it must come from prior knowledge. The fit converges only if every initial
birefringence is within half a fringe. This suite's start is 0.25 / 0.13 / 0.11 fringe out — asserted
directly, so that loosening the start without re-examining the plate fails here rather than silently
converging to an aliased branch.

---

## The measurement set

All plates **100 µm** (the vendor floor), all at **632.8 nm**. 46 configurations.

| Group | Configurations | Purpose |
|---|---|---|
| **F1** | 2 optic-axis cuts × 4 azimuths, normal, transmission = **8** | the anchor: zero linear retardance, 2.87° of rotation, and the order-breaker |
| **F2** | optic-axis cut × {20°, 40°, 60°} × {0°, 90°} × {T, R} = **12** | the *tilter* geometry — retardance grows continuously from exactly zero, so the order is tracked by continuity, never ambiguous |
| **F3** | 3 principal cuts × {0°, 45°} × {0°, 90°} × {T, R} = **24** | the only configurations that make the gyration OFF-diagonals observable (task 012's ablation result) |

---

## Results

### All nine constants recovered

| Quantity | Observed | Pinned band |
|---|---|---|
| relative error, `n₁` / `n₂` / `n₃` | 5.22e-15 / 5.13e-15 / 5.08e-15 | < 1e-8 |
| relative error, `g₁₁` / `g₂₂` / `g₃₃` | 5.33e-12 / 8.07e-12 / 2.62e-11 | < 1e-8 |
| relative error, `g₂₃` / `g₁₃` / `g₁₂` | 2.68e-10 / 7.47e-14 / 1.21e-10 | < 1e-8 |
| final χ² | 2.72e-23 | < 1e-18 |
| iterations | **28** (idealized design: 12) | > 0 |

The extra iterations are the price of the principal cuts carrying 6.69 waves instead of 0.27: the
residual surface is far more structured and the optimizer walks further to cross it.

### The payoff — every gyration component better constrained

Jacobian column norms at the truth, against the idealized 4 µm figures transcribed from
`013-biaxial-triclinic-inverse.md` (constants, not a re-run — task 016 forbids running the existing
suites):

| | `g₁₁` | `g₂₂` | `g₃₃` | `g₂₃` | `g₁₃` | `g₁₂` |
|---|---|---|---|---|---|---|
| idealized 4 µm | 2.36e-3 | 2.64e-3 | 2.39e-3 | 9.78e-4 | 1.48e-3 | 1.03e-3 |
| **feasible 100 µm** | 1.45e-2 | 4.03e-2 | 2.62e-2 | 1.71e-3 | 3.83e-2 | 2.25e-3 |
| **gain** | **×6.2** | **×15.3** | **×10.9** | **×1.7** | **×25.9** | **×2.2** |

All six improve; four improve more than fivefold. The two that barely move — `g₂₃` and `g₁₂` — are the
off-diagonal components task 012 showed are reachable *only* through oblique incidence on principal cuts,
and that is the one part of the design which did not change.

Index columns: 5.47 / 7.20 / 6.43 (idealized: 0.251 / 0.270 / 0.257) — **×26**, tracking the thickness
ratio as expected.

### The cost, reported rather than buried

**Jacobian condition number 7954, against the idealized design's 382 — 21× worse.** The index columns
grew even faster than the gyration ones (25× more retardance-bearing path), so the spread between the
strongest and weakest column widened.

Both statements are true and neither cancels the other. A better-conditioned problem is easier to *solve*;
but it is the **absolute** column norms that decide how much a *noisy* measurement can say, and those are
uniformly better here. Since `015` established that the idealized design's fatal weakness under a
realistic bench was gyration error bars of the same order as the values themselves, the feasible design
is the better experiment on the axis that matters.

---

## Extraction

`BiaxialSample.fs` now holds the triclinic ground truth, `buildTriclinic`, `solverParameters`,
`scalingAround`, both optic-axis formulas, the tensor readers and `recoveredQuantities`.
`BiaxialInverseTests` was repointed at it and is otherwise untouched; per the task's instruction it was
**compiled but not run** — the whole point of a shared sample is that the two suites measure the same
crystal with different experiments, and duplicating the constants would destroy that comparison the
moment one copy was edited.

---

## Sources

- [Castech — LBO](https://www.castech.com/product/LBO---Lithium-Triborate-121.html), [Eksma — LBO crystals](https://eksmaoptics.com/nonlinear-and-laser-crystals/nonlinear-crystals/), [Newlight — thin/ultrathin crystals](https://www.newlightphotonics.com/) — the 100 µm minimum, ±20 µm tolerance, 20 arcsec parallelism, and the free-standing/bonded boundary
- [Altechna — ultrathin BBO](https://www.altechna.com/) — thinner than 50 µm free-standing not available, stated as a material property
- [Shopa, Ftomyn & Shopa, *Optical rotation in the lithium triborate nonlinear crystal*, J. Appl. Cryst. 56 (2023)](https://journals.iucr.org/j/issues/2023/02/00/gj5295/) — g₁₂ = 4.31e-5, ρ = 7.06 deg/mm along an optic axis
- [Kobayashi & Uesu, HAUP](https://onlinelibrary.wiley.com/doi/10.1107/S0021889883010493) and [Glazer/Kaminsky tilter method](https://iopscience.iop.org/article/10.1088/0953-8984/9/49/004) — optic-axis and tilt-sweep practice, real sample thicknesses
- [Arteaga, Canillas & Jellison, *Determination of the components of the gyration tensor of quartz by oblique incidence transmission two-modulator generalized ellipsometry*, Appl. Opt. 48, 5307 (2009)](https://opg.optica.org/ao/abstract.cfm?uri=ao-48-28-5307) — order-independent ratio observable on 1.02/1.06 mm plates
