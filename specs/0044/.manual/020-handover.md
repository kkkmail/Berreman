# Spec 0044 — handover

Restart point for a fresh session. Read **Status**, then **The map**, then **Traps**; everything else is
reference. Written at the end of the session that delivered manual task `016` (reports `017`–`019`).

---

## Status

**All work through manual task `016` is complete, green and COMMITTED.** `git status` is clean apart
from an untracked `.claude/` that predates this work and is not ours.

| Suite | Tests | Last measured runtime | State |
|---|---|---|---|
| `MuellerInverseTests` | 20 | 456 s | green (task 010/014) |
| `BiaxialInverseTests` | 11 | 997 s | green (task 012/014) |
| `FeasibleBiaxialInverseTests` | 5 | 104 s | green (task 016 pt 1) |
| `AbsorbingBiaxialInverseTests` | 7 | 177 s | green (task 016 pt 2) |
| `AbsorbingBiaxialNoisyTests` | 1 | **946 s** | green (task 016 pt 3) |
| `LangasiteInverseTests` | 6 | not timed | green (task 007) |
| **whole `BerremanTests` assembly** | **203** (5 skipped) | **~30 min, inferred** | 0 failed |

The 5 skips are pre-existing `SolverTests` wedge/Mueller facts and have nothing to do with this spec.

Clean `dotnet build Berreman.slnx -c Release --no-incremental` → **0 errors, 4 warnings**, all
pre-existing and outside our code (2 × `NU1701` from Wolfram.NETLink, 2 × `SYSLIB0051` from vendored
MathNet C#). That 4-warning baseline is the acceptance bar; anything else is ours to fix.

### The task/report ledger

| Task | Report | Subject |
|---|---|---|
| `005` | `006` | the original spec implementation (slices 0–8) |
| `007` | — | `LangasiteInverseTests`, real measured data |
| `010` | `011` | noisy measurement, uniaxial quartz |
| `012` | `013` | biaxial triclinic, 9 parameters |
| `014` | `015` | noisy measurement, biaxial |
| `016` | `017`, `018`, `019` | feasible experiments; absorbing 15-parameter; absorbing noisy |

`011` §N5 carries a **struck-through correction** — read it, it is the cautionary tale in this spec.

---

## The map

### Compile order (`BerremanTests.fsproj`) — load-bearing

```
MuellerReconstructionTests.fs
InverseFitHarness.fs          705 lines   shared machinery, namespace-level types
BiaxialSample.fs              186 lines   transparent triclinic ground truth
MuellerInverseTests.fs       1377 lines   uniaxial quartz, 4 params (+ noisy)
BiaxialInverseTests.fs       1016 lines   triclinic, 9 params (+ noisy)
FeasibleBiaxialInverseTests.fs 487 lines  triclinic, 9 params, REAL experiment
AbsorbingBiaxialSample.fs     197 lines   absorbing triclinic ground truth
AbsorbingBiaxialInverseTests.fs 423 lines absorbing triclinic, 15 params
AbsorbingBiaxialNoisyTests.fs  203 lines  absorbing triclinic, 15 params, noisy
LangasiteInverseTests.fs      577 lines   real measured data, 1 param
```

A sample module must compile **after** `InverseFitHarness` (it names `RecoveredQuantity`) and **before**
every suite that opens it. `AbsorbingBiaxialSample` additionally needs `BiaxialSample` (it reuses the
transparent constants).

### Three layers

**1. `OpticalConstructor.Domain/MuellerInverse.fs` — generic in `'Parameters`.**
Nothing here knows a material or a parameter count. `ParameterAxis<'P>` (`name` / `read` / `write`) is
what makes that work; a material supplies a list of them and everything downstream —
`ParameterScaling<'P>`, `toScaled`, `ofScaled`, `ForwardModelProxy<'P>`, `createBerremanForward`,
`forwardModels`, `jacobianColumnNorms`, `jacobianCondition`, `jacobianCorrelation` — is written once.
Parameter records living here: `UniaxialParameters` (4), `TriclinicParameters` (9),
`AbsorbingTriclinicParameters` (15), each with a `static member axes`.

Also here: `SampleCut` (`ZCut | XCut | YCut | TiltedCut of Angle`), `orientForCut`,
`MeasurementConfiguration`, `normalizeMueller`, `residualVector`, `analyticInversion`,
`cloudeEigenvalues`, `depolarizationIndex`.

**2. `BerremanTests/InverseFitHarness.fs` — the shared test machinery.**
Namespace-level types (visible to every suite without an `open`): `RelativeError`, `SearchBox`,
`InverseFit<'P>`, `NoiseSeed`, `AngleUncertainty`, `ElementUncertainty`, `ThroughputUncertainty`,
`NoiseParam`, `UniformDeviateProvider`, `RecoveredQuantity<'P>`, `RecoveryStatistics<'P>`,
`NoisyExperiment<'P>`.
Module functions: `configurationOf`, `observeWith`, `residualFor`, `throughputResidualFor`,
`directionalSensitivity`, `sumWeights`, `differenceWeights`, `combinationVariance`, `scalingAround`,
`fitWithResidual`, `fitObservations`, `fitAbsorbingObservations`, `recoveryErrors`, `describeErrors`,
`describeColumnNorms`, `assertMuellerEqual`, `ensembleSeeds`, `disturb`, `misread`, `misreadAbsolute`,
`noisyObserveWith`, `noisyObserveAbsoluteWith`, `statisticsFor`, `standardErrorOfMean`, `rowFor`,
`predictedRelativeErrors`.

**3. The sample modules and the suites.** Spec R3 keeps materials in the TEST assembly, not in
`OpticalProperties`. `BiaxialSample` and `AbsorbingBiaxialSample` exist because more than one suite
measures the same crystal with a *different experiment*, and the comparison only means anything if the
sample is held fixed.

---

## Standing operator rulings

These accumulated across `010` → `016`. They are binding until countermanded.

1. **Do not touch existing test classes** except to extract common parts.
2. **Run only the new tests.** Runtime is prohibitive. *"This is F# — if it compiles after the
   refactoring, then it works."* Do not run the pre-existing suites even after extracting from them.
3. **Never combine measurements across wavelengths.** Dispersion makes the constants
   wavelength-dependent, so one parameter set cannot describe two colours. A multi-colour experiment
   requires fitting a dispersion model — a different, larger inverse problem. *(This corrected a real
   error in the first draft of task 016 part 1.)*
4. **Extract, do not copy/paste/modify.**
5. **Reports go into the next `NNN`**, written once the main (no-noise) test passes.
6. **LF line endings.** `.gitattributes` stores all text as LF and `core.autocrlf` is false. Verify with
   `git diff --stat` against `git diff --ignore-cr-at-eol --stat` after editing.
7. **The §9 pinning protocol.** Make the test fail first with an impossible band (`1.0e-30` / `1.0e30`),
   harvest the printed actual values, then pin at a modest multiple. Never guess a band.
8. **Never fabricate agreement with literature.** Where synthetic constants are used, say so loudly and
   in the test itself.

---

## Traps — the expensive knowledge

Each of these cost at least one wasted cycle. Do not re-derive them.

### Engine / physics

- **The engine's `Rho` is the bi-anisotropic (Tellegen–Post) tensor, not the crystallographic gyration
  tensor.** Its IMAGINARY part drives optical rotation at first order. Its **REAL part has NO
  first-order effect at all** — a ×1000 increase multiplies the resulting circular dichroism by 1.7e7,
  not by 1000. **Circular dichroism is therefore not expressible through ρ in this engine**, which is
  why the absorbing suite carries 15 parameters and not 21. (`018`, and the first inverse fact in
  `AbsorbingBiaxialInverseTests`.)
- **Biradial ≠ binormal.** Two textbook formulas both called "the optic axis". For a plane wave at
  normal incidence the **binormal** `tan²V = (n₁⁻²−n₂⁻²)/(n₂⁻²−n₃⁻²)` is the right one. For the LBO
  sample they differ by 0.73°, which on a 100 µm plate is **29.6° of stray retardance against
  1.5e-5°** — the difference between an optic-axis plate and not one. `BiaxialSample` exposes both;
  `opticAxisFromLargest` is the biradial, `opticAxisWaveNormal` is the binormal.
- **`IncidenceAngle.create` folds its argument into [0, 90) modulo 90°.** A −0.2° excursion from normal
  incidence comes back as **89.8°**. Never use it for a perturbed angle — use the engine's own
  `IncidenceAngle + Angle` operator, which does not clamp. (`InverseFitHarness.disturb`.)
- **`Eps.create` / `Rho.create` are the only routes to an arbitrary complex tensor.**
  `Eps.fromComplexRefractionIndex` is diagonal-only, `Eps.fromRe` is real-only, there is no `Eps.fromIm`
  and no `Rho.fromRe`.
- **`OpticalProperties.rotate` conjugates eps, mu and rho TOGETHER.** There is no per-tensor rotate. To
  give ε′ and ε″ different principal frames, build the complex tensor directly rather than rotating.
  (The repo's own splice idiom is at `SolverTests.fs:1039-1040` if you need one tensor from a rotated
  copy.)
- In an **absorbing** biaxial crystal the two optic axes split into four *singular* optical axes, so a
  transparent binormal cut is only approximately retardance-free.

### Numerics / fitting

- **`FitQuality.residualJacobian` steps each parameter by `√eps · max(|x_p|, 1e-12)`** — RELATIVE, so it
  collapses to ~1e-20 at exactly `x = 0` and reports every column dead. **Never evaluate a Jacobian at
  the origin of the scaled space.** Centre the scaling on the perturbed start and evaluate at the truth.
- **`FitQuality`'s covariance underflows on noiseless data** (finding F3): `Cov = reducedχ²·(JᵀJ)⁻¹`,
  reduced χ² ~1e-30, every standard error and correlation comes back a flat 0. Use `jacobianCorrelation`
  for the noiseless case. With real noise present the covariance works and agrees with an ensemble to
  within a factor of ~2 across four decades of uncertainty.
- **The scale of a DIFFERENCE of two coordinates sharing a scale is that scale, not the difference of
  the scales.** Getting this wrong produced a factor of exactly zero and a confidently wrong published
  finding — see the struck-through `011` §N5. Use `differenceWeights` + `combinationVariance` and write
  the quadratic form as `wᵀCw` (which also avoids assuming the numerically-inverted matrix is exactly
  symmetric).
- **Parallelising ensemble seeds is SLOWER, measured twice.** Two seeds in parallel took 184 s against
  ~100 s serially; eight took 504 s against 365 s. The forward solves allocate heavily and extra threads
  buy contention. Run seeds serially.
- **A blind start converges at 9 parameters and NOT at 15.** The absorbing suite's blind isotropic start
  gave 240 iterations, χ² = 36.8 (a converged fit reaches 1e-23) and `g23` out by a factor of six.
  Fifteen unknowns need a starting point.
- **The RNG draw ORDER is load-bearing.** Every noisy suite's pinned numbers depend on it. `misread`
  consumes 15 draws (m₀₀ skipped); `disturb` consumes 2; `misreadAbsolute` consumes 1 + 15, throughput
  first. **Adding a draw to an existing function silently invalidates every pinned band in every suite
  that uses it** — which is why `misreadAbsolute` was added beside `misread` rather than replacing it.
  Seeded `System.Random` keeps the legacy algorithm, so streams are reproducible across runtime versions.

### Tooling

- **`dotnet test` does not work here.** The project is on Microsoft.Testing.Platform and the VSTest path
  errors out. Run the built executable directly:
  `Berreman/BerremanTests/bin/x64/Release/net10.0/BerremanTests.exe`
  with `-class "BerremanTests.X"`, `-method "*pattern*"`, `-method- "*exclude*"`, `-list methods`.
- Build with `dotnet build Berreman.slnx -c Release` from the `Berreman/` subdirectory (NOT MSBuild —
  that is the *Softellect* repo's rule, not this one).
- **Do not rebuild while a test run holds the exe** — the file is locked and the build fails.
- Assertion messages are the harvest channel: xUnit stops at the first failing assert per test, so put
  everything you need to read into that one message.

---

## Runtime — plan around it

| | |
|---|---|
| one forward solve | ~10–14 ms |
| one LM iteration | `(nParams + 1) × nConfigs` solves, plus line search |
| 9-param noiseless fit, 40–46 configs | 12–28 iterations, ~60–100 s |
| 15-param noiseless fit, 44 configs | 22 iterations, ~150 s |
| 15-param **noisy** fit | 28–62 iterations, ~190 s each |

**The single most expensive test in the repository is `AbsorbingBiaxialNoisyTests` at 946 s** (five
15-parameter noisy fits). `BiaxialInverseTests` is second at 997 s for 11 facts, dominated by its
eight-seed ensemble.

Levers, in order of least damage: **fewer seeds** (5 seeds ≈ 35 % uncertainty on the standard deviation
against 8 seeds' 27 %); **a looser `epsX` for noisy fits only** (polishing to 1e-12 scaled units is far
below what the data supports, and noisy fits spend 2–3× the iterations of noiseless ones); **a reduced
configuration set**, which changes the experiment and should be last.

---

## Where the physics currently stands

| Suite | Crystal | Unknowns | Plate | Verdict on a 0.2° / 0.005 bench |
|---|---|---|---|---|
| `MuellerInverseTests` | uniaxial quartz | 4 | 1 mm + 20 µm | indices 0.05 %, `g₃₃` 4.6 % |
| `BiaxialInverseTests` | triclinic | 9 | 4 µm *(infeasible)* | indices 0.09 %, gyration 21–187 % |
| `FeasibleBiaxialInverseTests` | triclinic | 9 | **100 µm** *(vendor minimum)* | noiseless only; gyration Jacobian ×1.7–×25.9 better |
| `AbsorbingBiaxialInverseTests` | absorbing triclinic | 15 | 100 µm | noiseless only; condition 7677 |
| `AbsorbingBiaxialNoisyTests` | absorbing triclinic | 15 | 100 µm | absorption 0.05–0.51 %, gyration 2.4–312 % |

**The recurring result across every suite: linear effects (birefringence, dichroism) are easy and
optical activity is hard.** Absorption turned out to be the *easiest* thing in the absorbing problem —
its six Jacobian columns are an order of magnitude stronger than any gyration column.

**The recurring structural result: differences survive far better than levels.** Birefringences are
recovered 64–208× better than any single index in ABSOLUTE terms, in every ensemble, because linear
retardance depends on `Δn` and the common index level is the near-null direction. Always report the
differences; a table of only the fitted parameters understates the experiment by that factor.

---

## Candidate next slices

Nothing is owed — task `016` is closed. If work continues, these are the live threads, roughly in order
of value:

1. **A noisy run of `FeasibleBiaxialInverseTests`.** It is the only suite with no noisy half, and it is
   the one whose whole purpose is realism. `015` predicted the feasible design should give materially
   better gyration error bars than the 4 µm one; the Jacobian says so but no ensemble has confirmed it.
   Cost: ~8 nine-parameter noisy fits ≈ 15–20 min.
2. **Dispersion.** Every suite fixes one wavelength, and ruling 3 above means multi-colour data cannot
   be used without it. Fitting a Sellmeier-style dispersion model would unlock the standard laboratory
   answer to the retardance-order problem and is the single largest capability gap.
3. **The order-independent ratio observable (2-MGE).** Arteaga/Canillas/Jellison measure `CB/LB` from
   the Mueller matrix and never determine the retardance order at all, which is what let them work with
   1.02 mm quartz. It would remove the half-fringe precondition on the start guess that
   `FeasibleBiaxialInverseTests` currently has to assert.
4. **Cloude filtering in the pipeline.** Real measured matrices are never exactly Mueller–Jones; the
   analytic inversion assumes they are. A depolarisation/noise stage plus a Cloude filter would exercise
   the code path real data requires.
5. **Fitting thickness and orientation as nuisance parameters.** Both are known exactly today (spec R2),
   and plate thickness carries a ±20 µm vendor tolerance that the current tests treat as zero.
6. **The LDLB cross-talk adversarial case.** Construct a ground truth with zero circular dichroism but
   large `LD′·LB − LB′·LD` and assert the solver returns zero rather than the artifact. This is the most
   common real-world failure mode in crystal chiroptics and is exactly reproducible synthetically.

---

## If you change the harness

`InverseFitHarness` is now shared by five suites with pinned numbers. Before editing it:

- Adding or reordering a **random draw** invalidates pinned bands. Add a new function instead.
- Changing `residualFor`, `fitObservations` or `statisticsFor` changes results everywhere.
- Ruling 2 forbids re-running the pre-existing suites to re-pin — so a change that would need re-pinning
  is a change that needs the operator's agreement first, not a change to make and then discover.
