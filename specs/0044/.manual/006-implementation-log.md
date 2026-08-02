# Spec 0044 — implementation log

Implementing `004-active-anisotropic-inverse-problem.md` per the task in `005-task.txt`.
This file is the restart point: read the **Status** table, then **Next action**, and continue from there.

Slice numbering follows §10 of the spec.

---

## Status

| Slice | Title | State |
|---|---|---|
| 0 | Baseline build + test counts | **DONE** |
| 1 | Provenance (scripts, reference JSON, README, .gitattributes) | **DONE** |
| 2 | Archive-backed data seam | **DONE** |
| 3 | Golden precision literals | **DONE** |
| 4 | Quartz test data + forward configurations (T1, T2, T5) | **DONE** |
| 5 | Inverse primitives (T4) | **DONE** |
| 6 | Stage A analytic inversion (T3, T6) | **DONE** |
| 7 | Proxies (T10, T11) | **DONE** |
| 8 | Stage B fit + acceptance (T7, T8, T9) | **DONE** |

**Next action:** none — the spec is fully implemented. Three findings in §"Findings" below change
statements made in the spec itself and should be folded back into `004-…md` if it is ever revised.

**Gate state (final):** clean `--no-incremental` Release build succeeds with **zero warnings from our
code** (only the pre-existing 2 × `NU1701` and 2 × `SYSLIB0051` from vendored MathNet C#).
`BerremanTests` **165 passed / 5 skipped / 170 total** (baseline 146/5/151);
`OpticalConstructor.Tests` 693 passed; `OpticalConstructor.Ui.Tests` 685 passed.

---

## Findings that contradict or extend the spec

These were measured, not assumed. Each one changed the implementation.

### F1 — The engine's `Rho` is NOT the crystallographic gyration tensor

Measured by varying `g11`, `g33`, `n` and `d` independently through the forward model. For a z-cut
plate at normal incidence the engine's behaviour is exactly

```
specific rotation = 2π·g₁₁ / λ          (agreement with the closed form: 2.1e-16 relative)
```

— driven by the **transverse** `Rho` component `g₁₁`, **exactly independent of `g₃₃`**, and **exactly
independent of the refractive index**. `Rho` is therefore the bi-anisotropic (Tellegen–Post)
magnetoelectric tensor of `D = εE + ρH`, not the crystallographic gyration tensor `g` of
`D = εE + i(G × E)`. The two share the symbol `g` in the literature and in
`Active.Rho.type_3_4_6_Crystal`, but they are different parameterizations.

Consequences:

- Spec §4.1 asserted that C1 would reproduce the measured rotatory power of α-quartz, 18.8 °/mm. It
  does not: with quartz's constants in the engine's `Rho` slots a z-cut plate rotates at
  **33.57 °/mm**. To make the engine reproduce 18.8 °/mm the transverse component would have to be
  3.305e-5 rather than 5.9e-5.
- The T5 fact therefore asserts the engine's own closed form at machine precision (a stronger test —
  it pins the constitutive convention, the propagation and the plate handling at once) and documents
  the convention gap and the literature number in full. Fabricating agreement was not an option.
- Spec §3.2's role assignment is **inverted** for this engine: `G(θ) = g₁₁sin²θ + g₃₃cos²θ` is the
  crystallographic formula, under which the axial component is read along the axis. Here, propagation
  along the optic axis reads `g₁₁`, and `g₃₃` only becomes visible when the crystal 3-axis is
  transverse to the beam — i.e. from the x-cut. The *structure* of the argument (≥ 2 cuts required,
  Snell caps a single cut at 40.4° internal) is unchanged and is asserted by the ablation fact;
  only which component is "the hard one" swaps.
- `gyrationProjection` is retained as the literature bridge and documented as such; nothing uses it to
  predict the engine.

### F2 — Spec §3.4 mis-classified C2's thickness

§3.3 states the rule: keep every retardance-bearing configuration sub-wave so the χ² surface stays
unimodal. §3.4 then put C2 (z-cut, **oblique** incidence) at 1.0 mm, on the reasoning that a z-cut
plate carries no linear retardance. That holds only at *normal* incidence. Tilted, the internal angle
is non-zero, the effective birefringence grows as sin²θ, and a 1 mm z-cut plate at 70° carries ~5.9
waves — exactly the multimodality the rule exists to avoid. C2 therefore uses the same 20 µm plate
(~0.12 wave). C1 stays at 1.0 mm because it genuinely has zero linear retardance. This applies the
spec's own rule to a configuration the spec mis-classified.

### F3 — `FitQuality` cannot report correlation at a zero residual

Spec §3.5 argued that the parameter correlation matrix survives noiseless data because the `reducedχ²`
factor cancels in `corrᵢⱼ = covᵢⱼ / (σᵢσⱼ)`. That is true algebraically and **false in floating
point**: `FitQuality` forms `Cov = reducedχ²·(JᵀJ)⁻¹` first, and at `reducedχ² ~ 1e-30` the covariance
diagonal underflows, `stdErr` hits the `v > 0` guard, and every off-diagonal correlation comes back as
a flat `0.0` — indistinguishable from "perfectly independent", i.e. actively misleading. Measured
directly: all six pairs reported `0`.

Fix: `MuellerInverse.jacobianCorrelation` computes `corrᵢⱼ = Cᵢⱼ/√(CᵢᵢCⱼⱼ)` with `C = (JᵀJ)⁻¹`,
dropping the `reducedχ²` factor before the division. Cross-checked against the fit-based path in the
one regime where that path still works (non-zero residual): 0.9999895 vs 0.99998958 — same number.
`FitQuality` remains correct once real noise is present.

A related trap, same area: `FitQuality.residualJacobian` steps each parameter by
`√eps·max(|x_p|, 1e-12)`, which is **relative** and collapses to ~1e-20 at exactly `x = 0`. Evaluating
a Jacobian at the origin of the scaled space reports *every* column as dead. Both diagnostic facts
therefore evaluate at the ground truth expressed in a scaling centred on the perturbed start, and
assert up-front that no coordinate is zero.

---

## Baseline (slice 0)

- `dotnet build Berreman.slnx -c Release` — **succeeded**, 0 errors, 4 warnings, all pre-existing and
  outside our code: 2 × `NU1701` (Wolfram.NETLink fallback restore, exempt per CLAUDE.md) and
  2 × `SYSLIB0051` from vendored `MathNetNumerics/Numerics/Exceptions.cs`.
- `dotnet test --no-build -c Release` in `Berreman/BerremanTests` — **Failed: 0, Passed: 146,
  Skipped: 5, Total: 151**. The 5 skips are pre-existing `SolverTests` wedge/Mueller facts.

This machine still has the external checkout present, so the two data-backed facts were already
running rather than skipping; on a clean machine the baseline would have been 144.

---

## Slice 1 — provenance — DONE

Copied from `https://github.com/NihilismVoid/optics-mueller.git`, branch `main`, commit
`2f1a35b70f877e41fa9b576fadd138d7dc80b858`, into `Berreman/Data/MuellerMatrix/`, converting CRLF → LF
(all six sources were CRLF):

| New file | Source |
|---|---|
| `matrix_step1_air_fit.py` | `analysis/scripts/matrix_step1_air_fit.py` |
| `matrix_glue.py` | `analysis/scripts/matrix_glue.py` |
| `matrix_fit_linear.py` | `analysis/scripts/matrix_fit_linear.py` |
| `cpl_cpl_analyzer.py` | `analysis/scripts/cpl_cpl_analyzer.py` |
| `reference_step1_summary.json` | `analysis/matrix_solution/step1/summary.json` |
| `reference_step2_linear_summary.json` | `analysis/matrix_solution/step2_linear/summary.json` |

Plus `README.md` with the provenance table, the full `data.zip` inventory (7 consumed data sets with
row counts, 11 unconsumed exploratory entries), per-file descriptions, and the statement that nothing
builds or executes the Python.

`.gitattributes`: added `*.py text eol=lf` and `*.zip binary`.

Spec §8.5 confirmed against reality: the reference result files exist and are complete. No Python
needed to be run.

---

## Slice 2 — archive-backed data seam — DONE

**Domain (`MuellerReconstruction.fs`):** added `MuellerDataSet` (7 cases, pure identity — no path,
entry or archive knowledge on it or any member); added `ArchiveUnreadable of archive * reason` and
`DataSetMissing of dataSet * archive` to `MuellerDataError`; re-keyed `MuellerDataProxy` from
`tryLoadFamily : DataFilePath -> …` to `tryLoadDataSet : MuellerDataSet -> …`; dropped the now-unused
`open …Experiments`.

**Storage (`MuellerDataStore.fs`):** added `MuellerArchivePath` (in Storage, not Domain, so nothing
above the boundary learns the data is packaged as an archive); private `entryName` (the one place the
identity → location mapping exists) and private `committedArchive ()`; public
`tryLoadFromArchive : MuellerArchivePath -> MuellerDataSet -> Result<…>` (whole IO block under
`try/with`); `createArchiveBacked () : MuellerDataProxy` taking **no argument**. **Deleted**
`createFileBacked` and its private `loadThrough`. `parseMuellerCsv` untouched.

**Confirmed (spec §12 item):** `System.IO.Compression` needs **no** `PackageReference` on `net10.0`.

**Tests:** mock re-keyed on `MuellerDataSet`; `tryFindOpmFinalDir ()` and both `Assert.Skip` sites
deleted; `loadDataSet` fails with the typed error instead. The `createFileBacked` missing-file fact
was replaced by three: `createArchiveBacked` loads all seven data sets with their documented row
counts (209/209/209/170/57/6/2); `tryLoadFromArchive` maps an absent archive to `ArchiveUnreadable`;
and maps an archive lacking the entry to `DataSetMissing` (built on a throwaway temp zip, deleted in a
`finally`). The §7.2 test body was de-indented out of its `match … | Some finalDir ->` branch.

**Project:** `BerremanTests.fsproj` gained the `Content` copy of `..\Data\MuellerMatrix\data.zip`.

**Cleanup:** per spec §8.4, the literal strings `optics-mueller` and `OPM` no longer appear anywhere
under `.fs` / `.fsproj` — only in this spec folder and in `Data/MuellerMatrix/README.md`. Verified by
grep.

---

## Slice 3 — golden precision — DONE

Replaced the report-rounded expected values with the reference pipeline's own full-precision output,
and restructured both data-backed facts to report the **worst** deviation across all compared
quantities in one assertion rather than failing at the first element out of band.

**Measured** (bands set to 1e-30 to force a full report):

| Quantity | Observed deviation from the reference |
|---|---|
| `dark_mean`, `z_LP`, `z_CPL`, `δ_an` | **0** — bit-exact |
| `θ_src` | 7.105e-15 ° |
| `δ_src` | 1.421e-14 ° |
| `M_QZ` | 4.492e-11 (worst element, M[2,3]) |
| `M_LR` | 4.888e-11 (worst element, M[0,0]) |
| `M_{QZ+LR}` | 3.873e-11 (worst element, M[1,3]) |

Four of six calibration constants reproduce bit-exactly; the other two differ by one to two ulp, and
that ulp-level difference is amplified to ~5e-11 through the 900×16 least-squares solve (condition
number ≈ 10).

**Pinned:** `stage1Tol = 1.0e-12` (≈70× observed), `dataMatrixTol = 1.0e-9` (≈20× observed) — 7–8
orders tighter than the 1° / 2e-3 bands they replace.

**Corroboration:** the reference `delta_matrix` independently confirms the 0042 data-driven
correction — the two largest cascade differences are at 0-indexed (3,3) = −0.26806 and
(3,1) = −0.26783, not the (4,2)/(2,2) the 0042 slice text named.

---

## Slices 4–7 — quartz, inverse primitives, Stage A, proxies — DONE

**New `OpticalConstructor.Domain/MuellerInverse.fs`** (material-agnostic — names no crystal class and
no material, so the Domain still needs no reference to `OpticalProperties`):

- elevated inputs: `OpticAxisCut`, `SampleAzimuth`, `Observable`, `MeasurementConfiguration`,
  `MaterialParameters`, `MuellerObservation`;
- forward helpers: `gyrationProjection`, `internalAngle`, `orientForCut`;
- reduction: `normalizeMueller`, `normalizedElements`, `observationResidual`, `residualVector`,
  with `InversionError`;
- Stage A: `Dichroism`, `ElementaryAnisotropy`, `muellerOfBirefringence` (Rodrigues),
  `analyticInversion` (rotation logarithm), `transparencyResidual`, `depolarizationIndex`,
  `cloudeEigenvalues`;
- Stage B seams: `ParameterScaling` / `toScaled` / `ofScaled`, `ForwardModelProxy` +
  `createBerremanForward`, `forwardModels`, `NonlinearRequest` / `NonlinearSolution` /
  `NonlinearSolverError` / `NonlinearSolverProxy`;
- diagnostics: `jacobianColumnNorms`, `jacobianCondition`, `jacobianCorrelation` (see F3).

**New `OpticalConstructor.Optimization/MuellerInverseSolver.fs`** — `createAlglibLevenbergMarquardt`,
the real backend behind the Domain-declared seam. It lives in Optimization because that is the only
project allowed to name ALGLIB.

**`cloudeEigenvalues` implementation note.** `Berreman/MatrixEvd.fs` exists in the source tree but is
**not in `Berreman.fsproj`'s compile list**, so `ComplexMatrix.evd` is unavailable. The coherency
matrix is instead assembled from its definition as a sum of Pauli outer products (checkable by eye,
unlike transcribed element formulas) and decomposed through the backing MathNet matrix, reached the
same way `createMathNetSvd` reaches `.Svd()`.

**New `BerremanTests/MuellerInverseTests.fs`** — quartz ground truth (spec R3: in the test, not the
library; `OpticalProperties` untouched), the C1–C4 configuration set, and facts T1–T6, T10, T11.

---

## Slice 8 — Stage B fit and acceptance — DONE

**Configuration set as built:** C1 = 1 (z-cut 1.0 mm, normal, transmission); C2 = 16 (z-cut 20 µm,
10/30/50/70°, azimuth 0/45°, T and R); C3 = 4 (x-cut 20 µm, normal, azimuth 0/22.5/45/67.5°, T);
C4 = 8 (x-cut 20 µm, 15/45°, azimuth 0/45°, T and R). 29 configurations → 435 residual entries
against 4 unknowns.

**Start guess:** `g₁₁`, `g₃₃` 30 % wrong; `n_o` +0.2 %, `n_e` +0.1 %. The index perturbations look
small but move the *birefringence* by −17 %, which is what the retardance depends on. They cannot be
larger for a structural reason: retardance enters through cos/sin of `2π(n_e−n_o)d/λ`, so a start more
than about half a fringe away lands in a different basin. That is physics, not optimizer weakness.

**Measured results** (all bands then pinned per the §9 protocol):

| Quantity | Observed | Pinned band |
|---|---|---|
| T7 relative error, `n_o` | 1.87e-15 | < 1e-11 (≈70× worst) |
| T7 relative error, `n_e` | 1.86e-15 | < 1e-11 |
| T7 relative error, `g₁₁` | 1.84e-15 | < 1e-11 |
| T7 relative error, `g₃₃` | 1.39e-13 | < 1e-11 |
| T7 final χ² | 1.40e-22 | < 1e-18 |
| T7 iterations | 13 | > 0 |
| T8 Jacobian condition number | 436.4 | < 5e3 |
| T8 column norms (`n_o`, `n_e`, `g₁₁`, `g₃₃`) | 0.934, 0.934, 0.282, 0.0079 | each > 1e-6 |
| T8 correlation `n_o`/`n_e` | 0.9999895 | < 0.999999 |
| T8 all other correlation pairs | ≤ 0.0443 | < 0.5 |
| T9 C1-only column norms | 3.84e-4, **0**, 0.281, **0** | see below |
| T6 depolarization, transmission worst | 0.0295 (C2-T) | < 0.05 |
| T6 depolarization, overall worst | 0.416 (C4-R) | < 0.5 |
| T5 rotation vs closed form | 2.1e-16 relative | < 1e-12 |

**T8 interpretation.** `n_o` and `n_e` are correlated at 0.99999, and that is physical rather than a
defect: linear retardance depends on the *difference* (0.009) against absolute indices of 1.54, so the
data constrains the difference ~170× better than the common level and moving both together is nearly a
null direction. The fit still resolves both, because the Fresnel amplitudes and the internal
refraction angle do depend on the absolute level — which is exactly what the oblique-incidence and
reflection configurations contribute. Every other pair is ≤ 0.044.

`g₃₃`'s Jacobian column is 118× weaker than `n_o`'s — it is the poorly-determined constant here, the
mirror image of the literature's `g₁₁` (Arteaga et al. report their axial component to 0.7 % and their
transverse one to ~10 %). Same structural cause, roles swapped by F1.

**T9 (ablation) — the executable answer to "what experiments are needed".** From C1 alone:
`n_e` and `g₃₃` have **exactly zero** Jacobian columns (structurally invisible: the crystal 3-axis is
parallel to the beam); `g₁₁` is strongly constrained (0.281); and `n_o` survives only through a
second-order channel at 1/730 of `g₁₁`'s sensitivity. That channel is real, not noise — the plate emits
a direct beam rotated by ψ and a twice-bounced beam rotated by 3ψ, and `n_o` sets their relative
weight through the surface reflectance. Because 2×2 rotations commute and form a one-parameter group,
that weighted sum is *exactly* a scaled rotation, which is why C1 shows no depolarization at all
(1.1e-16) despite summing two beams — but its angle shifts with the weight. Adding C3, the second
crystal cut, restores all four columns.

**T6 interpretation.** Reflection depolarizes far more than transmission (0.42 vs 0.03) because it has
no dominant term: the front-surface reflection (~4.5 %) and the internally-reflected beam (~4.1 %) are
comparable in amplitude while transforming polarization differently. This is correct physics for an
incoherent multiple-reflection model, so the fact asserts two bands — tight for transmission, loose for
reflection — rather than one slack one. Consequence recorded in the test: the closed-form analytic
inversion is meaningful for transmission and not for reflection; the nonlinear fit is unaffected
because it compares against the full Berreman model, which reproduces the depolarization exactly.

**Runtime.** The `MuellerInverseTests` class takes ~58 s, dominated by T7's fit (13 LM iterations ×
5 residual evaluations × 29 forward solves ≈ 1 900 solver calls). T8 and T9 deliberately do **not**
re-run the fit — identifiability is a property of the Jacobian at the solution, and for noiseless data
the solution is the ground truth, so both evaluate there directly. That keeps them at ~4 s each and
makes them statements about the experiment design rather than about one optimizer run. Angle grids
were left at the spec's density since the runtime is acceptable; §7's pre-authorized trim was not
needed.

---

## Files changed

**New:** `Berreman/Data/MuellerMatrix/{README.md, matrix_step1_air_fit.py, matrix_glue.py,
matrix_fit_linear.py, cpl_cpl_analyzer.py, reference_step1_summary.json,
reference_step2_linear_summary.json}`; `OpticalConstructor.Domain/MuellerInverse.fs`;
`OpticalConstructor.Optimization/MuellerInverseSolver.fs`; `BerremanTests/MuellerInverseTests.fs`.

**Modified:** `.gitattributes`; `OpticalConstructor.Domain/{MuellerReconstruction.fs,
OpticalConstructor.Domain.fsproj}`; `OpticalConstructor.Optimization/OpticalConstructor.Optimization.fsproj`;
`OpticalConstructor.Storage/MuellerDataStore.fs`; `BerremanTests/{MuellerReconstructionTests.fs,
BerremanTests.fsproj}`.

All files verified LF (zero CR bytes).
