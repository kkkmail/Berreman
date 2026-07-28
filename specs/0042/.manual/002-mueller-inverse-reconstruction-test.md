# Spec 0042 (preliminary) — Reproduce the Mueller-matrix inverse problem (report §7.2) as a UI-less F# test in BNM

Status: **preliminary / for review.** Open questions are collected at the end (§13).

Author-facing sources this spec was built from:

- Report: `specs/0042/.manual/MuellerMatrix_final.pdf` (Russian; §7.2 is the target).
- Experiment repo (`OPM`): `C:\GitHub\optics-mueller\` — setup `notes/setup_intro.txt`, model
  `notes/matrix_solution.md`, normalization caveats `notes/final_normalization_caveat.md`,
  data `data/raw/final/*.csv`.
- **Reference algorithm (authoritative, exact):** `C:\GitHub\optics-mueller\analysis\scripts\`
  `matrix_step1_air_fit.py` → `matrix_glue.py` → `matrix_fit_linear.py` (plus
  `cpl_cpl_analyzer.py::parse_experiment_name`). The PDF is a human summary of what these three
  scripts compute; when the spec and the PDF seem to disagree, the scripts win.

---

## 1. Purpose and scope

Write **UI-less test(s)** in the BNM repo that reproduce report §7.2: reconstruct the Mueller
matrices of two samples — **QZ** (quartz, ≈ optically-active pure rotator) and **LR** (Lego red
transparent plastic, a "messy" polymer) — from rotating-analyzer intensity data, reconstruct the
combined-stack matrix, and confirm the cascade identity

```
M_{QZ+LR}  ≈  M_LR · M_QZ          (beam hits QZ first, then LR)
```

holds "within experimental accuracy," matching the report's Frobenius/element metrics.

Constraints from the task:

- It is **OK to hardcode all procedures** for the test (calibration constants, gain rules,
  the excluded point, the split time). We are mimicking a specific published result, not building
  a general polarimeter.
- **Many tiny pure reusable functions** doing small steps — not one giant function.
- **IO handled by a `*Proxy`** (the repo's established shape); the numeric core stays pure.
- The test **pulls data directly from the `OPM` repo via a relative path** (no copy of the data
  into BNM, unless we decide otherwise — see §13 Q3).

### 1.1 Headline finding (answers the task's central question)

**The entire pipeline — calibration *and* reconstruction — is linear least squares** plus a couple
of closed-form trig steps. In the reference scripts, every fit is `numpy.linalg.lstsq` on a
2-column (calibration cosine) or 16-column (Mueller `vec`) design matrix; retardances come from
`arccos(visibility)`. **There is no Levenberg–Marquardt / nonlinear / global optimizer anywhere.**

Therefore: **the currently-available F# numerics can do this with zero external interop.** The
16-unknown over-determined linear solve is `MathNet.Numerics` `MultipleRegression.Svd` (vendored,
already referenced). ALGLIB's live `minlm` is *not even needed* for the core result. **No Python,
no Wolfram.** (Details and citations in §9.)

---

## 2. The measurement model (what the test implements)

For one intensity reading (report §2, §7.1; `matrix_glue.py`):

```
I = e0ᵀ · M_A(α) · M_O(φ) · M_S · S_laser          e0ᵀ = [1,0,0,0]
```

Absorb the fixed source and analyzer into effective states, and push the object's physical
rotation `φ` into those states so the sample matrix stays at its base orientation `M(0)`:

```
s0        = M_S · S_laser                 (effective source Stokes)
a0ᵀ(α)    = e0ᵀ · M_A(α)                   (effective analyzer row = row 0 of the analyzer Mueller)
s_eff(φ)  = R(φ) · s0
a_effᵀ(α,φ) = a0ᵀ(α) · R(−φ)
I         = a_effᵀ(α,φ) · M(0) · s_eff(φ)
```

`R(φ)` is the Stokes/Mueller **frame rotation** carrying `cos 2φ / sin 2φ`. For a single sample the
observable is **linear in `vec(M)`**:

```
I = ( s_eff ⊗ a_eff )ᵀ · vec(M)            (column-major vec; see §5.3 for exact index order)
```

The combined stack `QZ+LR` is reconstructed the *same way* (as one 16-parameter unknown), then
compared against the independently-formed product `M_LR · M_QZ`.

### 2.1 Component models (all four source/analyzer families)

| Element | Model | Constant(s) — hardcodable, from report Table 1 / step-1 script |
|---|---|---|
| LP source (LP1) | linear Stokes `s0 = [1,1,0,0]` | axis aligned to laser (base frame) |
| CPL source (CPL1) | `s0 = M_retarder(θ_src, δ_src) · [1,1,0,0]` | θ_src = **39.31°**, δ_src = **82.55°** |
| LP analyzer (LP2) | `M_A = M_linPol(β)`, `β = −(desc − z_LP)` | z_LP = **155.44°**, dial sign **−1** |
| CPL analyzer (CPL2) | `M_A = M_linPol(β) · M_retarder(β+45°, δ_an)`, `β = +(desc − z_CPL)` | z_CPL = **97.00°**, dial sign **+1**, θ_an = **45°**, δ_an = **−83.67°** |

Notes carried from the reference:

- `M_retarder(axis, δ)` is a linear retarder: `R(−axis) · diag-core · R(axis)` with core
  `[[1,0,0,0],[0,1,0,0],[0,0,cosδ,sinδ],[0,0,−sinδ,cosδ]]`.
- The **retardance sign** is only constrained *relatively* by the CPL-CPL AIR check (source and
  analyzer rotate in opposite senses → analyzer `δ` is stored **negative**). Hardcode source `+`,
  analyzer `−` (matches `matrix_glue.py::load_step1_models`).
- The analyzer's effective row is **row 0** of `M_A` (`a_base = M_A[0, :]`).

---

## 3. The reference algorithm, stage by stage

Three deterministic stages. All hardcodable; all pure except the CSV read.

### Stage 1 — AIR calibration (`matrix_step1_air_fit.py`)

Only AIR series (no sample ⇒ `M_O = I`). Per family: group rows by `description`, average
`avg_total`, subtract `dark_mean`, normalize by the trace mean → `norm`. Then:

- **LP-LP** → free cosine fit `norm − 1 ≈ p·cos2α + q·sin2α` (2-column `lstsq`): gives LP2 zero
  `z_LP = ½·atan2(q,p)` = **155.44°**, repeat spread ≈ 0.45°.
- **CPL-LP** (source) → fit in the LP frame, then closed-form `derive_source_cpl_from_lp_frame`
  → θ_src = **39.31°**, δ_src = **82.55°** (RMSE 0.0551).
- **LP-CPL** (analyzer) → free cosine fit → z_CPL = **97.00°**; δ_an = `arccos(visibility)` =
  **83.67°** (RMSE 0.0308).
- **CPL-CPL** → consistency check that fixes *relative* handedness (opposite).

Everything here is linear-LSQ + `arccos`. **The test may skip Stage 1 and hardcode the five
constants above** (report Table 1), or port it to re-derive them (tighter reproduction; see §13 Q1).

### Stage 2 — "Glue": per-row effective states + design row (`matrix_glue.py`)

For every science row (all 5 CSVs):

1. Parse the `experiment` label → `(source, analyzer, obj1, φ1, obj2, φ2, note)`.
2. `φ` from the label (0/45/90); combined rows require `φ1 == φ2`; `matrix_kind ∈ {qz, lr, qz_lr_product}`.
3. `β = dial_sign · (description − zero)`; build `s_base`, `a_base(β)` from the family model (§2.1).
4. `s_eff = R(φ) · s_base`; `a_eff = a_base · R(−φ)`; `lincoef = kron(s_eff, a_eff)` (16-vector).

### Stage 3 — Linear fit + cascade comparison (`matrix_fit_linear.py`)

1. `dark_mean = mean(avg_total)` over `darkness_checks.csv` (≈ **869.666**).
2. **Per-family AIR gain** `g` (multiplicative source-drift correction),
   `g = Σ(signal·model) / Σ(model²)` over that block's AIR rows, where
   `signal = avg_total − dark_mean`, `model = a_eff·s_eff` (= AIR identity prediction). Rules:
   - `cpl_cpl_day1`: **interpolate** `g` in time between `CPL-(AIR#0)-CPL` and `…-CPL-BR` using `captured_at`.
   - `cpl_cpl_day2`: single `g` from `CPL-(AIR#0)-CPL-2`.
   - `lp_lp`, `lp_cpl`: mean of the two AIR repeats (`-2`, `-2R`).
   - `cpl_lp`: **split at the first BullshitCheck** (`2026-05-31T01:10:40Z`); pre → `AIR-2`, post → `AIR-2R`.
3. `signal_corrected = (avg_total − dark_mean) / g`.
4. For each `matrix_kind`, build `X` (n×16 of `lincoef`) and `y = signal_corrected`, excluding AIR
   rows and the one contaminated point (`LP-(LR#90)-CPL-2`, `description=140`, `capture_index=17`).
   Solve `β = lstsq(X, y)`; reshape column-major → `M`. Report rank (=16), condition number, RMSE.
5. `M_product = M_LR · M_QZ`; compare `M_{QZ+LR}` vs `M_product` (Frobenius, mean|Δ|, max|Δ|).

---

## 4. Target numbers to assert (report §7.2 / §8)

These are the reference outputs the test reproduces (tolerances in §13 Q1).

Fit RMSE (intensity residual): **QZ 0.056, LR 0.091, QZ+LR 0.102.** Each solve is full rank 16.

```
M_QZ =                                    M_LR =
[  0.941  0.041 -0.026 -0.122 ]           [  0.993 -0.005  0.060 -0.104 ]
[  0.053  0.661 -0.366 -0.142 ]           [  0.353  0.147 -0.095 -0.657 ]
[ -0.013  0.406  0.609 -0.017 ]           [  0.343 -0.395  0.690 -0.597 ]
[ -0.096  0.075  0.018  1.031 ]           [ -0.010  0.125  0.461  0.591 ]

M_{QZ+LR} (fit as one object) =           M_LR · M_QZ (product of separate fits) =
[  0.900  0.083  0.035 -0.171 ]           [  0.944  0.054  0.010 -0.228 ]
[  0.341 -0.169 -0.158 -0.550 ]           [  0.404  0.024 -0.133 -0.739 ]
[  0.338 -0.105  0.768 -0.537 ]           [  0.350 -0.011  0.545 -0.613 ]
[  0.100  0.046  0.329  0.317 ]           [ -0.066  0.314  0.247  0.585 ]
```

Cascade comparison (§8), the **primary pass/fail**:

```
‖ M_{QZ+LR} − M_LR·M_QZ ‖_F = 0.571      mean|Δ| = 0.113      max|Δ| = 0.268
largest two element differences at (row,col) = (4,2) and (2,2), 1-indexed
```

Qualitative checks worth asserting: `M_QZ` ≈ the pure-rotator form
`[[1,0,0,0],[0,cos2ψ,sin2ψ,0],[0,−sin2ψ,cos2ψ,0],[0,0,0,1]]`; `M_LR` shows strong
linear↔circular coupling and row/column asymmetry.

---

## 5. Proposed F# design

### 5.1 Where it lives

**Primary home: `Berreman/BerremanTests`** — the only *truly* UI-less test project (no Avalonia;
references `OpticalProperties`, `Analytics`, `OpticalConstructor.Optimization`; xUnit v3 + FsCheck +
FluentAssertions; owns `MatrixComparison.fs`). `OpticalConstructor.Optimization` already brings in
`OpticalConstructor.Domain` (→ `Propagation.fs`) and `FSharp.Data` transitively, so the Mueller
algebra, `R(φ)`, the polarizer constructors and a CSV parser are all reachable. (See §13 Q2 — make
the `Domain` reference explicit rather than relying on transitivity.)

Put the reusable numeric functions in a small **new module in `OpticalConstructor.Domain`** (next to
`Propagation.fs`, which is where `rotationMueller` / `analyzerMueller` already live) so they are not
test-only, and keep the *test* + its hardcoded constants/gain-rules in `BerremanTests`.

### 5.2 Reuse map (call these — do not reinvent)

| Need | Reuse | Location |
|---|---|---|
| Stokes vector type + `+`, `Zero`, `.create` | `StokesVector` | `Berreman/Fields.fs:580` |
| Real 4×4 Mueller + `M*M`, `M*S`, `M+M`, `Zero` | `MuellerMatrix` | `Fields.fs:598` |
| Build/read a Mueller by rows | `Propagation.muellerOfRows` / `muellerElement` | `Propagation.fs:35 / 40` |
| Identity Mueller | `Propagation.identityMueller` | `Propagation.fs:99` |
| **Frame rotation R(φ)** `[1,0,0,0;0,c,s,0;0,−s,c,0;0,0,0,1]`, c=cos2φ | `Propagation.rotationMueller (Angle)` | `Propagation.fs:117` |
| Conjugated rotation `R(−φ)·M·R(φ)` | `Propagation.rotateMueller` | `Propagation.fs:128` |
| **Ideal linear polarizer Mueller** (== Python `linear_polarizer`) | `Propagation.analyzerMueller IdealLinear (Angle)` | `Propagation.fs:73` |
| Stokes components read-out | `Propagation.stokesComponents` / `s0` | `Propagation.fs:45 / 50` |
| Ordered product of a compound | `Propagation.compoundMueller` | `Propagation.fs:139` |
| Elevated primitives | `Angle` (`Geometry.fs:34`), `WaveLength`, `Polarization`, `Ellipticity` | `Fields.fs` |
| **16-unknown linear LSQ** | `MathNet.Numerics MultipleRegression.Svd(A,b)` (or `A.Svd().Solve b`) | vendored MathNet |
| Element-wise 4×4 tolerance assert | copy the `assertMuellerEqual` loop + `allowedDiff` | `MuellerMatrixTests.fs:20`, `MatrixComparison.fs:13` |
| Stokes 4-vector assert | `MatrixComparison.verifyVectorEqualityStokes` | `MatrixComparison.fs:73` |
| Fit-quality (χ²/covariance/RMSE) if wanted | `FitQuality.*` | `Optimization/FitQuality.fs` |
| IO proxy record + factory + mock shape | `ExperimentDataProxy` / `createFileBacked` / test mock | `ExperimentDataProxy.fs:43`, `ExperimentDataStore.fs:34`, `ExperimentDataProxyTests.fs:38` |

Verified equivalences: BNM `analyzerMueller IdealLinear θ` equals the reference `linear_polarizer(θ)`;
`rotationMueller`/`rotateMueller` equal the reference `rotation_matrix` / `R(−a)·M·R(a)`. So the F#
port sits directly on existing primitives.

### 5.3 Gaps to build (small, well-scoped)

1. **`retarderMueller (axis : Angle) (retardance : Retardance) : MuellerMatrix`** — the one missing
   physics primitive. Implement as `rotateMueller axis (muellerOfRows core)` with the diagonal
   retarder core above. Place next to `analyzerMueller`. (Introduce a `Retardance` single-case DU
   per the elevate-every-primitive rule — see §13 Q6.)
2. **`kron4 : StokesVector -> RealVector4 -> float[]`** (design row) and **`muellerOfVecColumnMajor :
   float[] -> MuellerMatrix`** / **`vecColumnMajor : MuellerMatrix -> float[]`**. There is no
   `vec`/Kronecker at the elevated level today. *Exact ordering* (must match the reference or the
   result is silently transposed): design row index `k = 4*i + j` carries `s_eff[i]·a_eff[j]`; the
   solved vector maps back as `M[j, i] = β[4*i + j]` (column-major `reshape(order='F')`). Add a
   round-trip unit test: `vec ∘ unvec = id`, and `I == a_effᵀ · unvec(kron(s,a)) · s_eff`.
3. **`solveLeastSquares : float[][] -> float[] -> float[]`** — a ~3-line MathNet SVD wrapper
   (`MultipleRegression.Svd`), the only new numeric-library seam. Keep `double[,]/double[]` confined
   to this boundary; elevate on both sides.
4. **`frobeniusDiff : MuellerMatrix -> MuellerMatrix -> {| frob; meanAbs; maxAbs; delta |}`** — element
   loop over `muellerElement`.
5. **No central 4×4 element-wise tolerance helper exists** (only relative-L2 for `ComplexMatrix`).
   Consider promoting a shared `assertMatrix4x4Equal` into `MatrixComparison.fs` instead of a third
   inline copy.

### 5.4 Tiny pure functions (the intended decomposition)

The task explicitly wants small composable steps. Suggested pure surface (all
`Result`-free except the parser; concrete elevated types on every signature):

```
// --- polarization primitives (Domain, next to Propagation) ---
retarderMueller       : Angle -> Retardance -> MuellerMatrix
linearPolarizerRow    : Angle -> RealVector4                 // row 0 of analyzerMueller IdealLinear
kron4                 : StokesVector -> RealVector4 -> float[]
vecColumnMajor        : MuellerMatrix -> float[]
muellerOfVecColumnMajor : float[] -> MuellerMatrix
frobeniusDiff         : MuellerMatrix -> MuellerMatrix -> MatrixDiff

// --- family / calibration models (test module; constants hardcoded) ---
type SourceModel   = LpSource | CplSource of thetaRel : Angle * retardance : Retardance
type AnalyzerModel = { kind : PolarizerKind; zeroDial : Angle; dialSign : int
                       thetaRel : Angle option; retardance : Retardance option }
sourceBaseStokes   : SourceModel -> StokesVector
physicalAngle      : AnalyzerModel -> RawDial -> Angle              // dialSign*(desc - zero)
analyzerBaseRow    : AnalyzerModel -> RawDial -> RealVector4
effectiveSource    : Angle -> StokesVector -> StokesVector          // R(phi) s_base
effectiveAnalyzer  : Angle -> RealVector4 -> RealVector4            // a_base R(-phi)
designRow          : StokesVector -> RealVector4 -> float[]          // kron4 of the two effective states

// --- label parsing (pure; mirror parse_experiment_name) ---
parseExperiment    : string -> ParsedExperiment                     // src, analyzer, objects, angles, note
family             : ParsedExperiment -> Family                     // LpLp | LpCpl | CplLp | CplCpl
matrixKind         : ParsedExperiment -> MatrixKind                 // Air | Qz | Lr | QzLrProduct

// --- signal reduction (pure; hardcoded rules) ---
darkSubtract       : DarkMean -> float -> float
airIdentityPred    : StokesVector -> RealVector4 -> float           // a_eff . s_eff
scalarGain         : (signal:float[]) -> (model:float[]) -> float
familyGain         : Family -> GainCalibration -> Timestamp -> float // encodes the per-family rules
correctSignal      : DarkMean -> float -> (gain:float) -> float

// --- reconstruction + compare ---
buildDesign        : Row list -> matrixKind:MatrixKind -> float[][] * float[]
reconstruct        : float[][] -> float[] -> MuellerMatrix          // solveLeastSquares + unvec
cascadeProduct     : MuellerMatrix -> MuellerMatrix -> MuellerMatrix // M_LR * M_QZ
```

### 5.5 IO via a Proxy (data stays out of the pure core)

Follow the repo's actual shape (module factory + private `try/with` boundary, per
`ExperimentDataStore.fs:34`), not a literal `static member create`:

```
type MuellerRawRow =                        // one CSV row, elevated
    { experiment : string; captureIndex : int; capturedAt : System.DateTimeOffset
      description : Angle; avgTotal : float }   // (+ other columns as needed)

type MuellerDataProxy =
    { tryLoadFamily : DataFilePath -> Result<MuellerRawRow list, MuellerDataError> }

// createFileBacked () owns File.ReadAllText inside try/with -> typed error, then calls the pure parser.
```

The pure parser (header-aware, multi-column, BOM-tolerant) reuses `FSharp.Data CsvFile.Parse`
(precedent: `SpectralImport.parseSpectrumCsv`). The **test substitutes an in-memory proxy** exactly
like `ExperimentDataProxyTests`, so the whole reduction/fit is exercised without touching disk.

### 5.6 The test(s)

Shape mirrors `LocalRefinementTests` (synthesize/ingest → fit → assert). xUnit v3 `[<Fact>]`,
back-tick names tagged with the acceptance criterion. Assertions:

1. **Reconstruction** — `M_QZ`, `M_LR`, `M_{QZ+LR}` each match §4 within tolerance; each solve reports
   rank 16.
2. **Cascade identity (primary)** — `‖M_{QZ+LR} − M_LR·M_QZ‖_F ≈ 0.571` (± tol); mean|Δ| ≈ 0.113;
   max|Δ| ≈ 0.268; the two largest element diffs at (4,2),(2,2).
3. **Fit quality** — per-kind RMSE ≈ {0.056, 0.091, 0.102}.
4. **Physics sanity** — `M_QZ` within tolerance of the pure-rotator template.
5. **Round-trip unit tests** for `kron4` / `vec` / `unvec` ordering (guards the transpose trap).

A `[<SkippableFact>]` guard (`Skip.If`) is recommended so the suite skips gracefully when the sibling
`OPM` repo/data is absent (see §13 Q3), rather than failing CI.

---

## 6. Data inventory (for the loader)

`C:\GitHub\optics-mueller\data\raw\final\` — CSV columns:
`experiment, capture_index, captured_at, description, iso, exposure_ns, focus_distance,
region_x/y/w/h, n_pixels, avg_R, avg_G, avg_B, avg_total`. `description` = raw analyzer dial angle
(degrees). UTF-8 BOM on the header; `captured_at` is ISO-8601.

| File | Rows | Content |
|---|---|---|
| `lp_lp.csv` | 209 | LP-LP family: AIR(-2,-2R), QZ 0/45/90, LR 0/45/90, QZ+LR 0/45/90 |
| `lp_cpl.csv` | 209 | LP-CPL family: same set |
| `cpl_lp.csv` | 209 | CPL-LP family: same set (least trustworthy; source-state jump) |
| `cpl_cpl_day1_main.csv` | 170 | CPL-CPL day 1: AIR, AIR-BR, QZ 0/45/90, LR 0/90, QZ+LR 0 & 90 (**no LR#45, no QZ#45+LR#45**) |
| `cpl_cpl_day2_corrections.csv` | 57 | CPL-CPL day 2: AIR-2, LR#45, QZ#45+LR#45 (the day-1 gaps) |
| `darkness_checks.csv` | 6 | dark frames → `dark_mean` ≈ 869.666 |
| `bullshit_checks.csv` | 2 | first `BullshitCheck` `captured_at` = CPL-LP split time |

Label grammar (mirror `parse_experiment_name`):
`^(src)-\((inside)\)-(an)(?:-(note))?$`, objects inside as `([A-Z]+)#(-?\d+)` joined by `-`.
Examples: `CPL-(AIR#0)-CPL`, `LP-(QZ#45)-LP-2`, `CPL-(QZ#90-LR#90)-CPL`.

---

## 7. Known data flaws to replicate (to match §7.2, not "fix")

- **LR orientation bug** (`OPM/notes/cpl_cpl_task.txt`): LR was physically rotated to **−45/−90**,
  but labels say `LR#45/#90`. The reference `object_rotation_from_parsed` uses the **label** angle as
  `φ`, so the published `M_LR`/`M_{QZ+LR}` embed this error. To reproduce §7.2 the test must do the
  same (use the label angle). See §13 Q5.
- **Excluded point**: `LP-(LR#90)-CPL-2`, `description=140`, `capture_index=17` — dropped from fits.
- **CPL-LP source jump**: handled only by the pre/post AIR gain split at the first BullshitCheck.
  This family is flagged least trustworthy; we still include it exactly as the reference does.

---

## 8. Reproduction fidelity

If the F# port replicates the reference exactly (same constants, gain rules, excluded point,
column-major `vec`, SVD least squares), it should reproduce the §7.2 matrices closely. Residual
mismatch sources: (a) the report's constants are **rounded** (39.31°, 82.55°, …) — hardcoding the
rounded values gives a looser match than re-deriving them in Stage 1 at full precision; (b) the
time-interpolated CPL-CPL day-1 gain depends on `captured_at` parsing; (c) float/LSQ backend
differences (negligible). Recommendation: **port all three stages** so F# computes the same numbers,
and additionally allow a mode that runs the reference Python once to capture full-precision
constants for a tight anchor. Set element tolerance from this (see §13 Q1).

---

## 9. Optimization / library decision (definitive)

| Step | Recommended (in-repo) | Alternative (in-repo) | External needed? |
|---|---|---|---|
| 16-unknown linear LSQ (reconstruction) | **MathNet `MultipleRegression.Svd(A,b)`** (SVD → free condition number) | ALGLIB `rmatrixsolvels` (returns numerical rank) | **No** |
| Stage-1 calibration (2-col cosine fits) | same MathNet SVD/QR least squares | closed-form normal equations | **No** |
| *Optional* nonlinear re-refinement | ALGLIB `minlm` (live in `AlglibAdapter.fs`) | MathNet `LevenbergMarquardtMinimizer` | **No** |
| *Optional* physical-realizability (Cloude) | Hermitian EVD: MathNet `Evd` / ALGLIB `hmatrixevd` | — | **No** |

**Conclusion: the required minimization is ordinary linear least squares; F# does it natively with
the vendored MathNet.Numerics. Python/Wolfram interop is not required and not justified** (it would
add a process boundary and non-determinism for zero capability gain). The repo has *no* Python and
only a chart-only Wolfram path in `BerremanRunner` — neither is on this code path. Reserve external
tools solely for offline cross-validation during development, never in the shipped test.

Sources: ALGLIB dense solvers `https://www.alglib.net/linear-solvers/dense.php`; ALGLIB `minlm`
`https://www.alglib.net/optimization/levenbergmarquardt.php`; MathNet `MultipleRegression`
`https://numerics.mathdotnet.com/api/MathNet.Numerics.LinearRegression/MultipleRegression.htm`;
MathNet regression guide `https://github.com/mathnet/mathnet-numerics/blob/master/docs/Regression.md`.

---

## 10. Industry best practice (web research) — alignment & optional upgrades

The report's method **is** the mainstream: the **measurement-matrix / data-reduction least-squares**
estimator (Chipman, *Polarized Light and Optical Systems*, Ch. 7; Goldstein, *Appl. Opt.* 31, 6676
(1992)). Build `W` row-by-row as `s_eff ⊗ a_eff`, then `vec(M) = W⁺ I`. Preferring **direct linear
LSQ over the Fourier method** is correct here because the source has only 3 discrete states and the
analyzer sampling is coarse/non-ideal — exactly the case where the W⁺ approach wins.

Corroborated points the spec bakes in:

- **Observability / full rank.** A *linear-only* source+analyzer spans only S0/S1/S2 → `W` is
  rank-deficient and circular-coupling elements are unrecoverable. This experiment's **CPL
  (elliptical) families inject/analyze S3**, which is *why* all three solves reach full rank 16.
  The test should **assert rank 16** as a first-class check (it is the observability guarantee).
- **180° periodicity.** 18 analyzer angles over 360° = **9 distinct states + duplicates**; the
  repeats buy noise averaging and drift self-checks, not new rank.
- **Drift/background handling** — dark-subtract then per-block AIR/air normalization is textbook
  (matches the reference exactly).
- **Cascade metric** — normalized Frobenius + per-element ΔM map + consistent `m00` frames.

Optional value-adds (recommend as secondary assertions or a follow-up slice — §13 Q4):

- **Cloude coherency-matrix eigenvalue test** (Gil, *Appl. Opt.* 55, 5449 (2016)): map each recovered
  `M` to its Hermitian coherency matrix; positive-semidefinite ⇔ physically realizable. Report the
  most-negative eigenvalue as a data-quality scalar; optionally Frobenius-nearest filter.
- **Lu–Chipman polar decomposition** (JOSA A 13, 1106 (1996)): factor into diattenuation / retardance
  / depolarization; verify `M_QZ` reads as an almost-pure rotator and its retardance axis tracks the
  0/45/90 orientation.
- **Condition number κ(W) / EWV** reporting for the assembled design (cheap from the SVD).

Key references (full list gathered during research; abbreviated here): Chipman/Lam/Young CRC Ch.7;
Goldstein 1992; Smith *Appl. Opt.* 41, 2488 (2002); Gil 2016 (arXiv:1605.04704); Lu–Chipman 1996;
Compain et al. (Eigenvalue Calibration Method) *Appl. Opt.* 38, 3490 (1999).

---

## 11. Proposed phasing (slices)

1. **Primitives + unit tests** — `retarderMueller`, `kron4`, `vec`/`unvec`, `solveLeastSquares`,
   `frobeniusDiff`; round-trip and `I = a_effᵀ M s_eff` identity tests. (Pure; no data.)
2. **Loader + proxy** — `MuellerDataProxy`, pure CSV parser, in-memory mock test.
3. **Glue + reduction** — label parse, family/kind, effective states, dark-subtract, per-family gain.
4. **Reconstruction + cascade test** — assemble design, solve, compare; assert §4 targets.
5. **(optional)** Stage-1 calibration port; Cloude/Lu–Chipman validations; κ(W)/EWV reporting.

A green `dotnet build Berreman.slnx -c Release` + `dotnet test` in `BerremanTests` is the gate.

---

## 12. Risks

- **Column-major `vec` ordering** is the classic footgun — a wrong order silently transposes `M`.
  Mitigated by the round-trip + identity unit tests (Slice 1) *before* touching real data.
- **Relative path to a sibling repo** is fragile (assumes both repos under a common parent, and
  `__SOURCE_DIRECTORY__` = compile-time location). Guarded by a Skippable test. (§13 Q3.)
- **Rounded-constant vs re-derived** reproduction gap (§8) — pick the tolerance deliberately.
- **`Propagation.fs` lives in `OpticalConstructor.Domain`, not `Berreman.Core`.** A *core-only* test
  can't see `rotationMueller`/`analyzerMueller`. `BerremanTests` reaches them transitively today, but
  make it explicit (§13 Q2).

---

## 13. Open questions

1. **Reproduction target & tolerance.** Do we (a) hardcode the rounded report constants and assert the
   §7.2 matrices within a loose element tolerance (~0.03–0.05), or (b) port Stage-1 to re-derive
   full-precision constants and assert tight (~1e-3)? Recommendation: (b) for the matrices plus a
   comfortable band on the headline Frobenius `0.571`. What element tolerance do you consider a
   faithful "mimic" of §7.2?
2. **Test project & references.** Confirm `BerremanTests` as the home and that we may add an explicit
   `ProjectReference` to `OpticalConstructor.Domain` (and rely on the transitive `FSharp.Data`), or
   would you prefer the numeric helpers live in `Berreman.Core` (which then needs `rotationMueller` /
   the linear-polarizer Mueller re-provided there, since those currently live in `Domain`)?
3. **Data access.** Keep the task's "relative path into the `OPM` repo" (source-dir walk-up to a
   sibling `C:\GitHub\optics-mueller`), with the test **skipped** when the data is absent? Or copy the
   7 CSVs into BNM as test fixtures (self-contained, CI-safe, but duplicates a foreign repo's data and
   must be refreshed by hand)? Is it acceptable that the test can't run without the `OPM` checkout?
4. **How much of the report to assert.** Is the core deliverable the four §7.2 matrices + the §8
   comparison, or should the test also reproduce the §6 calibration table, the §7 per-family RMSEs,
   and the AIR-identity check? Should the optional Cloude realizability / Lu–Chipman / κ(W) validations
   be in-scope now or a follow-up?
5. **LR orientation bug.** Confirm we replicate §7.2 as-is (use the *label* angle for LR, embedding the
   known −45/−90 physical error), rather than attempting a correction — since "mimic the report" means
   matching its numbers.
6. **Retardance elevation.** OK to introduce a `Retardance` single-case DU (degrees on the wire) per
   the elevate-every-primitive rule, or reuse `Angle` for retardance to minimize new types?
7. **Wolfram interop pointer.** The task offered to point at existing Python/Wolfram interop. Given the
   finding that the problem is pure linear least squares solvable natively in F#, we do **not** plan to
   use it. Do you still want a Wolfram/Python cross-check wired as an *offline, non-shipped* validation
   during development, or skip entirely?
8. **Reference scripts as oracle.** May the test/dev harness invoke the `OPM` Python
   (`matrix_step1_air_fit.py` → `matrix_glue.py` → `matrix_fit_linear.py`) once to generate a
   full-precision golden `summary.json` to diff against, or should the F# port be validated purely
   against the (rounded) numbers printed in the PDF?
```
