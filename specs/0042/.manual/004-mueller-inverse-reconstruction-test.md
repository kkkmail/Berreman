# Spec 0042 (preliminary, rev 2) — Reproduce the Mueller-matrix inverse problem (report §7.2) as a UI-less F# test in BNM

Status: **preliminary, updated after review.** This supersedes `002-…md` and folds in the review
answers and corrections from `003-comments.txt` (the decisions are summarized in §13). Only the
review points were changed; the rest of the design is unchanged.

Sources this spec was built from:

- Report: `specs/0042/.manual/MuellerMatrix_final.pdf` (Russian; §7.2 is the target, §4.1–4.2 the
  measurement/rotation conventions, §8 the comparison, §9 the discussion of the LR re-measurement).
- Experiment repo (`OPM`): `C:\GitHub\optics-mueller\` — setup `notes/setup_intro.txt`, model
  `notes/matrix_solution.md`, normalization caveats `notes/final_normalization_caveat.md`,
  the LR-orientation note `notes/cpl_cpl_task.txt`, data `data/raw/final/*.csv`.
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

holds "within experimental accuracy," matching the report's Frobenius/element metrics (§8).

**Order of business (review Q5): reproduce the report result first; only then consider tweaks.**

Constraints from the task and review:

- **Port all three reference stages** (including the AIR calibration) so the F# computes the same
  numbers the reference Python does, and assert against §7.2 tightly (review Q1 = option b). Hardcoding
  procedure-level choices (gain rules, excluded point, split time) is fine; the *calibration constants*
  are re-derived, not hardcoded (the report's rounded values remain as cross-checks).
- **Many tiny pure reusable functions** doing small steps — not one giant function.
- **IO handled by `*Proxy` records** — both the CSV data access **and the optimization/solver**
  (review Q7: the numeric solve is an external routine ⇒ put it behind a proxy so backends are
  swappable). The numeric core stays pure.
- The test **pulls data directly from the `OPM` repo via a relative path** and **skips gracefully when
  the data is absent** (review Q3).
- The test executes **pure F#**. A Python script may be run **once**, offline, to emit a committed
  full-precision golden file into BNM (review Q8); the test never shells out to Python.

### 1.1 Headline finding (the capability question)

**The entire pipeline — calibration *and* reconstruction — is linear least squares** plus a couple of
closed-form trig steps. In the reference scripts every fit is `numpy.linalg.lstsq` on a 2-column
(calibration cosine) or 16-column (Mueller `vec`) design matrix; retardances come from
`arccos(visibility)`. **There is no Levenberg–Marquardt / nonlinear / global optimizer anywhere.**

Therefore the currently-available F# numerics do this with **zero external interop**: the 16-unknown
over-determined solve is `MathNet.Numerics` `MultipleRegression.Svd` (vendored, already referenced).
Per review Q7 we still route that solve through a **swappable solver proxy** (default MathNet SVD;
ALGLIB `rmatrixsolvels` as an alternative), so the optimization backend can be changed without
touching the reconstruction logic. **No Python, no Wolfram in the shipped path** (review Q7).

---

## 2. The measurement model (what the test implements)

For one intensity reading (report §2, §7.1; `matrix_glue.py`):

```
I = e0ᵀ · M_A(α) · M_O(φ) · M_S · S_laser          e0ᵀ = [1,0,0,0]
```

Absorb the fixed source and analyzer into effective states, and push the object's physical rotation
`φ` into those states so the sample matrix stays at its base orientation `M(0)`:

```
s0          = M_S · S_laser               (effective source Stokes)
a0ᵀ(α)      = e0ᵀ · M_A(α)                 (effective analyzer row = row 0 of the analyzer Mueller)
s_eff(φ)    = R(φ) · s0
a_effᵀ(α,φ) = a0ᵀ(α) · R(−φ)
I           = a_effᵀ(α,φ) · M(0) · s_eff(φ)
```

`R(φ)` is the Stokes/Mueller **frame rotation** carrying `cos 2φ / sin 2φ`. For a single sample the
observable is **linear in `vec(M)`**:

```
I = ( s_eff ⊗ a_eff )ᵀ · vec(M)            (column-major vec; see §5.3 for exact index order)
```

The combined stack `QZ+LR` is reconstructed the *same way* (as one 16-parameter unknown), then
compared against the independently-formed product `M_LR · M_QZ`.

### 2.1 Rotation conventions and the LR re-measurement (report §4.1–4.2, §9) — READ FIRST

The report fixes the conventions the port must honor:

- Objects are rotated, not the source; equivalent in the Stokes–Mueller model (§4.1).
- Three object orientations **0°, +45°, +90°**. Sign is physical: standing behind the laser looking
  along the beam, a positive angle is the usual counter-clockwise rotation (§4.1).
- The two analyzer families read the same physical rotation with **opposite dial signs**: at a common
  +45° rotation, CPL2's dial shows +45° while LP2's shows −45°; the true angle is recovered in
  processing from the analyzer calibration model (dial sign **−1** for LP, **+1** for CPL) — never
  hand-convert the raw `description` column (§4.2).

Object rotation enters the model **only through `R(φ)`, whose entries depend on `2φ`.** Two
consequences the review calls out, and which the port relies on:

- **+90° and −90° are the same measurement.** `2φ` differs by 360° (`cos 2φ`, `sin 2φ` unchanged), so
  `R(+90°) = R(−90°)`, hence `M(+90°) = M(−90°)` and `s_eff`/`a_eff` are identical.
- **+45° and −45° are different.** `2φ` differs by 180°, flipping the sign of `sin 2φ` — genuinely
  distinct states.

During day 1, LR was mistakenly rotated to **−45°/−90° instead of +45°/+90°**
(`OPM/notes/cpl_cpl_task.txt`). By the two facts above, the **−90° points are physically inert**,
while the **−45° points genuinely differ and were re-measured on day 2**: `cpl_cpl_day2_corrections.csv`
supplies exactly the `LR#45` and `QZ#45-LR#45` sweeps that `cpl_cpl_day1_main.csv` lacks. The report
notes this correction in §9. **So the reconstruction that uses the label angle as `φ` reproduces §7.2
faithfully — it is not an embedded error to work around** (this replaces the "known data flaw"
framing in `002-…md`). A `+90°/−90°` invariance unit test is a good guard (§5.6).

### 2.2 Component models (all four source/analyzer families)

| Element | Model | Constant(s) — re-derived in Stage 1; report values shown as cross-checks |
|---|---|---|
| LP source (LP1) | linear Stokes `s0 = [1,1,0,0]` | axis aligned to laser (base frame) |
| CPL source (CPL1) | `s0 = M_retarder(θ_src, δ_src) · [1,1,0,0]` | θ_src ≈ **39.31°**, δ_src ≈ **82.55°** |
| LP analyzer (LP2) | `M_A = M_linPol(β)`, `β = −(desc − z_LP)` | z_LP ≈ **155.44°**, dial sign **−1** |
| CPL analyzer (CPL2) | `M_A = M_linPol(β) · M_retarder(β+45°, δ_an)`, `β = +(desc − z_CPL)` | z_CPL ≈ **97.00°**, dial sign **+1**, θ_an = **45°**, δ_an ≈ **−83.67°** |

- `M_retarder(axis, δ)` is a linear retarder: `R(−axis) · core · R(axis)` with core
  `[[1,0,0,0],[0,1,0,0],[0,0,cosδ,sinδ],[0,0,−sinδ,cosδ]]`.
- The **retardance sign** is only constrained *relatively* by the CPL-CPL AIR check (source and
  analyzer rotate in opposite senses → analyzer `δ` is stored **negative**). Keep source `+`,
  analyzer `−` (matches `matrix_glue.py::load_step1_models`).
- The analyzer's effective row is **row 0** of `M_A` (`a_base = M_A[0, :]`).

---

## 3. The reference algorithm, stage by stage

Three deterministic stages, all ported to F#. All pure except the CSV read.

### Stage 1 — AIR calibration (`matrix_step1_air_fit.py`) — PORTED (review Q1b)

Only AIR series (no sample ⇒ `M_O = I`). Per family: group rows by `description`, average
`avg_total`, subtract `dark_mean`, normalize by the trace mean → `norm`. Then:

- **LP-LP** → free cosine fit `norm − 1 ≈ p·cos2α + q·sin2α` (2-column least squares): gives LP2 zero
  `z_LP = ½·atan2(q,p)` (≈ 155.44°), repeat spread ≈ 0.45°.
- **CPL-LP** (source) → fit in the LP frame, then closed-form `derive_source_cpl_from_lp_frame`
  → θ_src (≈ 39.31°), δ_src (≈ 82.55°); RMSE ≈ 0.0551.
- **LP-CPL** (analyzer) → free cosine fit → z_CPL (≈ 97.00°); δ_an = `arccos(visibility)` (≈ 83.67°);
  RMSE ≈ 0.0308.
- **CPL-CPL** → consistency check that fixes *relative* handedness (opposite).

Everything here is 2-column least squares + `atan2`/`arccos`. The re-derived constants feed Stage 2;
the report's rounded values (§2.2 table) are asserted as cross-checks.

### Stage 2 — "Glue": per-row effective states + design row (`matrix_glue.py`)

For every science row (all 5 CSVs):

1. Parse the `experiment` label → `(source, analyzer, obj1, φ1, obj2, φ2, note)`.
2. `φ` from the label (0/45/90); combined rows require `φ1 == φ2`; `matrix_kind ∈ {qz, lr, qz_lr_product}`.
3. `β = dial_sign · (description − zero)`; build `s_base`, `a_base(β)` from the family model (§2.2).
4. `s_eff = R(φ) · s_base`; `a_eff = a_base · R(−φ)`; `lincoef = kron(s_eff, a_eff)` (16-vector).

### Stage 3 — Linear fit + cascade comparison (`matrix_fit_linear.py`)

1. `dark_mean = mean(avg_total)` over `darkness_checks.csv` (≈ **869.666**).
2. **Per-family AIR gain** `g` (multiplicative source-drift correction),
   `g = Σ(signal·model) / Σ(model²)` over that block's AIR rows, where
   `signal = avg_total − dark_mean`, `model = a_eff·s_eff` (AIR identity prediction). Rules (hardcodable):
   - `cpl_cpl_day1`: **interpolate** `g` in time between `CPL-(AIR#0)-CPL` and `…-CPL-BR` (`captured_at`).
   - `cpl_cpl_day2`: single `g` from `CPL-(AIR#0)-CPL-2`.
   - `lp_lp`, `lp_cpl`: mean of the two AIR repeats (`-2`, `-2R`).
   - `cpl_lp`: **split at the first BullshitCheck** (`2026-05-31T01:10:40Z`); pre → `AIR-2`, post → `AIR-2R`.
3. `signal_corrected = (avg_total − dark_mean) / g`.
4. For each `matrix_kind`, build `X` (n×16 of `lincoef`) and `y = signal_corrected`, excluding AIR
   rows and the one contaminated point (`LP-(LR#90)-CPL-2`, `description=140`, `capture_index=17`).
   Solve `β = leastSquares(X, y)` **through the solver proxy**; reshape column-major → `M`. Report
   rank (=16), condition number, RMSE.
5. `M_product = M_LR · M_QZ`; compare `M_{QZ+LR}` vs `M_product` (Frobenius, mean|Δ|, max|Δ|).

---

## 4. Target numbers to assert (report §7.2 / §8)

Core deliverable (review Q4): **the §7.2 matrices + the §8 comparison.** Everything else is a
documented candidate (§5.6).

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

**Tolerances (review Q1b = tight):** port all three stages so F# matches the reference at full
precision, then assert each element within ≈ **2e-3** of the printed §7.2 value (absorbs the report's
3-decimal print-rounding plus float-backend jitter), and the Frobenius/mean/max metrics within a
comparable band. A committed full-precision golden file (Q8, §5.6) enables an optional much tighter
regression (~1e-9) against the reference Python's own output. The exact tolerance is tuned during
implementation.

Qualitative check worth asserting: `M_QZ` ≈ the pure-rotator form
`[[1,0,0,0],[0,cos2ψ,sin2ψ,0],[0,−sin2ψ,cos2ψ,0],[0,0,0,1]]`.

---

## 5. Proposed F# design

### 5.1 Where it lives (review Q2 — confirmed)

**Home: `Berreman/BerremanTests`** — the truly UI-less test project (no Avalonia; xUnit v3 + FsCheck +
FluentAssertions; owns `MatrixComparison.fs`). Per review Q2 the test project is free to reference
non-test projects, so **add explicit `ProjectReference`s** to `OpticalConstructor.Domain` (for
`Propagation`) and, if the ALGLIB solver backend is wired, `OpticalConstructor.Optimization` — do not
rely on transitivity. Put the reusable numeric functions in a **new module in
`OpticalConstructor.Domain`** (next to `Propagation.fs`, where `rotationMueller`/`analyzerMueller`
already live); keep the *test* plus its hardcoded gain-rules/split-time/excluded-point in
`BerremanTests`.

### 5.2 Reuse map (call these — do not reinvent)

| Need | Reuse | Location |
|---|---|---|
| Stokes vector type + `+`, `Zero`, `.create` | `StokesVector` | `Berreman/Fields.fs:580` |
| Real 4×4 Mueller + `M*M`, `M*S`, `M+M`, `Zero` | `MuellerMatrix` | `Fields.fs:598` |
| Build/read a Mueller by rows | `Propagation.muellerOfRows` / `muellerElement` | `Propagation.fs:35 / 40` |
| Identity Mueller | `Propagation.identityMueller` | `Propagation.fs:99` |
| **Frame rotation R(φ)** `[1,0,0,0;0,c,s,0;0,−s,c,0;0,0,0,1]`, c=cos2φ | `Propagation.rotationMueller (Angle)` | `Propagation.fs:117` |
| Conjugated rotation `R(−φ)·M·R(φ)` | `Propagation.rotateMueller` | `Propagation.fs:128` |
| **Ideal linear polarizer Mueller** (== reference `linear_polarizer`) | `Propagation.analyzerMueller IdealLinear (Angle)` | `Propagation.fs:73` |
| Stokes components read-out | `Propagation.stokesComponents` / `s0` | `Propagation.fs:45 / 50` |
| Ordered product of a compound | `Propagation.compoundMueller` | `Propagation.fs:139` |
| Elevated primitives | `Angle` (`Geometry.fs:34`), `WaveLength`, `Polarization`, `Ellipticity` | `Fields.fs` |
| **16-unknown linear LSQ** (behind the proxy) | `MathNet.Numerics MultipleRegression.Svd(A,b)` (or `A.Svd().Solve b`) | vendored MathNet |
| Element-wise 4×4 tolerance assert | copy the `assertMuellerEqual` loop + `allowedDiff` | `MuellerMatrixTests.fs:20`, `MatrixComparison.fs:13` |
| Stokes 4-vector assert | `MatrixComparison.verifyVectorEqualityStokes` | `MatrixComparison.fs:73` |
| Fit-quality (χ²/covariance/RMSE) if wanted | `FitQuality.*` | `Optimization/FitQuality.fs` |
| IO proxy record + factory + mock shape | `ExperimentDataProxy` / `createFileBacked` / test mock | `ExperimentDataProxy.fs:43`, `ExperimentDataStore.fs:34`, `ExperimentDataProxyTests.fs:38` |
| ALGLIB boundary (only file allowed to touch ALGLIB) | `AlglibAdapter` | `Optimization/AlglibAdapter.fs` |

Verified equivalences: BNM `analyzerMueller IdealLinear θ` equals the reference `linear_polarizer(θ)`;
`rotationMueller`/`rotateMueller` equal the reference `rotation_matrix` / `R(−a)·M·R(a)`.

### 5.3 Gaps to build (small, well-scoped)

1. **`retarderMueller (axis : Angle) (retardance : Retardance) : MuellerMatrix`** — the one missing
   physics primitive. Implement as `rotateMueller axis (muellerOfRows core)` with the diagonal
   retarder core above. Place next to `analyzerMueller`.
2. **`Retardance` single-case DU (review Q6).** Retardance is a *phase delay* δ, not a spatial
   azimuth, so it is semantically distinct from `Angle` even though both carry radians/degrees — use a
   dedicated type per the elevate-every-primitive rule:
   ```fsharp
   type Retardance =
       | Retardance of double                       // radians on the wire

       member this.value = let (Retardance r) = this in r
       member this.degrees = this.value / degree
       static member degree (d : double) : Retardance = d * degree |> Retardance
   ```
3. **`kron4 : StokesVector -> RealVector4 -> float[]`** (design row) and
   **`muellerOfVecColumnMajor : float[] -> MuellerMatrix`** / **`vecColumnMajor : MuellerMatrix ->
   float[]`**. *Exact ordering* (must match the reference or the result is silently transposed):
   design-row index `k = 4*i + j` carries `s_eff[i]·a_eff[j]`; the solved vector maps back as
   `M[j, i] = β[4*i + j]` (column-major `reshape(order='F')`). Add a round-trip unit test:
   `vec ∘ unvec = id`, and `I == a_effᵀ · unvec(kron(s,a)) · s_eff`.
4. **The solver proxy (review Q7)** — see §5.5.
5. **`frobeniusDiff : MuellerMatrix -> MuellerMatrix -> MatrixDiff`** — element loop over `muellerElement`.
6. **No central 4×4 element-wise tolerance helper exists** (only relative-L2 for `ComplexMatrix`).
   Consider promoting a shared `assertMatrix4x4Equal` into `MatrixComparison.fs` instead of a third
   inline copy.

### 5.4 Tiny pure functions (the intended decomposition)

Suggested pure surface (all `Result`-free except the parser and the proxy fields; concrete elevated
types on every signature):

```
// --- polarization primitives (Domain, next to Propagation) ---
retarderMueller       : Angle -> Retardance -> MuellerMatrix
linearPolarizerRow    : Angle -> RealVector4                 // row 0 of analyzerMueller IdealLinear
kron4                 : StokesVector -> RealVector4 -> float[]
vecColumnMajor        : MuellerMatrix -> float[]
muellerOfVecColumnMajor : float[] -> MuellerMatrix
frobeniusDiff         : MuellerMatrix -> MuellerMatrix -> MatrixDiff

// --- family / calibration models (constants re-derived in Stage 1) ---
type SourceModel   = LpSource | CplSource of thetaRel : Angle * retardance : Retardance
type AnalyzerModel = { kind : PolarizerKind; zeroDial : Angle; dialSign : int
                       thetaRel : Angle option; retardance : Retardance option }
sourceBaseStokes   : SourceModel -> StokesVector
physicalAngle      : AnalyzerModel -> RawDial -> Angle              // dialSign*(desc - zero)
analyzerBaseRow    : AnalyzerModel -> RawDial -> RealVector4
effectiveSource    : Angle -> StokesVector -> StokesVector          // R(phi) s_base
effectiveAnalyzer  : Angle -> RealVector4 -> RealVector4            // a_base R(-phi)
designRow          : StokesVector -> RealVector4 -> float[]          // kron4 of the two effective states

// --- Stage-1 calibration (all 2-column least squares + closed form) ---
freeCosineFit      : (angleDeg:float[]) -> (norm:float[]) -> {| zeroDeg:float; visibility:float; rmse:float |}
sourceCplFromLpFrame : (qLp:float) -> (uLp:float) -> {| thetaRelDeg:float; retardanceDeg:float |}
retardanceFromVisibility : float -> Retardance                     // arccos(|vis|)

// --- label parsing (pure; mirror parse_experiment_name) ---
parseExperiment    : string -> ParsedExperiment
family             : ParsedExperiment -> Family                     // LpLp | LpCpl | CplLp | CplCpl
matrixKind         : ParsedExperiment -> MatrixKind                 // Air | Qz | Lr | QzLrProduct

// --- signal reduction (pure; hardcoded rules) ---
darkSubtract       : DarkMean -> float -> float
airIdentityPred    : StokesVector -> RealVector4 -> float           // a_eff . s_eff
scalarGain         : (signal:float[]) -> (model:float[]) -> float
familyGain         : Family -> GainCalibration -> Timestamp -> float // encodes the per-family rules
correctSignal      : DarkMean -> float -> (gain:float) -> float

// --- reconstruction + compare (solve goes through the proxy) ---
buildDesign        : Row list -> matrixKind:MatrixKind -> float[][] * float[]
reconstruct        : MuellerSolverProxy -> float[][] -> float[] -> Result<MuellerMatrix, SolverError>
cascadeProduct     : MuellerMatrix -> MuellerMatrix -> MuellerMatrix // M_LR * M_QZ
```

### 5.5 IO proxies (data access **and** the optimization backend)

Two proxies, both following the repo's actual shape (module factory + private `try/with` boundary,
per `ExperimentDataStore.fs:34`), not a literal `static member create`.

**Data proxy** — keeps disk access out of the pure core:

```fsharp
type MuellerRawRow =                        // one CSV row, elevated
    { experiment : string; captureIndex : int; capturedAt : System.DateTimeOffset
      description : Angle; avgTotal : float }

type MuellerDataProxy =
    { tryLoadFamily : DataFilePath -> Result<MuellerRawRow list, MuellerDataError> }
// createFileBacked () owns File.ReadAllText inside try/with -> typed error, then calls the pure parser.
```

The pure parser (header-aware, multi-column, BOM-tolerant) reuses `FSharp.Data CsvFile.Parse`
(precedent `SpectralImport.parseSpectrumCsv`). The test substitutes an in-memory proxy exactly like
`ExperimentDataProxyTests`, exercising the whole reduction/fit without disk.

**Solver proxy (review Q7)** — the optimization *is* an external routine, so it lives behind a proxy
whose function fields bake in the backend; swapping backends is a one-line factory change:

```fsharp
type SolverError = SingularDesign of reason : string | RankDeficient of rank : int

type MuellerSolverProxy =
    { solveLinearLeastSquares : float[][] -> float[] -> Result<float[], SolverError> }
    // createMathNetSvd () : MuellerSolverProxy      -- default; MultipleRegression.Svd (Berreman.Core/Domain)
    // createAlglibLstsq () : MuellerSolverProxy      -- alternative; ALGLIB rmatrixsolvels, wired in AlglibAdapter
```

Only the thin proxy body touches `double[,]`/`double[]` and the numeric library; both sides of the
proxy are elevated. The reconstruction takes the proxy as a parameter, so the default MathNet path and
any future ALGLIB/nonlinear path are interchangeable without editing the reconstruction logic. (The
record can later gain a `refineNonlinear` field if a nonlinear refinement is ever wanted — not needed
for §7.2.)

### 5.6 The test(s)

Shape mirrors `LocalRefinementTests` (ingest → fit → assert). xUnit v3 `[<Fact>]`, back-tick names
tagged with the acceptance criterion. Per review Q3 the data-dependent facts are `[<SkippableFact>]`
guarded (`Skip.If`) so the suite **skips when the sibling `OPM` data is absent** instead of failing.

**Core assertions (review Q4 — this is the initial scope):**

1. **Reconstruction** — `M_QZ`, `M_LR`, `M_{QZ+LR}` each match §4 within tolerance; each solve reports
   rank 16.
2. **Cascade identity (primary)** — `‖M_{QZ+LR} − M_LR·M_QZ‖_F ≈ 0.571` (± tol); mean|Δ| ≈ 0.113;
   max|Δ| ≈ 0.268; the two largest element diffs at (4,2),(2,2).

**Pure unit tests (no external data, always run):** `kron4`/`vec`/`unvec` round-trip and the
`I = a_effᵀ M s_eff` identity (guards the transpose trap); the `+90°/−90°` invariance from §2.1;
`retarderMueller` sanity (δ=0 ⇒ identity; QWP form).

**Future test candidates — document these in a `///` XML doc block at the top of the test file**
(review Q4), not implemented yet:

```fsharp
/// Reproduces report §7.2 (M_QZ, M_LR, M_{QZ+LR}) and the §8 cascade comparison.
/// Future candidates (not yet asserted):
///  - §6 calibration table (LP2 zero, CPL1/CPL2 retarder angle + retardance, RMSEs)
///  - §7 per-family fit RMSE and the AIR-identity residual check
///  - physical realizability: Cloude coherency-matrix eigenvalue test (nearest-physical filter)
///  - Lu–Chipman polar decomposition (M_QZ ≈ pure rotator; retardance/diattenuation scalars)
///  - conditioning: condition number κ(W) / EWV of the design matrix
///  - tweaks beyond faithful reproduction (e.g. explicit ±45/±90 handling) — after §7.2 is matched
```

**Golden anchor (review Q8):** optionally run the OPM Python once, offline, to emit a committed
full-precision `reference_summary.json` (matrices + metrics) into a BerremanTests fixtures folder
(copied to output, per the `MaterialImportTests` fixtures precedent). The F# test then additionally
diffs its recomputed result against that golden file at ~1e-9. The raw CSVs stay external (Q3); only
the small golden numbers are committed. The test still runs pure F# — Python is never invoked at test
time.

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
| `cpl_cpl_day1_main.csv` | 170 | CPL-CPL day 1: AIR, AIR-BR, QZ 0/45/90, LR **0 & 90**, QZ+LR **0 & 90** (no LR#45 / QZ#45+LR#45 — those are the ±45 points, re-measured day 2; see §2.1) |
| `cpl_cpl_day2_corrections.csv` | 57 | CPL-CPL day 2: AIR-2, **LR#45**, **QZ#45+LR#45** (the corrected ±45 sweeps) |
| `darkness_checks.csv` | 6 | dark frames → `dark_mean` ≈ 869.666 |
| `bullshit_checks.csv` | 2 | first `BullshitCheck` `captured_at` = CPL-LP split time |

Label grammar (mirror `parse_experiment_name`):
`^(src)-\((inside)\)-(an)(?:-(note))?$`, objects inside as `([A-Z]+)#(-?\d+)` joined by `-`.
Examples: `CPL-(AIR#0)-CPL`, `LP-(QZ#45)-LP-2`, `CPL-(QZ#90-LR#90)-CPL`.

Other rows to handle per the reference: exclude AIR from the object fits; drop the single contaminated
point (`LP-(LR#90)-CPL-2`, `description=140`, `capture_index=17`); use the first `BullshitCheck` time
as the CPL-LP gain split.

---

## 7. Optimization / library decision (definitive)

| Step | Recommended (in-repo, behind the solver proxy) | Alternative (in-repo) | External needed? |
|---|---|---|---|
| 16-unknown linear LSQ (reconstruction) | **MathNet `MultipleRegression.Svd(A,b)`** (SVD → free condition number) | ALGLIB `rmatrixsolvels` (returns numerical rank) | **No** |
| Stage-1 calibration (2-col cosine fits) | same MathNet SVD/QR least squares | closed-form normal equations | **No** |
| *Future* nonlinear re-refinement | ALGLIB `minlm` (live in `AlglibAdapter.fs`) | MathNet `LevenbergMarquardtMinimizer` | **No** |
| *Future* physical-realizability (Cloude) | Hermitian EVD: MathNet `Evd` / ALGLIB `hmatrixevd` | — | **No** |

**Conclusion: the required minimization is ordinary linear least squares; F# does it natively with the
vendored MathNet.Numerics. Wolfram/Python interop is not used in the shipped test** (review Q7). The
solve is nonetheless routed through the `MuellerSolverProxy` so the backend (MathNet ↔ ALGLIB ↔ a
future routine) is swappable. Python is permitted only to generate the once-off golden file offline
(review Q8), never at test time.

Sources: ALGLIB dense solvers `https://www.alglib.net/linear-solvers/dense.php`; ALGLIB `minlm`
`https://www.alglib.net/optimization/levenbergmarquardt.php`; MathNet `MultipleRegression`
`https://numerics.mathdotnet.com/api/MathNet.Numerics.LinearRegression/MultipleRegression.htm`;
MathNet regression guide `https://github.com/mathnet/mathnet-numerics/blob/master/docs/Regression.md`.

---

## 8. Industry best practice (web research) — alignment & future upgrades

The report's method **is** the mainstream: the **measurement-matrix / data-reduction least-squares**
estimator (Chipman, *Polarized Light and Optical Systems*, Ch. 7; Goldstein, *Appl. Opt.* 31, 6676
(1992)). Build `W` row-by-row as `s_eff ⊗ a_eff`, then `vec(M) = W⁺ I`. Preferring **direct linear
LSQ over the Fourier method** is correct here (3 discrete source states, coarse/non-ideal analyzer
sampling).

Points the design already reflects:

- **Observability / full rank.** A *linear-only* source+analyzer spans only S0/S1/S2 → `W` is
  rank-deficient and circular-coupling elements are unrecoverable. This experiment's **CPL
  (elliptical) families inject/analyze S3**, which is *why* all three solves reach full rank 16 —
  hence the rank-16 assertion is a first-class observability check.
- **180° periodicity.** 18 analyzer angles over 360° = 9 distinct states + duplicates (noise averaging,
  not new rank). Consistent with §2.1's `2φ` reasoning.
- **Drift/background handling** — dark-subtract then per-block AIR normalization is textbook.
- **Cascade metric** — normalized Frobenius + per-element ΔM map with consistent `m00` frames.

Documented as **future candidates** in the test's XML comment (review Q4), not in the initial scope:
Cloude coherency-eigenvalue realizability test (Gil, *Appl. Opt.* 55, 5449 (2016)) with Frobenius-
nearest filtering; Lu–Chipman polar decomposition (JOSA A 13, 1106 (1996)) to confirm `M_QZ` reads as
a near-pure rotator; κ(W)/EWV reporting.

Key references: Chipman/Lam/Young CRC Ch. 7; Goldstein 1992; Smith *Appl. Opt.* 41, 2488 (2002); Gil
2016 (arXiv:1605.04704); Lu–Chipman 1996; Compain et al. (Eigenvalue Calibration Method) *Appl. Opt.*
38, 3490 (1999).

---

## 9. Proposed phasing (slices)

1. **Primitives + unit tests** — `Retardance`, `retarderMueller`, `kron4`, `vec`/`unvec`,
   `frobeniusDiff`; the `MuellerSolverProxy` with the MathNet factory; round-trip, `I = a_effᵀ M s_eff`,
   and `+90°/−90°` invariance tests. (Pure; no external data.)
2. **Loader + data proxy** — `MuellerDataProxy`, pure CSV parser, in-memory mock test.
3. **Glue + reduction** — label parse, family/kind, effective states, dark-subtract, per-family gain.
4. **Stage-1 calibration port** — cosine fits + closed-form retardance; cross-check vs the §6 constants.
5. **Reconstruction + cascade test** — assemble design, solve via proxy, compare; assert §4 targets.
   Optionally add the committed golden-file regression (Q8).
6. **(later)** the documented future candidates (§5.6) and any post-reproduction tweaks (Q5).

Gate: green `dotnet build Berreman.slnx -c Release` + `dotnet test` in `BerremanTests`.

---

## 10. Risks

- **Column-major `vec` ordering** is the classic footgun — a wrong order silently transposes `M`.
  Mitigated by the round-trip + identity unit tests (Slice 1) *before* touching real data.
- **Relative path to a sibling repo** is fragile (assumes both repos under a common parent;
  `__SOURCE_DIRECTORY__` = compile-time location). Guarded by the Skippable tests (Q3).
- **Rounded-constant vs re-derived** reproduction gap — resolved by porting Stage 1 (Q1b) and the
  optional golden file (Q8); the element tolerance is tuned in Slice 5.
- **`Propagation.fs` lives in `OpticalConstructor.Domain`.** Reachable now via the explicit
  `ProjectReference` (Q2).

---

## 11. Review decisions (from 003-comments.txt)

- **Rotation convention correction.** Object rotation enters via `R(φ)` (depends on `2φ`), so **+90° ≡
  −90°** (physically inert mislabel) while **+45° ≠ −45°**; the day-1 LR ±45° error was **re-measured on
  day 2**. The reconstruction using label angles therefore reproduces §7.2 faithfully — the earlier
  "embedded data flaw" framing is removed (§2.1). **Reproduce the report first, then tweak (Q5).**
- **Q1 → (b):** port all three stages, re-derive calibration constants, assert §7.2 tightly (§3, §4).
- **Q2:** home is `BerremanTests`; add explicit `ProjectReference`s to the needed non-test projects (§5.1).
- **Q3:** data stays relative to the `OPM` repo; data-dependent tests are Skippable when it is absent (§5.6).
- **Q4:** initial scope = §7.2 matrices + §8 comparison; all other candidates documented in the test's
  top-of-file XML comment (§5.6).
- **Q5:** see the rotation correction above — first reproduce §7.2, then consider tweaks.
- **Q6:** introduce a dedicated `Retardance` DU (phase ≠ azimuth), not `Angle` (§5.3).
- **Q7:** no Wolfram; route the optimization through a swappable `MuellerSolverProxy` (§5.5, §7).
- **Q8:** tests run pure F#; a Python script may be run **once**, offline, to emit a committed
  full-precision golden file for a tight regression anchor (§5.6).

### Residual items to settle during implementation (not blockers)

- The exact element tolerance for the §7.2 match (target ≈ 2e-3 vs the printed values; ~1e-9 vs the
  golden file) — tuned in Slice 5.
- The committed golden file's location/format (proposed: `BerremanTests/fixtures/mueller/reference_summary.json`,
  copied to output).
- Whether to promote a shared `assertMatrix4x4Equal` into `MatrixComparison.fs` rather than a third
  inline copy of the element-wise loop.
```
