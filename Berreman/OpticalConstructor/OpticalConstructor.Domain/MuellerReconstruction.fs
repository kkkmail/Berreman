namespace OpticalConstructor.Domain

open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields

/// Spec 0042 (001) — the pure Mueller-reconstruction primitives. A *retarder* (wave plate) delays one
/// linear eigen-polarization relative to the other by a phase `Retardance`; its Mueller matrix is the
/// diagonal retarder core (fast axis at 0°) physically rotated to the plate's azimuth. It is built ONLY
/// from the EXISTING `Propagation` seams (`muellerOfRows` + `rotateMueller`) so no new 4×4 algebra is
/// introduced — the rotation reuses the same R(−θ)·M·R(θ) convention the ideal-polarizer pipeline pins.
module MuellerReconstruction =

    /// The phase delay a retarder (wave plate) introduces between its two linear eigen-polarizations,
    /// elevated to its own single-case DU. It is a PHASE, deliberately distinct from the azimuth `Angle`
    /// (`Geometry.fs`): mixing the two up is exactly the bug this elevation prevents. Mirrors the engine's
    /// `Angle` shape — a `.value` in radians, a `.degrees` view, and a `degree`-taking constructor.
    type Retardance =
        | Retardance of double

        /// The retardance in radians (the IO seam — read only where a raw phase is needed).
        member this.value = let (Retardance r) = this in r

        /// The retardance expressed in degrees.
        member this.degrees = let (Retardance r) = this in (r / degree)

        /// Build a `Retardance` from a value given in degrees.
        static member degree (d : double) : Retardance = d * degree |> Retardance

    /// The Mueller matrix of an ideal (lossless) retarder whose fast axis is at azimuth `axis` and whose
    /// phase delay is `retardance`. The diagonal retarder core (fast axis at 0°) is
    ///   [1,0,0,0], [0,1,0,0], [0,0,cos d,sin d], [0,0,−sin d,cos d]   (d = retardance in radians),
    /// physically rotated to `axis` by the EXISTING `Propagation.rotateMueller` (R(−θ)·M·R(θ)). With
    /// `Retardance.degree 0.0` the core is the identity and the result is `Propagation.identityMueller`
    /// for any azimuth; with a quarter wave (d = 90°) at azimuth 0° it is the standard QWP form.
    let retarderMueller (axis : Angle) (retardance : Retardance) : MuellerMatrix =
        let d = retardance.value
        let c = cos d
        let s = sin d
        let core =
            Propagation.muellerOfRows
                [ [ 1.0; 0.0; 0.0; 0.0 ]
                  [ 0.0; 1.0; 0.0; 0.0 ]
                  [ 0.0; 0.0; c; s ]
                  [ 0.0; 0.0; -s; c ] ]
        Propagation.rotateMueller axis core

    // -----------------------------------------------------------------------------------------------------
    // Spec 0042 (002) — the column-major linear-algebra core the least-squares reconstruction builds on.
    // Every matrix element is read/written ONLY through the EXISTING `Propagation` seams (`muellerOfRows` at
    // Propagation.fs:35 / `muellerElement` at :40) — no direct `RealMatrix4x4` indexing. The vec convention is
    // numpy reshape order F (column-major): the flattened index `k = 4*i + j` carries `M[j, i]` (column `i`
    // laid out contiguously). Dotting the design row against that vec is the single measurement equation
    // `a · (M · s)`, and the "column-major transpose guard" fact pins exactly that identity.
    // -----------------------------------------------------------------------------------------------------

    /// The Kronecker design row `aᵀ ⊗ sᵀ` for one measurement: the 16-element row whose entry at
    /// `k = 4*i + j` is `s[i] · a[j]` (`i = k / 4`, `j = k % 4`). Dotted with `vecColumnMajor M` it yields
    /// `a · (M · s)` — the linear form a single (input Stokes `s`, analyzer row `a`) measurement imposes on
    /// the unknown Mueller matrix `M`. `s` is read by unwrapping its backing `RealVector4`; `a` directly.
    let kron4 (s : StokesVector) (a : RealVector4) : float[] =
        let (StokesVector sv) = s
        Array.init 16 (fun k -> sv.[k / 4] * a.[k % 4])

    /// Flatten a `MuellerMatrix` to a 16-vector in COLUMN-MAJOR order (numpy reshape order F): the entry at
    /// `k = 4*i + j` is `M[j, i]`, so column `i` occupies the contiguous block `k = 4*i .. 4*i+3`. Read
    /// through the `Propagation.muellerElement` seam only (never direct `RealMatrix4x4` indexing).
    let vecColumnMajor (m : MuellerMatrix) : float[] =
        Array.init 16 (fun k -> Propagation.muellerElement m (k % 4) (k / 4))

    /// The inverse of `vecColumnMajor`: rebuild a `MuellerMatrix` from its 16-vector under the SAME
    /// column-major (order-F) convention — `M[j, i] = v[4*i + j]`, so row `r` / column `c` reads
    /// `v[4*c + r]`. Written through the `Propagation.muellerOfRows` seam, so
    /// `muellerOfVecColumnMajor (vecColumnMajor m) = m` for every 4×4 (the vec round-trip).
    let muellerOfVecColumnMajor (v : float[]) : MuellerMatrix =
        Propagation.muellerOfRows
            [ for r in 0 .. 3 -> [ for c in 0 .. 3 -> v.[4 * c + r] ] ]

    /// The elementwise difference summary between two `MuellerMatrix`es (the reconstruction residual
    /// readout): the Frobenius norm, the mean and max absolute element difference, and the `(i, j)` index of
    /// the largest absolute difference. Elevated to a record — never a bare float tuple.
    type MatrixDiff =
        {
            frobenius : float
            meanAbs : float
            maxAbs : float
            argMax : int * int
        }

    /// The elementwise difference between two `MuellerMatrix`es, read through the `Propagation.muellerElement`
    /// seam only: `frobenius` = √Σ dᵢⱼ², `meanAbs` = mean |dᵢⱼ| over the 16 entries, and `maxAbs` / `argMax`
    /// = the largest |dᵢⱼ| and its `(i, j)` index (the first occurrence on ties).
    let frobeniusDiff (a : MuellerMatrix) (b : MuellerMatrix) : MatrixDiff =
        let diffs =
            [ for i in 0 .. 3 do
                for j in 0 .. 3 ->
                    (i, j), abs (Propagation.muellerElement a i j - Propagation.muellerElement b i j) ]
        let sumSq = diffs |> List.sumBy (fun (_, d) -> d * d)
        let sumAbs = diffs |> List.sumBy (fun (_, d) -> d)
        let (argMax, maxAbs) = diffs |> List.maxBy snd
        {
            frobenius = sqrt sumSq
            meanAbs = sumAbs / 16.0
            maxAbs = maxAbs
            argMax = argMax
        }

    // -----------------------------------------------------------------------------------------------------
    // Spec 0042 (003, ADD_CONTRACT SVC_XDUO_0002) — the least-squares SOLVE seam. Declares, in the Domain,
    // the DECLARED-lifecycle `MuellerSolverProxy`: the optimization boundary that turns the over-determined
    // design system (the stacked Kronecker rows `A` from `kron4`, and the measured intensities `b`) into the
    // reconstructed Mueller vec, or a typed `SolverError`. It sits behind the pure column-major core above —
    // each `A` row is one `kron4 s a`; the returned solution un-vecs back to a `MuellerMatrix` via
    // `muellerOfVecColumnMajor` — so a future real proxy is exactly the missing `A, b -> least-squares x`
    // adapter, and its native/library exceptions are caught AT this boundary and mapped to the typed
    // `SolverError` channel (never thrown across it).
    //
    // Kept as pure DATA (the `ExperimentDataProxy` convention, ExperimentDataProxy.fs:43): a record of
    // camelCase `Result`-returning functions, so logic that holds the proxy stays referentially transparent
    // and a test substitutes a canned in-memory stub of the SAME shape. Its one field is function-valued and
    // so has no structural equality, so the record is `[<ReferenceEquality>]` — a host context
    // (Optimization / Elmish) that holds one keeps its required equality, comparing the proxy by identity.
    //
    // DECLARED lifecycle: this is the seam ONLY — no SVD, no ALGLIB, no wiring. A later
    // `IMPLEMENT_CONTRACT SVC_XDUO_0002` (step 004) supplies the real `createMathNetSvd ()` backed by the
    // vendored MathNet.Numerics SVD least-squares, leaving every consumer that holds the proxy unchanged.
    // -----------------------------------------------------------------------------------------------------

    /// A typed failure of the least-squares solve — never a throw across the proxy boundary.
    /// `SingularDesign` carries a human-readable `reason` (an empty design, a non-finite entry, or a
    /// native/library failure mapped onto the channel); `RankDeficient` carries the numerical `rank` the
    /// solver reported, strictly less than the column count the caller needs, so the reconstruction is not
    /// uniquely determined.
    type SolverError =
        | SingularDesign of reason : string
        | RankDeficient of rank : int

    /// The elevated result of a linear least-squares solve — never a bare `float[]`. `solution` is the
    /// minimizing `x` (the reconstructed Mueller vec, un-vec'd by `muellerOfVecColumnMajor`); `rank` is the
    /// numerical rank the solver reported — a first-class observability check, equal to the column count on a
    /// full-rank solve, that a caller compares against to trust the fit; `rmse` is the root-mean-square
    /// residual `‖A·x − b‖ / √m` over the `m` measurements.
    type LeastSquaresSolution =
        {
            solution : float[]
            rank : int
            rmse : float
        }

    /// The Mueller least-squares SOLVE seam (the functional-proxy convention): a record of one camelCase
    /// `Result`-returning function that resolves the over-determined design system to its minimizing solution.
    ///
    /// - `solveLinearLeastSquares design rhs` — solve `A·x ≈ b` in the least-squares sense, where `design` is
    ///   the row-major `A` (each inner array one stacked `kron4` measurement row) and `rhs` is the measured
    ///   intensities `b`; returns the `LeastSquaresSolution` (solution, numerical rank, residual RMSE), or a
    ///   typed `SolverError` for a singular or rank-deficient design (never a throw).
    ///
    /// DECLARED lifecycle: the seam only — the real MathNet-SVD-backed `createMathNetSvd ()` lands in a later
    /// `IMPLEMENT_CONTRACT SVC_XDUO_0002` (step 004) in this same module.
    [<ReferenceEquality>]
    type MuellerSolverProxy =
        {
            solveLinearLeastSquares : float[][] -> float[] -> Result<LeastSquaresSolution, SolverError>
        }
