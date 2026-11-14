namespace OpticalConstructor.Domain

open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields
open OpticalConstructor.Domain.Library                // PolarizerKind / IdealLinear (step 007 source & analyzer models)
open OpticalConstructor.Domain.MeasuredData           // MeasuredIntensity: a capture's averaged intensity (spec 0048 E020)
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.NamingTemplates

/// Spec 0042 (001) — the pure Mueller-reconstruction primitives. A *retarder* (wave plate) delays one
/// linear eigen-polarization relative to the other by a phase `Retardance`; its Mueller matrix is the
/// diagonal retarder core (fast axis at 0°) physically rotated to the plate's azimuth. It is built ONLY
/// from the EXISTING seams (`RealMatrix4x4.create` + `Propagation.rotateMueller`) so no new 4×4 algebra is
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
        member this.degrees = let (Retardance r) = this in (r / Angle.radiansPerDegree)

        /// Build a `Retardance` from a value given in degrees (typed in degrees, as `Angle.degree` is).
        static member degree (d : float<Berreman.Constants.deg>) : Retardance = float d * Angle.radiansPerDegree |> Retardance

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
            RealMatrix4x4.create
                [ [ 1.0; 0.0; 0.0; 0.0 ]
                  [ 0.0; 1.0; 0.0; 0.0 ]
                  [ 0.0; 0.0; c; s ]
                  [ 0.0; 0.0; -s; c ] ]
            |> MuellerMatrix
        Propagation.rotateMueller axis core

    // -----------------------------------------------------------------------------------------------------
    // Spec 0042 (002) — the column-major linear-algebra core the least-squares reconstruction builds on.
    // Every matrix element is written through `RealMatrix4x4.create` and read through the `MuellerMatrix.element`
    // member (Propagation.fs) — no direct `RealMatrix4x4` indexing. The vec convention is
    // numpy reshape order F (column-major): the flattened index `k = 4*i + j` carries `M[j, i]` (column `i`
    // laid out contiguously). Dotting the design row against that vec is the single measurement equation
    // `a · (M · s)`, and the "column-major transpose guard" fact pins exactly that identity.
    // -----------------------------------------------------------------------------------------------------

    /// One row of a least-squares design: the coefficients a single measurement imposes on the unknowns (16 for a
    /// Mueller reconstruction, 2 for a Stage-1 cosine fit).
    [<Softellect.Sys.NumericCarrier("a row of a least-squares design matrix (R3): the coefficients one measurement imposes, stacked whole into the MathNet SVD solve")>]
    type DesignRow =
        | DesignRow of float[]

        member this.value = let (DesignRow r) = this in r

    /// The Kronecker design row `aᵀ ⊗ sᵀ` for one measurement: the 16-element row whose entry at
    /// `k = 4*i + j` is `s[i] · a[j]` (`i = k / 4`, `j = k % 4`). Dotted with `M.vecColumnMajor` it yields
    /// `a · (M · s)` — the linear form a single (input Stokes `s`, analyzer row `a`) measurement imposes on
    /// the unknown Mueller matrix `M`. `s` is read by unwrapping its backing `RealVector4`; `a` directly.
    let kron4 (s : StokesVector) (a : RealVector4) : DesignRow =
        let (StokesVector sv) = s
        Array.init 16 (fun k -> sv.[k / 4] * a.[k % 4]) |> DesignRow

    type MuellerMatrix with

        /// This matrix flattened to a 16-vector in COLUMN-MAJOR order (numpy reshape order F): the entry at
        /// `k = 4*i + j` is `M[j, i]`, so column `i` occupies the contiguous block `k = 4*i .. 4*i+3`. Read
        /// through the `MuellerMatrix.element` seam only (never direct `RealMatrix4x4` indexing).
        member this.vecColumnMajor : RealVector =
            Array.init 16 (fun k -> this.element (k % 4) (k / 4)) |> RealVector.create

    type RealVector with

        /// The `MuellerMatrix` this 16-vector flattens under the SAME column-major (order-F) convention:
        /// row `r` / column `c` reads entry `4*c + r`, so `m.vecColumnMajor.columnMajorMueller = m` for every
        /// 4×4 (the vec round-trip).
        member this.columnMajorMueller : MuellerMatrix =
            RealMatrix4x4.create
                [ for r in 0 .. 3 -> [ for c in 0 .. 3 -> this.[4 * c + r] ] ]
            |> MuellerMatrix

    /// A size of the difference between two `MuellerMatrix`es: a norm of it, or one element's magnitude.
    type MatrixDeviation =
        | MatrixDeviation of float

        member this.value = let (MatrixDeviation d) = this in d

    /// A row of a 4×4 Mueller matrix, 0 to 3.
    type MuellerRowIndex =
        | MuellerRowIndex of int

        member this.value = let (MuellerRowIndex i) = this in i

    /// A column of a 4×4 Mueller matrix, 0 to 3.
    type MuellerColumnIndex =
        | MuellerColumnIndex of int

        member this.value = let (MuellerColumnIndex j) = this in j

    /// The elementwise difference summary between two `MuellerMatrix`es (the reconstruction residual
    /// readout): the Frobenius norm, the mean and max absolute element difference, and the (row, column) of
    /// the largest absolute difference.
    type MatrixDiff =
        {
            frobenius : MatrixDeviation
            meanAbs : MatrixDeviation
            maxAbs : MatrixDeviation
            argMax : MuellerRowIndex * MuellerColumnIndex
        }

    /// The elementwise difference between two `MuellerMatrix`es, read through the `MuellerMatrix.element`
    /// seam only: `frobenius` = √Σ dᵢⱼ², `meanAbs` = mean |dᵢⱼ| over the 16 entries, and `maxAbs` / `argMax`
    /// = the largest |dᵢⱼ| and its (row, column) (the first occurrence on ties).
    let frobeniusDiff (a : MuellerMatrix) (b : MuellerMatrix) : MatrixDiff =
        let diffs =
            [ for i in 0 .. 3 do
                for j in 0 .. 3 ->
                    (MuellerRowIndex i, MuellerColumnIndex j), abs (a.element i j - b.element i j) ]
        let sumSq = diffs |> List.sumBy (fun (_, d) -> d * d)
        let sumAbs = diffs |> List.sumBy (fun (_, d) -> d)
        let (argMax, maxAbs) = diffs |> List.maxBy snd
        {
            frobenius = MatrixDeviation (sqrt sumSq)
            meanAbs = MatrixDeviation (sumAbs / 16.0)
            maxAbs = MatrixDeviation maxAbs
            argMax = argMax
        }

    // -----------------------------------------------------------------------------------------------------
    // Spec 0042 (003, ADD_CONTRACT SVC_XDUO_0002) — the least-squares SOLVE seam. Declares, in the Domain,
    // the DECLARED-lifecycle `MuellerSolverProxy`: the optimization boundary that turns the over-determined
    // design system (the stacked Kronecker rows `A` from `kron4`, and the measured intensities `b`) into the
    // reconstructed Mueller vec, or a typed `SolverError`. It sits behind the pure column-major core above —
    // each `A` row is one `kron4 s a`; the returned solution un-vecs back to a `MuellerMatrix` via
    // `columnMajorMueller` — so a future real proxy is exactly the missing `A, b -> least-squares x`
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

    /// The numerical rank of a least-squares design, as the SVD reports it.
    type MatrixRank =
        | MatrixRank of int

        member this.value = let (MatrixRank r) = this in r

    /// The root-mean-square residual of a least-squares fit, `‖A·x − b‖ / √m` over its `m` measurements.
    type ResidualRmse =
        | ResidualRmse of float

        member this.value = let (ResidualRmse r) = this in r

    /// A typed failure of the least-squares solve — never a throw across the proxy boundary.
    /// `SingularDesignErr` carries a human-readable `reason` (an empty design, a non-finite entry, or a
    /// native/library failure mapped onto the channel); `RankDeficientErr` carries the numerical `rank` the
    /// solver reported, strictly less than the column count the caller needs, so the reconstruction is not
    /// uniquely determined.
    type SolverError =
        | SingularDesignErr of reason : string
        | RankDeficientErr of rank : MatrixRank

        /// The failed condition and its payload (spec 0050 S11.3).
        member this.describe : string =
            match this with
            | SingularDesignErr reason -> $"the design is singular: {reason}"
            | RankDeficientErr rank -> $"the design is rank-deficient: rank {rank.value}"

    /// The over-determined system `A·x ≈ b` one least-squares solve resolves: the stacked design rows `A` and
    /// the measured right-hand side `b`, one entry per row.
    [<Softellect.Sys.NumericCarrier("the right-hand side of a least-squares solve (R3): the measured intensities the MathNet SVD consumes whole, beside the design rows")>]
    type LeastSquaresSystem =
        {
            design : DesignRow[]
            rhs : float[]
        }

        /// The residuals `A·x − b` of a solution over this system's rows, one per row (spec 0050 S11.4).
        member this.residualsAt (solution : RealVector) : RealVector =
            (RealMatrix.create (this.design |> Array.map (fun row -> row.value)) * solution) - RealVector.create this.rhs

        /// The residual RMSE `‖A·x − b‖ / √m` of a solution over this system's rows, the one the solver reports and the
        /// product check reuses (spec 0050 S10.3).
        member this.residualRmseAt (solution : RealVector) : ResidualRmse =
            (this.residualsAt solution).norm / sqrt (float this.design.Length) |> ResidualRmse

        /// The systems' design rows and right-hand sides, stacked in list order (spec 0050 S11.3).
        static member concat (systems : LeastSquaresSystem list) : LeastSquaresSystem =
            {
                design = systems |> List.map (fun system -> system.design) |> Array.concat
                rhs = systems |> List.map (fun system -> system.rhs) |> Array.concat
            }

    /// The elevated result of a linear least-squares solve. `solution` is the minimizing `x` (for a
    /// reconstruction, the Mueller vec its `columnMajorMueller` un-vecs); `rank` is the numerical rank the
    /// solver reported — a first-class observability check, equal to the column count on a full-rank solve,
    /// that a caller compares against to trust the fit; `rmse` is the residual over the `m` measurements.
    type LeastSquaresSolution =
        {
            solution : RealVector
            rank : MatrixRank
            rmse : ResidualRmse
        }

    /// The Mueller least-squares SOLVE seam (the functional-proxy convention): a record of one camelCase
    /// `Result`-returning function that resolves the over-determined design system to its minimizing solution.
    ///
    /// - `solveLinearLeastSquares system` — solve `A·x ≈ b` in the least-squares sense (each design row one
    ///   stacked `kron4` measurement row, `rhs` the measured intensities `b`); returns the
    ///   `LeastSquaresSolution` (solution, numerical rank, residual RMSE), or a typed `SolverError` for a
    ///   singular or rank-deficient design (never a throw).
    ///
    /// DECLARED lifecycle: the seam only — the real MathNet-SVD-backed `createMathNetSvd ()` lands in a later
    /// `IMPLEMENT_CONTRACT SVC_XDUO_0002` (step 004) in this same module.
    [<ReferenceEquality>]
    type MuellerSolverProxy =
        {
            solveLinearLeastSquares : LeastSquaresSystem -> Result<LeastSquaresSolution, SolverError>
        }

    // -----------------------------------------------------------------------------------------------------
    // Spec 0042 (004, IMPLEMENT_CONTRACT SVC_XDUO_0002) — the REAL least-squares solve behind the DECLARED
    // `MuellerSolverProxy` seam above. `createMathNetSvd ()` builds a proxy whose `solveLinearLeastSquares`
    // resolves the over-determined design system `A·x ≈ b` through the vendored MathNet.Numerics SVD
    // least-squares (`matrix.Svd(true).Solve(b)`), reporting the numerical `rank` (the count of
    // non-negligible singular values) and the intensity residual `rmse = ‖A·x − b‖ / √m`. MathNet is reached
    // TRANSITIVELY through `Berreman.MathNetNumericsMath` exactly as `Propagation` opens it (Propagation.fs:4)
    // — no direct `MathNet.Numerics` open is added to the Domain: the design `A` and rhs `b` are built through
    // the EXISTING `RealMatrix.create` / `RealVector.create` wrappers, unwrapped to their backing
    // `Matrix<double>` / `Vector<double>` ONLY for the `.Svd()` / `.Solve()` members the DU does not surface.
    //
    // The seam stays TOTAL: a degenerate or rank-deficient design is mapped to a typed `SolverError`, never a
    // throw across the proxy — `RankDeficientErr rank` when the SVD reports a rank below the column count the
    // reconstruction needs (an under-determined design lands here since `rank ≤ rows < columns`);
    // `SingularDesignErr reason` for a degenerate shape (no rows, no columns, an `rhs` whose length does not
    // match the row count, a non-finite entry) or for ANY MathNet exception caught AT this boundary
    // (CLAUDE.md: native/library exceptions are mapped to a typed error, never handled above the seam).
    //
    // ALGLIB alternative (documented, NOT required for §7.2): the same seam could be backed by ALGLIB's
    // `rmatrixsolvels`, which would live ONLY in `OpticalConstructor.Optimization/AlglibAdapter.fs:20` (the
    // single ALGLIB-referencing file). The Mueller reconstruction (§7.2) does NOT depend on it — the vendored
    // MathNet SVD here is the sole required backend.
    // -----------------------------------------------------------------------------------------------------

    /// The REAL `MuellerSolverProxy`, backed by the vendored MathNet.Numerics SVD least-squares (the
    /// `IMPLEMENT_CONTRACT SVC_XDUO_0002` construction behind the DECLARED seam above). Its
    /// `solveLinearLeastSquares system` solves `A·x ≈ b` — `design` the stacked `kron4` measurement rows `A`,
    /// `rhs` the measured intensities `b` — and returns the minimizing `LeastSquaresSolution` (`solution`,
    /// numerical `rank`, residual `rmse = ‖A·x − b‖ / √m`), or a typed `SolverError`:
    ///   - `SingularDesignErr reason` — a degenerate design (no rows, no columns, an `rhs` whose length does
    ///     not match the row count, or a non-finite entry), or any MathNet exception mapped onto the channel;
    ///   - `RankDeficientErr rank` — the SVD's effective numerical `rank` is below the column count, so the
    ///     reconstruction is not uniquely determined.
    /// Never throws across the proxy boundary (the `.Svd()` / `.Solve()` block runs under `try/with`).
    let createMathNetSvd () : MuellerSolverProxy =
        {
            solveLinearLeastSquares =
                fun (system : LeastSquaresSystem) ->
                    let design = system.design |> Array.map (fun row -> row.value)
                    let rhs = system.rhs
                    let widths = design |> Array.map Array.length
                    let notFinite (v : float) : bool = not (System.Double.IsFinite v)
                    if design.Length = 0 then
                        Error (SingularDesignErr "the design matrix has no rows (an empty least-squares system)")
                    elif widths |> Array.exists (fun w -> w <> widths.[0]) then
                        Error (SingularDesignErr "the design matrix is ragged (its rows are of unequal length)")
                    elif widths.[0] = 0 then
                        Error (SingularDesignErr "the design matrix has no columns (there are no unknowns to solve for)")
                    elif rhs.Length <> design.Length then
                        Error (SingularDesignErr $"the right-hand side has {rhs.Length} entries but the design has {design.Length} rows")
                    elif (design |> Array.exists (Array.exists notFinite)) || (rhs |> Array.exists notFinite) then
                        Error (SingularDesignErr "the design matrix or the right-hand side contains a non-finite entry (NaN or infinity)")
                    else
                        try
                            let am = RealMatrix.create design
                            let bv = RealVector.create rhs
                            let (RealMatrix a) = am
                            let (RealVector b) = bv
                            let svd = a.Svd(true)
                            let rank = svd.Rank
                            let columns = a.ColumnCount
                            if rank < columns then
                                Error (RankDeficientErr (MatrixRank rank))
                            else
                                let x = svd.Solve(b)
                                Ok { solution = RealVector x; rank = MatrixRank rank; rmse = system.residualRmseAt (RealVector x) }
                        with
                        | e -> Error (SingularDesignErr $"the SVD least-squares solve failed: {e.Message}")
        }

    // The capture rows of a Phone ROI CSV file, read through the Phone ROI CSV import profile (spec 0042; spec
    // 0050 S9.4 deleted the archive-keyed seam).

    /// The label of the experiment a Mueller capture belongs to, e.g. `CPL-(QZ#90-LR#90)-CPL`; it reads through
    /// `NamingTemplate.standard` (spec 0050 S8.3).
    type MuellerExperimentLabel =
        | MuellerExperimentLabel of string

        member this.value = let (MuellerExperimentLabel l) = this in l

        /// The label's text as a name (spec 0050 S8.3).
        member this.nameText : NameText = let (MuellerExperimentLabel l) = this in NameText l

    /// A capture's ordinal within its data set.
    type CaptureIndex =
        | CaptureIndex of int

        member this.value = let (CaptureIndex i) = this in i

    /// One raw capture row of a Mueller measurement family, as loaded from a CSV file. `experiment` is the
    /// source experiment label; `captureIndex` the row's ordinal within the family; `capturedAt` the capture
    /// timestamp; `description` the analyzer azimuth (the engine `Angle`, NOT a bare degree); `avgTotal` the
    /// averaged total intensity for that capture. The (`description`, `avgTotal`) pair is the (azimuth,
    /// intensity) measurement the reconstruction's `kron4` / `MuellerSolverProxy` consume.
    type MuellerRawRow =
        {
            experiment : MuellerExperimentLabel
            captureIndex : CaptureIndex
            capturedAt : System.DateTimeOffset
            description : Angle
            avgTotal : MeasuredIntensity
        }

    /// A typed failure of parsing a Phone ROI CSV text, never a throw (spec 0050 S9.4 removed the archive cases).
    type MuellerDataError =
        | MalformedRowErr of reason : string
        | EmptyFileErr of reason : string

    // -----------------------------------------------------------------------------------------------------
    // Spec 0042 (007, IMPLEMENT) — the per-row GLUE (§2.2 / Part D): the family / matrix-kind classification, the
    // per-family source & analyzer MODELS, and the base → effective state builders. Every function is a tiny pure
    // function with a concrete elevated signature, mirroring the reference `matrix_glue.py` /
    // `cpl_cpl_analyzer.py` grammar 1:1. The effective states are built ONLY from
    // the EXISTING Propagation.fs seams (`Angle.rotationMueller`, `Propagation.analyzerMueller IdealLinear`,
    // `MuellerMatrix.element`) plus the step-001 `retarderMueller` and the
    // step-002 `kron4` — no new 4×4 algebra is introduced. The Stage-1 AIR calibration constants that POPULATE a
    // `SourceModel` / `AnalyzerModel` (zero dial, source/analyzer θ_rel, retardances) and the `buildDesign` /
    // `reconstruct` assembly are later slices; this slice ships the glue functions they consume.
    // -----------------------------------------------------------------------------------------------------

    /// The experiment family — the (source, analyzer) polarizer pair an experiment sweeps, the reference
    /// `canonical_polarizer(source)-canonical_polarizer(analyzer)` key (matrix_glue.py:92). `Lp` is an ideal
    /// linear polarizer, `Cpl` a circular (LP + retarder) polarizer; the four cases are the four families the
    /// reconstruction glue knows.
    type Family =
        | LpLp
        | LpCpl
        | CplLp
        | CplCpl

        /// The family's (source, analyzer) key as the reference writes it (spec 0050 S11.3).
        member this.text : string =
            match this with
            | LpLp -> "LP-LP"
            | LpCpl -> "LP-CPL"
            | CplLp -> "CPL-LP"
            | CplCpl -> "CPL-CPL"

    /// Which sample (or the empty AIR calibration) a measurement row constrains — the reference
    /// `object_rotation_from_parsed` matrix-kind (matrix_glue.py:101). `Air` is a bare AIR#0 calibration row (no
    /// sample); `Qz` / `Lr` a single object at its labelled azimuth; `QzLrProduct` the combined QZ·LR stack
    /// (both objects at the same azimuth).
    type MatrixKind =
        | Air
        | Qz
        | Lr
        | QzLrProduct

        /// The kind's name as the reference writes it (spec 0050 S11.3).
        member this.text : string =
            match this with
            | Air -> "AIR"
            | Qz -> "QZ"
            | Lr -> "LR"
            | QzLrProduct -> "QZ+LR"

    type NameReading with

        /// The reference `family_from_experiment` (matrix_glue.py:92): the source (Prep 1) and analyzer parts are each
        /// circular when their text contains `CPL`, an absent part being linear (spec 0050 S8.3).
        member this.family : Family =
            let isCircular (role : SlotRole) : bool =
                match this.tryFindPart role with
                | Some part -> part.value.Contains "CPL"
                | None -> false

            match isCircular (PrepSlotRole (SlotPosition 1)), isCircular (AnalyzerSlotRole (SlotPosition 1)) with
            | false, false -> LpLp
            | false, true -> LpCpl
            | true, false -> CplLp
            | true, true -> CplCpl

        /// The reference `object_rotation_from_parsed` (matrix_glue.py:101) over the Obj parts that have an angle, in
        /// position order: none is AIR at φ = 0, one its angle and kind, two or more the QZ·LR product at the first's
        /// angle (spec 0050 S8.3).
        member this.objectRotation : Angle * MatrixKind =
            let objects =
                this.parts
                |> List.choose (fun slotPart ->
                    match slotPart.role with
                    | ObjectSlotRole position ->
                        this.tryFindAngle position |> Option.map (fun angle -> position, slotPart.part, angle)
                    | SourceSlotRole
                    | PrepSlotRole _
                    | AnalyzerSlotRole _
                    | DetectorSlotRole _ -> None)
                |> List.sortBy (fun (position, _, _) -> position)

            match objects with
            | [] -> Angle.zero, Air
            | [ (_, part, angle) ] ->
                let kind =
                    match part.value.ToUpperInvariant() with
                    | "QZ" -> Qz
                    | "LR" -> Lr
                    | _ -> Air

                angle, kind
            | (_, _, angle) :: _ -> angle, QzLrProduct

        /// The kind half of `objectRotation` (spec 0050 S8.3).
        member this.matrixKind : MatrixKind = snd this.objectRotation

        /// The angle half of `objectRotation`, the label's rotation used directly as φ (spec 0050 S8.3).
        member this.objectPhi : Angle = fst this.objectRotation

    /// The per-family SOURCE model — how the input Stokes state is synthesized (the reference `SourceModel`,
    /// matrix_glue.py:128). `LpSource` is the ideal linear source `[1;1;0;0]`; `CplSource` prepends a retarder
    /// (fast axis at `thetaRel`, phase `retardance`) to it, exactly `retarder(theta_rel, retardance) @ s_lin`.
    type SourceModel =
        | LpSource
        | CplSource of thetaRel : Angle * retardance : Retardance

        /// The base (un-rotated) source Stokes state — the reference `SourceModel.base_state` (matrix_glue.py:134):
        /// `[1;1;0;0]` for a linear source, `retarderMueller thetaRel retardance · [1;1;0;0]` for a circular one.
        member this.baseStokes : StokesVector =
            let sLinear = StokesVector.create [ 1.0; 1.0; 0.0; 0.0 ]
            match this with
            | LpSource -> sLinear
            | CplSource (thetaRel, retardance) -> retarderMueller thetaRel retardance * sLinear

    /// The per-family ANALYZER model — how the analyzer's Mueller row is synthesized from its dial reading (the
    /// reference `AnalyzerModel`, matrix_glue.py:144). `kind` selects the linear (`IdealLinear`) vs circular
    /// (LP + retarder) construction; `zeroDial` / `dialSign` map the raw dial to the physical azimuth; a circular
    /// analyzer additionally carries its retarder's `thetaRelOpt` / `retardanceOpt`.
    type AnalyzerModel =
        {
            kind : PolarizerKind
            zeroDial : Angle
            dialSign : DialSign
            thetaRelOpt : Angle option
            retardanceOpt : Retardance option
        }

        /// An ideal linear analyzer, no retarder, read on `sign` from `zero` (spec 0050 S11.3).
        static member linearAt (zero : Angle) (sign : DialSign) : AnalyzerModel =
            {
                kind = IdealLinear
                zeroDial = zero
                dialSign = sign
                thetaRelOpt = None
                retardanceOpt = None
            }

        /// An LP behind a retarder at +45°, its phase the negative of `retardance`: only the relative sign to the source is
        /// constrained (`matrix_glue.py:211`; spec 0050 S11.3).
        static member circularAt (zero : Angle) (sign : DialSign) (retardance : Retardance) : AnalyzerModel =
            {
                kind = IdealCircularLeft
                zeroDial = zero
                dialSign = sign
                thetaRelOpt = Some (Angle.degree 45.0<Berreman.Constants.deg>)
                retardanceOpt = Some (Retardance.degree (- retardance.degrees * 1.0<Berreman.Constants.deg>))
            }

    /// The analyzer's physical azimuth for a raw dial reading — the reference `AnalyzerModel.physical_angle_deg`
    /// (matrix_glue.py:152): `dialSign · (dial − zeroDial)`, the rule `DialSign.physicalAngleOf` holds (spec 0050 S8.2).
    let physicalAngle (analyzer : AnalyzerModel) (dial : Angle) : Angle =
        analyzer.dialSign.physicalAngleOf analyzer.zeroDial dial

    /// Row 0 of the analyzer's Mueller matrix at a raw dial reading — the reference `AnalyzerModel.base_row`
    /// (matrix_glue.py:155). The physical azimuth β = `physicalAngle`; a linear analyzer is
    /// `analyzerMueller IdealLinear β`, a circular one is `analyzerMueller IdealLinear β · retarderMueller
    /// (β + thetaRel) retardance` (an LP behind a retarder). Row 0 is read through the `MuellerMatrix.element`
    /// seam only.
    let analyzerBaseRow (analyzer : AnalyzerModel) (dial : Angle) : RealVector4 =
        let beta = physicalAngle analyzer dial
        let linear = Propagation.analyzerMueller IdealLinear beta
        let mm =
            match analyzer.kind with
            | IdealLinear -> linear
            | IdealCircularLeft | IdealCircularRight ->
                match analyzer.thetaRelOpt, analyzer.retardanceOpt with
                | Some thetaRel, Some retardance -> linear * retarderMueller (beta + thetaRel) retardance
                | _ -> linear
        RealVector4.create
            [ mm.element 0 0
              mm.element 0 1
              mm.element 0 2
              mm.element 0 3 ]

    /// The effective source Stokes state after the object frame rotation φ — the reference `s_eff = R(φ) · s_base`
    /// (matrix_glue.py:266): `φ.rotationMueller · s`.
    let effectiveSource (phi : Angle) (s : StokesVector) : StokesVector =
        phi.rotationMueller * s

    /// The effective analyzer row after the object frame rotation φ — the reference `a_eff = a_base · R(−φ)`
    /// (matrix_glue.py:267): the row-vector times R(−φ) (`Angle.rotationMueller`), entry `j = Σᵢ a[i]·R(−φ)[i,j]`,
    /// read through the `MuellerMatrix.element` seam.
    let effectiveAnalyzer (phi : Angle) (a : RealVector4) : RealVector4 =
        let r = (Angle (- phi.value)).rotationMueller
        RealVector4.create
            [ for j in 0 .. 3 ->
                [ 0 .. 3 ] |> List.sumBy (fun i -> a.[i] * r.element i j) ]

    /// The design row one measurement imposes — the reference `lincoef = np.kron(s_eff, a_eff)`
    /// (matrix_glue.py:268): the 16-element Kronecker row `kron4 s a` (entry `4·i + j = s[i]·a[j]`), the same
    /// column-major row `MuellerMatrix.vecColumnMajor` and the `MuellerSolverProxy` consume.
    let designRow (s : StokesVector) (a : RealVector4) : DesignRow = kron4 s a

    type LeastSquaresSystem with

        /// One `designRow s a` and its signal per projection, in order (spec 0050 S11.3).
        static member ofProjections (projections : (StokesVector * RealVector4 * DetectorIntensity) list) : LeastSquaresSystem =
            {
                design = projections |> List.map (fun (s, a, _) -> designRow s a) |> List.toArray
                rhs = projections |> List.map (fun (_, _, signal) -> signal.value) |> List.toArray
            }

    // -----------------------------------------------------------------------------------------------------
    // Spec 0042 (008, IMPLEMENT) — Stage-1 AIR calibration (the reference `matrix_step1_air_fit.py`) as three
    // tiny pure functions over the AIR-series normalized traces. Only 2-column least squares + `atan2`/`arccos`
    // — no nonlinear optimizer anywhere. The re-derived constants (NOT the report's rounded §2.2 values) are
    // what POPULATE the Stage-2 `SourceModel` / `AnalyzerModel`: `freeCosineFit` gives an analyzer/source zero
    // dial `z = ½·atan2(q,p)` and a fringe `visibility`; `FringeVisibility.retardance` turns a visibility into a
    // retarder phase `arccos(|vis|)`; `sourceCplFromLpFrame` is the closed form that turns the CPL-LP LP-frame
    // fit coefficients into the source retarder's `(thetaRel, retardance)`. The 2-column solve goes THROUGH the
    // `MuellerSolverProxy` (steps 003/004) — the same optimization seam the 16-unknown reconstruction uses — so
    // the calibration and the reconstruction share one solver boundary.
    // -----------------------------------------------------------------------------------------------------

    /// The angle of `deg` degrees wrapped into `[0°, 180°)` — the reference `angle_mod_180`
    /// (matrix_step1_air_fit.py:24), i.e. numpy's `angle % 180.0` (a POSITIVE modulo). F#'s `%` takes the sign
    /// of the dividend, so a negative result is lifted by one period; a `½·atan2` zero-azimuth and a retarder
    /// fast-axis are both period-180. The wrap is taken in degrees, as the reference takes it.
    let private angleMod180 (deg : float<Berreman.Constants.deg>) : Angle =
        let m = deg % 180.0<Berreman.Constants.deg>
        Angle.degree (if m < 0.0<Berreman.Constants.deg> then m + 180.0<Berreman.Constants.deg> else m)

    /// The contrast of a calibration fringe: `√(p² + q²)` of a cosine fit `p·cos 2α + q·sin 2α`.
    type FringeVisibility =
        | FringeVisibility of float

        member this.value = let (FringeVisibility v) = this in v

        /// The retarder phase this visibility implies (the reference `retardance_from_visibility`,
        /// matrix_step1_air_fit.py:32): `arccos(|visibility|)`, with `|visibility|` clamped to `[0, 1]` so a
        /// visibility nudged past unity by fit noise stays real. A visibility of 1 (a perfect linear fringe) is
        /// zero retardance; a visibility of 0 (no contrast) is a quarter-wave (90°).
        member this.retardance : Retardance =
            let clamped = max 0.0 (min 1.0 (abs this.value))
            Retardance (acos clamped)

    /// One coefficient of a Stage-1 cosine fit `norm − 1 ≈ p·cos 2α + q·sin 2α`: a fringe component.
    type CosineCoefficient =
        | CosineCoefficient of float

        member this.value = let (CosineCoefficient c) = this in c

    /// The result of a Stage-1 2-column cosine calibration fit (the reference `fit_free_cosine`,
    /// matrix_step1_air_fit.py:101): the analyzer/source zero azimuth `zeroAzimuth = ½·atan2(q, p)`, wrapped to
    /// `[0°, 180°)` (an analyzer's `zeroDial`); the fringe `visibility = √(p² + q²)` (the retarder-phase readout
    /// via `FringeVisibility.retardance`); and the normalized-intensity residual `rmse`.
    type CosineFit =
        {
            zeroAzimuth : Angle
            visibility : FringeVisibility
            rmse : ResidualRmse
        }

    /// An AIR calibration trace: the analyzer dial angles in degrees and the normalized intensity at each, the
    /// two arrays index for index.
    [<Softellect.Sys.NumericCarrier("a measured series (R3): the dial angles and normalized intensities of an AIR calibration trace, mapped whole into a Stage-1 cosine-fit design")>]
    type AirTrace =
        {
            anglesDeg : float[]
            norm : float[]
        }

    /// The re-derived CPL SOURCE calibration constants from the LP-frame fit (the reference
    /// `derive_source_cpl_from_lp_frame`, matrix_step1_air_fit.py:147): the source retarder's relative fast-axis
    /// azimuth `thetaRel` and its phase `retardance`, both elevated (never a bare degree/phase). These feed the
    /// Stage-2 `CplSource (thetaRel, retardance)` model directly — they are the RE-derived constants, not the
    /// report's rounded §2.2 cross-check values.
    type SourceCplModel =
        {
            thetaRel : Angle
            retardance : Retardance
        }

    /// The Stage-1 free 2-column cosine fit (the reference `fit_free_cosine`, matrix_step1_air_fit.py:101): fit
    /// `norm − 1 ≈ p·cos2α + q·sin2α` over the AIR trace (`α` = the analyzer dial angle in degrees), SOLVED
    /// through the injected `MuellerSolverProxy` — the same least-squares seam the 16-unknown reconstruction
    /// uses. `design` is the two columns `[cos 2α, sin 2α]`, `rhs` is `norm − 1`; from the solved `(p, q)`:
    ///   `visibility = √(p² + q²)`  (numpy `hypot`),
    ///   `zeroAzimuth = wrap180(½·atan2(q, p)°)`,
    ///   `rmse` = the proxy's residual RMSE. Because `rhs = norm − 1` and the fitted model is `1 + design·(p,q)`,
    ///   the proxy residual `‖A·x − b‖/√m` is exactly the reference's `√(mean(resid²))` over `norm − fitted`.
    /// Returns a typed `SolverError` (never a throw) when the design is degenerate — e.g. fewer than two
    /// distinct doubled angles makes the two columns collinear (`RankDeficientErr`).
    let freeCosineFit (solver : MuellerSolverProxy) (trace : AirTrace) : Result<CosineFit, SolverError> =
        let design =
            trace.anglesDeg
            |> Array.map (fun a ->
                let twoAlpha = 2.0 * a * Angle.radiansPerDegree
                DesignRow [| cos twoAlpha; sin twoAlpha |])
        let rhs = trace.norm |> Array.map (fun n -> n - 1.0)
        solver.solveLinearLeastSquares { design = design; rhs = rhs }
        |> Result.map (fun sol ->
            let p = sol.solution.[0]
            let q = sol.solution.[1]
            {
                zeroAzimuth = angleMod180 (0.5 * (atan2 q p) / Angle.radiansPerDegree * 1.0<Berreman.Constants.deg>)
                visibility = FringeVisibility (sqrt (p * p + q * q))
                rmse = sol.rmse
            })

    /// The closed-form CPL-source model from the CPL-LP LP-frame fit coefficients (the reference
    /// `derive_source_cpl_from_lp_frame`, matrix_step1_air_fit.py:147). Given the LP-frame fit's `(qLp, uLp)`
    /// (the `[cos 2(α−z_LP), sin 2(α−z_LP)]` coefficients), the source retarder's fast axis and phase are
    ///   `2θ = atan2(1 − qLp, uLp)`,  `thetaRel = wrap180(½·2θ)`,
    ///   `cosδ = 1 − (1 − qLp) / sin²2θ`,  `retardance = arccos(clamp₋₁₊₁ cosδ)`.
    /// `cosδ` is clamped to `[−1, 1]` exactly as the reference; the port stays TOTAL — where the reference
    /// RAISES on a numerically ill-conditioned `sin 2θ ≈ 0`, the F# stays inside `SourceCplModel` (the CPL-LP
    /// AIR data has `2θ ≈ 79°`, far from the singularity, and CLAUDE.md forbids throwing across this boundary).
    let sourceCplFromLpFrame (qLp : CosineCoefficient) (uLp : CosineCoefficient) : SourceCplModel =
        let twoTheta = atan2 (1.0 - qLp.value) uLp.value
        let sinTwoTheta = sin twoTheta
        let cosDelta = 1.0 - (1.0 - qLp.value) / (sinTwoTheta * sinTwoTheta)
        let clampedCosDelta = max (-1.0) (min 1.0 cosDelta)
        {
            thetaRel = angleMod180 (0.5 * (twoTheta / Angle.radiansPerDegree) * 1.0<Berreman.Constants.deg>)
            retardance = Retardance (acos clampedCosDelta)
        }

    // -----------------------------------------------------------------------------------------------------
    // Spec 0042 (009, IMPLEMENT) — Stage-3 signal reduction (the reference `matrix_fit_linear.py`
    // `assign_gain_model`). Turns each science row's raw averaged intensity into the `signal_corrected`
    // target the least-squares reconstruction fits, in three composable steps: dark-subtract the CCD floor,
    // fit the AIR SCALAR gain, and divide out the session's gain rule (`SessionGainRule.gainAt`, spec 0050 S11.1).
    // Only dot products and a mean — no optimizer. The AIR gain is fit against the `airIdentityPred` model
    // (`a·s`, the effective-analyzer row dotted with the effective source Stokes state — the ideal AIR#0
    // prediction from step 007's `effectiveSource` / `effectiveAnalyzer`) via `scalarGain`. The
    // procedure-level constants (dark mean ≈ 869.666, the first-BullshitCheck split timestamp, the excluded
    // contaminated point) are DATA the caller supplies — never embedded in these pure functions — so the
    // Domain stays reusable and the test pins them explicitly.
    // -----------------------------------------------------------------------------------------------------

    /// The mean dark-frame (`darkness_checks.csv`) averaged intensity — the CCD/read-noise floor subtracted
    /// from every science row before fitting (the reference `dark_mean`, matrix_fit_linear.py:75). Elevated to
    /// its own single-case DU so a raw intensity floor is never confused with a corrected `signal`: `.value` is
    /// the mean count (read only at the arithmetic seam). On the measured data it is ≈ 869.666.
    type DarkMean =
        | DarkMean of double

        /// The mean dark-frame intensity (the IO/arithmetic seam — read only where a raw count is needed).
        member this.value = let (DarkMean d) = this in d

    /// Dark subtraction — the reference `signal_dark_sub = value_col − dark_mean` (matrix_fit_linear.py:76):
    /// the raw averaged intensity `avgTotal` minus the `DarkMean` floor. The elementary first step of Stage-3;
    /// `correctSignal` composes it with the gain divide.
    let darkSubtract (darkMean : DarkMean) (avgTotal : MeasuredIntensity) : MeasuredIntensity =
        MeasuredIntensity (avgTotal.value - darkMean.value)

    /// The AIR identity prediction for one measurement — the reference `identity_prediction`
    /// (matrix_fit_linear.py:60): `a · s`, the effective-analyzer row `a` (`effectiveAnalyzer`) dotted with the
    /// effective source Stokes state `s` (`effectiveSource`). For an AIR#0 row (no sample) the true Mueller
    /// matrix is the identity, so `a·(I·s) = a·s` is the ideal normalized intensity the scalar gain scales the
    /// measured `signal` onto. `s` is read by unwrapping its backing `RealVector4`; `a` directly.
    let airIdentityPred (s : StokesVector) (a : RealVector4) : DetectorIntensity =
        let (StokesVector sv) = s
        [ 0 .. 3 ] |> List.sumBy (fun i -> a.[i] * sv.[i]) |> DetectorIntensity

    /// The AIR gain of a family: the scale from the ideal AIR prediction to the dark-subtracted measured
    /// intensity, divided out of every science row (the reference `gain_est`).
    type AirGain =
        | AirGain of float

        member this.value = let (AirGain g) = this in g

    /// The scalar gain that best (least-squares) scales `model` onto `signal` — the reference `fit_scalar_gain`
    /// (matrix_fit_linear.py:66): `Σ(signalᵢ·modelᵢ) / Σ(modelᵢ²)` (`dot(signal, model) / dot(model, model)`),
    /// the closed-form 1-parameter fit of `signal ≈ gain · model` over a family's AIR#0 rows (`model` = the
    /// per-row `airIdentityPred`). `signal` and `model` are paired equal-length arrays (one entry per AIR row).
    /// Stays TOTAL: where the reference RAISES on a degenerate `Σ(modelᵢ²) ≤ 0`, the F# returns the IEEE result
    /// of the division (CLAUDE.md forbids throwing across this boundary; a real AIR model is never degenerate).
    let scalarGain (signal : MeasuredIntensity[]) (model : DetectorIntensity[]) : AirGain =
        let dot (x : float[]) (y : float[]) : float = Array.map2 (*) x y |> Array.sum
        let signalValues = signal |> Array.map (fun s -> s.value)
        let modelValues = model |> Array.map (fun m -> m.value)
        AirGain ((dot signalValues modelValues) / (dot modelValues modelValues))

    /// The corrected science-row signal the reconstruction fits — the reference `signal_corrected =
    /// signal_dark_sub / gain_est` (matrix_fit_linear.py:160): dark-subtract the raw `avgTotal`, then divide by
    /// the AIR `gain` of the row's session (its `SessionGainRule.gainAt`, spec 0050 S11.1). Composes `darkSubtract`
    /// with the gain divide, so a single dark-mean seam governs both readouts.
    let correctSignal (darkMean : DarkMean) (avgTotal : MeasuredIntensity) (gain : AirGain) : DetectorIntensity =
        DetectorIntensity ((darkSubtract darkMean avgTotal).value / gain.value)

    // -----------------------------------------------------------------------------------------------------
    // Spec 0042 (010, IMPLEMENT) — Part E's numeric core: the Stage-2 DESIGN assembly, the least-squares
    // RECONSTRUCTION through the solver proxy, and the QZ-first CASCADE product, as three tiny pure functions
    // over the step-002/007/009 primitives. `buildDesign` mirrors the reference `solve_single_object`'s row
    // selection (matrix_fit_linear.py:169) — filter to the requested `matrix_kind`, drop AIR#0 rows and the one
    // excluded contaminated point — and, for each kept science row, builds one `designRow s a` (the `lincoef`
    // column of the reference glue) paired with its `signal_corrected` target. `reconstruct` solves the
    // over-determined system `A·x ≈ b` through the injected `MuellerSolverProxy` (steps 003/004) and reshapes
    // the solved 16-vector column-major back into a `MuellerMatrix` via `columnMajorMueller`, carrying the
    // solver's `rank` / `rmse`. `cascadeProduct` forms `M_LR · M_QZ` (beam meets QZ first, then LR) reusing the
    // engine's `MuellerMatrix` `*` operator (Fields.fs:651). The composition-root wiring against the real measured
    // data (Stage 1 → 2 → 3 end-to-end, the §7.2 assertions) is the final slice.
    // -----------------------------------------------------------------------------------------------------

    /// One contaminated science point to drop from every fit — the reference `EXCLUDED_POINTS` entry
    /// (matrix_glue.py:22): the `LP-(LR#90)-CPL-2` capture at description 140° / capture_index 17 ("likely
    /// contaminated frame"). Kept as DATA the caller (`BerremanTests`) supplies — spec §0 pins the excluded
    /// contaminated point to `BerremanTests`, not the pure core — matched on `experiment` + `captureIndex` +
    /// `description`, as `MuellerRawRow` carries them.
    type ExcludedPoint =
        {
            experiment : MuellerExperimentLabel
            description : Angle
            captureIndex : CaptureIndex
        }

    /// The reconstructed Mueller matrix from a least-squares solve. `matrix` is the solved 16-vector un-vec'd
    /// column-major (`columnMajorMueller`); `rank` is the numerical rank the solver reported (16 on a full-rank
    /// reconstruction — the observability check §7.2 asserts); `rmse` is the intensity residual RMSE the solver
    /// reported. Carries the `LeastSquaresSolution` readouts lifted into Mueller space.
    type ReconstructedMatrix =
        {
            matrix : MuellerMatrix
            rank : MatrixRank
            rmse : ResidualRmse
        }

    /// Assemble the n×16 design and its corrected-signal target for one `matrix_kind` — the reference
    /// `solve_single_object` row selection (matrix_fit_linear.py:169). A science row is kept iff its label reads
    /// through `NamingTemplate.standard` with a `matrixKind` equal to `kind` (a label the template does not read is
    /// never kept, spec 0050 S8.3), it is not an AIR#0 calibration row, and it is not one of the `excluded`
    /// contaminated points (matched on experiment + capture index + analyzer dial). Each label is read once: a kept
    /// row is projected by `prepareRow row reading` with the reading that kept it, so no projection reads a label
    /// again (spec 0050 S10.3). The projection gives the row's effective source Stokes state `s`, effective
    /// analyzer row `a` and corrected signal (the reference glue's `s_eff` / `a_eff` / `signal_corrected`); the
    /// design row is `designRow s a` (the `lincoef = kron(s_eff, a_eff)` column), the target is the corrected
    /// signal. Returns the system: `design` the stacked `designRow`s `A`, `rhs` the matching targets `b`.
    let buildDesign
        (prepareRow : MuellerRawRow -> NameReading -> StokesVector * RealVector4 * DetectorIntensity)
        (excluded : ExcludedPoint list)
        (rows : MuellerRawRow list)
        (kind : MatrixKind)
        : LeastSquaresSystem =
        let isExcluded (r : MuellerRawRow) : bool =
            excluded
            |> List.exists (fun e ->
                r.experiment = e.experiment
                && r.captureIndex = e.captureIndex
                && abs (r.description.value - e.description.value) < 1e-9)
        let prepared =
            rows
            |> List.choose (fun r ->
                match NamingTemplate.standard.readName r.experiment.nameText with
                | Ok reading when
                    reading.matrixKind = kind
                    && not (r.experiment.value.Contains("(AIR#0)"))
                    && not (isExcluded r) ->
                    Some (prepareRow r reading)
                | Ok _
                | Error _ -> None)
        LeastSquaresSystem.ofProjections prepared

    /// Solve the over-determined design system `A·x ≈ b` through the injected `MuellerSolverProxy` and lift the
    /// result into Mueller space — the reference `solve_single_object` least-squares + `matrix_from_vector`
    /// column-major reshape (matrix_fit_linear.py:178/:166). `system` is the design from `buildDesign` and its
    /// corrected-signal target. On success the solved 16-vector is un-vec'd column-major via
    /// `columnMajorMueller` into the reconstructed `MuellerMatrix`, carrying the solver's numerical `rank` and
    /// residual `rmse`; a degenerate / rank-deficient design passes the typed `SolverError` through unchanged —
    /// never a throw.
    let reconstruct (solver : MuellerSolverProxy) (system : LeastSquaresSystem) : Result<ReconstructedMatrix, SolverError> =
        solver.solveLinearLeastSquares system
        |> Result.map (fun sol ->
            {
                matrix = sol.solution.columnMajorMueller
                rank = sol.rank
                rmse = sol.rmse
            })

    /// The QZ-first cascade Mueller matrix — the reference `M_LR @ M_QZ` product (matrix_fit_linear.py:358):
    /// the beam meets QZ first, then LR, so the combined stack's Mueller matrix is `M_LR · M_QZ` (the analyzer
    /// end operator left-most). Reuses the engine's `MuellerMatrix` `*` operator (Fields.fs:651) — no new 4×4
    /// algebra. Compared against the independently-reconstructed `M_{QZ+LR}` in the §7.2 cascade-identity check.
    let cascadeProduct (mLr : MuellerMatrix) (mQz : MuellerMatrix) : MuellerMatrix = mLr * mQz
