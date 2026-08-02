namespace OpticalConstructor.Domain

open System.Text.RegularExpressions
open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields
open OpticalConstructor.Domain.Library                // PolarizerKind / IdealLinear (step 007 source & analyzer models)

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
    // throw across the proxy — `RankDeficient rank` when the SVD reports a rank below the column count the
    // reconstruction needs (an under-determined design lands here since `rank ≤ rows < columns`);
    // `SingularDesign reason` for a degenerate shape (no rows, no columns, an `rhs` whose length does not
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
    /// `solveLinearLeastSquares design rhs` solves `A·x ≈ b` — `design` the row-major `A` (each inner array one
    /// stacked `kron4` measurement row), `rhs` the measured intensities `b` — and returns the minimizing
    /// `LeastSquaresSolution` (`solution`, numerical `rank`, residual `rmse = ‖A·x − b‖ / √m`), or a typed
    /// `SolverError`:
    ///   - `SingularDesign reason` — a degenerate design (no rows, no columns, an `rhs` whose length does not
    ///     match the row count, or a non-finite entry), or any MathNet exception mapped onto the channel;
    ///   - `RankDeficient rank` — the SVD's effective numerical `rank` is below the column count, so the
    ///     reconstruction is not uniquely determined.
    /// Never throws across the proxy boundary (the `.Svd()` / `.Solve()` block runs under `try/with`).
    let createMathNetSvd () : MuellerSolverProxy =
        {
            solveLinearLeastSquares =
                fun (design : float[][]) (rhs : float[]) ->
                    let widths = design |> Array.map Array.length
                    let notFinite (v : float) : bool = not (System.Double.IsFinite v)
                    if design.Length = 0 then
                        Error (SingularDesign "the design matrix has no rows (an empty least-squares system)")
                    elif widths |> Array.exists (fun w -> w <> widths.[0]) then
                        Error (SingularDesign "the design matrix is ragged (its rows are of unequal length)")
                    elif widths.[0] = 0 then
                        Error (SingularDesign "the design matrix has no columns (there are no unknowns to solve for)")
                    elif rhs.Length <> design.Length then
                        Error (SingularDesign $"the right-hand side has {rhs.Length} entries but the design has {design.Length} rows")
                    elif (design |> Array.exists (Array.exists notFinite)) || (rhs |> Array.exists notFinite) then
                        Error (SingularDesign "the design matrix or the right-hand side contains a non-finite entry (NaN or infinity)")
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
                                Error (RankDeficient rank)
                            else
                                let x = svd.Solve(b)
                                let residual = (am * RealVector x) - bv
                                let rows = a.RowCount
                                let rmse = residual.norm / sqrt (float rows)
                                Ok { solution = x.ToArray(); rank = rank; rmse = rmse }
                        with
                        | e -> Error (SingularDesign $"the SVD least-squares solve failed: {e.Message}")
        }

    // -----------------------------------------------------------------------------------------------------
    // Spec 0042 (005, ADD_CONTRACT STORE_XDUO_0009) — the Mueller measured-data LOAD seam. Declares, in the
    // Domain, the `MuellerDataProxy`: the IO boundary that turns a `MuellerDataSet` — the IDENTITY of one
    // measurement data set — into its parsed raw capture rows, or a typed `MuellerDataError`. It is the EDGE
    // the least-squares reconstruction sits behind: each loaded `MuellerRawRow` carries one capture's analyzer
    // azimuth (`description`, the engine `Angle`) and averaged intensity (`avgTotal`), the very
    // (azimuth, intensity) pairs `kron4` / the `MuellerSolverProxy` consume. A real proxy's native/parse
    // exceptions are caught AT this boundary and mapped to the typed `MuellerDataError` channel (never thrown
    // across it).
    //
    // Spec 0044 (§8.3) RE-KEYED this seam from a LOCATION to an IDENTITY. It previously took an
    // `Experiments.DataFilePath`, which forced every caller to know where the data physically lived — a file
    // path, and (once the data moved into a committed archive) an entry name inside that archive. The key is
    // now the `MuellerDataSet` DU below: pure identity, carrying NO path, NO entry name and NO archive
    // knowledge. Everything about storage — the archive, its location on disk, and the data-set → entry
    // mapping — lives behind the Storage-side factory, so a consumer (notably the test suite) states WHICH
    // data set it wants and nothing at all about WHERE it is.
    //
    // Kept as pure DATA (the `ExperimentDataProxy` convention, ExperimentDataProxy.fs:43): a record of one
    // camelCase `Result`-returning function, so logic that holds the proxy stays referentially transparent and
    // a test substitutes a canned in-memory stub of the SAME shape. Its one field is function-valued and so
    // has no structural equality, so the record is `[<ReferenceEquality>]` — a host context (Optimization /
    // Elmish) that holds one keeps its required equality, comparing the proxy by identity.
    //
    // The real archive-backed construction is `MuellerDataStore.createArchiveBacked` (Storage); the mock that
    // satisfies this surface (canned rows keyed by `MuellerDataSet`) lives with its test in `BerremanTests`,
    // mirroring the `MuellerSolverProxy` mock.
    // -----------------------------------------------------------------------------------------------------

    /// One raw capture row of a Mueller measurement family, as loaded from a CSV file — the row-level DTO AT
    /// the load boundary (bare primitives are permitted here, the parse/storage seam). `experiment` is the
    /// source experiment label; `captureIndex` the row's ordinal within the family; `capturedAt` the capture
    /// timestamp; `description` the analyzer azimuth (the engine `Angle`, NOT a bare degree); `avgTotal` the
    /// averaged total intensity for that capture. The (`description`, `avgTotal`) pair is the (azimuth,
    /// intensity) measurement the reconstruction's `kron4` / `MuellerSolverProxy` consume.
    type MuellerRawRow =
        {
            experiment : string
            captureIndex : int
            capturedAt : System.DateTimeOffset
            description : Angle
            avgTotal : float
        }

    /// WHICH measured Mueller data set a load is asking for — the seam's key (spec 0044 §8.3). This is pure
    /// IDENTITY: it names a data set by what it CONTAINS, never by where it is stored. There is deliberately
    /// no file path, no archive name and no entry name here or on any member — resolving a case to physical
    /// storage is the Storage-side factory's job alone (`MuellerDataStore.createArchiveBacked`), which is what
    /// lets a consumer ask for `LpLpFamily` without knowing that an archive exists at all.
    ///
    /// The five science sets are the experiment sweeps; `DarknessChecks` supplies the dark level subtracted
    /// from every capture, and `BullshitChecks` the diagnostic captures whose earliest timestamp is the
    /// CPL-LP gain split time. Note this is NOT the `Family` DU below: `Family` names a (source, analyzer)
    /// polarizer PAIR, whereas a `MuellerDataSet` names one recorded body of captures — CPL-CPL was recorded
    /// across two days and is therefore one `Family` but two data sets, each with its own gain rule.
    type MuellerDataSet =
        | LpLpFamily
        | LpCplFamily
        | CplLpFamily
        | CplCplDay1
        | CplCplDay2
        | DarknessChecks
        | BullshitChecks

    /// A typed failure of a Mueller data-set load — never a throw across the proxy boundary. `MalformedRow`
    /// carries a human-readable `reason` (a row that fails to parse, or a bad field); `EmptyFile` carries a
    /// `reason` for a source that yielded no capture rows. The two archive cases (spec 0044 §8.3) name the
    /// two distinct, separately actionable storage failures — a container that cannot be opened at all versus
    /// a container that opens but does not hold the requested data set — each carrying enough payload to
    /// diagnose it from a log line alone. `archive` is a bare string because it IS the storage location: a
    /// primitive at the IO boundary, which is the one place the elevation rule admits them.
    type MuellerDataError =
        | MalformedRow of reason : string
        | EmptyFile of reason : string
        | ArchiveUnreadable of archive : string * reason : string
        | DataSetMissing of dataSet : MuellerDataSet * archive : string

    /// The Mueller measured-data LOAD seam (the functional-proxy convention): a record of one camelCase
    /// `Result`-returning function that resolves a `MuellerDataSet` to its parsed capture rows.
    ///
    /// - `tryLoadDataSet dataSet` — read + parse the named data set into its ordered `MuellerRawRow` list, or
    ///   a typed `MuellerDataError` (a malformed row, an empty source, an unreadable archive, or a data set
    ///   the archive does not carry); never a throw.
    ///
    /// The caller states only WHICH data set it wants. Where that data physically lives, and how it is
    /// packaged, is entirely the factory's business (spec 0044 §8.3) — which is why this signature mentions
    /// no path type at all.
    [<ReferenceEquality>]
    type MuellerDataProxy =
        {
            tryLoadDataSet : MuellerDataSet -> Result<MuellerRawRow list, MuellerDataError>
        }

    // -----------------------------------------------------------------------------------------------------
    // Spec 0042 (007, IMPLEMENT) — the per-row GLUE (§2.2 / Part D): the experiment-label parser, the family /
    // matrix-kind classification, the per-family source & analyzer MODELS, and the base → effective state
    // builders. Every function is a tiny pure function with a concrete elevated signature, mirroring the
    // reference `matrix_glue.py` / `cpl_cpl_analyzer.py` grammar 1:1. The effective states are built ONLY from
    // the EXISTING `Propagation` seams (`rotationMueller` at Propagation.fs:117, `analyzerMueller IdealLinear` at
    // Propagation.fs:73, `muellerElement` at Propagation.fs:40) plus the step-001 `retarderMueller` and the
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

    /// Which sample (or the empty AIR calibration) a measurement row constrains — the reference
    /// `object_rotation_from_parsed` matrix-kind (matrix_glue.py:101). `Air` is a bare AIR#0 calibration row (no
    /// sample); `Qz` / `Lr` a single object at its labelled azimuth; `QzLrProduct` the combined QZ·LR stack
    /// (both objects at the same azimuth).
    type MatrixKind =
        | Air
        | Qz
        | Lr
        | QzLrProduct

    /// One parsed object token inside an experiment label's parenthesised list — a `NAME#angle` entry such as
    /// `QZ#90` (parse-boundary DTO: the raw `name` token is a primitive at the parse seam, but its labelled
    /// rotation is elevated to the engine `Angle`, since the label rotation IS used as the object frame φ
    /// directly, spec §0).
    type ParsedObject =
        {
            name : string
            labelAngle : Angle
        }

    /// An experiment label parsed into its grammar pieces — the F# port of the reference `parse_experiment_name`
    /// (cpl_cpl_analyzer.py:31): a source token, a parenthesised dash-joined object list, an analyzer token, and
    /// an optional note suffix (e.g. `CPL-(QZ#90-LR#90)-CPL`). A label that does not match the grammar parses to
    /// the empty shape (`sourceToken = None`, `analyzerToken = None`, no objects) — never a throw. The raw tokens
    /// are primitives at the parse seam.
    type ParsedExperiment =
        {
            sourceToken : string option
            analyzerToken : string option
            note : string
            objects : ParsedObject list
        }

    /// The experiment-label grammar, ported verbatim from the reference `EXPERIMENT_RE` (cpl_cpl_analyzer.py:18):
    /// `source-(inside)-analyzer` with an optional `-note` suffix.
    let private experimentRegex : Regex =
        Regex(@"^(?<src>[^-]+)-\((?<inside>[^)]*)\)-(?<an>[^-]+)(?:-(?<note>.*))?$")

    /// One `NAME#angle` object token, ported verbatim from the reference `OBJECT_RE` (cpl_cpl_analyzer.py:19).
    let private objectRegex : Regex =
        Regex(@"(?<obj>[A-Z]+)#(?<angle>-?\d+)")

    /// Parse an experiment label into its `ParsedExperiment` grammar pieces (the reference
    /// `parse_experiment_name`, cpl_cpl_analyzer.py:31). The parenthesised inner list is split on `-` and each
    /// part matched as a `NAME#angle` object; a non-matching label yields the empty parse. Total — never throws.
    let parseExperiment (label : string) : ParsedExperiment =
        let m = experimentRegex.Match(label)
        if not m.Success then
            { sourceToken = None; analyzerToken = None; note = ""; objects = [] }
        else
            let inside = m.Groups.["inside"].Value
            let parts = if inside = "" then [] else inside.Split('-') |> Array.toList
            let objects =
                parts
                |> List.choose (fun part ->
                    let om = objectRegex.Match(part)
                    if om.Success then
                        match System.Int32.TryParse(om.Groups.["angle"].Value) with
                        | true, deg -> Some { name = om.Groups.["obj"].Value; labelAngle = Angle.degree (float deg) }
                        | _ -> None
                    else
                        None)
            {
                sourceToken = Some m.Groups.["src"].Value
                analyzerToken = Some m.Groups.["an"].Value
                note = m.Groups.["note"].Value
                objects = objects
            }

    /// The experiment family from a parsed label — the reference `family_from_experiment` (matrix_glue.py:92): a
    /// token is the circular family iff it contains `CPL` (the reference `canonical_polarizer`), otherwise the
    /// linear family. Total over the four families.
    let family (parsed : ParsedExperiment) : Family =
        let isCpl (tokenOpt : string option) : bool =
            match tokenOpt with
            | Some token -> token.Contains("CPL")
            | None -> false
        match isCpl parsed.sourceToken, isCpl parsed.analyzerToken with
        | false, false -> LpLp
        | false, true -> LpCpl
        | true, false -> CplLp
        | true, true -> CplCpl

    /// The object frame rotation φ and the effective matrix kind for a parsed label — the reference
    /// `object_rotation_from_parsed` (matrix_glue.py:101). No objects → an AIR calibration at φ = 0; one object →
    /// its labelled azimuth and the object's kind (QZ / LR, else AIR); two objects → the QZ·LR product at the
    /// first object's azimuth. The label rotation is used as φ directly (spec §0).
    let private objectRotation (parsed : ParsedExperiment) : Angle * MatrixKind =
        match parsed.objects with
        | [] -> Angle.zero, Air
        | [ o ] ->
            let kind =
                match o.name.ToUpperInvariant() with
                | "QZ" -> Qz
                | "LR" -> Lr
                | _ -> Air
            o.labelAngle, kind
        | first :: _ -> first.labelAngle, QzLrProduct

    /// The effective matrix kind a parsed label constrains (the kind half of `objectRotation`).
    let matrixKind (parsed : ParsedExperiment) : MatrixKind = snd (objectRotation parsed)

    /// The object frame rotation φ a parsed label carries (the angle half of `objectRotation`): the label
    /// rotation used directly as φ (spec §0), or 0 for an AIR calibration row.
    let objectPhi (parsed : ParsedExperiment) : Angle = fst (objectRotation parsed)

    /// A named two-case sign for the analyzer dial → physical-angle map — never a naked `int`. `.value` is the
    /// multiplicative sign (`+1` / `−1`) the reference `AnalyzerModel.dial_sign` carries (matrix_glue.py:152).
    type DialSign =
        | DialPositive
        | DialNegative

        /// The multiplicative sign (+1 / −1) — read at the arithmetic seam.
        member this.value : float =
            match this with
            | DialPositive -> 1.0
            | DialNegative -> -1.0

    /// The per-family SOURCE model — how the input Stokes state is synthesized (the reference `SourceModel`,
    /// matrix_glue.py:128). `LpSource` is the ideal linear source `[1;1;0;0]`; `CplSource` prepends a retarder
    /// (fast axis at `thetaRel`, phase `retardance`) to it, exactly `retarder(theta_rel, retardance) @ s_lin`.
    type SourceModel =
        | LpSource
        | CplSource of thetaRel : Angle * retardance : Retardance

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

    /// The base (un-rotated) source Stokes state for a source model — the reference `SourceModel.base_state`
    /// (matrix_glue.py:134): `[1;1;0;0]` for a linear source, `retarderMueller thetaRel retardance · [1;1;0;0]`
    /// for a circular one.
    let sourceBaseStokes (source : SourceModel) : StokesVector =
        let sLinear = StokesVector.create [ 1.0; 1.0; 0.0; 0.0 ]
        match source with
        | LpSource -> sLinear
        | CplSource (thetaRel, retardance) -> retarderMueller thetaRel retardance * sLinear

    /// The analyzer's physical azimuth for a raw dial reading — the reference `AnalyzerModel.physical_angle_deg`
    /// (matrix_glue.py:152): `dialSign · (dial − zeroDial)`.
    let physicalAngle (analyzer : AnalyzerModel) (dial : Angle) : Angle =
        Angle (analyzer.dialSign.value * (dial.value - analyzer.zeroDial.value))

    /// Row 0 of the analyzer's Mueller matrix at a raw dial reading — the reference `AnalyzerModel.base_row`
    /// (matrix_glue.py:155). The physical azimuth β = `physicalAngle`; a linear analyzer is
    /// `analyzerMueller IdealLinear β`, a circular one is `analyzerMueller IdealLinear β · retarderMueller
    /// (β + thetaRel) retardance` (an LP behind a retarder). Row 0 is read through the `Propagation.muellerElement`
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
            [ Propagation.muellerElement mm 0 0
              Propagation.muellerElement mm 0 1
              Propagation.muellerElement mm 0 2
              Propagation.muellerElement mm 0 3 ]

    /// The effective source Stokes state after the object frame rotation φ — the reference `s_eff = R(φ) · s_base`
    /// (matrix_glue.py:266): `Propagation.rotationMueller φ · s`.
    let effectiveSource (phi : Angle) (s : StokesVector) : StokesVector =
        Propagation.rotationMueller phi * s

    /// The effective analyzer row after the object frame rotation φ — the reference `a_eff = a_base · R(−φ)`
    /// (matrix_glue.py:267): the row-vector times `Propagation.rotationMueller (−φ)`, entry `j = Σᵢ a[i]·R(−φ)[i,j]`,
    /// read through the `Propagation.muellerElement` seam.
    let effectiveAnalyzer (phi : Angle) (a : RealVector4) : RealVector4 =
        let r = Propagation.rotationMueller (Angle (- phi.value))
        RealVector4.create
            [ for j in 0 .. 3 ->
                [ 0 .. 3 ] |> List.sumBy (fun i -> a.[i] * Propagation.muellerElement r i j) ]

    /// The design row one measurement imposes — the reference `lincoef = np.kron(s_eff, a_eff)`
    /// (matrix_glue.py:268): the 16-element Kronecker row `kron4 s a` (entry `4·i + j = s[i]·a[j]`), the same
    /// column-major row `vecColumnMajor` and the `MuellerSolverProxy` consume.
    let designRow (s : StokesVector) (a : RealVector4) : float[] = kron4 s a

    // -----------------------------------------------------------------------------------------------------
    // Spec 0042 (008, IMPLEMENT) — Stage-1 AIR calibration (the reference `matrix_step1_air_fit.py`) as three
    // tiny pure functions over the AIR-series normalized traces. Only 2-column least squares + `atan2`/`arccos`
    // — no nonlinear optimizer anywhere. The re-derived constants (NOT the report's rounded §2.2 values) are
    // what POPULATE the Stage-2 `SourceModel` / `AnalyzerModel`: `freeCosineFit` gives an analyzer/source zero
    // dial `z = ½·atan2(q,p)` and a fringe `visibility`; `retardanceFromVisibility` turns a visibility into a
    // retarder phase `arccos(|vis|)`; `sourceCplFromLpFrame` is the closed form that turns the CPL-LP LP-frame
    // fit coefficients into the source retarder's `(thetaRel, retardance)`. The 2-column solve goes THROUGH the
    // `MuellerSolverProxy` (steps 003/004) — the same optimization seam the 16-unknown reconstruction uses — so
    // the calibration and the reconstruction share one solver boundary.
    // -----------------------------------------------------------------------------------------------------

    /// Wrap a degree value into `[0, 180)` — the reference `angle_mod_180` (matrix_step1_air_fit.py:24), i.e.
    /// numpy's `angle % 180.0` (a POSITIVE modulo). F#'s `%` takes the sign of the dividend, so a negative
    /// result is lifted by one period; a `½·atan2` zero-azimuth and a retarder fast-axis are both period-180.
    let private wrapDeg180 (deg : float) : float =
        let m = deg % 180.0
        if m < 0.0 then m + 180.0 else m

    /// The result of a Stage-1 2-column cosine calibration fit (the reference `fit_free_cosine`,
    /// matrix_step1_air_fit.py:101): the analyzer/source zero azimuth `zeroDeg = ½·atan2(q, p)` in degrees,
    /// wrapped to `[0, 180)`; the fringe `visibility = √(p² + q²)` (the retarder-phase readout via
    /// `retardanceFromVisibility`); and the normalized-intensity residual `rmse`. These are dimensionless fit
    /// scalars (a raw dial angle, a contrast ratio, and a residual) reported exactly as the reference emits
    /// them — the elevated `Angle` / `Retardance` that CONSUME them are built downstream (a `zeroDial`, a
    /// source `retardance`), keeping this record a faithful 1:1 mirror of the reference fit output.
    type CosineFit =
        {
            zeroDeg : float
            visibility : float
            rmse : float
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
    ///   `zeroDeg = wrap180(½·atan2(q, p)°)`,
    ///   `rmse` = the proxy's residual RMSE. Because `rhs = norm − 1` and the fitted model is `1 + design·(p,q)`,
    ///   the proxy residual `‖A·x − b‖/√m` is exactly the reference's `√(mean(resid²))` over `norm − fitted`.
    /// Returns a typed `SolverError` (never a throw) when the design is degenerate — e.g. fewer than two
    /// distinct doubled angles makes the two columns collinear (`RankDeficient`).
    let freeCosineFit
        (solver : MuellerSolverProxy)
        (anglesDeg : float[])
        (norm : float[])
        : Result<CosineFit, SolverError> =
        let design =
            anglesDeg
            |> Array.map (fun a ->
                let twoAlpha = 2.0 * a * degree
                [| cos twoAlpha; sin twoAlpha |])
        let rhs = norm |> Array.map (fun n -> n - 1.0)
        solver.solveLinearLeastSquares design rhs
        |> Result.map (fun sol ->
            let p = sol.solution.[0]
            let q = sol.solution.[1]
            {
                zeroDeg = wrapDeg180 (0.5 * (atan2 q p) / degree)
                visibility = sqrt (p * p + q * q)
                rmse = sol.rmse
            })

    /// A retarder phase from a fringe visibility (the reference `retardance_from_visibility`,
    /// matrix_step1_air_fit.py:32): `arccos(|visibility|)`, with `|visibility|` clamped to `[0, 1]` so a
    /// visibility nudged past unity by fit noise stays real. Returned as an elevated `Retardance` (radians on
    /// the wire; read its `.degrees` for the ≈83.67° analyzer-δ cross-check). A visibility of 1 (a perfect
    /// linear fringe) is zero retardance; a visibility of 0 (no contrast) is a quarter-wave (90°).
    let retardanceFromVisibility (visibility : float) : Retardance =
        let clamped = max 0.0 (min 1.0 (abs visibility))
        Retardance (acos clamped)

    /// The closed-form CPL-source model from the CPL-LP LP-frame fit coefficients (the reference
    /// `derive_source_cpl_from_lp_frame`, matrix_step1_air_fit.py:147). Given the LP-frame fit's `(qLp, uLp)`
    /// (the `[cos 2(α−z_LP), sin 2(α−z_LP)]` coefficients), the source retarder's fast axis and phase are
    ///   `2θ = atan2(1 − qLp, uLp)`,  `thetaRel = wrap180(½·2θ)`,
    ///   `cosδ = 1 − (1 − qLp) / sin²2θ`,  `retardance = arccos(clamp₋₁₊₁ cosδ)`.
    /// `cosδ` is clamped to `[−1, 1]` exactly as the reference; the port stays TOTAL — where the reference
    /// RAISES on a numerically ill-conditioned `sin 2θ ≈ 0`, the F# stays inside `SourceCplModel` (the CPL-LP
    /// AIR data has `2θ ≈ 79°`, far from the singularity, and CLAUDE.md forbids throwing across this boundary).
    let sourceCplFromLpFrame (qLp : float) (uLp : float) : SourceCplModel =
        let twoTheta = atan2 (1.0 - qLp) uLp
        let sinTwoTheta = sin twoTheta
        let cosDelta = 1.0 - (1.0 - qLp) / (sinTwoTheta * sinTwoTheta)
        let clampedCosDelta = max (-1.0) (min 1.0 cosDelta)
        {
            thetaRel = Angle.degree (wrapDeg180 (0.5 * (twoTheta / degree)))
            retardance = Retardance (acos clampedCosDelta)
        }

    // -----------------------------------------------------------------------------------------------------
    // Spec 0042 (009, IMPLEMENT) — Stage-3 signal reduction (the reference `matrix_fit_linear.py`
    // `assign_gain_model`). Turns each science row's raw averaged intensity into the `signal_corrected`
    // target the least-squares reconstruction fits, in three composable steps: dark-subtract the CCD floor,
    // fit the per-family AIR SCALAR gain, and divide it out. Only dot products, a mean, and a linear time
    // interpolation — no optimizer. The per-family AIR gain is fit against the `airIdentityPred` model
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
    let darkSubtract (darkMean : DarkMean) (avgTotal : float) : float =
        avgTotal - darkMean.value

    /// The AIR identity prediction for one measurement — the reference `identity_prediction`
    /// (matrix_fit_linear.py:60): `a · s`, the effective-analyzer row `a` (`effectiveAnalyzer`) dotted with the
    /// effective source Stokes state `s` (`effectiveSource`). For an AIR#0 row (no sample) the true Mueller
    /// matrix is the identity, so `a·(I·s) = a·s` is the ideal normalized intensity the scalar gain scales the
    /// measured `signal` onto. `s` is read by unwrapping its backing `RealVector4`; `a` directly.
    let airIdentityPred (s : StokesVector) (a : RealVector4) : float =
        let (StokesVector sv) = s
        [ 0 .. 3 ] |> List.sumBy (fun i -> a.[i] * sv.[i])

    /// The scalar gain that best (least-squares) scales `model` onto `signal` — the reference `fit_scalar_gain`
    /// (matrix_fit_linear.py:66): `Σ(signalᵢ·modelᵢ) / Σ(modelᵢ²)` (`dot(signal, model) / dot(model, model)`),
    /// the closed-form 1-parameter fit of `signal ≈ gain · model` over a family's AIR#0 rows (`model` = the
    /// per-row `airIdentityPred`). `signal` and `model` are paired equal-length arrays (one entry per AIR row).
    /// Stays TOTAL: where the reference RAISES on a degenerate `Σ(modelᵢ²) ≤ 0`, the F# returns the IEEE result
    /// of the division (CLAUDE.md forbids throwing across this boundary; a real AIR model is never degenerate).
    let scalarGain (signal : float[]) (model : float[]) : float =
        let dot (x : float[]) (y : float[]) : float = Array.map2 (*) x y |> Array.sum
        (dot signal model) / (dot model model)

    /// The AIR gain of a "mean of two repeats" family (the stable `lp_lp` / `lp_cpl` families,
    /// matrix_fit_linear.py:123/:132): the two valid AIR#0 repeat gains, whose mean `familyGain` applies to
    /// every row of the family. Bare `float` gains by the slice's `scalarGain … : float` contract — a gain is a
    /// dimensionless scale readout, elevated where the corrected signal is consumed downstream.
    type RepeatPairGain =
        {
            gainA : float
            gainB : float
        }

    /// The CPL-CPL family's AIR gain, whose two acquisition days need different rules (matrix_fit_linear.py:92/
    /// :116). `Day1Interp` time-INTERPOLATES between the family's opening and closing AIR blocks — `gainStart`
    /// at the block's mean capture time `timeStart`, `gainEnd` at `timeEnd`; `Day2Single` is the one AIR block's
    /// gain (a stable session). The caller picks the case matching the row's acquisition session (the reference
    /// dispatches on `source_glue_file`).
    type CplCplGain =
        | Day1Interp of gainStart : float * timeStart : System.DateTimeOffset * gainEnd : float * timeEnd : System.DateTimeOffset
        | Day2Single of gain : float

    /// The CPL-LP family's AIR gain, SPLIT at the first BullshitCheck timestamp (matrix_fit_linear.py:141): rows
    /// captured strictly before `splitTime` take `gainPre` (the pre-check AIR block); rows at or after it take
    /// `gainPost` (the post-check AIR block). `splitTime` is the caller-supplied split constant (the reference
    /// reads it from `bullshit_checks_comparison.json[0]`), never embedded here.
    type SplitGain =
        {
            gainPre : float
            gainPost : float
            splitTime : System.DateTimeOffset
        }

    /// The per-family AIR gain calibration bundle — one entry per family's gain rule (matrix_fit_linear.py's
    /// `assign_gain_model` branches). `familyGain` selects the entry by `Family` and applies its rule at a
    /// capture time. `Family` is 4-case but the reference has five gain rules (CPL-CPL splits into day1/day2), so
    /// `cplCpl` carries a `CplCplGain` DU the caller resolves per acquisition session — using both mandated
    /// `familyGain` parameters (`family` as the dispatch key, `cal` as the data) meaningfully.
    type GainCalibration =
        {
            lpLp : RepeatPairGain
            lpCpl : RepeatPairGain
            cplLp : SplitGain
            cplCpl : CplCplGain
        }

    /// The per-family scalar AIR gain to divide out of a science row captured at time `t` — the reference
    /// `assign_gain_model` per-`source_glue_file` dispatch (matrix_fit_linear.py:73) collapsed to a pure
    /// function keyed by `Family`:
    ///   - `LpLp` / `LpCpl` — the mean of the family's two AIR repeat gains (time-independent, a stable family);
    ///   - `CplLp` — split at the first BullshitCheck timestamp: `gainPre` when `t < splitTime`, else `gainPost`;
    ///   - `CplCpl` `Day2Single` — the one AIR block's gain; `Day1Interp` — the time-interpolated gain
    ///     `(1−w)·gainStart + w·gainEnd`, `w = clamp₀₁((t − timeStart)/(timeEnd − timeStart))` (the reference's
    ///     clipped linear weight, matrix_fit_linear.py:108).
    /// Stays TOTAL: where the reference RAISES on a non-positive `Day1Interp` interval, the F# uses weight 0
    /// (a degenerate interval never arises on real AIR data; CLAUDE.md forbids throwing across this boundary).
    let familyGain (family : Family) (cal : GainCalibration) (t : System.DateTimeOffset) : float =
        let meanPair (p : RepeatPairGain) : float = 0.5 * (p.gainA + p.gainB)
        match family with
        | LpLp -> meanPair cal.lpLp
        | LpCpl -> meanPair cal.lpCpl
        | CplLp -> if t < cal.cplLp.splitTime then cal.cplLp.gainPre else cal.cplLp.gainPost
        | CplCpl ->
            match cal.cplCpl with
            | Day2Single gain -> gain
            | Day1Interp (gainStart, timeStart, gainEnd, timeEnd) ->
                let span = (timeEnd - timeStart).TotalSeconds
                let weight =
                    if span <= 0.0 then 0.0
                    else max 0.0 (min 1.0 ((t - timeStart).TotalSeconds / span))
                (1.0 - weight) * gainStart + weight * gainEnd

    /// The corrected science-row signal the reconstruction fits — the reference `signal_corrected =
    /// signal_dark_sub / gain_est` (matrix_fit_linear.py:160): dark-subtract the raw `avgTotal`, then divide by
    /// the per-family AIR `gain` (from `familyGain`). Composes `darkSubtract` with the gain divide, so a single
    /// dark-mean seam governs both readouts.
    let correctSignal (darkMean : DarkMean) (avgTotal : float) (gain : float) : float =
        (darkSubtract darkMean avgTotal) / gain

    // -----------------------------------------------------------------------------------------------------
    // Spec 0042 (010, IMPLEMENT) — Part E's numeric core: the Stage-2 DESIGN assembly, the least-squares
    // RECONSTRUCTION through the solver proxy, and the QZ-first CASCADE product, as three tiny pure functions
    // over the step-002/007/009 primitives. `buildDesign` mirrors the reference `solve_single_object`'s row
    // selection (matrix_fit_linear.py:169) — filter to the requested `matrix_kind`, drop AIR#0 rows and the one
    // excluded contaminated point — and, for each kept science row, builds one `designRow s a` (the `lincoef`
    // column of the reference glue) paired with its `signal_corrected` target. `reconstruct` solves the
    // over-determined system `A·x ≈ b` through the injected `MuellerSolverProxy` (steps 003/004) and reshapes
    // the solved 16-vector column-major back into a `MuellerMatrix` via `muellerOfVecColumnMajor`, carrying the
    // solver's `rank` / `rmse`. `cascadeProduct` forms `M_LR · M_QZ` (beam meets QZ first, then LR) reusing the
    // engine's `MuellerMatrix` `*` operator (Fields.fs:651). The composition-root wiring against the real measured
    // data (Stage 1 → 2 → 3 end-to-end, the §7.2 assertions) is the final slice.
    // -----------------------------------------------------------------------------------------------------

    /// One contaminated science point to drop from every fit — the reference `EXCLUDED_POINTS` entry
    /// (matrix_glue.py:22): the `LP-(LR#90)-CPL-2` capture at description 140° / capture_index 17 ("likely
    /// contaminated frame"). Kept as DATA the caller (`BerremanTests`) supplies — spec §0 pins the excluded
    /// contaminated point to `BerremanTests`, not the pure core — matched on `experiment` + `captureIndex` +
    /// `description` (the raw `experiment` token is a primitive at the label seam; the analyzer dial is the
    /// elevated engine `Angle`, matching `MuellerRawRow`).
    type ExcludedPoint =
        {
            experiment : string
            description : Angle
            captureIndex : int
        }

    /// The reconstructed Mueller matrix from a least-squares solve — never a bare `float[]`. `matrix` is the
    /// solved 16-vector un-vec'd column-major (`muellerOfVecColumnMajor`); `rank` is the numerical rank the
    /// solver reported (16 on a full-rank reconstruction — the observability check §7.2 asserts); `rmse` is the
    /// intensity residual RMSE the solver reported. Carries the `LeastSquaresSolution` readouts lifted into
    /// Mueller space.
    type ReconstructedMatrix =
        {
            matrix : MuellerMatrix
            rank : int
            rmse : float
        }

    /// Assemble the n×16 design and its corrected-signal target for one `matrix_kind` — the reference
    /// `solve_single_object` row selection (matrix_fit_linear.py:169). A science row is kept iff its parsed
    /// `matrixKind` equals `kind`, it is not an AIR#0 calibration row, and it is not one of the `excluded`
    /// contaminated points (matched on experiment + capture index + analyzer dial). Each kept row is projected
    /// through `prepareRow` to its effective source Stokes state `s`, effective analyzer row `a`, and corrected
    /// signal (the caller — slice 011 — closes `prepareRow` over the calibrated per-family models + gain
    /// calibration + dark mean, i.e. the reference glue `s_eff` / `a_eff` / `signal_corrected` columns); the
    /// design row is `designRow s a` (the `lincoef = kron(s_eff, a_eff)` column), the target is the corrected
    /// signal. Returns `(design, target)` — `design` the row-major `A` (each inner array one `designRow`),
    /// `target` the matching `b`.
    let buildDesign
        (prepareRow : MuellerRawRow -> StokesVector * RealVector4 * float)
        (excluded : ExcludedPoint list)
        (rows : MuellerRawRow list)
        (kind : MatrixKind)
        : float[][] * float[] =
        let isExcluded (r : MuellerRawRow) : bool =
            excluded
            |> List.exists (fun e ->
                r.experiment = e.experiment
                && r.captureIndex = e.captureIndex
                && abs (r.description.value - e.description.value) < 1e-9)
        let kept =
            rows
            |> List.filter (fun r ->
                matrixKind (parseExperiment r.experiment) = kind
                && not (r.experiment.Contains("(AIR#0)"))
                && not (isExcluded r))
        let prepared = kept |> List.map prepareRow
        let design = prepared |> List.map (fun (s, a, _) -> designRow s a) |> List.toArray
        let target = prepared |> List.map (fun (_, _, signal) -> signal) |> List.toArray
        design, target

    /// Solve the over-determined design system `A·x ≈ b` through the injected `MuellerSolverProxy` and lift the
    /// result into Mueller space — the reference `solve_single_object` least-squares + `matrix_from_vector`
    /// column-major reshape (matrix_fit_linear.py:178/:166). `x` is the row-major design (each inner array one
    /// `designRow` from `buildDesign`), `y` the corrected-signal target. On success the solved 16-vector is
    /// un-vec'd column-major via `muellerOfVecColumnMajor` into the reconstructed `MuellerMatrix`, carrying the
    /// solver's numerical `rank` and residual `rmse`; a degenerate / rank-deficient design passes the typed
    /// `SolverError` through unchanged — never a throw.
    let reconstruct
        (solver : MuellerSolverProxy)
        (x : float[][])
        (y : float[])
        : Result<ReconstructedMatrix, SolverError> =
        solver.solveLinearLeastSquares x y
        |> Result.map (fun sol ->
            {
                matrix = muellerOfVecColumnMajor sol.solution
                rank = sol.rank
                rmse = sol.rmse
            })

    /// The QZ-first cascade Mueller matrix — the reference `M_LR @ M_QZ` product (matrix_fit_linear.py:358):
    /// the beam meets QZ first, then LR, so the combined stack's Mueller matrix is `M_LR · M_QZ` (the analyzer
    /// end operator left-most). Reuses the engine's `MuellerMatrix` `*` operator (Fields.fs:651) — no new 4×4
    /// algebra. Compared against the independently-reconstructed `M_{QZ+LR}` in the §7.2 cascade-identity check.
    let cascadeProduct (mLr : MuellerMatrix) (mQz : MuellerMatrix) : MuellerMatrix = mLr * mQz
