namespace BerremanTests

open System.IO
open System.IO.Compression                             // ZipArchive — used ONLY to build a throwaway archive for a failure-path test
open Berreman.Geometry
open Berreman.Fields
open OpticalConstructor.Domain
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Storage                        // MuellerDataStore — the REAL parseMuellerCsv / createArchiveBacked
open Xunit
open BerremanTests.MatrixComparison

/// Spec 0042 — acceptance suite for the Mueller-reconstruction pipeline (`MuellerReconstruction` + the
/// archive-backed `MuellerDataStore` + the vendored-MathNet SVD solver). Most facts reuse the
/// element-by-element Mueller compare loop from `MuellerMatrixTests.fs:20` and the shared `allowedDiff`
/// tolerance (`MatrixComparison.fs:13`).
///
/// Spec 0044 §8 retired the external data dependency and tightened the measured-data acceptance:
///   - the measured data is COMMITTED (`Berreman/Data/MuellerMatrix/data.zip`) and reached through the
///     re-keyed `MuellerDataProxy`, which takes a `MuellerDataSet` (an identity) rather than a
///     `DataFilePath` (a location) — no test here names a path, an archive or an entry;
///   - nothing walks up to, or otherwise probes for, an external checkout, and the two
///     data-backed facts no longer skip: absent data is now a defect and fails;
///   - the §7.2 matrices and the Stage-1 constants are asserted against the reference pipeline's own
///     full-precision output (`reference_step*_summary.json` in that same folder) at `dataMatrixTol` /
///     `stage1Tol`, rather than against the report's rounded printed values at ~2e-3.
///
/// The following report facets are deliberately NOT asserted here — each is a candidate for a future slice:
///   - §6 calibration table (the per-constant re-derivation table),
///   - §7 per-family fit RMSE (the by-family residual breakdown),
///   - Cloude realizability (coherency-matrix eigenvalue non-negativity),
///   - Lu–Chipman decomposition (diattenuator / retarder / depolarizer factorization),
///   - condition-number reporting (the design's singular-value spread), and
///   - the ±45 / ±90 sign / handedness tweaks (source / analyzer retardance-sign sweeps).
type MuellerReconstructionTests() =

    /// The same element-by-element compare loop as `MuellerMatrixTests`, over the shared `allowedDiff`.
    let assertMuellerEqual (MuellerMatrix expected) (MuellerMatrix actual) =
        for i in 0..3 do
            for j in 0..3 do
                let d = abs (expected.[i, j] - actual.[i, j])
                Assert.True(d < allowedDiff, $"M[{i},{j}] differs by {d}")

    // Spec 0042 (002) — the column-major linear-algebra core. A NON-symmetric 4×4 with distinct entries so a
    // row/column-major mix-up is observable (M[i,j] ≠ M[j,i] off the diagonal); reused by the round-trip and
    // the transpose guard. Bound here, ahead of the members: in an F# class type every `let` binding must
    // precede the first member (FS0960), so this fixture cannot sit down among the [<Fact>] members below.
    let asymmetricM =
        Propagation.muellerOfRows
            [ [ 1.0; 0.2; -0.3; 0.4 ]
              [ 0.5; 0.6; 0.7; -0.8 ]
              [ -0.9; 1.0; 0.1; 0.2 ]
              [ 0.3; -0.4; 0.5; 0.6 ] ]

    // Spec 0042 (003, ADD_CONTRACT SVC_XDUO_0002) — the least-squares SOLVE seam. The MOCK and its canned
    // solution are bound here, ahead of the members (FS0960: in a class type every `let` binding precedes the
    // first member). `makeMockSolver` is an inline `MuellerSolverProxy` stub whose `solveLinearLeastSquares`
    // keys off the design HEIGHT (16 = the Mueller-vec unknown count): a full-height design returns the canned
    // `LeastSquaresSolution`; fewer rows than unknowns returns `Error (RankDeficient rows)`; an empty design
    // returns `Error (SingularDesign _)`. This exercises the exact signature and BOTH typed error cases with
    // no real linear algebra — the real MathNet-SVD proxy (`createMathNetSvd ()`) lands in step 004.
    let cannedSolution : LeastSquaresSolution =
        { solution = Array.init 16 (fun k -> 0.1 * float k); rank = 16; rmse = 0.0 }

    let makeMockSolver () : MuellerSolverProxy =
        {
            solveLinearLeastSquares =
                fun (design : float[][]) (_rhs : float[]) ->
                    match design with
                    | [||] -> Error (SingularDesign "the mock rejects an empty design matrix")
                    | rows when rows.Length < 16 -> Error (RankDeficient rows.Length)
                    | _ -> Ok cannedSolution
        }

    // Spec 0042 (005, ADD_CONTRACT STORE_XDUO_0009) — the Mueller measured-data LOAD seam, re-keyed by spec
    // 0044 §8.3 from a LOCATION (`DataFilePath`) to an IDENTITY (`MuellerDataSet`). The MOCK and its canned
    // rows are bound here, ahead of the members (FS0960: in a class type every `let` binding precedes the
    // first member).
    //
    // `makeMockData` is an inline `MuellerDataProxy` stub whose `tryLoadDataSet` keys the canned
    // `MuellerRawRow` list off the `MuellerDataSet` case itself: a seeded data set returns its rows; an
    // unseeded one returns `Error (EmptyFile _)`, never a throw. Keying on the DU rather than on a path
    // string is the whole point of the re-key — the mock, like the real store, is asked WHICH data set is
    // wanted and is never told where anything lives. This exercises the exact signature and the typed-error
    // case with no filesystem IO at all. The `capturedAt` timestamps are fixed literals (deterministic across
    // runs — no ambient clock read).
    let cannedRows : MuellerRawRow list =
        [ { experiment = "E1"; captureIndex = 0; capturedAt = System.DateTimeOffset(2026, 7, 15, 12, 0, 0, System.TimeSpan.Zero); description = Angle.degree 0.0; avgTotal = 0.42 }
          { experiment = "E1"; captureIndex = 1; capturedAt = System.DateTimeOffset(2026, 7, 15, 12, 5, 0, System.TimeSpan.Zero); description = Angle.degree 45.0; avgTotal = 0.31 } ]

    let makeMockData (rowsByDataSet : Map<MuellerDataSet, MuellerRawRow list>) : MuellerDataProxy =
        {
            tryLoadDataSet =
                fun (dataSet : MuellerDataSet) ->
                    match Map.tryFind dataSet rowsByDataSet with
                    | Some rows -> Ok rows
                    | None -> Error (EmptyFile $"the mock has no canned capture rows for data set %A{dataSet}")
        }

    let seededDataMock () : MuellerDataProxy =
        makeMockData (Map.ofList [ LpLpFamily, cannedRows ])

    // Spec 0042 (006, IMPLEMENT_CONTRACT STORE_XDUO_0009) — a committed multi-column Mueller capture CSV
    // fixture for the REAL MuellerDataStore.parseMuellerCsv. A UTF-8 BOM prefix (char 0xFEFF, built here rather
    // than embedded in the source) exercises the parser's BOM-tolerance; the header carries the many real
    // columns (iso / exposure_ns / avg_R/G/B / …) the reconstruction does not consume, so reading the five
    // required columns BY NAME is exactly what is under test. The `captured_at` timestamps carry an explicit
    // UTC offset — deterministic across runs, no ambient-clock or local-timezone dependence. Bound here, ahead
    // of the members (FS0960: in a class type every `let` binding precedes the first member).
    let muellerCsvFixture : string =
        string (char 0xFEFF)
        + "experiment,capture_index,captured_at,description,iso,exposure_ns,avg_R,avg_G,avg_B,avg_total\n"
        + "CPL-(AIR#0)-CPL,0,2026-07-15T12:00:00+00:00,0,100,5000,10.0,11.0,12.0,869.5\n"
        + "CPL-(AIR#0)-CPL,1,2026-07-15T12:05:00+00:00,45,100,5000,20.0,21.0,22.0,912.25\n"

    // A malformed fixture: the `capture_index` cell is not an integer, so parseMuellerCsv must map it to a
    // typed MalformedRow (never a throw across into the pure Domain).
    let malformedMuellerCsv : string =
        "experiment,capture_index,captured_at,description,avg_total\n"
        + "E1,not-an-int,2026-07-15T12:00:00+00:00,0,869.5\n"

    // Spec 0042 (007, IMPLEMENT) — component-wise closeness helpers for the effective-state +90/−90 invariance,
    // over the shared `allowedDiff` (no hand-rolled epsilon). Bound here, ahead of the members (FS0960: in a
    // class type every `let` binding precedes the first member). StokesVector is read through the
    // `Propagation.stokesComponents` seam; RealVector4 through its indexer.
    let assertStokesClose (a : StokesVector) (b : StokesVector) =
        let (a0, a1, a2, a3) = Propagation.stokesComponents a
        let (b0, b1, b2, b3) = Propagation.stokesComponents b
        Assert.True(abs (a0 - b0) < allowedDiff, $"S0 differs by {abs (a0 - b0)}")
        Assert.True(abs (a1 - b1) < allowedDiff, $"S1 differs by {abs (a1 - b1)}")
        Assert.True(abs (a2 - b2) < allowedDiff, $"S2 differs by {abs (a2 - b2)}")
        Assert.True(abs (a3 - b3) < allowedDiff, $"S3 differs by {abs (a3 - b3)}")

    let assertVec4Close (a : RealVector4) (b : RealVector4) =
        for i in 0 .. 3 do
            Assert.True(abs (a.[i] - b.[i]) < allowedDiff, $"component {i} differs by {abs (a.[i] - b.[i])}")

    // Spec 0042 (008, IMPLEMENT) — Stage-1 AIR calibration helpers. Bound here, ahead of the members (FS0960:
    // in a class type every `let` binding precedes the first member). Radians-per-degree reached by its full
    // module path (no extra `open` of the numeric-wrapper module, which would drag unrelated names into scope).
    let degree = Berreman.MathNetNumericsMath.degree

    // ---------------------------------------------------------------------------------------------------
    // Spec 0044 §8.5 / §9 — acceptance bands for the measured-data facts, against the reference pipeline's
    // OWN full-precision output (Berreman/Data/MuellerMatrix/reference_step1_summary.json and
    // reference_step2_linear_summary.json). These replaced the former ~1° / 2e-3 bands, which compared
    // against the report's ROUNDED printed values and therefore could not detect numerical drift smaller
    // than the rounding.
    //
    // The §9 protocol governs these two numbers: measure the actual agreement, pin ONE ORDER OF MAGNITUDE
    // looser than observed, and record the observed figure here. They are never widened to make a failing
    // test pass — the F# re-derives every constant independently through a different SVD implementation, so
    // agreement worse than ~1e-6 would indicate a PROCEDURAL divergence from the reference pipeline that
    // must be found and fixed, not absorbed.
    //
    // OBSERVED (measured 2026-08-02; recorded per §9, see specs/0044/.manual/006-implementation-log.md):
    //   Stage-1 constants — dark_mean, z_LP, z_CPL and δ_an reproduce the reference BIT-EXACTLY (Δ = 0);
    //                       θ_src is off by 7.1e-15° and δ_src by 1.4e-14°, i.e. one to two ulp.
    //   Stage-3 matrices  — worst element deviations 4.5e-11 (M_QZ, M[2,3]), 4.9e-11 (M_LR, M[0,0]) and
    //                       3.9e-11 (M_{QZ+LR}, M[1,3]).
    // The matrix figures are the amplification of that ulp-level calibration difference through a
    // 900-row × 16-column least-squares solve of condition number ≈ 10 — that is, the F# port and the Python
    // reference agree to the last bit the calculation can carry, and differ only in floating-point summation
    // order. For scale: the reconstructed elements are O(1) and the previous acceptance band was 2e-3, so
    // these bands are seven to eight orders of magnitude tighter than what they replace.
    // ---------------------------------------------------------------------------------------------------

    /// Acceptance band for the Stage-1 calibration constants (degrees). Pinned per §9 at ~70× the observed
    /// worst deviation of 1.4e-14°; the extra headroom over the bare order-of-magnitude rule absorbs
    /// cross-CPU differences in vectorized summation, which this quantity is directly exposed to.
    let stage1Tol = 1.0e-12

    /// Acceptance band for the Stage-3 reconstructed matrix elements and the §8 cascade metrics. Pinned per
    /// §9 at ~20× the observed worst deviation of 4.9e-11.
    let dataMatrixTol = 1.0e-9

    /// Load one measured data set through the REAL archive-backed store, failing the test on any typed error.
    ///
    /// Spec 0044 §8.4: the measured data is COMMITTED to the repository (`Berreman/Data/MuellerMatrix/data.zip`,
    /// copied next to this assembly by `BerremanTests.fsproj`), so a load failure is a DEFECT, not an
    /// environmental condition. The previous behaviour — walk up the directory tree looking for a sibling
    /// external checkout and `Assert.Skip` when it was absent — is gone, together with every reference to
    /// that checkout. Note what this helper does NOT mention: no path, no archive, no entry name. It names
    /// a `MuellerDataSet` and the store resolves everything else (§8.3 / R5).
    let loadDataSet (data : MuellerDataProxy) (dataSet : MuellerDataSet) : MuellerRawRow list =
        match data.tryLoadDataSet dataSet with
        | Ok rows -> rows
        | Error e -> failwith $"could not load the measured data set %A{dataSet} from the committed archive: %A{e}"

    /// Reduce an experiment family's raw capture rows to its AIR#0 normalized trace — the reference
    /// `load_air_traces` + `prepare_normalized_traces` (matrix_step1_air_fit.py:78, :85): keep only the AIR#0
    /// rows, group by experiment; within each experiment average `avgTotal` per dial `description`, dark-subtract
    /// (`signal = avg − darkMean`), and normalize by that experiment's own mean signal; concatenate all
    /// experiments' points. Returns the `(dialAngleDeg[], norm[])` pair `freeCosineFit` consumes.
    let reduceAirTraces (darkMean : float) (rows : MuellerRawRow list) : float[] * float[] =
        let reduced =
            rows
            |> List.filter (fun r -> r.experiment.Contains("(AIR#0)"))
            |> List.groupBy (fun r -> r.experiment)
            |> List.collect (fun (_, expRows) ->
                let byDescription =
                    expRows
                    |> List.groupBy (fun r -> r.description.degrees)
                    |> List.map (fun (descDeg, g) -> descDeg, (g |> List.averageBy (fun r -> r.avgTotal)) - darkMean)
                    |> List.sortBy fst
                let meanSignal = byDescription |> List.averageBy snd
                byDescription |> List.map (fun (descDeg, signal) -> descDeg, signal / meanSignal))
        reduced |> List.map fst |> List.toArray,
        reduced |> List.map snd |> List.toArray

    /// The Stage-1 LP-frame fit coefficients `(qLp, uLp)` — the reference `fit_lp_frame_coeffs`
    /// (matrix_step1_air_fit.py:132): the same 2-column least squares as `freeCosineFit` but with the design
    /// phased on the source LP zero (`[cos 2(α − zeroDeg), sin 2(α − zeroDeg)]`), returning the raw coefficients
    /// (not a zero/visibility readout) that feed `sourceCplFromLpFrame`. Solved through the real proxy exactly
    /// as `freeCosineFit` does; a well-formed AIR trace is full column rank 2, so `Ok` is expected.
    let lpFrameCoeffs (solver : MuellerSolverProxy) (anglesDeg : float[]) (norm : float[]) (zeroDeg : float) : float * float =
        let design =
            anglesDeg
            |> Array.map (fun a ->
                let twoAlpha = 2.0 * (a - zeroDeg) * degree
                [| cos twoAlpha; sin twoAlpha |])
        let rhs = norm |> Array.map (fun n -> n - 1.0)
        match solver.solveLinearLeastSquares design rhs with
        | Ok sol -> sol.solution.[0], sol.solution.[1]
        | Error e -> failwith $"the LP-frame calibration least squares failed: %A{e}"

    /// Unwrap a `freeCosineFit` result in the data-dependent cross-check, failing the test (never a silent
    /// pass) if a well-formed AIR trace somehow yields a `SolverError`.
    let unwrapFit (label : string) (result : Result<CosineFit, SolverError>) : CosineFit =
        match result with
        | Ok fit -> fit
        | Error e -> failwith $"the {label} calibration cosine fit failed: %A{e}"

    [<Fact>]
    member _.``zero retardance is the identity Mueller matrix (arbitrary azimuth)`` () =
        // A wave plate with no phase delay is a pass-through, whatever its azimuth: R(-θ)·I·R(θ) = I.
        let actual = retarderMueller (Angle.degree 37.0) (Retardance.degree 0.0)
        assertMuellerEqual Propagation.identityMueller actual

    [<Fact>]
    member _.``zero retardance at zero azimuth is the identity Mueller matrix`` () =
        let actual = retarderMueller Angle.zero (Retardance.degree 0.0)
        assertMuellerEqual Propagation.identityMueller actual

    [<Fact>]
    member _.``quarter-wave retarder at azimuth 0 is the standard QWP Mueller form`` () =
        // Standard QWP, fast axis horizontal: [[1,0,0,0],[0,1,0,0],[0,0,0,1],[0,0,-1,0]].
        let expected =
            Propagation.muellerOfRows
                [ [ 1.0; 0.0; 0.0; 0.0 ]
                  [ 0.0; 1.0; 0.0; 0.0 ]
                  [ 0.0; 0.0; 0.0; 1.0 ]
                  [ 0.0; 0.0; -1.0; 0.0 ] ]
        let actual = retarderMueller Angle.zero (Retardance.degree 90.0)
        assertMuellerEqual expected actual

    [<Fact>]
    member _.``quarter-wave retarder at azimuth 45 is the standard rotated QWP Mueller form`` () =
        // Standard QWP, fast axis at 45°: [[1,0,0,0],[0,0,0,-1],[0,0,1,0],[0,1,0,0]] — exercises rotateMueller.
        let expected =
            Propagation.muellerOfRows
                [ [ 1.0; 0.0; 0.0; 0.0 ]
                  [ 0.0; 0.0; 0.0; -1.0 ]
                  [ 0.0; 0.0; 1.0; 0.0 ]
                  [ 0.0; 1.0; 0.0; 0.0 ] ]
        let actual = retarderMueller (Angle.degree 45.0) (Retardance.degree 90.0)
        assertMuellerEqual expected actual

    [<Fact>]
    member _.``Retardance degrees and value round-trip`` () =
        let r = Retardance.degree 90.0
        Assert.True(abs (r.degrees - 90.0) < allowedDiff, $"degrees = {r.degrees}")
        Assert.True(abs (r.value - (System.Math.PI / 2.0)) < allowedDiff, $"value = {r.value}")

    [<Fact>]
    member _.``vecColumnMajor round-trips through muellerOfVecColumnMajor`` () =
        // muellerOfVecColumnMajor (vecColumnMajor M) = M for every 4×4 (the vec round-trip acceptance).
        let actual = muellerOfVecColumnMajor (vecColumnMajor asymmetricM)
        assertMuellerEqual asymmetricM actual

    [<Fact>]
    member _.``kron4 dotted with the column-major vec is a dot (M times s) (transpose guard)`` () =
        // The design row aᵀ⊗sᵀ dotted with vec_F(M) MUST equal a·(M·s); the non-symmetric M makes this fail
        // under a row-major flatten, so the identity pins the column-major (order F) convention.
        let s = StokesVector.create [ 1.0; 0.3; -0.7; 0.5 ]
        let a = RealVector4.create [ 0.2; -1.1; 0.4; 0.9 ]
        let lhs = Array.map2 (*) (kron4 s a) (vecColumnMajor asymmetricM) |> Array.sum
        let (StokesVector ms) = asymmetricM * s
        let rhs = [ 0 .. 3 ] |> List.sumBy (fun j -> a.[j] * ms.[j])
        Assert.True(abs (lhs - rhs) < allowedDiff, $"lhs = {lhs}, rhs = {rhs}, diff = {abs (lhs - rhs)}")

    [<Fact>]
    member _.``frobeniusDiff of a matrix with itself is zero`` () =
        let d = frobeniusDiff asymmetricM asymmetricM
        Assert.True(d.frobenius < allowedDiff, $"frobenius = {d.frobenius}")
        Assert.True(d.maxAbs < allowedDiff, $"maxAbs = {d.maxAbs}")
        Assert.True(d.meanAbs < allowedDiff, $"meanAbs = {d.meanAbs}")

    [<Fact>]
    member _.``frobeniusDiff localizes the largest element difference and its index`` () =
        // Perturb the single element at (2,1) from 1.0 to 3.0 (a diff of 2.0); the readout must localize it.
        let perturbed =
            Propagation.muellerOfRows
                [ [ 1.0; 0.2; -0.3; 0.4 ]
                  [ 0.5; 0.6; 0.7; -0.8 ]
                  [ -0.9; 3.0; 0.1; 0.2 ]
                  [ 0.3; -0.4; 0.5; 0.6 ] ]
        let d = frobeniusDiff asymmetricM perturbed
        let (mi, mj) = d.argMax
        Assert.Equal(2, mi)
        Assert.Equal(1, mj)
        Assert.True(abs (d.maxAbs - 2.0) < allowedDiff, $"maxAbs = {d.maxAbs}")
        Assert.True(abs (d.frobenius - 2.0) < allowedDiff, $"frobenius = {d.frobenius}")
        Assert.True(abs (d.meanAbs - (2.0 / 16.0)) < allowedDiff, $"meanAbs = {d.meanAbs}")

    [<Fact>]
    member _.``a mock MuellerSolverProxy solves through its exact signature, returning a canned solution and typed SolverErrors`` () =
        // Spec 0042 (003) acceptance: build a stub MuellerSolverProxy and call solveLinearLeastSquares
        // through its EXACT signature, returning a canned solution AND a typed SolverError.
        let proxy = makeMockSolver ()

        // Pin the EXACT signature the acceptance names by binding the field to an explicitly-typed local:
        // the compiler rejects the file if `solveLinearLeastSquares` drifts from its declared shape.
        let solve : float[][] -> float[] -> Result<LeastSquaresSolution, SolverError> = proxy.solveLinearLeastSquares

        // A full-height design (16 rows for the 16 Mueller unknowns) returns the canned solution.
        let fullDesign : float[][] = Array.init 16 (fun r -> Array.init 16 (fun c -> if r = c then 1.0 else 0.0))
        let rhs : float[] = Array.init 16 (fun r -> 0.1 * float r)
        match solve fullDesign rhs with
        | Ok sol ->
            Assert.Equal(16, sol.solution.Length)
            for k in 0 .. 15 do
                Assert.True(abs (sol.solution.[k] - 0.1 * float k) < allowedDiff, $"solution[{k}] = {sol.solution.[k]}")
            Assert.Equal(16, sol.rank)
            Assert.True(abs sol.rmse < allowedDiff, $"rmse = {sol.rmse}")
        | Error e -> Assert.Fail($"expected the canned LeastSquaresSolution, got %A{e}")

        // An under-determined design (fewer rows than the 16 unknowns) returns a typed RankDeficient — never a throw.
        match solve [| [| 1.0; 0.0 |]; [| 0.0; 1.0 |] |] rhs with
        | Error (RankDeficient rank) -> Assert.Equal(2, rank)
        | other -> Assert.Fail($"expected Error (RankDeficient _), got %A{other}")

        // An empty design returns a typed SingularDesign carrying a non-empty reason — never a throw.
        match solve [||] rhs with
        | Error (SingularDesign reason) -> Assert.False(System.String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (SingularDesign _), got %A{other}")

    [<Fact>]
    member _.``a MuellerSolverProxy compares by reference (the ReferenceEquality convention)`` () =
        // The proxy's only field is function-valued, so it has no structural equality; the
        // [<ReferenceEquality>] attribute makes it compare by identity, so a host context holding one
        // stays comparable (mirrors the ExperimentDataProxy convention).
        let p = makeMockSolver ()
        let same = p
        Assert.True((p = same))
        Assert.False((p = makeMockSolver ()))

    [<Fact>]
    member _.``the real createMathNetSvd proxy solves a known over-determined full-rank system`` () =
        // Spec 0042 (004) acceptance: instantiate the REAL createMathNetSvd () (not the mock) and solve a
        // small known over-determined, full-column-rank, consistent system A·x = b. The returned solution
        // MUST match the known x within tolerance, with rank equal to the column count and a ~zero residual.
        let proxy = createMathNetSvd ()

        // A is 4×2 with independent columns (full column rank 2); x = [3; -2]; b = A·x (a consistent system).
        let design : float[][] =
            [| [| 1.0;  0.0 |]
               [| 0.0;  1.0 |]
               [| 1.0;  1.0 |]
               [| 2.0; -1.0 |] |]
        let known : float[] = [| 3.0; -2.0 |]
        let rhs : float[] = [| 3.0; -2.0; 1.0; 8.0 |]

        match proxy.solveLinearLeastSquares design rhs with
        | Ok sol ->
            Assert.Equal(2, sol.solution.Length)
            for k in 0 .. 1 do
                Assert.True(abs (sol.solution.[k] - known.[k]) < allowedDiff, $"solution[{k}] = {sol.solution.[k]}, expected {known.[k]}")
            // Rank equals the column count on a full-rank solve — the observability check the acceptance names.
            Assert.Equal(2, sol.rank)
            Assert.True(sol.rmse < allowedDiff, $"rmse = {sol.rmse}")
        | Error e -> Assert.Fail($"expected the known least-squares solution, got %A{e}")

    [<Fact>]
    member _.``the real createMathNetSvd proxy maps degenerate designs to typed SolverErrors (never a throw)`` () =
        // The real proxy honours the TOTAL seam: a singular / rank-deficient design returns a typed value, not
        // an exception across the proxy boundary.
        let proxy = createMathNetSvd ()

        // An empty design → SingularDesign carrying a non-empty reason.
        match proxy.solveLinearLeastSquares [||] [||] with
        | Error (SingularDesign reason) -> Assert.False(System.String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (SingularDesign _), got %A{other}")

        // A rank-deficient design (column 2 = 2× column 1, so the numerical rank is 1 < the 2 columns).
        let dependent : float[][] =
            [| [| 1.0; 2.0 |]
               [| 2.0; 4.0 |]
               [| 3.0; 6.0 |] |]
        match proxy.solveLinearLeastSquares dependent [| 1.0; 2.0; 3.0 |] with
        | Error (RankDeficient rank) -> Assert.True(rank < 2, $"rank = {rank}")
        | other -> Assert.Fail($"expected Error (RankDeficient _), got %A{other}")

    [<Fact>]
    member _.``a mock MuellerDataProxy loads canned capture rows through its exact tryLoadDataSet signature`` () =
        // Spec 0042 (005) acceptance, re-keyed by spec 0044 §8.3: build a stub MuellerDataProxy and load
        // canned rows through the EXACT tryLoadDataSet signature.
        let proxy = seededDataMock ()

        // Pin the EXACT signature the acceptance names by binding the field to an explicitly-typed local:
        // the compiler rejects the file if `tryLoadDataSet` drifts from its declared shape. That the
        // annotation reads `MuellerDataSet -> ...` and NOT `DataFilePath -> ...` is itself the assertion
        // that the seam is keyed by identity rather than by location.
        let loadDataSet : MuellerDataSet -> Result<MuellerRawRow list, MuellerDataError> = proxy.tryLoadDataSet

        match loadDataSet LpLpFamily with
        | Ok rows ->
            Assert.Equal(2, rows.Length)
            Assert.Equal<MuellerRawRow list>(cannedRows, rows)
        | Error e -> Assert.Fail($"expected the canned capture rows, got %A{e}")

    [<Fact>]
    member _.``an unseeded data set yields a typed MuellerDataError from tryLoadDataSet, never a throw`` () =
        // Spec 0042 (005) acceptance, re-keyed by spec 0044 §8.3: a data set the mock does not carry returns
        // a typed MuellerDataError — never a throw. `CplCplDay2` is a valid data set the mock was simply not
        // seeded with, so this exercises the miss path without any invalid input existing at all: with the
        // seam keyed by a DU there is no such thing as an unparseable key.
        let proxy = seededDataMock ()
        match proxy.tryLoadDataSet CplCplDay2 with
        | Error (EmptyFile reason) -> Assert.False(System.String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (EmptyFile _) for an unseeded data set, got %A{other}")

    [<Fact>]
    member _.``a MuellerDataProxy compares by reference (the ReferenceEquality convention)`` () =
        // The proxy's only field is function-valued, so it has no structural equality; the
        // [<ReferenceEquality>] attribute makes it compare by identity, so a host context holding one stays
        // comparable (mirrors the ExperimentDataProxy / MuellerSolverProxy convention).
        let p = seededDataMock ()
        let same = p
        Assert.True((p = same))
        Assert.False((p = seededDataMock ()))

    [<Fact>]
    member _.``the real parseMuellerCsv parses a committed multi-column CSV fixture into MuellerRawRows (description elevated to Angle)`` () =
        // Spec 0042 (006) acceptance: parse a committed multi-column, BOM-prefixed CSV fixture through the REAL
        // MuellerDataStore.parseMuellerCsv (not a mock), yielding MuellerRawRow values whose `description` is
        // elevated to the engine Angle. Reading the five required columns BY NAME ignores the unread columns.
        match MuellerDataStore.parseMuellerCsv muellerCsvFixture with
        | Ok rows ->
            Assert.Equal(2, rows.Length)
            let r0 = rows.[0]
            Assert.Equal("CPL-(AIR#0)-CPL", r0.experiment)
            Assert.Equal(0, r0.captureIndex)
            Assert.Equal(System.DateTimeOffset(2026, 7, 15, 12, 0, 0, System.TimeSpan.Zero), r0.capturedAt)
            // `description` elevated to Angle: read back its `degrees` view (the raw analyzer dial angle).
            Assert.True(abs r0.description.degrees < allowedDiff, $"description = {r0.description.degrees}")
            Assert.True(abs (r0.avgTotal - 869.5) < allowedDiff, $"avgTotal = {r0.avgTotal}")
            let r1 = rows.[1]
            Assert.Equal(1, r1.captureIndex)
            Assert.True(abs (r1.description.degrees - 45.0) < allowedDiff, $"description = {r1.description.degrees}")
            Assert.True(abs (r1.avgTotal - 912.25) < allowedDiff, $"avgTotal = {r1.avgTotal}")
        | Error e -> Assert.Fail($"expected the parsed capture rows, got %A{e}")

    [<Fact>]
    member _.``the real parseMuellerCsv maps a malformed file to a typed MuellerDataError, never a throw`` () =
        // Spec 0042 (006) acceptance: a malformed file (capture_index is not an integer) yields a typed
        // MuellerDataError — no exception crosses into the pure Domain.
        match MuellerDataStore.parseMuellerCsv malformedMuellerCsv with
        | Error (MalformedRow reason) -> Assert.False(System.String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (MalformedRow _) for a malformed file, got %A{other}")

    [<Fact>]
    member _.``createArchiveBacked produces a MuellerDataProxy that loads every committed data set`` () =
        // Spec 0044 (§8.3 / §8.4) acceptance: the REAL archive-backed store resolves the committed archive by
        // itself and loads all seven data sets. Note what this test does not contain — no path, no archive
        // name, no entry name, no environment probing. It calls a zero-argument factory and names data sets.
        // That is the whole contract of the 0044 re-key (R5).
        //
        // The expected row counts are the archive's documented inventory (Berreman/Data/MuellerMatrix/README.md,
        // matching spec 0042 §6): asserting them here means a truncated or wrong-build archive fails loudly at
        // the load, rather than silently producing a slightly-wrong reconstruction downstream.
        let proxy = MuellerDataStore.createArchiveBacked ()

        // Pin the EXACT signature by binding the field to an explicitly-typed local: the compiler rejects the
        // file if `tryLoadDataSet` drifts from its declared shape.
        let load : MuellerDataSet -> Result<MuellerRawRow list, MuellerDataError> = proxy.tryLoadDataSet

        let expectedRowCounts =
            [ LpLpFamily, 209
              LpCplFamily, 209
              CplLpFamily, 209
              CplCplDay1, 170
              CplCplDay2, 57
              DarknessChecks, 6
              BullshitChecks, 2 ]

        for (dataSet, expected) in expectedRowCounts do
            match load dataSet with
            | Ok rows -> Assert.Equal(expected, rows.Length)
            | Error e -> Assert.Fail($"the committed archive must carry %A{dataSet}, got %A{e}")

    [<Fact>]
    member _.``tryLoadFromArchive maps an absent archive to a typed ArchiveUnreadable, never a throw`` () =
        // Spec 0044 (§8.3) acceptance: the storage boundary is TOTAL. A missing container is caught at the
        // boundary and mapped to the typed ArchiveUnreadable case carrying both the location and the
        // underlying reason — no exception crosses into the pure Domain.
        //
        // This is the one place a location is named at all, and deliberately so: the failure paths must stay
        // reachable from a test without any production caller ever knowing where the data lives, which is
        // exactly why `tryLoadFromArchive` is public while `createArchiveBacked ()` takes no argument.
        let absent = MuellerDataStore.MuellerArchivePath "C:/data/definitely-missing-mueller-archive-0044.zip"
        match MuellerDataStore.tryLoadFromArchive absent LpLpFamily with
        | Error (ArchiveUnreadable (archive, reason)) ->
            Assert.Equal("C:/data/definitely-missing-mueller-archive-0044.zip", archive)
            Assert.False(System.String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (ArchiveUnreadable _) for a missing archive, got %A{other}")

    [<Fact>]
    member _.``tryLoadFromArchive maps an archive without the requested entry to a typed DataSetMissing, never a throw`` () =
        // Spec 0044 (§8.3) acceptance: the second archive failure mode is reported DISTINCTLY from the first,
        // because the two call for different fixes — an unreadable container is a deployment problem (the
        // Content copy did not happen), whereas a container that opens but lacks the entry is a data problem
        // (the wrong archive was shipped). A caller that cannot tell them apart cannot act on the log line.
        //
        // A valid but empty archive is written to a temp file so the entry lookup genuinely misses on a
        // genuinely-openable container; it is deleted again whatever happens.
        let tempArchive = Path.Combine(Path.GetTempPath(), $"mueller-empty-archive-{System.Guid.NewGuid():N}.zip")
        try
            use (stream : FileStream) = File.Create tempArchive
            use (zip : ZipArchive) = new ZipArchive(stream, ZipArchiveMode.Create)
            zip.CreateEntry("unrelated.txt") |> ignore
            zip.Dispose()
            stream.Dispose()

            match MuellerDataStore.tryLoadFromArchive (MuellerDataStore.MuellerArchivePath tempArchive) CplCplDay1 with
            | Error (DataSetMissing (dataSet, archive)) ->
                Assert.Equal(CplCplDay1, dataSet)
                Assert.Equal(tempArchive, archive)
            | other -> Assert.Fail($"expected Error (DataSetMissing _) for an archive without the entry, got %A{other}")
        finally
            if File.Exists tempArchive then File.Delete tempArchive

    [<Fact>]
    member _.``parseExperiment decodes CPL-(QZ#90-LR#90)-CPL and the +90 / -90 object rotation is inert (R(2 phi) invariance)`` () =
        // Spec 0042 (007) acceptance. The label decodes to the CPL-CPL family, the QZ·LR product kind, and an
        // object frame rotation phi = 90 deg.
        let parsed = parseExperiment "CPL-(QZ#90-LR#90)-CPL"
        Assert.Equal(CplCpl, family parsed)
        Assert.Equal(QzLrProduct, matrixKind parsed)
        Assert.True(abs ((objectPhi parsed).degrees - 90.0) < allowedDiff, $"phi = {(objectPhi parsed).degrees}")

        // R(phi) depends on 2*phi, so +90 deg and -90 deg differ by a full 180 deg in 2*phi and are physically
        // identical: the effective source Stokes state and the effective analyzer row built at +90 MUST equal
        // those at -90 within allowedDiff. Non-symmetric base states (the 2*phi=+/-180 flip is observable).
        let sBase = StokesVector.create [ 1.0; 0.7; -0.4; 0.2 ]
        let aBase = RealVector4.create [ 0.5; 0.25; -0.3; 0.1 ]
        assertStokesClose (effectiveSource (Angle.degree 90.0) sBase) (effectiveSource (Angle.degree (-90.0)) sBase)
        assertVec4Close (effectiveAnalyzer (Angle.degree 90.0) aBase) (effectiveAnalyzer (Angle.degree (-90.0)) aBase)

    [<Fact>]
    member _.``freeCosineFit recovers a synthetic cosine zero and visibility within tolerance (always-run)`` () =
        // Spec 0042 (008) acceptance (always-run): a synthetic AIR trace norm = 1 + v·cos2(α − z) is fit through
        // the REAL solver proxy; freeCosineFit MUST recover the known zero z, the known visibility v, and a
        // (near-)zero normalized residual — the closed-form ½·atan2(q,p) / √(p²+q²) readouts of the 2-column fit.
        let solver = createMathNetSvd ()
        let knownZeroDeg = 30.0
        let knownVisibility = 0.6
        let anglesDeg = [| for k in 0 .. 17 -> 10.0 * float k |]     // 0,10,…,170 — >2 distinct doubled angles
        let norm = anglesDeg |> Array.map (fun a -> 1.0 + knownVisibility * cos (2.0 * (a - knownZeroDeg) * degree))
        match freeCosineFit solver anglesDeg norm with
        | Ok fit ->
            Assert.True(abs (fit.zeroDeg - knownZeroDeg) < 1e-6, $"zeroDeg = {fit.zeroDeg}")
            Assert.True(abs (fit.visibility - knownVisibility) < 1e-6, $"visibility = {fit.visibility}")
            Assert.True(fit.rmse < 1e-6, $"rmse = {fit.rmse}")
        | Error e -> Assert.Fail($"expected a CosineFit for the synthetic trace, got %A{e}")

    [<Fact>]
    member _.``Stage-1 re-derived calibration constants match the reference pipeline's full-precision values`` () =
        // Spec 0042 (008) acceptance, upgraded by spec 0044 §8.5: the constants RE-derived by the three
        // Stage-1 functions from the committed measured data are compared against the REFERENCE PIPELINE's
        // own output at full precision, not against the report's 2-decimal rounded values.
        //
        // Provenance of the expected values: Berreman/Data/MuellerMatrix/reference_step1_summary.json, which
        // is the Python reference implementation's own emitted summary (see that folder's README.md for the
        // source repository, branch and commit). Transcribing the literals here rather than parsing the JSON
        // at run time is deliberate — the test then reads NO file for its expectations, which is what keeps
        // it free of any location knowledge (§8.5).
        //
        // This fact no longer skips. The data is committed to the repository, so a load failure is a defect
        // (§8.4 / R6) and `loadDataSet` fails the test with the typed error.
        let solver = createMathNetSvd ()
        let data = MuellerDataStore.createArchiveBacked ()
        let load = loadDataSet data

        // dark_mean = mean avg_total over the darkness checks.
        let darkMean = load DarknessChecks |> List.averageBy (fun r -> r.avgTotal)

        // LP-LP AIR → free cosine fit → the LP analyzer's zero dial z_LP.
        let (anglesLpLp, normLpLp) = reduceAirTraces darkMean (load LpLpFamily)
        let lpLpFit = unwrapFit "LP-LP" (freeCosineFit solver anglesLpLp normLpLp)

        // LP-CPL AIR → free cosine fit → the CPL analyzer's zero dial z_CPL; δ_an = arccos(visibility).
        let (anglesLpCpl, normLpCpl) = reduceAirTraces darkMean (load LpCplFamily)
        let lpCplFit = unwrapFit "LP-CPL" (freeCosineFit solver anglesLpCpl normLpCpl)
        let deltaAnDeg = (retardanceFromVisibility lpCplFit.visibility).degrees

        // CPL-LP AIR, fit in the source LP frame (phased on z_LP) → the closed-form source model.
        let (anglesCplLp, normCplLp) = reduceAirTraces darkMean (load CplLpFamily)
        let (qLp, uLp) = lpFrameCoeffs solver anglesCplLp normCplLp lpLpFit.zeroDeg
        let source = sourceCplFromLpFrame qLp uLp

        // Every constant is compared in ONE assertion reporting all six deviations, so a regression message
        // shows which constants moved and by how much rather than stopping at the first one out of band.
        let constants =
            [ "dark_mean", darkMean, 869.666
              "z_LP", lpLpFit.zeroDeg, 155.44011830109392
              "z_CPL", lpCplFit.zeroDeg, 96.99613336321012
              "δ_an", deltaAnDeg, 83.66804489441891
              "θ_src", source.thetaRel.degrees, 39.31100224361002
              "δ_src", source.retardance.degrees, 82.55344158462403 ]
        let worst = constants |> List.map (fun (_, actual, expected) -> abs (actual - expected)) |> List.max
        Assert.True(
            worst < stage1Tol,
            $"""Stage-1 constants (tol {stage1Tol}): {String.concat "; " [ for (n, a, e) in constants -> $"{n} {a} vs {e} (Δ {abs (a - e)})" ]}""")

    [<Fact>]
    member _.``Stage-3 signal reduction pins darkSubtract, scalarGain, airIdentityPred, correctSignal and every per-family familyGain rule (always-run)`` () =
        // Spec 0042 (009) acceptance (always-run, deterministic — fixed-literal timestamps, no ambient clock):
        // pin the Stage-3 reduction primitives on known values, and familyGain firing each per-family rule.

        // scalarGain — a proportional pair signal = 2·model has best scalar gain exactly 2 …
        let model = [| 1.0; 2.0; 3.0; 4.0 |]
        let signalProp = model |> Array.map (fun m -> 2.0 * m)
        Assert.True(abs (scalarGain signalProp model - 2.0) < allowedDiff, $"scalarGain (proportional) = {scalarGain signalProp model}")
        // … and a general pair matches the closed form Σ(s·m)/Σ(m²).
        let signalGen = [| 3.0; 1.0; 4.0; 1.0 |]
        let expectedGain = (Array.map2 (*) signalGen model |> Array.sum) / (Array.map2 (*) model model |> Array.sum)
        Assert.True(abs (scalarGain signalGen model - expectedGain) < allowedDiff, $"scalarGain (general) = {scalarGain signalGen model}, expected {expectedGain}")

        // darkSubtract — avg_total − dark_mean, with the ≈ 869.666 measured dark mean passed in as data.
        let darkMean = DarkMean 869.666
        Assert.True(abs (darkSubtract darkMean 1000.0 - (1000.0 - 869.666)) < allowedDiff, $"darkSubtract = {darkSubtract darkMean 1000.0}")

        // correctSignal — (avg_total − dark_mean) / gain (dark-subtract composed with the gain divide).
        Assert.True(abs (correctSignal darkMean 1000.0 2.0 - ((1000.0 - 869.666) / 2.0)) < allowedDiff, $"correctSignal = {correctSignal darkMean 1000.0 2.0}")

        // airIdentityPred — a·s, the effective-analyzer row dotted with the effective source Stokes state.
        let s = StokesVector.create [ 1.0; 0.5; -0.3; 0.2 ]
        let a = RealVector4.create [ 2.0; -1.0; 0.4; 0.8 ]
        let expectedDot = 2.0 * 1.0 + (-1.0) * 0.5 + 0.4 * (-0.3) + 0.8 * 0.2
        Assert.True(abs (airIdentityPred s a - expectedDot) < allowedDiff, $"airIdentityPred = {airIdentityPred s a}, expected {expectedDot}")

        // familyGain — one calibration bundle, then assert each per-family rule fires (fixed-literal times).
        let t0 = System.DateTimeOffset(2026, 7, 15, 12, 0, 0, System.TimeSpan.Zero)
        let cal =
            {
                lpLp = { gainA = 10.0; gainB = 20.0 }                            // mean → 15.0
                lpCpl = { gainA = 4.0; gainB = 6.0 }                             // mean → 5.0
                cplLp = { gainPre = 100.0; gainPost = 200.0; splitTime = t0 }    // split at t0
                cplCpl = Day2Single 42.0                                         // single AIR block → 42.0
            }

        // LP-LP / LP-CPL — the mean of the two AIR repeats (time-independent).
        Assert.True(abs (familyGain LpLp cal t0 - 15.0) < allowedDiff, $"familyGain LpLp = {familyGain LpLp cal t0}")
        Assert.True(abs (familyGain LpCpl cal t0 - 5.0) < allowedDiff, $"familyGain LpCpl = {familyGain LpCpl cal t0}")

        // CPL-LP — split at the BullshitCheck timestamp: strictly before → pre, at/after → post.
        let before = t0.AddMinutes(-5.0)
        let after = t0.AddMinutes(5.0)
        Assert.True(abs (familyGain CplLp cal before - 100.0) < allowedDiff, $"familyGain CplLp (before) = {familyGain CplLp cal before}")
        Assert.True(abs (familyGain CplLp cal after - 200.0) < allowedDiff, $"familyGain CplLp (after) = {familyGain CplLp cal after}")
        Assert.True(abs (familyGain CplLp cal t0 - 200.0) < allowedDiff, $"familyGain CplLp (at split) = {familyGain CplLp cal t0}")

        // CPL-CPL day2 — the single AIR block's gain.
        Assert.True(abs (familyGain CplCpl cal t0 - 42.0) < allowedDiff, $"familyGain CplCpl (day2) = {familyGain CplCpl cal t0}")

        // CPL-CPL day1 — time-interpolated between the two AIR blocks (10:00 → 14:00, gains 30 → 50).
        let tStart = System.DateTimeOffset(2026, 7, 15, 10, 0, 0, System.TimeSpan.Zero)
        let tEnd = System.DateTimeOffset(2026, 7, 15, 14, 0, 0, System.TimeSpan.Zero)
        let calDay1 = { cal with cplCpl = Day1Interp (30.0, tStart, 50.0, tEnd) }
        Assert.True(abs (familyGain CplCpl calDay1 t0 - 40.0) < allowedDiff, $"familyGain CplCpl (day1 midpoint) = {familyGain CplCpl calDay1 t0}")   // 12:00 → 0.5·30 + 0.5·50
        Assert.True(abs (familyGain CplCpl calDay1 tStart - 30.0) < allowedDiff, $"familyGain CplCpl (day1 start) = {familyGain CplCpl calDay1 tStart}")
        Assert.True(abs (familyGain CplCpl calDay1 tEnd - 50.0) < allowedDiff, $"familyGain CplCpl (day1 end) = {familyGain CplCpl calDay1 tEnd}")
        Assert.True(abs (familyGain CplCpl calDay1 (tStart.AddHours(-1.0)) - 30.0) < allowedDiff, $"familyGain CplCpl (day1 clamp below) = {familyGain CplCpl calDay1 (tStart.AddHours(-1.0))}")
        Assert.True(abs (familyGain CplCpl calDay1 (tEnd.AddHours(1.0)) - 50.0) < allowedDiff, $"familyGain CplCpl (day1 clamp above) = {familyGain CplCpl calDay1 (tEnd.AddHours(1.0))}")

    [<Fact>]
    member _.``reconstruct recovers a seeded MuellerMatrix from a synthetic full-rank design at rank 16 (always-run)`` () =
        // Spec 0042 (010) acceptance (always-run): a synthetic FULL-RANK design — 4 independent Stokes states ⊗
        // 4 independent analyzer rows = 16 linearly independent designRows (column rank 16) — with the target
        // seeded exactly as y = X·vec_F(M_seed). The unique least-squares solution un-vecs (column-major) back to
        // the seeded MuellerMatrix, so `reconstruct` MUST recover the seed within tolerance at rank 16. The seed
        // is ASYMMETRIC with distinct entries so a row/column-major transpose bug is observable.
        let seed =
            Propagation.muellerOfRows
                [ [ 1.0;  2.0;  3.0;  4.0 ]
                  [ 5.0;  6.0;  7.0;  8.0 ]
                  [ 9.0; 10.0; 11.0; 12.0 ]
                  [ 13.0; 14.0; 15.0; 16.0 ] ]
        let vSeed = vecColumnMajor seed
        // Four independent Stokes states and four independent analyzer rows; every (s, a) pair is one designRow,
        // so the 16 rows span the tensor space (rank = rank{s}·rank{a} = 4·4 = 16).
        let stokesStates =
            [ StokesVector.create [ 1.0; 1.0; 0.0; 0.0 ]
              StokesVector.create [ 1.0; -1.0; 0.0; 0.0 ]
              StokesVector.create [ 1.0; 0.0; 1.0; 0.0 ]
              StokesVector.create [ 1.0; 0.0; 0.0; 1.0 ] ]
        let analyzerRows =
            [ RealVector4.create [ 1.0; 1.0; 0.0; 0.0 ]
              RealVector4.create [ 1.0; -1.0; 0.0; 0.0 ]
              RealVector4.create [ 1.0; 0.0; 1.0; 0.0 ]
              RealVector4.create [ 1.0; 0.0; 0.0; 1.0 ] ]
        let design =
            [ for s in stokesStates do
                for a in analyzerRows -> designRow s a ]
            |> List.toArray
        // The measurement identity: each row's signal is designRow · vec_F(M_seed) = a·(M_seed·s) (a consistent
        // system, so the full-rank solve recovers the seed exactly).
        let target = design |> Array.map (fun row -> Array.map2 (*) row vSeed |> Array.sum)

        match reconstruct (createMathNetSvd ()) design target with
        | Ok recon ->
            // The observability check §7.2 asserts: full rank 16 and a ~zero residual.
            Assert.Equal(16, recon.rank)
            Assert.True(recon.rmse < allowedDiff, $"rmse = {recon.rmse}")
            assertMuellerEqual seed recon.matrix
        | Error e -> Assert.Fail($"expected a full-rank reconstruction of the seed, got %A{e}")

    [<Fact>]
    member _.``cascadeProduct forms M_LR times M_QZ (beam hits QZ first) via the MuellerMatrix product operator (always-run)`` () =
        // Spec 0042 (010) acceptance (always-run): cascadeProduct mLr mQz MUST equal mLr * mQz — the QZ-first
        // cascade (light meets QZ then LR, so the combined Mueller product is M_LR · M_QZ), reusing the engine's
        // MuellerMatrix `*` operator. Non-commuting operands (rotation vs a general matrix) make the ordering
        // observable — a swapped product would differ.
        let mQz =
            Propagation.muellerOfRows
                [ [ 1.0; 0.0; 0.0; 0.0 ]
                  [ 0.0; 0.6; 0.8; 0.0 ]
                  [ 0.0; -0.8; 0.6; 0.0 ]
                  [ 0.0; 0.0; 0.0; 1.0 ] ]
        let mLr =
            Propagation.muellerOfRows
                [ [ 1.0; 0.2; -0.3; 0.4 ]
                  [ 0.5; 0.6; 0.7; -0.8 ]
                  [ -0.9; 1.0; 0.1; 0.2 ]
                  [ 0.3; -0.4; 0.5; 0.6 ] ]
        assertMuellerEqual (mLr * mQz) (cascadeProduct mLr mQz)

    [<Fact>]
    member _.``buildDesign keeps only the requested kind, dropping AIR rows and the excluded contaminated point (always-run)`` () =
        // Spec 0042 (010): buildDesign selects the science rows of the requested MatrixKind, drops AIR#0
        // calibration rows and the one contaminated point (LP-(LR#90)-CPL-2 / description 140 / capture_index
        // 17 — supplied here as BerremanTests data per spec §0), and assembles one designRow + one corrected
        // signal per kept row via the injected per-row projection.
        let excluded : ExcludedPoint list =
            [ { experiment = "LP-(LR#90)-CPL-2"; description = Angle.degree 140.0; captureIndex = 17 } ]
        // A stub per-row projection: unit effective states (one 16-wide designRow) and the row's avgTotal as the
        // corrected signal, so the assembled target identifies exactly which rows survived, in order.
        let prepareRow (r : MuellerRawRow) : StokesVector * RealVector4 * float =
            StokesVector.create [ 1.0; 0.0; 0.0; 0.0 ],
            RealVector4.create [ 1.0; 0.0; 0.0; 0.0 ],
            r.avgTotal
        let t = System.DateTimeOffset(2026, 7, 15, 12, 0, 0, System.TimeSpan.Zero)
        let rows : MuellerRawRow list =
            [ { experiment = "LP-(LR#90)-LP"; captureIndex = 0; capturedAt = t; description = Angle.degree 10.0; avgTotal = 1.0 }        // LR — kept
              { experiment = "LP-(AIR#0)-LP"; captureIndex = 1; capturedAt = t; description = Angle.degree 20.0; avgTotal = 2.0 }         // AIR — dropped
              { experiment = "LP-(QZ#0)-LP"; captureIndex = 2; capturedAt = t; description = Angle.degree 30.0; avgTotal = 3.0 }          // QZ — wrong kind, dropped
              { experiment = "LP-(LR#90)-CPL-2"; captureIndex = 17; capturedAt = t; description = Angle.degree 140.0; avgTotal = 4.0 }    // contaminated — dropped
              { experiment = "LP-(LR#0)-LP"; captureIndex = 3; capturedAt = t; description = Angle.degree 40.0; avgTotal = 5.0 } ]        // LR — kept
        let (design, target) = buildDesign prepareRow excluded rows Lr
        // Only the two LR science rows survive (avgTotal 1.0 and 5.0), in order; each contributes one 16-wide row.
        Assert.Equal(2, design.Length)
        Assert.Equal(2, target.Length)
        Assert.Equal<float[]>([| 1.0; 5.0 |], target)
        Assert.Equal(16, design.[0].Length)

    [<Fact>]
    member _.``section 7.2 end-to-end: the Stage 1 to 2 to 3 pipeline reconstructs M_QZ, M_LR and M_{QZ+LR} at rank 16 and reproduces the reference pipeline exactly`` () =
        // Spec 0042 (011, WIRE) acceptance — the composition-root cross-check. Wire the REAL
        // createArchiveBacked () (Storage) and createMathNetSvd () (Domain) proxies, load the five science
        // data sets + the darkness and bullshit checks, RE-derive the Stage-1 AIR calibration constants
        // (matrix_step1_air_fit.py), populate the Stage-2 per-family source/analyzer models (matrix_glue.py
        // load_step1_models), reduce each science row to its corrected signal (Stage-3, matrix_fit_linear.py
        // assign_gain_model), and reconstruct M_QZ / M_LR / M_{QZ+LR} (solve_single_object).
        //
        // Spec 0044 changed two things here and nothing else about the pipeline:
        //
        //   §8.4 — the data is COMMITTED, so this fact no longer skips. It previously walked up the directory
        //          tree hunting for an external sibling checkout and skipped when it was absent, which
        //          meant the strongest test in the suite silently did nothing on a clean machine. Absence of
        //          the data is now a defect and `loadDataSet` fails with the typed error.
        //
        //   §8.5 — the expected values are the REFERENCE PIPELINE's own full-precision output rather than the
        //          report's 6-decimal printed matrices, and the band tightened from 2e-3 to `dataMatrixTol`
        //          accordingly. At 2e-3 a refactor could introduce a real numerical error a thousand times
        //          larger than the port's actual reproducibility and nothing would notice.
        //          Provenance: Berreman/Data/MuellerMatrix/reference_step2_linear_summary.json (see that
        //          folder's README.md for the source repository, branch and commit). The literals are
        //          transcribed rather than parsed at run time, so this test reads no file for its
        //          expectations and therefore knows nothing about where anything lives.
        //
        // ---- Composition root: the REAL proxies (the WIRE deliverable). ----
        let solver = createMathNetSvd ()
        let store = MuellerDataStore.createArchiveBacked ()
        let load = loadDataSet store

        let lpLpRows = load LpLpFamily
        let lpCplRows = load LpCplFamily
        let cplLpRows = load CplLpFamily
        let cplCplDay1Rows = load CplCplDay1
        let cplCplDay2Rows = load CplCplDay2

        // ---- Stage-3 procedure constants supplied as DATA (spec section 0): dark mean + the CPL-LP split time. ----
        let darkMeanValue = load DarknessChecks |> List.averageBy (fun r -> r.avgTotal)
        let darkMean = DarkMean darkMeanValue
        // The CPL-LP AIR gain splits at the FIRST BullshitCheck timestamp (matrix_fit_linear.py:82 reads the
        // diagnostics[0] capture; the earliest bullshit-check capture is that same first check).
        let splitTime =
            match load BullshitChecks with
            | [] -> failwith "the bullshit-check data set carried no rows"
            | rows -> (rows |> List.minBy (fun r -> r.capturedAt)).capturedAt

        // ---- Stage 1 (calibration): RE-derive the AIR constants through the real solver (matrix_step1_air_fit.py). ----
        let (anglesLpLp, normLpLp) = reduceAirTraces darkMeanValue lpLpRows
        let lpLpFit = unwrapFit "LP-LP" (freeCosineFit solver anglesLpLp normLpLp)
        let lpZeroDeg = lpLpFit.zeroDeg                                             // LP analyzer zero dial (z_LP)

        let (anglesLpCpl, normLpCpl) = reduceAirTraces darkMeanValue lpCplRows
        let lpCplFit = unwrapFit "LP-CPL" (freeCosineFit solver anglesLpCpl normLpCpl)
        let cplZeroDeg = lpCplFit.zeroDeg                                           // CPL analyzer zero dial (z_CPL)
        let analyzerDeltaMagDeg = (retardanceFromVisibility lpCplFit.visibility).degrees   // |delta_an|

        let (anglesCplLp, normCplLp) = reduceAirTraces darkMeanValue cplLpRows
        let (qLp, uLp) = lpFrameCoeffs solver anglesCplLp normCplLp lpZeroDeg
        let sourceCplModel = sourceCplFromLpFrame qLp uLp                           // { thetaRel; retardance }

        // ---- Stage 2 (glue): the per-family calibrated SOURCE and ANALYZER models (matrix_glue.py:182). ----
        // Source retardance is positive, analyzer retardance negative — only the RELATIVE sign is constrained
        // (matrix_glue.py:211); the analyzer's fixed retarder axis is +45 deg from its LP.
        let sourceLp = LpSource
        let sourceCpl = CplSource (sourceCplModel.thetaRel, sourceCplModel.retardance)
        let analyzerLp : AnalyzerModel =
            {
                kind = Library.IdealLinear
                zeroDial = Angle.degree lpZeroDeg
                dialSign = DialNegative
                thetaRelOpt = None
                retardanceOpt = None
            }
        let analyzerCpl : AnalyzerModel =
            {
                kind = Library.IdealCircularLeft
                zeroDial = Angle.degree cplZeroDeg
                dialSign = DialPositive
                thetaRelOpt = Some (Angle.degree 45.0)
                retardanceOpt = Some (Retardance.degree (- analyzerDeltaMagDeg))
            }

        // ---- Stage 3 (reduction): the per-family AIR scalar gains (matrix_fit_linear.py assign_gain_model). ----
        let airRowsOf (rows : MuellerRawRow list) (experiment : string) : MuellerRawRow list =
            rows |> List.filter (fun r -> r.experiment = experiment)
        // The AIR#0 scalar gain fitting `signal_dark_sub ~ gain * (a_eff . s_eff)` over one AIR experiment's
        // rows (fit_air_gain + fit_scalar_gain). An AIR row carries no object (phi = 0), so its effective
        // states equal its base states, and `airIdentityPred base base` is the per-row identity prediction.
        let fitAirGain (source : SourceModel) (analyzer : AnalyzerModel) (rows : MuellerRawRow list) : float =
            let sBase = sourceBaseStokes source
            let preds =
                rows |> List.map (fun r -> airIdentityPred sBase (analyzerBaseRow analyzer r.description)) |> List.toArray
            let signals = rows |> List.map (fun r -> darkSubtract darkMean r.avgTotal) |> List.toArray
            scalarGain signals preds
        // The mean capture time of an AIR block (fit_air_gain's `air_rows["captured_at"].mean()`), computed
        // relative to the first tick so the tick sum never overflows int64.
        let meanTime (rows : MuellerRawRow list) : System.DateTimeOffset =
            match rows with
            | [] -> failwith "meanTime of an empty AIR block"
            | first :: _ ->
                let t0 = first.capturedAt.UtcTicks
                let meanDelta = rows |> List.averageBy (fun r -> float (r.capturedAt.UtcTicks - t0))
                System.DateTimeOffset(t0 + int64 (System.Math.Round meanDelta), System.TimeSpan.Zero)

        let day1Start = airRowsOf cplCplDay1Rows "CPL-(AIR#0)-CPL"
        let day1End = airRowsOf cplCplDay1Rows "CPL-(AIR#0)-CPL-BR"
        // The per-family AIR gain calibration bundle. LP-LP / LP-CPL take the mean of two AIR repeats; CPL-LP
        // splits at the bullshit-check time; CPL-CPL day 2 is one AIR block (the `cplCpl` default below).
        let calibration : GainCalibration =
            {
                lpLp =
                    {
                        gainA = fitAirGain sourceLp analyzerLp (airRowsOf lpLpRows "LP-(AIR#0)-LP-2")
                        gainB = fitAirGain sourceLp analyzerLp (airRowsOf lpLpRows "LP-(AIR#0)-LP-2R")
                    }
                lpCpl =
                    {
                        gainA = fitAirGain sourceLp analyzerCpl (airRowsOf lpCplRows "LP-(AIR#0)-CPL-2")
                        gainB = fitAirGain sourceLp analyzerCpl (airRowsOf lpCplRows "LP-(AIR#0)-CPL-2R")
                    }
                cplLp =
                    {
                        gainPre = fitAirGain sourceCpl analyzerLp (airRowsOf cplLpRows "CPL-(AIR#0)-LP-2")
                        gainPost = fitAirGain sourceCpl analyzerLp (airRowsOf cplLpRows "CPL-(AIR#0)-LP-2R")
                        splitTime = splitTime
                    }
                cplCpl = Day2Single (fitAirGain sourceCpl analyzerCpl (airRowsOf cplCplDay2Rows "CPL-(AIR#0)-CPL-2"))
            }
        // CPL-CPL day 1 is a distinct FILE with the time-INTERPOLATED rule (AIR start -> AIR end); day 2 keeps
        // the single-block rule in `calibration` above.
        let day1Cal =
            { calibration with
                cplCpl =
                    Day1Interp (
                        fitAirGain sourceCpl analyzerCpl day1Start, meanTime day1Start,
                        fitAirGain sourceCpl analyzerCpl day1End, meanTime day1End) }

        // ---- The per-row Stage-2+3 projection each family closes over its models + gain (enrich_frame). ----
        let prepareRow (source : SourceModel) (analyzer : AnalyzerModel) (fam : Family) (cal : GainCalibration)
                       (r : MuellerRawRow) : StokesVector * RealVector4 * float =
            let phi = objectPhi (parseExperiment r.experiment)
            let sEff = effectiveSource phi (sourceBaseStokes source)
            let aEff = effectiveAnalyzer phi (analyzerBaseRow analyzer r.description)
            let signal = correctSignal darkMean r.avgTotal (familyGain fam cal r.capturedAt)
            sEff, aEff, signal

        // Every science file maps to exactly one family; CPL-CPL day1/day2 are distinct files with distinct
        // gain rules (interpolated vs single block), so each carries its own calibration.
        let families : (MuellerRawRow list * (MuellerRawRow -> StokesVector * RealVector4 * float)) list =
            [ lpLpRows,       prepareRow sourceLp  analyzerLp  LpLp   calibration
              lpCplRows,      prepareRow sourceLp  analyzerCpl LpCpl  calibration
              cplLpRows,      prepareRow sourceCpl analyzerLp  CplLp  calibration
              cplCplDay1Rows, prepareRow sourceCpl analyzerCpl CplCpl day1Cal
              cplCplDay2Rows, prepareRow sourceCpl analyzerCpl CplCpl calibration ]

        // The one contaminated point excluded from every fit (matrix_glue.py EXCLUDED_POINTS[0]), supplied
        // here as BerremanTests data (spec section 0).
        let excluded : ExcludedPoint list =
            [ { experiment = "LP-(LR#90)-CPL-2"; description = Angle.degree 140.0; captureIndex = 17 } ]

        // Assemble the full n*16 design + corrected-signal target for one kind across every family, then solve
        // through the real SVD proxy (buildDesign drops AIR rows and the excluded point per file).
        let reconstructKind (kind : MatrixKind) : ReconstructedMatrix =
            let parts = families |> List.map (fun (rows, prep) -> buildDesign prep excluded rows kind)
            let design = parts |> List.collect (fun (d, _) -> List.ofArray d) |> List.toArray
            let target = parts |> List.collect (fun (_, t) -> List.ofArray t) |> List.toArray
            match reconstruct solver design target with
            | Ok recon -> recon
            | Error e -> failwith $"the %A{kind} reconstruction failed: %A{e}"

        let mQz = reconstructKind Qz
        let mLr = reconstructKind Lr
        let mComb = reconstructKind QzLrProduct

        // ---- Section 7.2 acceptance: each solve is full rank 16, and each reconstructed matrix reproduces
        //      the reference pipeline's own value to `dataMatrixTol` (spec 0044 §8.5). ----
        //
        // The expected matrices below are `qz_matrix`, `lr_matrix` and `combined_matrix` transcribed verbatim
        // from Berreman/Data/MuellerMatrix/reference_step2_linear_summary.json. They are the SAME numbers the
        // report prints in §7.2, carried to full precision instead of six decimals — so this assertion is
        // simultaneously the physics claim (the port reproduces the published result) and a characterization
        // test (nothing in the pipeline drifts numerically without a test failing).
        // Reports the WORST element deviation over the whole matrix rather than failing on the first element
        // past the band. With a characterization test that is the number a reader actually needs: it is the
        // figure the §9 protocol pins the band from, and on a regression it says how far the pipeline moved,
        // not merely that it moved.
        let worstElementDeviation (MuellerMatrix expected) (MuellerMatrix actual) : float * int * int =
            [ for i in 0 .. 3 do
                for j in 0 .. 3 -> (abs (expected.[i, j] - actual.[i, j]), i, j) ]
            |> List.maxBy (fun (d, _, _) -> d)

        let describeDeviation (label : string) (expected : MuellerMatrix) (actual : MuellerMatrix) : float * string =
            let (worst, i, j) = worstElementDeviation expected actual
            worst, $"{label} worst {worst} at M[{i},{j}]"

        let expectedQz =
            Propagation.muellerOfRows
                [ [  0.9412063946;  0.0409543238; -0.0259356814; -0.1220013287 ]
                  [  0.0525351141;  0.6606049231; -0.3659862371; -0.1421373359 ]
                  [ -0.0129854586;  0.4061015401;  0.6094438915; -0.0169558310 ]
                  [ -0.0957187769;  0.0752040278;  0.0184691606;  1.0306648444 ] ]
        let expectedLr =
            Propagation.muellerOfRows
                [ [  0.9931797644; -0.0046529422;  0.0596850441; -0.1036207527 ]
                  [  0.3531940843;  0.1474307188; -0.0954724381; -0.6568249890 ]
                  [  0.3425940212; -0.3946501705;  0.6900321099; -0.5968927113 ]
                  [ -0.0104722933;  0.1247347199;  0.4614122160;  0.5910486653 ] ]
        let expectedComb =
            Propagation.muellerOfRows
                [ [  0.9001173290;  0.0831007067;  0.0350162173; -0.1712567077 ]
                  [  0.3413132788; -0.1694202380; -0.1576526274; -0.5502997288 ]
                  [  0.3377633308; -0.1048712506;  0.7681312510; -0.5366531986 ]
                  [  0.0999436274;  0.0459751884;  0.3292576266;  0.3168329794 ] ]

        Assert.Equal(16, mQz.rank)
        Assert.Equal(16, mLr.rank)
        Assert.Equal(16, mComb.rank)
        // One assertion over ALL THREE matrices, reporting every matrix's worst element so a regression run
        // shows the whole picture in a single message instead of stopping at the first element out of band.
        let matrixDeviations =
            [ describeDeviation "M_QZ" expectedQz mQz.matrix
              describeDeviation "M_LR" expectedLr mLr.matrix
              describeDeviation "M_{QZ+LR}" expectedComb mComb.matrix ]
        let worstMatrixDeviation = matrixDeviations |> List.map fst |> List.max
        Assert.True(
            worstMatrixDeviation < dataMatrixTol,
            $"""section 7.2 matrices (tol {dataMatrixTol}): {String.concat "; " (matrixDeviations |> List.map snd)}""")

        // ---- Section 8 cascade identity: M_{QZ+LR} vs the QZ-first product M_LR . M_QZ. ----
        // Expected values transcribed from `combined_vs_product_matrix` in the same reference summary.
        let cascade = cascadeProduct mLr.matrix mQz.matrix
        let d = frobeniusDiff mComb.matrix cascade
        let worstMetricDeviation =
            [ abs (d.frobenius - 0.5714178795538456)
              abs (d.meanAbs - 0.11328628636347338)
              abs (d.maxAbs - 0.26806464658062423) ]
            |> List.max
        Assert.True(
            worstMetricDeviation < dataMatrixTol,
            $"cascade metrics: worst deviation {worstMetricDeviation} (frobenius = {d.frobenius}, meanAbs = {d.meanAbs}, maxAbs = {d.maxAbs}, tol {dataMatrixTol})")

        // The two largest element differences both live in the 4th row (1-indexed) — at columns 4 and 2, each
        // ~0.268 and near-tied. (The slice text names (4,2) and (2,2) 1-indexed; the real data's runner-up to
        // (4,2) is (4,4), not (2,2): (2,2)~0.193 is only the 4th-largest. Followed the data per the base
        // protocol's skepticism rule — see the state-of-the-world Gotchas.)
        //
        // Spec 0044 note: the reference pipeline's own `delta_matrix` now corroborates this independently —
        // delta[3][3] = -0.2680646466 and delta[3][1] = -0.2678257477 are the two largest by magnitude, so
        // the data-driven correction the 0042 implementation made was right and this assertion stands.
        let topTwoDiffPositions =
            [ for i in 0 .. 3 do
                for j in 0 .. 3 ->
                    (i, j), abs (Propagation.muellerElement mComb.matrix i j - Propagation.muellerElement cascade i j) ]
            |> List.sortByDescending snd
            |> List.truncate 2
            |> List.map fst
            |> Set.ofList
        Assert.True((topTwoDiffPositions = Set.ofList [ (3, 3); (3, 1) ]), $"the two largest abs-delta positions (0-indexed) = %A{topTwoDiffPositions}")
