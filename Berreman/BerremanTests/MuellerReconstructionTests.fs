namespace BerremanTests

open System.IO
open Berreman.Geometry
open Berreman.Fields
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Experiments             // DataFilePath (step 025) — keyed by the CSV-load mock (step 005)
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Storage                        // MuellerDataStore — the REAL parseMuellerCsv / createFileBacked (step 006)
open Xunit
open BerremanTests.MatrixComparison

/// Spec 0042 (001) — acceptance for `MuellerReconstruction.retarderMueller`. Reuses the element-by-element
/// Mueller compare loop from `MuellerMatrixTests.fs:20` and the shared `allowedDiff` tolerance
/// (`MatrixComparison.fs:13`) — no hand-rolled epsilon logic.
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

    // Spec 0042 (005, ADD_CONTRACT STORE_XDUO_0009) — the Mueller measured-data CSV-LOAD seam. The MOCK and its
    // canned rows are bound here, ahead of the members (FS0960: in a class type every `let` binding precedes
    // the first member). `makeMockData` is an inline `MuellerDataProxy` stub whose `tryLoadFamily` keys the
    // canned `MuellerRawRow` list off `DataFilePath.value`: a known path returns its rows; an unknown path
    // returns `Error (EmptyFile _)` — staying WITHIN the declared two-case channel, never a throw. This
    // exercises the exact signature and the typed-error case with no real filesystem IO — the real file-backed
    // proxy lands in a later IMPLEMENT_CONTRACT STORE_XDUO_0009. The `capturedAt` timestamps are fixed literals
    // (deterministic across runs — no ambient clock read).
    let cannedRows : MuellerRawRow list =
        [ { experiment = "E1"; captureIndex = 0; capturedAt = System.DateTimeOffset(2026, 7, 15, 12, 0, 0, System.TimeSpan.Zero); description = Angle.degree 0.0; avgTotal = 0.42 }
          { experiment = "E1"; captureIndex = 1; capturedAt = System.DateTimeOffset(2026, 7, 15, 12, 5, 0, System.TimeSpan.Zero); description = Angle.degree 45.0; avgTotal = 0.31 } ]

    let makeMockData (rowsByPath : Map<string, MuellerRawRow list>) : MuellerDataProxy =
        {
            tryLoadFamily =
                fun (path : DataFilePath) ->
                    match Map.tryFind path.value rowsByPath with
                    | Some rows -> Ok rows
                    | None -> Error (EmptyFile $"the mock has no canned capture rows for path '{path.value}'")
        }

    let seededDataMock () : MuellerDataProxy =
        makeMockData (Map.ofList [ "C:/data/mueller-family.csv", cannedRows ])

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

    /// Locate the sibling `optics-mueller` checkout's AIR data folder by walking up from the test output
    /// directory to the first ancestor that carries `optics-mueller\data\raw\final\lp_lp.csv`, returning that
    /// `final` directory — or `None` when no such checkout is present, so the data-dependent cross-check SKIPS
    /// (manual §5.6 / §13 Q3: the test pulls data via a relative walk-up, never a committed copy).
    let tryFindOpmFinalDir () : string option =
        let rec walk (dir : DirectoryInfo) : string option =
            if isNull dir then None
            else
                let candidate = Path.Combine(dir.FullName, "optics-mueller", "data", "raw", "final")
                if File.Exists(Path.Combine(candidate, "lp_lp.csv")) then Some candidate
                else walk dir.Parent
        walk (DirectoryInfo(System.AppContext.BaseDirectory))

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
    member _.``a mock MuellerDataProxy loads canned capture rows through its exact tryLoadFamily signature`` () =
        // Spec 0042 (005) acceptance: build a stub MuellerDataProxy and load canned rows through the EXACT
        // tryLoadFamily signature.
        let proxy = seededDataMock ()

        // Pin the EXACT signature the acceptance names by binding the field to an explicitly-typed local:
        // the compiler rejects the file if `tryLoadFamily` drifts from its declared shape.
        let loadFamily : DataFilePath -> Result<MuellerRawRow list, MuellerDataError> = proxy.tryLoadFamily

        match loadFamily (DataFilePath.create "C:/data/mueller-family.csv") with
        | Ok rows ->
            Assert.Equal(2, rows.Length)
            Assert.Equal<MuellerRawRow list>(cannedRows, rows)
        | Error e -> Assert.Fail($"expected the canned capture rows, got %A{e}")

    [<Fact>]
    member _.``an unknown path yields a typed MuellerDataError from tryLoadFamily, never a throw`` () =
        // Spec 0042 (005) acceptance: an unknown path returns a typed MuellerDataError (the mock has no rows
        // for it) — never a throw.
        let proxy = seededDataMock ()
        match proxy.tryLoadFamily (DataFilePath.create "C:/data/nowhere.csv") with
        | Error (EmptyFile reason) -> Assert.False(System.String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (EmptyFile _) for an unknown path, got %A{other}")

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
    member _.``createFileBacked produces a MuellerDataProxy that maps a missing file to a typed error (never a throw)`` () =
        // Spec 0042 (006) acceptance: createFileBacked () produces a MuellerDataProxy; its tryLoadFamily reads
        // at the IO boundary and maps a missing file to a typed MuellerDataError — never a throw.
        let proxy = MuellerDataStore.createFileBacked ()

        // Pin the EXACT signature the acceptance names by binding the field to an explicitly-typed local: the
        // compiler rejects the file if `tryLoadFamily` drifts from its declared shape.
        let loadFamily : DataFilePath -> Result<MuellerRawRow list, MuellerDataError> = proxy.tryLoadFamily

        match loadFamily (DataFilePath.create "C:/data/definitely-missing-mueller-file-0042.csv") with
        | Error _ -> ()   // a missing file is mapped to a typed MuellerDataError at the IO boundary — never a throw
        | Ok _ -> Assert.Fail("expected a typed MuellerDataError for a missing file, got Ok")

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
    member _.``Stage-1 re-derived calibration constants cross-check the section 2.2 rounded values (OPM AIR data present)`` () =
        // Spec 0042 (008) acceptance (data-dependent): when the sibling OPM AIR data is present, the constants
        // RE-derived by the three Stage-1 functions each land within a loose (~1°) band of their §2.2 cross-check
        // (z_LP≈155.44, z_CPL≈97.00, δ_an≈83.67, θ_src≈39.31, δ_src≈82.55). When the OPM checkout is absent the
        // fact SKIPS (xunit.v3-native dynamic skip) instead of failing — see the state-of-the-world Gotchas for
        // why this is a plain [<Fact>] + Assert.Skip rather than the v2-only [<SkippableFact>] the slice names.
        match tryFindOpmFinalDir () with
        | None ->
            Assert.Skip("OPM AIR calibration data is absent (no sibling optics-mueller checkout) — skipping the section 2.2 cross-check")
        | Some finalDir ->
            let solver = createMathNetSvd ()
            let data = MuellerDataStore.createFileBacked ()
            let load (name : string) : MuellerRawRow list =
                match data.tryLoadFamily (DataFilePath.create (Path.Combine(finalDir, name))) with
                | Ok rows -> rows
                | Error e -> failwith $"could not load the OPM family '{name}': %A{e}"

            // dark_mean = mean avg_total over darkness_checks.csv (the reference ≈ 869.666).
            let darkMean = load "darkness_checks.csv" |> List.averageBy (fun r -> r.avgTotal)

            // LP-LP AIR → free cosine fit → LP analyzer zero z_LP ≈ 155.44.
            let (anglesLpLp, normLpLp) = reduceAirTraces darkMean (load "lp_lp.csv")
            let lpLpFit = unwrapFit "LP-LP" (freeCosineFit solver anglesLpLp normLpLp)
            Assert.True(abs (lpLpFit.zeroDeg - 155.44) < 1.0, $"z_LP = {lpLpFit.zeroDeg}")

            // LP-CPL AIR → free cosine fit → CPL analyzer zero z_CPL ≈ 97.00; δ_an = arccos(vis) ≈ 83.67.
            let (anglesLpCpl, normLpCpl) = reduceAirTraces darkMean (load "lp_cpl.csv")
            let lpCplFit = unwrapFit "LP-CPL" (freeCosineFit solver anglesLpCpl normLpCpl)
            Assert.True(abs (lpCplFit.zeroDeg - 97.00) < 1.0, $"z_CPL = {lpCplFit.zeroDeg}")
            let deltaAnDeg = (retardanceFromVisibility lpCplFit.visibility).degrees
            Assert.True(abs (deltaAnDeg - 83.67) < 1.0, $"δ_an = {deltaAnDeg}")

            // CPL-LP AIR, fit in the source LP frame (phased on z_LP) → closed-form source: θ_src ≈ 39.31,
            // δ_src ≈ 82.55.
            let (anglesCplLp, normCplLp) = reduceAirTraces darkMean (load "cpl_lp.csv")
            let (qLp, uLp) = lpFrameCoeffs solver anglesCplLp normCplLp lpLpFit.zeroDeg
            let source = sourceCplFromLpFrame qLp uLp
            Assert.True(abs (source.thetaRel.degrees - 39.31) < 1.0, $"θ_src = {source.thetaRel.degrees}")
            Assert.True(abs (source.retardance.degrees - 82.55) < 1.0, $"δ_src = {source.retardance.degrees}")

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

        // darkSubtract — avg_total − dark_mean, with the ≈ 869.666 OPM dark mean passed in as data.
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
