namespace BerremanTests

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
