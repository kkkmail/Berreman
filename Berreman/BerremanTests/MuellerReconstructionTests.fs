namespace BerremanTests

open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.NamingTemplates
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Storage
open OpticalConstructor.Storage.MuellerDataStore
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Storage.LabFileImport
open OpticalConstructor.Optimization.StagedMuellerPath
open Xunit
open BerremanTests.MatrixComparison
open BerremanTestSupport

/// Spec 0042 — acceptance suite for the Mueller-reconstruction pipeline (`MuellerReconstruction` + the
/// Phone ROI CSV parser + the vendored-MathNet SVD solver). Most facts reuse the
/// element-by-element Mueller compare loop from `MuellerMatrixTests.fs:20` and the shared `allowedDiff`
/// tolerance (`MatrixComparison.fs:13`).
///
/// Spec 0044 §8 retired the external data dependency and tightened the measured-data acceptance:
///   - the measured data is the optics-mueller kit's lab files, found from the repository root and read
///     read-only through the Phone ROI CSV import profile (spec 0050 S9.4);
///   - nothing walks up to, or otherwise probes for, an external checkout, and the two
///     data-backed facts no longer skip: absent data is now a defect and fails;
///   - the §7.2 matrices and the Stage-1 constants are asserted against the reference pipeline's own
///     full-precision output (`reference_step*_summary.json` in `data/optics-mueller/data/reference/`) at
///     `dataMatrixTol` / `stage1Tol`, rather than against the report's rounded printed values at ~2e-3; the
///     expectations live in `BerremanTestSupport.MuellerReference` (spec 0050 S10.3).
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
        RealMatrix4x4.create
            [ [ 1.0; 0.2; -0.3; 0.4 ]
              [ 0.5; 0.6; 0.7; -0.8 ]
              [ -0.9; 1.0; 0.1; 0.2 ]
              [ 0.3; -0.4; 0.5; 0.6 ] ]
        |> MuellerMatrix

    // Spec 0042 (003, ADD_CONTRACT SVC_XDUO_0002) — the least-squares SOLVE seam. The MOCK and its canned
    // solution are bound here, ahead of the members (FS0960: in a class type every `let` binding precedes the
    // first member). `makeMockSolver` is an inline `MuellerSolverProxy` stub whose `solveLinearLeastSquares`
    // keys off the design HEIGHT (16 = the Mueller-vec unknown count): a full-height design returns the canned
    // `LeastSquaresSolution`; fewer rows than unknowns returns `Error (RankDeficientErr rows)`; an empty design
    // returns `Error (SingularDesignErr _)`. This exercises the exact signature and BOTH typed error cases with
    // no real linear algebra — the real MathNet-SVD proxy (`createMathNetSvd ()`) lands in step 004.
    let cannedSolution : LeastSquaresSolution =
        { solution = RealVector.create (Array.init 16 (fun k -> 0.1 * float k)); rank = MatrixRank 16; rmse = ResidualRmse 0.0 }

    let makeMockSolver () : MuellerSolverProxy =
        {
            solveLinearLeastSquares =
                fun (system : LeastSquaresSystem) ->
                    match system.design with
                    | [||] -> Error (SingularDesignErr "the mock rejects an empty design matrix")
                    | rows when rows.Length < 16 -> Error (RankDeficientErr (MatrixRank rows.Length))
                    | _ -> Ok cannedSolution
        }

    // Spec 0042 (006, IMPLEMENT_CONTRACT STORE_XDUO_0009) — a committed multi-column Mueller capture CSV
    // fixture for the REAL MuellerDataStore.parseMuellerCsv. A UTF-8 BOM prefix (char 0xFEFF, built here rather
    // than embedded in the source) exercises the parser's BOM-tolerance; the header carries the many real
    // columns (iso / exposure_ns / avg_R/G/B / …) the reconstruction does not consume, so reading the five
    // required columns BY NAME is exactly what is under test. The `captured_at` timestamps carry an explicit
    // UTC offset — deterministic across runs, no ambient-clock or local-timezone dependence. Bound here, ahead
    // of the members (FS0960: in a class type every `let` binding precedes the first member).
    let muellerCsvFixture : DataFileText =
        DataFileText (
            string (char 0xFEFF)
            + "experiment,capture_index,captured_at,description,iso,exposure_ns,avg_R,avg_G,avg_B,avg_total\n"
            + "CPL-(AIR#0)-CPL,0,2026-07-15T12:00:00+00:00,0,100,5000,10.0,11.0,12.0,869.5\n"
            + "CPL-(AIR#0)-CPL,1,2026-07-15T12:05:00+00:00,45,100,5000,20.0,21.0,22.0,912.25\n")

    // A malformed fixture: the `capture_index` cell is not an integer, so parseMuellerCsv must map it to a
    // typed MalformedRowErr (never a throw across into the pure Domain).
    let malformedMuellerCsv : DataFileText =
        DataFileText (
            "experiment,capture_index,captured_at,description,avg_total\n"
            + "E1,not-an-int,2026-07-15T12:00:00+00:00,0,869.5\n")

    // Spec 0042 (007, IMPLEMENT) — component-wise closeness helpers for the effective-state +90/−90 invariance,
    // over the shared `allowedDiff` (no hand-rolled epsilon). Bound here, ahead of the members (FS0960: in a
    // class type every `let` binding precedes the first member). StokesVector is read through the
    // `StokesVector.s0` … `s3` members; RealVector4 through its indexer.
    let assertStokesClose (a : StokesVector) (b : StokesVector) =
        let (a0, a1, a2, a3) = a.s0, a.s1, a.s2, a.s3
        let (b0, b1, b2, b3) = b.s0, b.s1, b.s2, b.s3
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
    let degree = Angle.radiansPerDegree

    /// One lab file of the optics-mueller kit, read read-only through the Phone ROI CSV import profile; a missing kit
    /// folder or any refusal fails the test with what it says (spec 0050 S9.4).
    let loadDataSet (file : KitRelativePath) : MuellerRawRow list =
        match OpticsMuellerKit.tryFindFolder with
        | None -> failwith $"the optics-mueller kit folder holding '{file.value}' was not found under the repository root"
        | Some folder ->
            match (KitProxy.create folder).readOnlyView.readDataFile file with
            | Error e -> failwith $"could not read the lab file '{file.value}': {e.describe}"
            | Ok data ->
                match (BuiltInProfileChoice PhoneRoiCsvProfile).mapText data.text with
                | Ok (CapturePoints rows) -> rows
                | Ok _ -> failwith $"the Phone ROI CSV profile mapped '{file.value}' to points other than capture rows"
                | Error e -> failwith $"the Phone ROI CSV profile refused '{file.value}': %A{e}"

    let kitPath (text : string) : KitRelativePath =
        match KitRelativePath.tryCreate text with
        | Ok path -> path
        | Error e -> failwith e.describe

    let lpLpFile = kitPath "data/raw/final/lp_lp.csv"
    let lpCplFile = kitPath "data/raw/final/lp_cpl.csv"
    let cplLpFile = kitPath "data/raw/final/cpl_lp.csv"
    let cplCplDay1File = kitPath "data/raw/final/cpl_cpl_day1_main.csv"
    let cplCplDay2File = kitPath "data/raw/final/cpl_cpl_day2_corrections.csv"
    let darknessChecksFile = kitPath "data/raw/final/darkness_checks.csv"
    let bullshitChecksFile = kitPath "data/raw/final/bullshit_checks.csv"

    /// The seven lab files' capture rows, the input of the staged Mueller path (spec 0050 S10.3).
    let opticsMuellerCaptures () : StagedMuellerCaptureData =
        {
            lpLpRows = loadDataSet lpLpFile
            lpCplRows = loadDataSet lpCplFile
            cplLpRows = loadDataSet cplLpFile
            cplCplDay1Rows = loadDataSet cplCplDay1File
            cplCplDay2Rows = loadDataSet cplCplDay2File
            darknessRows = loadDataSet darknessChecksFile
            bullshitCheckRows = loadDataSet bullshitChecksFile
        }

    /// The label read through the standard naming template, failing the test with the refusal's `describe` (spec 0050
    /// S8.3).
    let standardReading (label : MuellerExperimentLabel) : NameReading =
        match NamingTemplate.standard.readName label.nameText with
        | Ok reading -> reading
        | Error error -> failwith error.describe

    [<Fact>]
    member _.``zero retardance is the identity Mueller matrix (arbitrary azimuth)`` () =
        // A wave plate with no phase delay is a pass-through, whatever its azimuth: R(-θ)·I·R(θ) = I.
        let actual = retarderMueller (Angle.degree 37.0<Berreman.Constants.deg>) (Retardance.degree 0.0<Berreman.Constants.deg>)
        assertMuellerEqual Propagation.identityMueller actual

    [<Fact>]
    member _.``zero retardance at zero azimuth is the identity Mueller matrix`` () =
        let actual = retarderMueller Angle.zero (Retardance.degree 0.0<Berreman.Constants.deg>)
        assertMuellerEqual Propagation.identityMueller actual

    [<Fact>]
    member _.``quarter-wave retarder at azimuth 0 is the standard QWP Mueller form`` () =
        // Standard QWP, fast axis horizontal: [[1,0,0,0],[0,1,0,0],[0,0,0,1],[0,0,-1,0]].
        let expected =
            RealMatrix4x4.create
                [ [ 1.0; 0.0; 0.0; 0.0 ]
                  [ 0.0; 1.0; 0.0; 0.0 ]
                  [ 0.0; 0.0; 0.0; 1.0 ]
                  [ 0.0; 0.0; -1.0; 0.0 ] ]
            |> MuellerMatrix
        let actual = retarderMueller Angle.zero (Retardance.degree 90.0<Berreman.Constants.deg>)
        assertMuellerEqual expected actual

    [<Fact>]
    member _.``quarter-wave retarder at azimuth 45 is the standard rotated QWP Mueller form`` () =
        // Standard QWP, fast axis at 45°: [[1,0,0,0],[0,0,0,-1],[0,0,1,0],[0,1,0,0]] — exercises rotateMueller.
        let expected =
            RealMatrix4x4.create
                [ [ 1.0; 0.0; 0.0; 0.0 ]
                  [ 0.0; 0.0; 0.0; -1.0 ]
                  [ 0.0; 0.0; 1.0; 0.0 ]
                  [ 0.0; 1.0; 0.0; 0.0 ] ]
            |> MuellerMatrix
        let actual = retarderMueller (Angle.degree 45.0<Berreman.Constants.deg>) (Retardance.degree 90.0<Berreman.Constants.deg>)
        assertMuellerEqual expected actual

    [<Fact>]
    member _.``Retardance degrees and value round-trip`` () =
        let r = Retardance.degree 90.0<Berreman.Constants.deg>
        Assert.True(abs (r.degrees - 90.0) < allowedDiff, $"degrees = {r.degrees}")
        Assert.True(abs (r.value - (System.Math.PI / 2.0)) < allowedDiff, $"value = {r.value}")

    [<Fact>]
    member _.``vecColumnMajor round-trips through muellerOfVecColumnMajor`` () =
        // M.vecColumnMajor.columnMajorMueller = M for every 4×4 (the vec round-trip acceptance).
        let actual = asymmetricM.vecColumnMajor.columnMajorMueller
        assertMuellerEqual asymmetricM actual

    [<Fact>]
    member _.``kron4 dotted with the column-major vec is a dot (M times s) (transpose guard)`` () =
        // The design row aᵀ⊗sᵀ dotted with vec_F(M) MUST equal a·(M·s); the non-symmetric M makes this fail
        // under a row-major flatten, so the identity pins the column-major (order F) convention.
        let s = StokesVector.create [ 1.0; 0.3; -0.7; 0.5 ]
        let a = RealVector4.create [ 0.2; -1.1; 0.4; 0.9 ]
        let (RealVector vec) = asymmetricM.vecColumnMajor
        let lhs = Array.map2 (*) (kron4 s a).value (vec.ToArray()) |> Array.sum
        let (StokesVector ms) = asymmetricM * s
        let rhs = [ 0 .. 3 ] |> List.sumBy (fun j -> a.[j] * ms.[j])
        Assert.True(abs (lhs - rhs) < allowedDiff, $"lhs = {lhs}, rhs = {rhs}, diff = {abs (lhs - rhs)}")

    [<Fact>]
    member _.``frobeniusDiff of a matrix with itself is zero`` () =
        let d = frobeniusDiff asymmetricM asymmetricM
        Assert.True(d.frobenius.value < allowedDiff, $"frobenius = {d.frobenius.value}")
        Assert.True(d.maxAbs.value < allowedDiff, $"maxAbs = {d.maxAbs.value}")
        Assert.True(d.meanAbs.value < allowedDiff, $"meanAbs = {d.meanAbs.value}")

    [<Fact>]
    member _.``frobeniusDiff localizes the largest element difference and its index`` () =
        // Perturb the single element at (2,1) from 1.0 to 3.0 (a diff of 2.0); the readout must localize it.
        let perturbed =
            RealMatrix4x4.create
                [ [ 1.0; 0.2; -0.3; 0.4 ]
                  [ 0.5; 0.6; 0.7; -0.8 ]
                  [ -0.9; 3.0; 0.1; 0.2 ]
                  [ 0.3; -0.4; 0.5; 0.6 ] ]
            |> MuellerMatrix
        let d = frobeniusDiff asymmetricM perturbed
        let (mi, mj) = d.argMax
        Assert.Equal(MuellerRowIndex 2, mi)
        Assert.Equal(MuellerColumnIndex 1, mj)
        Assert.True(abs (d.maxAbs.value - 2.0) < allowedDiff, $"maxAbs = {d.maxAbs.value}")
        Assert.True(abs (d.frobenius.value - 2.0) < allowedDiff, $"frobenius = {d.frobenius.value}")
        Assert.True(abs (d.meanAbs.value - (2.0 / 16.0)) < allowedDiff, $"meanAbs = {d.meanAbs.value}")

    [<Fact>]
    member _.``a mock MuellerSolverProxy solves through its exact signature, returning a canned solution and typed SolverErrors`` () =
        // Spec 0042 (003) acceptance: build a stub MuellerSolverProxy and call solveLinearLeastSquares
        // through its EXACT signature, returning a canned solution AND a typed SolverError.
        let proxy = makeMockSolver ()

        // Pin the EXACT signature the acceptance names by binding the field to an explicitly-typed local:
        // the compiler rejects the file if `solveLinearLeastSquares` drifts from its declared shape.
        let solve : LeastSquaresSystem -> Result<LeastSquaresSolution, SolverError> = proxy.solveLinearLeastSquares

        // A full-height design (16 rows for the 16 Mueller unknowns) returns the canned solution.
        let fullDesign : DesignRow[] = Array.init 16 (fun r -> DesignRow (Array.init 16 (fun c -> if r = c then 1.0 else 0.0)))
        let rhs : float[] = Array.init 16 (fun r -> 0.1 * float r)
        match solve { design = fullDesign; rhs = rhs } with
        | Ok sol ->
            let (RealVector solution) = sol.solution
            Assert.Equal(16, solution.Count)
            for k in 0 .. 15 do
                Assert.True(abs (sol.solution.[k] - 0.1 * float k) < allowedDiff, $"solution[{k}] = {sol.solution.[k]}")
            Assert.Equal(MatrixRank 16, sol.rank)
            Assert.True(abs sol.rmse.value < allowedDiff, $"rmse = {sol.rmse.value}")
        | Error e -> Assert.Fail($"expected the canned LeastSquaresSolution, got %A{e}")

        // An under-determined design (fewer rows than the 16 unknowns) returns a typed RankDeficientErr — never a throw.
        match solve { design = [| DesignRow [| 1.0; 0.0 |]; DesignRow [| 0.0; 1.0 |] |]; rhs = rhs } with
        | Error (RankDeficientErr rank) -> Assert.Equal(MatrixRank 2, rank)
        | other -> Assert.Fail($"expected Error (RankDeficientErr _), got %A{other}")

        // An empty design returns a typed SingularDesignErr carrying a non-empty reason — never a throw.
        match solve { design = [||]; rhs = rhs } with
        | Error (SingularDesignErr reason) -> Assert.False(System.String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (SingularDesignErr _), got %A{other}")

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
        let design : DesignRow[] =
            [| DesignRow [| 1.0;  0.0 |]
               DesignRow [| 0.0;  1.0 |]
               DesignRow [| 1.0;  1.0 |]
               DesignRow [| 2.0; -1.0 |] |]
        let known : float[] = [| 3.0; -2.0 |]
        let rhs : float[] = [| 3.0; -2.0; 1.0; 8.0 |]

        match proxy.solveLinearLeastSquares { design = design; rhs = rhs } with
        | Ok sol ->
            let (RealVector solution) = sol.solution
            Assert.Equal(2, solution.Count)
            for k in 0 .. 1 do
                Assert.True(abs (sol.solution.[k] - known.[k]) < allowedDiff, $"solution[{k}] = {sol.solution.[k]}, expected {known.[k]}")
            // Rank equals the column count on a full-rank solve — the observability check the acceptance names.
            Assert.Equal(MatrixRank 2, sol.rank)
            Assert.True(sol.rmse.value < allowedDiff, $"rmse = {sol.rmse.value}")
        | Error e -> Assert.Fail($"expected the known least-squares solution, got %A{e}")

    [<Fact>]
    member _.``the real createMathNetSvd proxy maps degenerate designs to typed SolverErrors (never a throw)`` () =
        // The real proxy honours the TOTAL seam: a singular / rank-deficient design returns a typed value, not
        // an exception across the proxy boundary.
        let proxy = createMathNetSvd ()

        // An empty design → SingularDesignErr carrying a non-empty reason.
        match proxy.solveLinearLeastSquares { design = [||]; rhs = [||] } with
        | Error (SingularDesignErr reason) -> Assert.False(System.String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (SingularDesignErr _), got %A{other}")

        // A rank-deficient design (column 2 = 2× column 1, so the numerical rank is 1 < the 2 columns).
        let dependent : DesignRow[] =
            [| DesignRow [| 1.0; 2.0 |]
               DesignRow [| 2.0; 4.0 |]
               DesignRow [| 3.0; 6.0 |] |]
        match proxy.solveLinearLeastSquares { design = dependent; rhs = [| 1.0; 2.0; 3.0 |] } with
        | Error (RankDeficientErr rank) -> Assert.True(rank.value < 2, $"rank = {rank.value}")
        | other -> Assert.Fail($"expected Error (RankDeficientErr _), got %A{other}")

    [<Fact>]
    member _.``the real parseMuellerCsv parses a committed multi-column CSV fixture into MuellerRawRows (description elevated to Angle)`` () =
        // Spec 0042 (006) acceptance: parse a committed multi-column, BOM-prefixed CSV fixture through the REAL
        // MuellerDataStore.parseMuellerCsv (not a mock), yielding MuellerRawRow values whose `description` is
        // elevated to the engine Angle. Reading the five required columns BY NAME ignores the unread columns.
        match muellerCsvFixture.parseMuellerCsv with
        | Ok rows ->
            Assert.Equal(2, rows.Length)
            let r0 = rows.[0]
            Assert.Equal(MuellerExperimentLabel "CPL-(AIR#0)-CPL", r0.experiment)
            Assert.Equal(CaptureIndex 0, r0.captureIndex)
            Assert.Equal(System.DateTimeOffset(2026, 7, 15, 12, 0, 0, System.TimeSpan.Zero), r0.capturedAt)
            // `description` elevated to Angle: read back its `degrees` view (the raw analyzer dial angle).
            Assert.True(abs r0.description.degrees < allowedDiff, $"description = {r0.description.degrees}")
            Assert.True(abs (r0.avgTotal.value - 869.5) < allowedDiff, $"avgTotal = {r0.avgTotal.value}")
            let r1 = rows.[1]
            Assert.Equal(CaptureIndex 1, r1.captureIndex)
            Assert.True(abs (r1.description.degrees - 45.0) < allowedDiff, $"description = {r1.description.degrees}")
            Assert.True(abs (r1.avgTotal.value - 912.25) < allowedDiff, $"avgTotal = {r1.avgTotal.value}")
        | Error e -> Assert.Fail($"expected the parsed capture rows, got %A{e}")

    [<Fact>]
    member _.``the real parseMuellerCsv maps a malformed file to a typed MuellerDataError, never a throw`` () =
        // Spec 0042 (006) acceptance: a malformed file (capture_index is not an integer) yields a typed
        // MuellerDataError — no exception crosses into the pure Domain.
        match malformedMuellerCsv.parseMuellerCsv with
        | Error (MalformedRowErr reason) -> Assert.False(System.String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (MalformedRowErr _) for a malformed file, got %A{other}")

    [<Fact>]
    member _.``the optics-mueller kit's final lab files load through the Phone ROI CSV profile with their documented row counts`` () =
        // Spec 0050 S9.4: the expected row counts are the inventory of data/optics-mueller/data/reference/README.md
        // (spec 0042 §6), so a truncated or wrong lab file fails loudly at the load rather than as a slightly wrong
        // reconstruction downstream.
        let expectedRowCounts : (KitRelativePath * int) list =
            [ lpLpFile, 209
              lpCplFile, 209
              cplLpFile, 209
              cplCplDay1File, 170
              cplCplDay2File, 57
              darknessChecksFile, 6
              bullshitChecksFile, 2 ]

        let rowCounts = expectedRowCounts |> List.map (fun (file, _) -> file, (loadDataSet file).Length)
        Assert.Equal<(KitRelativePath * int) list>(expectedRowCounts, rowCounts)

    [<Fact>]
    member _.``the optics-mueller kit holds every file of the former data archive byte for byte`` () =
        // Spec 0050 S9.4: each file's hex SHA-256 is its former `data.zip` entry's; the `-text !eol` rule in
        // .gitattributes keeps the lab files' CRLF and byte-order mark through a clone (001 §E).
        let archiveHashes : (KitRelativePath * ContentHash) list =
            [ kitPath "data/raw/cpl_cpl.csv", ContentHash "c1cbbde1c7b77acf6d23f972bb85f679e0072779d396a49aaf02b1d92e447eb5"
              kitPath "data/raw/cpl_cpl_big_center.csv", ContentHash "515d16bb7a375b734038a8dbc6730bb6e726d99bfd11d954b986adf81d47b7b8"
              kitPath "data/raw/cpl_cpl_small_center.csv", ContentHash "f2d97088f562cb5e8097b827b6774d1ba88baefd25b15f312515ad77b958d703"
              kitPath "data/raw/cpl_cpl_small_small_center.csv", ContentHash "aa18398266782f7cedf15008a8e66beff28f89c81304d68e062d6f8f8216c78b"
              kitPath "data/raw/cpl_cpl_standard_ellipse.csv", ContentHash "94b8ef01770a12617f55e1b329d1513670044cee3c0c5e340e9c5826588372e5"
              kitPath "data/raw/cpl_lr45_standard_ellipse_scale_095.csv", ContentHash "59a7b7d7f223bf28c4623299df6a530f63787570eb346935b76d468228fbd3bc"
              kitPath "data/raw/cpl_lr45_standard_ellipse_scale_105.csv", ContentHash "4fd5b0b5fc09cd291d57732edfd1f582c37119c230c7550d100ebd4a94493336"
              kitPath "data/raw/cpl_lr45_standard_ellipse_subset.csv", ContentHash "53de707ce70b9792d024db41ae39eac8506656916c97b0db1ef9f1512ef50813"
              kitPath "data/raw/final/bullshit_checks.csv", ContentHash "f2f7d28d881b57f5760562d030c12d1e554c7c8cc82d6657e9e4fb5c281c2c9e"
              kitPath "data/raw/final/cpl_cpl_day1_main.csv", ContentHash "9920ae4768558bd865c0190878adf97dde9901e9436a299a0c49e7d48ed39664"
              kitPath "data/raw/final/cpl_cpl_day2_corrections.csv", ContentHash "fcace7c401940d6b4a84347acd2b2afb925f5ea9c2c57e64fcc6d88e4ad26b2e"
              kitPath "data/raw/final/cpl_lp.csv", ContentHash "9737ebe6534446ce3d4e1bcbcda222b88fae2d3c58de0cc8db1d322a5c002f39"
              kitPath "data/raw/final/darkness_checks.csv", ContentHash "3b11f42dd67f29a09275b13442849415c82d7a967f5d5933713abb06dfb43cc2"
              kitPath "data/raw/final/lp_cpl.csv", ContentHash "d014c90144a98eb1949d30716be1f01049ec9763e0e3b94ceb68d2e43c4b1ba8"
              kitPath "data/raw/final/lp_lp.csv", ContentHash "0fc29a63fd7de90b85842af5020cd79fb1e74ac2d24b49f7c7673cbead82cd81"
              kitPath "data/reference/shapes/standard_air_ellipse_scale_095.json", ContentHash "f8b15f8069d4986c6a68924349c66b0455b7a976da441adfdf5620af3d9e9425"
              kitPath "data/reference/shapes/standard_air_ellipse_scale_105.json", ContentHash "e254c679a2861bf5f625a03b5d62c2e6a6f3aac38f29afdfba632521370bedeb"
              kitPath "data/reference/shapes/standard_air_ellipse_v1.json", ContentHash "76bb56c20c63356640ad286f1f1b109eaa9714f7ea16c2606086705d88854f2d" ]

        match OpticsMuellerKit.tryFindFolder with
        | None -> Assert.Fail "the optics-mueller kit folder was not found under the repository root"
        | Some folder ->
            let kit = (KitProxy.create folder).readOnlyView
            let storedHashes =
                archiveHashes
                |> List.map (fun (path, _) ->
                    match kit.readDataFile path with
                    | Ok file -> path, file.contentHash
                    | Error e -> failwith $"could not read '{path.value}' from the optics-mueller kit: {e.describe}")
            Assert.Equal<(KitRelativePath * ContentHash) list>(archiveHashes, storedHashes)

    [<Fact>]
    member _.``the standard naming template reads CPL-(QZ#90-LR#90)-CPL as the CPL-CPL family and the QZ LR product at phi 90, and the +90 / -90 object rotation is inert (R(2 phi) invariance)`` () =
        // Spec 0042 (007) acceptance, read through the naming template since spec 0050 S8.3. The label reads as the
        // CPL-CPL family, the QZ·LR product kind, and an object frame rotation phi = 90 deg.
        let reading = standardReading (MuellerExperimentLabel "CPL-(QZ#90-LR#90)-CPL")
        Assert.Equal(CplCpl, reading.family)
        Assert.Equal(QzLrProduct, reading.matrixKind)
        Assert.True(abs (reading.objectPhi.degrees - 90.0) < allowedDiff, $"phi = {reading.objectPhi.degrees}")

        // R(phi) depends on 2*phi, so +90 deg and -90 deg differ by a full 180 deg in 2*phi and are physically
        // identical: the effective source Stokes state and the effective analyzer row built at +90 MUST equal
        // those at -90 within allowedDiff. Non-symmetric base states (the 2*phi=+/-180 flip is observable).
        let sBase = StokesVector.create [ 1.0; 0.7; -0.4; 0.2 ]
        let aBase = RealVector4.create [ 0.5; 0.25; -0.3; 0.1 ]
        assertStokesClose (effectiveSource (Angle.degree 90.0<Berreman.Constants.deg>) sBase) (effectiveSource (Angle.degree (-90.0<Berreman.Constants.deg>)) sBase)
        assertVec4Close (effectiveAnalyzer (Angle.degree 90.0<Berreman.Constants.deg>) aBase) (effectiveAnalyzer (Angle.degree (-90.0<Berreman.Constants.deg>)) aBase)

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
        match freeCosineFit solver { anglesDeg = anglesDeg; norm = norm } with
        | Ok fit ->
            Assert.True(abs (fit.zeroAzimuth.degrees - knownZeroDeg) < 1e-6, $"zeroAzimuth = {fit.zeroAzimuth.degrees}")
            Assert.True(abs (fit.visibility.value - knownVisibility) < 1e-6, $"visibility = {fit.visibility.value}")
            Assert.True(fit.rmse.value < 1e-6, $"rmse = {fit.rmse.value}")
        | Error e -> Assert.Fail($"expected a CosineFit for the synthetic trace, got %A{e}")

    [<Fact>]
    member _.``Stage-1 re-derived calibration constants match the reference pipeline's full-precision values`` () =
        // Spec 0042 (008) acceptance, upgraded by spec 0044 §8.5: the constants RE-derived by stage one of the
        // staged Mueller path (spec 0050 S10.3) from the committed measured data are compared against the
        // REFERENCE PIPELINE's own output at full precision, not against the report's 2-decimal rounded values.
        //
        // The expected values are `MuellerReference`'s, transcribed from the reference pipeline's
        // reference_step1_summary.json, so the test reads NO file for its expectations (§8.5; spec 0050 S10.3).
        //
        // This fact no longer skips. The data is committed to the repository, so a load failure is a defect
        // (§8.4 / R6) and `loadDataSet` fails the test with the typed error.
        match calibrateStageOne (createMathNetSvd ()) (opticsMuellerCaptures ()) with
        | Error e -> Assert.Fail($"stage one of the staged Mueller path failed: %A{e}")
        | Ok stageOne ->
            // Every constant is compared in ONE assertion reporting all six deviations, so a regression message
            // shows which constants moved and by how much rather than stopping at the first one out of band.
            let constants =
                [ "dark_mean", stageOne.darkMean.value, MuellerReference.darkMean.value
                  "z_LP", stageOne.lpZero.degrees, MuellerReference.lpAnalyzerZero.degrees
                  "z_CPL", stageOne.cplZero.degrees, MuellerReference.cplAnalyzerZero.degrees
                  "δ_an", stageOne.cplAnalyzerRetardance.degrees, MuellerReference.cplAnalyzerRetardance.degrees
                  "θ_src", stageOne.cplSourceModel.thetaRel.degrees, MuellerReference.cplSourceAxisOffset.degrees
                  "δ_src", stageOne.cplSourceModel.retardance.degrees, MuellerReference.cplSourceRetardance.degrees ]
            let worst = constants |> List.map (fun (_, actual, expected) -> abs (actual - expected)) |> List.max
            Assert.True(
                worst < MuellerReference.stage1Tol,
                $"""Stage-1 constants (tol {MuellerReference.stage1Tol}): {String.concat "; " [ for (n, a, e) in constants -> $"{n} {a} vs {e} (Δ {abs (a - e)})" ]}""")

    [<Fact>]
    member _.``Stage-3 signal reduction pins darkSubtract, scalarGain, airIdentityPred, correctSignal and every per-family familyGain rule (always-run)`` () =
        // Spec 0042 (009) acceptance (always-run, deterministic — fixed-literal timestamps, no ambient clock):
        // pin the Stage-3 reduction primitives on known values, and familyGain firing each per-family rule.

        // scalarGain — a proportional pair signal = 2·model has best scalar gain exactly 2 …
        let modelValues = [| 1.0; 2.0; 3.0; 4.0 |]
        let model = modelValues |> Array.map DetectorIntensity
        let signalProp = modelValues |> Array.map (fun m -> MeasuredIntensity (2.0 * m))
        Assert.True(abs ((scalarGain signalProp model).value - 2.0) < allowedDiff, $"scalarGain (proportional) = {(scalarGain signalProp model).value}")
        // … and a general pair matches the closed form Σ(s·m)/Σ(m²).
        let signalGenValues = [| 3.0; 1.0; 4.0; 1.0 |]
        let signalGen = signalGenValues |> Array.map MeasuredIntensity
        let expectedGain = (Array.map2 (*) signalGenValues modelValues |> Array.sum) / (Array.map2 (*) modelValues modelValues |> Array.sum)
        Assert.True(abs ((scalarGain signalGen model).value - expectedGain) < allowedDiff, $"scalarGain (general) = {(scalarGain signalGen model).value}, expected {expectedGain}")

        // darkSubtract — avg_total − dark_mean, with the ≈ 869.666 measured dark mean passed in as data.
        let darkMean = DarkMean 869.666
        let avgTotal = MeasuredIntensity 1000.0
        Assert.True(abs ((darkSubtract darkMean avgTotal).value - (1000.0 - 869.666)) < allowedDiff, $"darkSubtract = {(darkSubtract darkMean avgTotal).value}")

        // correctSignal — (avg_total − dark_mean) / gain (dark-subtract composed with the gain divide).
        let gain = AirGain 2.0
        Assert.True(abs ((correctSignal darkMean avgTotal gain).value - ((1000.0 - 869.666) / 2.0)) < allowedDiff, $"correctSignal = {(correctSignal darkMean avgTotal gain).value}")

        // airIdentityPred — a·s, the effective-analyzer row dotted with the effective source Stokes state.
        let s = StokesVector.create [ 1.0; 0.5; -0.3; 0.2 ]
        let a = RealVector4.create [ 2.0; -1.0; 0.4; 0.8 ]
        let expectedDot = 2.0 * 1.0 + (-1.0) * 0.5 + 0.4 * (-0.3) + 0.8 * 0.2
        Assert.True(abs ((airIdentityPred s a).value - expectedDot) < allowedDiff, $"airIdentityPred = {(airIdentityPred s a).value}, expected {expectedDot}")

        // familyGain — one calibration bundle, then assert each per-family rule fires (fixed-literal times).
        let t0 = System.DateTimeOffset(2026, 7, 15, 12, 0, 0, System.TimeSpan.Zero)
        let cal =
            {
                lpLp = { gainA = AirGain 10.0; gainB = AirGain 20.0 }                            // mean → 15.0
                lpCpl = { gainA = AirGain 4.0; gainB = AirGain 6.0 }                             // mean → 5.0
                cplLp = { gainPre = AirGain 100.0; gainPost = AirGain 200.0; splitTime = t0 }    // split at t0
                cplCpl = Day2Single (AirGain 42.0)                                               // single AIR block → 42.0
            }

        // LP-LP / LP-CPL — the mean of the two AIR repeats (time-independent).
        Assert.True(abs ((familyGain LpLp cal t0).value - 15.0) < allowedDiff, $"familyGain LpLp = {(familyGain LpLp cal t0).value}")
        Assert.True(abs ((familyGain LpCpl cal t0).value - 5.0) < allowedDiff, $"familyGain LpCpl = {(familyGain LpCpl cal t0).value}")

        // CPL-LP — split at the BullshitCheck timestamp: strictly before → pre, at/after → post.
        let before = t0.AddMinutes(-5.0)
        let after = t0.AddMinutes(5.0)
        Assert.True(abs ((familyGain CplLp cal before).value - 100.0) < allowedDiff, $"familyGain CplLp (before) = {(familyGain CplLp cal before).value}")
        Assert.True(abs ((familyGain CplLp cal after).value - 200.0) < allowedDiff, $"familyGain CplLp (after) = {(familyGain CplLp cal after).value}")
        Assert.True(abs ((familyGain CplLp cal t0).value - 200.0) < allowedDiff, $"familyGain CplLp (at split) = {(familyGain CplLp cal t0).value}")

        // CPL-CPL day2 — the single AIR block's gain.
        Assert.True(abs ((familyGain CplCpl cal t0).value - 42.0) < allowedDiff, $"familyGain CplCpl (day2) = {(familyGain CplCpl cal t0).value}")

        // CPL-CPL day1 — time-interpolated between the two AIR blocks (10:00 → 14:00, gains 30 → 50).
        let tStart = System.DateTimeOffset(2026, 7, 15, 10, 0, 0, System.TimeSpan.Zero)
        let tEnd = System.DateTimeOffset(2026, 7, 15, 14, 0, 0, System.TimeSpan.Zero)
        let calDay1 = { cal with cplCpl = Day1Interp { gainStart = AirGain 30.0; timeStart = tStart; gainEnd = AirGain 50.0; timeEnd = tEnd } }
        Assert.True(abs ((familyGain CplCpl calDay1 t0).value - 40.0) < allowedDiff, $"familyGain CplCpl (day1 midpoint) = {(familyGain CplCpl calDay1 t0).value}")   // 12:00 → 0.5·30 + 0.5·50
        Assert.True(abs ((familyGain CplCpl calDay1 tStart).value - 30.0) < allowedDiff, $"familyGain CplCpl (day1 start) = {(familyGain CplCpl calDay1 tStart).value}")
        Assert.True(abs ((familyGain CplCpl calDay1 tEnd).value - 50.0) < allowedDiff, $"familyGain CplCpl (day1 end) = {(familyGain CplCpl calDay1 tEnd).value}")
        Assert.True(abs ((familyGain CplCpl calDay1 (tStart.AddHours(-1.0))).value - 30.0) < allowedDiff, $"familyGain CplCpl (day1 clamp below) = {(familyGain CplCpl calDay1 (tStart.AddHours(-1.0))).value}")
        Assert.True(abs ((familyGain CplCpl calDay1 (tEnd.AddHours(1.0))).value - 50.0) < allowedDiff, $"familyGain CplCpl (day1 clamp above) = {(familyGain CplCpl calDay1 (tEnd.AddHours(1.0))).value}")

    [<Fact>]
    member _.``reconstruct recovers a seeded MuellerMatrix from a synthetic full-rank design at rank 16 (always-run)`` () =
        // Spec 0042 (010) acceptance (always-run): a synthetic FULL-RANK design — 4 independent Stokes states ⊗
        // 4 independent analyzer rows = 16 linearly independent designRows (column rank 16) — with the target
        // seeded exactly as y = X·vec_F(M_seed). The unique least-squares solution un-vecs (column-major) back to
        // the seeded MuellerMatrix, so `reconstruct` MUST recover the seed within tolerance at rank 16. The seed
        // is ASYMMETRIC with distinct entries so a row/column-major transpose bug is observable.
        let seed =
            RealMatrix4x4.create
                [ [ 1.0;  2.0;  3.0;  4.0 ]
                  [ 5.0;  6.0;  7.0;  8.0 ]
                  [ 9.0; 10.0; 11.0; 12.0 ]
                  [ 13.0; 14.0; 15.0; 16.0 ] ]
            |> MuellerMatrix
        let (RealVector vSeedVector) = seed.vecColumnMajor
        let vSeed = vSeedVector.ToArray()
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
        let target = design |> Array.map (fun row -> Array.map2 (*) row.value vSeed |> Array.sum)

        match reconstruct (createMathNetSvd ()) { design = design; rhs = target } with
        | Ok recon ->
            // The observability check §7.2 asserts: full rank 16 and a ~zero residual.
            Assert.Equal(MatrixRank 16, recon.rank)
            Assert.True(recon.rmse.value < allowedDiff, $"rmse = {recon.rmse.value}")
            assertMuellerEqual seed recon.matrix
        | Error e -> Assert.Fail($"expected a full-rank reconstruction of the seed, got %A{e}")

    [<Fact>]
    member _.``cascadeProduct forms M_LR times M_QZ (beam hits QZ first) via the MuellerMatrix product operator (always-run)`` () =
        // Spec 0042 (010) acceptance (always-run): cascadeProduct mLr mQz MUST equal mLr * mQz — the QZ-first
        // cascade (light meets QZ then LR, so the combined Mueller product is M_LR · M_QZ), reusing the engine's
        // MuellerMatrix `*` operator. Non-commuting operands (rotation vs a general matrix) make the ordering
        // observable — a swapped product would differ.
        let mQz =
            RealMatrix4x4.create
                [ [ 1.0; 0.0; 0.0; 0.0 ]
                  [ 0.0; 0.6; 0.8; 0.0 ]
                  [ 0.0; -0.8; 0.6; 0.0 ]
                  [ 0.0; 0.0; 0.0; 1.0 ] ]
            |> MuellerMatrix
        let mLr =
            RealMatrix4x4.create
                [ [ 1.0; 0.2; -0.3; 0.4 ]
                  [ 0.5; 0.6; 0.7; -0.8 ]
                  [ -0.9; 1.0; 0.1; 0.2 ]
                  [ 0.3; -0.4; 0.5; 0.6 ] ]
            |> MuellerMatrix
        assertMuellerEqual (mLr * mQz) (cascadeProduct mLr mQz)

    [<Fact>]
    member _.``buildDesign keeps only the requested kind, dropping AIR rows and the excluded contaminated point (always-run)`` () =
        // Spec 0042 (010): buildDesign selects the science rows of the requested MatrixKind, drops AIR#0
        // calibration rows and the one contaminated point (LP-(LR#90)-CPL-2 / description 140 / capture_index
        // 17 — supplied as `MuellerReference.excludedPoints` per spec §0), and assembles one designRow + one
        // corrected signal per kept row via the injected per-row projection.
        // A stub per-row projection: unit effective states (one 16-wide designRow) and the row's avgTotal as the
        // corrected signal (no dark floor, unit gain), so the assembled target identifies exactly which rows
        // survived, in order.
        let prepareRow (r : MuellerRawRow) : StokesVector * RealVector4 * DetectorIntensity =
            StokesVector.create [ 1.0; 0.0; 0.0; 0.0 ],
            RealVector4.create [ 1.0; 0.0; 0.0; 0.0 ],
            correctSignal (DarkMean 0.0) r.avgTotal (AirGain 1.0)
        let t = System.DateTimeOffset(2026, 7, 15, 12, 0, 0, System.TimeSpan.Zero)
        let rows : MuellerRawRow list =
            [ { experiment = MuellerExperimentLabel "LP-(LR#90)-LP"; captureIndex = CaptureIndex 0; capturedAt = t; description = Angle.degree 10.0<Berreman.Constants.deg>; avgTotal = MeasuredIntensity 1.0 }        // LR — kept
              { experiment = MuellerExperimentLabel "LP-(AIR#0)-LP"; captureIndex = CaptureIndex 1; capturedAt = t; description = Angle.degree 20.0<Berreman.Constants.deg>; avgTotal = MeasuredIntensity 2.0 }         // AIR — dropped
              { experiment = MuellerExperimentLabel "LP-(QZ#0)-LP"; captureIndex = CaptureIndex 2; capturedAt = t; description = Angle.degree 30.0<Berreman.Constants.deg>; avgTotal = MeasuredIntensity 3.0 }          // QZ — wrong kind, dropped
              { experiment = MuellerExperimentLabel "LP-(LR#90)-CPL-2"; captureIndex = CaptureIndex 17; capturedAt = t; description = Angle.degree 140.0<Berreman.Constants.deg>; avgTotal = MeasuredIntensity 4.0 }    // contaminated — dropped
              { experiment = MuellerExperimentLabel "LP-(LR#0)-LP"; captureIndex = CaptureIndex 3; capturedAt = t; description = Angle.degree 40.0<Berreman.Constants.deg>; avgTotal = MeasuredIntensity 5.0 } ]        // LR — kept
        let system = buildDesign prepareRow MuellerReference.excludedPoints rows Lr
        // Only the two LR science rows survive (avgTotal 1.0 and 5.0), in order; each contributes one 16-wide row.
        Assert.Equal(2, system.design.Length)
        Assert.Equal(2, system.rhs.Length)
        Assert.Equal<float[]>([| 1.0; 5.0 |], system.rhs)
        Assert.Equal(16, system.design.[0].value.Length)

    [<Fact>]
    member _.``section 7.2 end-to-end: the Stage 1 to 2 to 3 pipeline reconstructs M_QZ, M_LR and M_{QZ+LR} at rank 16 and reproduces the reference pipeline exactly`` () =
        // Spec 0042 (011, WIRE) acceptance — the composition-root cross-check. Wire the REAL
        // createMathNetSvd () (Domain) proxy, load the optics-mueller kit's five science lab files + the
        // darkness and bullshit checks (spec 0050 S9.4), RE-derive the Stage-1 AIR calibration constants and
        // the Stage-2 per-family source/analyzer models through `calibrateStageOne` (spec 0050 S10.3), reduce
        // each science row to its corrected signal (Stage-3, matrix_fit_linear.py assign_gain_model), and
        // reconstruct M_QZ / M_LR / M_{QZ+LR} (solve_single_object).
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
        //          The expected values are `MuellerReference`'s, transcribed from
        //          reference_step2_linear_summary.json, so this test reads no file for its expectations and
        //          therefore knows nothing about where anything lives (spec 0050 S10.3).
        //
        // ---- Composition root: the REAL proxies (the WIRE deliverable). ----
        let solver = createMathNetSvd ()
        let captures = opticsMuellerCaptures ()

        let lpLpRows = captures.lpLpRows
        let lpCplRows = captures.lpCplRows
        let cplLpRows = captures.cplLpRows
        let cplCplDay1Rows = captures.cplCplDay1Rows
        let cplCplDay2Rows = captures.cplCplDay2Rows

        // ---- Stages 1 and 2: the staged Mueller path's AIR calibration and the models built from it (spec 0050 S10.3). ----
        let stageOne =
            match calibrateStageOne solver captures with
            | Ok calibration -> calibration
            | Error e -> failwith $"stage one of the staged Mueller path failed: %A{e}"
        let sourceLp = LpSource
        let sourceCpl = stageOne.cplSource
        let analyzerLp = stageOne.lpAnalyzer
        let analyzerCpl = stageOne.cplAnalyzer

        // ---- Stage-3 procedure constants supplied as DATA (spec section 0): dark mean + the CPL-LP split time. ----
        let darkMean = stageOne.darkMean
        // The CPL-LP AIR gain splits at the FIRST BullshitCheck timestamp (matrix_fit_linear.py:82 reads the
        // diagnostics[0] capture; the earliest bullshit-check capture is that same first check).
        let splitTime =
            match captures.bullshitCheckRows with
            | [] -> failwith "the bullshit-check data set carried no rows"
            | rows -> (rows |> List.minBy (fun r -> r.capturedAt)).capturedAt

        // ---- Stage 3 (reduction): the per-family AIR scalar gains (matrix_fit_linear.py assign_gain_model). ----
        let airRowsOf (rows : MuellerRawRow list) (experiment : MuellerExperimentLabel) : MuellerRawRow list =
            rows |> List.filter (fun r -> r.experiment = experiment)
        // The AIR#0 scalar gain fitting `signal_dark_sub ~ gain * (a_eff . s_eff)` over one AIR experiment's
        // rows (fit_air_gain + fit_scalar_gain). An AIR row carries no object (phi = 0), so its effective
        // states equal its base states, and `airIdentityPred base base` is the per-row identity prediction.
        let fitAirGain (source : SourceModel) (analyzer : AnalyzerModel) (rows : MuellerRawRow list) : AirGain =
            let sBase = source.baseStokes
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

        let day1Start = airRowsOf cplCplDay1Rows (MuellerExperimentLabel "CPL-(AIR#0)-CPL")
        let day1End = airRowsOf cplCplDay1Rows (MuellerExperimentLabel "CPL-(AIR#0)-CPL-BR")
        // The per-family AIR gain calibration bundle. LP-LP / LP-CPL take the mean of two AIR repeats; CPL-LP
        // splits at the bullshit-check time; CPL-CPL day 2 is one AIR block (the `cplCpl` default below).
        let calibration : GainCalibration =
            {
                lpLp =
                    {
                        gainA = fitAirGain sourceLp analyzerLp (airRowsOf lpLpRows (MuellerExperimentLabel "LP-(AIR#0)-LP-2"))
                        gainB = fitAirGain sourceLp analyzerLp (airRowsOf lpLpRows (MuellerExperimentLabel "LP-(AIR#0)-LP-2R"))
                    }
                lpCpl =
                    {
                        gainA = fitAirGain sourceLp analyzerCpl (airRowsOf lpCplRows (MuellerExperimentLabel "LP-(AIR#0)-CPL-2"))
                        gainB = fitAirGain sourceLp analyzerCpl (airRowsOf lpCplRows (MuellerExperimentLabel "LP-(AIR#0)-CPL-2R"))
                    }
                cplLp =
                    {
                        gainPre = fitAirGain sourceCpl analyzerLp (airRowsOf cplLpRows (MuellerExperimentLabel "CPL-(AIR#0)-LP-2"))
                        gainPost = fitAirGain sourceCpl analyzerLp (airRowsOf cplLpRows (MuellerExperimentLabel "CPL-(AIR#0)-LP-2R"))
                        splitTime = splitTime
                    }
                cplCpl = Day2Single (fitAirGain sourceCpl analyzerCpl (airRowsOf cplCplDay2Rows (MuellerExperimentLabel "CPL-(AIR#0)-CPL-2")))
            }
        // CPL-CPL day 1 is a distinct FILE with the time-INTERPOLATED rule (AIR start -> AIR end); day 2 keeps
        // the single-block rule in `calibration` above.
        let day1Cal =
            { calibration with
                cplCpl =
                    Day1Interp
                        {
                            gainStart = fitAirGain sourceCpl analyzerCpl day1Start
                            timeStart = meanTime day1Start
                            gainEnd = fitAirGain sourceCpl analyzerCpl day1End
                            timeEnd = meanTime day1End
                        } }

        // ---- The per-row Stage-2+3 projection each family closes over its models + gain (enrich_frame). ----
        let prepareRow (source : SourceModel) (analyzer : AnalyzerModel) (fam : Family) (cal : GainCalibration)
                       (r : MuellerRawRow) : StokesVector * RealVector4 * DetectorIntensity =
            let phi = (standardReading r.experiment).objectPhi
            let sEff = effectiveSource phi source.baseStokes
            let aEff = effectiveAnalyzer phi (analyzerBaseRow analyzer r.description)
            let signal = correctSignal darkMean r.avgTotal (familyGain fam cal r.capturedAt)
            sEff, aEff, signal

        // Every science file maps to exactly one family; CPL-CPL day1/day2 are distinct files with distinct
        // gain rules (interpolated vs single block), so each carries its own calibration.
        let families : (MuellerRawRow list * (MuellerRawRow -> StokesVector * RealVector4 * DetectorIntensity)) list =
            [ lpLpRows,       prepareRow sourceLp  analyzerLp  LpLp   calibration
              lpCplRows,      prepareRow sourceLp  analyzerCpl LpCpl  calibration
              cplLpRows,      prepareRow sourceCpl analyzerLp  CplLp  calibration
              cplCplDay1Rows, prepareRow sourceCpl analyzerCpl CplCpl day1Cal
              cplCplDay2Rows, prepareRow sourceCpl analyzerCpl CplCpl calibration ]

        // Assemble the full n*16 design + corrected-signal target for one kind across every family, then solve
        // through the real SVD proxy (buildDesign drops AIR rows and the excluded point per file).
        let reconstructKind (kind : MatrixKind) : ReconstructedMatrix =
            let parts = families |> List.map (fun (rows, prep) -> buildDesign prep MuellerReference.excludedPoints rows kind)
            let system =
                {
                    design = parts |> List.collect (fun p -> List.ofArray p.design) |> List.toArray
                    rhs = parts |> List.collect (fun p -> List.ofArray p.rhs) |> List.toArray
                }
            match reconstruct solver system with
            | Ok recon -> recon
            | Error e -> failwith $"the %A{kind} reconstruction failed: %A{e}"

        let mQz = reconstructKind Qz
        let mLr = reconstructKind Lr
        let mComb = reconstructKind QzLrProduct

        // ---- Section 7.2 acceptance: each solve is full rank 16, and each reconstructed matrix reproduces
        //      the reference pipeline's own value to `MuellerReference.dataMatrixTol` (spec 0044 §8.5). ----
        //
        // The expected matrices are `MuellerReference`'s `qz_matrix`, `lr_matrix` and `combined_matrix` from
        // reference_step2_linear_summary.json. They are the SAME numbers the report prints in §7.2, carried to
        // full precision instead of six decimals — so this
        // assertion is simultaneously the physics claim (the port reproduces the published result) and a
        // characterization test (nothing in the pipeline drifts numerically without a test failing).
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

        Assert.Equal(MatrixRank 16, mQz.rank)
        Assert.Equal(MatrixRank 16, mLr.rank)
        Assert.Equal(MatrixRank 16, mComb.rank)
        // One assertion over ALL THREE matrices, reporting every matrix's worst element so a regression run
        // shows the whole picture in a single message instead of stopping at the first element out of band.
        let matrixDeviations =
            [ describeDeviation "M_QZ" MuellerReference.qzMatrix mQz.matrix
              describeDeviation "M_LR" MuellerReference.lrMatrix mLr.matrix
              describeDeviation "M_{QZ+LR}" MuellerReference.combinedMatrix mComb.matrix ]
        let worstMatrixDeviation = matrixDeviations |> List.map fst |> List.max
        Assert.True(
            worstMatrixDeviation < MuellerReference.dataMatrixTol,
            $"""section 7.2 matrices (tol {MuellerReference.dataMatrixTol}): {String.concat "; " (matrixDeviations |> List.map snd)}""")

        // ---- Section 8 cascade identity: M_{QZ+LR} vs the QZ-first product M_LR . M_QZ. ----
        // The expected values are `MuellerReference`'s, from `combined_vs_product_matrix` in the same summary.
        let cascade = cascadeProduct mLr.matrix mQz.matrix
        let d = frobeniusDiff mComb.matrix cascade
        let worstMetricDeviation =
            [ abs (d.frobenius.value - MuellerReference.cascadeFrobenius.value)
              abs (d.meanAbs.value - MuellerReference.cascadeMeanAbs.value)
              abs (d.maxAbs.value - MuellerReference.cascadeMaxAbs.value) ]
            |> List.max
        Assert.True(
            worstMetricDeviation < MuellerReference.dataMatrixTol,
            $"cascade metrics: worst deviation {worstMetricDeviation} (frobenius = {d.frobenius.value}, meanAbs = {d.meanAbs.value}, maxAbs = {d.maxAbs.value}, tol {MuellerReference.dataMatrixTol})")

        // The two largest element differences both live in the 4th row (1-indexed) — at columns 4 and 2, each
        // ~0.268 and near-tied. (The slice text names (4,2) and (2,2) 1-indexed; the real data's runner-up to
        // (4,2) is (4,4), not (2,2): (2,2)~0.193 is only the 4th-largest. Followed the data per the base
        // protocol's skepticism rule — see the state-of-the-world Gotchas.)
        let topTwoDiffPositions =
            [ for i in 0 .. 3 do
                for j in 0 .. 3 ->
                    (MuellerRowIndex i, MuellerColumnIndex j), abs (mComb.matrix.element i j - cascade.element i j) ]
            |> List.sortByDescending snd
            |> List.truncate 2
            |> List.map fst
            |> Set.ofList
        Assert.True((topTwoDiffPositions = MuellerReference.cascadeLargestDeltaPositions), $"the two largest abs-delta positions (0-indexed) = %A{topTwoDiffPositions}")
