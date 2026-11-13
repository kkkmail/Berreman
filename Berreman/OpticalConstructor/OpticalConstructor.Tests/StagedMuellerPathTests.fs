namespace OpticalConstructor.Tests

open Berreman.Geometry
open Berreman.Fields
open OpticalConstructor.Domain
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Optimization.StagedMuellerPath
open Xunit

/// Spec 0050 S10.3 — the staged Mueller path answers a typed error, never a throw, over fixed-literal capture rows and
/// the real MathNet SVD solver, and its product check reads a fixed two-row system.
module StagedMuellerPathTests =

    let private noCaptures : StagedMuellerCaptureData =
        {
            lpLpRows = []
            lpCplRows = []
            cplLpRows = []
            cplCplDay1Rows = []
            cplCplDay2Rows = []
            darknessRows = []
            bullshitCheckRows = []
        }

    let private darknessRow : MuellerRawRow =
        {
            experiment = MuellerExperimentLabel "DarknessCheck"
            captureIndex = CaptureIndex 0
            capturedAt = System.DateTimeOffset(2026, 7, 15, 12, 0, 0, System.TimeSpan.Zero)
            description = Angle.zero
            avgTotal = MeasuredIntensity 869.5
        }

    let private bullshitCheckRow : MuellerRawRow =
        {
            experiment = MuellerExperimentLabel "BullshitCheck"
            captureIndex = CaptureIndex 1
            capturedAt = System.DateTimeOffset(2026, 7, 15, 14, 0, 0, System.TimeSpan.Zero)
            description = Angle.degree 60.0<Berreman.Constants.deg>
            avgTotal = MeasuredIntensity 24344.0
        }

    /// One AIR block: nine readings at 0°, 20°, …, 160° whose `avgTotal` is 2 + cos 2α, so its cosine fits solve.
    let private airRows (label : MuellerExperimentLabel) : MuellerRawRow list =
        [ for reading in 0 .. 8 ->
            let dial = Angle.degree (20.0<Berreman.Constants.deg> * float reading)
            {
                experiment = label
                captureIndex = CaptureIndex reading
                capturedAt = System.DateTimeOffset(2026, 7, 15, 13, 0, 0, System.TimeSpan.Zero)
                description = dial
                avgTotal = MeasuredIntensity (2.0 + cos (2.0 * dial.value))
            } ]

    /// Stage one's AIR files hold their first block only, and nothing else is missing before the AIR gains.
    let private firstBlocksOnly : StagedMuellerCaptureData =
        { noCaptures with
            lpLpRows = airRows (MuellerExperimentLabel "LP-(AIR#0)-LP-2")
            lpCplRows = airRows (MuellerExperimentLabel "LP-(AIR#0)-CPL-2")
            cplLpRows = airRows (MuellerExperimentLabel "CPL-(AIR#0)-LP-2")
            darknessRows = [ darknessRow ]
            bullshitCheckRows = [ bullshitCheckRow ] }

    [<Fact>]
    let ``stage one answers a typed error naming the empty darkness checks, never a throw`` () =
        match calibrateStageOne (createMathNetSvd ()) noCaptures with
        | Error (EmptyCaptureSetErr reason) -> Assert.Contains("darkness checks", reason)
        | other -> Assert.Fail($"expected Error (EmptyCaptureSetErr _), got %A{other}")

    [<Fact>]
    let ``stage one answers a typed error naming the family whose AIR cosine fit fails`` () =
        // No LP-LP row reduces to an empty AIR trace, an empty design the real solver refuses as singular.
        match calibrateStageOne (createMathNetSvd ()) { noCaptures with darknessRows = [ darknessRow ] } with
        | Error (AirCalibrationFitErr (LpLp, SingularDesignErr _)) -> ()
        | other -> Assert.Fail($"expected Error (AirCalibrationFitErr (LpLp, SingularDesignErr _)), got %A{other}")

    [<Fact>]
    let ``the staged path answers a typed error naming an AIR block that holds no capture row`` () =
        match reconstructStaged (createMathNetSvd ()) [] firstBlocksOnly with
        | Error (EmptyCaptureSetErr reason) -> Assert.Contains("LP-(AIR#0)-LP-2R", reason)
        | other -> Assert.Fail($"expected Error (EmptyCaptureSetErr _) naming LP-(AIR#0)-LP-2R, got %A{other}")

    [<Fact>]
    let ``the staged path answers a typed error naming the object whose solve fails`` () =
        // All nine AIR blocks and no science row: the empty QZ design reaches the real solver, which refuses it.
        let allAirBlocks =
            { firstBlocksOnly with
                lpLpRows = firstBlocksOnly.lpLpRows @ airRows (MuellerExperimentLabel "LP-(AIR#0)-LP-2R")
                lpCplRows = firstBlocksOnly.lpCplRows @ airRows (MuellerExperimentLabel "LP-(AIR#0)-CPL-2R")
                cplLpRows = firstBlocksOnly.cplLpRows @ airRows (MuellerExperimentLabel "CPL-(AIR#0)-LP-2R")
                cplCplDay1Rows = airRows (MuellerExperimentLabel "CPL-(AIR#0)-CPL") @ airRows (MuellerExperimentLabel "CPL-(AIR#0)-CPL-BR")
                cplCplDay2Rows = airRows (MuellerExperimentLabel "CPL-(AIR#0)-CPL-2") }

        match reconstructStaged (createMathNetSvd ()) [] allAirBlocks with
        | Error (ObjectReconstructionErr (Qz, SingularDesignErr _)) -> ()
        | other -> Assert.Fail($"expected Error (ObjectReconstructionErr (Qz, SingularDesignErr _)), got %A{other}")

    [<Fact>]
    let ``the product check compares the combined matrix with the cascade and the RMSEs over the combined rows`` () =
        // The two design rows read M₀₀ and M₁₁ (`kron4`): the cascade predicts 1.0 and 0.5 against the rows' 1.0 and 0.8.
        let qz =
            RealMatrix4x4.create
                [ [ 1.0; 0.0; 0.0; 0.0 ]
                  [ 0.0; 0.5; 0.0; 0.0 ]
                  [ 0.0; 0.0; 0.5; 0.0 ]
                  [ 0.0; 0.0; 0.0; 1.0 ] ]
            |> MuellerMatrix
        let combined : ReconstructedMatrix = { matrix = Propagation.identityMueller; rank = MatrixRank 16; rmse = ResidualRmse 0.1 }
        let combinedSystem : LeastSquaresSystem =
            {
                design =
                    [| kron4 (StokesVector.create [ 1.0; 0.0; 0.0; 0.0 ]) (RealVector4.create [ 1.0; 0.0; 0.0; 0.0 ])
                       kron4 (StokesVector.create [ 0.0; 1.0; 0.0; 0.0 ]) (RealVector4.create [ 0.0; 1.0; 0.0; 0.0 ]) |]
                rhs = [| 1.0; 0.8 |]
            }

        let check = productCheckOf Propagation.identityMueller qz combined combinedSystem

        for i in 0 .. 3 do
            for j in 0 .. 3 do
                Assert.Equal(qz.element i j, check.cascade.element i j, 1e-12)
        Assert.Equal(0.5, check.difference.maxAbs.value, 1e-12)
        Assert.Equal(sqrt 0.5, check.difference.frobenius.value, 1e-12)
        Assert.Equal(0.1, check.fitRmse.value, 1e-12)
        Assert.Equal(sqrt 0.045, check.productRmse.value, 1e-12)
        Assert.Equal(sqrt 0.045 / 0.1, check.rmseRatio.value, 1e-12)
