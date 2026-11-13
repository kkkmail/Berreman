namespace OpticalConstructor.Tests

open Berreman.Geometry
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Optimization.StagedMuellerPath
open Xunit

/// Spec 0050 S10.3 — stage one of the staged Mueller path answers a typed error, never a throw, over fixed-literal
/// capture rows and the real MathNet SVD solver.
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
