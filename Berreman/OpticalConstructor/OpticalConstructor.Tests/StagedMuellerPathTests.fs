namespace OpticalConstructor.Tests

open Berreman.Geometry
open Berreman.Fields
open OpticalConstructor.Domain
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Optimization.StagedMuellerPath
open Xunit

/// Spec 0050 S10.3 — the staged Mueller path answers a typed error, never a throw, over fixed-literal capture rows and
/// the real MathNet SVD solver, and its product check reads a fixed two-row system. Spec 0050 S11.3 — the pieces a study
/// calls alone (stage one over given AIR rows, an AIR anchor, a row's projection) are the reference path's own.
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

    /// A science block in an AIR file: readings at 0°, 20°, …, 160° whose `avgTotal` is 7 + sin 2α, which stage one
    /// must not read.
    let private scienceRows (label : MuellerExperimentLabel) : MuellerRawRow list =
        [ for reading in 0 .. 8 ->
            let dial = Angle.degree (20.0<Berreman.Constants.deg> * float reading)
            let row : MuellerRawRow =
                {
                    experiment = label
                    captureIndex = CaptureIndex (reading + 9)
                    capturedAt = System.DateTimeOffset(2026, 7, 15, 13, 30, 0, System.TimeSpan.Zero)
                    description = dial
                    avgTotal = MeasuredIntensity (7.0 + sin (2.0 * dial.value))
                }
            row ]

    let private stokesComponents (StokesVector (RealVector4 v)) : float[] = [| for i in 0 .. 3 -> v.[i] |]

    let private rowComponents (a : RealVector4) : float[] = [| for i in 0 .. 3 -> a.[i] |]

    let private designRows (system : LeastSquaresSystem) : float[][] = system.design |> Array.map (fun row -> row.value)

    let private assertSameAnalyzer (expected : AnalyzerModel) (actual : AnalyzerModel) : unit =
        Assert.Equal<Library.PolarizerKind>(expected.kind, actual.kind)
        Assert.Equal<Angle>(expected.zeroDial, actual.zeroDial)
        Assert.Equal<ItemModels.DialSign>(expected.dialSign, actual.dialSign)
        Assert.Equal<Angle option>(expected.thetaRelOpt, actual.thetaRelOpt)
        Assert.Equal<Retardance option>(expected.retardanceOpt, actual.retardanceOpt)

    [<Fact>]
    let ``stage one over AIR rows given directly equals stage one over the files that hold them`` () =
        // Each family file holds a science block beside its AIR block, so the files' stage one must select the AIR rows.
        let lpLpAir = airRows (MuellerExperimentLabel "LP-(AIR#0)-LP-2")
        let lpCplAir = airRows (MuellerExperimentLabel "LP-(AIR#0)-CPL-2")
        let cplLpAir = airRows (MuellerExperimentLabel "CPL-(AIR#0)-LP-2")
        let darkness = [ darknessRow; { darknessRow with captureIndex = CaptureIndex 1; avgTotal = MeasuredIntensity 870.25 } ]
        let files =
            { noCaptures with
                lpLpRows = scienceRows (MuellerExperimentLabel "LP-(QZ#30)-LP-2") @ lpLpAir
                lpCplRows = lpCplAir @ scienceRows (MuellerExperimentLabel "LP-(LR#60)-CPL-2")
                cplLpRows = scienceRows (MuellerExperimentLabel "CPL-(QZ#30-LR#30)-LP-2") @ cplLpAir
                darknessRows = darkness }
        let given : StageOneAirRows =
            {
                darknessRows = darkness
                lpLpAirRows = lpLpAir
                lpCplAirRows = lpCplAir
                cplLpAirRows = cplLpAir
            }

        match calibrateStageOne (createMathNetSvd ()) files, calibrateFromAirRows (createMathNetSvd ()) given with
        | Ok viaFiles, Ok direct ->
            Assert.Equal<DarkMean>(viaFiles.darkMean, direct.darkMean)
            Assert.Equal<Angle>(viaFiles.lpZero, direct.lpZero)
            Assert.Equal<Angle>(viaFiles.cplZero, direct.cplZero)
            Assert.Equal<Retardance>(viaFiles.cplAnalyzerRetardance, direct.cplAnalyzerRetardance)
            Assert.Equal<Angle>(viaFiles.cplSourceModel.thetaRel, direct.cplSourceModel.thetaRel)
            Assert.Equal<Retardance>(viaFiles.cplSourceModel.retardance, direct.cplSourceModel.retardance)
        | Error error, _
        | _, Error error -> Assert.Fail($"expected both stage-one calibrations to succeed, got {error.describe}")

    [<Fact>]
    let ``an AIR anchor's gain scales its ideal prediction onto its dark-subtracted signal, at its rows' mean time`` () =
        let run = Experiments.ExperimentName "LP-(AIR#0)-LP-2"
        let analyzer = AnalyzerModel.linearAt Angle.zero ItemModels.DialPositive
        let rowAt (index : int) (dial : Angle) (capturedAt : System.DateTimeOffset) : MuellerRawRow =
            let ideal = airIdentityPred LpSource.baseStokes (analyzerBaseRow analyzer dial)
            {
                experiment = MuellerExperimentLabel "LP-(AIR#0)-LP-2"
                captureIndex = CaptureIndex index
                capturedAt = capturedAt
                description = dial
                avgTotal = MeasuredIntensity (3.0 * ideal.value + 5.0)
            }
        let rows =
            [ rowAt 0 Angle.zero (System.DateTimeOffset(2026, 7, 15, 10, 0, 0, System.TimeSpan.Zero))
              rowAt 1 (Angle.degree 40.0<Berreman.Constants.deg>) (System.DateTimeOffset(2026, 7, 15, 10, 2, 0, System.TimeSpan.Zero)) ]

        match airAnchorGainOf LpSource analyzer (DarkMean 5.0) run rows with
        | Ok anchor ->
            Assert.Equal<Experiments.ExperimentName>(run, anchor.run)
            Assert.Equal(3.0, anchor.gain.value, 1e-12)
            Assert.Equal<System.DateTimeOffset>(System.DateTimeOffset(2026, 7, 15, 10, 1, 0, System.TimeSpan.Zero), anchor.capturedAt)
        | Error error -> Assert.Fail($"expected the AIR anchor, got {error.describe}")

        match airAnchorGainOf LpSource analyzer (DarkMean 5.0) run [] with
        | Error (EmptyCaptureSetErr reason) -> Assert.Contains("LP-(AIR#0)-LP-2", reason)
        | Error error -> Assert.Fail($"expected Error (EmptyCaptureSetErr _), got {error.describe}")
        | Ok anchor -> Assert.Fail($"expected Error (EmptyCaptureSetErr _), got a gain of {anchor.gain.value}")

    [<Fact>]
    let ``a capture row is projected into its object frame and its signal corrected by the dark and the gain`` () =
        let source = CplSource (Angle.degree 20.0<Berreman.Constants.deg>, Retardance.degree 85.0<Berreman.Constants.deg>)
        let analyzer =
            AnalyzerModel.circularAt (Angle.degree 10.0<Berreman.Constants.deg>) ItemModels.DialPositive (Retardance.degree 88.0<Berreman.Constants.deg>)
        let phi = Angle.degree 30.0<Berreman.Constants.deg>
        let dark = DarkMean 5.0
        let gain = AirGain 2.0
        let row : MuellerRawRow =
            {
                experiment = MuellerExperimentLabel "CPL-(QZ#30)-CPL-2"
                captureIndex = CaptureIndex 3
                capturedAt = System.DateTimeOffset(2026, 7, 15, 11, 0, 0, System.TimeSpan.Zero)
                description = Angle.degree 50.0<Berreman.Constants.deg>
                avgTotal = MeasuredIntensity 105.0
            }

        let (s, a, signal) = projectCaptureRow source analyzer phi dark gain row

        Assert.Equal<float[]>(stokesComponents (effectiveSource phi source.baseStokes), stokesComponents s)
        Assert.Equal<float[]>(rowComponents (effectiveAnalyzer phi (analyzerBaseRow analyzer row.description)), rowComponents a)
        Assert.Equal<DetectorIntensity>(correctSignal dark row.avgTotal gain, signal)

        let laterRow : MuellerRawRow = { row with captureIndex = CaptureIndex 4; description = Angle.degree 120.0<Berreman.Constants.deg>; avgTotal = MeasuredIntensity 61.0 }
        let (s2, a2, signal2) = projectCaptureRow source analyzer (Angle.degree 75.0<Berreman.Constants.deg>) dark gain laterRow
        let system = LeastSquaresSystem.ofProjections [ (s, a, signal); (s2, a2, signal2) ]

        Assert.Equal<float[][]>([| (designRow s a).value; (designRow s2 a2).value |], designRows system)
        Assert.Equal<float[]>([| signal.value; signal2.value |], system.rhs)

        // Stacked in list order: the later projection's system first.
        let stacked = LeastSquaresSystem.concat [ LeastSquaresSystem.ofProjections [ (s2, a2, signal2) ]; LeastSquaresSystem.ofProjections [ (s, a, signal) ] ]

        Assert.Equal<float[][]>([| (designRow s2 a2).value; (designRow s a).value |], designRows stacked)
        Assert.Equal<float[]>([| signal2.value; signal.value |], stacked.rhs)

    [<Fact>]
    let ``the stage-one analyzer and source models are the shared calibrated models of its constants`` () =
        let stageOne : StageOneCalibrationData =
            {
                darkMean = DarkMean 869.5
                lpZero = Angle.degree 12.0<Berreman.Constants.deg>
                cplZero = Angle.degree 47.0<Berreman.Constants.deg>
                cplAnalyzerRetardance = Retardance.degree 86.0<Berreman.Constants.deg>
                cplSourceModel = { thetaRel = Angle.degree 39.5<Berreman.Constants.deg>; retardance = Retardance.degree 91.0<Berreman.Constants.deg> }
            }
        let linear = AnalyzerModel.linearAt stageOne.lpZero ItemModels.DialNegative
        let circular = AnalyzerModel.circularAt stageOne.cplZero ItemModels.DialPositive stageOne.cplAnalyzerRetardance

        assertSameAnalyzer linear stageOne.lpAnalyzer
        assertSameAnalyzer circular stageOne.cplAnalyzer

        // The shared models are `load_step1_models`' analyzers: a bare linear one, and an LP behind a retarder at +45° whose
        // phase is the negated stage-one retardance.
        let loadedLinear : AnalyzerModel =
            {
                kind = Library.IdealLinear
                zeroDial = stageOne.lpZero
                dialSign = ItemModels.DialNegative
                thetaRelOpt = None
                retardanceOpt = None
            }
        let loadedCircular : AnalyzerModel =
            {
                kind = Library.IdealCircularLeft
                zeroDial = stageOne.cplZero
                dialSign = ItemModels.DialPositive
                thetaRelOpt = Some (Angle.degree 45.0<Berreman.Constants.deg>)
                retardanceOpt = Some (Retardance.degree (- stageOne.cplAnalyzerRetardance.degrees * 1.0<Berreman.Constants.deg>))
            }

        assertSameAnalyzer loadedLinear linear
        assertSameAnalyzer loadedCircular circular
        Assert.Equal<SourceModel>(CplSource (stageOne.cplSourceModel.thetaRel, stageOne.cplSourceModel.retardance), stageOne.cplSource)

    [<Fact>]
    let ``every staged path error renders its condition and its payload`` () =
        let singular = SingularDesignErr "x"
        let rankDeficient = RankDeficientErr (MatrixRank 7)
        let refused = Studies.MissingAnchorGainErr "'LP-(AIR#0)-LP-2R' is not among the 1 AIR gains given"
        let emptyReason = "the darkness checks hold no capture row"
        let rendered : (StagedMuellerError * string) list =
            [ (EmptyCaptureSetErr emptyReason, $"a set of capture rows the staged path needs is empty: {emptyReason}")
              (AirCalibrationFitErr (LpCpl, singular), "the AIR cosine fit of LP-CPL failed: the design is singular: x")
              (ObjectReconstructionErr (QzLrProduct, rankDeficient), "the reconstruction of QZ+LR failed: the design is rank-deficient: rank 7")
              (SessionGainErr refused, $"a session gain model was refused: {refused.describe}") ]

        for (error, text) in rendered do
            Assert.Equal(text, error.describe)
        Assert.Contains("'LP-(AIR#0)-LP-2R'", (SessionGainErr refused).describe)
        Assert.Equal<int>(rendered.Length, rendered |> List.map (fun (error, _) -> error.describe) |> List.distinct |> List.length)

        Assert.Equal<string list>([ "LP-LP"; "LP-CPL"; "CPL-LP"; "CPL-CPL" ], [ LpLp; LpCpl; CplLp; CplCpl ] |> List.map (fun family -> family.text))
        Assert.Equal<string list>([ "AIR"; "QZ"; "LR"; "QZ+LR" ], [ Air; Qz; Lr; QzLrProduct ] |> List.map (fun kind -> kind.text))
        Assert.Equal("the design is singular: x", singular.describe)
        Assert.Equal("the design is rank-deficient: rank 7", rankDeficient.describe)
