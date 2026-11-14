namespace OpticalConstructor.Optimization

open Berreman.Geometry
open Berreman.Fields
open OpticalConstructor.Domain
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.NamingTemplates
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Studies

/// The optics-mueller reference pipeline (spec 0042 §7.2) as the study's staged Mueller path: a pure function of the lab
/// files' capture rows and a `MuellerSolverProxy`, promoted from `MuellerReconstructionTests` by spec 0050 S10.3.
module StagedMuellerPath =

    /// The capture rows of the seven lab files as the Phone ROI CSV profile reads them (spec 0050 S9.2), the
    /// `pd.read_csv` inputs of `matrix_step1_air_fit.py` `main` and `matrix_fit_linear.py` `main`.
    type StagedMuellerCaptureData =
        {
            lpLpRows : MuellerRawRow list
            lpCplRows : MuellerRawRow list
            cplLpRows : MuellerRawRow list
            cplCplDay1Rows : MuellerRawRow list
            cplCplDay2Rows : MuellerRawRow list
            darknessRows : MuellerRawRow list
            bullshitCheckRows : MuellerRawRow list
        }

    /// A typed failure of the staged path, never a throw.
    type StagedMuellerError =
        | EmptyCaptureSetErr of reason : string
        | AirCalibrationFitErr of family : Family * error : SolverError
        | ObjectReconstructionErr of kind : MatrixKind * error : SolverError
        /// A session's gain model the AIR gains cannot resolve (spec 0050 S11.1).
        | SessionGainErr of error : StudyError

        /// The failed condition and its payload (spec 0050 S11.3).
        member this.describe : string =
            match this with
            | EmptyCaptureSetErr reason -> $"a set of capture rows the staged path needs is empty: {reason}"
            | AirCalibrationFitErr (family, error) -> $"the AIR cosine fit of {family.text} failed: {error.describe}"
            | ObjectReconstructionErr (kind, error) -> $"the reconstruction of {kind.text} failed: {error.describe}"
            | SessionGainErr error -> $"a session gain model was refused: {error.describe}"

    /// The stage-1 constants of `matrix_step1_air_fit.py` `main`, with the stage-2 models `matrix_glue.py`
    /// `load_step1_models` builds from them as members.
    type StageOneCalibrationData =
        {
            darkMean : DarkMean
            lpZero : Angle
            cplZero : Angle
            cplAnalyzerRetardance : Retardance
            cplSourceModel : SourceCplModel
        }

        /// `load_step1_models`' `analyzer_lp`: an ideal linear analyzer read on a negative dial from `lpZero`.
        member this.lpAnalyzer : AnalyzerModel = AnalyzerModel.linearAt this.lpZero DialNegative

        /// `load_step1_models`' `analyzer_cpl`: the circular analyzer of `cplZero` and `cplAnalyzerRetardance`, read on a
        /// positive dial.
        member this.cplAnalyzer : AnalyzerModel = AnalyzerModel.circularAt this.cplZero DialPositive this.cplAnalyzerRetardance

        /// `load_step1_models`' `source_cpl`.
        member this.cplSource : SourceModel = CplSource (this.cplSourceModel.thetaRel, this.cplSourceModel.retardance)

    /// A family's AIR#0 normalized trace over its AIR rows, the reference `load_air_traces` + `prepare_normalized_traces`
    /// (`matrix_step1_air_fit.py:78`, `:85`): per experiment, the dark-subtracted mean per dial reading over that
    /// experiment's mean signal.
    let private reduceAirTraces (darkMean : DarkMean) (rows : MuellerRawRow list) : AirTrace =
        let reduced =
            rows
            |> List.groupBy (fun r -> r.experiment)
            |> List.collect (fun (_, expRows) ->
                let byDescription =
                    expRows
                    |> List.groupBy (fun r -> r.description.degrees)
                    |> List.map (fun (descDeg, g) -> descDeg, (g |> List.averageBy (fun r -> r.avgTotal.value)) - darkMean.value)
                    |> List.sortBy fst
                let meanSignal = byDescription |> List.averageBy snd
                byDescription |> List.map (fun (descDeg, signal) -> descDeg, signal / meanSignal))
        {
            anglesDeg = reduced |> List.map fst |> List.toArray
            norm = reduced |> List.map snd |> List.toArray
        }

    /// The CPL source model from the reference `fit_lp_frame_coeffs` (`matrix_step1_air_fit.py:132`), the 2-column
    /// solve phased on the LP zero, and `derive_source_cpl_from_lp_frame` (`sourceCplFromLpFrame`).
    let private lpFrameSourceModel (solver : MuellerSolverProxy) (trace : AirTrace) (zero : Angle) : Result<SourceCplModel, SolverError> =
        let zeroDeg = zero.degrees
        let design =
            trace.anglesDeg
            |> Array.map (fun a ->
                let twoAlpha = 2.0 * (a - zeroDeg) * Angle.radiansPerDegree
                DesignRow [| cos twoAlpha; sin twoAlpha |])
        let rhs = trace.norm |> Array.map (fun n -> n - 1.0)
        solver.solveLinearLeastSquares { design = design; rhs = rhs }
        |> Result.map (fun sol ->
            let qLp = CosineCoefficient sol.solution.[0]
            let uLp = CosineCoefficient sol.solution.[1]
            sourceCplFromLpFrame qLp uLp)

    /// The rows stage one reads: the darkness checks and the AIR#0 rows of the LP-LP, LP-CPL and CPL-LP families, as the
    /// caller selected them (spec 0050 S11.3).
    type StageOneAirRows =
        {
            darknessRows : MuellerRawRow list
            lpLpAirRows : MuellerRawRow list
            lpCplAirRows : MuellerRawRow list
            cplLpAirRows : MuellerRawRow list
        }

    /// Stage 1, the AIR calibration of `matrix_step1_air_fit.py` `main` over the given rows: the dark mean, the LP-LP and
    /// LP-CPL free cosine fits and the CPL-LP LP-frame fit. The first failed solve answers its family's
    /// `AirCalibrationFitErr` (spec 0050 S11.3).
    let calibrateFromAirRows (solver : MuellerSolverProxy) (rows : StageOneAirRows) : Result<StageOneCalibrationData, StagedMuellerError> =
        match rows.darknessRows with
        | [] -> Error (EmptyCaptureSetErr "the darkness checks hold no capture row, so stage one has no dark mean")
        | darknessRows ->
            let darkMean = DarkMean (darknessRows |> List.averageBy (fun r -> r.avgTotal.value))
            let airFit (family : Family) (airRows : MuellerRawRow list) : Result<CosineFit, StagedMuellerError> =
                freeCosineFit solver (reduceAirTraces darkMean airRows) |> Result.mapError (fun e -> AirCalibrationFitErr (family, e))

            airFit LpLp rows.lpLpAirRows
            |> Result.bind (fun lpLpFit ->
                airFit LpCpl rows.lpCplAirRows
                |> Result.bind (fun lpCplFit ->
                    lpFrameSourceModel solver (reduceAirTraces darkMean rows.cplLpAirRows) lpLpFit.zeroAzimuth
                    |> Result.mapError (fun e -> AirCalibrationFitErr (CplLp, e))
                    |> Result.map (fun cplSourceModel ->
                        {
                            darkMean = darkMean
                            lpZero = lpLpFit.zeroAzimuth
                            cplZero = lpCplFit.zeroAzimuth
                            cplAnalyzerRetardance = lpCplFit.visibility.retardance
                            cplSourceModel = cplSourceModel
                        })))

    /// Stage 1 over the lab files: `calibrateFromAirRows` of the darkness checks and each family file's rows whose label
    /// holds `(AIR#0)` (spec 0050 S11.3).
    let calibrateStageOne (solver : MuellerSolverProxy) (captures : StagedMuellerCaptureData) : Result<StageOneCalibrationData, StagedMuellerError> =
        let airRowsOf (rows : MuellerRawRow list) : MuellerRawRow list =
            rows |> List.filter (fun r -> r.experiment.value.Contains("(AIR#0)"))

        calibrateFromAirRows
            solver
            {
                darknessRows = captures.darknessRows
                lpLpAirRows = airRowsOf captures.lpLpRows
                lpCplAirRows = airRowsOf captures.lpCplRows
                cplLpAirRows = airRowsOf captures.cplLpRows
            }

    /// The cascade's prediction RMSE over the combined fit's own, `product / fit` (`matrix_fit_linear.py:334`).
    type ProductRmseRatio =
        | ProductRmseRatio of float

        member this.value = let (ProductRmseRatio r) = this in r

        /// The one place the ratio is formed.
        static member between (productRmse : ResidualRmse) (fitRmse : ResidualRmse) : ProductRmseRatio =
            productRmse.value / fitRmse.value |> ProductRmseRatio

    /// The product check for the series objects (`matrix_fit_linear.py:358-373`): the cascade M_LR · M_QZ, its difference
    /// from M_{QZ+LR}, and the combined fit's RMSE and the cascade's, both over the combined rows.
    type ProductCheckData =
        {
            cascade : MuellerMatrix
            difference : MatrixDiff
            fitRmse : ResidualRmse
            productRmse : ResidualRmse
        }

        /// `product_rmse / fit_rmse`, the ratio the reference report prints.
        member this.rmseRatio : ProductRmseRatio = ProductRmseRatio.between this.productRmse this.fitRmse

    /// The product check of `combined`, solved over `combinedSystem`, against the cascade of `qz` then `lr`.
    let productCheckOf
        (lr : MuellerMatrix)
        (qz : MuellerMatrix)
        (combined : ReconstructedMatrix)
        (combinedSystem : LeastSquaresSystem)
        : ProductCheckData =
        let cascade = cascadeProduct lr qz
        {
            cascade = cascade
            difference = frobeniusDiff combined.matrix cascade
            fitRmse = combined.rmse
            productRmse = combinedSystem.residualRmseAt cascade.vecColumnMajor
        }

    /// The staged path's answer: the stage-1 constants, the nine AIR runs' gains in the reference order (LP-LP, LP-CPL
    /// and CPL-LP each `-2` then `-2R`, then `CPL-(AIR#0)-CPL`, `-CPL-BR` and `-CPL-2`, spec 0050 S11.1), M_QZ, M_LR and
    /// M_{QZ+LR} with their rank and residual, and the product check.
    type StagedMuellerData =
        {
            stageOne : StageOneCalibrationData
            airGains : AirAnchorGain list
            qz : ReconstructedMatrix
            lr : ReconstructedMatrix
            qzLr : ReconstructedMatrix
            productCheck : ProductCheckData
        }

    /// An AIR run's anchor over its rows, each with the source state and analyzer row it is predicted by at φ = 0
    /// (`fit_air_gain` + `fit_scalar_gain`): the scalar gain of `signal_dark_sub ~ gain * (a . s)`, at the rows' mean
    /// capture time; no row answers `EmptyCaptureSetErr` naming the run (spec 0050 S11.3).
    let airAnchorGainOver
        (dark : DarkMean)
        (run : ExperimentName)
        (projections : (StokesVector * RealVector4 * MuellerRawRow) list)
        : Result<AirAnchorGain, StagedMuellerError> =
        match projections with
        | [] -> Error (EmptyCaptureSetErr $"the AIR block {run.value} holds no capture row, so its gain has no fit")
        | (_, _, firstRow) :: _ ->
            let preds = projections |> List.map (fun (s, a, _) -> airIdentityPred s a) |> List.toArray
            let signals = projections |> List.map (fun (_, _, r) -> darkSubtract dark r.avgTotal) |> List.toArray

            // The mean is taken relative to the first tick so the tick sum never overflows int64.
            let t0 = firstRow.capturedAt.UtcTicks
            let meanDelta = projections |> List.averageBy (fun (_, _, r) -> float (r.capturedAt.UtcTicks - t0))

            Ok
                {
                    run = run
                    gain = scalarGain signals preds
                    capturedAt = System.DateTimeOffset(t0 + int64 (System.Math.Round meanDelta), System.TimeSpan.Zero)
                }

    /// The AIR run's anchor over its rows (`fit_air_gain` + `fit_scalar_gain`), each predicted by the family's source
    /// and the analyzer's row at its dial (spec 0050 S11.3).
    let airAnchorGainOf
        (source : SourceModel)
        (analyzer : AnalyzerModel)
        (dark : DarkMean)
        (run : ExperimentName)
        (rows : MuellerRawRow list)
        : Result<AirAnchorGain, StagedMuellerError> =
        airAnchorGainOver dark run (rows |> List.map (fun r -> source.baseStokes, analyzerBaseRow analyzer r.description, r))

    /// A science row's effective source and analyzer in its object frame φ, from the state reaching the object and the
    /// detector's S₀ row past it, and its signal dark-subtracted and divided by `gain` (`enrich_frame`; spec 0050 S11.3).
    let projectCaptureRowFrom
        (arriving : StokesVector)
        (analyzing : RealVector4)
        (phi : Angle)
        (dark : DarkMean)
        (gain : AirGain)
        (row : MuellerRawRow)
        : StokesVector * RealVector4 * DetectorIntensity =
        let sEff = effectiveSource phi arriving
        let aEff = effectiveAnalyzer phi analyzing
        let signal = correctSignal dark row.avgTotal gain
        sEff, aEff, signal

    /// A science row's effective source and analyzer in its object frame φ and its signal dark-subtracted and divided by
    /// `gain` (`enrich_frame`; spec 0050 S11.3).
    let projectCaptureRow
        (source : SourceModel)
        (analyzer : AnalyzerModel)
        (phi : Angle)
        (dark : DarkMean)
        (gain : AirGain)
        (row : MuellerRawRow)
        : StokesVector * RealVector4 * DetectorIntensity =
        projectCaptureRowFrom source.baseStokes (analyzerBaseRow analyzer row.description) phi dark gain row

    // The reference procedure's nine AIR runs: in a kit they are experiments of these names, which a gain model anchors
    // to (spec 0050 S11.1; 001 §L).
    let private lpLpAir = ExperimentName "LP-(AIR#0)-LP-2"
    let private lpLpAirRepeat = ExperimentName "LP-(AIR#0)-LP-2R"
    let private lpCplAir = ExperimentName "LP-(AIR#0)-CPL-2"
    let private lpCplAirRepeat = ExperimentName "LP-(AIR#0)-CPL-2R"
    let private cplLpAir = ExperimentName "CPL-(AIR#0)-LP-2"
    let private cplLpAirRepeat = ExperimentName "CPL-(AIR#0)-LP-2R"
    let private cplCplDay1Opening = ExperimentName "CPL-(AIR#0)-CPL"
    let private cplCplDay1Closing = ExperimentName "CPL-(AIR#0)-CPL-BR"
    let private cplCplDay2Air = ExperimentName "CPL-(AIR#0)-CPL-2"

    // The six sessions' gain models, `assign_gain_model`'s rules: the CPL-LP file is two sessions split at the bullshit
    // check, and the CPL-CPL days are two files (spec 0050 S11.1).
    let private lpLpGainModel = AnchoredSessionGain [ lpLpAir; lpLpAirRepeat ]
    let private lpCplGainModel = AnchoredSessionGain [ lpCplAir; lpCplAirRepeat ]
    let private cplLpPreCheckGainModel = AnchoredSessionGain [ cplLpAir ]
    let private cplLpPostCheckGainModel = AnchoredSessionGain [ cplLpAirRepeat ]
    let private cplCplDay1GainModel = InterpolatedSessionGain { opening = cplCplDay1Opening; closing = cplCplDay1Closing }
    let private cplCplDay2GainModel = AnchoredSessionGain [ cplCplDay2Air ]

    /// One AIR run's anchor through `anchorOf`, over the block `assign_gain_model` selects (`matrix_fit_linear.py:94-144`):
    /// the rows of `rows` whose experiment, read as a name, is the run.
    let private airAnchor
        (anchorOf : ExperimentName -> MuellerRawRow list -> Result<AirAnchorGain, StagedMuellerError>)
        (rows : MuellerRawRow list)
        (run : ExperimentName)
        : Result<AirAnchorGain, StagedMuellerError> =
        anchorOf run (rows |> List.filter (fun r -> r.experiment.nameText.toExperimentName = Ok run))

    /// The anchors of an AIR run and its repeat in one file, in that order.
    let private airAnchorPair
        (anchorOf : ExperimentName -> MuellerRawRow list -> Result<AirAnchorGain, StagedMuellerError>)
        (rows : MuellerRawRow list)
        (run : ExperimentName)
        (repeat : ExperimentName)
        : Result<AirAnchorGain list, StagedMuellerError> =
        airAnchor anchorOf rows run
        |> Result.bind (fun first -> airAnchor anchorOf rows repeat |> Result.map (fun second -> [ first; second ]))

    /// The nine AIR runs' anchors in the reference order, each fitted with its file's source and analyzer.
    let private airAnchors
        (stageOne : StageOneCalibrationData)
        (captures : StagedMuellerCaptureData)
        : Result<AirAnchorGain list, StagedMuellerError> =
        let lpAnchor (analyzer : AnalyzerModel) = airAnchorGainOf LpSource analyzer stageOne.darkMean
        let cplAnchor (analyzer : AnalyzerModel) = airAnchorGainOf stageOne.cplSource analyzer stageOne.darkMean

        airAnchorPair (lpAnchor stageOne.lpAnalyzer) captures.lpLpRows lpLpAir lpLpAirRepeat
        |> Result.bind (fun lpLp ->
            airAnchorPair (lpAnchor stageOne.cplAnalyzer) captures.lpCplRows lpCplAir lpCplAirRepeat
            |> Result.bind (fun lpCpl ->
                airAnchorPair (cplAnchor stageOne.lpAnalyzer) captures.cplLpRows cplLpAir cplLpAirRepeat
                |> Result.bind (fun cplLp ->
                    airAnchorPair (cplAnchor stageOne.cplAnalyzer) captures.cplCplDay1Rows cplCplDay1Opening cplCplDay1Closing
                    |> Result.bind (fun cplCplDay1 ->
                        airAnchor (cplAnchor stageOne.cplAnalyzer) captures.cplCplDay2Rows cplCplDay2Air
                        |> Result.map (fun cplCplDay2 -> lpLp @ lpCpl @ cplLp @ cplCplDay1 @ [ cplCplDay2 ])))))

    /// The rules the six sessions' gain models resolve to.
    type private StagedSessionRules =
        {
            lpLp : SessionGainRule
            lpCpl : SessionGainRule
            cplLpPreSplit : SessionGainRule
            cplLpPostSplit : SessionGainRule
            cplCplDay1 : SessionGainRule
            cplCplDay2 : SessionGainRule
        }

    /// Each session's gain model through `ruleWith` over the AIR anchors; the first refused answers `SessionGainErr`.
    let private sessionRules (anchors : AirAnchorGain list) : Result<StagedSessionRules, StagedMuellerError> =
        let ruleOf (model : SessionGainModel) : Result<SessionGainRule, StagedMuellerError> =
            model.ruleWith anchors |> Result.mapError SessionGainErr

        ruleOf lpLpGainModel
        |> Result.bind (fun lpLp ->
            ruleOf lpCplGainModel
            |> Result.bind (fun lpCpl ->
                ruleOf cplLpPreCheckGainModel
                |> Result.bind (fun cplLpPreSplit ->
                    ruleOf cplLpPostCheckGainModel
                    |> Result.bind (fun cplLpPostSplit ->
                        ruleOf cplCplDay1GainModel
                        |> Result.bind (fun cplCplDay1 ->
                            ruleOf cplCplDay2GainModel
                            |> Result.map (fun cplCplDay2 ->
                                {
                                    lpLp = lpLp
                                    lpCpl = lpCpl
                                    cplLpPreSplit = cplLpPreSplit
                                    cplLpPostSplit = cplLpPostSplit
                                    cplCplDay1 = cplCplDay1
                                    cplCplDay2 = cplCplDay2
                                }))))))

    /// Stages 2 and 3 after `calibrateStageOne` (`matrix_fit_linear.py` `main`): the nine AIR gains and the sessions' gain
    /// rules, then M_QZ, M_LR and M_{QZ+LR}, each the five files' designs over `excluded` solved by `reconstruct`, then
    /// the product check. The first missing capture set, refused gain model or failed solve answers its typed error.
    let reconstructStaged
        (solver : MuellerSolverProxy)
        (excluded : ExcludedPoint list)
        (captures : StagedMuellerCaptureData)
        : Result<StagedMuellerData, StagedMuellerError> =
        calibrateStageOne solver captures
        |> Result.bind (fun stageOne ->
            match captures.bullshitCheckRows with
            | [] -> Error (EmptyCaptureSetErr "the bullshit checks hold no capture row, so the CPL-LP gain has no split time")
            | checkRows ->
                // `assign_gain_model` reads the first check; the earliest capture is that same check.
                let splitTime = (checkRows |> List.minBy (fun r -> r.capturedAt)).capturedAt

                airAnchors stageOne captures
                |> Result.bind (fun anchors ->
                    sessionRules anchors
                    |> Result.bind (fun rules ->
                        let darkMean = stageOne.darkMean

                        // A CPL-LP row captured strictly before the split is the pre-split session's (`assign_gain_model`).
                        let cplLpRuleOf (row : MuellerRawRow) : SessionGainRule =
                            if row.capturedAt < splitTime then rules.cplLpPreSplit else rules.cplLpPostSplit

                        // φ comes from the reading `buildDesign` kept the row by, so the label is read once (spec 0050
                        // S10.3); the gain is the row's session rule at its capture time (spec 0050 S11.1).
                        let projectionOf
                            (source : SourceModel)
                            (analyzer : AnalyzerModel)
                            (ruleOf : MuellerRawRow -> SessionGainRule)
                            (row : MuellerRawRow)
                            (reading : NameReading)
                            : StokesVector * RealVector4 * DetectorIntensity =
                            projectCaptureRow source analyzer reading.objectPhi darkMean ((ruleOf row).gainAt row.capturedAt) row

                        let families : (MuellerRawRow list * (MuellerRawRow -> NameReading -> StokesVector * RealVector4 * DetectorIntensity)) list =
                            [ captures.lpLpRows,       projectionOf LpSource           stageOne.lpAnalyzer  (fun _ -> rules.lpLp)
                              captures.lpCplRows,      projectionOf LpSource           stageOne.cplAnalyzer (fun _ -> rules.lpCpl)
                              captures.cplLpRows,      projectionOf stageOne.cplSource stageOne.lpAnalyzer  cplLpRuleOf
                              captures.cplCplDay1Rows, projectionOf stageOne.cplSource stageOne.cplAnalyzer (fun _ -> rules.cplCplDay1)
                              captures.cplCplDay2Rows, projectionOf stageOne.cplSource stageOne.cplAnalyzer (fun _ -> rules.cplCplDay2) ]

                        let systemOf (kind : MatrixKind) : LeastSquaresSystem =
                            families
                            |> List.map (fun (rows, projection) -> buildDesign projection excluded rows kind)
                            |> LeastSquaresSystem.concat

                        let reconstructObject (kind : MatrixKind) (system : LeastSquaresSystem) : Result<ReconstructedMatrix, StagedMuellerError> =
                            reconstruct solver system |> Result.mapError (fun error -> ObjectReconstructionErr (kind, error))

                        reconstructObject Qz (systemOf Qz)
                        |> Result.bind (fun qz ->
                            reconstructObject Lr (systemOf Lr)
                            |> Result.bind (fun lr ->
                                // The product check reads the combined rows the M_{QZ+LR} solve fitted.
                                let combinedSystem = systemOf QzLrProduct

                                reconstructObject QzLrProduct combinedSystem
                                |> Result.map (fun qzLr ->
                                    {
                                        stageOne = stageOne
                                        airGains = anchors
                                        qz = qz
                                        lr = lr
                                        qzLr = qzLr
                                        productCheck = productCheckOf lr.matrix qz.matrix qzLr combinedSystem
                                    }))))))
