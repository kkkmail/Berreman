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
        member this.lpAnalyzer : AnalyzerModel =
            {
                kind = Library.IdealLinear
                zeroDial = this.lpZero
                dialSign = DialNegative
                thetaRelOpt = None
                retardanceOpt = None
            }

        /// `load_step1_models`' `analyzer_cpl`: an LP behind a retarder at +45°, its retardance negative (only the
        /// relative sign to the source is constrained, `matrix_glue.py:211`).
        member this.cplAnalyzer : AnalyzerModel =
            {
                kind = Library.IdealCircularLeft
                zeroDial = this.cplZero
                dialSign = DialPositive
                thetaRelOpt = Some (Angle.degree 45.0<Berreman.Constants.deg>)
                retardanceOpt = Some (Retardance.degree (- this.cplAnalyzerRetardance.degrees * 1.0<Berreman.Constants.deg>))
            }

        /// `load_step1_models`' `source_cpl`.
        member this.cplSource : SourceModel = CplSource (this.cplSourceModel.thetaRel, this.cplSourceModel.retardance)

    /// A family's AIR#0 normalized trace, the reference `load_air_traces` + `prepare_normalized_traces`
    /// (`matrix_step1_air_fit.py:78`, `:85`): per experiment, the dark-subtracted mean per dial reading over that
    /// experiment's mean signal.
    let private reduceAirTraces (darkMean : DarkMean) (rows : MuellerRawRow list) : AirTrace =
        let reduced =
            rows
            |> List.filter (fun r -> r.experiment.value.Contains("(AIR#0)"))
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

    /// Stage 1, the AIR calibration of `matrix_step1_air_fit.py` `main`: the dark mean, the LP-LP and LP-CPL free cosine
    /// fits and the CPL-LP LP-frame fit. The first failed solve answers its family's `AirCalibrationFitErr`.
    let calibrateStageOne (solver : MuellerSolverProxy) (captures : StagedMuellerCaptureData) : Result<StageOneCalibrationData, StagedMuellerError> =
        match captures.darknessRows with
        | [] -> Error (EmptyCaptureSetErr "the darkness checks hold no capture row, so stage one has no dark mean")
        | darknessRows ->
            let darkMean = DarkMean (darknessRows |> List.averageBy (fun r -> r.avgTotal.value))
            let airFit (family : Family) (rows : MuellerRawRow list) : Result<CosineFit, StagedMuellerError> =
                freeCosineFit solver (reduceAirTraces darkMean rows) |> Result.mapError (fun e -> AirCalibrationFitErr (family, e))

            airFit LpLp captures.lpLpRows
            |> Result.bind (fun lpLpFit ->
                airFit LpCpl captures.lpCplRows
                |> Result.bind (fun lpCplFit ->
                    lpFrameSourceModel solver (reduceAirTraces darkMean captures.cplLpRows) lpLpFit.zeroAzimuth
                    |> Result.mapError (fun e -> AirCalibrationFitErr (CplLp, e))
                    |> Result.map (fun cplSourceModel ->
                        {
                            darkMean = darkMean
                            lpZero = lpLpFit.zeroAzimuth
                            cplZero = lpCplFit.zeroAzimuth
                            cplAnalyzerRetardance = lpCplFit.visibility.retardance
                            cplSourceModel = cplSourceModel
                        })))

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

    /// An AIR block holding at least one capture row: the rows of its file whose experiment is its run.
    type private AirBlockRows =
        {
            firstRow : MuellerRawRow
            laterRows : MuellerRawRow list
        }

        member this.rows : MuellerRawRow list = this.firstRow :: this.laterRows

        /// `fit_air_gain`'s `air_rows["captured_at"].mean()`, relative to the first tick so the tick sum never
        /// overflows int64.
        member this.meanTime : System.DateTimeOffset =
            let t0 = this.firstRow.capturedAt.UtcTicks
            let meanDelta = this.rows |> List.averageBy (fun r -> float (r.capturedAt.UtcTicks - t0))
            System.DateTimeOffset(t0 + int64 (System.Math.Round meanDelta), System.TimeSpan.Zero)

    /// The AIR block `assign_gain_model` selects (`matrix_fit_linear.py:94-144`): the rows whose experiment, read as a
    /// name, is the run; none answers `EmptyCaptureSetErr` naming the run.
    let private airBlock (rows : MuellerRawRow list) (run : ExperimentName) : Result<AirBlockRows, StagedMuellerError> =
        match rows |> List.filter (fun r -> r.experiment.nameText.toExperimentName = Ok run) with
        | [] -> Error (EmptyCaptureSetErr $"the AIR block {run.value} holds no capture row, so its gain has no fit")
        | firstRow :: laterRows -> Ok { firstRow = firstRow; laterRows = laterRows }

    /// The AIR scalar gain fitting `signal_dark_sub ~ gain * (a_eff . s_eff)` over one block (`fit_air_gain` +
    /// `fit_scalar_gain`). An AIR row carries no object (phi = 0), so its effective states are its base states.
    let private fitAirGain (darkMean : DarkMean) (source : SourceModel) (analyzer : AnalyzerModel) (block : AirBlockRows) : AirGain =
        let sBase = source.baseStokes
        let preds = block.rows |> List.map (fun r -> airIdentityPred sBase (analyzerBaseRow analyzer r.description)) |> List.toArray
        let signals = block.rows |> List.map (fun r -> darkSubtract darkMean r.avgTotal) |> List.toArray
        scalarGain signals preds

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

    /// One AIR run's anchor: its block in `rows`, the gain `gainOf` fits over it and the block's mean capture time.
    let private airAnchor
        (gainOf : AirBlockRows -> AirGain)
        (rows : MuellerRawRow list)
        (run : ExperimentName)
        : Result<AirAnchorGain, StagedMuellerError> =
        airBlock rows run |> Result.map (fun block -> { run = run; gain = gainOf block; capturedAt = block.meanTime })

    /// The anchors of an AIR run and its repeat in one file, in that order.
    let private airAnchorPair
        (gainOf : AirBlockRows -> AirGain)
        (rows : MuellerRawRow list)
        (run : ExperimentName)
        (repeat : ExperimentName)
        : Result<AirAnchorGain list, StagedMuellerError> =
        airAnchor gainOf rows run
        |> Result.bind (fun first -> airAnchor gainOf rows repeat |> Result.map (fun second -> [ first; second ]))

    /// The nine AIR runs' anchors in the reference order, each fitted with its file's source and analyzer.
    let private airAnchors
        (stageOne : StageOneCalibrationData)
        (captures : StagedMuellerCaptureData)
        : Result<AirAnchorGain list, StagedMuellerError> =
        let lpGain = fitAirGain stageOne.darkMean LpSource
        let cplGain = fitAirGain stageOne.darkMean stageOne.cplSource

        airAnchorPair (lpGain stageOne.lpAnalyzer) captures.lpLpRows lpLpAir lpLpAirRepeat
        |> Result.bind (fun lpLp ->
            airAnchorPair (lpGain stageOne.cplAnalyzer) captures.lpCplRows lpCplAir lpCplAirRepeat
            |> Result.bind (fun lpCpl ->
                airAnchorPair (cplGain stageOne.lpAnalyzer) captures.cplLpRows cplLpAir cplLpAirRepeat
                |> Result.bind (fun cplLp ->
                    airAnchorPair (cplGain stageOne.cplAnalyzer) captures.cplCplDay1Rows cplCplDay1Opening cplCplDay1Closing
                    |> Result.bind (fun cplCplDay1 ->
                        airAnchor (cplGain stageOne.cplAnalyzer) captures.cplCplDay2Rows cplCplDay2Air
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

    /// A science row's effective source, effective analyzer and corrected signal (`enrich_frame`), divided by its
    /// session's gain rule at its capture time (spec 0050 S11.1). φ comes from the reading `buildDesign` kept the row
    /// by, so the label is read once (spec 0050 S10.3).
    let private prepareRow
        (darkMean : DarkMean)
        (source : SourceModel)
        (analyzer : AnalyzerModel)
        (ruleOf : MuellerRawRow -> SessionGainRule)
        (row : MuellerRawRow)
        (reading : NameReading)
        : StokesVector * RealVector4 * DetectorIntensity =
        let phi = reading.objectPhi
        let sEff = effectiveSource phi source.baseStokes
        let aEff = effectiveAnalyzer phi (analyzerBaseRow analyzer row.description)
        let signal = correctSignal darkMean row.avgTotal ((ruleOf row).gainAt row.capturedAt)
        sEff, aEff, signal

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

                        let families : (MuellerRawRow list * (MuellerRawRow -> NameReading -> StokesVector * RealVector4 * DetectorIntensity)) list =
                            [ captures.lpLpRows,       prepareRow darkMean LpSource           stageOne.lpAnalyzer  (fun _ -> rules.lpLp)
                              captures.lpCplRows,      prepareRow darkMean LpSource           stageOne.cplAnalyzer (fun _ -> rules.lpCpl)
                              captures.cplLpRows,      prepareRow darkMean stageOne.cplSource stageOne.lpAnalyzer  cplLpRuleOf
                              captures.cplCplDay1Rows, prepareRow darkMean stageOne.cplSource stageOne.cplAnalyzer (fun _ -> rules.cplCplDay1)
                              captures.cplCplDay2Rows, prepareRow darkMean stageOne.cplSource stageOne.cplAnalyzer (fun _ -> rules.cplCplDay2) ]

                        let systemOf (kind : MatrixKind) : LeastSquaresSystem =
                            let parts = families |> List.map (fun (rows, projection) -> buildDesign projection excluded rows kind)
                            {
                                design = parts |> List.map (fun p -> p.design) |> Array.concat
                                rhs = parts |> List.map (fun p -> p.rhs) |> Array.concat
                            }

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
