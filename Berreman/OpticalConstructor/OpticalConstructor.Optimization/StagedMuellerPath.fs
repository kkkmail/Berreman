namespace OpticalConstructor.Optimization

open Berreman.Geometry
open Berreman.Fields
open OpticalConstructor.Domain
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.NamingTemplates
open OpticalConstructor.Domain.MuellerReconstruction

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

    /// The staged path's answer: the stage-1 constants, the AIR gain rules (`cplCpl` the day-2 block's `Day2Single`),
    /// the day-1 CPL-CPL interpolation, and M_QZ, M_LR and M_{QZ+LR} with their rank and residual.
    type StagedMuellerData =
        {
            stageOne : StageOneCalibrationData
            gains : GainCalibration
            cplCplDay1Gain : InterpolatedGain
            qz : ReconstructedMatrix
            lr : ReconstructedMatrix
            qzLr : ReconstructedMatrix
        }

    /// An AIR block holding at least one capture row: the rows of its file whose experiment is its label.
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

    /// The AIR block `assign_gain_model` selects by label (`matrix_fit_linear.py:94-144`); none answers
    /// `EmptyCaptureSetErr` naming the label.
    let private airBlock (rows : MuellerRawRow list) (label : MuellerExperimentLabel) : Result<AirBlockRows, StagedMuellerError> =
        match rows |> List.filter (fun r -> r.experiment = label) with
        | [] -> Error (EmptyCaptureSetErr $"the AIR block {label.value} holds no capture row, so its gain has no fit")
        | firstRow :: laterRows -> Ok { firstRow = firstRow; laterRows = laterRows }

    /// The AIR scalar gain fitting `signal_dark_sub ~ gain * (a_eff . s_eff)` over one block (`fit_air_gain` +
    /// `fit_scalar_gain`). An AIR row carries no object (phi = 0), so its effective states are its base states.
    let private fitAirGain (darkMean : DarkMean) (source : SourceModel) (analyzer : AnalyzerModel) (block : AirBlockRows) : AirGain =
        let sBase = source.baseStokes
        let preds = block.rows |> List.map (fun r -> airIdentityPred sBase (analyzerBaseRow analyzer r.description)) |> List.toArray
        let signals = block.rows |> List.map (fun r -> darkSubtract darkMean r.avgTotal) |> List.toArray
        scalarGain signals preds

    /// LP-LP and LP-CPL take the mean of two AIR repeats of their file.
    let private repeatPairGain
        (gainOf : AirBlockRows -> AirGain)
        (rows : MuellerRawRow list)
        (labelA : MuellerExperimentLabel)
        (labelB : MuellerExperimentLabel)
        : Result<RepeatPairGain, StagedMuellerError> =
        airBlock rows labelA
        |> Result.bind (fun blockA ->
            airBlock rows labelB |> Result.map (fun blockB -> { gainA = gainOf blockA; gainB = gainOf blockB }))

    /// CPL-LP splits at the bullshit-check time.
    let private splitAirGain
        (gainOf : AirBlockRows -> AirGain)
        (rows : MuellerRawRow list)
        (labelPre : MuellerExperimentLabel)
        (labelPost : MuellerExperimentLabel)
        (splitTime : System.DateTimeOffset)
        : Result<SplitGain, StagedMuellerError> =
        airBlock rows labelPre
        |> Result.bind (fun blockPre ->
            airBlock rows labelPost
            |> Result.map (fun blockPost -> { gainPre = gainOf blockPre; gainPost = gainOf blockPost; splitTime = splitTime }))

    /// CPL-CPL day 1 time-interpolates from its opening AIR block to its closing one.
    let private interpolatedAirGain
        (gainOf : AirBlockRows -> AirGain)
        (rows : MuellerRawRow list)
        (labelStart : MuellerExperimentLabel)
        (labelEnd : MuellerExperimentLabel)
        : Result<InterpolatedGain, StagedMuellerError> =
        airBlock rows labelStart
        |> Result.bind (fun blockStart ->
            airBlock rows labelEnd
            |> Result.map (fun blockEnd ->
                {
                    gainStart = gainOf blockStart
                    timeStart = blockStart.meanTime
                    gainEnd = gainOf blockEnd
                    timeEnd = blockEnd.meanTime
                }))

    /// The stage-2 AIR gain rules (`assign_gain_model`): the bundle whose `cplCpl` is the day-2 block's, and the day-1
    /// CPL-CPL interpolation, each block in the reference's order. The nine labels are the reference procedure's own,
    /// literals here until spec 0050 S11.1 turns them into session data.
    let private airGainRules
        (stageOne : StageOneCalibrationData)
        (splitTime : System.DateTimeOffset)
        (captures : StagedMuellerCaptureData)
        : Result<GainCalibration * InterpolatedGain, StagedMuellerError> =
        let lpGain = fitAirGain stageOne.darkMean LpSource
        let cplGain = fitAirGain stageOne.darkMean stageOne.cplSource

        repeatPairGain (lpGain stageOne.lpAnalyzer) captures.lpLpRows
            (MuellerExperimentLabel "LP-(AIR#0)-LP-2") (MuellerExperimentLabel "LP-(AIR#0)-LP-2R")
        |> Result.bind (fun lpLp ->
            repeatPairGain (lpGain stageOne.cplAnalyzer) captures.lpCplRows
                (MuellerExperimentLabel "LP-(AIR#0)-CPL-2") (MuellerExperimentLabel "LP-(AIR#0)-CPL-2R")
            |> Result.bind (fun lpCpl ->
                splitAirGain (cplGain stageOne.lpAnalyzer) captures.cplLpRows
                    (MuellerExperimentLabel "CPL-(AIR#0)-LP-2") (MuellerExperimentLabel "CPL-(AIR#0)-LP-2R") splitTime
                |> Result.bind (fun cplLp ->
                    interpolatedAirGain (cplGain stageOne.cplAnalyzer) captures.cplCplDay1Rows
                        (MuellerExperimentLabel "CPL-(AIR#0)-CPL") (MuellerExperimentLabel "CPL-(AIR#0)-CPL-BR")
                    |> Result.bind (fun cplCplDay1Gain ->
                        airBlock captures.cplCplDay2Rows (MuellerExperimentLabel "CPL-(AIR#0)-CPL-2")
                        |> Result.map (fun day2Block ->
                            let gains : GainCalibration =
                                {
                                    lpLp = lpLp
                                    lpCpl = lpCpl
                                    cplLp = cplLp
                                    cplCpl = Day2Single (cplGain stageOne.cplAnalyzer day2Block)
                                }

                            gains, cplCplDay1Gain)))))

    /// A science row's effective source, effective analyzer and corrected signal (`enrich_frame`). φ comes from the
    /// reading `buildDesign` kept the row by, so the label is read once (spec 0050 S10.3).
    let private prepareRow
        (darkMean : DarkMean)
        (source : SourceModel)
        (analyzer : AnalyzerModel)
        (family : Family)
        (gains : GainCalibration)
        (row : MuellerRawRow)
        (reading : NameReading)
        : StokesVector * RealVector4 * DetectorIntensity =
        let phi = reading.objectPhi
        let sEff = effectiveSource phi source.baseStokes
        let aEff = effectiveAnalyzer phi (analyzerBaseRow analyzer row.description)
        let signal = correctSignal darkMean row.avgTotal (familyGain family gains row.capturedAt)
        sEff, aEff, signal

    /// Stages 2 and 3 after `calibrateStageOne` (`matrix_fit_linear.py` `main`): the AIR gain rules, then M_QZ, M_LR
    /// and M_{QZ+LR}, each the five families' designs over `excluded` solved by `reconstruct`. The first missing
    /// capture set or failed solve answers its typed error.
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

                airGainRules stageOne splitTime captures
                |> Result.bind (fun (gains, cplCplDay1Gain) ->
                    let darkMean = stageOne.darkMean
                    let day1Gains = { gains with cplCpl = Day1Interp cplCplDay1Gain }

                    // Every science file is one family; the CPL-CPL days are two files with two gain rules.
                    let families : (MuellerRawRow list * (MuellerRawRow -> NameReading -> StokesVector * RealVector4 * DetectorIntensity)) list =
                        [ captures.lpLpRows,       prepareRow darkMean LpSource           stageOne.lpAnalyzer  LpLp   gains
                          captures.lpCplRows,      prepareRow darkMean LpSource           stageOne.cplAnalyzer LpCpl  gains
                          captures.cplLpRows,      prepareRow darkMean stageOne.cplSource stageOne.lpAnalyzer  CplLp  gains
                          captures.cplCplDay1Rows, prepareRow darkMean stageOne.cplSource stageOne.cplAnalyzer CplCpl day1Gains
                          captures.cplCplDay2Rows, prepareRow darkMean stageOne.cplSource stageOne.cplAnalyzer CplCpl gains ]

                    let reconstructObject (kind : MatrixKind) : Result<ReconstructedMatrix, StagedMuellerError> =
                        let parts = families |> List.map (fun (rows, projection) -> buildDesign projection excluded rows kind)
                        let system : LeastSquaresSystem =
                            {
                                design = parts |> List.map (fun p -> p.design) |> Array.concat
                                rhs = parts |> List.map (fun p -> p.rhs) |> Array.concat
                            }

                        reconstruct solver system |> Result.mapError (fun error -> ObjectReconstructionErr (kind, error))

                    reconstructObject Qz
                    |> Result.bind (fun qz ->
                        reconstructObject Lr
                        |> Result.bind (fun lr ->
                            reconstructObject QzLrProduct
                            |> Result.map (fun qzLr ->
                                {
                                    stageOne = stageOne
                                    gains = gains
                                    cplCplDay1Gain = cplCplDay1Gain
                                    qz = qz
                                    lr = lr
                                    qzLr = qzLr
                                })))))
