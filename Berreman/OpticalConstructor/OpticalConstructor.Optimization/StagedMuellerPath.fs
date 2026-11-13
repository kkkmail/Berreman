namespace OpticalConstructor.Optimization

open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.ItemModels
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
