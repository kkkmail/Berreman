namespace OpticalConstructor.Optimization

open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Optimization.MuellerFitting

/// What a fit's covariance and residual Jacobian say about the uncertainty of its parameters and of their combinations.
/// Moved from `BerremanTests/InverseFitHarness.fs` by spec 0050 S10.1 for the Planner (001 §M).
module FitUncertainty =

    /// A weight vector over a fit's parameters, in its scaling's axis order.
    [<Softellect.Sys.NumericCarrier("a weight vector over a fit's parameters in its scaling's axis order (R3): read whole against the covariance and the residual Jacobian")>]
    type ParameterWeights =
        | ParameterWeights of float[]

        member this.value = let (ParameterWeights w) = this in w

    /// The variance a fit's covariance predicts for a linear combination of its parameters, in scaled units.
    type PredictedVariance =
        | PredictedVariance of double

        member this.value = let (PredictedVariance v) = this in v

    /// Each fitted parameter's standard error, converted out of the scaled space into the relative units an ensemble's
    /// scatter is quoted in: one fit's prediction about repeat experiments, in axis order.
    let predictedRelativeErrors
        (truth : 'Parameters)
        (fit : MuellerFitData<'Parameters>)
        (quality : FitQuality.FitReport)
        : (ParameterName * RelativeError) list =
        fit.scaling.axes
        |> List.mapi (fun i a ->
            a.name,
            quality.standardErrors.value.[i] * abs (a.read fit.scaling.scale) / abs (a.read truth) |> RelativeErr)

    /// `wᵀ C w` over the fit's covariance. Offered, not trusted: the difference of two strongly correlated parameters
    /// is a near-total cancellation in a covariance that has already spent precision inverting `JᵀJ`.
    let combinationVariance (quality : FitQuality.FitReport) (weights : ParameterWeights) : PredictedVariance =
        let w = weights.value
        let n = w.Length
        [ for i in 0 .. n - 1 do
            for j in 0 .. n - 1 -> w.[i] * quality.covariance.value.[i].[j] * w.[j] ]
        |> List.sum
        |> PredictedVariance

    /// The weights of `plus − minus` in a scaling's axis order (−1.0 when the two coincide), or `UnknownAxisErr`.
    let differenceWeights
        (scaling : ParameterScaling<'Parameters>)
        (plus : ParameterName)
        (minus : ParameterName)
        : Result<ParameterWeights, MuellerFitError> =
        let names = scaling.axes |> List.map (fun a -> a.name)
        match names |> List.tryFindIndex ((=) plus), names |> List.tryFindIndex ((=) minus) with
        | Some p, Some m ->
            Array.init scaling.dimension (fun i -> if i = m then -1.0 elif i = p then 1.0 else 0.0)
            |> ParameterWeights
            |> Ok
        | None, _ -> Error (UnknownAxisErr plus)
        | Some _, None -> Error (UnknownAxisErr minus)
