namespace OpticalConstructor.Tests

open Xunit
open Berreman.MaterialProperties
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.MeasurementPlanner
open OpticalConstructor.Optimization
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.FitUncertainty
open OpticalConstructor.Optimization.PlannerInformation
open CrystalMaterialFixtures

/// Spec 0050 S14.2 — the Measurement Planner's Fisher information over Jacobians written as literals.
module PlannerInformationTests =

    let private a : ParameterName = ParameterName "a"

    let private b : ParameterName = ParameterName "b"

    let private c : ParameterName = ParameterName "c"

    /// The information of `candidates` over `coordinates`, failing on a refusal.
    let private informationOver (coordinates : ParameterName list) (candidates : CandidateRows list) : PlannerInformation =
        match PlannerInformation.over coordinates candidates with
        | Ok information -> information
        | Error error -> failwith $"Expected the candidates' information, got: {error.describe}"

    /// The refusal of `candidates` over `coordinates`, failing on an answer.
    let private refusalOver (coordinates : ParameterName list) (candidates : CandidateRows list) : PlannerError =
        match PlannerInformation.over coordinates candidates with
        | Error error -> error
        | Ok _ -> failwith "Expected a refusal, got the candidates' information."

    /// The scaled sigmas of `information`, failing where it has none.
    let private scaledSigmasOf (information : PlannerInformation) : float[] =
        match information.scaledSigmasOpt with
        | Some (DesignVector sigmas) -> sigmas
        | None -> failwith "Expected the Cramer-Rao bound at full rank, got none."

    let private assertClose (tolerance : float) (expected : float[]) (actual : float[]) : unit =
        Assert.Equal(expected.Length, actual.Length)
        Array.iter2 (fun (e : float) (x : float) -> Assert.Equal(e, x, tolerance)) expected actual

    [<Fact>]
    let ``the Cramer-Rao bound is the covariance a fit reports when its residual level is the noise`` () : unit =
        let axes : ParameterAxis<UniaxialParameters> list = UniaxialParameters.axes |> List.take 2
        let coordinates : ParameterName list = axes |> List.map (fun (axis : ParameterAxis<UniaxialParameters>) -> axis.name)
        Assert.Equal<ParameterName list>([ ParameterName "n_o"; ParameterName "n_e" ], coordinates)

        // JᵀJ = [2 1; 1 5], so (JᵀJ)⁻¹ = [5 −1; −1 2] / 9, and σₖ = √(0.04 · (JᵀJ)⁻¹ₖₖ).
        let expected : float[] = [| 0.2 * sqrt (5.0 / 9.0); 0.2 * sqrt (2.0 / 9.0) |]

        let single : CandidateRows =
            {
                jacobian = ResidualJacobian [| [| 1.0; 0.0 |]; [| 1.0; 1.0 |]; [| 0.0; 2.0 |] |]
                noise = NoiseVariance 0.04
            }

        let fromSingle = informationOver coordinates [ single ]
        assertClose 1e-12 expected (scaledSigmasOf fromSingle)

        // The same rows as two candidates: the last two doubled at four times the noise carry the same information.
        let first : CandidateRows = { jacobian = ResidualJacobian [| [| 1.0; 0.0 |] |]; noise = NoiseVariance 0.04 }

        let doubled : CandidateRows =
            {
                jacobian = ResidualJacobian [| [| 2.0; 2.0 |]; [| 0.0; 4.0 |] |]
                noise = NoiseVariance 0.16
            }

        let fromTwo = informationOver coordinates [ first; doubled ]
        assertClose 1e-12 expected (scaledSigmasOf fromTwo)
        Assert.Equal(NoiseVariance 0.04, fromTwo.noise)

        let scaling : ParameterScaling<UniaxialParameters> =
            {
                axes = axes
                centre = uniaxialParameters
                scale =
                    { uniaxialParameters with
                        ordinaryIndex = RefractionIndex 1.0e-3
                        extraordinaryIndex = RefractionIndex 1.0e-3
                    }
            }

        match fromSingle.boundsOver scaling with
        | Some bounds ->
            Assert.Equal<ParameterName list>(coordinates, bounds |> List.map (fun (bound : TargetBound) -> bound.name))

            assertClose
                1e-15
                (expected |> Array.map (fun (sigma : float) -> sigma * 1.0e-3))
                (bounds |> List.map (fun (bound : TargetBound) -> bound.sigma.value) |> Array.ofList)
        | None -> failwith "Expected a bound per target at full rank, got none."

    [<Fact>]
    let ``two coordinates the candidates move only together are one confounded combination`` () : unit =
        let coordinates : ParameterName list = [ a; b; c ]

        let candidate : CandidateRows =
            {
                jacobian = ResidualJacobian [| [| 1.0; 1.0; 0.0 |]; [| 2.0; 2.0; 1.0 |] |]
                noise = NoiseVariance 1.0
            }

        let information = informationOver coordinates [ candidate ]
        Assert.Equal(MatrixRank 2, information.rank)
        Assert.Equal(None, information.conditionOpt)
        Assert.Equal(None, information.scaledSigmasOpt)
        Assert.Equal(None, information.traceOpt)

        let scaling : ParameterScaling<UniaxialParameters> =
            {
                axes = UniaxialParameters.axes |> List.take 3
                centre = uniaxialParameters
                scale = uniaxialParameters
            }

        Assert.Equal(None, information.boundsOver scaling)

        match information.confounded with
        | [ direction ] ->
            let (ParameterWeights weights) = direction.weights
            let half = 1.0 / sqrt 2.0
            let sign = if weights.[0] > 0.0 then 1.0 else -1.0
            assertClose 1e-12 [| sign * half; -sign * half; 0.0 |] weights
            Assert.Equal(0.0, direction.singularValue.value, 1e-12)
            Assert.Contains(direction.text coordinates, [ CombinationText "+0.71 a -0.71 b"; CombinationText "-0.71 a +0.71 b" ])
        | directions -> failwith $"Expected one confounded combination, got {directions.Length}."

        match information.weakestOpt with
        | Some weakest ->
            let singular = weakest.singularValue.value
            Assert.True(singular > 0.0, $"the weakest identifiable combination's singular value is {singular}")
        | None -> failwith "Expected a weakest identifiable combination at rank 2, got none."

    [<Fact>]
    let ``with no candidate nothing is identifiable, and a noiseless or misshapen candidate is refused`` () : unit =
        let coordinates : ParameterName list = [ a; b; c ]
        let empty = informationOver coordinates []
        Assert.Equal(MatrixRank 0, empty.rank)
        Assert.Equal(NoiseVariance 1.0, empty.noise)
        Assert.Equal(0.0, empty.logDeterminant.value)
        Assert.Equal(None, empty.weakestOpt)

        let identity : float[][] = [| [| 1.0; 0.0; 0.0 |]; [| 0.0; 1.0; 0.0 |]; [| 0.0; 0.0; 1.0 |] |]

        Assert.Equal<float[] list>(
            List.ofArray identity,
            empty.confounded |> List.map (fun (direction : SingularDirection) -> direction.weights.value))

        let valid : CandidateRows =
            {
                jacobian = ResidualJacobian [| [| 1.0; 0.0; 0.0 |] |]
                noise = NoiseVariance 1.0
            }

        let noiseless : CandidateRows = { valid with noise = NoiseVariance 0.0 }

        match refusalOver coordinates [ noiseless ] with
        | NoiselessPlannerCandidateErr reason as error ->
            Assert.Contains("candidate 1", reason)
            Assert.StartsWith("the candidate's noise variance is not above zero: ", error.describe)
        | error -> failwith $"Expected a noiseless candidate, got: {error.describe}"

        // Nothing is answered in part: a refusal after a valid candidate still refuses the whole.
        match refusalOver coordinates [ valid; noiseless ] with
        | NoiselessPlannerCandidateErr reason -> Assert.Contains("candidate 2", reason)
        | error -> failwith $"Expected a noiseless second candidate, got: {error.describe}"

        let misshapen : CandidateRows =
            {
                jacobian = ResidualJacobian [| [| 1.0; 0.0 |]; [| 0.0; 1.0 |] |]
                noise = NoiseVariance 1.0
            }

        match refusalOver coordinates [ misshapen ] with
        | PlannerShapeErr reason as error ->
            Assert.Contains("candidate 1", reason)
            Assert.Contains("2 entries", reason)
            Assert.Contains("3 targets", reason)
            Assert.StartsWith("the candidate's Jacobian does not fit the targets: ", error.describe)
        | error -> failwith $"Expected a misshapen candidate, got: {error.describe}"

    [<Fact>]
    let ``the log determinant counts each identifiable combination, and the trace sums the targets' variances`` () : unit =
        let coordinates : ParameterName list = [ a; b ]
        let rows : ResidualJacobian = ResidualJacobian [| [| 2.0; 0.0 |]; [| 0.0; 3.0 |] |]
        let unitNoise = informationOver coordinates [ { jacobian = rows; noise = NoiseVariance 1.0 } ]
        Assert.Equal(2.0 * log 2.0 + 2.0 * log 3.0, unitNoise.logDeterminant.value, 1e-12)

        match unitNoise.traceOpt with
        | Some trace -> Assert.Equal(1.0 / 4.0 + 1.0 / 9.0, trace.value, 1e-12)
        | None -> failwith "Expected the covariance trace at full rank, got none."

        let fourfold = informationOver coordinates [ { jacobian = rows; noise = NoiseVariance 4.0 } ]
        Assert.Equal(2.0 * log 2.0 + 2.0 * log 3.0 - 2.0 * log 4.0, fourfold.logDeterminant.value, 1e-12)

        Assert.Equal(NoiseVariance 0.25, (FitQuality.ReducedChiSquared 0.25).noiseVariance)
