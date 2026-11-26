namespace BerremanTests

open Berreman.Geometry
open Berreman.Fields
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.PlannerSymmetry
open OpticalConstructor.Domain.MeasurementPlanner
open OpticalConstructor.Optimization
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.MuellerFitting
open OpticalConstructor.Optimization.FitUncertainty
open OpticalConstructor.Optimization.BenchNoise
open OpticalConstructor.Optimization.RecoveryEnsembles
open OpticalConstructor.Optimization.PlannerInformation
open Xunit
open BerremanTests.InverseFitHarness
open BerremanTestSupport

/// The Measurement Planner over spec 0044's own designs: its layer-1 report reproduces their ablation facts (spec 0050
/// S14.1), and its layer-2 information their rank and covariance facts (S14.2).
type MeasurementPlannerTests() =

    // The constants' names as the records' `axes` write them (`MuellerInverse.fs`).
    let nO = ParameterName "n_o"
    let nE = ParameterName "n_e"
    let n1 = ParameterName "n1"
    let n2 = ParameterName "n2"
    let n3 = ParameterName "n3"
    let g11 = ParameterName "g11"
    let g22 = ParameterName "g22"
    let g33 = ParameterName "g33"
    let g23 = ParameterName "g23"
    let g13 = ParameterName "g13"
    let g12 = ParameterName "g12"

    let quartz : CrystalParameters = UniaxialCrystal QuartzSample.quartz

    let triclinic : CrystalParameters = TriclinicCrystal BiaxialSample.triclinic

    /// `crystal`'s structural report on every parameter of its record, `design` read as full Mueller looks.
    let reportOver (crystal : CrystalParameters) (design : MeasurementConfiguration list) : RecoverabilityReport =
        let looks : PlannerLook list =
            design
            |> List.map (fun (configuration : MeasurementConfiguration) -> configuration.plannerLookWith FullMuellerReach)

        let targets : ParameterName list =
            crystal.parameterAxes |> List.map (fun (axis : ParameterAxis<CrystalParameters>) -> axis.name)

        match crystal.structuralReport targets looks with
        | Ok report -> report
        | Error error -> failwith error.describe

    /// The names of `components`, in order.
    let namesOf (components : CrystalComponent list) : ParameterName list =
        components |> List.map (fun (held : CrystalComponent) -> held.name)

    /// The report's findings as it states them.
    let findingTextsOf (report : RecoverabilityReport) : string list =
        report.findings |> List.map (fun (finding : PlannerFinding) -> finding.text)

    /// The report's finding on `name`; without one the fact fails naming every finding.
    let targetFindingOf (report : RecoverabilityReport) (name : ParameterName) : TargetFinding =
        report.findings
        |> List.tryPick (fun (finding : PlannerFinding) ->
            match finding with
            | UnrecoverableTargetFinding targetFinding when targetFinding.target.name = name -> Some targetFinding
            | UnrecoverableTargetFinding _
            | OffAxisCutFinding _ -> None)
        |> Option.defaultWith (fun () ->
            let texts : string = findingTextsOf report |> String.concat " "
            failwith $"the report has no finding on {name.value}: {texts}")

    /// The planner's information over the names of `axes` from one candidate; a refusal fails the fact.
    let informationOver (axes : ParameterAxis<'Parameters> list) (candidate : CandidateRows) : PlannerInformation =
        let coordinates : ParameterName list = axes |> List.map (fun (axis : ParameterAxis<'Parameters>) -> axis.name)

        match PlannerInformation.over coordinates [ candidate ] with
        | Ok information -> information
        | Error error -> failwith error.describe

    /// The coordinates `combination` names, in coordinate order.
    let namesCarriedBy (coordinates : ParameterName list) (combination : CombinationText) : ParameterName list =
        let tokens : string list = combination.value.Split(' ') |> List.ofArray
        coordinates |> List.filter (fun (name : ParameterName) -> List.contains name.value tokens)

    /// The planner's bounds at the noisy fit's own Jacobian and residual level, each over the magnitude of its axis's
    /// value in `truth`, against the relative errors the fit's own covariance predicts (step 477).
    let assertBoundsArePredicted (truth : 'Parameters) (first : NoisyExperiment<'Parameters>) : unit =
        let quality : FitQuality.FitReport = first.fit.report
        let predicted : (ParameterName * RelativeError) list = predictedRelativeErrors truth first.fit quality
        let scaling : ParameterScaling<'Parameters> = first.fit.scaling

        // `reportFrom` takes the same `FitQuality.residualJacobian` at the same point (steps 336, 477).
        let candidate : CandidateRows =
            {
                jacobian = residualJacobianAt first.fit.residual first.fit.solution.solution
                noise = quality.reducedChiSquared.noiseVariance
            }

        let information : PlannerInformation = informationOver scaling.axes candidate

        match information.boundsOver scaling with
        | Some bounds ->
            Assert.Equal<ParameterName list>(
                predicted |> List.map fst,
                bounds |> List.map (fun (bound : TargetBound) -> bound.name))

            for (axis, bound, (_, prediction)) in List.zip3 scaling.axes bounds predicted do
                let relative : float = bound.sigma.value / abs (axis.read truth)

                Assert.True(
                    abs (relative - prediction.value) <= 1.0e-12 * abs prediction.value,
                    $"{bound.name.value}: the planner's bound {relative}, the fit's prediction {prediction.value}")
        | None ->
            Assert.Fail
                $"the planner bounds no constant at the fit's residual level: rank {information.rank.value} of {scaling.dimension}"

    [<Fact>]
    member _.``the planner finds C1 blind to g33, leaves three of the four quartz constants unrecoverable and proposes the second cut`` () : unit =
        // `MuellerInverseTests`: C1 is completely blind to g33, and C1 alone cannot determine three of the four.
        let report : RecoverabilityReport = reportOver quartz QuartzSample.c1
        let cap : Angle = internalAngle QuartzSample.quartzOrdinaryIndex IncidenceAngle.maxValue
        let addXCutOrTilt : PlannerRemedy list = [ AddPlateRemedy XCut; ObliqueIncidenceRemedy cap ]

        Assert.Equal<ParameterName list>([ nO; nE; g33 ], namesOf report.unrecoverable)
        Assert.Equal<ParameterName list>([ g11 ], namesOf report.reachable)
        Assert.Equal(AlongEveryPropagationReason, (targetFindingOf report g33).reason)
        Assert.Equal(NoRetardanceReason, (targetFindingOf report nO).reason)

        Assert.Equal<PlannerRemedy list list>(
            List.replicate 3 addXCutOrTilt,
            [ nO; nE; g33 ] |> List.map (fun (name : ParameterName) -> (targetFindingOf report name).remedies))

        match report.offAxisCutOpt with
        | Some need ->
            Assert.Equal<ParameterName list>([ nE; g33 ], namesOf need.targets)
            Assert.Equal(cap, need.capAngle)
            Assert.Equal<SampleCut list>([ XCut ], need.cuts)
        | None ->
            let texts : string = findingTextsOf report |> String.concat " "
            Assert.Fail $"C1 alone must call for the second cut, and the report found: {texts}"

    [<Fact>]
    member _.``the planner finds every quartz constant reachable once the second cut joins C1, and over the full C1-C4 set`` () : unit =
        // `MuellerInverseTests`: C1 with the x-cut restored, and the full set in which every parameter is observable.
        let assertEveryConstantReachable (design : MeasurementConfiguration list) : unit =
            let report : RecoverabilityReport = reportOver quartz design

            Assert.Equal<string list>([], findingTextsOf report)
            Assert.Equal<ParameterName list>([ nO; nE; g11; g33 ], namesOf report.reachable)

        assertEveryConstantReachable (QuartzSample.c1 @ QuartzSample.c3)
        assertEveryConstantReachable QuartzSample.fullConfigurations

    [<Fact>]
    member _.``the planner flags g12, g13 and g23 for a normal-incidence-only biaxial design and names oblique incidence and further cuts as the remedy`` () : unit =
        // `BiaxialInverseTests`: the cuts buy the gyration DIAGONAL and only the tilt buys its OFF-DIAGONAL.
        let cap : Angle = internalAngle BiaxialSample.index1 IncidenceAngle.maxValue
        let offDiagonal : ParameterName list = [ g23; g13; g12 ]

        let assertOffDiagonalNeedsTilt (report : RecoverabilityReport) : unit =
            for name in offDiagonal do
                let finding : TargetFinding = targetFindingOf report name

                Assert.Equal(NormalIncidenceOnlyReason, finding.reason)
                Assert.Equal<PlannerRemedy list>([ ObliqueIncidenceRemedy cap ], finding.remedies)

        let zCutOnly : RecoverabilityReport = reportOver triclinic (BiaxialDesign.normalIncidenceOn ZCut)

        Assert.Equal<ParameterName list>([ n3; g33; g23; g13; g12 ], namesOf zCutOnly.unrecoverable)
        assertOffDiagonalNeedsTilt zCutOnly

        for name in [ n3; g33 ] do
            let finding : TargetFinding = targetFindingOf zCutOnly name

            Assert.Equal(AlongEveryPropagationReason, finding.reason)

            Assert.Equal<PlannerRemedy list>(
                [ AddPlateRemedy XCut; AddPlateRemedy YCut; ObliqueIncidenceRemedy cap ],
                finding.remedies)

        match zCutOnly.offAxisCutOpt with
        | Some need ->
            Assert.Equal<ParameterName list>([ n3; g33 ], namesOf need.targets)
            Assert.Equal(cap, need.capAngle)
            Assert.Equal<SampleCut list>([ XCut; YCut ], need.cuts)
        | None ->
            let texts : string = findingTextsOf zCutOnly |> String.concat " "
            Assert.Fail $"the z-cut alone must call for an off-axis cut, and the report found: {texts}"

        // Cutting more plates does not substitute for tilting them.
        let assertOnlyOffDiagonalUnrecoverable (design : MeasurementConfiguration list) : unit =
            let report : RecoverabilityReport = reportOver triclinic design

            Assert.Equal<ParameterName list>(offDiagonal, namesOf report.unrecoverable)
            assertOffDiagonalNeedsTilt report
            Assert.Equal<OffAxisCutNeed option>(None, report.offAxisCutOpt)

        assertOnlyOffDiagonalUnrecoverable (BiaxialDesign.normalIncidenceOn ZCut @ BiaxialDesign.normalIncidenceOn XCut)
        assertOnlyOffDiagonalUnrecoverable BiaxialDesign.b1

    [<Fact>]
    member _.``the planner finds all nine biaxial constants reachable over the full biaxial measurement set`` () : unit =
        // `BiaxialInverseTests`: the full set in which every one of the nine parameters is observable.
        let report : RecoverabilityReport = reportOver triclinic BiaxialDesign.fullConfigurations

        Assert.Equal<string list>([], findingTextsOf report)
        Assert.Equal<ParameterName list>([ n1; n2; n3; g11; g22; g33; g23; g13; g12 ], namesOf report.reachable)

    [<Fact>]
    member _.``the planner finds the full C1-C4 set full rank with every quartz constant bounded, and C1 alone blind to n_e and g33`` () : unit =
        // `MuellerInverseTests`: the full set is well conditioned with every parameter observable, and C1 is completely
        // blind to n_e and g33, their Jacobian columns identically zero (spec 0050 S14.2; 001 §M).
        let scaling : ParameterScaling<UniaxialParameters> = QuartzSample.quartzScalingAround QuartzSample.quartz

        let informationAtQuartz (design : MeasurementConfiguration list) : PlannerInformation =
            candidateRowsOver QuartzSample.forward scaling QuartzSample.quartz NoiseParam.decentLab design
            |> expectOk
            |> informationOver UniaxialParameters.axes

        let isFinitePositive (value : float) : bool = System.Double.IsFinite value && value > 0.0

        let full : PlannerInformation = informationAtQuartz QuartzSample.fullConfigurations

        Assert.True(full.rank = MatrixRank 4, $"the full set has rank {full.rank.value} of 4")
        Assert.Empty(full.confounded)

        match full.conditionOpt with
        | Some condition -> Assert.True(System.Double.IsFinite condition.value, $"the full set's condition is {condition.value}")
        | None -> Assert.Fail "the full set has no finite condition number"

        match full.scaledSigmasOpt with
        | Some (DesignVector sigmas) ->
            let described : string = sigmas |> Array.map string |> String.concat ", "

            Assert.Equal(4, sigmas.Length)
            Assert.True(Array.forall isFinitePositive sigmas, $"the full set's scaled sigmas are {described}")
        | None -> Assert.Fail "the full set has no Cramer-Rao bound"

        match full.boundsOver scaling with
        | Some bounds ->
            Assert.Equal<ParameterName list>([ nO; nE; g11; g33 ], bounds |> List.map (fun (bound : TargetBound) -> bound.name))

            for bound in bounds do
                Assert.True(isFinitePositive bound.sigma.value, $"{bound.name.value} is bounded by {bound.sigma.value}")
        | None -> Assert.Fail "the full set bounds no quartz constant"

        let c1 : PlannerInformation = informationAtQuartz QuartzSample.c1

        let texts : CombinationText list =
            c1.confounded |> List.map (fun (direction : SingularDirection) -> direction.text c1.coordinates)

        let described : string = texts |> List.map (fun (text : CombinationText) -> text.value) |> String.concat "; "

        let carried : ParameterName list =
            c1.coordinates
            |> List.filter (fun (name : ParameterName) ->
                texts |> List.exists (fun (text : CombinationText) -> List.contains name (namesCarriedBy c1.coordinates text)))

        Assert.True(c1.rank = MatrixRank 2, $"C1 alone has rank {c1.rank.value} of 4, confounded: {described}")
        Assert.True(texts.Length = 2, $"C1 alone has {texts.Length} confounded combinations: {described}")
        Assert.Equal<ParameterName list>([ nE; g33 ], carried)

    [<Fact>]
    member _.``the planner's bounds for the noisy quartz design are what one noisy fit's own covariance predicts`` () : unit =
        // `MuellerInverseTests`' ensemble fact: its first experiment's own covariance predicts the ensemble's scatter.
        QuartzSample.recoverQuartzFromNoisy NoiseParam.decentLab (List.head BiaxialSample.ensembleSeeds)
        |> expectOk
        |> assertBoundsArePredicted QuartzSample.quartz

    [<Fact>]
    member _.``the planner's bounds for AT-2's noisy design are what one noisy fit's own covariance predicts`` () : unit =
        // AT-2's design read at design level, `fullConfigurations` = a1 @ a2 @ a3: its kit carries each cut on its
        // observation rows (step 433).
        AbsorbingBiaxialSample.recoverAbsorbingFromNoisy NoiseParam.decentLab (List.head AbsorbingBiaxialSample.noisySeeds)
        |> expectOk
        |> assertBoundsArePredicted AbsorbingBiaxialSample.absorbing
