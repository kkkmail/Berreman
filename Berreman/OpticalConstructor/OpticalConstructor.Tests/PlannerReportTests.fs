namespace OpticalConstructor.Tests

open Xunit
open Berreman.Constants
open Berreman.Geometry
open Berreman.MaterialProperties
open Berreman.Fields
open Berreman.Media
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.PlannerSymmetry
open OpticalConstructor.Domain.MeasurementPlanner
open CrystalMaterialFixtures

/// The Measurement Planner's looks at a plate and what one look sees of each tensor component (spec 0050 S14.1).
module PlannerReportTests =

    let private uniaxial : CrystalParameters = UniaxialCrystal uniaxialParameters

    let private triclinic : CrystalParameters = TriclinicCrystal triclinicParameters

    let private absorbing : CrystalParameters = AbsorbingTriclinicCrystal absorbingTriclinicParameters

    let private obliqueIncidence : IncidenceAngle = IncidenceAngle.create (Angle.degree 30.0<deg>)

    let private principalAxes : CrystalAxis list = [ FirstCrystalAxis; SecondCrystalAxis; ThirdCrystalAxis ]

    /// A z-cut plate at normal incidence, transmitted, read by a polarimeter.
    let private normalZCut : PlannerLook =
        {
            normalAxisOpt = Some ThirdCrystalAxis
            incidence = IncidenceAngle.normal
            observable = TransmittedMueller
            reach = FullMuellerReach
        }

    let private obliqueZCut : PlannerLook = { normalZCut with incidence = obliqueIncidence }

    let private normalXCut : PlannerLook = { normalZCut with normalAxisOpt = Some FirstCrystalAxis }

    let private normalYCut : PlannerLook = { normalZCut with normalAxisOpt = Some SecondCrystalAxis }

    /// A plate with no principal axis on its normal (a tilted cut) at normal incidence.
    let private normalTiltedCut : PlannerLook = { normalZCut with normalAxisOpt = None }

    /// The parameter of `crystal` named `name`.
    let private crystalComponentNamed (crystal : CrystalParameters) (name : ParameterName) : CrystalComponent =
        crystal.components |> List.find (fun (held : CrystalComponent) -> held.name = name)

    /// The component `components` names `name` as.
    let private componentNamed (crystal : CrystalParameters) (name : ParameterName) : TensorComponent =
        (crystalComponentNamed crystal name).``component``

    /// The look's sight of each named parameter of `crystal`.
    let private sightsOf (look : PlannerLook) (crystal : CrystalParameters) (names : ParameterName list) : LookSight list =
        names |> List.map (fun name -> look.sightOf crystal (componentNamed crystal name))

    /// What `looks` recover of the named parameter of `crystal`.
    let private targetSightOf (looks : PlannerLook list) (crystal : CrystalParameters) (name : ParameterName) : TargetSight =
        crystal.targetSightOf looks (componentNamed crystal name)

    /// The report of `crystal` over `targets` and `looks`; a refusal fails the fact.
    let private reportOf (targets : ParameterName list) (looks : PlannerLook list) (crystal : CrystalParameters) : RecoverabilityReport =
        match crystal.structuralReport targets looks with
        | Ok report -> report
        | Error error -> failwith error.describe

    /// The report over `looks` with every parameter of `crystal` a target, in its order.
    let private reportOver (looks : PlannerLook list) (crystal : CrystalParameters) : RecoverabilityReport =
        reportOf (crystal.components |> List.map (fun (held : CrystalComponent) -> held.name)) looks crystal

    /// The report's unrecoverable-target findings, in order.
    let private targetFindingsOf (report : RecoverabilityReport) : TargetFinding list =
        report.findings
        |> List.choose (fun (finding : PlannerFinding) ->
            match finding with
            | UnrecoverableTargetFinding targetFinding -> Some targetFinding
            | OffAxisCutFinding _ -> None)

    [<Fact>]
    let ``a look sees a component only through an axis transverse to its propagation`` () : unit =
        Assert.Equal<LookSight list>(
            [ AlongPropagationInLook; AlongPropagationInLook; SeenInLook ],
            sightsOf normalZCut uniaxial [ ParameterName "n_e"; ParameterName "g33"; ParameterName "g11" ])

        Assert.Equal<LookSight list>(
            [ SeenInLook; SeenInLook ],
            sightsOf obliqueZCut uniaxial [ ParameterName "n_e"; ParameterName "g33" ])

        Assert.Equal<LookSight list>([ SeenInLook ], sightsOf normalXCut uniaxial [ ParameterName "g33" ])

        Assert.Equal(NormalIncidenceLook, normalZCut.tilt)
        Assert.Equal(LongitudinalAxis, normalZCut.aspectOf ThirdCrystalAxis)
        Assert.Equal(TransverseAxis, normalZCut.aspectOf FirstCrystalAxis)

        Assert.Equal(ObliqueIncidenceLook, obliqueZCut.tilt)
        Assert.Equal<AxisAspect list>(List.replicate 3 TransverseAxis, principalAxes |> List.map obliqueZCut.aspectOf)
        Assert.Equal<AxisAspect list>(List.replicate 3 TransverseAxis, principalAxes |> List.map normalTiltedCut.aspectOf)

    [<Fact>]
    let ``an index needs linear retardance at normal incidence, or an oblique look`` () : unit =
        Assert.Equal<LookSight list>([ NoRetardanceInLook ], sightsOf normalZCut uniaxial [ ParameterName "n_o" ])

        Assert.Equal<LookSight list>(
            [ NoRetardanceInLook ],
            sightsOf { normalZCut with observable = ReflectedMueller } uniaxial [ ParameterName "n_o" ])

        Assert.Equal<LookSight list>(
            [ SeenInLook; SeenInLook ],
            sightsOf normalXCut uniaxial [ ParameterName "n_o"; ParameterName "n_e" ])

        Assert.Equal<LookSight list>(
            [ SeenInLook; SeenInLook; AlongPropagationInLook ],
            sightsOf normalZCut triclinic [ ParameterName "n1"; ParameterName "n2"; ParameterName "n3" ])

        Assert.Equal<LookSight list>(
            [ NoRetardanceInLook ],
            sightsOf { normalXCut with reach = IntensityOnlyReach } uniaxial [ ParameterName "n_o" ])

        Assert.Equal<LookSight list>(
            [ SeenInLook ],
            sightsOf { obliqueZCut with reach = IntensityOnlyReach } uniaxial [ ParameterName "n_o" ])

    [<Fact>]
    let ``gyration needs a full Mueller reach, and an off-diagonal component an oblique look`` () : unit =
        Assert.Equal<LookSight list>(
            [ NormalIncidenceInLook; SeenInLook ],
            sightsOf normalZCut triclinic [ ParameterName "g12"; ParameterName "g11" ])

        Assert.Equal<LookSight list>(
            [ SeenInLook; SeenInLook ],
            sightsOf obliqueZCut triclinic [ ParameterName "g12"; ParameterName "g13" ])

        Assert.Equal<LookSight list>(
            [ ReachMissesInLook ],
            sightsOf { normalXCut with reach = EllipsometricReach } uniaxial [ ParameterName "g33" ])

        Assert.Equal<LookSight list>(
            [ ReachMissesInLook ],
            sightsOf { normalZCut with reach = EllipsometricReach } uniaxial [ ParameterName "g11" ])

        Assert.Equal<LookSight list>(
            [ ReachMissesInLook ],
            sightsOf { obliqueZCut with reach = IntensityOnlyReach } triclinic [ ParameterName "g12" ])

        Assert.Equal<LookSight list>([ SeenInLook ], sightsOf normalTiltedCut triclinic [ ParameterName "g12" ])

    [<Fact>]
    let ``an absorption component is seen through its transverse axes`` () : unit =
        Assert.Equal<LookSight list>(
            [ SeenInLook; SeenInLook; AlongPropagationInLook; AlongPropagationInLook ],
            sightsOf normalZCut absorbing [ ParameterName "e11"; ParameterName "e12"; ParameterName "e33"; ParameterName "e13" ])

        Assert.Equal<LookSight list>(
            [ SeenInLook; SeenInLook ],
            sightsOf obliqueZCut absorbing [ ParameterName "e33"; ParameterName "e13" ])

    [<Fact>]
    let ``a measurement configuration is a look at its cut's normal axis, incidence and observable`` () : unit =
        let configuration : MeasurementConfiguration =
            {
                cut = XCut
                thickness = Thickness.mkm 20.0<mkm>
                incidenceAngle = obliqueIncidence
                azimuth = SampleAzimuth.degree 45.0<deg>
                observable = ReflectedMueller
                waveLength = WaveLength.nm 632.8<nm>
            }

        let look = configuration.plannerLookWith EllipsometricReach

        Assert.Equal<PlannerLook>(
            {
                normalAxisOpt = Some FirstCrystalAxis
                incidence = obliqueIncidence
                observable = ReflectedMueller
                reach = EllipsometricReach
            },
            look)

        Assert.Equal(ObliqueIncidenceLook, look.tilt)

        let normalIncidence = IncidenceAngle.create (Angle.degree 0.0<deg>)
        let zCutConfiguration : MeasurementConfiguration = { configuration with cut = ZCut; incidenceAngle = normalIncidence }
        let zCutLook = zCutConfiguration.plannerLookWith EllipsometricReach

        Assert.Equal<PlannerLook>(
            {
                normalAxisOpt = Some ThirdCrystalAxis
                incidence = normalIncidence
                observable = ReflectedMueller
                reach = EllipsometricReach
            },
            zCutLook)

        Assert.Equal(NormalIncidenceLook, zCutLook.tilt)

    [<Fact>]
    let ``a target is reachable when any look sees it, and unreachable when there is no look`` () : unit =
        Assert.Equal(ReachableTarget, targetSightOf [ normalZCut; normalXCut ] uniaxial (ParameterName "n_e"))

        Assert.Equal<TargetSight list>(
            List.replicate 4 (UnreachableTarget NoLookReason),
            uniaxial.components |> List.map (fun (held : CrystalComponent) -> uniaxial.targetSightOf [] held.``component``))

    [<Fact>]
    let ``an unreachable target takes the reason of the look that came closest to seeing it`` () : unit =
        Assert.Equal(UnreachableTarget NoRetardanceReason, targetSightOf [ normalZCut ] uniaxial (ParameterName "n_o"))
        Assert.Equal(UnreachableTarget AlongEveryPropagationReason, targetSightOf [ normalZCut ] uniaxial (ParameterName "g33"))

        Assert.Equal(
            UnreachableTarget FullMuellerNeededReason,
            targetSightOf [ normalZCut; { normalXCut with reach = EllipsometricReach } ] uniaxial (ParameterName "g33"))

        Assert.Equal(
            UnreachableTarget NormalIncidenceOnlyReason,
            targetSightOf [ normalXCut; normalYCut; normalZCut ] triclinic (ParameterName "g12"))

        Assert.Equal<string list>(
            [
                "no look of the kit measures a plate of it"
                "every look propagates along its axes"
                "it is seen only off normal incidence"
                "no look carries its linear retardance or an oblique change of the Fresnel amplitudes"
                "it needs a full Mueller polarimeter, or a polarizer and an analyzer around the plate"
            ],
            [
                NoLookReason
                AlongEveryPropagationReason
                NormalIncidenceOnlyReason
                NoRetardanceReason
                FullMuellerNeededReason
            ]
            |> List.map (fun (reason : UnseenReason) -> reason.text))

    [<Fact>]
    let ``the Snell cap is the internal angle of the smallest principal index at the steepest incidence`` () : unit =
        Assert.Equal(internalAngle (RefractionIndex 1.544) IncidenceAngle.maxValue, uniaxial.snellCap)
        Assert.Equal(internalAngle triclinicParameters.index1 IncidenceAngle.maxValue, triclinic.snellCap)

    [<Fact>]
    let ``each unrecoverable target names the missing plates and the tilt or the reach that would recover it`` () : unit =
        let uniaxialNamed : ParameterName -> CrystalComponent = crystalComponentNamed uniaxial
        let addXCutOrTilt : PlannerRemedy list = [ AddPlateRemedy XCut; ObliqueIncidenceRemedy uniaxial.snellCap ]
        let overZCut = reportOver [ normalZCut ] uniaxial

        Assert.Equal<CrystalComponent list>([ uniaxialNamed (ParameterName "g11") ], overZCut.reachable)

        Assert.Equal<TargetFinding list>(
            [
                { target = uniaxialNamed (ParameterName "n_o"); reason = NoRetardanceReason; remedies = addXCutOrTilt }
                { target = uniaxialNamed (ParameterName "n_e"); reason = AlongEveryPropagationReason; remedies = addXCutOrTilt }
                { target = uniaxialNamed (ParameterName "g33"); reason = AlongEveryPropagationReason; remedies = addXCutOrTilt }
            ],
            targetFindingsOf overZCut)

        Assert.Equal<CrystalComponent list>(
            [ uniaxialNamed (ParameterName "n_o"); uniaxialNamed (ParameterName "n_e"); uniaxialNamed (ParameterName "g33") ],
            overZCut.unrecoverable)

        Assert.Equal<TargetFinding list>(
            [
                { target = uniaxialNamed (ParameterName "g11"); reason = FullMuellerNeededReason; remedies = [ FullMuellerRemedy ] }
                { target = uniaxialNamed (ParameterName "g33"); reason = FullMuellerNeededReason; remedies = [ FullMuellerRemedy ] }
            ],
            targetFindingsOf (reportOver [ { normalXCut with reach = EllipsometricReach } ] uniaxial))

        Assert.Equal<PlannerFinding list>(
            [
                UnrecoverableTargetFinding
                    {
                        target = crystalComponentNamed triclinic (ParameterName "g12")
                        reason = NormalIncidenceOnlyReason
                        remedies = [ ObliqueIncidenceRemedy triclinic.snellCap ]
                    }
            ],
            (reportOf [ ParameterName "g12" ] [ normalXCut; normalYCut; normalZCut ] triclinic).findings)

        let noLook = reportOver [] uniaxial

        Assert.Equal<TargetFinding list>(
            [
                {
                    target = uniaxialNamed (ParameterName "g11")
                    reason = NoLookReason
                    remedies = [ AddPlateRemedy ZCut; AddPlateRemedy XCut; FullMuellerRemedy ]
                }
            ],
            targetFindingsOf noLook |> List.filter (fun (finding : TargetFinding) -> finding.target.name = ParameterName "g11"))

        Assert.Equal<CrystalComponent list>([], noLook.reachable)
        Assert.Equal<OffAxisCutNeed option>(None, noLook.offAxisCutOpt)

    [<Fact>]
    let ``plates that all put the same axis on their normal call for an off-axis cut`` () : unit =
        let need : OffAxisCutNeed =
            {
                targets = [ crystalComponentNamed uniaxial (ParameterName "n_e"); crystalComponentNamed uniaxial (ParameterName "g33") ]
                capAngle = uniaxial.snellCap
                cuts = [ XCut ]
            }

        let overZCut = reportOver [ normalZCut ] uniaxial

        Assert.Equal<OffAxisCutNeed option>(Some need, overZCut.offAxisCutOpt)
        Assert.Equal(OffAxisCutFinding need, List.last overZCut.findings)

        let tiltedZCut = reportOver [ normalZCut; obliqueZCut ] uniaxial

        Assert.Equal<TargetFinding list>([], targetFindingsOf tiltedZCut)
        Assert.Equal<PlannerFinding list>([ OffAxisCutFinding need ], tiltedZCut.findings)

        Assert.Equal<PlannerFinding list>([], (reportOver [ normalZCut; normalXCut ] uniaxial).findings)

        let triclinicNeed : OffAxisCutNeed =
            {
                targets = [ crystalComponentNamed triclinic (ParameterName "n3"); crystalComponentNamed triclinic (ParameterName "g33") ]
                capAngle = triclinic.snellCap
                cuts = [ XCut; YCut ]
            }

        Assert.Equal<OffAxisCutNeed option>(Some triclinicNeed, (reportOver [ normalZCut ] triclinic).offAxisCutOpt)

    [<Fact>]
    let ``a target the crystal does not have refuses the report, naming it`` () : unit =
        match uniaxial.structuralReport [ ParameterName "n_o"; ParameterName "n2"; ParameterName "n9" ] [ normalZCut ] with
        | Error (UnknownPlannerTargetErr reason as error) ->
            Assert.Contains("n2", reason)
            Assert.Contains("n_o, n_e, g11, g33", reason)
            Assert.DoesNotContain("n9", reason)
            Assert.StartsWith("the crystal has no such parameter: ", error.describe)
        | Ok _ -> Assert.Fail "a target the crystal does not have must refuse the report"

    [<Fact>]
    let ``the findings read as sentences naming the parameter, the reason and the remedy`` () : unit =
        let overZCut = reportOver [ normalZCut ] uniaxial

        Assert.Equal<string list>(
            [
                "This kit can never recover n_o: no look carries its linear retardance or an oblique change of the Fresnel amplitudes."
                "This kit can never recover n_e: every look propagates along its axes."
                "This kit can never recover g33: every look propagates along its axes."
                "You need an off-axis cut: n_e, g33 lie along the normal of every plate, and a tilt carries the wave at most 40.4° off it; add one x-cut plate."
            ],
            overZCut.findings |> List.map (fun (finding : PlannerFinding) -> finding.text))

        Assert.Equal<string list>(
            [ "add one x-cut plate"; "tilt a plate (its internal angle reaches at most 40.4°)" ],
            targetFindingsOf overZCut
            |> List.filter (fun (finding : TargetFinding) -> finding.target.name = ParameterName "n_o")
            |> List.collect (fun (finding : TargetFinding) -> finding.remedies |> List.map (fun (remedy : PlannerRemedy) -> remedy.text)))

        Assert.Equal<string list>(
            [ "z-cut plate"; "x-cut plate"; "y-cut plate"; "30.0° tilted-cut plate" ],
            [ ZCut; XCut; YCut; TiltedCut (Angle.degree 30.0<deg>) ] |> List.map (fun (cut : SampleCut) -> cut.plateText))

        Assert.Equal("use a full Mueller polarimeter, or a polarizer and an analyzer around the plate", FullMuellerRemedy.text)

        Assert.Equal<string option>(
            Some "You need an off-axis cut: n3, g33 lie along the normal of every plate, and a tilt carries the wave at most 41.1° off it; add one x-cut plate or one y-cut plate.",
            (reportOver [ normalZCut ] triclinic).offAxisCutOpt |> Option.map (fun (need : OffAxisCutNeed) -> (OffAxisCutFinding need).text))
