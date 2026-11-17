namespace OpticalConstructor.Tests

open Xunit
open Berreman.Constants
open Berreman.Geometry
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

    /// A plate with no principal axis on its normal (a tilted cut) at normal incidence.
    let private normalTiltedCut : PlannerLook = { normalZCut with normalAxisOpt = None }

    /// The look's sight of each named parameter of `crystal`, through the component `components` names it as.
    let private sightsOf (look : PlannerLook) (crystal : CrystalParameters) (names : ParameterName list) : LookSight list =
        names
        |> List.map (fun name ->
            let held = crystal.components |> List.find (fun (held : CrystalComponent) -> held.name = name)
            look.sightOf crystal held.``component``)

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
