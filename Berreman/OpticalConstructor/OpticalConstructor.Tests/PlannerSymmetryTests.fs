namespace OpticalConstructor.Tests

open Xunit
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain.MaterialComplexityEditor
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.LibraryFacets
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.PlannerSymmetry
open CrystalMaterialFixtures

/// The Measurement Planner's layer-1 point-group table, the symmetry a crystal record states, its parameters' tensor
/// components and the axis a cut puts on the plate normal (spec 0050 S14.1).
module PlannerSymmetryTests =

    let private symbolsOf (groups : PointGroup list) : string list =
        groups |> List.map (fun group -> group.symbol)

    let private groupsOf (anisotropy : Anisotropy) : PointGroup list =
        PointGroup.all |> List.filter (fun group -> group.anisotropy = anisotropy)

    let private groupsWithAngles (count : ComponentCount) : PointGroup list =
        PointGroup.all |> List.filter (fun group -> group.orientationAngleCount = count)

    let private tensorComponentsOf (crystal : CrystalParameters) : TensorComponent list =
        crystal.components |> List.map (fun (held : CrystalComponent) -> held.``component``)

    let private axisPair (firstAxis : CrystalAxis) (secondAxis : CrystalAxis) : CrystalAxisPair =
        { firstAxis = firstAxis; secondAxis = secondAxis }

    let private eulerOrientation (theta : Angle) : CrystalOrientation =
        EulerRotation { EulerAngles.convention = ZmYpXp; phi = Angle.zero; theta = theta; psi = Angle.zero }

    [<Fact>]
    let ``the 32 point groups split into 11 centrosymmetric and 21 non-centrosymmetric classes, 18 of them gyrotropic`` () : unit =
        let symbols = symbolsOf PointGroup.all
        Assert.Equal(32, symbols.Length)
        Assert.Equal(32, symbols |> List.distinct |> List.length)

        let centrosymmetric, nonCentrosymmetric =
            PointGroup.all |> List.partition (fun group -> group.centrosymmetry = CentrosymmetricClass)

        Assert.Equal<string list>(
            [ "-1"; "2/m"; "mmm"; "4/m"; "4/mmm"; "-3"; "-3m"; "6/m"; "6/mmm"; "m-3"; "m-3m" ],
            symbolsOf centrosymmetric)

        Assert.Equal(21, nonCentrosymmetric.Length)
        Assert.Equal<Centrosymmetry list>(List.replicate 21 NonCentrosymmetricClass, nonCentrosymmetric |> List.map (fun group -> group.centrosymmetry))

        let gyrotropic, nonGyrotropic =
            nonCentrosymmetric |> List.partition (fun group -> group.gyrotropy = GyrotropicClass)

        Assert.Equal<string list>(
            [ "1"; "2"; "m"; "222"; "mm2"; "4"; "-4"; "422"; "4mm"; "-42m"; "3"; "32"; "3m"; "6"; "622"; "6mm"; "23"; "432" ],
            symbolsOf gyrotropic)

        Assert.Equal<string list>([ "-6"; "-6m2"; "-43m" ], symbolsOf nonGyrotropic)
        Assert.Equal<Gyrotropy list>(List.replicate 11 NonGyrotropicClass, centrosymmetric |> List.map (fun group -> group.gyrotropy))

    [<Fact>]
    let ``a point group fixes how many independent principal indices and orientation angles a crystal has`` () : unit =
        Assert.Equal<string list>([ "23"; "m-3"; "432"; "-43m"; "m-3m" ], symbolsOf (groupsOf Isotropic))

        Assert.Equal<string list>(
            [ "4"; "-4"; "4/m"; "422"; "4mm"; "-42m"; "4/mmm"; "3"; "-3"; "32"; "3m"; "-3m"; "6"; "-6"; "6/m"; "622"; "6mm"; "-6m2"; "6/mmm" ],
            symbolsOf (groupsOf Uniaxial))

        Assert.Equal<string list>([ "1"; "-1"; "2"; "m"; "2/m"; "222"; "mm2"; "mmm" ], symbolsOf (groupsOf Biaxial))

        for anisotropy, groupCount, indexCount in [ Isotropic, 5, ComponentCount 1; Uniaxial, 19, ComponentCount 2; Biaxial, 8, ComponentCount 3 ] do
            Assert.Equal<ComponentCount list>(
                List.replicate groupCount indexCount,
                groupsOf anisotropy |> List.map (fun group -> group.anisotropy.independentIndexCount))

        Assert.Equal<string list>([ "1"; "-1" ], symbolsOf (groupsWithAngles (ComponentCount 3)))
        Assert.Equal<string list>([ "2"; "m"; "2/m" ], symbolsOf (groupsWithAngles (ComponentCount 1)))
        Assert.Equal(27, (groupsWithAngles (ComponentCount 0)).Length)

    [<Fact>]
    let ``a crystal record states its anisotropy and the point groups whose tensor form it holds`` () : unit =
        let uniaxialCrystal = UniaxialCrystal uniaxialParameters
        let uniaxial = uniaxialCrystal.symmetry
        Assert.Equal(Uniaxial, uniaxial.anisotropy)
        Assert.Equal(uniaxialCrystal.anisotropy, uniaxial.anisotropy)
        Assert.Equal<string list>([ "3"; "32"; "4"; "422"; "6"; "622" ], symbolsOf uniaxial.pointGroups)
        Assert.Equal<Gyrotropy list>(List.replicate 6 GyrotropicClass, uniaxial.pointGroups |> List.map (fun group -> group.gyrotropy))
        Assert.Equal(ComponentCount 2, uniaxial.independentIndexCount)
        Assert.Equal(ComponentCount 0, uniaxial.orientationAngleCount)

        for crystal in [ TriclinicCrystal triclinicParameters; AbsorbingTriclinicCrystal absorbingTriclinicParameters ] do
            let symmetry = crystal.symmetry
            Assert.Equal(Biaxial, symmetry.anisotropy)
            Assert.Equal(crystal.anisotropy, symmetry.anisotropy)
            Assert.Equal<string list>([ "1" ], symbolsOf symmetry.pointGroups)
            Assert.Equal(ComponentCount 3, symmetry.independentIndexCount)
            Assert.Equal(ComponentCount 3, symmetry.orientationAngleCount)

        let noGroup : CrystalSymmetry = { anisotropy = Isotropic; pointGroups = [] }
        Assert.Equal(ComponentCount 0, noGroup.orientationAngleCount)

    [<Fact>]
    let ``every crystal record names its parameters' tensor components in its axis order`` () : unit =
        for crystal in crystals do
            Assert.Equal<ParameterName list>(
                crystal.parameterAxes |> List.map (fun axis -> axis.name),
                crystal.components |> List.map (fun (held : CrystalComponent) -> held.name))

        let uniaxial = tensorComponentsOf (UniaxialCrystal uniaxialParameters)

        Assert.Equal<TensorComponent list>(
            [
                PrincipalIndexComponent [ FirstCrystalAxis; SecondCrystalAxis ]
                PrincipalIndexComponent [ ThirdCrystalAxis ]
                GyrationDiagonalComponent [ FirstCrystalAxis; SecondCrystalAxis ]
                GyrationDiagonalComponent [ ThirdCrystalAxis ]
            ],
            uniaxial)

        let offDiagonalPairs =
            [
                axisPair SecondCrystalAxis ThirdCrystalAxis
                axisPair FirstCrystalAxis ThirdCrystalAxis
                axisPair FirstCrystalAxis SecondCrystalAxis
            ]

        let triclinic = tensorComponentsOf (TriclinicCrystal triclinicParameters)
        Assert.Equal(9, triclinic.Length)
        Assert.Equal<TensorComponent list>(offDiagonalPairs |> List.map GyrationOffDiagonalComponent, triclinic |> List.skip 6)

        Assert.Equal<CrystalAxis list list>(
            [ [ SecondCrystalAxis; ThirdCrystalAxis ]; [ FirstCrystalAxis; ThirdCrystalAxis ]; [ FirstCrystalAxis; SecondCrystalAxis ] ],
            triclinic |> List.skip 6 |> List.map (fun tensorComponent -> tensorComponent.axes))

        let absorbing = tensorComponentsOf (AbsorbingTriclinicCrystal absorbingTriclinicParameters)
        Assert.Equal(15, absorbing.Length)
        Assert.Equal<TensorComponent list>(offDiagonalPairs |> List.map AbsorptionOffDiagonalComponent, absorbing |> List.skip 6 |> List.take 3)
        Assert.Equal<TensorComponent list>(offDiagonalPairs |> List.map GyrationOffDiagonalComponent, absorbing |> List.skip 12)

        Assert.Equal<CrystalAxis list list>(
            [ [ FirstCrystalAxis; SecondCrystalAxis ]; [ ThirdCrystalAxis ]; [ FirstCrystalAxis ] ],
            [ uniaxial.[0].axes; uniaxial.[3].axes; absorbing.[3].axes ])

    [<Fact>]
    let ``each principal cut puts on the normal the axis orientForCut carries there, and a tilted cut puts none`` () : unit =
        Assert.Equal<CrystalAxis option list>(
            [ Some ThirdCrystalAxis; Some FirstCrystalAxis; Some SecondCrystalAxis; None ],
            [ ZCut; XCut; YCut; TiltedCut (Angle.degree 30.0<deg>) ] |> List.map (fun (cut : SampleCut) -> cut.normalAxisOpt))

        Assert.Equal<CrystalAxis option>(
            Some FirstCrystalAxis,
            CrystalAxis.normalOf (orientForCut XCut (SampleAzimuth.degree 40.0<deg>) CrystalAxis.marker))

        Assert.Equal<float list>([ 1.0; 4.0; 9.0 ], [ 0..2 ] |> List.map (fun i -> CrystalAxis.marker.eps.[i, i].Real))

    [<Fact>]
    let ``a plate's crystal orientation puts its axis on the normal as the system builder rotates it`` () : unit =
        Assert.Equal<CrystalAxis option>(Some ThirdCrystalAxis, PrimaryAxes.normalAxisOpt)
        Assert.Equal<CrystalAxis option>(ZCut.normalAxisOpt, PrimaryAxes.normalAxisOpt)

        let xCutRotation = eulerOrientation (Angle.degree 90.0<deg>)
        Assert.Equal<CrystalAxis option>(Some FirstCrystalAxis, xCutRotation.normalAxisOpt)
        Assert.Equal<CrystalAxis option>(XCut.normalAxisOpt, xCutRotation.normalAxisOpt)

        Assert.Equal<CrystalAxis option>(None, (eulerOrientation (Angle.degree 30.0<deg>)).normalAxisOpt)
