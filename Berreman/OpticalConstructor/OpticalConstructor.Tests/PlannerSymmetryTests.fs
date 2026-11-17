namespace OpticalConstructor.Tests

open Xunit
open OpticalConstructor.Domain.MaterialComplexityEditor
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.LibraryFacets
open OpticalConstructor.Domain.PlannerSymmetry
open CrystalMaterialFixtures

/// The Measurement Planner's layer-1 point-group table and the symmetry a crystal record states (spec 0050 S14.1).
module PlannerSymmetryTests =

    let private symbolsOf (groups : PointGroup list) : string list =
        groups |> List.map (fun group -> group.symbol)

    let private groupsOf (anisotropy : Anisotropy) : PointGroup list =
        PointGroup.all |> List.filter (fun group -> group.anisotropy = anisotropy)

    let private groupsWithAngles (count : ComponentCount) : PointGroup list =
        PointGroup.all |> List.filter (fun group -> group.orientationAngleCount = count)

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
