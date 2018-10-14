namespace BerremanTests


open Berreman.MaterialProperties
open Berreman.Geometry
open BerremanTests.MatrixComparison
open Xunit
open Xunit.Abstractions

    type RotationTestData =
        {
            eps : Eps
            rotation : Rotation
            expectedEps : Eps
        }


type RotationTests(output : ITestOutputHelper) =

    let data = 
        [|
            {
                eps = failwith ""
                rotation = failwith ""
                expectedEps = failwith ""
            }
        |]

    let runTest (d : RotationTestData) = 
        let p = d.eps |> OpticalProperties.fromEpsion
        let r = p.rotate d.rotation
        verifyMatrixEqualityEps output r.eps d.expectedEps

    [<Fact>]
    member __.rotationTest0 () = runTest (data.[0])
