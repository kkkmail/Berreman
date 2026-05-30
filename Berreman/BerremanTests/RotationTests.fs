namespace BerremanTests

open Berreman.MaterialProperties
open Berreman.Geometry
open BerremanTests.MatrixComparison
open Xunit

    type RotationTestData =
        {
            eps : Eps
            rotation : Rotation
            expectedEps : Eps
        }


type RotationTests() =
    let output = TestContext.Current.TestOutputHelper

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


    //[<Fact>]
    //member _.rotationTest0 () = runTest (data.[0])
