namespace BerremanTests

open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.BerremanMatrix

open Xunit
open Xunit.Abstractions

open MatrixComparison


type BerremanMatrixTestData =
    {
        description : string
        opticalProperties : OpticalProperties
        n1SinFita : N1SinFita
        expected : ComplexMatrix
    }


type BerremanMatrixTests(output : ITestOutputHelper) =
    let data = 
        [|
            {
                description = "Homegenious media, normal incidence angle."
                opticalProperties = 1.5 |> RefractionIndex.create |> OpticalProperties.fromRefractionIndex
                n1SinFita = N1SinFita.create 1.0 (Angle.degree 0.0 |> IncidenceAngle)

                expected = 
                    [
                        [ 0.0; 1.0; 0.0; 0.0 ]
                        [ 2.25; 0.0; 0.0; 0.0 ]
                        [ 0.0; 0.0; 0.0; 1.0 ]
                        [ 0.0; 0.0; 2.25; 0.0 ]
                    ]
                    |> ComplexMatrix.fromRe
            }

            {
                description = "Random real epsilon, random incidence angle."
                opticalProperties = 
                    {
                        eps = 
                            [
                                [ 4.182840238976937; 0.4399678152485267; 0.24937829248903232 ]
                                [ 0.4399678152485267; 3.409521940849067; 0.8817720799949946 ]
                                [ 0.24937829248903232; 0.8817720799949945; 1.7317211303338491 ]
                            ]
                            |> Eps.fromRe
                        mu = Mu.vacuum
                        rho = Rho.vacuum
                    }
                n1SinFita = N1SinFita.create 1.0 (Angle.degree 19.0 |> IncidenceAngle)

                expected = 
                    [
                        [ -0.04688377881701955; 0.9387923255425054; -0.16577548411647242; 0. ]
                        [ 4.146928260035641; -0.04688377881701956; 0.31298731485395; 0. ]
                        [ 0.; 0.; 0.; 1. ]
                        [ 0.3129873148539499; -0.16577548411647247; 2.854539321006106; 0. ]
                    ]
                    |> ComplexMatrix.fromRe
            }

            {
                description = "All random optical properties, randome incidence angle."
                opticalProperties = 
                    {
                        eps = 
                            [
                                [ createComplex 2.2108445373965475 0.003606762657455528; createComplex 0.3917972387692459 -0.0008306597873681838; createComplex -0.33546707028271566 -0.0003185282981231219 ]
                                [ createComplex 0.391797238769246 -0.0008306597873681833; createComplex 2.516781571187761 0.00448394268988372; createComplex -0.20562344050069822 0.0014226568145231013 ]
                                [ createComplex -0.335467070282716 -0.0003185282981231219; createComplex -0.20562344050069867 0.0014226568145231018; createComplex 2.8990427550750897 0.005012161886697677 ]
                            ]
                            |> Eps.create
                        mu = 
                            [
                                [ 0.9927965096102219; 0.004679182260859449; -0.01523408689474312 ]
                                [ 0.004679182260859449; 0.9940450496409975; -0.02910824169850346 ]
                                [ -0.01523408689474312; -0.029108241698503456; 1.0657422460670098 ]
                            ]
                            |> Mu.fromRe
                        rho = 
                            [
                                [ -0.00022136713805367622; 0.06166773157320616; 0.005379370844434909 ]
                                [ 0.061667731573206166; -0.035619418563562225; 0.004056865541623645 ]
                                [ 0.005379370844434909; 0.004056865541623645; -0.08246553212362771 ]
                            ]
                            |> Rho.fromIm
                    }
                n1SinFita = N1SinFita.create 1.0 (Angle.degree 48.0 |> IncidenceAngle)

                expected = 
                    [
                        [ createComplex 0.08607659694417036 -0.062090983572361515; createComplex 0.8023304210318745 0.0003308095002124304; createComplex 0.032507755412372545 0.049696580383995735; createComplex -0.0042586087374033525 0.0010791503454091623 ]
                        [ createComplex 2.1720091877851124 0.003600217346427447; createComplex 0.08607692385187013 0.06195635462385157; createComplex 0.3679911503054432 -0.0035567207129593463; createComplex -3.8049089780536243e-7 -0.0003420710009216313 ]
                        [ createComplex 3.804908978053343e-7 0.0003420710009216352; createComplex -0.004262348475787381 -0.0010791594262887678; createComplex 0.010541847320044062 0.06202400235029927; createComplex 0.9925726484666699 1.056931983087678e-8 ]
                        [ createComplex 0.368001567690997 0.002262161392498498; createComplex 0.032458779443745804 -0.05061046848984253; createComplex 1.9828552540134243 0.004713119655511653; createComplex 0.010536682326775926 -0.062023712974967664 ]
                    ]
                    |> ComplexMatrix.create
            }
        |]

    // Calculated and expected Berreman matrix.
    member __.runTest (d : BerremanMatrixTestData) = 
        output.WriteLine d.description
        let bm = BerremanMatrix.create d.n1SinFita d.opticalProperties
        let (ComplexMatrix4x4 result) = bm.berremanMatrix
        verifyMatrixEquality output result d.expected

    [<Fact>]
    member this.berremanMatrixTest0 () = this.runTest (data.[0])

    [<Fact>]
    member this.berremanMatrixTest1 () = this.runTest (data.[1])

    [<Fact>]
    member this.berremanMatrixTest2 () = this.runTest (data.[2])

