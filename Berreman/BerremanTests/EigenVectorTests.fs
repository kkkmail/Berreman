namespace BerremanTests

open Berreman.Constants
open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.BerremanMatrix
open Berreman.Solvers

open Xunit
open Xunit.Abstractions

open MatrixComparison
open Berreman.Media
open OpticalProperties.Standard

type EigenVectorTestData =
    {
        description : string
        opticalProperties : OpticalProperties
        n1SinFita : N1SinFita
        values : ComplexVector4
        vectors : ComplexMatrix4x4
    }

type EigenVectorTests(output : ITestOutputHelper) =
    let data = 
        [
            {
                description = "Vacuum, 7 degrees incidence angle."
                opticalProperties = OpticalProperties.vacuum
                n1SinFita = N1SinFita.create 7.0 (Angle.degree 0.0 |> IncidenceAngle)

                values = 
                    [ 0.9925461516413222; 0.9925461516413222; -0.992546151641322; -0.9925461516413219 ]
                    |> ComplexVector4.fromRe
                vectors = 
                    [
                        [ 0.7044566603032238; 0.7097470068654241; 0.; 0. ]
                        [ 0.; 0.; 0.7097470068654242; 0.7044566603032237 ]
                        [ 0.; 0.; 0.7097470068654242; -0.7044566603032238 ]
                        [ -0.704456660303224; 0.7097470068654241; 0.; 0. ]
                    ]
                    |> ComplexMatrix4x4.fromRe
            }

            {
                description = "Standard transparent glass, 7 degrees incidence angle."
                opticalProperties = OpticalProperties.transparentGlass
                n1SinFita = N1SinFita.create 7.0 (Angle.degree 0.0 |> IncidenceAngle)

                values = 
                    [ 1.5151065517441338; -1.5151065517441336; -1.5151065517441333; 1.5151065517441322 ]
                    |> ComplexVector4.fromRe
                vectors = 
                    [
                        [ 0.; 0.; 0.5508535951895028; 0.8346018911234261 ]
                        [ 0.; 0.; -0.5508535951895027; 0.8346018911234262 ]
                        [ -0.5483797817402465; 0.8362294033209545; 0.; 0. ]
                        [ 0.5483797817402465; 0.8362294033209545; 0.; 0. ]
                    ]
                    |> ComplexMatrix4x4.fromRe
            }
        ]

    member __.runTest (d : EigenVectorTestData) = 
        //output.WriteLine d.description
        //let solver = BaseOpticalSystemSolver (d.opticalSystem, d.info)

        //output.WriteLine("eigenBasisUpper = {0}\n", solver.eigenBasisUpper)
        //output.WriteLine("eigenBasisFilm = {0}\n", solver.eigenBasisFilm)
        //output.WriteLine("eigenBasisLower = {0}\n", solver.eigenBasisLower)
        //output.WriteLine("coeffTbl = {0}\n", solver.coeffTbl)
        //output.WriteLine("freeTbl = {0}\n", solver.freeTbl)
        //output.WriteLine("cfm = {0}\n", solver.cfm)

        //let eI = solver.incidentLight.e
        //let hI = solver.incidentLight.h

        //let eR = solver.reflectedLight.e
        //let hR = solver.reflectedLight.h

        //let eT = solver.transmittedLight.e
        //let hT = solver.transmittedLight.h

        //verifyVectorEquality output "eI" eI d.expected.incident.e
        //verifyVectorEquality output "hI" hI d.expected.incident.h

        //verifyVectorEquality output "eR" eR d.expected.reflected.e
        //verifyVectorEquality output "hR" hR d.expected.reflected.h

        //verifyVectorEquality output "eT" eT d.expected.transmitted.e
        //verifyVectorEquality output "hT" hT d.expected.transmitted.h
        failwith ""


    //[<Fact>]
    //member this.basicSolverTest0 () = this.runTest (data.[0])

    //[<Fact>]
    //member this.basicSolverTest1 () = this.runTest (data.[1])
