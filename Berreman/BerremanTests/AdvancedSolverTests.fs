namespace BerremanTests

open System

open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Solvers
open Berreman.FieldFunctions

open Xunit
open Xunit.Abstractions
open FluentAssertions

open MatrixComparison
open Berreman.Media
open OpticalProperties.Standard
open FluentAssertions.Execution


type AdbancedOpticalSystemTestData =
    {
        description : string
        opticalSystem : BaseOpticalSystem
        info : IncidentLightInfo
        expected : EmFieldSystem
        stokes : StokesSystem option
    }


type AdvancedSolverTests(output : ITestOutputHelper) =


    let wedgeAt40DegreesS =
        let incidenceAngle = IncidenceAngle.normal
        
        let refractionIndex = RefractionIndex.transparentGlass150
        let opticalSystem = OpticalSystem.totalReflGlass150System.baseSystem
        
        let waveLength = WaveLength.nm 600.0
        let n1SinFita = N1SinFita.create refractionIndex incidenceAngle

        {
            description = OpticalSystem.totalReflGlass150System.description |> Option.defaultValue String.Empty
            opticalSystem = OpticalSystem.wedgelGlass150System
            info =
                {
                    waveLength = waveLength
                    refractionIndex = refractionIndex
                    incidenceAngle = incidenceAngle
                    polarization = Polarization.defaultValue
                    ellipticity = Ellipticity.defaultValue
                }
            expected =
                {
                    incident =
                        {
                            waveLength = waveLength
                            n1SinFita = n1SinFita
                            opticalProperties = opticalSystem.upper
                            e =
                                [ 0.766044; 0.; -0.642788 ]
                                |> E.fromRe
                            h =
                                [ 0.; 1.5; 0. ]
                                |> H.fromRe
                        }
                    reflected =
                        {
                            waveLength = waveLength
                            n1SinFita = n1SinFita
                            opticalProperties = opticalSystem.upper
                            e =
                                [ -0.242322; 0.; -0.203333 ]
                                |> E.fromRe
                            h =
                                [ 0.; 0.474494; 0. ]
                                |> H.fromRe
                        }
                    transmitted =
                        {
                            waveLength = waveLength
                            n1SinFita = n1SinFita
                            opticalProperties = opticalSystem.lower
                            e =
                                [ 0.523722; 0.; -1.90377; ]
                                |> E.fromRe
                            h =
                                [ 0.; 1.97449; 0. ]
                                |> H.fromRe
                        }
                }

            stokes = None
//                {
//                    incidentStokes = [ 1.; 1.; 0.; 0. ] |> StokesVector.create
//                    reflectedStokes = [ 0.0417427189970538; 0.0417427189970538; 0.; 0. ] |> StokesVector.create
//                    transmittedStokes  = [ 0.6277542496577975; 0.6277542496577975; 0.; 0. ] |> StokesVector.create
//                } |> Some
        }


