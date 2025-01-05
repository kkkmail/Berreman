namespace BerremanTests

open System

open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Solvers
open Berreman.FieldFunctions
open Berreman.Constants

open MatrixComparison
open Berreman.Media
open OpticalProperties.Standard
open OpticalProperties.Active

open Xunit
open Xunit.Abstractions
open FluentAssertions
open FluentAssertions.Execution


type ResultComparisionType =
    | Field
    | Intensity


type TestData =
    {
        description : string
        info : IncidentLightInfo
        expected : EmFieldSystemValue
        stokes : StokesSystem option
    }


type OpticalSystemTestData =
    {
        opticalSystem : OpticalSystem
        testData : TestData
    }


type BaseOpticalSystemTestData =
    {
        baseOpticalSystem : BaseOpticalSystem
        testData : TestData
    }

    member sys.opticalSystemTestData : OpticalSystemTestData =
        {
            testData = sys.testData
            opticalSystem = sys.baseOpticalSystem.fullSystem
        }


type SolverTests(output : ITestOutputHelper) =

    let addLayer (l : Layer) (d : OpticalSystemTestData) =
        { d with opticalSystem = { d.opticalSystem with films = l :: d.opticalSystem.films } }


    let rec addLayers (ls : List<Layer>) (d : OpticalSystemTestData) =
        match (ls |> List.rev) with
        | [] -> d
        | h :: t -> addLayers (t |> List.rev) (addLayer h d)


    let stdGlassLayer =
        {
            properties = OpticalProperties.transparentGlass
            thickness = Thickness.nm 100.0<nm>
        }


    let vacuumLayer =
        {
            properties = OpticalProperties.vacuum
            thickness = Thickness.nm 150.0<nm>
        }


    let createStdGlassLightAt7Degrees description =
        let opticalProperties = OpticalProperties.transparentGlass
        let incidenceAngle = Angle.degree 7.0 |> IncidenceAngle
        let waveLength = WaveLength.nm 600.0<nm>
        let n1SinFita = N1SinFita.create RefractionIndex.vacuum incidenceAngle

        {
            testData =
                {
                description = description
                info =
                    {
                        waveLength = waveLength
                        refractionIndex = RefractionIndex.vacuum
                        incidenceAngle = incidenceAngle
                        polarization = Polarization.defaultValue
                        ellipticity = Ellipticity.defaultValue
                    }
                expected =
                    {
                        incidentValue =
                            {
                                waveLength = waveLength
                                opticalProperties = OpticalProperties.vacuum
                                e =
                                    [ 0.992546151641322; 0.; -0.12186934340514745 ]
                                    |> E.fromRe
                                h =
                                    [ 0.; 0.9999999999999998; 0. ]
                                    |> H.fromRe
                            }
                        reflectedValue =
                            {
                                waveLength = waveLength
                                opticalProperties = OpticalProperties.vacuum
                                e =
                                    [ -0.2027874513413428; 0.; -0.024899168169565895 ]
                                    |> E.fromRe
                                h =
                                    [ 0.; 0.2043103497061609; 0. ]
                                    |> H.fromRe
                            }
                        transmittedValue =
                            {
                                waveLength = waveLength
                                opticalProperties = opticalProperties
                                e =
                                    [ 0.7897587002999794; 0.; -0.06352515217049573; ]
                                    |> E.fromRe
                                h =
                                    [ 0.; 1.2043103497061607; 0. ]
                                    |> H.fromRe
                            }
                    }

                stokes =
                    {
                        incidentStokes = [ 1.; 1.; 0.; 0. ] |> StokesVector.create
                        reflectedStokes = [ 0.0417427189970538; 0.0417427189970538; 0.; 0. ] |> StokesVector.create
                        transmittedStokes  = [ 0.6277542496577975; 0.6277542496577975; 0.; 0. ] |> StokesVector.create
                    } |> Some
                }

            opticalSystem =
                {
                    description = Some description
                    upper = OpticalProperties.vacuum
                    films =
                        [
                        ]
                    lower = opticalProperties
                }.fullSystem
        }


    let totalReflectionAt40Degrees =
        let incidenceAngle = Angle.degree 40.0 |> IncidenceAngle

        let refractionIndex = RefractionIndex.transparentGlass150
        let opticalSystem = OpticalSystem.totalReflGlass150System

        let waveLength = WaveLength.nm 600.0<nm>
        let n1SinFita = N1SinFita.create refractionIndex incidenceAngle

        {
            opticalSystem = opticalSystem

            testData =
                {
                    description = OpticalSystem.totalReflGlass150System.description |> Option.defaultValue String.Empty
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
                            incidentValue =
                                {
                                    waveLength = waveLength
                                    opticalProperties = opticalSystem.upper
                                    e =
                                        [ 0.766044; 0.; -0.642788 ]
                                        |> E.fromRe
                                    h =
                                        [ 0.; 1.5; 0. ]
                                        |> H.fromRe
                                }
                            reflectedValue =
                                {
                                    waveLength = waveLength
                                    opticalProperties = opticalSystem.upper
                                    e =
                                        [ -0.242322; 0.; -0.203333 ]
                                        |> E.fromRe
                                    h =
                                        [ 0.; 0.474494; 0. ]
                                        |> H.fromRe
                                }
                            transmittedValue =
                                {
                                    waveLength = waveLength
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
        }


    let totalReflectionAt50DegreesS =
        let incidenceAngle = Angle.degree 50.0 |> IncidenceAngle

        let refractionIndex = RefractionIndex.transparentGlass150
        let opticalSystem = OpticalSystem.totalReflGlass150System

        let waveLength = WaveLength.nm 600.0<nm>
        let n1SinFita = N1SinFita.create refractionIndex incidenceAngle

        {
            opticalSystem = opticalSystem

            testData =
                {
                    description = opticalSystem.description |> Option.defaultValue String.Empty
                    info =
                        {
                            waveLength = waveLength
                            refractionIndex = refractionIndex
                            incidenceAngle = incidenceAngle
                            polarization = Polarization.s
                            ellipticity = Ellipticity.defaultValue
                        }
                    expected =
                        {
                            incidentValue =
                                {
                                    waveLength = waveLength
                                    opticalProperties = opticalSystem.upper
                                    e =
                                        [ 0.642788; 0.; -0.766044 ]
                                        |> E.fromRe
                                    h =
                                        [ 0.; 1.5; 0. ]
                                        |> H.fromRe
                                }
                            reflectedValue =
                                {
                                    waveLength = waveLength
                                    opticalProperties = opticalSystem.upper
                                    e =
                                        [ 0.642788; 0.; 0.766044 ]
                                        |> E.fromRe
                                    h =
                                        [ 0.; -1.5; 0. ]
                                        |> H.fromRe
                                }
                            transmittedValue =
                                {
                                    waveLength = waveLength
                                    opticalProperties = opticalSystem.lower
                                    e =
                                        [ 0.0; 0.; 0.0 ]
                                        |> E.fromRe
                                    h =
                                        [ 0.0; 0.0; 0.0 ]
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
        }


    let totalReflectionAt50DegreesP =
        let incidenceAngle = Angle.degree 50.0 |> IncidenceAngle

        let refractionIndex = RefractionIndex.transparentGlass150
        let opticalSystem = OpticalSystem.totalReflGlass150System

        let waveLength = WaveLength.nm 600.0<nm>
        let n1SinFita = N1SinFita.create refractionIndex incidenceAngle

        {
            opticalSystem = opticalSystem

            testData =
                {
                    description = opticalSystem.description |> Option.defaultValue String.Empty
                    info =
                        {
                            waveLength = waveLength
                            refractionIndex = refractionIndex
                            incidenceAngle = incidenceAngle
                            polarization = Polarization.p
                            ellipticity = Ellipticity.defaultValue
                        }
                    expected =
                        {
                            incidentValue =
                                {
                                    waveLength = waveLength
                                    opticalProperties = opticalSystem.upper
                                    e =
                                        [ 0.0; 1.0; 0.0 ]
                                        |> E.fromRe
                                    h =
                                        [ -0.964181; 0.0; 1.14907 ]
                                        |> H.fromRe
                                }
                            reflectedValue =
                                {
                                    waveLength = waveLength
                                    opticalProperties = opticalSystem.upper
                                    e =
                                        [ 0.0; -1.0; 0.0 ]
                                        |> E.fromRe
                                    h =
                                        [ -0.964181; 0.0; -1.14907 ]
                                        |> H.fromRe
                                }
                            transmittedValue =
                                {
                                    waveLength = waveLength
                                    opticalProperties = opticalSystem.lower
                                    e =
                                        [ 0.0; 0.; 0.0 ]
                                        |> E.fromRe
                                    h =
                                        [ 0.0; 0.0; 0.0 ]
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
        }


    let wedgeAt00Degrees =
        let incidenceAngle = IncidenceAngle.normal
        let refractionIndex = RefractionIndex.vacuum
        let opticalSystem = OpticalSystem.getWedgeGlass150Thickness1mmSystem WedgeAngle.defaultValue

        let waveLength = WaveLength.nm 600.0<nm>
        let n1SinFita = N1SinFita.create refractionIndex incidenceAngle

        {
            opticalSystem = opticalSystem

            testData =
                {
                    description = opticalSystem.description |> Option.defaultValue String.Empty
                    info =
                        {
                            waveLength = waveLength
                            refractionIndex = refractionIndex
                            incidenceAngle = incidenceAngle
                            polarization = Polarization.s
                            ellipticity = Ellipticity.defaultValue
                        }
                    expected =
                        {
                            incidentValue =
                                {
                                    waveLength = waveLength
                                    opticalProperties = opticalSystem.upper
                                    e =
                                        [ 1.0; 0.; 0. ]
                                        |> E.fromRe
                                    h =
                                        [ 0.; 1.0; 0. ]
                                        |> H.fromRe
                                }
                            reflectedValue =
                                {
                                    waveLength = waveLength
                                    opticalProperties = opticalSystem.upper
                                    e =
                                        [ -0.2; 0.; 0. ]
                                        |> E.fromRe
                                    h =
                                        [ 0.; 0.2; 0. ]
                                        |> H.fromRe
                                }
                            transmittedValue =
                                {
                                    waveLength = waveLength
                                    opticalProperties = opticalSystem.lower
                                    e =
                                        [ 0.96; 0.0; 0.0; ]
                                        |> E.fromRe
                                    h =
                                        [ 0.; 0.96; 0. ]
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
        }


    let wedgeAt07DegreesS =
        let incidenceAngle = IncidenceAngle.normal
        let refractionIndex = RefractionIndex.vacuum
        let opticalSystem = OpticalSystem.getWedgeGlass150Thickness1mmSystem (7.0 |> Angle.degree |> WedgeAngle)

        let waveLength = WaveLength.nm 600.0<nm>
        let n1SinFita = N1SinFita.create refractionIndex incidenceAngle

        {
            opticalSystem = opticalSystem

            testData =
                {
                    description = opticalSystem.description |> Option.defaultValue String.Empty
                    info =
                        {
                            waveLength = waveLength
                            refractionIndex = refractionIndex
                            incidenceAngle = incidenceAngle
                            polarization = Polarization.s
                            ellipticity = Ellipticity.defaultValue
                        }
                    expected =
                        {
                            incidentValue =
                                {
                                    waveLength = waveLength
                                    opticalProperties = opticalSystem.upper
                                    e =
                                        [ 1.0; 0.; 0. ]
                                        |> E.fromRe
                                    h =
                                        [ 0.; 1.0; 0. ]
                                        |> H.fromRe
                                }
                            reflectedValue =
                                {
                                    waveLength = waveLength
                                    opticalProperties = opticalSystem.upper
                                    e =
                                        [ -0.2; 0.; 0. ]
                                        |> E.fromRe
                                    h =
                                        [ 0.; 0.2; 0. ]
                                        |> H.fromRe
                                }
                            transmittedValue =
                                {
                                    waveLength = waveLength
                                    opticalProperties = opticalSystem.lower
                                    e =
                                        [ 0.96; 0.0; 0.0; ]
                                        |> E.fromRe
                                    h =
                                        [ 0.; 0.96; 0. ]
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
        }


    let wedgeAt40DegreesS =
        let incidenceAngle = IncidenceAngle.normal
        let refractionIndex = RefractionIndex.vacuum
        let opticalSystem = OpticalSystem.wedge40DegGlass150System

        let waveLength = WaveLength.nm 600.0<nm>
        let n1SinFita = N1SinFita.create refractionIndex incidenceAngle

        {
            opticalSystem = opticalSystem

            testData =
                {
                    description = opticalSystem.description |> Option.defaultValue String.Empty
                    info =
                        {
                            waveLength = waveLength
                            refractionIndex = refractionIndex
                            incidenceAngle = incidenceAngle
                            polarization = Polarization.s
                            ellipticity = Ellipticity.defaultValue
                        }
                    expected =
                        {
                            incidentValue =
                                {
                                    waveLength = waveLength
                                    opticalProperties = opticalSystem.upper
                                    e =
                                        [ 1.0; 0.; 0. ]
                                        |> E.fromRe
                                    h =
                                        [ 0.; 1.0; 0. ]
                                        |> H.fromRe
                                }
                            reflectedValue =
                                {
                                    waveLength = waveLength
                                    opticalProperties = opticalSystem.upper
                                    e =
                                        [ -0.2; 0.; 0. ]
                                        |> E.fromRe
                                    h =
                                        [ 0.; 0.2; 0. ]
                                        |> H.fromRe
                                }
                            transmittedValue =
                                {
                                    waveLength = waveLength
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
        }


    let wedgeAt40DegreesP =
        let incidenceAngle = IncidenceAngle.normal
        let refractionIndex = RefractionIndex.vacuum
        let opticalSystem = OpticalSystem.wedge40DegGlass150System

        let waveLength = WaveLength.nm 600.0<nm>
        let n1SinFita = N1SinFita.create refractionIndex incidenceAngle

        {
            opticalSystem = opticalSystem

            testData =
                {
                    description = opticalSystem.description |> Option.defaultValue String.Empty
                    info =
                        {
                            waveLength = waveLength
                            refractionIndex = refractionIndex
                            incidenceAngle = incidenceAngle
                            polarization = Polarization.p
                            ellipticity = Ellipticity.defaultValue
                        }
                    expected =
                        {
                            incidentValue =
                                {
                                    waveLength = waveLength
                                    opticalProperties = opticalSystem.upper
                                    e =
                                        [ 1.0; 0.; 0. ]
                                        |> E.fromRe
                                    h =
                                        [ 0.; 1.0; 0. ]
                                        |> H.fromRe
                                }
                            reflectedValue =
                                {
                                    waveLength = waveLength
                                    opticalProperties = opticalSystem.upper
                                    e =
                                        [ -0.2; 0.; 0. ]
                                        |> E.fromRe
                                    h =
                                        [ 0.; 0.2; 0. ]
                                        |> H.fromRe
                                }
                            transmittedValue =
                                {
                                    waveLength = waveLength
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
        }


    let wedgeAt50DegreesS =
        let incidenceAngle = IncidenceAngle.normal
        let refractionIndex = RefractionIndex.vacuum
        let opticalSystem = OpticalSystem.wedge50DegGlass150System

        let waveLength = WaveLength.nm 600.0<nm>
        let n1SinFita = N1SinFita.create refractionIndex incidenceAngle

        {
            opticalSystem = opticalSystem

            testData =
                {
                    description = opticalSystem.description |> Option.defaultValue String.Empty
                    info =
                        {
                            waveLength = waveLength
                            refractionIndex = refractionIndex
                            incidenceAngle = incidenceAngle
                            polarization = Polarization.s
                            ellipticity = Ellipticity.defaultValue
                        }
                    expected =
                        {
                            incidentValue =
                                {
                                    waveLength = waveLength
                                    opticalProperties = opticalSystem.upper
                                    e =
                                        [ 1.0; 0.; 0. ]
                                        |> E.fromRe
                                    h =
                                        [ 0.; 1.0; 0. ]
                                        |> H.fromRe
                                }
                            reflectedValue =
                                {
                                    waveLength = waveLength
                                    opticalProperties = opticalSystem.upper
                                    e =
                                        [ -0.2; 0.; 0. ]
                                        |> E.fromRe
                                    h =
                                        [ 0.; 0.2; 0. ]
                                        |> H.fromRe
                                }
                            transmittedValue =
                                {
                                    waveLength = waveLength
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
        }


    let data =
        [
            createStdGlassLightAt7Degrees "Snell's law for standard transparent glass, 7 degrees incidence angle."

            createStdGlassLightAt7Degrees "Snell's law for thin standard glass film on standard transparent glass, 7 degrees incidence angle."
            |> addLayer stdGlassLayer

            createStdGlassLightAt7Degrees "Snell's law for vacuum film on standard transparent glass, 7 degrees incidence angle."
            |> addLayer vacuumLayer

            createStdGlassLightAt7Degrees "Snell's law for vacuum film + thin standard glass film on standard transparent glass, 7 degrees incidence angle."
            |> addLayer stdGlassLayer
            |> addLayer vacuumLayer
        ]


    let randomData =
        let opticalProperties = OpticalProperties.transparentGlass
        let incidenceAngle = Angle.degree 50.0 |> IncidenceAngle
        let waveLength = WaveLength.nm 394.0<nm>
        let n1SinFita = N1SinFita.create RefractionIndex.vacuum incidenceAngle
        let beta = Angle.degree 28.0
        let ellipticity = Ellipticity 0.41378575406900664

        {
            opticalSystem =
                {
                    description = None
                    upper = OpticalProperties.vacuum
                    films =
                        [
                            {
                                properties =
                                    {
                                        eps =
                                            [
                                                [ createComplex 2.9951561294777456 0.004168150397966634; createComplex -0.5829262349784788 0.00044895006717315185; createComplex 1.1068496972419206 -0.0007575417483233533 ]
                                                [ createComplex -0.5829262349784788 0.00044895006717315185; createComplex 3.2091311788032657 0.0026794460594853266; createComplex 0.1037267395830751 0.0002308596522789394 ]
                                                [ createComplex 1.1068496972419206 -0.0007575417483233533; createComplex 0.1037267395830751 0.00023085965227893945; createComplex 3.8412649402347565 0.005053313374866639 ]
                                            ]
                                            |> Eps.create
                                        mu =
                                            [
                                                [ 1.0465011813510727; 0.005487385174496151; -0.0033534986252906213 ]
                                                [ 0.0054873851744962066; 1.027948494963144; 0.0035387414684809326 ]
                                                [ -0.0033534986252905657; 0.0035387414684808494; 1.0436182675132528 ]
                                            ]
                                            |> Mu.fromRe
                                        rho =
                                            [
                                                [ -0.05646672882733954; -0.000710796791700627; 0.0116005197723429 ]
                                                [ -0.0007107967917006237; -0.05672604545420977; -0.014253526385969741 ]
                                                [ 0.0116005197723429; -0.014253526385969741; -0.017920566339098533 ]
                                            ]
                                            |> Rho.fromIm
                                    }
                                thickness = Thickness.nm 86.0<nm>
                            }
                        ]
                    lower = opticalProperties
                }.fullSystem

            testData =
                {
                    description = "Random 1-layer film (r04), incidence angle 50 degrees."
                    info =
                        {
                            waveLength = waveLength
                            refractionIndex = RefractionIndex.vacuum
                            incidenceAngle = incidenceAngle
                            polarization = Polarization beta
                            ellipticity = ellipticity
                        }
                    expected =
                        {
                            incidentValue =
                                {
                                    waveLength = waveLength
                                    opticalProperties = OpticalProperties.vacuum
                                    e =
                                        [ createComplex 0.5244250568473124 -0.1153807433449855; createComplex 0.4338007527265541 0.3375914755203022; createComplex -0.6249854455442077 0.13750541539757702 ]
                                        |> E.create
                                    h =
                                        [ createComplex -0.27884174892532315 -0.21699961760024683; createComplex 0.8158605563399906 -0.17950057158265997; createComplex 0.3323106560470066 0.258610073866664 ]
                                        |> H.create
                                }
                            reflectedValue =
                                {
                                    waveLength = waveLength
                                    opticalProperties = OpticalProperties.vacuum
                                    e =
                                        [ createComplex -0.007302575642388954 0.024443690065852296; createComplex -0.08450713524591316 -0.15735967361301947; createComplex -0.008702870757008006 0.029130855452238875 ]
                                        |> E.create
                                    h =
                                        [ createComplex -0.0543201394661776 -0.10114884846276676; createComplex 0.011360790924314978 -0.03802763105183758; createComplex -0.0647362213590357 -0.12054450354226964 ]
                                        |> H.create
                                }
                            transmittedValue =
                                {
                                    waveLength = waveLength
                                    opticalProperties = opticalProperties
                                    e =
                                        [ createComplex 0.07161377408818743 0.5399618144532632; createComplex -0.20383484246715727 0.17727725562901583; createComplex -0.041786434962015576 -0.3150661940234894 ]
                                        |> E.create
                                    h =
                                        [ createComplex 0.2676046341577096 -0.2327384983003056; createComplex 0.1260284311223 0.950243737436539; createComplex -0.1561465483859981 0.13580225656599015 ]
                                        |> H.create
                                }
                        }

                    stokes = None
                        //{
                        //    incidentStokes = [ 1.; 1.; 0.; 0. ] |> StokesVector.create
                        //    reflectedStokes = [ 0.0417427189970538; 0.0417427189970538; 0.; 0. ] |> StokesVector.create
                        //    transmittedStokes  = [ 0.6277542496577975; 0.6277542496577975; 0.; 0. ] |> StokesVector.create
                        //} |> Some
                }
        }


    /// Random optical properties are in the film there.
    let randomProperties = randomData.opticalSystem.films.Head.properties


    let runTest (d : OpticalSystemTestData) (c : ResultComparisionType) =
        let t = d.testData

        output.WriteLine t.description
        use e = new AssertionScope()
        let solver = OpticalSystemSolver (t.info, d.opticalSystem)

        let emSys = solver.solution.emSys

        //output.WriteLine("eigenBasisUpper = {0}\n", solver.eigenBasisUpper)
        //output.WriteLine("eigenBasisFilm = {0}\n", solver.eigenBasisFilm)
        //output.WriteLine("eigenBasisLower = {0}\n", solver.eigenBasisLower)
        //output.WriteLine("coeffTbl = {0}\n", solver.coeffTbl)
        //output.WriteLine("freeTbl = {0}\n", solver.freeTbl)
        //output.WriteLine("cfm = {0}\n", solver.cfm)

        output.WriteLine("stokesVector (I) = {0}\n", emSys.incident.stokesVector)
        output.WriteLine("stokesVector (R) = {0}\n", emSys.reflected.stokesVector)
        output.WriteLine("stokesVector (T) = {0}\n", emSys.transmitted.stokesVector)

        output.WriteLine("I = {0}\n", emSys.i)
        output.WriteLine("Ip = {0}\n", emSys.ip)
        output.WriteLine("Is = {0}\n", emSys.is)

        output.WriteLine("R = {0}\n", emSys.r)
        output.WriteLine("Rp = {0}\n", emSys.rp)
        output.WriteLine("Rs = {0}\n", emSys.rs)

        output.WriteLine("T = {0}\n", emSys.t)
        output.WriteLine("Tp = {0}\n", emSys.tp)
        output.WriteLine("Ts = {0}\n", emSys.ts)

        let eI = emSys.incident.e
        let hI = emSys.incident.h

        let eR = emSys.reflected.e
        let hR = emSys.reflected.h

        let eT = emSys.transmitted.e
        let hT = emSys.transmitted.h

        match c with
        | Field ->
            verifyVectorEqualityE output "eI" eI t.expected.incidentValue.e
            verifyVectorEqualityH output "hI" hI t.expected.incidentValue.h

            verifyVectorEqualityE output "eR" eR t.expected.reflectedValue.e
            verifyVectorEqualityH output "hR" hR t.expected.reflectedValue.h

            verifyVectorEqualityE output "eT" eT t.expected.transmittedValue.e
            verifyVectorEqualityH output "hT" hT t.expected.transmittedValue.h
        | Intensity ->
            Skip.If(true, "Intensity based checks are not implemented yet.")


    let runTestMuellerMatrixR descr info (sys : OpticalSystem) =
        output.WriteLine descr
        let solver = OpticalSystemSolver(info, sys)
        let mr = solver.muellerMatrixR()
        let i = solver.solution.stokesI
        let r = solver.solution.stokesR

        let r1 = mr * i

        output.WriteLine("stokesVector (I)  = {0}\n", i)
        output.WriteLine("stokesVector (R)  = {0}\n", r)
        output.WriteLine("stokesVector (R1) = {0}\n", r1)
        output.WriteLine("muellerMatrix (R) = {0}\n", mr)

        match solver.solution with
        | Single s ->
            output.WriteLine $"incident.complexBasis = %A{s.emSys.incident.complexBasis}"
            output.WriteLine $"reflected.complexBasis = %A{s.emSys.reflected.complexBasis}"
        | Multiple _ -> output.WriteLine "Multiple solution is not supported yet."

        verifyVectorEqualityStokes output "[Stokes vector R] vs mR * [Stokes vector I]" r r1


    let runTestMuellerMatrixR1 (d : OpticalSystemTestData) =
        runTestMuellerMatrixR d.testData.description d.testData.info d.opticalSystem


    let runTestMuellerMatrixT descr info (sys : OpticalSystem) =
        output.WriteLine descr
        let solver = OpticalSystemSolver(info, sys)
        let mt = solver.muellerMatrixT()
        let i = solver.solution.stokesI
        let t = solver.solution.stokesT

        let t1 = mt * i

        output.WriteLine("stokesVector (I)  = {0}\n", i)
        output.WriteLine("stokesVector (T)  = {0}\n", t)
        output.WriteLine("stokesVector (T1) = {0}\n", t1)
        output.WriteLine("muellerMatrix (T) = {0}\n", mt)

        verifyVectorEqualityStokes output "[Stokes vector T] vs mT * [Stokes vector I]" t t1


    let runTestMuellerMatrixT1 (d : BaseOpticalSystemTestData) =
        runTestMuellerMatrixT d.testData.description d.testData.info d.baseOpticalSystem.fullSystem


    [<Fact>]
    member _.basicSolverTest0 () = runTest (data.[0]) Field

    [<SkippableFact>]
    member _.basicSolverTest1 () = runTest (data.[1]) Intensity

    [<SkippableFact>]
    member _.basicSolverTest2 () = runTest (data.[2]) Intensity

    [<SkippableFact>]
    member _.basicSolverTest3 () = runTest (data.[3]) Intensity

    [<Fact>]
    member _.basicSolverTestRandom () = runTest randomData Field

    [<Fact>]
    member _.muellerMatrixR_Test0 () = runTestMuellerMatrixR1 (data.[0])

    [<Fact>]
    member _.muellerMatrixR_Test1 () = runTestMuellerMatrixR1 (data.[1])

    [<Fact>]
    member _.muellerMatrixR_Test2 () = runTestMuellerMatrixR1 (data.[2])

    [<Fact>]
    member _.muellerMatrixR_Test3 () = runTestMuellerMatrixR1 (data.[3])

    [<Fact>]
    member _.muellerMatrixR_Random () = runTestMuellerMatrixR1 randomData

    [<Fact>]
    member _.muellerMatrixR_TransparentGlassSystem () =
        let descr = "Transparent glass, inclined 19 degrees incident light."
        let info = light600nmInclinedDegreeLPs 19.0
        runTestMuellerMatrixR descr info BaseOpticalSystem.transparentGlassSystem.fullSystem

    [<Fact>]
    member _.muellerMatrixT_TransparentGlassSystem () =
        let descr = "Transparent glass, inclined 19 degrees incident light."
        let info = light600nmInclinedDegreeLPs 19.0
        runTestMuellerMatrixT descr info BaseOpticalSystem.transparentGlassSystem.fullSystem

    [<Fact>]
    member _.muellerMatrixR_BiaxialCrystalSystem () =
        let descr = "Biaxial Crystal, inclined 19 degrees incident light."
        let info = light600nmInclinedDegreeLPs 19.0
        runTestMuellerMatrixR descr info BaseOpticalSystem.biaxialCrystalSystem.fullSystem

    [<Fact>]
    member _.muellerMatrixR_TransparentGlassFilmSystem () =
        let descr = "Transparent glass 100 nm thin film, inclined 19 degrees incident light."
        let info = light600nmInclinedDegreeLPs 19.0
        runTestMuellerMatrixR descr info (BaseOpticalSystem.transparentGlassFilmSystem (Thickness.nm 100.0<nm>)).fullSystem

    [<Fact>]
    member _.muellerMatrixR_TransparentGlassSystem_Polarized () =
        let descr = "Transparent glass, inclined 19 degrees incident light, 27 degrees polarization plane angle."
        let info = { light600nmInclinedDegreeLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization }
        runTestMuellerMatrixR descr info BaseOpticalSystem.transparentGlassSystem.fullSystem

    [<Fact>]
    member _.muellerMatrixR_BiaxialCrystalSystem_Polarized () =
        let descr = "Biaxial Crystal, inclined 19 degrees incident light, 27 degrees polarization plane angle."
        let info = { light600nmInclinedDegreeLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization }
        runTestMuellerMatrixR descr info BaseOpticalSystem.biaxialCrystalSystem.fullSystem

    [<Fact>]
    member _.muellerMatrixR_TransparentGlassFilmSystem_Polarized () =
        let descr = "Transparent glass 100 nm thin film, inclined 19 degrees incident light, 27 degrees polarization plane angle."
        let info = { light600nmInclinedDegreeLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization }
        runTestMuellerMatrixR descr info (BaseOpticalSystem.transparentGlassFilmSystem (Thickness.nm 100.0<nm>)).fullSystem

    [<Fact>]
    member _.muellerMatrixR_TransparentGlassSystem_Polarized_WithEllipticity () =
        let descr = "Transparent glass, inclined 19 degrees incident light, 27 degrees polarization plane angle, with ellipticity 0.58."
        let info = { light600nmInclinedDegreeLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization; ellipticity = Ellipticity 0.58 }
        runTestMuellerMatrixR descr info BaseOpticalSystem.transparentGlassSystem.fullSystem

    [<Fact>]
    member _.muellerMatrixR_BiaxialCrystalSystem_Polarized_WithEllipticity () =
        let descr = "Biaxial Crystal, inclined 19 degrees incident light, 27 degrees polarization plane angle, with ellipticity 0.58."
        let info = { light600nmInclinedDegreeLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization; ellipticity = Ellipticity 0.58 }
        runTestMuellerMatrixR descr info BaseOpticalSystem.biaxialCrystalSystem.fullSystem

    [<Fact>]
    member _.muellerMatrixR_BiaxialCrystalSystemRotatedX_Polarized_WithEllipticity () =
        let descr = "Biaxial Crystal, rotated around X, inclined 19 degrees incident light, 27 degrees polarization plane angle, with ellipticity 0.58."
        let info = { light600nmInclinedDegreeLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization; ellipticity = Ellipticity 0.58 }

        let eps = (BaseOpticalSystem.biaxialCrystalSystem.lower.rotateX (Angle.degree 30.0)).eps
        let sys = { BaseOpticalSystem.biaxialCrystalSystem with lower.eps = eps}
        output.WriteLine $"eps = %A{eps}"
        output.WriteLine $"sys = %A{sys}"
        runTestMuellerMatrixR descr info sys.fullSystem

    [<Fact>]
    member _.muellerMatrixR_BiaxialCrystalSystemRotatedY_Polarized_WithEllipticity () =
        let descr = "Biaxial Crystal, rotated around Y, inclined 19 degrees incident light, 27 degrees polarization plane angle, with ellipticity 0.58."
        let info = { light600nmInclinedDegreeLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization; ellipticity = Ellipticity 0.58 }

        let eps = (BaseOpticalSystem.biaxialCrystalSystem.lower.rotateY (Angle.degree 30.0)).eps
        let sys = { BaseOpticalSystem.biaxialCrystalSystem with lower.eps = eps}
        output.WriteLine $"eps = %A{eps}"
        output.WriteLine $"sys = %A{sys}"
        runTestMuellerMatrixR descr info sys.fullSystem

    [<Fact>]
    member _.muellerMatrixR_BiaxialCrystalSystemRotatedZ_Polarized_WithEllipticity () =
        let descr = "Biaxial Crystal, rotated around Z, inclined 19 degrees incident light, 27 degrees polarization plane angle, with ellipticity 0.58."
        let info = { light600nmInclinedDegreeLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization; ellipticity = Ellipticity 0.58 }

        let eps = (BaseOpticalSystem.biaxialCrystalSystem.lower.rotateZ (Angle.degree 30.0)).eps
        let sys = { BaseOpticalSystem.biaxialCrystalSystem with lower.eps = eps}
        output.WriteLine $"eps = %A{eps}"
        output.WriteLine $"sys = %A{sys}"
        runTestMuellerMatrixR descr info sys.fullSystem

    [<Fact>]
    member _.muellerMatrixR_BiaxialCrystalFilmSystem_Polarized_WithEllipticity () =
        let descr = "Biaxial Crystal 100 nm, inclined 19 degrees incident light, 27 degrees polarization plane angle, with ellipticity 0.58."
        let info = { light600nmInclinedDegreeLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization; ellipticity = Ellipticity 0.58 }
        runTestMuellerMatrixR descr info (BaseOpticalSystem.biaxialCrystalFilmSystem (Thickness.nm 100.0<nm>)).fullSystem

    [<Fact>]
    member _.muellerMatrixR_RandomAbsorbingSystem_Polarized_WithEllipticity () =
        let descr = "Random absorbing system, inclined 19 degrees incident light, 27 degrees polarization plane angle, with ellipticity 0.58."
        let info = { light600nmInclinedDegreeLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization; ellipticity = Ellipticity 0.58 }

        let sys = { BaseOpticalSystem.transparentGlassSystem with lower.eps = randomProperties.eps}
        runTestMuellerMatrixR descr info sys.fullSystem

    [<Fact>]
    member _.muellerMatrixR_RandomSystem_Polarized_WithEllipticity () =
        let descr = "Random system, inclined 19 degrees incident light, 27 degrees polarization plane angle, with ellipticity 0.58."
        let info = { light600nmInclinedDegreeLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization; ellipticity = Ellipticity 0.58 }

        let eps = randomProperties.eps.re
        let sys = { BaseOpticalSystem.transparentGlassSystem with lower.eps = eps}
        output.WriteLine $"eps = %A{eps}"
        output.WriteLine $"sys = %A{sys}"
        runTestMuellerMatrixR descr info sys.fullSystem

    [<Fact>]
    member _.muellerMatrixR_RandomMagneticSystem_Polarized_WithEllipticity () =
        let descr = "Random magnetic system, inclined 19 degrees incident light, 27 degrees polarization plane angle, with ellipticity 0.58."
        let info = { light600nmInclinedDegreeLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization; ellipticity = Ellipticity 0.58 }

        let sys = { BaseOpticalSystem.transparentGlassSystem with lower.mu = randomProperties.mu}
        runTestMuellerMatrixR descr info sys.fullSystem

    [<Fact>]
    member _.muellerMatrixR_RandomOpticallyActiveSystem_Polarized_WithEllipticity () =
        let descr = "Random optically active system, inclined 19 degrees incident light, 27 degrees polarization plane angle, with ellipticity 0.58."
        let info = { light600nmInclinedDegreeLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization; ellipticity = Ellipticity 0.58 }

        let sys = { BaseOpticalSystem.transparentGlassSystem with lower.rho = randomProperties.rho}
        runTestMuellerMatrixR descr info sys.fullSystem

    [<Fact>]
    member _.muellerMatrixR_BiaxialCrystalSubstrateSystem_Polarized_WithEllipticity () =
        let descr = "Biaxial Crystal 1000 nm, inclined 19 degrees incident light, 27 degrees polarization plane angle, with ellipticity 0.58."
        let info = { light600nmInclinedDegreeLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization; ellipticity = Ellipticity 0.58 }
        runTestMuellerMatrixR descr info (OpticalSystem.biaxialCrystalSubstrateSystem (Thickness.nm 1000.0<nm>))

    [<Fact>]
    member _.muellerMatrixT_BiaxialCrystalSubstrateSystem_Polarized_WithEllipticity () =
        let descr = "Biaxial Crystal 1000 nm, inclined 19 degrees incident light, 27 degrees polarization plane angle, with ellipticity 0.58."
        let info = { light600nmInclinedDegreeLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization; ellipticity = Ellipticity 0.58 }
        runTestMuellerMatrixT descr info (OpticalSystem.biaxialCrystalSubstrateSystem (Thickness.nm 1000.0<nm>))

    [<Fact (Skip = "Cannot work.")>]
    member _.muellerMatrixT_BiaxialCrystalWedgeSystem_Polarized_WithEllipticity () =
        let descr = "Biaxial Crystal 1000 nm, NORMAL incident light, 27 degrees polarization plane angle, with ellipticity 0.58."
        let info = { light600nmNormalLPs with polarization = Angle.degree 27.0 |> Polarization; ellipticity = Ellipticity 0.58 }
        runTestMuellerMatrixT descr info (OpticalSystem.biaxialCrystalWedgeSystem (Thickness.nm 1000.0<nm>) (Angle.degree 10.0 |> WedgeAngle))

    [<Fact>]
    member _.muellerMatrixT_TransparentGlassWedgeSystem_Polarized_WithEllipticity () =
        let descr = "Transparent glass 1000 nm, NORMAL incident light, 27 degrees polarization plane angle, with ellipticity 0.58."
        let info = { light600nmNormalLPs with polarization = Angle.degree 27.0 |> Polarization; ellipticity = Ellipticity 0.58 }
        runTestMuellerMatrixT descr info (OpticalSystem.getWedgeGlass150System (Thickness.nm 1000.0<nm>) (Angle.degree 10.0 |> WedgeAngle))

    [<Fact>]
    member _.totalReflectionAt40DegreesTest () = runTest totalReflectionAt40Degrees Field

    [<Fact>]
    member _.totalReflectionAt50DegreesTestS () = runTest totalReflectionAt50DegreesS Field

    [<Fact>]
    member _.totalReflectionAt50DegreesTestP () = runTest totalReflectionAt50DegreesP Field

    [<Fact>]
    member _.wedgeAt00DegreesTest () = runTest wedgeAt00Degrees Field

    [<Fact (Skip = "Set the correct data for the test.")>]
    member _.wedgeAt07DegreesTestS () = runTest wedgeAt07DegreesS Field

    [<Fact (Skip = "Set the correct data for the test.")>]
    member _.wedgeAt40DegreesTestS () = runTest wedgeAt40DegreesS Field

    [<Fact (Skip = "Set the correct data for the test.")>]
    member _.wedgeAt40DegreesTestP () = runTest wedgeAt40DegreesP Field

    [<Fact (Skip = "Set the correct data for the test.")>]
    member _.wedgeAt50DegreesTestS () = runTest wedgeAt50DegreesS Field

    [<Fact>]
    member _.wedgeActiveShouldNotBlowUp () =
        let e11 = 1.5 |> RefractionIndex |> EpsValue.fromRefractionIndex
        let e33 = 1.7 |> RefractionIndex |> EpsValue.fromRefractionIndex
        let r12 = 1.0e-4 |> RhoValue
        let thickness = Thickness.oneMilliMeter
        let polarization = 45.0 |> Angle.degree |> Polarization.create
        let light = { light600nmNormalLPs with polarization = polarization }
        let d = "Planar active crystal."
        let p = OpticalProperties.planarCrystal e11 e33 r12
        let wedgeAngle = 48.58 |> Angle.degree |> WedgeAngle
        let opticalSystem = (OpticalSystem.wedgeSystem p d thickness wedgeAngle)
        let solver = OpticalSystemSolver (light, opticalSystem)

        let emSys = solver.solution.emSys

        use e = new AssertionScope()

        let ts = emSys.ts
        let tp = emSys.tp
        let rs = emSys.rs
        let rp = emSys.rs

        ts.Should().BeLessOrEqualTo(1.0, "ts") |> ignore
        tp.Should().BeLessOrEqualTo(1.0, "tp") |> ignore
        rs.Should().BeLessOrEqualTo(1.0, "rs") |> ignore
        rp.Should().BeLessOrEqualTo(1.0, "rp") |> ignore
