﻿namespace BerremanTests

open Berreman.Constants
open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.BerremanMatrix
open Berreman.Solvers
open Berreman.FieldFunctions

open Xunit
open Xunit.Abstractions

open MatrixComparison
open Berreman.Media
open OpticalProperties.Standard


type BaseOpticalSystemTestData =
    {
        description : string
        opticalSystem : BaseOpticalSystem
        info : IncidentLightInfo
        expected : EmFieldSystem
        stokes : StokesSystem option
    }


type ResultComparisionType = 
    | Field
    | Intensity


type BasicSolverTests(output : ITestOutputHelper) =

    let addLayer (l : Layer) (d : BaseOpticalSystemTestData) = 
        { d with opticalSystem = { d.opticalSystem with films = l :: d.opticalSystem.films } }


    let rec addLayers (ls : List<Layer>) (d : BaseOpticalSystemTestData) = 
        match (ls |> List.rev) with 
        | [] -> d
        | h :: t -> addLayers (t |> List.rev) (addLayer h d)


    let stdGlassLayer = 
        {
            properties = OpticalProperties.transparentGlass
            thickness = Thickness.nm 100.0
        }


    let vacuumLayer = 
        {
            properties = OpticalProperties.vacuum
            thickness = Thickness.nm 150.0
        }


    let createStdGlassLightAt7Degrees description = 
        let opticalProperties = OpticalProperties.transparentGlass
        let incidenceAngle = Angle.degree 7.0 |> IncidenceAngle
        let waveLength = WaveLength.nm 600.0
        let n1SinFita = N1SinFita.create 1.0 incidenceAngle

        {
            description = description
            opticalSystem = 
                {
                    description = Some description
                    upper = OpticalProperties.vacuum
                    films =
                        [
                        ]
                    lower = opticalProperties
                }
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
                    incident = 
                        {
                            waveLength = waveLength
                            n1SinFita = n1SinFita
                            opticalProperties = OpticalProperties.vacuum
                            e = 
                                [ 0.992546151641322; 0.; -0.12186934340514745 ]
                                |> E.fromRe
                            h = 
                                [ 0.; 0.9999999999999998; 0. ]
                                |> H.fromRe
                        }
                    reflected = 
                        {
                            waveLength = waveLength
                            n1SinFita = n1SinFita
                            opticalProperties = OpticalProperties.vacuum
                            e = 
                                [ -0.2027874513413428; 0.; -0.024899168169565895 ]
                                |> E.fromRe
                            h = 
                                [ 0.; 0.2043103497061609; 0. ]
                                |> H.fromRe
                        }
                    transmitted = 
                        {
                            waveLength = waveLength
                            n1SinFita = n1SinFita
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
        let waveLength = WaveLength.nm 394.0
        let n1SinFita = N1SinFita.create 1.0 incidenceAngle
        let beta = Angle.degree 28.0
        let ellipticity = Ellipticity 0.41378575406900664

        {
            description = "Random 1-layer film (r04), incidence angle 50 degrees."
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
                                thickness = Thickness.nm 86.0
                            }
                        ]
                    lower = opticalProperties
                }
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
                    incident = 
                        {
                            waveLength = waveLength
                            n1SinFita = n1SinFita
                            opticalProperties = OpticalProperties.vacuum
                            e = 
                                [ createComplex 0.5244250568473124 -0.1153807433449855; createComplex 0.4338007527265541 0.3375914755203022; createComplex -0.6249854455442077 0.13750541539757702 ]
                                |> E.create
                            h = 
                                [ createComplex -0.27884174892532315 -0.21699961760024683; createComplex 0.8158605563399906 -0.17950057158265997; createComplex 0.3323106560470066 0.258610073866664 ]
                                |> H.create
                        }
                    reflected = 
                        {
                            waveLength = waveLength
                            n1SinFita = n1SinFita
                            opticalProperties = OpticalProperties.vacuum
                            e = 
                                [ createComplex -0.007302575642388954 0.024443690065852296; createComplex -0.08450713524591316 -0.15735967361301947; createComplex -0.008702870757008006 0.029130855452238875 ]
                                |> E.create
                            h = 
                                [ createComplex -0.0543201394661776 -0.10114884846276676; createComplex 0.011360790924314978 -0.03802763105183758; createComplex -0.0647362213590357 -0.12054450354226964 ]
                                |> H.create
                        }
                    transmitted = 
                        {
                            waveLength = waveLength
                            n1SinFita = n1SinFita
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


    /// Random optical properties are in the film there.
    let randomProperties = randomData.opticalSystem.films.Head.properties


    let runTest (d : BaseOpticalSystemTestData) (c : ResultComparisionType) = 
        output.WriteLine d.description
        let solver = BaseOpticalSystemSolver (d.info, d.opticalSystem)

        //output.WriteLine("eigenBasisUpper = {0}\n", solver.eigenBasisUpper)
        //output.WriteLine("eigenBasisFilm = {0}\n", solver.eigenBasisFilm)
        //output.WriteLine("eigenBasisLower = {0}\n", solver.eigenBasisLower)
        //output.WriteLine("coeffTbl = {0}\n", solver.coeffTbl)
        //output.WriteLine("freeTbl = {0}\n", solver.freeTbl)
        //output.WriteLine("cfm = {0}\n", solver.cfm)

        output.WriteLine("stokesVector (I) = {0}\n", solver.emSys.incident.stokesVector)
        output.WriteLine("stokesVector (R) = {0}\n", solver.emSys.reflected.stokesVector)
        output.WriteLine("stokesVector (T) = {0}\n", solver.emSys.transmitted.stokesVector)

        output.WriteLine("I = {0}\n", solver.emSys.i)
        output.WriteLine("Ip = {0}\n", solver.emSys.ip)
        output.WriteLine("Is = {0}\n", solver.emSys.is)

        output.WriteLine("R = {0}\n", solver.emSys.r)
        output.WriteLine("Rp = {0}\n", solver.emSys.rp)
        output.WriteLine("Rs = {0}\n", solver.emSys.rs)

        output.WriteLine("T = {0}\n", solver.emSys.t)
        output.WriteLine("Tp = {0}\n", solver.emSys.tp)
        output.WriteLine("Ts = {0}\n", solver.emSys.ts)

        let eI = solver.emSys.incident.e
        let hI = solver.emSys.incident.h

        let eR = solver.emSys.reflected.e
        let hR = solver.emSys.reflected.h

        let eT = solver.emSys.transmitted.e
        let hT = solver.emSys.transmitted.h

        match c with 
        | Field -> 
            verifyVectorEqualityE output "eI" eI d.expected.incident.e
            verifyVectorEqualityH output "hI" hI d.expected.incident.h

            verifyVectorEqualityE output "eR" eR d.expected.reflected.e
            verifyVectorEqualityH output "hR" hR d.expected.reflected.h

            verifyVectorEqualityE output "eT" eT d.expected.transmitted.e
            verifyVectorEqualityH output "hT" hT d.expected.transmitted.h
        | Intensity ->
            Skip.If(true, "Intensity based checks are not implemented yet.")


    let runTestMuellerMatrixR descr info (sys : BaseOpticalSystem) =
        output.WriteLine descr
        let solver = OpticalSystemSolver(info, sys.fullSystem)
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
            output.WriteLine(sprintf "incident.complexBasis = %A" (s.emSys.incident.complexBasis))
            output.WriteLine(sprintf "reflected.complexBasis = %A" (s.emSys.reflected.complexBasis))
        | Multiple _ -> failwith "Multiple solution is not supported yet."

        verifyVectorEqualityStokes output "[Stokes vector R] vs mR * [Stokes vector I]" r r1


    let runTestMuellerMatrixR1 (d : BaseOpticalSystemTestData) = 
        runTestMuellerMatrixR d.description d.info d.opticalSystem


    [<Fact>]
    member __.basicSolverTest0 () = runTest (data.[0]) Field

    [<SkippableFact>]
    member __.basicSolverTest1 () = runTest (data.[1]) Intensity

    [<SkippableFact>]
    member __.basicSolverTest2 () = runTest (data.[2]) Intensity

    [<SkippableFact>]
    member __.basicSolverTest3 () = runTest (data.[3]) Intensity

    [<Fact>]
    member __.basicSolverTestRandom () = runTest randomData Field

    [<Fact>]
    member __.muellerMatrixR_Test0 () = runTestMuellerMatrixR1 (data.[0])

    // Useless
    //[<Fact>]
    //member __.muellerMatrixR_Test1 () = runTestMuellerMatrixR (data.[1])

    //[<Fact>]
    //member __.muellerMatrixR_Test2 () = runTestMuellerMatrixR (data.[2])

    //[<Fact>]
    //member __.muellerMatrixR_Test3 () = runTestMuellerMatrixR (data.[3])

    // Does not work yet.
    //[<Fact>]
    //member __.muellerMatrixR_Random () = runTestMuellerMatrixR1 randomData

    [<Fact>]
    member __.muellerMatrixR_TransparentGlassSystem () = 
        let descr = "Transparent glass, inclined 19 degrees incident light."
        let info = light600nmInclinedDegreelLPs 19.0
        runTestMuellerMatrixR descr info BaseOpticalSystem.transparentGlassSystem

    [<Fact>]
    member __.muellerMatrixR_BiaxialCrystalSystem () = 
        let descr = "Biaxial Crystal, inclined 19 degrees incident light."
        let info = light600nmInclinedDegreelLPs 19.0
        runTestMuellerMatrixR descr info BaseOpticalSystem.biaxialCrystalSystem

    [<Fact>]
    member __.muellerMatrixR_TransparentGlassFilmSystem () = 
        let descr = "Transparent glass 100 nm thin flim, inclined 19 degrees incident light."
        let info = light600nmInclinedDegreelLPs 19.0
        runTestMuellerMatrixR descr info (BaseOpticalSystem.transparentGlasslFilmSystem (Thickness.nm 100.))

    [<Fact>]
    member __.muellerMatrixR_TransparentGlassSystem_Polarized () = 
        let descr = "Transparent glass, inclined 19 degrees incident light, 27 degrees polarization plane angle."
        let info = { light600nmInclinedDegreelLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization }
        runTestMuellerMatrixR descr info BaseOpticalSystem.transparentGlassSystem

    [<Fact>]
    member __.muellerMatrixR_BiaxialCrystalSystem_Polarized () = 
        let descr = "Biaxial Crystal, inclined 19 degrees incident light, 27 degrees polarization plane angle."
        let info = { light600nmInclinedDegreelLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization }
        runTestMuellerMatrixR descr info BaseOpticalSystem.biaxialCrystalSystem

    [<Fact>]
    member __.muellerMatrixR_TransparentGlassFilmSystem_Polarized () = 
        let descr = "Transparent glass 100 nm thin flim, inclined 19 degrees incident light, 27 degrees polarization plane angle."
        let info = { light600nmInclinedDegreelLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization }
        runTestMuellerMatrixR descr info (BaseOpticalSystem.transparentGlasslFilmSystem (Thickness.nm 100.))

    [<Fact>]
    member __.muellerMatrixR_TransparentGlassSystem_Polarized_WithEllipticity () = 
        let descr = "Transparent glass, inclined 19 degrees incident light, 27 degrees polarization plane angle, with ellipticity 0.58."
        let info = { light600nmInclinedDegreelLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization; ellipticity = Ellipticity 0.58 }
        runTestMuellerMatrixR descr info BaseOpticalSystem.transparentGlassSystem

    [<Fact>]
    member __.muellerMatrixR_BiaxialCrystalSystem_Polarized_WithEllipticity () = 
        let descr = "Biaxial Crystal, inclined 19 degrees incident light, 27 degrees polarization plane angle, with ellipticity 0.58."
        let info = { light600nmInclinedDegreelLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization; ellipticity = Ellipticity 0.58 }
        runTestMuellerMatrixR descr info BaseOpticalSystem.biaxialCrystalSystem

    [<Fact>]
    member __.muellerMatrixR_BiaxialCrystalSystemRotatedX_Polarized_WithEllipticity () = 
        let descr = "Biaxial Crystal, rotated around X, inclined 19 degrees incident light, 27 degrees polarization plane angle, with ellipticity 0.58."
        let info = { light600nmInclinedDegreelLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization; ellipticity = Ellipticity 0.58 }

        let eps = (BaseOpticalSystem.biaxialCrystalSystem.lower.rotateX (Angle.degree 30.0)).eps
        let sys = { BaseOpticalSystem.biaxialCrystalSystem with lower = { BaseOpticalSystem.biaxialCrystalSystem.lower with eps = eps }}
        output.WriteLine(sprintf "eps = %A" eps)
        output.WriteLine(sprintf "sys = %A" sys)
        runTestMuellerMatrixR descr info sys

    [<Fact>]
    member __.muellerMatrixR_BiaxialCrystalSystemRotatedY_Polarized_WithEllipticity () = 
        let descr = "Biaxial Crystal, rotated around Y, inclined 19 degrees incident light, 27 degrees polarization plane angle, with ellipticity 0.58."
        let info = { light600nmInclinedDegreelLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization; ellipticity = Ellipticity 0.58 }

        let eps = (BaseOpticalSystem.biaxialCrystalSystem.lower.rotateY (Angle.degree 30.0)).eps
        let sys = { BaseOpticalSystem.biaxialCrystalSystem with lower = { BaseOpticalSystem.biaxialCrystalSystem.lower with eps = eps }}
        output.WriteLine(sprintf "eps = %A" eps)
        output.WriteLine(sprintf "sys = %A" sys)
        runTestMuellerMatrixR descr info sys

    [<Fact>]
    member __.muellerMatrixR_BiaxialCrystalSystemRotatedZ_Polarized_WithEllipticity () = 
        let descr = "Biaxial Crystal, rotated around Z, inclined 19 degrees incident light, 27 degrees polarization plane angle, with ellipticity 0.58."
        let info = { light600nmInclinedDegreelLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization; ellipticity = Ellipticity 0.58 }

        let eps = (BaseOpticalSystem.biaxialCrystalSystem.lower.rotateZ (Angle.degree 30.0)).eps
        let sys = { BaseOpticalSystem.biaxialCrystalSystem with lower = { BaseOpticalSystem.biaxialCrystalSystem.lower with eps = eps }}
        output.WriteLine(sprintf "eps = %A" eps)
        output.WriteLine(sprintf "sys = %A" sys)
        runTestMuellerMatrixR descr info sys

    [<Fact>]
    member __.muellerMatrixR_BiaxialCrystalFilmSystem_Polarized_WithEllipticity () = 
        let descr = "Biaxial Crystal 100 nm, inclined 19 degrees incident light, 27 degrees polarization plane angle, with ellipticity 0.58."
        let info = { light600nmInclinedDegreelLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization; ellipticity = Ellipticity 0.58 }
        runTestMuellerMatrixR descr info (BaseOpticalSystem.transparentGlasslFilmSystem (Thickness.nm 100.))

    [<Fact>]
    member __.muellerMatrixR_RandomAbsorbingSystem_Polarized_WithEllipticity () = 
        let descr = "Random absorbing system, inclined 19 degrees incident light, 27 degrees polarization plane angle, with ellipticity 0.58."
        let info = { light600nmInclinedDegreelLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization; ellipticity = Ellipticity 0.58 }

        let sys = { BaseOpticalSystem.transparentGlassSystem with lower = { BaseOpticalSystem.transparentGlassSystem.lower with eps = randomProperties.eps }}
        runTestMuellerMatrixR descr info sys

    [<Fact>]
    member __.muellerMatrixR_RandomSystem_Polarized_WithEllipticity () = 
        let descr = "Random system, inclined 19 degrees incident light, 27 degrees polarization plane angle, with ellipticity 0.58."
        let info = { light600nmInclinedDegreelLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization; ellipticity = Ellipticity 0.58 }

        let eps = randomProperties.eps.re
        let sys = { BaseOpticalSystem.transparentGlassSystem with lower = { BaseOpticalSystem.transparentGlassSystem.lower with eps = eps }}
        output.WriteLine(sprintf "eps = %A" eps)
        output.WriteLine(sprintf "sys = %A" sys)
        runTestMuellerMatrixR descr info sys

    [<Fact>]
    member __.muellerMatrixR_RandomMagneticSystem_Polarized_WithEllipticity () = 
        let descr = "Random magnetic system, inclined 19 degrees incident light, 27 degrees polarization plane angle, with ellipticity 0.58."
        let info = { light600nmInclinedDegreelLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization; ellipticity = Ellipticity 0.58 }

        let sys = { BaseOpticalSystem.transparentGlassSystem with lower = { BaseOpticalSystem.transparentGlassSystem.lower with mu = randomProperties.mu }}
        runTestMuellerMatrixR descr info sys

    [<Fact>]
    member __.muellerMatrixR_RandomOpticallyActiveSystem_Polarized_WithEllipticity () = 
        let descr = "Random optically active system, inclined 19 degrees incident light, 27 degrees polarization plane angle, with ellipticity 0.58."
        let info = { light600nmInclinedDegreelLPs 19.0 with polarization = Angle.degree 27.0 |> Polarization; ellipticity = Ellipticity 0.58 }

        let sys = { BaseOpticalSystem.transparentGlassSystem with lower = { BaseOpticalSystem.transparentGlassSystem.lower with rho = randomProperties.rho }}
        runTestMuellerMatrixR descr info sys
