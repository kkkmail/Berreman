namespace BerremanTests

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
open Berreman.FieldFunctions


type IncidentLightTestData =
    {
        description : string
        info : IncidentLightInfo
        expected : EmField
    }


type IncidentLightTests(output : ITestOutputHelper) =

    let waveLength = WaveLength.nm 600.0
    let getN1SinFita incidenceAngle = N1SinFita.create 1.0 incidenceAngle

    let data =
        [
            (
                let incidenceAngle = Angle.degree 7.0 |> IncidenceAngle
                let n1SinFita = getN1SinFita incidenceAngle

                {
                    description = "Linear polarization, 0 degrees polarization angle."
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
                }
            )

            (
                let incidenceAngle = Angle.degree 7.0 |> IncidenceAngle
                let n1SinFita = getN1SinFita incidenceAngle

                {
                    description = "Elliptic polarization (0.2), 0 degrees polarization angle."
                    info =
                        {
                            waveLength = waveLength
                            refractionIndex = RefractionIndex.vacuum
                            incidenceAngle = incidenceAngle
                            polarization = Polarization.defaultValue
                            ellipticity = Ellipticity 0.2
                        }
                    expected =
                            {
                                waveLength = waveLength
                                n1SinFita = n1SinFita
                                opticalProperties = OpticalProperties.vacuum
                                e =
                                    [ createComplex 0.97327157603087 0.; createComplex 0. 0.19611613513818404; createComplex -0.11950272310222826 0. ]
                                    |> E.create
                                h =
                                    [ createComplex 0. -0.194654315206174; createComplex 0.9805806756909199 0.; createComplex 0. 0.02390054462044566 ]
                                    |> H.create
                            }
                }
            )

            (
                let incidenceAngle = Angle.degree 7.0 |> IncidenceAngle
                let n1SinFita = getN1SinFita incidenceAngle
                let beta = Angle.degree 25.0

                {
                    description = "Linear polarization, 25 degrees polarization angle."
                    info =
                        {
                            waveLength = waveLength
                            refractionIndex = RefractionIndex.vacuum
                            incidenceAngle = incidenceAngle
                            polarization = Polarization beta
                            ellipticity = Ellipticity.defaultValue
                        }
                    expected =
                            {
                                waveLength = waveLength
                                n1SinFita = n1SinFita
                                opticalProperties = OpticalProperties.vacuum
                                e =
                                    [ 0.8995523062257899; 0.42261826174069944; -0.11045113492912874 ]
                                    |> E.fromRe
                                h =
                                    [ -0.4194681293040762; 0.9063077870366498; 0.0515042100693638 ]
                                    |> H.fromRe
                            }
                }
            )

            (
                let incidenceAngle = Angle.degree 11.0 |> IncidenceAngle
                let n1SinFita = getN1SinFita incidenceAngle
                let beta = Angle.degree -34.0
                let ellipticity = Ellipticity.create 0.38

                {
                    description = "Linear polarization, 25 degrees polarization angle."
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
                                waveLength = waveLength
                                n1SinFita = n1SinFita
                                opticalProperties = OpticalProperties.vacuum
                                e =
                                    [ createComplex 0.7607323158175443 0.1949857619013546; createComplex -0.5227243445459797 0.2944888699958045; createComplex -0.14787138271966674 -0.03790139267583888 ]
                                    |> E.create
                                h =
                                    [ createComplex 0.5131204260561963 -0.2890782800106668; createComplex 0.774970710515275 0.1986352509274723; createComplex -0.09974050704168126 0.05619112543347336 ]
                                    |> H.create
                            }
                }
            )
        ]

    let createField (d : IncidentLightTestData) = EmField.create(d.info, OpticalProperties.vacuum)

    member __.runTest (d : IncidentLightTestData) =
        output.WriteLine d.description
        let i = createField d
        verifyVectorEqualityE output "eI" i.e d.expected.e
        verifyVectorEqualityH output "hI" i.h d.expected.h

    member __.runAzimuthAndEllipticityTest(d : IncidentLightTestData) =
        output.WriteLine d.description
        let i = createField d
        let el = i.ellipticity
        let p = i.azimuth

        output.WriteLine (sprintf "d.info = \n%A" d.info)
        output.WriteLine (sprintf "el, = %A, p = %A" el p)

        verifyPolarizationEquality output "Polarization" (d.info.polarization) p
        verifyEllipticityEquality output "Ellipticity" (d.info.ellipticity) el

    [<Fact>]
    member this.incidentLightTest0 () = this.runTest (data.[0])

    [<Fact>]
    member this.incidentLightTest1 () = this.runTest (data.[1])

    [<Fact>]
    member this.incidentLightTest2 () = this.runTest (data.[2])

    [<Fact>]
    member this.incidentLightTest3 () = this.runTest (data.[3])

    [<Fact>]
    member this.incidentLightAzimuthAndEllipticityTest0 () = this.runAzimuthAndEllipticityTest (data.[0])

    [<Fact>]
    member this.incidentLightAzimuthAndEllipticityTest1 () = this.runAzimuthAndEllipticityTest (data.[1])

    [<Fact>]
    member this.incidentLightAzimuthAndEllipticityTest2 () = this.runAzimuthAndEllipticityTest (data.[2])

    [<Fact>]
    member this.incidentLightAzimuthAndEllipticityTest3 () = this.runAzimuthAndEllipticityTest (data.[3])
