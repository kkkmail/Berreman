namespace BerremanTests

open Berreman.Constants
open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.BerremanMatrix

open Xunit
open Xunit.Abstractions

open MatrixComparison
open Berreman.Media

type ThinFilmTestData =
    {
        description : string
        thinFilms : List<Layer>
        light : IncidentLightInfo
        expected : ComplexMatrix
    }

type ThinFilmTests(output : ITestOutputHelper) =

    let data =
        [
            {
                description = "One layer homogeneous media, normal incidence angle."
                thinFilms =
                    [
                        {
                            properties = 1.52 |> RefractionIndex.create |> OpticalProperties.fromRefractionIndex
                            thickness = Thickness.nm 75.0<nm>
                        }
                    ]
                light =
                    {
                        waveLength = WaveLength.nm 600.0<nm>
                        refractionIndex = RefractionIndex.vacuum
                        incidenceAngle = Angle.degree 0.0 |> IncidenceAngle
                        polarization = Polarization.defaultValue
                        ellipticity = Ellipticity.defaultValue
                    }
                expected =
                    [
                        [ createComplex 0.36812455268467836 0.; createComplex 0. 0.6116950565054284; createComplex 0. 0.; createComplex 0. 0. ]
                        [ createComplex 0. 1.4132602585501424; createComplex 0.3681245526846782 0.; createComplex 0. 0.; createComplex 0. 0. ]
                        [ createComplex 0. 0.; createComplex 0. 0.; createComplex 0.36812455268467836 0.; createComplex 0. 0.6116950565054284 ]
                        [ createComplex 0. 0.; createComplex 0. 0.; createComplex 0. 1.4132602585501424; createComplex 0.3681245526846782 0.]
                    ]
                    |> ComplexMatrix.create
            }

            {
                description = "One layer homogeneous media, 85 degrees incidence angle."
                thinFilms =
                    [
                        {
                            properties = 1.52 |> RefractionIndex.create |> OpticalProperties.fromRefractionIndex
                            thickness = Thickness.nm 75.0<nm>
                        }
                    ]
                light =
                    {
                        waveLength = WaveLength.nm 600.0<nm>
                        refractionIndex = RefractionIndex.vacuum
                        incidenceAngle = Angle.degree 85.0 |> IncidenceAngle
                        polarization = Polarization.defaultValue
                        ellipticity = Ellipticity.defaultValue
                    }
                expected =
                    [
                        [ createComplex 0.6203020411609136 0.; createComplex 0. 0.38975079646975697; createComplex 0. 0.; createComplex 0. 0. ]
                        [ createComplex 0. 1.578509609997277; createComplex 0.6203020411609138 0.; createComplex 0. 0.; createComplex 0. 0. ]
                        [ createComplex 0. 0.; createComplex 0. 0.; createComplex 0.620302041160914 0.; createComplex 0. 0.6832191871525611 ]
                        [ createComplex 0. 0.; createComplex 0. 0.; createComplex 0. 0.900480240163726; createComplex 0.6203020411609139 0. ]
                    ]
                    |> ComplexMatrix.create
            }

            {
                description = "One layer media with random real epsilon, random incident light."
                thinFilms =
                    [
                        {
                            properties =
                                {
                                    eps =
                                        [
                                            [ 3.2348312413417437; -0.13274761403308766; -0.46073345815011246 ]
                                            [ -0.13274761403308766; 3.381257622001705; -0.6979528788314281 ]
                                            [ -0.46073345815011235; -0.697952878831428; 4.5371548045970345 ]
                                        ]
                                        |> Eps.fromRe
                                    mu = Mu.vacuum
                                    rho = Rho.vacuum
                                }
                            thickness = Thickness.nm 227.0<nm>
                        }
                    ]
                light =
                    {
                        waveLength = WaveLength.nm 687.0<nm>
                        refractionIndex = RefractionIndex.vacuum
                        incidenceAngle = Angle.degree 53.0 |> IncidenceAngle
                        polarization = Angle.degree 50.0 |> Polarization
                        ellipticity = -0.802933683069591 |> Ellipticity
                    }
                expected =
                    [
                        [ createComplex -0.9274014609420038 -0.16759861804121412; createComplex 0.017170207434521613 -0.1456139561785692; createComplex -0.017403663019614386 -0.13060623757618983; createComplex 0.014075507609934673 0.06306021155774927 ]
                        [ createComplex 0.06324527789089204 -0.5351670353761716; createComplex -0.9274014609420037 -0.16759861804121412; createComplex 0.045205165589206765 0.21607199115505854; createComplex -0.0201958480970428 -0.13942823231310325 ]
                        [ createComplex -0.020195848097042807 -0.13942823231310336; createComplex 0.01407550760993468 0.06306021155774927; createComplex -0.9585857675082211 0.0015144028039448532; createComplex -0.009673150258669137 -0.12936024950038505 ]
                        [ createComplex 0.04520516558920681 0.21607199115505868; createComplex -0.0174036630196144 -0.13060623757618986; createComplex -0.03084636619918583 -0.3709728596674446; createComplex -0.9585857675082216 0.0015144028039448393 ]
                    ]
                    |> ComplexMatrix.create
            }

            {
                description = "One layer media with random optical properties, random incident light."
                thinFilms =
                    [
                        {
                            properties =
                                {
                                    eps =
                                        [
                                            [ createComplex 3.5021307590677586 0.004910796055146478; createComplex 0.1448188382704857 0.000834260445303435; createComplex 0.0421100441439744 -0.0008311798560446699 ]
                                            [ createComplex 0.1448188382704858 0.0008342604453034346; createComplex 4.395445093161517 0.004762172783970623; createComplex 0.01455357523209506 -0.00008366130786027271 ]
                                            [ createComplex 0.0421100441439744 -0.0008311798560446706; createComplex 0.01455357523209506 -0.0000836613078602726; createComplex 1.0673056268307946 0.005955215513141588 ]
                                        ]
                                        |> Eps.create
                                    mu =
                                        [
                                            [ createComplex 0.9582047927795885 0.0; createComplex 0.002456702160523204 0.0; createComplex -0.023259102856946406 0.0 ]
                                            [ createComplex 0.002456702160523218 0.0; createComplex 1.0480733247992344 0.0; createComplex 0.012816054669739748 0.0 ]
                                            [ createComplex -0.02325910285694638 0.0; createComplex 0.012816054669739748 0.0; createComplex 1.0726398036824272 0.0 ]
                                        ]
                                        |> Mu.create
                                    rho =
                                        [
                                            [ createComplex 0. -0.044424020098732225; createComplex 0. 0.020697425762887564; createComplex 0. 0.05026489175739626 ]
                                            [ createComplex 0. 0.020697425762887564; createComplex 0. -0.029357526291481178; createComplex 0. 0.07178570497243361 ]
                                            [ createComplex 0. 0.05026489175739626; createComplex 0. 0.07178570497243361; createComplex 0. 0.01603950250386385 ]
                                        ]
                                        |> Rho.create
                                }
                            thickness = Thickness.nm 168.0<nm>
                        }
                    ]
                light =
                    {
                        waveLength = WaveLength.nm 504.0<nm>
                        refractionIndex = RefractionIndex.vacuum
                        incidenceAngle = Angle.degree 41.0 |> IncidenceAngle
                        polarization = Angle.degree 64.0 |> Polarization
                        ellipticity = -0.42025305376355426 |> Ellipticity
                    }
                expected =
                    [
                        [ createComplex -0.98816443746049 0.055880632349881706; createComplex 0.006127601130424354 0.005034180198318153; createComplex 0.08128797263166429 0.03840650088667767; createComplex 0.035631210452151146 -0.010682458054276114 ]
                        [ createComplex 0.03213856680463725 0.026358904038529737; createComplex -0.9901747181153763 0.05390938194829858; createComplex 0.14380921471417246 -0.07570770676695995; createComplex 0.10398185697252946 0.03570545194284099 ]
                        [ createComplex -0.03172579399246865 -0.034489837610064196; createComplex -0.036589100436663816 -0.037239617827756887; createComplex -0.5703176562171782 -0.013369118260331655; createComplex 0.015451305510010923 -0.39437272074873647 ]
                        [ createComplex -0.1479301284999799 -0.2112747395151823; createComplex -0.03231766219583817 -0.03485213054808696; createComplex 0.06380912203475694 -1.6456372276292885; createComplex -0.5860131981735811 -0.011814338101499983 ]
                    ]
                    |> ComplexMatrix.create
            }

            {
                description = "Two layer inhomogeneous media."
                thinFilms =
                    [
                        {
                            properties =
                                {
                                    eps =
                                        [
                                            [ 2.25; 0.0; 0.0 ]
                                            [ 0.0; 4.; 0.0 ]
                                            [ 0.0; 0.0; 3.0625 ]
                                        ]
                                        |> Eps.fromRe
                                    mu = Mu.vacuum
                                    rho = Rho.vacuum
                                }
                            thickness = Thickness.nm 75.0<nm>
                        }

                        {
                            properties =
                                {
                                    eps =
                                        [
                                            [ 3.0625; 0.0; 0.0 ]
                                            [ 0.0; 2.25; 0.0 ]
                                            [ 0.0; 0.0; 4. ]
                                        ]
                                        |> Eps.fromRe
                                    mu = Mu.vacuum
                                    rho =
                                        [
                                            [ createComplex 0. 0.0019; createComplex 0. -0.0035; createComplex 0.0 0.0 ]
                                            [ createComplex 0. 0.0035; createComplex 0. 0.0019; createComplex 0.0 0.0 ]
                                            [ createComplex 0.0 0.0; createComplex 0.0 0.0; createComplex 0. -0.0057 ]
                                        ]
                                        |> Rho.create
                                }
                            thickness = Thickness.nm 100.0<nm>
                        }
                    ]
                light =
                    {
                        waveLength = WaveLength.nm 600.0<nm>
                        refractionIndex = RefractionIndex.vacuum
                        incidenceAngle = Angle.degree 35.0 |> IncidenceAngle
                        polarization = Angle.degree 0.0 |> Polarization
                        ellipticity = 0.0 |> Ellipticity
                    }
                expected =
                    [
                        [ createComplex -0.8485039057576538 0.; createComplex 0. 0.13283157354221561; createComplex -0.0020704335282441806 0.; createComplex 0. 0.0000422277299978338 ]
                        [ createComplex 0. 0.5347834505187596; createComplex -1.0948222010162802 0.; createComplex 0. 0.00007390465299844232; createComplex -0.0014277057856303319 0. ]
                        [ createComplex 0.0015677909592953017 0.; createComplex 0. -0.0004410721015131045; createComplex -1.3618717897715529 0.; createComplex 0. 0.1079522775922998 ]
                        [ createComplex 0. -0.0011390718740658097; createComplex 0.0015669677854363532 0.; createComplex 0. 0.32322207831362226; createComplex -0.7086603075529351 0. ]
                    ]
                    |> ComplexMatrix.create
            }

            {
                description = "Two layer media with random optical properties, random incident light."
                thinFilms =
                    [
                        {
                            properties =
                                {
                                    eps =
                                        [
                                            [ createComplex 1.6229392617495504 0.007895343463263902; createComplex -0.012875560449085471 -0.0018573990732654619; createComplex 0.01140303906526267 -0.0007114143338732871 ]
                                            [ createComplex -0.012875560449085471 -0.0018573990732654617; createComplex 2.2410029463497887 0.006256075978668986; createComplex 0.4998570259368046 -0.0010734607231248966 ]
                                            [ createComplex 0.01140303906526267 -0.0007114143338732871; createComplex 0.49985702593680437 -0.0010734607231248966; createComplex 3.274581737837376 0.00790862579383793 ]
                                        ]
                                        |> Eps.create
                                    mu =
                                        [
                                            [ 1.0752234830197742; 0.0013550391998404174; -0.011020254808265406 ]
                                            [ 0.0013550391998404174; 1.0814019591272515; -0.003564423558419294 ]
                                            [ -0.011020254808265406; -0.0035644235584192524; 1.0379932189480254 ]
                                        ]
                                        |> Mu.fromRe
                                    rho =
                                        [
                                            [ 0.029844968141584625; -0.028763456042956777; 0.02967893641567288 ]
                                            [ -0.028763456042956777; 0.08086531310521229; -0.008102599261662975 ]
                                            [ 0.029678936415672878; -0.008102599261662971; -0.002768651477557617 ]
                                        ]
                                        |> Rho.fromIm
                                }
                            thickness = Thickness.nm 137.0<nm>
                        }

                        {
                            properties =
                                {
                                    eps =
                                        [
                                            [ createComplex 4.093032870157705 0.008171658275933694; createComplex -0.43907349972552323 0.0007297634441781452; createComplex 0.16606340745107284 0.0009007821122230149 ]
                                            [ createComplex -0.439073499725523 0.0007297634441781455; createComplex 3.470984197432937 0.005853023141142169; createComplex 0.12069703508523659 -0.0005644826514393578 ]
                                            [ createComplex 0.1660634074510728 0.0009007821122230149; createComplex 0.12069703508523656 -0.0005644826514393578; createComplex 1.048122711751765 0.006030083363077285 ]
                                        ]
                                        |> Eps.create
                                    mu =
                                        [
                                            [ 1.0611136888365906; 0.02673834542393358; 0.013429064297689877 ]
                                            [ 0.02673834542393355; 0.9263839240886705; -0.0012911236702604911 ]
                                            [ 0.013429064297689877; -0.0012911236702604911; 0.9587673756718665 ]
                                        ]
                                        |> Mu.fromRe
                                    rho =
                                        [
                                            [ -0.054014619992991966; 0.006888036849500936; 0.04764456404037003 ]
                                            [ 0.006888036849500938; 0.07760464614164607; 0.015470784063986442 ]
                                            [ 0.04764456404037003; 0.015470784063986439; 0.03178409630852125 ]
                                        ]
                                        |> Rho.fromIm
                                }
                            thickness = Thickness.nm 108.0<nm>
                        }
                    ]
                light =
                    {
                        waveLength = WaveLength.nm 553.0<nm>
                        refractionIndex = RefractionIndex.vacuum
                        incidenceAngle = Angle.degree 63.0 |> IncidenceAngle
                        polarization = Angle.degree 60.0 |> Polarization
                        ellipticity = -0.43910436091105565 |> Ellipticity
                    }
                expected =
                    [
                        [ createComplex -0.32872665940734547 0.07513745800952036; createComplex 0.07518575799759133 0.3235168796466171; createComplex -0.10129094865899069 0.07509724581660444; createComplex 0.11960879746762723 0.08993784648874378 ]
                        [ createComplex 0.006931905968003277 -0.3335527410704436; createComplex -2.862833829114862 0.5368250659420413; createComplex 0.19181848462366236 0.20204680633965483; createComplex -0.2750678558475286 0.7339990447614074 ]
                        [ createComplex -0.015189533275087201 0.053558696483902844; createComplex -0.15947009800218834 0.33387229005853564; createComplex -0.43950485477761825 -0.002596221135850757; createComplex 0.05922623686616131 -0.5094461692859155 ]
                        [ createComplex -0.2801703300419268 0.2304947668577903; createComplex 0.24843941106131673 0.1616699012007487; createComplex 0.012608876313015929 -0.8918431079164595; createComplex -0.9559642856086528 -0.03029991123705772 ]
                    ]
                    |> ComplexMatrix.create
            }
        ]

    member _.runTest (d : ThinFilmTestData) =
        output.WriteLine d.description

        let (BerremanMatrixPropagated (ComplexMatrix4x4 bm)) =
            BerremanMatrixPropagated.propagate (d.thinFilms, (EmField.create (d.light, OpticalProperties.vacuum)).emComponents.[0], d.light.waveLength)

        verifyMatrixEquality output bm d.expected

    [<Fact>]
    member this.berremanMatrixTest0 () = this.runTest (data.[0])

    [<Fact>]
    member this.berremanMatrixTest1 () = this.runTest (data.[1])

    [<Fact>]
    member this.berremanMatrixTest2 () = this.runTest (data.[2])

    [<Fact>]
    member this.berremanMatrixTest3 () = this.runTest (data.[3])

    [<Fact>]
    member this.berremanMatrixTest4 () = this.runTest (data.[4])

    [<Fact>]
    member this.berremanMatrixTest5 () = this.runTest (data.[5])
