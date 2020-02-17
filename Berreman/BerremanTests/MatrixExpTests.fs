namespace BerremanTests

open Berreman.MatrixExp
open Berreman.MathNetNumericsMath

open Xunit
open Xunit.Abstractions

open MatrixComparison


type MatriExpTestData =
    {
        matrix : ComplexMatrix
        exp : ComplexMatrix
    }


type MatrixExpTests(output : ITestOutputHelper) =
    let data = 
        [|
            {
                matrix = 
                    [
                        [ 1.; 0.; 0.; 0. ]
                        [ 0.; 1.; 0.; 0. ]
                        [ 0.; 0.; 1.; 0. ]
                        [ 0.; 0.; 0.; 1. ]
                    ]
                    |> ComplexMatrix.fromRe
                exp = 
                    [
                        [ 2.718281828459045; 0.; 0.; 0. ]
                        [ 0.; 2.718281828459045; 0.; 0. ]
                        [ 0.; 0.; 2.718281828459045; 0. ]
                        [ 0.; 0.; 0.; 2.718281828459045 ]
                    ]
                    |> ComplexMatrix.fromRe
            }

            {
                matrix = 
                    [
                        [ 0.; 0.; 0.; 1. ]
                        [ 0.; 0.; 1.; 0. ]
                        [ 0.; 1.; 0.; 0. ]
                        [ 1.; 0.; 0.; 0. ]
                    ]
                    |> ComplexMatrix.fromRe
                exp = 
                    [
                        [ 1.5430806348152437; 0.; 0.; 1.1752011936438014 ]
                        [ 0.; 1.5430806348152437; 1.1752011936438014; 0. ]
                        [ 0.; 1.1752011936438014; 1.5430806348152437; 0. ]
                        [ 1.1752011936438014; 0.; 0.; 1.5430806348152437 ]
                    ]
                    |> ComplexMatrix.fromRe
            }

            {
                matrix = 
                    [
                        [ 0.; 1.; 0.; 0. ]
                        [ 2.25; 0.; 0.; 0. ]
                        [ 0.; 0.; 0.; 1. ]
                        [ 0.; 0.; 2.25; 0. ]
                    ]
                    |> ComplexMatrix.fromRe
                exp = 
                    [
                        [ 2.352409615243247; 1.4195196367298781; 0.; 0. ]
                        [ 3.1939191826422255; 2.3524096152432468; 0.; 0. ]
                        [ 0.; 0.; 2.352409615243247; 1.4195196367298781 ]
                        [ 0.; 0.; 3.1939191826422255; 2.3524096152432468 ]
                    ]
                    |> ComplexMatrix.fromRe
            }

            {
                matrix = 
                    [
                        [ createComplex 0.434019224414145 0.3597936176617338; createComplex -0.028969337488555125 -0.2331671374340436; createComplex -0.38793719441289354 -0.17669428507034324; createComplex -0.3683415930192917 -0.1866072073826357 ]
                        [ createComplex -0.05288986643686955 -0.26613918522944213; createComplex -0.3872670051288296 0.054708036112359526; createComplex 0.44826408380231353 -0.06677607969711241; createComplex -0.47643529127135253 0.2679087432590246 ]
                        [ createComplex 0.4927380938509902 0.3736186259661498; createComplex -0.23306761739817317 0.09314339965657514; createComplex 0.33129946055179516 -0.2901267863371071; createComplex 0.3746195233468159 0.12752466944891006 ]
                        [ createComplex 0.16659862837095085 0.022244704795009174; createComplex 0.48894614408726356 0.38797810060764326; createComplex 0.23324167272783902 0.09536921773102325; createComplex 0.16152033609014071 0.055957933963475925 ]
                    ]
                    |> ComplexMatrix.create
                exp = 
                    [
                        [ createComplex 1.301107463228251 0.30149892205262907; createComplex 0.035994474436527014 -0.3594482497768818; createComplex -0.6028227611799336 -0.37242907436917494; createComplex -0.46836947017605535 -0.329899169881008 ]
                        [ createComplex 0.07424919735412158 -0.17208118696828723; createComplex 0.49120419109194813 0.04142968284048359; createComplex 0.3104091059312637 -0.05379263120721354; createComplex -0.36279080282391385 0.25001280133413606 ]
                        [ createComplex 0.7483563871888792 0.5670798602782706; createComplex -0.0673133350346172 0.11990614008253163; createComplex 1.2593670913517983 -0.5104347651227148; createComplex 0.43110469521325057 -0.12419565878039274 ]
                        [ createComplex 0.31214390274179327 0.11371591227836135; createComplex 0.37023926111805394 0.32796921412780233; createComplex 0.36941421206475317 0.1023595425159583; createComplex 1.0348549113024064 0.03634180985247387 ]
                    ]
                    |> ComplexMatrix.create
            }

            {
                matrix = 
                    [
                        [ createComplex 3.0788513100411707 0.49469830314298635; createComplex -1.2080122993694453 -2.9928065873634124; createComplex 2.4599665563888995 -1.951807695221146; createComplex 1.2099931269579978 -0.6828005219227951 ]
                        [ createComplex 3.3050571888868996 0.995186567278834; createComplex 2.229025568155867 3.597611495379678; createComplex -0.10073452994519005 3.0077337086555; createComplex -4.979881057783793 -0.6444300340434861 ]
                        [ createComplex -4.62642051481263 4.750179329357742; createComplex -0.5397643243097705 1.4610534297437394; createComplex -1.0115250430409373 -1.6887973429102465; createComplex -3.0029685585060606 2.885432008018882 ]
                        [ createComplex 1.4593960547080087 3.5343157315154206; createComplex -0.03742460540420822 2.5349218035454313; createComplex 0.9800291706836317 -2.836872950400562; createComplex 0.579580853515842 -3.9496444121256213 ]
                    ]
                    |> ComplexMatrix.create
                exp = 
                    [
                        [ createComplex -219.37086621359 -115.23008858190974; createComplex -19.902768973161834 97.50032057594959; createComplex -97.06734461578789 112.760545242185; createComplex -98.86670930101616 -53.47253717705648 ]
                        [ createComplex 106.01381822534191 36.11684971301449; createComplex -16.262352063490873 -38.36044749694726; createComplex 23.92161817624071 -79.20561686192471; createComplex 81.68704653744192 7.447492693074164 ]
                        [ createComplex 155.67353737744872 -181.5933601216344; createComplex -71.65108076165014 -36.48074527997643; createComplex -86.90961646795193 -95.13397379587344; createComplex 43.92309929010122 -89.87026842005339 ]
                        [ createComplex -134.94729734952608 -73.11350150850473; createComplex -16.81832962660528 35.715044080387244; createComplex -30.602096612141942 60.50962579593738; createComplex -57.8838491505365 10.530237584614776 ]
                    ]
                    |> ComplexMatrix.create
            }
        |]

    // Complex matrix and expected matrix exponent
    member __.runTest (d : MatriExpTestData) = verifyMatrixEquality output (d.matrix.matrixExp()) d.exp

    [<Fact>]
    member this.matrixExpTest0 () = this.runTest (data.[0])

    [<Fact>]
    member this.matrixExpTest1 () = this.runTest (data.[1])

    [<Fact>]
    member this.matrixExpTest2 () = this.runTest (data.[2])

    [<Fact>]
    member this.matrixExpTest3 () = this.runTest (data.[3])

    [<Fact>]
    member this.matrixExpTest4 () = this.runTest (data.[4])
