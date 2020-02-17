//#r "System.Core.dll"
//#r "System.dll"
//#r "System.Numerics.dll"
#r "./bin/Debug/Extreme.Numerics.Net40.dll"
#r "./bin/Debug/Extreme.Numerics.Generic.Net40.dll"
#r "./bin/Debug/Extreme.Numerics.FSharp.Net40.dll"

//open System.Numerics
open Extreme.Mathematics

NumericsConfiguration.Providers.RegisterGenericProvider()

let cplx r = Complex(r, 0.0)



let m = 
    [
        [cplx 0.0; cplx 1.0; cplx 0.0; cplx 0.0]
        [cplx 2.25; cplx 0.0; cplx 0.0; cplx 0.0]
        [cplx 0.0; cplx 0.0; cplx 0.0; cplx 1.0]
        [cplx 0.0; cplx 0.0; cplx 2.25; cplx 0.0]
    ]

let m0 = Matrix.Create(array2D m)
printfn "m0 = %A" m0

try
    // This one fails with th efolliwing exception:
    //System.NullReferenceException: Object reference not set to an instance of an object.
    //   at Extreme.Mathematics.Operations`1.IsOne(T a)
    //   at Extreme.Mathematics.Matrix`1.MultiplyCore(T value, Matrix`1 result)
    //   at Extreme.Mathematics.Matrix`1.WG(T , Matrix`1 , Matrix`1 )
    //   at <StartupCode$FSI_0002>.$FSI_0002.main@() in C:\GitHub\BM\Berreman\Berreman\Extreme60_Err.fsx:line 26
    let m1 = (cplx 2.0) * m0
    printfn "m1 = %A" m1
with 
    | e -> printfn "Exception:\n%A" e


let d = m0.GetDeterminant()
printfn "d = %A" d

