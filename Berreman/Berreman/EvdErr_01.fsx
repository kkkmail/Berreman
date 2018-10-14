#r "System.Numerics.dll"
#r "../packages/MathNet.Numerics.4.5.1/lib/net461/MathNet.Numerics.dll"
#r "../packages/MathNet.Numerics.FSharp.4.5.1/lib/net45/MathNet.Numerics.FSharp.dll"

open System.Numerics
open MathNet.Numerics.LinearAlgebra

let cplx a = Complex(a, 0.0)

let c = 
    [
        [cplx 0.0; cplx 1.0; cplx 0.0; cplx 0.0]
        [cplx 2.25; cplx 0.0; cplx 0.0; cplx 0.0]
        [cplx 0.0; cplx 0.0; cplx 0.0; cplx 1.0]
        [cplx 0.0; cplx 0.0; cplx 2.25; cplx 0.0]
    ]
    |> matrix

printfn "c = %A" c

#time
printfn "Calculating Evd"
let ec = c.Evd()
printfn "ec = %A" ec
#time
