//===========================================================
#load "References.fsx"
//===========================================================
open Berreman.MathNetNumericsMath
open OdeSolvers.Solver
open System.Numerics
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.Optimization
open Microsoft.FSharp.Core
open System
open FSharp.Collections.ParallelSeq

let printv s (v : #seq<'a>) = 
    printfn "%s:" s
    v 
    |> List.ofSeq
    |> List.map(fun e -> printfn "    %A" e)


let n = 100000
let tEnd = 1000.0
let odeParams = { OdeParams.defaultValue with endTime = tEnd }

let f (x : double[]) (t : double) : double[] = 
    let mult = -0.01 * (1.0 + 4.0 * cos(pi * t / 4.0))
    x |> Array.mapi (fun i _ -> mult * x.[if (i + 1) < n then (i + 1) else 0])
    //x |> Seq.ofArray |> PSeq.mapi (fun i _ -> mult * x.[if (i + 1) < n then (i + 1) else 0]) |> Array.ofSeq

let d t (v : Vector<double>) = f (v.ToArray()) t |> vector

let i = [| for i in 1..n -> double i |]

#time
let result1 = solveA odeParams f i
#time

let r1 = result1.[1,*]
//printfn "r1 = %A" r1
printfn "Completed."

//#time
//let result = solve odeParams d (i |> vector)
//#time

//let r = (result |> List.ofArray |> List.rev |> List.head).ToArray()
////printfn "i = %A" i
////printfn "r = %A" r

//let diff = 
//    Array.zip r r1
//    |> Array.fold (fun acc (a, b) -> acc + pown (a-b) 2) 0.0

//printfn "diff = %A" (sqrt (diff /(double n)))

