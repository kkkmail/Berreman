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

let printv s (v : #seq<'a>) = 
    printfn "%s:" s
    v 
    |> List.ofSeq
    |> List.map(fun e -> printfn "    %A" e)


let n = 100
let tEnd = 1000.0
let odeParams = { OdeParams.defaultValue with endTime = tEnd }

let d t (v : Vector<double>) = 
    let x = v.ToArray()
    let x1 = x |> Array.mapi (fun i _ -> x.[if (i + 1) < n then (i + 1) else 0]) |> vector
    -0.01 * (1.0 + 4.0 * cos(pi * t / 4.0)) * x1

let i = [ for i in 1..n -> double i ] |> vector

#time
let result = solve odeParams d i
#time

let f = result |> List.ofArray |> List.rev |> List.head
printv "i" i
printv "f" f

