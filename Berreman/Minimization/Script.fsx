//===========================================================
#load "References.fsx"
//===========================================================
open Minimization.Generic
open System.Numerics
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.Optimization
open Microsoft.FSharp.Core
open System

let rosenbrockFunction x y = 
    //return Math.Pow((1 - input[0]), 2) + 100 * Math.Pow((input[1] - input[0] * input[0]), 2);
    (pown (1.0 - x) 2) + 100.0 * (pown (y - x * x) 2)


let r (x : Vector<double>) = rosenbrockFunction x.[0] x.[1]
let i = [ 1.2; 1.2] |> vector

#time
let result = minimizeDefault r i
#time

printfn "result = %A" result
printfn "result.FunctionInfoAtMinimum.Value = %A" result.FunctionInfoAtMinimum.Value
printfn "result.Iterations = %A" result.Iterations
printfn "result.MinimizingPoint = %A" result.MinimizingPoint

// Does not work for 15.
let nn = 15

let r1 (x : Vector<double>) = 
    x.ToArray()
    |> Array.fold (fun acc e -> acc + e * e) 0.0

let i1 = [ for i in 1..nn -> float i ] |> vector

#time
let result1 = minimizeDefault r1 i1
#time

printfn "result1.FunctionInfoAtMinimum.Value = %A" result1.FunctionInfoAtMinimum.Value
printfn "result1.Iterations = %A" result1.Iterations
printfn "result1.MinimizingPoint = %A" result1.MinimizingPoint
