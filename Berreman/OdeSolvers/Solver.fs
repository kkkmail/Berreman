namespace OdeSolvers

open System.Numerics
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.OdeSolvers
open Microsoft.FSharp.Core
open System

module Solver = 

    type OdeParams = 
        {
            startTime : double
            endTime : double
            stepSize : double
        }

        static member defaultValue =
            {
                startTime = 0.0
                endTime = 10.0
                stepSize = 0.01
            }


    let solve (o : OdeParams) f i = 
        let d = Func<double, Vector<double>, Vector<double>> (fun t x -> f t x)
        let n = ((o.endTime - o.startTime) / o.stepSize |> int) + 1
        let result = RungeKutta.FourthOrder(i, o.startTime, o.endTime, n, d)
        result
