namespace Minimization

open System.Numerics
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.Optimization
open Microsoft.FSharp.Core
open System
//open System.Collections
//open System.Collections.Generic
//open System.Linq

module Generic = 

    type OptimizationParams = 
        {
            convergenceTolerance : double
            maximumIterations : int
        }

        static member defaultValue =
            {
                convergenceTolerance = 1e-5
                maximumIterations = 1_000_000
            }


    let minimize (o : OptimizationParams) f i = 
        let obj = Func<Vector<double>, double> (fun x -> f x) |> ObjectiveFunction.Value
        let solver = new NelderMeadSimplex(o.convergenceTolerance, o.maximumIterations)
        let result = solver.FindMinimum(obj, i)
        result

    let minimizeDefault f i = minimize OptimizationParams.defaultValue f i

