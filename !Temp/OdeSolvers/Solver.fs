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


    // public delegate void ndimensional_ode_rp (double[] y, double x, double[] dy, object obj);
    let solveA (o : OdeParams) (f : double[] -> double -> double[]) (i : array<double>) = 
        let n = i.Length
        //let mutable dy = [| for _ in 0..i.Length -> 0.0 |]
        //let d = Func<double[], double, double[], obj> (fun x t -> f x t)


        //alglib.odesolverstate s;
        //int m;
        //double[] xtbl;
        //double[,] ytbl;
        //alglib.odesolverreport rep;
        //alglib.odesolverrkck(y, x, eps, h, out s);
        //alglib.odesolversolve(s, ode_function_1_diff, null);
        //alglib.odesolverresults(s, out m, out xtbl, out ytbl, out rep);

        //odesolverresults(odesolverstate state, out int m, out double[] xtbl, out double[,] ytbl, out odesolverreport rep)

        let eps = 0.00001
        let h = o.stepSize
        //let mutable s = new alglib.odesolverstate()
        //let mutable m: int = 2
        //let mutable xtbl : array<double> = [||]
        //let mutable ytbl : double[,] = Array2D.zeroCreate<double> n n
        let x : array<double> = [| o.startTime; o.endTime |]
        let y : array<double> = i
        let rep = new alglib.odesolverreport ()
        let d = alglib.ndimensional_ode_rp (fun x t y _ -> f x t |> Array.mapi(fun i e -> y.[i] <- e) |> ignore)

        //odesolverrkck(double[] y, int n, double[] x, int m, double eps, double h, out odesolverstate state)
        let mutable s = alglib.odesolverrkck(y, x, eps, h)
        //do alglib.odesolverrkck(y, int n, x, int m, eps, h, ref s)
        do alglib.odesolversolve(s, d, null)
        let mutable (m, xtbl, ytbl, rep) = alglib.odesolverresults(s)
        //printfn "ytbl = %A" (ytbl.[1,*])
        ytbl
