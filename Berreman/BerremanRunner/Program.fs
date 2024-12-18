namespace Softellect.Samples.DistrProc.SolverRunner

open System
open Softellect.Sys.ExitErrorCodes
open Softellect.DistributedProcessing.SolverRunner.Implementation
open Softellect.DistributedProcessing.SolverRunner.Program
open Softellect.DistributedProcessing.Primitives.Common
open Softellect.DistributedProcessing.SolverRunner.Primitives
open Softellect.DistributedProcessing.SolverRunner.OdeSolver
open Softellect.Sys.Logging
open Softellect.Sys.Primitives
open Softellect.Sys.Core
open Plotly.NET
open Giraffe.ViewEngine
open Softellect.Analytics.Wolfram
open Softellect.Analytics.AppSettings
open Softellect.Analytics.Primitives
open System.Threading

module Program =

    let solverId = "3B6ACC75-AC22-4838-B275-1866F6E68C0B" |> Guid.Parse |> SolverId
    let solverName = SolverName "Berreman"

    /// Treat all values of u less than this as zero.
    let correctionValue = 1.0e-12


    /// https://en.wikipedia.org/wiki/Harmonic_oscillator
    /// Interesting values: k = 1, c = 0.1
    let dampedHarmonicOscillator (k: double) (c: double) (t: double) (x: double[]) (i: int): double =
        match i with
        | 0 -> x.[1]                            // dx1/dt = velocity
        | 1 -> -k * x.[0] - c * x.[1]            // dx2/dt = -kx - cx (acceleration)
        | _ -> failwith "Invalid index"


    /// https://en.wikipedia.org/wiki/Lorenz_system
    /// Interesting values: sigma = 10, rho = 28, beta = 8/3
    let lorenzSystem (sigma: double) (rho: double) (beta: double) (t: double) (x: double[]) (i: int): double =
        Logger.logTrace $"lorenzSystem: t = {t}."
        // Thread.Sleep(1_000_000) // Frees the derivative like forever.

        match i with
        | 0 -> sigma * (x.[1] - x.[0])               // dx1/dt = sigma * (x2 - x1)
        | 1 -> x.[0] * (rho - x.[2]) - x.[1]         // dx2/dt = x1 * (rho - x3) - x2
        | 2 -> x.[0] * x.[1] - beta * x.[2]          // dx3/dt = x1 * x2 - beta * x3
        | _ -> failwith "Invalid index"


    /// https://en.wikipedia.org/wiki/Lotka%E2%80%93Volterra_equations
    /// Interesting values: alpha = 2/3, beta = 4/3, gamma = 1, delta = 1
    let lotkaVolterra (alpha: double) (beta: double) (gamma: double) (delta: double) (t: double) (x: double[]) (i: int): double =
        match i with
        | 0 -> alpha * x.[0] - beta * x.[0] * x.[1]   // dx1/dt = alpha * x1 - beta * x1 * x2 (prey)
        | 1 -> delta * x.[0] * x.[1] - gamma * x.[1]  // dx2/dt = delta * x1 * x2 - gamma * x2 (predator)
        | _ -> failwith "Invalid index"


    type DerivativeCalculator
        with
        static member dampedHarmonicOscillator k c =
            dampedHarmonicOscillator k c |> OneByOne

        static member lorenzSystem sigma rho beta =
            lorenzSystem sigma rho beta |> OneByOne

        static member lotkaVolterra alpha beta gamma delta =
            lotkaVolterra alpha beta gamma delta |> OneByOne

        member d.delayed n =
            match n with
            | None -> d
            | Some (e : int) ->
                match d with
                | OneByOne f ->
                    let g t x i =
                        if i = 0
                        then
                            Logger.logTrace $"Sleeping for {e} ms..."
                            Thread.Sleep(e)
                        f t x i
                    OneByOne g
                | FullArray f ->
                    let g t x =
                        Logger.logTrace $"Sleeping for {e} ms..."
                        Thread.Sleep(e)
                        f t x
                    FullArray g


    type DampedHarmonicOscillatorData =
        {
            k: double
            c: double
        }

        member d.derivativeCalculator = DerivativeCalculator.dampedHarmonicOscillator d.k d.c


    type LorenzSystemData =
        {
            sigma : double
            rho : double
            beta : double
        }

        member d.derivativeCalculator = DerivativeCalculator.lorenzSystem d.sigma d.rho d.beta


    type LotkaVolterraData =
        {
            alpha : double
            beta : double
            gamma : double
            delta : double
        }

        member d.derivativeCalculator = DerivativeCalculator.lotkaVolterra d.alpha d.beta d.gamma d.delta


    type TestDerivativeData =
        | DampedHarmonicOscillator of DampedHarmonicOscillatorData
        | LorenzSystem of LorenzSystemData
        | LotkaVolterra of LotkaVolterraData

        member d.derivativeCalculator =
            match d with
            | DampedHarmonicOscillator d -> d.derivativeCalculator
            | LorenzSystem d -> d.derivativeCalculator
            | LotkaVolterra d -> d.derivativeCalculator


    let outputParams =
        {
            noOfOutputPoints = 4_000
            noOfProgressPoints = 100
            noOfResultDetailedPoints = Some 20
        }


    /// That's 'I in the type signature. This is just data, no functions, as it must be serializable.
    type TestInitialData =
        {
            seedValue : int
            delay : int option
            evolutionTime : EvolutionTime
            modelId : int
        }


    /// That's 'D in the type signature. This is a mix of data and functions.
    type TestSolverData =
        {
            initialData : TestInitialData
            derivativeData : TestDerivativeData
            initialValues : double[]
            chartLabels : string[]
        }

        member d.inputParams =
            {
                startTime = EvolutionTime 0m
                endTime = d.initialData.evolutionTime
            }

        member d.odeParams =
            {
                stepSize = 0.0
                absoluteTolerance = AbsoluteTolerance.defaultValue
                odeSolverType = OdePack (Bdf, ChordWithDiagonalJacobian, UseNonNegative correctionValue)
                derivative = d.derivativeData.derivativeCalculator.delayed d.initialData.delay
            }


    type DampedHarmonicOscillatorData
        with
        static member create i =
            let rnd = Random(i.seedValue)

            {
                initialData = i
                derivativeData =
                    {
                        k = 1.0 + (rnd.NextDouble() - 0.5) * 0.1
                        c = 0.1 + (rnd.NextDouble() - 0.5) * 0.01
                    }
                    |> DampedHarmonicOscillator
                initialValues = [| 10.0 + (rnd.NextDouble() - 0.5) * 1.0; 10.0 + (rnd.NextDouble() - 0.5) * 1.0 |]
                chartLabels = [| "Velocity"; "Acceleration" |]
            }


    type LorenzSystemData
        with
        static member create i =
            let rnd = Random(i.seedValue)

            {
                initialData = i
                derivativeData =
                    {
                        sigma = 10.0 + (rnd.NextDouble() - 0.5) * 1.0
                        rho = 28.0 + (rnd.NextDouble() - 0.5) * 2.0
                        beta = (8.0 / 3.0) + (rnd.NextDouble() - 0.5) * 0.1
                    }
                    |> LorenzSystem
                initialValues = [| 10.0 + (rnd.NextDouble() - 0.5) * 1.0; 10.0 + (rnd.NextDouble() - 0.5) * 1.0; 10.0 + (rnd.NextDouble() - 0.5) * 1.0 |]
                chartLabels = [| "x"; "y"; "z" |]
            }


    type LotkaVolterraData
        with
        static member create i =
            let rnd = Random(i.seedValue)

            {
                initialData = i
                derivativeData =
                    {
                        alpha = 2.0 / 3.0 + (rnd.NextDouble() - 0.5) * 0.1
                        beta = 4.0 / 3.0 + (rnd.NextDouble() - 0.5) * 0.1
                        gamma = 1.0 + (rnd.NextDouble() - 0.5) * 0.1
                        delta = 1.0 + (rnd.NextDouble() - 0.5) * 0.1
                    }
                    |> LotkaVolterra
                initialValues = [| 10.0 + (rnd.NextDouble() - 0.5) * 1.0; 10.0 + (rnd.NextDouble() - 0.5) * 1.0 |]
                chartLabels = [| "Prey"; "Predator" |]
            }


    type TestSolverData
        with
        static member create i =
            let data =
                match i.modelId with
                | 1 -> LotkaVolterraData.create i
                | 2 -> DampedHarmonicOscillatorData.create i
                | 3 -> LorenzSystemData.create i
                | _ -> failwith $"Invalid model id: {i.modelId}."

            data


    /// That's 'P in the type signature.
    type TestProgressData =
        {
            x : int
        }


    /// That's 'C in the type signature.
    type TestChartData =
        {
            x : double[]
        }


    type ChartDescription =
        {
            Heading : string
            Text : string
        }


    let toDescription h t =
        {
            Heading = h
            Text = t
        }


    let toEmbeddedHtmlWithDescription (description : ChartDescription) (gChart : GenericChart) =
        let plotlyRef = PlotlyJSReference.Full

        let displayOpts =
            DisplayOptions.init(
                AdditionalHeadTags = [
                    script [_src description.Heading] []
                ],
                // Description = [
                //     h1 [] [str description.Heading]
                //     h2 [] [str description.Text]
                // ],
                PlotlyJSReference = plotlyRef
            )

        let result =
            gChart
            |> Chart.withDisplayOptions(displayOpts)
            |> GenericChart.toEmbeddedHTML

        result


    let toHtmlFileName (FileName fileName) =
        if fileName.EndsWith(".html", StringComparison.OrdinalIgnoreCase) then fileName
        else fileName + ".html"
        |> FileName


    let getHtmlChart fileName d ch =
        {
            textContent = toEmbeddedHtmlWithDescription d ch
            fileName = toHtmlFileName fileName
        }
        |> TextResult


    let tryGetInputFileName inputFolder (q : RunQueueId) = (FileName $"{q.value}.m").tryGetFullFileName (Some inputFolder)
    let tryGetOutputFileName outputFolder (q : RunQueueId) = (FileName $"{q.value}.png").tryGetFullFileName (Some outputFolder)


    let getWolframChart (q : RunQueueId) (d : TestSolverData) (c : list<ResultSliceData<TestChartData>>) =
        let w = getSolverWolframParams solverId

        match tryGetInputFileName w.wolframInputFolder q, tryGetOutputFileName w.wolframOutputFolder q with
        | Ok i, Ok o ->
            let c1 = c |> List.rev
            let t = c1 |> List.map(fun e -> double e.t)
            let legends = d.chartLabels

            let d =
                c1.Head.resultData.x
                |> Array.mapi (fun i  _ -> { dataLabel = legends[i] |> DataLabel; dataPoints = c1 |> List.mapi (fun j e -> { x = t[j]; y = e.resultData.x[i] }) })

            let p = { ListLineParams.defaultValue with imageSize = UserDefinedImageSize "1000" |> Some}

            getListLinePlot i o p d
        | _ ->
            Logger.logError $"getWolframChart - Cannot get data for: %A{q}."
            None


    let getCharts (q : RunQueueId) (d : TestSolverData) (c : list<ResultSliceData<TestChartData>>) =
        Logger.logTrace $"getChart - q: '%A{q}', c.Length: '%A{c.Length}'."

        let charts =
            match c |> List.tryHead with
            | Some h ->
                h.resultData.x
                |> Array.mapi (fun i  _ -> Chart.Line(c |> List.map (fun c -> c.t, c.resultData.x[i]), Name = d.chartLabels[i]))
            | None -> [||]

        let chart = Chart.combine charts

        [
            getHtmlChart (FileName $"{q.value}") (toDescription "Heading" "Text") chart |> Some
            getWolframChart q d c
        ]
        |> List.choose id
        |> Some


    [<EntryPoint>]
    let main argv =
        let retVal =
            try
                // To check that invariant is actually passed back.
                let rnd = Random()
                let getInvariant() = (1.0 + (rnd.NextDouble() - 0.5) * 0.0001) |> RelativeInvariant

                let chartGenerator =
                    {
                        getResultData = fun _ _ (x : double[]) -> { x = x }
                        generateResults = fun q d _ c -> getCharts q d c
                        generateDetailedResults = fun _ _ _ _ -> None
                    }

                let getUserProxy (solverData : TestSolverData) =
                    let solverRunner = createOdeSolver solverData.inputParams solverData.odeParams

                    let solverProxy =
                        {
                            getInitialData = _.initialValues
                            getProgressData = None
                            getInvariant = fun _ _ _ -> getInvariant()
                            getOptionalFolder = fun _ _ -> None
                        }

                    {
                        solverRunner = solverRunner
                        solverProxy = solverProxy
                        resultGenerator = chartGenerator
                    }

                // Call solverRunnerMain<'D, 'P, 'X, 'C>
                Logger.logTrace "Calling solverRunnerMain..."
                solverRunnerMain<TestSolverData, TestProgressData, double[], TestChartData> solverId getUserProxy argv
            with
            | e ->
                Logger.logCrit($"Exception: %A{e}.")
                CriticalError

        // Console.ReadLine() |> ignore
        retVal
